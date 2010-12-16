//
// $Id$

package coreen.java

import java.io.{File, StringWriter, Writer}
import java.util.regex.{Pattern, Matcher}
import java.net.{URI, URL}

import javax.lang.model.element.ElementKind
import javax.tools.JavaFileObject
import javax.tools.SimpleJavaFileObject
import javax.tools.ToolProvider

import com.sun.source.tree._
import com.sun.source.util.{JavacTask, TreePath, TreePathScanner}
import com.sun.tools.javac.code.{Flags, Scope, Symbol, Type, Types, TypeTags}
import com.sun.tools.javac.code.Symbol._
import com.sun.tools.javac.tree.JCTree._
import com.sun.tools.javac.tree.{JCTree, Pretty}
import com.sun.tools.javac.util.{List => JCList, Name, Context}

import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scala.xml.{Elem, NodeSeq}

import scalaj.collection.Imports._

/**
 * Provides an API for convering Java source to name-resolved source.
 */
object Reader
{
  /**
   * Processes a list of source files.
   * @return a list of {@code <compunit>} elements containing their defs and uses.
   */
  def process (files :Seq[File], classpath :Seq[File]) :Iterable[Elem] = {
    val fm = _compiler.getStandardFileManager(null, null, null) // TODO: args?
    process0(fm.getJavaFileObjects(files.toArray :_*).asScala.toList, classpath)
  }

  /**
   * Processes the supplied text as a source file with the specified name.
   * @return a {@code <compunit>} element containing the source code's defs and uses.
   */
  def process (filename :String, content :String) :Elem =
    process0(List(mkTestObject(filename, content)), List()) head

  /**
   * Processes the supplied (filename, source text) pairs.
   * @return a list of {@code <compunit>} elements containing the source code's defs and uses.
   */
  def process (sources :List[(String, String)]) :Iterable[Elem] =
    process0(sources.map(p => mkTestObject(p._1, p._2)), List())

  private def process0 (files :List[JavaFileObject], classpath :Seq[File]) :Iterable[Elem] = {
    val cpopt = if (classpath.length == 0) Nil
                else List("-classpath", classpath.mkString(File.pathSeparator))
    val options = List("-Xjcov") ++ cpopt
    val task = _compiler.getTask(
      null, null, null, options.asJava, null, files.asJava).asInstanceOf[JavacTask]
    val asts = task.parse.asScala
    task.analyze

    // reach in and get the Context that we can use to get access to compiler services
    val cf = task.getClass.getDeclaredField("context")
    cf.setAccessible(true)
    _context = cf.get(task).asInstanceOf[Context]
    _types = Types.instance(_context)

    // annoyingly, there's no (public) way to tell the task that we're done without generating
    // .class files, so instead we have to do this reach around
    val endContext = task.getClass.getDeclaredMethods.find(_.getName == "endContext").head
    endContext.setAccessible(true)
    endContext.invoke(task)

    asts.view map(ast => {
      val text = ast.asInstanceOf[JCCompilationUnit].sourcefile.getCharContent(true).toString
      // TODO: someday we should be able to remove .getPath (or maybe even use toUri.toString)
      val file = new File(ast.asInstanceOf[JCCompilationUnit].sourcefile.toUri.getPath)
      <compunit src={file.toURI.toString}>
      {_scanner.scan(text, ast)}
      </compunit>
    })
  }

  private def mkTestObject (file :String, content :String) =
    new SimpleJavaFileObject(URI.create("test:/" + file), JavaFileObject.Kind.SOURCE) {
      override def getCharContent (ignoreEncodingErrors :Boolean) :CharSequence = content
    }

  private class Scanner extends TreePathScanner[Unit,ArrayBuffer[Elem]] {
    def scan (text :String, path :Tree) :List[Elem] = {
      val buf = ArrayBuffer[Elem]()
      _text = text
      scan(path, buf)
      buf toList
    }

    override def visitCompilationUnit (node :CompilationUnitTree, buf :ArrayBuffer[Elem]) {
      val oldunit = _curunit
      _curunit = node.asInstanceOf[JCCompilationUnit]
      withId(_curunit.packge.toString) {
        buf += <def name={_curunit.packge.toString} id={_curid} kind="module" flavor="none"
                    access={"public"}
                    start={_text.indexOf(_curunit.packge.toString, _curunit.pos).toString}>
                 <sig>{_curunit.packge.toString}</sig>{
                    capture(super.visitCompilationUnit(node, _))
                 }</def>
      }
      _curunit = oldunit
    }

    override def visitClass (node :ClassTree, buf :ArrayBuffer[Elem]) = withScope {
      val oclass = _curclass
      _curclass = node.asInstanceOf[JCClassDecl]

      val isAnon = _curclass.name.toString == ""
      val clid = _curclass.name + (if (isAnon) "$" + nextanon() else "")

      val cname = if (isAnon) {
        if (_curclass.extending != null) _curclass.extending.toString
        else _curclass.implementing.toString
      } else clid

      val flavor = if (hasFlag(_curclass.mods, Flags.ANNOTATION)) "annotation"
                 else if (hasFlag(_curclass.mods, Flags.ENUM)) "enum"
                 else if (hasFlag(_curclass.mods, Flags.INTERFACE)) "interface"
                 else if (hasFlag(_curclass.mods, Flags.ABSTRACT)) "abstract_class"
                 else "class"

      def getSupers (t :Type) =
        if (t == null) Nil
        else if (t.isInterface) _types.interfaces(t).asScala
        else _types.interfaces(t).prepend(_types.supertype(t)).asScala
      val supers = getSupers(_curclass.`type`) map(
        t => targetForTypeSym(_types.erasure(t).tsym)) mkString(";")

      // TODO: improve approach to finding position of class name
      val start = _text.indexOf(cname, _curclass.getStartPosition)

      val ocount = _anoncount
      _anoncount = 0
      withId(joinDefIds(_curid, clid)) {
        // name in anon classes is "", but for signature generation we want to replace it with the
        // name that will be later assigned by the compiler EnclosingClass$N
        val sigw = new StringWriter
        val sigp = new SigPrinter(sigw, _curid, _curclass.name.table.fromString(clid)) {
          override def printAnnotations (trees :JCList[JCAnnotation]) {
            super.printAnnotations(trees)
            if (!trees.isEmpty) println
          }
        }
        sigp.printExpr(_curclass)

        // we allow the name to be "" for anonymous classes so that they can be properly filtered
        // in the user interface; we eventually probably want to be more explicit about this
        buf += <def name={_curclass.name.toString} id={_curid} kind="type" flavor={flavor}
                    access={flagsToAccess(_curclass.mods.flags)} supers={supers}
                    start={start.toString} bodyStart={_curclass.getStartPosition.toString}
                    bodyEnd={_curclass.getEndPosition(_curunit.endPositions).toString}>
                 <sig>{sigw.toString}{sigp.elems}</sig>{findDoc(_curclass.getStartPosition)}{
                   capture(super.visitClass(node, _))
                 }</def>
      }
      _curclass = oclass
      _anoncount = ocount
    }

    override def visitMethod (node :MethodTree, buf :ArrayBuffer[Elem]) = withScope {
      val ometh = _curmeth
      _curmeth = node.asInstanceOf[JCMethodDecl]

      // don't emit a def for synthesized ctors
      if (!hasFlag(_curmeth.mods, Flags.GENERATEDCONSTR)) {
        val isCtor = (_curmeth.name eq _curmeth.name.table.init)
        val flavor = if (isCtor) "constructor"
                   else if (hasFlag(_curclass.mods, Flags.INTERFACE) ||
                            hasFlag(_curmeth.mods, Flags.ABSTRACT)) "abstract_method"
                   else if (hasFlag(_curmeth.mods, Flags.STATIC)) "static_method"
                   else "method"

        // interface methods are specially defined to always be public
        val access = if (hasFlag(_curclass.mods.flags, Flags.INTERFACE)) "public"
                     else flagsToAccess(_curmeth.mods.flags)

        val name = if (isCtor) _curclass.name else _curmeth.name

        val supers = Option(_curmeth.sym) flatMap(findSuperMethod) map(
          s => targetForSym(_curmeth.name, s)) getOrElse("")

        val methid = if (_curmeth.`type` == null) "" else _curmeth.`type`.toString
        withId(joinDefIds(_curid, name + methid)) {
          val sig = new StringWriter
          val sigp = new SigPrinter(sig, _curid, _curclass.name)
          sigp.printExpr(_curmeth)

          buf += <def name={name.toString} id={_curid} kind="func"
                      flavor={flavor} access={access} supers={supers}
                      start={_text.indexOf(name.toString, _curmeth.getStartPosition).toString}
                      bodyStart={_curmeth.getStartPosition.toString}
                      bodyEnd={_curmeth.getEndPosition(_curunit.endPositions).toString}>
                   <sig>{sig.toString.trim}{sigp.elems}</sig>{findDoc(_curmeth.getStartPosition)}{
                     capture(super.visitMethod(node, _))
                   }</def>
        }
      }
      _curmeth = ometh
    }

    override def visitTypeParameter (node :TypeParameterTree, buf :ArrayBuffer[Elem]) {
      val tree = node.asInstanceOf[JCTypeParameter]

      withId(joinDefIds(_curid, node.getName.toString)) {
        val sig = new StringWriter
        val sigp = new SigPrinter(sig, _curid, null)
        sigp.printExpr(tree)

        buf += <def name={node.getName.toString} id={_curid} kind="term"
                    flavor="type_param" access="public" start={tree.getStartPosition.toString}>
                 <sig>{sig.toString}{sigp.elems}</sig>{
                   capture(super.visitTypeParameter(node, _))
                 }</def>
      }
    }

    override def visitBlock (node :BlockTree, buf :ArrayBuffer[Elem]) {
      withScope(super.visitBlock(node, buf))
    }

    override def visitVariable (node :VariableTree, buf :ArrayBuffer[Elem]) {
      val tree = node.asInstanceOf[JCVariableDecl]
      val flavor = if (_curmeth == null) {
                   if (hasFlag(tree.mods, Flags.STATIC)) "static_field"
                   else "field"
                 } else if (hasFlag(tree.mods, Flags.PARAMETER)) "param"
                   else "local"
      val access = flavor match {
        case "static_field" | "field" => flagsToAccess(tree.mods.flags)
        case _ => "default"
      }

      val doc = if (_curmeth == null) findDoc(tree.getStartPosition) else NodeSeq.Empty
      withId(joinDefIds(_curid, tree.name.toString)) {
        // add a symtab mapping for this vardef
        if (tree.sym != null) _symtab.head += (tree.sym -> _curid)

        val sigw = new StringWriter
        val sigp = new SigPrinter(sigw, _curid, _curclass.name)
        sigp.printExpr(tree)
        // filter out the wacky crap Pretty puts in for enums
        val sig = sigw.toString.trim.replace("/*public static final*/ ", "")

        val varend = tree.vartype.getEndPosition(_curunit.endPositions)
        val start = _text.indexOf(tree.name.toString, varend)
        val bodyStart = if (tree.getStartPosition == -1) start else tree.getStartPosition
        buf += <def name={tree.name.toString} id={_curid} kind="term" flavor={flavor}
                    access={access} start={start.toString} bodyStart={bodyStart.toString}
                    bodyEnd={tree.getEndPosition(_curunit.endPositions).toString}>
                 <sig>{sig}{sigp.elems}</sig>{doc}{
                   if (hasFlag(tree.mods, Flags.ENUM)) NodeSeq.Empty
                   else capture(super.visitVariable(node, _))
                 }</def>
      }
    }

    override def visitIdentifier (node :IdentifierTree, buf :ArrayBuffer[Elem]) {
      val tree = node.asInstanceOf[JCIdent]
      if (_curclass != null && // make sure we're not looking at an import
          tree.sym != null && !inAnonExtendsOrImplements &&
          !hasFlag(tree.sym.flags, Flags.SYNTHETIC) && !isSynthSuper(tree))
      {
        // if this identifier is part of a "new C" expression, we want to climb up the AST and get
        // the constructor from our parent tree node
        val pnode = getCurrentPath.getParentPath.getLeaf
        val tsym = pnode.getKind match {
          case Tree.Kind.NEW_CLASS => {
            val csym = pnode.asInstanceOf[JCNewClass].constructor
            // if the ctor type could not be resolved, bail early
            if (csym == null) tree.sym
            // if this is an anonymous class constructor...
            else if (csym.owner.name == csym.owner.name.table.empty) {
              // TODO: if the parent type is an interface, there will be no constructor (and it
              // would be weird to link to the zero-argument Object constructor), but if the type
              // is a class, it would be nice to link to the appropriate constructor
              // val ptype = _types.supertype(csym.owner.`type`)
              // TODO: if supertype is not Object, find and use super ctor
              tree.sym // for now, target the type itself
            } else csym
          }
          case _ => tree.sym
        }
        val target = targetForSym(tree.name, tsym)
        buf += <use name={tree.name.toString} target={target} kind={kindForSym(tree.sym)}
                    start={tree.getStartPosition.toString}/>
      }
    }

    override def visitMemberSelect (node :MemberSelectTree, buf :ArrayBuffer[Elem]) {
      super.visitMemberSelect(node, buf)
      val tree = node.asInstanceOf[JCFieldAccess]
      if (_curclass != null && // make sure we're not looking at an import
          tree.sym != null) {
        val target = targetForSym(tree.name, tree.sym)
        // TODO: is there a better way to get the start position of the selected name?
        val selend = tree.getStartPosition + tree.selected.toString.length
        buf += <use name={tree.name.toString} target={target} kind={kindForSym(tree.sym)}
                    start={_text.indexOf(tree.name.toString, selend).toString}/>
      }
    }

    /** Anonymous classes will emit a use for the supertype of the anonymous class in the block
     * that constructs the class, however the AST is expanded to:
     * {@code foo = new <empty> extends AnonSuper { ... } // or implements AnonSuper { ... }}
     * which will result in a second use for the anonymous supertype, which we want to suppress. */
    private def inAnonExtendsOrImplements = getCurrentPath.getParentPath.getLeaf match {
      case cd :JCClassDecl => cd.name.toString == ""
      case _ => false
    }

    // the only way to identify a synthesized super() seems to be to check that its source position
    // is the same as the enclosing block, javac helpfully fails to add a SYNTHETIC flag
    private def isSynthSuper (tree :JCIdent) =
      (tree.name == tree.name.table._super && enclosingBlock(getCurrentPath).map(
        tree.getStartPosition != _.getStartPosition).getOrElse(false))

    private def enclosingBlock (path :TreePath) :Option[JCBlock] =
      Option(path) flatMap(_.getLeaf match {
        case tree :JCBlock => Some(tree)
        case _ => enclosingBlock(path.getParentPath)
      })

    private def pathToString (path :TreePath) :String =
      (if (path.getParentPath == null) ""
       else pathToString(path.getParentPath) + ".") + path.getLeaf.getKind

    private def targetForSym (name :Name, sym :Symbol) :String = targetForSym(name.toString, sym)
    private def targetForSym (name :String, sym :Symbol) :String = sym match {
      case vs :VarSymbol => vs.getKind match {
        case ElementKind.FIELD => joinDefIds(targetForSym("<error>", vs.owner), name)
        // ENUM_CONSTANT: TODO
        // EXCEPTION_PARAMETER, PARAMETER, LOCAL_VARIABLE (all in symtab)
        case _ => _symtab.map(_.get(vs)).flatten.headOption.getOrElse("unknown")
      }
      case _ => targetForTypeSym(sym)
    }

    // TODO: strictly speaking this should find all interface methods but it currently stops at the
    // first
    private def findSuperMethod (m :MethodSymbol) :Option[MethodSymbol] = {
      val owner = m.owner.asInstanceOf[TypeSymbol]
      for (sup <- _types.closure(owner.`type`).asScala; if (sup != owner.`type`)) {
        val scope = sup.tsym.members
        var e = scope.lookup(m.name)
        while (e.scope != null) {
          if (!e.sym.isStatic && m.overrides(e.sym, owner, _types, true))
            return Some(e.sym.asInstanceOf[MethodSymbol])
          e = e.next
        }
      }
      None
    }

    private def findDoc (pos :Int) :NodeSeq = {
      val docEnd = _text.lastIndexOf("*/", pos)
      if (docEnd == -1) NodeSeq.Empty
      else {
        val docToDef = _text.substring(docEnd+2, pos)
        if (docToDef.trim.length != 0) NodeSeq.Empty
        else {
          val commentStart = _text.lastIndexOf("/*", docEnd)
          val docStart = _text.lastIndexOf("/**", docEnd)
          if (docStart != commentStart) NodeSeq.Empty
          else {
            val doc = trimDoc(_text.substring(docStart+3, docEnd))
            try {
              processDoc(doc)
            } catch {
              case e => {
                e.printStackTrace(System.out)
                println(doc)
                <doc>doc</doc>
              }
            }
          }
        }
      }
    }
    private val _starPref = Pattern.compile("""^\* ?""")
    private def snipStar (l :String) = _starPref.matcher(l).replaceFirst("")
    private def trimDoc (text :String) =
      text.split(_lineSeparator).map(_.trim).map(snipStar).filter(
        _.length != 0).mkString(_lineSeparator)

    private def resolveLink (text :String) :(String, Option[Symbol]) = {
      val hidx = text.indexOf("#")
      if (hidx == -1) {
        (text, None)
      } else {
        // TODO: we need to do a bunch of stuff for Javadoc symbol resolution
        val (tname, mname) = (text.substring(0, hidx), text.substring(hidx+1))
        if (tname == "") (mname, Option(_curclass) flatMap(
          cc => lookup(cc.sym.members, cc.name.table.fromString(mname))))
        else (mname, None)
      }
    }

    private def resolveValue (text :String) :(String, Option[Symbol]) = {
      println("TODO resolveValue: " + text)
      (text, None)
    }

    private def lookup (scope :Scope, name :Name) :Option[Symbol] = {
      val e = scope.lookup(name)
      if (e.scope == null) None
      else Some(e.sym)
    }

      /** Performs some primitive post-processing of Javadocs. Presently: handles {at code} and
     * strips at tags (at param etc. will be handled later, and you can look at the full source for
     * at author, etc.). */
    private def processDoc (text :String) = {
      // first expand brace tag patterns
      val btm = _braceTagPat.matcher(text)
      val uses = ArrayBuffer[Elem]()
      val sb = new StringBuffer
      while (btm.find) {
        val target = btm.group(2)
        val (start, end, (tname, tsym)) = btm.group(1) match {
          case "code" => ("<code>", "</code>", (target, None))
          case "link" => ("<code>", "</code>", resolveLink(target))
          case "linkplain" => ("", "", resolveLink(target))
          case "value" => ("<code>", "</code>", resolveValue(target))
          case _ => ("", "", (target, None)) // link, etc?
        }
        btm.appendReplacement(sb, Matcher.quoteReplacement(start))
        tsym map(sym => {
          uses += <use name={tname} target={targetForSym(tname, sym)}
                       kind={kindForSym(sym)} start={sb.length.toString}/>
        })
        sb.append(Matcher.quoteReplacement(escapeEntities(tname) + end))
      }
      btm.appendTail(sb)
      val etext = sb.toString

      // now look for a block tags section and process it
      val tm = _tagPat.matcher(etext)
      if (!tm.find) <doc>{etext}{uses}</doc>
      else {
        val preText = etext.substring(0, tm.start).trim
        var tstart = tm.start
        var tend = tm.end
        var tags = new ArrayBuffer[(String,String)]()
        while (tm.find) {
          tags += Pair(etext.substring(tstart, tend), etext.substring(tend, tm.start).trim)
          tstart = tm.start
          tend = tm.end
        }
        tags += Pair(etext.substring(tstart, tend), etext.substring(tend).trim)

        val ttlist = tags.map(_ match { case (tag, text) => tag match {
          case "@exception" | "@throws" => firstRest(text) match {
            case (e, "") => Some("<dt>throws " + e + "</dt><dd>(no docs)</dd>") // TODO: magic
            case (e, d) =>  Some("<dt>throws " + e + "</dt><dd>" + d + "</dd>") // TODO: magic
          }
          case "@param" => firstRest(text) match {
            case (p, "") => Some("<dt>" + p + "</dt><dd>(no docs)</dd>")
            case (p, d) => Some("<dt>" + p + "</dt><dd>" + d + "</dd>") // TODO: magic
          }
          case "@deprecated" => Some("<dt>Deprecated</dt><dd>" + text + "</dd>")
          case "@return" => Some("<dt>Returns</dt><dd>" + text + "</dd>")
          case "@see" => Some("<dt>See</dt><dd><code>" + text + "</code></dd>") // TODO: magic
          case "@author" | "@serial" | "@serialData" | "@serialField" | "@since"
             | "@version" => None
        }})

        <doc>{preText + (ttlist.flatten.mkString("\n") match {
          case "" => ""
          case text => "<dl>\n" + text + "</dl>"
        })}{uses}</doc>
      }
    }

    // TODO: this may need to be a full parser since it may match braces inside an @code block
    private val _braceTagPat = Pattern.compile(
      """\{@(code|link|linkplain|literal|value)\s([^}]*)\}""", Pattern.DOTALL)
    private val _tagPat = Pattern.compile(
      """@(author|deprecated|exception|param|return|see|serial|serialData|serialField|""" +
      """since|throws|version)""")

    private def firstRest (text :String) = {
      val didx = text.indexOf(" ")
      if (didx == -1) (text, "")
      else (text.substring(0, didx), text.substring(didx+1))
    }

    private def hasFlag (mods :JCModifiers, flag :Long) :Boolean = (mods.flags & flag) != 0
    private def hasFlag (flags :Long, flag :Long) :Boolean = (flags & flag) != 0

    private def flagsToAccess (flags :Long) =
      if (hasFlag(flags, Flags.PUBLIC)) "public"
      else if (hasFlag(flags, Flags.PROTECTED)) "protected"
      else if (hasFlag(flags, Flags.PRIVATE)) "private"
      else "default"

    private def escapeEntities (text :String) =
      text.replaceAll("&","&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").
           replaceAll("\"", "&quot;").replaceAll("'", "&apos;")

    private def withId (id :String)(block : =>Unit) {
      val oid = _curid
      _curid = id
      block
      _curid = oid
    }

    private def withScope (block : => Unit) {
      _symtab = MMap[VarSymbol,String]() :: _symtab
      block
      _symtab = _symtab.tail
    }

    private def capture (call :ArrayBuffer[Elem] => Unit) :Seq[Elem] = {
      val sbuf = ArrayBuffer[Elem]()
      call(sbuf)
      sbuf
    }

    private def nextanon () = { _anoncount += 1; _anoncount }
    private var _anoncount = 0

    private var _curunit :JCCompilationUnit = _
    private var _curclass :JCClassDecl = _
    private var _curmeth :JCMethodDecl = _
    private var _symtab :List[MMap[VarSymbol,String]] = Nil
    private var _curid :String = _
    private var _text :String = _
  }

  private def kindForSym (sym :Symbol) = sym match {
    case cs :ClassSymbol => "type"
    case ms :MethodSymbol => "func"
    case ts :TypeSymbol => "term" // we treat type parameters as 'type-level terms'
    case vs :VarSymbol => "term"
    case _ => "unknown"
  }

  // note the more general targetForSym in the tree traverser which can handle local names; this
  // can only handle type names, which is fine for handling targets in docs and signatures
  private def targetForTypeSym (sym :Symbol) :String = sym match {
    case null => "" // the "root" type's owner; nothing to see here, move it along
    case cs :ClassSymbol => joinDefIds(targetForTypeSym(sym.owner), sym.name.toString)
    case ps :PackageSymbol => ps.toString // keep the dots between packages
    case ts :TypeSymbol => joinDefIds(targetForTypeSym(ts.owner), ""+sym.name) // type param
    case ms :MethodSymbol => {
      val mname = if (ms.name == ms.name.table.init) ms.owner.name else ms.name
      joinDefIds(targetForTypeSym(ms.owner), "" + mname + ms.`type`)
    }
    case _ => {
      Console.println("Unhandled type sym " + sym.getClass + " '" + sym + "'")
      sym.name.toString
    }
  }

  private def joinDefIds (first :String, second :String) = {
    val sep = if (!first.isEmpty) " " else ""
    first + sep + second
  }

  private class SigPrinter (
    out :StringWriter, id :String, enclClassName :Name
  ) extends Pretty(out, false) {
    // use nodes accumulated while printing a signature
    var elems = ArrayBuffer[Elem]()
    private var _nested = false

    override def printBlock (stats :JCList[_ <: JCTree]) { /* noop! */ }
    override def printEnumBody (stats :JCList[JCTree]) { /* noop! */ }
    override def printAnnotations (trees :JCList[JCAnnotation]) {
      var l = trees
      while (l.nonEmpty) {
        val olen = out.getBuffer.length
        printStat(l.head)
        // if the annotation wasn't ommitted, add a space after it
        if (out.getBuffer.length > olen) print(" ")
        l = l.tail
      }
    }

    override def visitClassDef (tree :JCClassDecl) {
      var cpos = 0
      printAnnotations(tree.mods.annotations)
      printFlags(tree.mods.flags & ~Flags.INTERFACE)
      if ((tree.mods.flags & Flags.INTERFACE) != 0) {
        print("interface " + enclClassName)
        cpos = out.getBuffer.length - enclClassName.toString.length
        printTypeParameters(tree.typarams)
        if (tree.implementing.nonEmpty()) {
          print(" extends ")
          printExprs(tree.implementing)
        }
      } else {
        if ((tree.mods.flags & Flags.ENUM) != 0) 
          print("enum " + enclClassName)
        else
          print("class " + enclClassName)
        cpos = out.getBuffer.length - enclClassName.toString.length
        printTypeParameters(tree.typarams)
        if (tree.extending != null) {
          print(" extends ")
          printExpr(tree.extending)
        }
        if (tree.implementing.nonEmpty()) {
          print("\n  implements ")
          printExprs(tree.implementing)
        }
      }
      elems += <sigdef id={id} name={enclClassName.toString} kind="type" start={cpos.toString}/>
    }

    override def visitMethodDef (tree :JCMethodDecl) {
      // only print non-anonymous constructors
      if (tree.name != tree.name.table.init || enclClassName != null) {
        var mname = ""
        var mpos = 0
        _nested = true
        printExpr(tree.mods)
        // type parameters are now extracted into separate defs
        // printTypeParameters(tree.typarams)
        if (tree.name == tree.name.table.init) {
          mpos = out.getBuffer.length
          mname = enclClassName.toString
          print(enclClassName)
        } else {
          printExpr(tree.restype)
          mpos = out.getBuffer.length+1
          mname = tree.name.toString
          print(" " + tree.name)
        }
        print("(")
        printExprs(tree.params)
        print(")")
        // omit throws from signatures
        // if (tree.thrown.nonEmpty()) {
        //   print("\n  throws ")
        //   printExprs(tree.thrown)
        // }
        if (tree.defaultValue != null) {
          print(" default ")
          printExpr(tree.defaultValue)
        }
        _nested = false
        elems += <sigdef id={id} name={mname} kind="func" start={mpos.toString}/>
      }
    }

    override def visitTypeParameter (tree :JCTypeParameter) {
      super.visitTypeParameter(tree);

      // if we're generating the signature for a class, go ahead and emit sigdefs for our type
      // parameters, but if we're generating the signature for a type parameter def, we don't want
      // to repeat the type parameter itself as a sigdef
      if (enclClassName != null) {
        val tid = joinDefIds(id, tree.name.toString)
        val tpos = out.getBuffer.length-tree.name.toString.length
        elems += <sigdef id={tid} name={tree.name.toString} kind="term" start={tpos.toString}/>
      }
    }

    override def visitAnnotation (tree :JCAnnotation) {
      // we skip override annotations in our signatures
      if (!IgnoredAnnotations(tree.annotationType.toString)) {
        print("@")
        printExpr(tree.annotationType)
        if (tree.args != null && !tree.args.isEmpty) {
          print("(")
          printExprs(tree.args)
          print(")")
        }
      }
    }

    override def visitVarDef (tree :JCVariableDecl) {
      val oinit = tree.init
      tree.init = null
      super.visitVarDef(tree)
      tree.init = oinit
      val vpos = out.getBuffer.length-tree.name.toString.length
      // we're either printing the sig for a plain old vardef, or we're nested, in which case we're
      // printing the signature for a method, but it has parameters, and 'id' is the method id, so
      // we need to append the var name to get the var def id
      val vid = if (_nested) joinDefIds(id, tree.name.toString) else id
      elems += <sigdef id={vid} name={tree.name.toString} kind="term" start={vpos.toString}/>
    }

    override def visitIdent (tree :JCIdent) {
      if (tree.sym != null) {
        val target = tree.sym match {
          case cs :ClassSymbol => targetForTypeSym(cs)
          case ts :TypeSymbol => targetForTypeSym(ts)
          case _ => Console.println("TODO? " + tree + " " + tree.sym.getClass); tree.sym.toString
        }
        elems += <use name={tree.name.toString} target={target} kind={kindForSym(tree.sym)}
                     start={out.getBuffer.length.toString}/>
      }
      super.visitIdent(tree)
    }
  }

  private val IgnoredAnnotations = Set("Override", "SuppressWarnings")
  private val _enclClassField = classOf[Pretty].getDeclaredField("enclClassName")
  /* Reader ctor */ _enclClassField.setAccessible(true)

  private val _scanner = new Scanner
  private val _compiler = com.sun.tools.javac.api.JavacTool.create
  private val _lineSeparator = System.getProperty("line.separator")

  // these are initialized on each invocation of a compiler task
  private var _context :Context = _
  private var _types :Types = _
}
