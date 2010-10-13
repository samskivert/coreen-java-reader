//
// $Id$

package coreen.java

import java.io.File
import java.io.StringWriter
import java.util.regex.{Pattern, Matcher}
import java.net.{URI, URL}

import javax.lang.model.element.ElementKind
import javax.tools.JavaFileObject
import javax.tools.SimpleJavaFileObject
import javax.tools.ToolProvider

import com.sun.source.tree._
import com.sun.source.util.{JavacTask, TreePathScanner}
import com.sun.tools.javac.code.{Flags, Symbol, Types}
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
        buf += <def name={_curunit.packge.toString} type="module" id={_curid}
                    sig={_curunit.packge.toString}
                    start={_text.indexOf(_curunit.packge.toString, _curunit.pos).toString}
               >{capture(super.visitCompilationUnit(node, _))}</def>
      }
      _curunit = oldunit
    }

    override def visitClass (node :ClassTree, buf :ArrayBuffer[Elem]) = withScope {
      val oclass = _curclass
      _curclass = node.asInstanceOf[JCClassDecl]

      val isAnon = _curclass.name.toString == ""
      val clid = _curclass.name + (if (isAnon) "$" + nextanon() else "")

      // name in anon classes is "", but for signature generation we want to replace it with the
      // name that will be later assigned by the compiler EnclosingClass$N
      val ocname = _curclass.name
      _curclass.name = _curclass.name.table.fromString(clid)
      val sig = new StringWriter
      new Pretty(sig, false) {
        override def printBlock (stats :JCList[_ <: JCTree]) { /* noop! */ }
        override def printEnumBody (stats :JCList[JCTree]) { /* noop! */ }
      }.printExpr(_curclass)
      _curclass.name = ocname

      val cname = if (isAnon) {
        if (_curclass.extending != null) _curclass.extending.toString
        else _curclass.implementing.toString
      } else clid

      val flavor = if ((_curclass.mods.flags & Flags.ANNOTATION) != 0) "annotation"
                   else if ((_curclass.mods.flags & Flags.ENUM) != 0) "enum"
                   else if ((_curclass.mods.flags & Flags.INTERFACE) != 0) "interface"
                   else if ((_curclass.mods.flags & Flags.ABSTRACT) != 0) "abstract_class"
                   else "class"

      val ocount = _anoncount
      _anoncount = 0
      withId(_curid + "." + clid) {
        // we allow the name to be "" for anonymous classes so that they can be properly filtered
        // in the user interface; we eventually probably want to be more explicit about this
        buf += <def name={_curclass.name.toString} id={_curid} type="type" flavor={flavor}
                    sig={sig.toString.trim} doc={findDoc(_curclass.getStartPosition)}
                    start={_text.lastIndexOf(cname, _curclass.getStartPosition).toString}
                    bodyStart={_curclass.getStartPosition.toString}
                    bodyEnd={_curclass.getEndPosition(_curunit.endPositions).toString}
               >{capture(super.visitClass(node, _))}</def>
      }
      _curclass = oclass
      _anoncount = ocount
    }

    override def visitMethod (node :MethodTree, buf :ArrayBuffer[Elem]) = withScope {
      val ometh = _curmeth
      _curmeth = node.asInstanceOf[JCMethodDecl]

      // don't emit a def for synthesized ctors
      if ((_curmeth.mods.flags & Flags.GENERATEDCONSTR) == 0) {
        val sig = new StringWriter
        new Pretty(sig, false) {
          override def printStat (stat :JCTree) { /* noop! */ }
        }.printExpr(_curmeth)

        val isCtor = (_curmeth.name.toString == "<init>")
        val flavor = if (isCtor) "constructor"
                     else if ((_curclass.mods.flags & Flags.INTERFACE) != 0 ||
                              (_curmeth.mods.flags & Flags.ABSTRACT) != 0) "abstract_method"
                     else if ((_curmeth.mods.flags & Flags.STATIC) != 0) "static_method"
                     else "method"

        val name = if (isCtor) _curclass.name else _curmeth.name
        val methid = (if (_curmeth.`type` == null) "" else _curmeth.`type`).toString
        withId(_curid + "." + name + methid) {
          buf += <def name={name.toString} id={_curid} type="func" flavor={flavor}
                      sig={sig.toString.trim} doc={findDoc(_curmeth.getStartPosition)}
                      start={_text.indexOf(name.toString, _curmeth.getStartPosition).toString}
                      bodyStart={_curmeth.getStartPosition.toString}
                      bodyEnd={_curmeth.getEndPosition(_curunit.endPositions).toString}
                 >{capture(super.visitMethod(node, _))}</def>
        }
      }
      _curmeth = ometh
    }

    override def visitBlock (node :BlockTree, buf :ArrayBuffer[Elem]) {
      withScope(super.visitBlock(node, buf))
    }

    override def visitVariable (node :VariableTree, buf :ArrayBuffer[Elem]) {
      val tree = node.asInstanceOf[JCVariableDecl]
      val oinit = tree.init
      val sig = try { tree.init = null ; tree.toString } finally { tree.init = oinit }

      val flavor = if (_curmeth == null) "field"
                   else if ((tree.mods.flags & Flags.PARAMETER) != 0) "param"
                   else "local"

      val doc = if (_curmeth == null) findDoc(tree.getStartPosition) else ""
      withId(_curid + "." + tree.name.toString) {
        // add a symtab mapping for this vardef
        if (tree.sym != null) _symtab.head += (tree.sym -> _curid)
        val varend = tree.vartype.getEndPosition(_curunit.endPositions)
        buf += <def name={tree.name.toString} id={_curid} type="term" flavor={flavor}
                    sig={sig} doc={doc}
                    start={_text.indexOf(tree.name.toString, varend).toString}
                    bodyStart={tree.getStartPosition.toString}
                    bodyEnd={tree.getEndPosition(_curunit.endPositions).toString}
               >{ if ((tree.mods.flags & Flags.ENUM) != 0) NodeSeq.Empty
                  else capture(super.visitVariable(node, _)) }</def>
      }
    }

    override def visitIdentifier (node :IdentifierTree, buf :ArrayBuffer[Elem]) {
      val tree = node.asInstanceOf[JCIdent]
      if (_curclass != null && // make sure we're not looking at an import
          tree.sym != null) {
        val target = targetForSym(tree.name, tree.sym)
        buf += <use name={tree.name.toString} target={target}
                    start={tree.getStartPosition.toString}/>
      }
    }

    override def visitMemberSelect (node :MemberSelectTree, buf :ArrayBuffer[Elem]) {
      super.visitMemberSelect(node, buf)
      val tree = node.asInstanceOf[JCFieldAccess]
      if (_curclass != null && // make sure we're not looking at an import
          tree.sym != null) {
        val target = targetForSym(tree.name, tree.sym)
        val selend = tree.selected.getEndPosition(_curunit.endPositions)
        buf += <use name={tree.name.toString} target={target}
                    start={_text.indexOf(tree.name.toString, selend).toString}/>
      }
    }

    private def targetForSym (name :Name, sym :Symbol) = sym match {
      case cs :ClassSymbol => _types.erasure(cs.`type`).toString
      case ms :MethodSymbol => ms.owner + "." + ms.name + ms.`type`
      case vs :VarSymbol => vs.getKind match {
        case ElementKind.FIELD => vs.owner + "." + name
        // ENUM_CONSTANT: TODO
        // EXCEPTION_PARAMETER, PARAMETER, LOCAL_VARIABLE (all in symtab)
        case _ => _symtab.map(_.get(vs)).flatten.headOption.getOrElse("unknown")
      }
      case _ => {
        // println("TODOOZ " + name + " " + sym.getClass)
        sym.toString // TODO
      }
    }

    private def findDoc (pos :Int) = {
      val docEnd = _text.lastIndexOf("*/", pos)
      if (docEnd == -1) ""
      else {
        val docToDef = _text.substring(docEnd+2, pos)
        if (docToDef.trim.length != 0) ""
        else {
          val commentStart = _text.lastIndexOf("/*", docEnd)
          val docStart = _text.lastIndexOf("/**", docEnd)
          if (docStart != commentStart) ""
          else {
            val doc = trimDoc(_text.substring(docStart+3, docEnd))
            try {
              processDoc(doc)
            } catch {
              case e => {
                e.printStackTrace(System.out)
                println(doc)
                doc
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

    /** Performs some primitive post-processing of Javadocs. Presently: handles {at code} and
     * strips at tags (at param etc. will be handled later, and you can look at the full source for
     * at author, etc.). */
    private def processDoc (text :String) = {
      // first expand brace tag patterns
      val btm = _braceTagPat.matcher(text)
      val sb = new StringBuffer
      while (btm.find) {
        val escaped = escapeEntities(btm.group(2))
        val (start, end) = btm.group(1) match {
          case "code" => ("<code>", "</code>")
          case "link" => ("<code><u>", "</u></code>") // TODO: magic
          case "linkplain" => ("<u>", "</u>")         // TODO: same magic
          case "value" => ("<code>", "</code>")       // TODO: yet more magic
          case _ => ("", "") // link, etc?
        }
        btm.appendReplacement(sb, Matcher.quoteReplacement(start + escaped + end))
      }
      btm.appendTail(sb)
      val etext = sb.toString

      // now look for a block tags section and process it
      val tm = _tagPat.matcher(etext)
      if (!tm.find) etext
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

        val sep = "<br/>\n"
        val tagText = tags.map(_ match { case (tag, text) => tag match {
          case "@deprecated" => Some("<em>Deprecated</em>: " + text)
          case "@exception"
             | "@throws"     => Some("<em>Throws</em>: " + text) // TODO: magic
          case "@param"      => Some("<em>Param</em>: " + text) // TODO: magic
          case "@return"     => Some("<em>Returns</em>: " + text)
          case "@see"        => Some("<em>See</em>: <code>" + text + "</code>") // TODO: magic
          case "@author" | "@serial" | "@serialData" | "@serialField" | "@since"
             | "@version" => None
        }}).flatten.mkString(sep)

        preText + (if (!preText.isEmpty && !tagText.isEmpty) sep else "") + tagText
      }
    }

    // TODO: this may need to be a full parser since it may match braces inside an @code block
    private val _braceTagPat = Pattern.compile(
      """\{@(code|link|linkplain|literal|value)\s([^}]*)\}""", Pattern.DOTALL)
    private val _tagPat = Pattern.compile(
      """@(author|deprecated|exception|param|return|see|serial|serialData|serialField|""" +
      """since|throws|version)""")

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

    private def capture (call :ArrayBuffer[Elem] => Unit) = {
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

  private val _scanner = new Scanner
  private val _compiler = com.sun.tools.javac.api.JavacTool.create
  private val _lineSeparator = System.getProperty("line.separator")

  // these are initialized on each invocation of a compiler task
  private var _context :Context = _
  private var _types :Types = _
}
