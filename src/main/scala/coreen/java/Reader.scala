//
// $Id$

package coreen.java

import java.io.File
import java.io.StringWriter
import java.net.{URI, URL}

import javax.tools.JavaFileObject
import javax.tools.SimpleJavaFileObject
import javax.tools.ToolProvider

import com.sun.source.tree._
import com.sun.source.util.{JavacTask, TreePathScanner}
import com.sun.tools.javac.code.Flags
import com.sun.tools.javac.code.Symbol._
import com.sun.tools.javac.tree.JCTree._
import com.sun.tools.javac.tree.{JCTree, Pretty}
import com.sun.tools.javac.util.{List => JCList}

import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scala.xml.Elem

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
  def process (files :Seq[File], classpath :Seq[File]) :List[Elem] = {
    val fm = _compiler.getStandardFileManager(null, null, null) // TODO: args?
    process0(fm.getJavaFileObjects(files.toArray :_*).asScala.toList, classpath)
  }

  /**
   * Processes the supplied text as a source file with the specified name.
   * @return a {@code <compunit>} element containing the source code's defs and uses.
   */
  def process (filename :String, content :String) :Elem =
    process0(List(mkTestObject(filename, content)), List()) head

  private def process0 (files :List[JavaFileObject], classpath :Seq[File]) :List[Elem] = {
    val cpopt = if (classpath.length == 0) Nil
                else List("-classpath", classpath.mkString(File.pathSeparator))
    val options = List("-Xjcov") ++ cpopt
    val task = _compiler.getTask(
      null, null, null, options.asJava, null, files.asJava).asInstanceOf[JavacTask]
    val asts = task.parse.asScala
    task.analyze

    // annoyingly, there's no (public) way to tell the task that we're done without generating
    // .class files, so instead we have to do this reach around
    val endContext = task.getClass.getDeclaredMethods.find(_.getName == "endContext").head
    endContext.setAccessible(true)
    endContext.invoke(task)

    asts map(ast => {
      val text = ast.asInstanceOf[JCCompilationUnit].sourcefile.getCharContent(true).toString
      // TODO: someday we should be able to remove .getPath (or maybe even use toUri.toString)
      val file = new File(ast.asInstanceOf[JCCompilationUnit].sourcefile.toUri.getPath)
      <compunit src={file.toURI.toString}>
      {_scanner.scan(text, ast)}
      </compunit>
    }) toList;
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

      val sig = new StringWriter
      new Pretty(sig, false) {
        override def printBlock (stats :JCList[_ <: JCTree]) { /* noop! */ }
        override def printEnumBody (stats :JCList[JCTree]) { /* noop! */ }
      }.printExpr(_curclass);

      withId(_curid + "." + _curclass.name.toString) {
        buf += <def name={_curclass.name.toString} type="type" id={_curid} sig={sig.toString.trim}
                    doc={findDoc(_curclass.getStartPosition)}
                    start={_text.indexOf(_curclass.name.toString, _curclass.pos).toString}
                    bodyStart={_curclass.getStartPosition.toString}
                    bodyEnd={_curclass.getEndPosition(_curunit.endPositions).toString}
               >{capture(super.visitClass(node, _))}</def>
      }
      _curclass = oclass
    }

    override def visitMethod (node :MethodTree, buf :ArrayBuffer[Elem]) = withScope {
      val ometh = _curmeth
      _curmeth = node.asInstanceOf[JCMethodDecl]

      // don't emit a def for synthesized ctors
      if ((_curmeth.mods.flags & Flags.GENERATEDCONSTR) == 0) {
        val sig = new StringWriter
        new Pretty(sig, false) {
          override def printStat (stat :JCTree) { /* noop! */ }
        }.printExpr(_curmeth);

        val name = if (_curmeth.name.toString == "<init>") _curclass.name else _curmeth.name
        val methid = (if (_curmeth.`type` == null) "" else _curmeth.`type`).toString
        withId(_curid + "." + name + methid) {
          buf += <def name={name.toString} type="func" id={_curid} sig={sig.toString.trim}
                      doc={findDoc(_curmeth.getStartPosition)}
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
      val target = if (tree.sym == null) "unknown" else tree.sym.`type`.toString
      val tname = tree.vartype.toString

      val oinit = tree.init
      tree.init = null
      val sig = tree.toString
      tree.init = oinit

      val doc = if (_curmeth == null) findDoc(tree.getStartPosition) else ""
      withId(_curid + "." + tree.name.toString) {
        // add a mapping for this vardef
        if (tree.sym != null) _symtab.head += (tree.sym -> _curid)
        buf += <def name={tree.name.toString} type="term" id={_curid} sig={sig} doc={doc}
                    start={_text.indexOf(tree.name.toString,
                                         tree.getStartPosition + tname.length).toString}
                    bodyStart={tree.getStartPosition.toString}
                    bodyEnd={tree.getEndPosition(_curunit.endPositions).toString}
               ><use name={tname}
                     target={target}
                     start={_text.indexOf(tname, tree.vartype.getStartPosition).toString}/></def>
      }
    }

    override def visitIdentifier (node :IdentifierTree, buf :ArrayBuffer[Elem]) {
      val tree = node.asInstanceOf[JCIdent]
      if (_curclass != null && // make sure we're not looking at an import
          tree.sym != null) {
        val target = tree.sym match {
          case cs :ClassSymbol => tree.sym.`type`.toString
          case vs :VarSymbol => _symtab.map(_.get(vs)).flatten.headOption.getOrElse("unknown")
          case _ => tree.sym.`type`.toString // TODO
        }
        buf += <use name={tree.name.toString} target={target}
                    start={tree.getStartPosition.toString}/>
      }
    }

    // override def visitMethodInvocation (node :MethodInvocationTree, buf :ArrayBuffer[Elem]) {
    //   val tree = node.asInstanceOf[JCMethodInvocation]
    //   super.visitMethodInvocation(node, buf)
    // }
    override def visitMemberSelect (node :MemberSelectTree, buf :ArrayBuffer[Elem]) {
      val tree = node.asInstanceOf[JCFieldAccess]
      if (_curclass != null && // make sure we're not looking at an import
          tree.sym != null) {
        val target = tree.sym match {
          case ms :MethodSymbol => ms.owner + "." + ms.name + ms.`type`
          case _ => tree.sym.toString // TODO
        }
        val selend = tree.selected.getEndPosition(_curunit.endPositions)
        buf += <use name={tree.name.toString} target={target}
                    start={_text.indexOf(tree.name.toString, selend).toString}/>
      }
      super.visitMemberSelect(node, buf)
    }

    protected def findDoc (pos :Int) = {
      val docEnd = _text.lastIndexOf("*/", pos)
      if (docEnd == -1) ""
      else {
        val docToDef = _text.substring(docEnd+2, pos)
        if (docToDef.trim.length != 0) ""
        else {
          val commentStart = _text.lastIndexOf("/*", docEnd)
          val docStart = _text.lastIndexOf("/**", docEnd)
          if (docStart != commentStart) ""
          else trimDoc(_text.substring(docStart+3, docEnd))
        }
      }
    }
    protected def trimDoc (text :String) =
      text.split(_lineSeparator).map(_.trim).map(snipStar).filter(_.length != 0).mkString(" ")
    protected def snipStar (l :String) = if (l.startsWith("*")) l.substring(1).trim else l

    protected def withId (id :String)(block : =>Unit) {
      val oid = _curid
      _curid = id
      block
      _curid = oid
    }

    protected def withScope (block : => Unit) {
      _symtab = MMap[VarSymbol,String]() :: _symtab
      block
      _symtab = _symtab.tail
    }

    protected def capture (call :ArrayBuffer[Elem] => Unit) = {
      val sbuf = ArrayBuffer[Elem]()
      call(sbuf)
      sbuf
    }

    protected var _curunit :JCCompilationUnit = _
    protected var _curclass :JCClassDecl = _
    protected var _curmeth :JCMethodDecl = _
    protected var _symtab :List[MMap[VarSymbol,String]] = Nil
    protected var _curid :String = _
    protected var _text :String = _
  }

  private val _scanner = new Scanner
  private val _compiler = com.sun.tools.javac.api.JavacTool.create
  private val _lineSeparator = System.getProperty("line.separator")
}
