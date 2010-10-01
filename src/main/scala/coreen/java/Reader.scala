//
// $Id$

package coreen.java

import java.io.File
import java.io.StringWriter
import java.net.{URI, URL}

import javax.tools.JavaFileObject
import javax.tools.SimpleJavaFileObject
import javax.tools.ToolProvider

import com.sun.source.tree.{ClassTree, CompilationUnitTree, MethodTree, Tree, VariableTree}
import com.sun.source.util.{JavacTask, TreePathScanner}
import com.sun.tools.javac.tree.{JCTree, Pretty}
import com.sun.tools.javac.tree.JCTree._
import com.sun.tools.javac.util.{List => JCList}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
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
    val options = if (classpath.length == 0) null
                  else List("-classpath", classpath.mkString(File.pathSeparator)).asJava
    val task = _compiler.getTask(null, null, null, options, null,
                                 files.asJava).asInstanceOf[JavacTask]
    val asts = task.parse.asScala
    task.analyze
    asts map(ast => {
      // TODO: someday we should be able to remove .getPath (or maybe even use toUri.toString)
      val file = new File(ast.asInstanceOf[JCCompilationUnit].sourcefile.toUri.getPath)
      val text = Source.fromFile(file).mkString("")
      <compunit src={file.toURI.toString}>
      {_scanner.scan(text, ast)}
      </compunit>
    }) toList
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
        buf += <def name={_curunit.packge.toString} type="type" id={_curid}
                    sig={_curunit.packge.toString}
                    start={_text.indexOf(_curunit.packge.toString, _curunit.pos).toString}
               >{capture(super.visitCompilationUnit(node, _))}</def>
      }
      _curunit = oldunit
    }

    override def visitClass (node :ClassTree, buf :ArrayBuffer[Elem]) {
      val oclass = _curclass
      _curclass = node.asInstanceOf[JCClassDecl]

      val sig = new StringWriter
      new Pretty(sig, false) {
        override def printBlock (stats :JCList[_ <: JCTree]) { /* noop! */ }
      }.printExpr(_curclass);

      withId(_curclass.`type`.toString) {
        buf += <def name={_curclass.name.toString} type="type" id={_curid} sig={sig.toString.trim}
                    start={_text.indexOf(_curclass.name.toString, _curclass.pos).toString}
               >{capture(super.visitClass(node, _))}</def>
      }
      _curclass = oclass
    }

    override def visitMethod (node :MethodTree, buf :ArrayBuffer[Elem]) {
      val ometh = _curmeth
      _curmeth = node.asInstanceOf[JCMethodDecl]

      // don't emit a def for synthesized ctors
      if (_curmeth.getStartPosition != _curmeth.getEndPosition(_curunit.endPositions)) {
        val sig = new StringWriter
        new Pretty(sig, false) {
          override def printStat (stat :JCTree) { /* noop! */ }
        }.printExpr(_curmeth);

        val name = if (_curmeth.name.toString == "<init>") _curclass.name else _curmeth.name
        withId(_curclass.`type`.toString + "." + name + _curmeth.`type`.toString) {
          buf += <def name={name.toString} type="func" id={_curid} sig={sig.toString.trim}
                      start={_text.indexOf(name.toString, _curmeth.getStartPosition).toString}
                 >{capture(super.visitMethod(node, _))}</def>
        }
      }
      _curmeth = ometh
    }

    override def visitVariable (node :VariableTree, buf :ArrayBuffer[Elem]) {
      val tree = node.asInstanceOf[JCVariableDecl]
      val target = if (tree.sym == null) "unknown" else tree.sym.`type`.toString
      val tname = tree.vartype.toString

      val oinit = tree.init
      tree.init = null
      val sig = tree.toString
      tree.init = oinit

      withId(_curid + "." + tree.name.toString) {
        buf += <def name={tree.name.toString} type="term" id={_curid} sig={sig}
                    start={_text.indexOf(tree.name.toString,
                                         tree.getStartPosition + tname.length).toString}
               ><use name={tname}
                     target={target}
                     start={_text.indexOf(tname, tree.vartype.getStartPosition).toString}/></def>
      }
    }

    protected def withId (id :String)(block : =>Unit) {
      val oid = _curid
      _curid = id
      block
      _curid = oid
    }

    protected def capture (call :ArrayBuffer[Elem] => Unit) = {
      val sbuf = ArrayBuffer[Elem]()
      call(sbuf)
      sbuf
    }

    protected var _curunit :JCCompilationUnit = _
    protected var _curclass :JCClassDecl = _
    protected var _curmeth :JCMethodDecl = _
    protected var _curid :String = _
    protected var _text :String = _
  }

  private val _scanner = new Scanner
  private val _compiler = com.sun.tools.javac.api.JavacTool.create
}
