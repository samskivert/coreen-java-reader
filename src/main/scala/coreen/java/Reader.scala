//
// $Id$

package coreen.java

import java.io.File
import java.net.URI

import javax.tools.JavaFileObject
import javax.tools.SimpleJavaFileObject
import javax.tools.ToolProvider

import com.sun.source.tree.ClassTree
import com.sun.source.tree.CompilationUnitTree
import com.sun.source.tree.MethodTree
import com.sun.source.tree.Tree
import com.sun.source.tree.VariableTree
import com.sun.source.util.JavacTask
import com.sun.source.util.TreePathScanner
import com.sun.tools.javac.tree.JCTree._

import scala.xml.Elem
import scala.collection.mutable.ArrayBuffer

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
  def process (files :Seq[File]) :List[Elem] = {
    val fm = compiler.getStandardFileManager(null, null, null) // TODO: args?
    process0(fm.getJavaFileObjects(files.toArray :_*).asScala.toList)
  }

  /**
   * Processes the supplied text as a source file with the specified name.
   * @return a {@code <compunit>} element containing the source code's defs and uses.
   */
  def process (filename :String, content :String) :Elem =
    process0(List(mkTestObject(filename, content))) head

  private def process0 (files :List[JavaFileObject]) :List[Elem] = {
    val options = null // List("-Xjcov").asJava
    val task = compiler.getTask(null, null, null, options, null,
                                files.asJava).asInstanceOf[JavacTask]
    val asts = task.parse.asScala
    task.analyze
    asts map(ast => {
      val path = ast.asInstanceOf[JCCompilationUnit].sourcefile.toUri.getPath
      <compunit src={path}>
      {scanner(ast)}
      </compunit>
    }) toList
  }

  private def mkTestObject (file :String, content :String) =
    new SimpleJavaFileObject(URI.create("test:/" + file), JavaFileObject.Kind.SOURCE) {
      override def getCharContent (ignoreEncodingErrors :Boolean) :CharSequence = content
    }

  // work around Scala's penchant to assign structural types where they are not wanted
  private val scanner = new TreePathScanner[Unit,ArrayBuffer[Elem]] with (Tree => List[Elem]) {
    def apply (path :Tree) :List[Elem] = {
      val buf = ArrayBuffer[Elem]()
      scan(path, buf)
      buf toList
    }

    override def visitCompilationUnit (node :CompilationUnitTree, buf :ArrayBuffer[Elem]) {
      val oldunit = _curunit
      _curunit = node.asInstanceOf[JCCompilationUnit]
      buf += <def name={_curunit.packge.toString} type="type"
                  id={_curunit.packge.toString}
                  start={_curunit.pos.toString}
             >{capture(super.visitCompilationUnit(node, _))}</def>
      _curunit = oldunit
    }

    override def visitClass (node :ClassTree, buf :ArrayBuffer[Elem]) {
      val otree = _curclass
      _curclass = node.asInstanceOf[JCClassDecl]
      buf += <def name={_curclass.name.toString} type="type"
                  id={_curclass.name.toString}
                  start={_curclass.pos.toString}
             >{capture(super.visitClass(node, _))}</def>
      _curclass = otree
    }

    override def visitMethod (node :MethodTree, buf :ArrayBuffer[Elem]) {
      val tree = node.asInstanceOf[JCMethodDecl]
      // don't emit a def for synthesized ctors
      if (tree.getStartPosition != tree.getEndPosition(_curunit.endPositions)) {
        val name = if (tree.name.toString == "<init>") _curclass.name else tree.name
        buf += <def name={name.toString} type="func"
                    start={tree.getStartPosition.toString}
               >{capture(super.visitMethod(node, _))}</def>
      }
    }

    override def visitVariable (node :VariableTree, buf :ArrayBuffer[Elem]) {
      val tree = node.asInstanceOf[JCVariableDecl]
      val target = if (tree.sym == null) "unknown" else tree.sym.`type`.toString
      buf += <def name={tree.name.toString} type="term"
                  start={tree.getStartPosition.toString}
             ><use name={tree.vartype.toString}
                   target={target}
                   start={tree.vartype.getStartPosition.toString}/></def>
    }

    protected def capture (call :ArrayBuffer[Elem] => Unit) = {
      val sbuf = ArrayBuffer[Elem]()
      call(sbuf)
      sbuf
    }

    protected var _curunit :JCCompilationUnit = _
    protected var _curclass :JCClassDecl = _
  }

  private val compiler = com.sun.tools.javac.api.JavacTool.create
}
