//
// $Id$

package coreen.java

import java.io.File
import java.net.URI

import javax.tools.JavaFileObject
import javax.tools.SimpleJavaFileObject

import com.sun.source.util.JavacTask
import com.sun.tools.javac.code.Types
import com.sun.tools.javac.tree.JCTree.JCCompilationUnit
import com.sun.tools.javac.util.Context

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
    val context = cf.get(task).asInstanceOf[Context]
    val trans = new Translator(Types.instance(context))

    // annoyingly, there's no (public) way to tell the task that we're done without generating
    // .class files, so instead we have to do this reach around
    val endContext = task.getClass.getDeclaredMethods.find(_.getName == "endContext").head
    endContext.setAccessible(true)
    endContext.invoke(task)

    asts.view map trans.translate
  }

  private def mkTestObject (file :String, content :String) =
    new SimpleJavaFileObject(URI.create("test:/" + file), JavaFileObject.Kind.SOURCE) {
      override def getCharContent (ignoreEncodingErrors :Boolean) :CharSequence = content
    }

  // private val _enclClassField = classOf[Pretty].getDeclaredField("enclClassName")
  // /* Reader ctor */ _enclClassField.setAccessible(true)

  private val _compiler = com.sun.tools.javac.api.JavacTool.create
}
