//
// $Id$

package coreen.java

import java.io.File
import java.util.Date
import java.lang.System.out

import scala.io.Source
import scala.xml.{Node, PrettyPrinter}

/**
 * Main entry point for Java Reader.
 */
object Main
{
  def main (args :Array[String]) {
    if (args.length < 2) {
      die("Usage: coreen.java.Main project_root_dir last_mod_stamp [src_dir ...]")
    }

    // extract and parse our arguments
    val (root, lastMod, srcPrefixes) = (new File(args(0)), args(1).toLong, args drop(2))

    // scan the project directory for source and jar files
    val files = Collector.collect(root, srcPrefixes, Set("jar"))

    // try to find our project dependencies via one of...
    val finders = List(locateJarsViaMaven _,       // a Maven pom.xml file
                       locateJarsViaDotCoreen _,   // a .coreen file
                       locateJarsViaScan(files) _) // all jar files found in our scan
    // ewww, there must be a better way
    val jars = finders.view.map(_(root)).find(_.isDefined).getOrElse(Some(List())).get
    out.println("Using classpath:")
    for (j <- jars) out.println("  " + j)

    // allow pretty printed output for debugging
    val print = if (java.lang.Boolean.getBoolean("pretty")) {
      val pp = new PrettyPrinter(999, 2)
      (e :Node) => out.println(pp.format(e))
    } else {
      (e :Node) => out.println(e)
    }

    // process only those sources newer than our supplied last modified cutoff
    val sources = files.get("java").getOrElse(Nil).filter(_.lastModified >= lastMod)
    if (sources.isEmpty) {
      out.println("No .java files modified after " + new Date(lastMod) + " in " + root)
    } else {
      out.println("Compiling " + sources.size + " Java source files...")
      Reader.process(sources, jars) foreach(print)
    }
  }

  def locateJarsViaMaven (root :File) :Option[Seq[File]] = {
    val pom = new File(root, "pom.xml")
    if (!pom.exists) None else {
      try {
        val p = Runtime.getRuntime.exec(Array("mvn", "dependency:build-classpath"), null, root)
        // the last line of output that does not start with '[' should be the classpath
        Source.fromInputStream(p.getInputStream).getLines.filterNot(
          _.startsWith("[")).toList.lastOption.map(toFiles)
      } catch {
        case e => out.println("Failure resolving Maven classpath " + e); None
      }
    }
  }

  def locateJarsViaDotCoreen (root :File) :Option[Seq[File]] = {
    val dc = new File(root, ".coreen")
    if (!dc.exists) None else readConfig(dc).get("cpcommand") flatMap { cpc =>
      try {
        val p = Runtime.getRuntime.exec(cpc, null, root)
        // assume the first line of output is the classpath in question
        Source.fromInputStream(p.getInputStream).getLines.toSeq.headOption.map(toFiles)
      } catch {
        case e => out.println("Failure resolving classpath via '" + cpc + "': " + e); None
      }
    }
  }

  def locateJarsViaScan (files :Map[String,Seq[File]])(root :File) = files.get("jar")

  /** Reads our .coreen configuration file into a map of key/value pairs. */
  def readConfig (cfile :File) :Map[String,String] = {
    def toPair (line :String) = line.indexOf("=") match {
      case -1 => None
      case eidx => Some(line.substring(0, eidx).trim, line.substring(eidx+1).trim)
    }
    Source.fromFile(cfile).getLines.toSeq.flatMap(toPair).toMap
  }

  /** Converts a classpath string (i.e. "foo/bar.jar:dist/classes") to a Seq[File]. */
  def toFiles (path :String) :Seq[File] = path.split(File.pathSeparator).map(new File(_)).toSeq

  def die (msg :String) = {
    System.err.println(msg)
    System.exit(255)
    error("Not reached")
  }
}
