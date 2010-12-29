//
// $Id$

package coreen.java

import java.io.File
import java.lang.System.out

import scala.io.Source
import scala.xml.{Node, PrettyPrinter}

/**
 * Main entry point for Java Reader.
 */
object Main
{
  def main (args :Array[String]) {
    if (args.length < 1) {
      die("Usage: coreen.java.Main project_root_dir [src_dir ...] [file ...]")
    }

    // partition our arguments into source directories and files
    val root = new File(args(0))
    val (filters, files) = args drop(1) partition(p => new File(root, p).isDirectory)

    // set up a filter function if any path prefixes were provided
    val filter = if (filters.isEmpty) (path :String) => true
                 else (path :String) => path.startsWith(args(0)) && {
                   val relpath = stripFS(path.substring(args(0).length))
                   filters exists(pre => relpath.startsWith(pre) || pre.startsWith(relpath))
                 }

    // this will scan the fill project directory for source and jar files, but we want to avoid
    // doing that unless we actually need the data, hence the laziness
    lazy val fmap = collectFiles(root, filter)

    // try to find our project dependencies via a pom.xml file or a .coreen file, and fall back to
    // scanning the project directories for all jar files in the absence of those
    val jars = locateJarsViaMaven(root).getOrElse(
      locateJarsViaDotCoreen(root).getOrElse(fmap.getOrElse("jar", List())))
    out.println("Using classpath: " + jars.mkString(File.pathSeparator))

    // if we were supplied source files on the command line, use those, otherwise use the files
    // from the project directory scan
    val sources = if (!files.isEmpty) files.map(new File(root, _)).toList
                  else fmap.get("java").getOrElse(Nil)

    // allow pretty printed output for debugging
    val print = if (java.lang.Boolean.getBoolean("pretty")) {
      val pp = new PrettyPrinter(999, 2)
      (e :Node) => out.println(pp.format(e))
    } else {
      (e :Node) => out.println(e)
    }

    if (sources.isEmpty) {
      out.println("Found no .java files in " + root)
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
        // the first line of output that does not start with '[' is the classpath
        Source.fromInputStream(p.getInputStream).getLines.find(!_.startsWith("[")).map(toFiles)
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

  def collectFiles (file :File, filter :(String => Boolean)) :Map[String,List[File]] = {
    def suffix (name :String) = name.substring(name.lastIndexOf(".")+1)
    def collect (file :File) :List[(String,File)] = {
      if (file.isDirectory) {
        if (filter(file.getPath)) file.listFiles.toList flatMap(collect)
        else List()
      } else suffix(file.getName) match {
        case "java" => List(("java", file.getCanonicalFile))
        case "jar" => List(("jar", file.getCanonicalFile))
        case _ => List()
      }
    }
    collect(file) groupBy(_._1) mapValues(_.map(_._2) distinct)
  }

  def stripFS (path :String) = if (path.startsWith(File.separator)) path.substring(1) else path

  private def die (msg :String) = {
    System.err.println(msg)
    System.exit(255)
    error("Not reached")
  }
}
