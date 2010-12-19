//
// $Id$

package coreen.java

import java.io.File
import java.lang.System.out

import scala.io.Source
import scala.xml.PrettyPrinter

/**
 * Main entry point for Java Reader.
 */
object Main
{
  def main (args :Array[String]) {
    if (args.length < 1) {
      die("Usage: coreen.java.Main project_root_dir [include_prefix ...]")
    }

    // set up a filter function if any path prefixes were provided
    val filters = args drop(1)
    val filter = if (filters.isEmpty) (path :String) => true
                 else (path :String) => path.startsWith(args(0)) && {
                   val relpath = stripFS(path.substring(args(0).length))
                   filters exists(pre => relpath.startsWith(pre) || pre.startsWith(relpath))
                 }

    // for now we just process every single java file we can find and pass every single jar file
    // we can find on the classpath to the compiler when doing so; in the future we'll be smart
    // and try to read Eclipse and other project files, and probably support a metadata format
    // of our own
    val root = new File(args(0))
    val files = collectFiles(root, filter)
    val pp = new PrettyPrinter(999, 2)
    val jars = locateJarsViaMaven(root).getOrElse(
      locateJarsViaDotCoreen(root).getOrElse(files.getOrElse("jar", List())))
    files get("java") match {
      case Some(javas) => {
        out.println("Compiling " + javas.size + " Java source files...")
        Reader.process(javas, jars) foreach(e => out.println(e))
      }
      case None => out.println("Found no .java files in " + root)
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
