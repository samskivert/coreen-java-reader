//
// $Id$

package coreen.java

import java.io.File

/**
 * Main entry point for Java Reader.
 */
object Main
{
  def main (args :Array[String]) {
    if (args.length != 1) {
      die("Usage: coreen.java.Main project_root_dir")
    }

    // for now we just process every single java file we can find and pass every single jar file
    // we can find on the classpath to the compiler when doing so; in the future we'll be smart
    // and try to read Eclipse and other project files, and probably support a metadata format
    // of our own
    val root = new File(args(0))
    val files = collectFiles(root)
    files get("java") match {
      case Some(javas) => Reader.process(javas, files getOrElse("jar", List())) foreach(println)
      case None => println("Found no .java files in " + root)
    }
  }

  def collectFiles (file :File) :Map[String,List[File]] = {
    def suffix (name :String) = name.substring(name.lastIndexOf(".")+1)
    def collect (file :File) :List[(String,File)] = {
      if (file.isDirectory) file.listFiles.toList flatMap(collect)
      else suffix(file.getName) match {
        case "java" => List(("java", file.getCanonicalFile))
        case "jar" => List(("jar", file.getCanonicalFile))
        case _ => List()
      }
    }
    collect(file) groupBy(_._1) mapValues(_.map(_._2) distinct)
  }

  private def die (msg :String) = {
    System.err.println(msg)
    System.exit(255)
    error("Not reached")
  }
}
