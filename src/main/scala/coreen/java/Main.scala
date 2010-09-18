//
// $Id$

package coreen.java

import java.io.{File => JFile, FileWriter, BufferedWriter}
import scala.tools.nsc.io.{Path, Directory, File}
import scala.tools.nsc.io.Path._

/**
 * Main entry point for Java Reader.
 */
object Main
{
  def main (args :Array[String]) {
    // TODO: fancy args processing
    if (args.length < 2 || args(0) != "--out") {
      die("Usage: coreen.java.Main --out outdir file [file ...]")
    }

    val out = Directory(args(1)).toAbsolute
    if (!out.isDirectory) die(out + " is not a directory?")

    Reader.process(args drop(2) map(new JFile(_))) foreach { cu =>
      val path = Path((cu \ "@src") toString).normalize
      val opath = out / (if (path.isAbsolute) stripPrefix(sharedPrefix(path, out), path) else path)
      opath.parent.createDirectory()
      opath.toFile writeAll(cu.toString)
    }
  }

  private[java] def sharedPrefix (p1 :Path, p2 :Path) = {
    val p1c = p1.toString.split(JFile.separator)
    val p2c = p2.toString.split(JFile.separator)
    Path(p1c zip(p2c) takeWhile(t => t._1 == t._2) map(_._1) mkString(JFile.separator))
  }

  private def stripPrefix (prefix :Path, from :Path) =
    Path(from.toString.substring(prefix.toString.length))

  private def die (msg :String) = {
    System.err.println(msg)
    System.exit(255)
    error("Not reached")
  }

  private def write (text :String, out :JFile) {
    val os = new BufferedWriter(new FileWriter(out))
    os.write(text)
    os.close
  }
}
