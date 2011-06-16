//
// $Id$

package coreen.java

import scala.collection.mutable.{Set => MSet}

import java.io.File

object Collector {
  /**
   * Finds and collects source and library files in a directory tree. Is robust in the face of
   * symlinks. Automatically prunes common version control directories.
   *
   * @param root the root path at which to scan for files.
   * @param srcPrefs the prefixes for the source directories (relative to the root). Source (but
   * not library) files not starting with these prefixes are omitted.
   * @param libSuffs the suffixes that identify library files (which will be sought in the entire
   * directory tree, even if they are outside `srcPrefs` subtrees).
   *
   * @return a mapping from file suffix to a list of files found with that suffix.
   */
  def collect (root :File, srcPrefs :Seq[String], libSuffs :Set[String]) :Map[String,Seq[File]] =
    new Collector(root, srcPrefs, libSuffs).collect
}

private class Collector (relRoot :File, srcPrefixes :Seq[String], libSuffixes :Set[String])
{
  val root = relRoot.getCanonicalFile
  val rootPath = root.getPath

  val ignoreDirs = Set(".git", ".svn", ".hg")

  def suffix (name :String) = name.substring(name.lastIndexOf(".")+1)

  def shouldAdd (file :File) = srcPrefixes.isEmpty || libSuffixes(suffix(file.getPath)) || {
    def stripFS (path :String) = if (path.startsWith(File.separator)) path.substring(1) else path
    val relpath = stripFS(file.getPath.substring(rootPath.length))
    srcPrefixes exists(pre => relpath.startsWith(pre) || pre.startsWith(relpath))
  }

  def collect :Map[String,Seq[File]] = {
    val seenDirs = MSet[File]()
    val files = MSet[File]()
    def collect (file :File) {
      val canon = file.getCanonicalFile
      // ignore files/dirs that are symlinks outside the project root
      if (canon.getPath.startsWith(rootPath)) {
        if (canon.isDirectory) {
          if (!seenDirs(canon) && !ignoreDirs(canon.getName)) {
            seenDirs += canon
            canon.listFiles foreach(collect)
          }
        } else {
          if (shouldAdd(canon)) files += canon
        }
      }
    }
    collect(root)

    files map(f => (suffix(f.getName), f)) groupBy(_._1) mapValues(_.map(_._2).toSeq)
  }
}
