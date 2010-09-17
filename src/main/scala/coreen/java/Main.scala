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
    Reader.process(args.map(new File(_))) foreach { cu =>
      println(cu)
    }
  }
}
