//
// $Id$

package coreen.java

import scala.tools.nsc.io.{Path, Directory, File}
import scala.tools.nsc.io.Path._

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * Tests some bits in from Main.
 */
class MainSpec extends FlatSpec with ShouldMatchers
{
  "stripSharedPrefix" should "handle partial matches" in {
    val p1 = "/foo/bar/baz"
    val p2 = "/foo/bar/boozle/bong"
    Main.sharedPrefix(p1, p2) should equal(Path("/foo/bar"))
  }

  "stripSharedPrefix" should "handle full matches in first arg" in {
    val p1 = "/foo/bar"
    val p2 = "/foo/bar/baz"
    Main.sharedPrefix(p1, p2) should equal(Path("/foo/bar"))
  }

  "stripSharedPrefix" should "handle full matches in second arg" in {
    val p1 = "/foo/bar/baz"
    val p2 = "/foo/bar"
    Main.sharedPrefix(p1, p2) should equal(Path("/foo/bar"))
  }
}
