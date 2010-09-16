import sbt._

class CoreenJavaReader (info :ProjectInfo) extends DefaultProject(info) {
  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  val scalaj_collection = "org.scalaj" %% "scalaj-collection" % "1.0"

  // TODO: we need to fork our tests otherwise they get javac stuff from the wrong place; we
  // might be able to only define tools.jar on the compile classpath but not the test classpath,
  // not sure if that's possible...

  // we need tools.jar in our classpath because we're building against javac bits
  override def unmanagedClasspath = super.unmanagedClasspath +++
    Path.fromFile(Path.fileProperty("java.home").asFile.getParent) / "lib" / "tools.jar"
}
