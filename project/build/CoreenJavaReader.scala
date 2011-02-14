import sbt._

class CoreenJavaReader (info :ProjectInfo) extends DefaultProject(info) with ProguardProject {
  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  val scalaj_collection = "org.scalaj" %% "scalaj-collection" % "1.0"

  // we need tools.jar in our classpath because we're building against javac bits
  val toolsJarPath = Path.fromFile(
    Path.fileProperty("java.home").asFile.getParent) / "lib" / "tools.jar"
  override def unmanagedClasspath = super.unmanagedClasspath +++ toolsJarPath

  // proguard plugin configurations
  override def proguardInJars = super.proguardInJars +++ toolsJarPath
  override def proguardLibraryJars = super.proguardLibraryJars +++ scalaLibraryPath
  override def proguardOptions = List(
    "-dontnote scala.**,sun.tools.**,sun.applet.**",
    "-keep class com.sun.tools.javac.** { *; }",
    proguardKeepMain("coreen.java.Main")
  )
}
