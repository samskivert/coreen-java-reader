import sbt._

class CoreenJavaReader (info :ProjectInfo) extends DefaultProject(info) with ProguardProject {
  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  val scalaj_collection = "org.scalaj" %% "scalaj-collection" % "1.0"

  // we need tools.jar in our classpath because we're building against javac bits
  val toolsJarPath = Path.fromFile(
    Path.fileProperty("java.home").asFile.getParent) / "lib" / "tools.jar"
  override def unmanagedClasspath = super.unmanagedClasspath +++ toolsJarPath

  // we need some crap from scala-compiler.jar; include it in our classpath
  override def filterScalaJars = false
  // and include it in our proguard configuration (with some filters)
  val scalaCompilerJar = new java.io.File(scalaLibraryJar.getParent, "scala-compiler.jar")
  val scalaCompilerJarPath = Path.fromFile(scalaCompilerJar)
  override def makeInJarFilter (path :String) = path match {
    case "scala-compiler.jar" => super.makeInJarFilter(path) + ",scala/tools/nsc/**.class"
    case _ => super.makeInJarFilter(path)
  }

  // proguard plugin configurations
  override def proguardInJars = super.proguardInJars +++ scalaLibraryPath +++ scalaCompilerJarPath
  override def proguardLibraryJars = super.proguardLibraryJars +++ toolsJarPath
  override def proguardOptions = List(
    "-dontnote scala.**,sun.tools.**,sun.applet.**",
    proguardKeepMain("coreen.java.Main")
  )
}
