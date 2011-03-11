import sbt._

class CoreenJavaReader (info :ProjectInfo) extends DefaultProject(info) with ProguardProject {
  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  val scalaj_collection = "org.scalaj" %% "scalaj-collection" % "1.0"

  // we need to locate tools.jar (Windows/Linux) or classes.jar (Mac OS X)
  val javaHomeParent = Path.fromFile(Path.fileProperty("java.home").asFile.getParent)
  val toolsJarPath = javaHomeParent / "lib" / "tools.jar"
  val classesJarPath = javaHomeParent / "Classes" / "classes.jar"
  val javacJarPath = if (toolsJarPath.asFile.exists) toolsJarPath
                     else if (classesJarPath.asFile.exists) classesJarPath
                     else throw new Exception("Unable to find tools.jar or classes.jar")

  // if we're on Windows/Linux, we need to add tools.jar to our classpath, but on the Mac OS
  // classes.jar is already part of the classpath so we need not add it
  override def unmanagedClasspath = super.unmanagedClasspath +++ toolsJarPath

  // on the Mac we end up using classes.jar as both an input jar *and* a library jar, but we
  // mitigate the ensuing hilarity by filtering all but the javac code from classes.jar when using
  // it as an input jar; this causes Proguard to behave mostly sanely
  override def makeInJarFilter (file :String) = file match {
    case "classes.jar" => "com/sun/tools/**,com/sun/source/**"
    case _ => super.makeInJarFilter(file)
  }

  // proguard plugin configurations
  override def proguardInJars = super.proguardInJars +++ javacJarPath
  override def proguardLibraryJars = super.proguardLibraryJars +++ scalaLibraryPath
  override def proguardOptions = List(
    "-dontnote scala.**,sun.tools.**,sun.applet.**",
    "-keep class com.sun.tools.javac.** { *; }",
    proguardKeepMain("coreen.java.Main")
  )
}
