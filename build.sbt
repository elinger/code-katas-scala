name := "code-katas-scala"

version := "0.1"

scalaVersion := "2.13.0"

lazy val commonSettings = Seq(
  scalaVersion := "2.13.1",
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

lazy val `word-chain` = (project in file("word-chain"))
  .settings(commonSettings)
  .enablePlugins(GraalVMNativeImagePlugin)

lazy val `the-knight-tour` = (project in file("the-knight-tour"))
  .settings(commonSettings)
  .enablePlugins(GraalVMNativeImagePlugin)

lazy val `roman-numerals` = (project in file("roman-numerals"))
  .settings(commonSettings)
  .enablePlugins(GraalVMNativeImagePlugin)
