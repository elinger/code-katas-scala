name := "code-katas-scala"

version := "0.1"

scalaVersion := "2.13.0"

lazy val commonSettings = Seq(
  scalaVersion := "2.13.1",
  libraryDependencies += "org.scalactic"    %% "scalactic" % "3.0.8",
  libraryDependencies += "org.scalatest"    %% "scalatest" % "3.0.8" % "test",
  libraryDependencies += "com.google.guava" % "guava"      % "29.0-jre",
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

lazy val `magic-forest` = (project in file("magic-forest"))
  .settings(commonSettings)
  .enablePlugins(GraalVMNativeImagePlugin)

lazy val `conways-game-of-life` = (project in file("conways-game-of-life"))
  .settings(commonSettings)
  .settings(
    Seq(
      libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"
    )
  )
  .enablePlugins(GraalVMNativeImagePlugin)

lazy val `i-before-e-except-after-c` = (project in file("i-before-e-except-after-c"))
  .settings(commonSettings)

lazy val `abc-blocks` = (project in file("abc-blocks"))
  .settings(commonSettings)

lazy val `marbles` = (project in file("marbles"))
  .settings(commonSettings)
