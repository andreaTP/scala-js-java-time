import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val root = crossProject(JVMPlatform, NativePlatform)
    .in(file("."))
    .enablePlugins(ScalaNativePlugin)
    .settings(
      organization := "org.akka-js",
      name := "scalanative-java-time",
      scalaVersion := "2.11.12",
      scalacOptions += "-target:jvm-1.8",
      nativeLinkStubs := true,
      testFrameworks += new TestFramework("utest.runner.Framework"),
      libraryDependencies += "com.lihaoyi" %%% "utest" % "0.6.3" % "test"
    )

lazy val rootJVM = root.jvm
lazy val rootNative = root.native