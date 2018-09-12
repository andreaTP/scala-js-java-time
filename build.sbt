
enablePlugins(ScalaNativePlugin)

scalaVersion := "2.11.12"

organization := "org.akka-js"

name := "scalanative-java-time"

testFrameworks += new TestFramework("utest.runner.Framework")

scalacOptions += "-target:jvm-1.8"

libraryDependencies ++= Seq(
  "com.lihaoyi" %%% "utest" % "0.6.3" % "test"
)

nativeLinkStubs := true
