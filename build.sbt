
enablePlugins(ScalaNativePlugin)

scalaVersion := "2.11.12"

organization := "org.akka-js"

name := "scalanative-java-time"

testFrameworks += new TestFramework("utest.runner.Framework")

scalacOptions += "-target:jvm-1.8"

libraryDependencies ++= Seq(
  "com.lihaoyi" %%% "utest" % "0.6.3" % "test"
)
    
// lazy val testSuite = crossProject(JSPlatform, JVMPlatform).
//   jsConfigure(_ .enablePlugins(ScalaJSJUnitPlugin)).
//   settings(commonSettings: _*).
//   settings(
//     testOptions +=
//       Tests.Argument(TestFramework("com.novocode.junit.JUnitFramework"), "-v", "-a"),
//     scalacOptions += "-target:jvm-1.8"
//   ).
//   jsSettings(
//     name := "java.time testSuite on JS"
//   ).
//   jsConfigure(_.dependsOn(root)).
//   jvmSettings(
//     name := "java.time testSuite on JVM",
//     libraryDependencies +=
//       "com.novocode" % "junit-interface" % "0.9" % "test"
//   )

// lazy val testSuiteJS = testSuite.js
// lazy val testSuiteJVM = testSuite.jvm
