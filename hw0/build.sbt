lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.unicss",
      scalaVersion := "2.13.3"
    )),
    name := "hw0-cse271"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
