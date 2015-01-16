organization := "edu.umass.cs.iesl"

name := "xml_annotator"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "org.jdom" % "jdom2" % "2.0.5",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
)
