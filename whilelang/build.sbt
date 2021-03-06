lazy val commonSettings = Seq(
  organization := "edu.berkeley",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "whilelang"
  )
