ThisBuild / scalaVersion := "3.3.0"

lazy val utils = project in file("utils")

lazy val `advent-2023` = (project in file("advent-2023"))
  .dependsOn(utils)