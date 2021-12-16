name := "scala3-rocket"

version := "0.1"

scalaVersion := "3.0.2"


val magnolia = "com.softwaremill.magnolia1_3" %% "magnolia" % "1.0.0-M5"

libraryDependencies += magnolia

libraryDependencies +=  "org.typelevel" %% "cats-core" % "2.6.1"
