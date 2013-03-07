name := "eithervalidation"

version := "1.0.0"

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.9.0-1", "2.9.1", "2.9.2", "2.10.0")

libraryDependencies <++= scalaVersion(specs2Dependencies(_))

libraryDependencies <++= scalaVersion(scalaCompilerDependency(_))