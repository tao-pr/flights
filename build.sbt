name         := "flights"
organization := "com.starcolon"
description  := "Travelling made easy with flights analysis"
homepage     := Some(url("https://github.com/starcolon/flights"))

version := "0.0.1-SNAPSHOT"

scalaVersion   := "2.11.8"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint")

libraryDependencies += "com.nrinaudo" %% "kantan.csv" % "0.1.9"

/**
 * Automated source formatting upon compile, for consistency and focused code
 * reviews.
 */
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)
