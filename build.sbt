name         := "flights"
organization := "com.starcolon"
description  := "Travelling made easy with flights analysis"
homepage     := Some(url("https://github.com/starcolon/flights"))

version := "0.0.1-SNAPSHOT"

scalaVersion   := "2.11.8"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint")

libraryDependencies ++= Seq(
  "com.nrinaudo"        %% "kantan.csv" % "0.1.9",
  "com.typesafe"        % "config" % "1.3.0",
  "org.slf4j"           % "slf4j-nop" % "1.6.4",
  "com.typesafe.slick"  %% "slick" % "3.1.1",
  "com.h2database"      % "h2" % "1.4.192",
  "net.liftweb"         %% "lift-json" % "2.6"
)

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
