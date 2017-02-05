scalaVersion := "2.12.1"

val scalazVersion = "7.2.8"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion
)

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"