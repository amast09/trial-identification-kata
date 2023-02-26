moduleName := "trial-identification-kata"
name := "Trial Identification Kata"
version := "0.0.1"
scalaVersion := "2.13.10"
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Ywarn-value-discard",
  "-Wconf:cat=unchecked:error",
  "-Xfatal-warnings"
)
libraryDependencies ++= Seq(
  "com.nrinaudo" %% "kantan.csv" % "0.7.0",
  "org.typelevel" %% "cats-core" % "2.9.0",
  "org.scalameta" %% "munit" % "0.7.29" % Test,
  "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
)
