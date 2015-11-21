crossPaths := false

scalaVersion := "2.11.6"
ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += Resolver.sonatypeRepo("public")

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.3.8"
libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.0.0"
libraryDependencies += "org.mongodb" %% "casbah" % "2.8.2"
libraryDependencies += "org.kamranzafar" % "jtar" % "2.3"
libraryDependencies += "danburkert" % "continuum_2.11" % "0.4-SNAPSHOT"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.13"

assemblyJarName in assembly := "Cuando.jar"
test in assembly := {}
