// import com.typesafe.sbt.packager.linux.LinuxPackageMapping

lazy val baseName         = "Verschraenkte-PlayVid"
lazy val baseNameL        = baseName.toLowerCase
lazy val projectVersion   = "0.1.0"

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "An algorithmic art project",
  scalaVersion        := "2.12.6",
  licenses            := Seq(gpl2),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint:-stars-align,_"),
  libraryDependencies ++= Seq(
    "de.sciss"               %% "fileutil"     % "1.1.3",
    "com.github.scopt"       %% "scopt"        % "3.7.0"
  ),
  target in assembly := baseDirectory.value,
)

lazy val gpl2 = "GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt")

lazy val root = project.withId(baseNameL).in(file("."))
  .settings(commonSettings)

// -------------
 
mainClass in assembly := Some("de.sciss.anemone.verschraenkte.PlayVid")

assemblyJarName in assembly := s"$baseNameL.jar"

