lazy val baseName         = "Verschränkte-Systeme-146-219"
lazy val baseNameL        = baseName.toLowerCase
lazy val projectVersion   = "0.1.0-SNAPSHOT"

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "An art project",
  homepage            := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion        := "2.12.6",
  licenses            := Seq(gpl3),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint"),
  libraryDependencies ++= Seq(
    "de.sciss"                %% "fileutil"           % "1.1.3",
    "de.sciss"                %% "numbers"            % "0.1.5",
    "de.sciss"                %% "kollflitz"          % "0.2.2",
    "com.github.scopt"        %% "scopt"              % "3.7.0",
    "de.sciss"                %% "swingplus"          % "0.3.0",
    "de.sciss"                %% "fscape"             % "2.13.0",
    "de.sciss"                %  "neuralgas-core"     % "2.3.1"
  )
)

lazy val gpl3 = "GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt")

lazy val root = Project(id = baseNameL, base = file("."))
  .settings(commonSettings)
