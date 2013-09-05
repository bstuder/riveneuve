import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "riveneuve-calendar"
    val appVersion      = "2.0"

    val appDependencies = Seq(
      "org.postgresql" % "postgresql" % "9.2-1003-jdbc4"
      ,"com.roundeights" % "hasher" % "0.3" //from "http://cloud.github.com/downloads/Nycto/Hasher/hasher_2.9.1-0.3.jar"
      ,"org.apache.poi" % "poi" % "3.9"
      ,"org.apache.poi" % "poi-ooxml" % "3.9"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here      

    )

}
