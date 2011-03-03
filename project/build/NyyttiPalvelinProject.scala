import sbt._

class NyyttiPalvelinProject(info: ProjectInfo) extends DefaultWebProject(info) with IdeaProject {
  val scalatraVersion = "2.0.0.M2"
  val scalatra = "org.scalatra" %% "scalatra" % scalatraVersion withSources()
  val servletApi = "org.mortbay.jetty" % "servlet-api" % "2.5-20081211" % "provided" withSources()
  val liftJson = "net.liftweb" %% "lift-json" % "2.2" withSources()

  // Alternatively, you could use scalatra-specs
  val scalatest = "org.scalatra" %% "scalatra-scalatest" % scalatraVersion % "test" withSources()

  // val sc = "org.scalacheck" % "scalacheck" % "1.5" withSources()

  // Pick your favorite slf4j binding
  val slf4jBinding = "ch.qos.logback" % "logback-classic" % "0.9.25" % "runtime" withSources()

  // http://groups.google.com/group/simple-build-tool/msg/1f17b43807d06cda
  override def testClasspath = super.testClasspath +++ buildCompilerJar

  val sonatypeNexusSnapshots = "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  // For Scalate
  val fuseSourceSnapshots = "FuseSource Snapshot Repository" at "http://repo.fusesource.com/nexus/content/repositories/snapshots"
}
