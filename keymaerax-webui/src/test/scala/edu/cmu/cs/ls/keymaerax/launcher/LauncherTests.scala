package edu.cmu.cs.ls.keymaerax.launcher

import java.io.File

import org.scalatest.{BeforeAndAfterEach, Matchers, FlatSpec}

class LauncherTests extends FlatSpec with Matchers with BeforeAndAfterEach {

  "Launcher" should "prove the bouncing ball from command line" in {
    val inputFileName = "keymaerax-webui/src/test/resources/examples/simple/bouncing-ball/bouncing-ball-tout.key"
    val tacticFileName = "keymaerax-webui/src/test/resources/examples/simple/bouncing-ball/BouncingBallTacticGenerator.scala"
    val outputFileName = File.createTempFile("bouncing-ball-tout", ".kyx.proof").getAbsolutePath

    KeYmaeraX.main(Array("-prove", inputFileName, "-tactic", tacticFileName, "-out", outputFileName))

    val actualFileContent = scala.io.Source.fromFile(outputFileName).mkString
    val expectedProof = scala.io.Source.fromFile("keymaerax-webui/src/test/resources/examples/simple/bouncing-ball/bouncing-ball-tout.kyx.proof").mkString
    //@note actual file content contains evidence comments: temporary file name in comments changes on every test run
    actualFileContent should include (expectedProof)
  }

}