package src.test.scala.unitTypeTests

import edu.cmu.cs.ls.keymaerax.btactics.TacticTestBase
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser.{KeYmaeraXProblemParser, KeYmaeraXProblemParserResult, ParseException}

/**
  * Created by vincenth on 01/05/16.
  */
class UnitTypeCheckerTests extends TacticTestBase {
  "UnitChecker" should "check simple unit stuff!" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : m.
        |End.
        |Problem.
        | x = y
        |End.
      """.stripMargin
    val (result, KeYmaeraXProblemParserResult(map, units)) = KeYmaeraXProblemParser(input)
    result shouldBe Equal(Variable("x"), Variable("y"))
  }

  "UnitChecker" should "catch simple mismatched units example!" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        | U n.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : n.
        |End.
        |Problem.
        | x = y
        |End.
      """.stripMargin
    val thrown = intercept[Exception] {
      val (result, whatever) = KeYmaeraXProblemParser(input)
    }
    assert(thrown.isInstanceOf[ParseException])
  }
}
