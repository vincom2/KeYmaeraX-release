package src.test.scala.unitTypeTests

import edu.cmu.cs.ls.keymaerax.btactics.TacticTestBase
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser.{KeYmaeraXProblemParser, KeYmaeraXProblemParserResult}
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._

/**
  * Created by vincenth on 27/04/16.
  */
class UnitTypeParserTests extends TacticTestBase {
  // val f = "1=1".asFormula // just an example
  "ProblemParser" should "parse ProgramUnits and annotations!" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        |End.
        |ProgramVariables.
        | R y.
        | R x : m.
        |End.
        |Problem.
        | x>0
        |End.
      """.stripMargin
    val (result, KeYmaeraXProblemParserResult(map, units)) = KeYmaeraXProblemParser(input)
    result shouldBe Greater(Variable("x"), Number(0))
    result match {
      case Greater(l,r) => l.sort shouldBe Real
    }

    units.get("m") match {
      case Some(m) => m shouldBe Map()
    }

    map.get("x", None) match {
      case Some(x) => x shouldBe (None, Real, UnitUnit("m"))
    }

  }
}
