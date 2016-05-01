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
    units("m") shouldBe Map()
    map("x", None) shouldBe (None, Real, UnitUnit("m"))
  }

  it should "parse unit annotations with products" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        |End.
        |ProgramVariables.
        | R x : m*m.
        |End.
        |Problem.
        | x = 0
        |End.
      """.stripMargin
    val (result, KeYmaeraXProblemParserResult(map, units)) = KeYmaeraXProblemParser(input)
    result shouldBe Equal(Variable("x"), Number(0))
    map("x", None) shouldBe (None, Real, ProductUnit(Map() + ("m" -> 2)))
  }

  it should "parse unit annotations with quotients" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        |End.
        |ProgramVariables.
        | R x : m/m.
        |End.
        |Problem.
        | x = 0
        |End.
      """.stripMargin
    val (result, KeYmaeraXProblemParserResult(map, units)) = KeYmaeraXProblemParser(input)
    result shouldBe Equal(Variable("x"), Number(0))
    map("x", None) shouldBe (None, Real, ProductUnit(Map() + ("m" -> 0)))
  }

  it should "parse unit annotations with exponents" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        |End.
        |ProgramVariables.
        | R x : m^2.
        |End.
        |Problem.
        | x = 0
        |End.
      """.stripMargin
    val (result, KeYmaeraXProblemParserResult(map, units)) = KeYmaeraXProblemParser(input)
    result shouldBe Equal(Variable("x"), Number(0))
    map("x", None) shouldBe (None, Real, ProductUnit(Map() + ("m" -> 2)))
  }
}
