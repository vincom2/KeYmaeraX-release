package src.test.scala.unitTypeTests

import edu.cmu.cs.ls.keymaerax.btactics.TacticTestBase
import edu.cmu.cs.ls.keymaerax.core.{Greater, Real, Sort, UnitOfMeasure}
import edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXProblemParser
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
/**
  * Created by vincom2 on 27/04/16.
  */
class UnitTypeParserTests extends TacticTestBase {
  val f = "1=1".asFormula

  "Parser" should "parse ProgramUnits!" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        |End.
        |ProgramVariables.
        | R x.
        |End.
        |Problem.
        | x>0
        |End.
      """.stripMargin
    val (result, map : Map[(String, Option[Int]), (Option[Sort], Sort)]) = KeYmaeraXProblemParser(input)
    result match {
      case Greater(l,r) => l.sort shouldBe Real
    }

    map.get("m", None) match {
      case Some(m) => m shouldBe (None, UnitOfMeasure)
    }
  }
}
