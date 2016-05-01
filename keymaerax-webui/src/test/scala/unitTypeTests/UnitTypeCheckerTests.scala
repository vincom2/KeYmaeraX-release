package src.test.scala.unitTypeTests

import edu.cmu.cs.ls.keymaerax.btactics.TacticTestBase
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser.{KeYmaeraXProblemParser, KeYmaeraXProblemParserResult, ParseException}

/**
  * Created by vincenth on 01/05/16.
  */
class UnitTypeCheckerTests extends TacticTestBase {
  "Unit checker" should "check simple unit stuff!" in {
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

  it should "catch simple mismatched units example!" in {
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
    intercept[ParseException] {
      val (result, whatever) = KeYmaeraXProblemParser(input)
    }
  }

  it should "allow products of actual units" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        | U n.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : n.
        | R z : m.
        | R w : n.
        |End.
        |Problem.
        | x * y <= w * z
        |End.
      """.stripMargin
    val (result, _) = KeYmaeraXProblemParser(input)
    result shouldBe LessEqual(Times(Variable("x"), Variable("y")), Times(Variable("w"), Variable("z")))
  }

  it should "disallow improper products of actual units" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        | U n.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : n.
        | R z : m.
        | R w : n.
        |End.
        |Problem.
        | x * y = z * z
        |End.
      """.stripMargin
    intercept[ParseException] {
      val (_, _) = KeYmaeraXProblemParser(input)
    }
  }

  it should "allow quotients of actual units" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        | U n.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : n.
        | R z : m.
        | R w : n.
        |End.
        |Problem.
        | x / y = z / w
        |End.
      """.stripMargin
    val (result, _) = KeYmaeraXProblemParser(input)
    result shouldBe Equal(Divide(Variable("x"), Variable("y")), Divide(Variable("z"), Variable("w")))
  }

  it should "disallow improper quotients of actual units" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        | U n.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : n.
        | R z : m.
        | R w : n.
        |End.
        |Problem.
        | x / y = w / z
        |End.
      """.stripMargin
    intercept[ParseException] {
      val (_, _) = KeYmaeraXProblemParser(input)
    }
  }

  it should "allow equality of divided-out units" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        | U n.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : m.
        | R n : n.
        |End.
        |Problem.
        | x != (y*n)/n
        |End.
      """.stripMargin
    val (result, _) = KeYmaeraXProblemParser(input)
    result shouldBe NotEqual(Variable("x"), Divide(Times(Variable("y"), Variable("n")), Variable("n")))
  }

  it should "disallow equality of improperly divided-out units" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        | U n.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : m.
        | R n : n.
        |End.
        |Problem.
        | x != (y*x)/n
        |End.
      """.stripMargin
    intercept[ParseException] {
      val (result, _) = KeYmaeraXProblemParser(input)
    }
  }

  it should "pass a program with correct units" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        | U n.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : n.
        | R m : m.
        | R n : n.
        |End.
        |Problem.
        | <x:=*; y:=*; m:=*; n:=*; ?(x*y = n*m);>(x>=m)
        |End.
      """.stripMargin
    val (_, _) = KeYmaeraXProblemParser(input)
  }

  it should "fail a program with incorrect units" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        | U n.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : n.
        | R m : m.
        | R n : n.
        |End.
        |Problem.
        | [x:=*; y:=*; m:=*; n:=*; ?(x*m = n*y);](x>=m)
        |End.
      """.stripMargin
    intercept[ParseException] {
      val (_, _) = KeYmaeraXProblemParser(input)
    }
  }

  it should "allow differential assignment of a quantity with correct units to a" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : m.
        | R t : s.
        |End.
        |Problem.
        | [y:=*; t := 1; x' := y/t;]x=x
        |End.
      """.stripMargin
    val (_, _) = KeYmaeraXProblemParser(input)
  }

  it should "prohibit differential assignment with incorrect units" in {
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
        | <x:=*; y:=*; x' := y;>x=x
        |End.
      """.stripMargin
    intercept[ParseException] {
      val (_, _) = KeYmaeraXProblemParser(input)
    }
  }

  it should "allow a differential equation with correct units without evolution domain constraint" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : m.
        | R t : s.
        |End.
        |Problem.
        | [{ x' = y/t }]false
        |End.
      """.stripMargin
    val (_, _) = KeYmaeraXProblemParser(input)
  }

  it should "allow a differential equation with evolution domain constraint with correct units" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : m.
        | R t : s.
        |End.
        |Problem.
        | [{ x' = y/t & t<1 }]false
        |End.
      """.stripMargin
    val (_, _) = KeYmaeraXProblemParser(input)
  }

  it should "prohibit a differential equation with (evolution domain constraint with incorrect units)" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : m.
        | R t : s.
        |End.
        |Problem.
        | [{ x' = y/t & t<y }]false
        |End.
      """.stripMargin
    intercept[ParseException] {
      val (_, _) = KeYmaeraXProblemParser(input)
    }
  }

  it should "prohibit a differential equation with incorrect units without evolution domain constraint" in {
    val input =
      """
        |ProgramUnits.
        | U m.
        |End.
        |ProgramVariables.
        | R x : m.
        | R y : m.
        | R t : s.
        |End.
        |Problem.
        | [{ x' = y }]false
        |End.
      """.stripMargin
    intercept[ParseException] {
      val (_, _) = KeYmaeraXProblemParser(input)
    }
  }

}
