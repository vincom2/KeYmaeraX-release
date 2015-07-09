/**
* Copyright (c) Carnegie Mellon University. CONFIDENTIAL
* See LICENSE.txt for the conditions of this license.
*/
import org.scalatest._
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser._
import scala.collection.immutable._
import testHelper.StringConverter._

class ParserParenTests extends FlatSpec with Matchers {
  // type declaration header for tests
  def makeInput(program : String) : String = {
    "Functions. B a. B b. B c. End." +
    "ProgramVariables. R p. R q. R r. R s. R r_0. End." +
    "Problem." + program + "\nEnd."
  }

  val parser = new KeYmaeraParser(false) 
  val alpParser = parser.ProofFileParser
  val x = Variable("x", None, Real)
  val y = Variable("y", None, Real)

  "The Parser" should "place implicit parens correctly (a.k.a. resolve abiguities correctly)" in {
    val equalPairs =
      // unary operator binds stronger than binary operator
      ("! p > 0 & p < 5", "(!(p>0)) & (p<5)") ::
      ("! p = 0 & p = 5", "(!(p=0)) & (p=5)") ::
      ("! p > 0 | p < 5", "(!(p>0)) | (p<5)") ::
      ("! p > 0 -> p > 5", "(!(p>0)) -> (p>5)") ::
      ("! p > 0 <-> p > 5", "(!(p>0)) <-> (p>5)") ::
      // quantifiers do not bind logical connectives but do bind inequalities.
      ("! \\forall x . x > 0 | p < 5", "(!(\\forall x . x>0)) | (p<5)") ::
      ("! \\exists x . x > 0 | p < 5", "(!(\\exists x . x>0)) | (p<5)") ::
      ("! \\forall x . [p:=x;]p >= x | p < 5", "(!(\\forall x . ([p:=x;](p>=x)))) | (p<5)") ::
      // quantifiers with multiple variables
      ("\\forall x, y . (y > x -> y > x)", "\\forall x, y . (y > x -> y > x)") ::
      ("\\exists y, x . (y > x -> y > x)", "\\exists y, x . (y > x -> y > x)") ::
      // modalities do not bind logical connectives.
      ("[p:=1;] p>0 & p < 1", "([p:=1;](p>0)) & (p<1)") ::
      ("[p:=1;] p>0 | p < 1", "([p:=1;](p>0)) | (p<1)") ::
      ("[p:=1;] p>0 -> p < 1", "([p:=1;](p>0)) -> (p<1)") ::
      ("<p:=1;> p>0 & p < 1", "(<p:=1;>(p>0)) & (p<1)") ::
      ("<p:=1;> p>0 | p < 1", "(<p:=1;>(p>0)) | (p<1)") ::
      ("<p:=1;> p>0 -> p < 1", "(<p:=1;>(p>0)) -> (p<1)") ::
      ("\\forall x . x > 2 & a", "(\\forall x . (x > 2)) & a") ::
      ("\\forall x . x > 2 | a", "(\\forall x . (x > 2)) | a") ::
      ("\\forall x . x > 2 -> a", "(\\forall x . (x > 2)) -> a") ::
      ("\\forall x . x > 2 <-> a", "(\\forall x . (x > 2)) <-> a") ::
      ("\\exists x . x > 2 & a", "(\\exists x . (x > 2)) & a") ::
      ("\\exists x . x > 2 | a", "(\\exists x . (x > 2)) | a") ::
      ("\\exists x . x > 2 -> a", "(\\exists x . (x > 2)) -> a") ::
      ("\\exists x . x > 2 <-> a", "(\\exists x . (x > 2)) <-> a") ::
      //nested modalities
      ("< p:=1; > <p:=2; > p>0", "<p:=1;>(<p:=2;>p>0)") ::
      ("[ p:=1; ] <p:=2; > p>0", "[p:=1;](<p:=2;>p>0)") ::
      ("< p:=1; > [p:=2; ] p>0", "<p:=1;>([p:=2;]p>0)") ::
      //[], <>, \forall, \exists magic.
      ("\\forall x . [x:=1;]<x:=2;>x>0","\\forall x . ([x:=1;]<x:=2;>(x>0))") ::
      ("\\exists x . [x:=1;]<x:=2;>x>0","\\exists x . ([x:=1;]<x:=2;>(x>0))") ::
      ("[p:=0;]\\forall x . [x:=p;] \\exists y . [q := x + y; ] q > 0", "[p:=0;](\\forall  x . [x:=p;] (\\exists y . [q := x + y; ] q > 0))") ::
      // <> vs >.
      ("< ?p>q; > p > 1", "<?(p > q);>(p>1)") ::
      ("[ ?p>q; ] p > 1", "[?(p > q);](p>1)") ::
      ("< ?a; ++ ?a; > a", "< {?a;} ++ {?a;} > a") ::
      //arith.
      ("p + q * r = s", "p + (q * r) = s") ::
      ("p * q + r = s", "(p * q) + r = s") ::
      ("p - q * r = s", "p - (q * r) = s") ::
      ("p * q - r = s", "(p * q) - r = s") ::
      ("-p + q = s", "(-p) + q = s") ::
      ("p - q - s = 0", "(p-q) - s = 0") ::
      ("p^2 >= 0", "(p^2) >= 0") ::
      ("p^2 + q^2 = s^2", "(p^2) + (q^2) = (s^2)") ::
      ("p^5 * p^3 * q^2 >= 0", "(p^5) * (p^3) * (q^2) >= 0") ::
      ("1^2 + 3^2 = s^2", "(1^2) + (3^2) = (s^2)") ::
      ("p^5 * p^3 * q^2 >= 0", "(p^5) * (p^3) * (q^2) >= 0")::
      // implicit {} either assumed correctly or rejected
      ("[ p:=1; p:=2; ++ p:=3] p>0", "[ {p:=1; p:=2;} ++ p:=3] p>0") ::
      ("[ p:=1; ++ p:=2; p:=3] p>0", "[ p:=1; ++ {p:=2; p:=3;}] p>0") ::
      ("[ p:=1; p:=2; p:=3*] p>0", "[ p:=1; p:=2; {{p:=3;}*}] p>0") ::
      ("[ p:=1; p:=2; ++ p:=3*] p>0", "[ {p:=1; p:=2;} ++ {{p:=3;}*}] p>0") ::
      Nil

    for(pair <- equalPairs) {
      val left : Expression = parser.runParser(makeInput(pair._1))
      val right : Expression = parser.runParser(makeInput(pair._2))
      left should be (right)
    }
  }

  it should "be the case that r_0 becomes Variable(r, Some(0), Real)" in {
    parser.runParser(makeInput("r_0 > 0")) should be (Greater(Variable("r", Some(0), Real), Number(0)))
  }

  it should "fail to parse bad input" in {
    val badInputs =
      "\\forall x . x > 2 > 3" ::
      Nil

    for(badInput <- badInputs) {
      a [Exception] should be thrownBy {
        parser.runParser(makeInput(badInput))
      }
    }
  }
  
  it should "parse all positive examples" in {
    val files =
      "abs.key" ::
      "dia.key" ::
      "ETCS-essentials.key" ::
      "ETCS-essentials-loopinv.key" ::
      "ETCS-safety.key" ::
      "forall.key" ::
      "functions.key" ::
      "jdq2.key" ::
      "passivesafety.key" ::
      "sections.key" ::
      "semicolons.key" ::
      "test.key" ::
      "unity.key" :: Nil

    for(testFile <- files) {
      val src = io.Source.fromInputStream(getClass.getResourceAsStream("/examples/dev/t/parsing/positive/" + testFile)).mkString
      withClue(testFile) {
        parser.runParser(src) //test fails on exception.
      }
    }
  }

  it should "parse predicates using functions" in {
    val src = io.Source.fromInputStream(getClass.getResourceAsStream("/examples/dev/t/parsing/positive/functions.key")).mkString
    parser.runParser(src)
  }

  it should "not parse any negative examples" in {
    val files =
      "finishparse.key" ::
      "scolon1.key" ::
      "scolon2.key" ::
      "scolon3.key" ::
      "UndeclaredVariables.key" :: Nil

    for(testFile <- files) {
      val src = io.Source.fromInputStream(getClass.getResourceAsStream("/examples/dev/t/parsing/negative/" + testFile)).mkString
      try {
        parser.runParser(src)
        fail("A negative file parsed correctly: " + testFile)
      }
      catch {
        case _ : Throwable => { } // ok
      }
    }
  }
  
  /*
   *@TODO setup pretty-printer so that it can be parsed again.
  it should "parse pretty-prints of random formulas" in {
	  val rand = RandomFormula
	  
      for(i <- 1 to 5) {
        val left : Expr = rand.nextFormula(10)
        val print : String = KeYmaeraPrettyPrinter.stringify(left)
        val right : Expr = parser.runParser(print)
        left should be (right)
      }
    }
  }
  */
  
  //////////////////////////////////////////////////////////////////////////////
  // Begin ALP Parser tests
  //////////////////////////////////////////////////////////////////////////////
  
  "The ALP Parser" should "parse all positive ALP examples" in {
    val files =
      "axioms.key.alp" ::
      "QE94.alp" ::
      "QE96.alp" :: Nil

    for(testFile <- files) {
      val src = io.Source.fromInputStream(getClass.getResourceAsStream("/examples/dev/t/parsing/positiveALP/" + testFile)).mkString
      try {
        alpParser.runParser(src) //test fails on exception.
      } catch {
        case ex: Exception => fail("Unable to parse " + testFile, ex)
      }
    }
  }
  
  it should "not parse any examples/t/negativeALP files" in {
    val files =
      "undeclared.key.alp" :: Nil

    for(testFile <- files) {
      val src = io.Source.fromInputStream(getClass.getResourceAsStream("/examples/dev/t/parsing/negativeALP/" + testFile)).mkString
      withClue(testFile) {
        a[Exception] should be thrownBy {
          alpParser.runParser(src)
        }
      }
    }
  }

  "Random test cases from development" should "reduce systems of diffEqs correctly." in {
    "[x'=y, y'=x;]true".asFormula shouldBe Box(ODESystem(DifferentialProduct(
      AtomicODE(DifferentialSymbol(x), y),
      AtomicODE(DifferentialSymbol(y), x)), True), True)
  }
}
