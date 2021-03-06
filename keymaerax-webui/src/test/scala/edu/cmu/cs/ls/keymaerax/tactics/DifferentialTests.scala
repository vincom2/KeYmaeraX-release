/**
* Copyright (c) Carnegie Mellon University.
* See LICENSE.txt for the conditions of this license.
*/

package edu.cmu.cs.ls.keymaerax.tactics

import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
import edu.cmu.cs.ls.keymaerax.tactics.TacticLibrary._
import edu.cmu.cs.ls.keymaerax.tactics._
import edu.cmu.cs.ls.keymaerax.tools.{Mathematica, KeYmaera}
import org.scalatest.{BeforeAndAfterEach, Matchers, FlatSpec}
import ODETactics.{diffSkipT, diffWeakenT, diffWeakenAxiomT, diffSolution, diamondDiffWeakenAxiomT}
import testHelper.SequentFactory._
import ProvabilityTestHelper._
import ProofFactory._
import edu.cmu.cs.ls.keymaerax.tags.ObsoleteTest


import scala.collection.immutable
import scala.collection.immutable.Map


/**
 * Created by nfulton on 12/1/14.
 * @author Nathan Fulton
 * @author Stefan Mitsch
 */
@ObsoleteTest
class DifferentialTests extends FlatSpec with Matchers with BeforeAndAfterEach {
  val helper = new ProvabilityTestHelper((x) => println(x))

  //Mathematica
  val mathematicaConfig: Map[String, String] = helper.mathematicaConfig

  override def beforeEach() = {
    Tactics.KeYmaeraScheduler = new Interpreter(KeYmaera)
    Tactics.MathematicaScheduler = new Interpreter(new Mathematica)
    Tactics.KeYmaeraScheduler.init(Map())
    Tactics.MathematicaScheduler.init(mathematicaConfig)
  }

  override def afterEach() = {
    Tactics.KeYmaeraScheduler.shutdown()
    Tactics.MathematicaScheduler.shutdown()
    Tactics.KeYmaeraScheduler = null
    Tactics.MathematicaScheduler = null
  }

  "Box differential weaken tactic" should "pull out evolution domain constraint" in {
    val s = sucSequent("[{x'=1 & x>2}]x>0".asFormula)

    val diffWeakenAx = locateSucc(diffWeakenAxiomT)
    getProofSequent(diffWeakenAx, new RootNode(s)) should be (sucSequent("[{x'=1 & x>2}](x>2 -> x>0)".asFormula))

    val diffWeaken = locateSucc(diffWeakenT)
    val result = helper.runTactic(diffWeaken, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "x>2 -> x>0".asFormula
  }

  it should "pull out evolution domain constraint in some context" in {
    val s = sucSequent("[x:=0;][{x'=1 & x>2}]x>0".asFormula)

    val diffWeakenAx = diffWeakenAxiomT(SuccPosition(0, PosInExpr(1::Nil)))
    getProofSequent(diffWeakenAx, new RootNode(s)) should be (sucSequent("[x:=0;][{x'=1 & x>2}](x>2 -> x>0)".asFormula))
  }

  it should "perform alpha renaming if necessary" in {
    val s = sucSequent("[{y'=y & y>2 & z<0}]y>0".asFormula)
    val diffWeakenAx = locateSucc(diffWeakenAxiomT)
    val result = helper.runTactic(diffWeakenAx, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[{y'=y & y>2 & z<0}](y>2 & z<0 -> y>0)".asFormula

    val diffWeaken = locateSucc(diffWeakenT)
    val result2 = helper.runTactic(diffWeaken, new RootNode(s))
    result2.openGoals() should have size 1
    result2.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result2.openGoals().flatMap(_.sequent.succ) should contain only "y>2 & z<0 -> y>0".asFormula
  }

  it should "introduce true if there is no evolution domain constraint" in {
    val s = sucSequent("[{x'=1}]x>0".asFormula)
    val diffWeakenAx = locateSucc(diffWeakenAxiomT)
    val result = helper.runTactic(diffWeakenAx, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[{x'=1}](true -> x>0)".asFormula

    val diffWeaken = locateSucc(diffWeakenT)
    val result2 = helper.runTactic(diffWeaken, new RootNode(s))
    result2.openGoals() should have size 1
    result2.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result2.openGoals().flatMap(_.sequent.succ) should contain only "true -> x>0".asFormula
  }

  "Diamond differential weaken tactic" should "pull out evolution domain constraint" in {
    val s = sucSequent("<{x'=1 & x>2}>x>0".asFormula)

    val diffWeakenAx = locateSucc(diamondDiffWeakenAxiomT)
    getProofSequent(diffWeakenAx, new RootNode(s)) should be (sucSequent("<{x'=1 & x>2}>(!(x>2 -> !x>0))".asFormula))

//    val diffWeaken = locateSucc(diffWeakenT)
//    val result = helper.runTactic(diffWeaken, new RootNode(s))
//    result.openGoals() should have size 1
//    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
//    result.openGoals().flatMap(_.sequent.succ) should contain only "x>2 -> x>0".asFormula
  }

  it should "work inside formula" in {
    val s = sucSequent("x>0 & <{x'=1 & x>2}>x>0".asFormula)

    val diffWeakenAx = diamondDiffWeakenAxiomT(SuccPosition(0, PosInExpr(1::Nil)))
    getProofSequent(diffWeakenAx, new RootNode(s)) should be (sucSequent("x>0 & <{x'=1 & x>2}>(!(x>2 -> !x>0))".asFormula))

    //    val diffWeaken = locateSucc(diffWeakenT)
    //    val result = helper.runTactic(diffWeaken, new RootNode(s))
    //    result.openGoals() should have size 1
    //    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    //    result.openGoals().flatMap(_.sequent.succ) should contain only "x>2 -> x>0".asFormula
  }

  "differential weaken of system of ODEs" should "pull out evolution domain constraint" in {
    val s = sucSequent("[{x'=x, y'=1 & y>2 & z<0}]y>0".asFormula)
    val diffWeakenAx = locateSucc(diffWeakenAxiomT)
    getProofSequent(diffWeakenAx, new RootNode(s)) should be (sucSequent("[{x'=x, y'=1 & y>2 & z<0}](y>2 & z<0 -> y>0)".asFormula))

    val diffWeaken = locateSucc(diffWeakenT)
    val result = helper.runTactic(diffWeaken, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "y>2 & z<0 -> y>0".asFormula
  }

  it should "also work when the ODEs are interdependent" in {
    val s = sucSequent("[{x'=x+y, y'=1, z'=2 & y>2 & z<0}]y>0".asFormula)
    val diffWeakenAx = locateSucc(diffWeakenAxiomT)
    getProofSequent(diffWeakenAx, new RootNode(s)) should be (
      sucSequent("[{x'=x+y, y'=1, z'=2 & y>2 & z<0}](y>2 & z<0 -> y>0)".asFormula))

    val diffWeaken = locateSucc(diffWeakenT)
    val result = helper.runTactic(diffWeaken, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "y>2 & z<0 -> y>0".asFormula
  }

  it should "also work inside formulas" in {
    val s = sucSequent("x>0 & [{x'=1 & y>2}]y>0".asFormula)
    val diffWeakenAx = diffWeakenAxiomT(SuccPosition(0, PosInExpr(1::Nil)))
    getProofSequent(diffWeakenAx, new RootNode(s)) should be (
      sucSequent("x>0 & [{x'=1 & y>2}](y>2 -> y>0)".asFormula))

    // TODO tactic with abstraction etc.
  }

  "DX differential skip" should "introduce a differential equation when asked" in {
    val s = sucSequent("x>0 -> x>=0".asFormula)
    val result = helper.runTactic(locateSucc(diffSkipT(AtomicODE(DifferentialSymbol(Variable("x")), Number(1)))), new RootNode(s))

    result.openGoals() should have size 1
    result.openGoals().head.sequent.ante shouldBe empty
    result.openGoals().head.sequent.succ should contain only "[{x'=1 & x>0}]x>=0".asFormula
  }

  it should "work with differential constants and other place holders" in {
    val s = sucSequent("H(??) -> p(??)".asFormula)
    val result = helper.runTactic(diffSkipT(DifferentialProgramConst("c"))(SuccPosition(0)), new RootNode(s))

    result.openGoals() should have size 1
    result.openGoals().head.sequent.ante shouldBe empty
    result.openGoals().head.sequent.succ should contain only "[{c & H(??)}]p(??)".asFormula
  }

  it should "work with differential constants and other place holders in context" in {
    val s = sucSequent("!(H(??) -> !p(??)) -> <{c&H(??)}>p(??)".asFormula)
    val result = helper.runTactic(diffSkipT(DifferentialProgramConst("c"))(SuccPosition(0, PosInExpr(0::0::Nil))), new RootNode(s))

    result.openGoals() should have size 1
    result.openGoals().head.sequent.ante shouldBe empty
    result.openGoals().head.sequent.succ should contain only "![{c & H(??)}]!p(??) -> <{c&H(??)}>p(??)".asFormula
  }

  "differential cut" should "cut in a simple formula" in {
    import ODETactics.diffCutT
    val s = sucSequent("[{x'=2}]x>=0".asFormula)
    val tactic = locateSucc(diffCutT("x>0".asFormula))
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 2
    result.openGoals()(0).sequent.ante shouldBe empty
    result.openGoals()(0).sequent.succ should contain only "[{x'=2 & true & x>0}]x>=0".asFormula
    result.openGoals()(0).tacticInfo.infos.get("branchLabel") shouldBe Some(BranchLabels.cutUseLbl)
    result.openGoals()(1).sequent.ante shouldBe empty
    result.openGoals()(1).sequent.succ should contain only "[{x'=2}]x>0".asFormula
    result.openGoals()(1).tacticInfo.infos.get("branchLabel") shouldBe Some(BranchLabels.cutShowLbl)
  }

  it should "retain context for showing condition" in {
    import ODETactics.diffCutT
    val s = sequent(Nil, "x>0".asFormula::Nil, "y<0".asFormula :: "[{x'=2}]x>=0".asFormula :: "z=0".asFormula :: Nil)
    val tactic = locateSucc(diffCutT("x>0".asFormula))
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 2
    result.openGoals()(0).sequent.ante should contain only "x>0".asFormula
    result.openGoals()(0).sequent.succ should contain only ("y<0".asFormula, "[{x'=2 & true & x>0}]x>=0".asFormula, "z=0".asFormula)
    result.openGoals()(0).tacticInfo.infos.get("branchLabel") shouldBe Some(BranchLabels.cutUseLbl)
    result.openGoals()(1).sequent.ante should contain only "x>0".asFormula
    result.openGoals()(1).sequent.succ should contain only ("y<0".asFormula, "[{x'=2}]x>0".asFormula, "z=0".asFormula)
    result.openGoals()(1).tacticInfo.infos.get("branchLabel") shouldBe Some(BranchLabels.cutShowLbl)
  }

  it should "cut formula into evolution domain constraint of rightmost ODE in ODEProduct" in {
    val s = sucSequent("[{x'=2, y'=3, z'=4 & y>4}]x>0".asFormula)
    val tactic = locateSucc(diffCutT("x>1".asFormula))

    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 2
    result.openGoals()(0).sequent.ante shouldBe empty
    result.openGoals()(0).sequent.succ should contain only "[{x'=2,y'=3,z'=4 & (y>4&x>1)}]x>0".asFormula
    result.openGoals()(0).tacticInfo.infos.get("branchLabel") shouldBe Some(BranchLabels.cutUseLbl)
    result.openGoals()(1).sequent.ante shouldBe empty
    result.openGoals()(1).sequent.succ should contain only "[{x'=2,y'=3,z'=4 & y>4}]x>1".asFormula
    result.openGoals()(1).tacticInfo.infos.get("branchLabel") shouldBe Some(BranchLabels.cutShowLbl)
  }

  it should "cut formula into rightmost ODE in ODEProduct, even if constraint empty" in {
    val s = sucSequent("[{x'=2, y'=3}]x>0".asFormula)
    val tactic = locateSucc(diffCutT("x>1".asFormula))

    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 2
    result.openGoals()(0).sequent.ante shouldBe empty
    result.openGoals()(0).sequent.succ should contain only "[{x'=2,y'=3 & (true&x>1)}]x>0".asFormula
    result.openGoals()(0).tacticInfo.infos.get("branchLabel") shouldBe Some(BranchLabels.cutUseLbl)
    result.openGoals()(1).sequent.ante shouldBe empty
    result.openGoals()(1).sequent.succ should contain only "[{x'=2, y'=3}]x>1".asFormula
    result.openGoals()(1).tacticInfo.infos.get("branchLabel") shouldBe Some(BranchLabels.cutShowLbl)
  }

  it should "preserve existing evolution domain constraint" in {
    import ODETactics.diffCutT
    val s = sucSequent("[{x'=2 & x>=0 | y<z}]x>=0".asFormula)
    val tactic = locateSucc(diffCutT("x>0".asFormula))
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 2
    result.openGoals()(0).sequent.ante shouldBe empty
    result.openGoals()(0).sequent.succ should contain only "[{x'=2 & (x>=0 | y<z) & x>0}]x>=0".asFormula
    result.openGoals()(0).tacticInfo.infos.get("branchLabel") shouldBe Some(BranchLabels.cutUseLbl)
    result.openGoals()(1).sequent.ante shouldBe empty
    result.openGoals()(1).sequent.succ should contain only "[{x'=2 & x>=0 | y<z}]x>0".asFormula
    result.openGoals()(1).tacticInfo.infos.get("branchLabel") shouldBe Some(BranchLabels.cutShowLbl)
  }

  it should "introduce ghosts when special function old is used" in {
    import ODETactics.diffCutT
    val s = sucSequent("[{x'=2 & x>=0 | y<z}]x>=0".asFormula)
    val tactic = locateSucc(diffCutT("x>old(x)".asFormula))
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 2
    result.openGoals()(0).sequent.ante should contain only "x0_1()=x".asFormula
    result.openGoals()(0).sequent.succ should contain only "[{x'=2 & (x>=0 | y<z) & x>x0_1()}]x>=0".asFormula
    result.openGoals()(0).tacticInfo.infos.get("branchLabel") shouldBe Some(BranchLabels.cutUseLbl)
    result.openGoals()(1).sequent.ante should contain only "x0_1()=x".asFormula
    result.openGoals()(1).sequent.succ should contain only "[{x'=2 & x>=0 | y<z}]x>x0_1()".asFormula
    result.openGoals()(1).tacticInfo.infos.get("branchLabel") shouldBe Some(BranchLabels.cutShowLbl)
  }

  "differential solution tactic" should "use Mathematica to find solution if None is provided" in {
    val s = sequent(Nil, "b=0 & x>b".asFormula :: Nil, "[{x'=2}]x>b".asFormula :: Nil)

    // solution = None -> Mathematica
    val tactic = locateSucc(diffSolution(None))

    val node = helper.runTactic(tactic, new RootNode(s))

    node.openGoals() should have size 1
    node.openGoals().flatMap(_.sequent.ante) should contain only (
      "b=0 & x>b".asFormula,
      "kxtime_1=0".asFormula,
      "x_2()=x".asFormula,
      "kxtime_4()=kxtime_1".asFormula
      )
    // specific form irrelevant for test, as long as we get an equivalent formula
    node.openGoals().flatMap(_.sequent.succ) should contain only
      "true & kxtime_5>=kxtime_4() & x_3=x_2() + 2*kxtime_5 -> x_3>b".asFormula
  }

  it should "find solutions for x'=v if None is provided" in {
    val s = sequent(Nil, "x>0 & v()>=0".asFormula :: Nil, "[{x'=v()}]x>0".asFormula :: Nil)

    // solution = None -> Mathematica
    val tactic = locateSucc(diffSolution(None)) & debugT("After Diff Solution") & locateSucc(ImplyRightT) & arithmeticT

    helper.runTactic(tactic, new RootNode(s)) shouldBe 'closed
  }

  it should "find solutions for x'=v, v'=a if None is provided" in {
    val s = sequent(Nil, "x>0 & v>=0 & a()>0".asFormula :: Nil, "[{x'=v, v'=a()}]x>0".asFormula :: Nil)

    // solution = None -> Mathematica
    val tactic = locateSucc(diffSolution(None)) & debugT("After Diff Solution") & locateSucc(ImplyRightT) & arithmeticT

    helper.runTactic(tactic, new RootNode(s)) shouldBe 'closed
  }

  it should "not introduce time if already present" in {
    val s = sequent(Nil, "x>0".asFormula :: "t=0".asFormula :: Nil, "[{x'=2, t'=1}]x>0".asFormula :: Nil)

    // solution = None -> Mathematica
    val tactic = locateSucc(diffSolution(None))
    val node = helper.runTactic(tactic, new RootNode(s))

    node.openGoals() should have size 1
    node.openGoals().flatMap(_.sequent.ante) should contain only (
      "x>0".asFormula,
      "t=0".asFormula,
      "x_2()=x".asFormula,
      "t_2()=t".asFormula
      )
    // specific form irrelevant for test, as long as we get an equivalent formula
    node.openGoals().flatMap(_.sequent.succ) should contain only "true & t_3>=t_2() & x_3=x_2() + 2*(t_3-t_2()) -> x_3>0".asFormula

    helper.runTactic(arithmeticT, node.openGoals().last) shouldBe 'closed
  }

  it should "preserve time evolution domain constraints when using Mathematica to find solution" in {
    val s = sequent(Nil, "x>0 & t=0".asFormula :: Nil, "[{x'=2, t'=1 & t<=5}]x>0".asFormula :: Nil)

    // solution = None -> Mathematica
    val tactic = locateSucc(diffSolution(None))
    val node = helper.runTactic(tactic, new RootNode(s))

    node.openGoals() should have size 1
    node.openGoals().flatMap(_.sequent.ante) should contain only (
      "x>0 & t=0".asFormula,
      "x_2()=x".asFormula,
      "t_2()=t".asFormula
      )
    // specific form irrelevant for test, as long as we get an equivalent formula
    node.openGoals().flatMap(_.sequent.succ) should contain only "t_3<=5 & t_3>=t_2() & x_3=x_2() + 2*(t_3-t_2()) -> x_3>0".asFormula

    helper.runTactic(arithmeticT, node.openGoals().last) shouldBe 'closed
  }

  it should "work with evolution domain constraints" in {
    val s = sequent(Nil, "x>0 & v>=0".asFormula :: Nil, "[{x'=v, v'=a() & v>=0}]x>0".asFormula :: Nil)

    // solution = None -> Mathematica
    val tactic = locateSucc(diffSolution(None)) & debugT("After Diff Solution") & arithmeticT

    helper.runTactic(tactic, new RootNode(s)) shouldBe 'closed
  }

  it should "use a user-defined tactic before DI" in {
    val s = sequent(Nil, "max(0,1) >= 0".asFormula :: "x>0 & v>=0 & a>0".asFormula :: Nil, "[{x'=v, v'=a}]x>0".asFormula :: Nil)

    // solution = None -> Mathematica
    val tactic = locateSucc(diffSolution(None, TactixLibrary.la(hideT, "max(0,1) >= 0")))

    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 1
    result.openGoals().head.sequent.ante should contain only ("max(0,1)>=0".asFormula, "x>0&v>=0&a>0".asFormula,
      "kxtime_1=0".asFormula, "x_2()=x".asFormula, "v_2()=v".asFormula, "kxtime_4()=kxtime_1".asFormula)
    result.openGoals().head.sequent.succ should contain only "true & kxtime_5>=kxtime_4() & v_3=v_2()+a*kxtime_5 & x_3=1/2*(2*x_2() + 2*v_2()*kxtime_5 + a*kxtime_5^2) -> x_3>0".asFormula
  }

  "Differential auxiliaries tactic" should "add y'=1 to [x'=2]x>0" in {
    import ODETactics.diffAuxiliaryT
    val s = sucSequent("[{y'=2}]y>0".asFormula)
    val tactic = locateSucc(diffAuxiliaryT(Variable("x", None, Real), "0".asTerm, "1".asTerm))
    getProofSequent(tactic, new RootNode(s)) should be (
      sucSequent("\\exists x [{y'=2,x'=0*x+1}]y>0".asFormula)
    )
  }

  it should "work without renaming (add y'=1 to [x'=2]x>0)" in {
    import ODETactics.diffAuxiliaryT
    val s = sucSequent("[{x'=2}]x>0".asFormula)
    val tactic = locateSucc(diffAuxiliaryT(Variable("y", None, Real), "0".asTerm, "1".asTerm))
    getProofSequent(tactic, new RootNode(s)) should be (
      sucSequent("\\exists y [{x'=2,y'=0*y+1}]x>0".asFormula)
    )
  }

  it should "add y'=3*y+10 to [x'=x+2*z]x>0" in {
    import ODETactics.diffAuxiliaryT
    val s = sucSequent("[{x'=2}]x>0".asFormula)
    val tactic = locateSucc(diffAuxiliaryT(Variable("y", None, Real), "3".asTerm, "10".asTerm))
    getProofSequent(tactic, new RootNode(s)) should be (
      sucSequent("\\exists y [{x'=2,y'=3*y+10}]x>0".asFormula)
    )
  }

  it should "add y'=3*y+z() to [x'=2]x>0" in {
    import ODETactics.diffAuxiliaryT
    val s = sucSequent("[{x'=2}]x>0".asFormula)
    val tactic = locateSucc(diffAuxiliaryT(Variable("y", None, Real), "3".asTerm, FuncOf(Function("z", None, Unit, Real), Nothing)))
    getProofSequent(tactic, new RootNode(s)) should be (
      sucSequent("\\exists y [{x'=2,y'=3*y+z()}]x>0".asFormula)
    )
  }

  it should "preserve evolution domain" in {
    import ODETactics.diffAuxiliaryT
    val s = sucSequent("[{x'=2 & x>=0}]x>0".asFormula)
    val tactic = locateSucc(diffAuxiliaryT(Variable("y", None, Real), "3".asTerm, "10".asTerm))
    getProofSequent(tactic, new RootNode(s)) should be (
      sucSequent("\\exists y [{x'=2,y'=3*y+10 & x>=0}]x>0".asFormula)
    )
  }

  it should "not allow non-linear ghosts (1)" in {
    import ODETactics.diffAuxiliaryT
    val s = sucSequent("[{x'=2}]x>0".asFormula)
    val tactic = locateSucc(diffAuxiliaryT(Variable("y", None, Real), "y".asTerm, "1".asTerm))
    tactic.applicable(new RootNode(s)) shouldBe false
  }

  it should "not allow non-linear ghosts (2)" in {
    import ODETactics.diffAuxiliaryT
    val s = sucSequent("[{x'=2}]x>0".asFormula)
    val tactic = locateSucc(diffAuxiliaryT(Variable("y", None, Real), "1".asTerm, "y".asTerm))
    tactic.applicable(new RootNode(s)) shouldBe false
  }

  it should "not allow ghosts that are already present in the ODE" in {
    import ODETactics.diffAuxiliaryT
    val s = sucSequent("[{x'=2}]x>0".asFormula)
    val tactic = locateSucc(diffAuxiliaryT(Variable("x", None, Real), "0".asTerm, "1".asTerm))
    tactic.applicable(new RootNode(s)) shouldBe false
  }

//  "Diamond diff solve axiom tactic" should "split a single differential equation into a branch for proving the solution and a branch for using it" in {
//    val s = sucSequent("<x'=3*a()+z() & x>5;>x>0".asFormula)
//    // provide anything as solution for testing
//    val tactic = locateSucc(ODETactics.diamondDiffSolveAxiomT(_ => "c()".asTerm))
//    val result = helper.runTactic(tactic, new RootNode(s))
//    result.openGoals() should have size 2
//    result.openGoals()(0).sequent.ante shouldBe empty
//    result.openGoals()(0).sequent.succ should contain only "c()=x & [t'=1;]c()'=3*a()+z()".asFormula
//    result.openGoals()(1).sequent.ante should contain only
//      "<x'=3*a()+z() & x>5;>x>0 <-> \\exists t. (t>=0 & (\\forall s. (0<=s&s<=t -> <x:=c();>x>5) & <x:=c();>x>0))".asFormula
//    result.openGoals()(1).sequent.succ should contain only "<x'=3*a()+z() & x>5;>x>0".asFormula
//  }
//
//  "Diamond diff solve tactic" should "introduce initial value ghosts and ask Mathematica for a solution" in {
//    val s = sucSequent("<x'=5 & x>2;>x>0".asFormula)
//    val tactic = locateSucc(ODETactics.diamondDiffSolveT)
//    val result = helper.runTactic(tactic, new RootNode(s))
//    result.openGoals() should have size 1
//    result.openGoals().flatMap(_.sequent.ante) should contain only "x_2()=x".asFormula
//    result.openGoals()flatMap(_.sequent.succ) should contain only
//      "\\exists t. (t>=0 & (\\forall s. (0<=s&s<=t -> <x:=5*s+x_2();>x>2) & <x:=5*t+x_2();>x>0))".asFormula
//  }

  "Differential auxiliaries proof rule" should "add y'=1 to [x'=2]x>0" in {
    import ODETactics.diffAuxiliariesRule
    val s = Sequent(Nil, immutable.IndexedSeq(), immutable.IndexedSeq("[{x'=2}]x>0".asFormula))
    val tactic = diffAuxiliariesRule(Variable("y"), "0".asTerm, "1".asTerm, "y>0 & x*y>0".asFormula)(1)
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 2
    result.openGoals()(0).sequent.ante should contain only "y>0 & x*y>0".asFormula
    result.openGoals()(0).sequent.succ should contain only "[{x'=2,y'=0*y+1}](y>0 & x*y>0)".asFormula
    result.openGoals()(0).tacticInfo.infos should contain ("branchLabel", "Diff. Aux. Result")
    result.openGoals()(1).sequent.ante shouldBe empty
    result.openGoals()(1).sequent.succ should contain only "x>0".asFormula
    result.openGoals()(1).tacticInfo.infos should contain ("branchLabel", "Diff. Aux. P Initially Valid")
  }

  it should "add y'=1 to [x'=2]x>0 when provided a provable explicitly" in {
    import ODETactics.diffAuxiliariesRule
    val s = Sequent(Nil, immutable.IndexedSeq(), immutable.IndexedSeq("[{x'=2}]x>0".asFormula))

    val auxEquiv = TactixLibrary.proveBy("x>0 <-> \\exists y (y>0 & x*y>0)".asFormula, TactixLibrary.QE)

    val tactic = diffAuxiliariesRule(Variable("y"), "0".asTerm, "1".asTerm, auxEquiv)(1)
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 2
    result.openGoals()(0).sequent.ante should contain only "y>0 & x*y>0".asFormula
    result.openGoals()(0).sequent.succ should contain only "[{x'=2,y'=0*y+1}](y>0 & x*y>0)".asFormula
    result.openGoals()(1).sequent.ante shouldBe empty
    result.openGoals()(1).sequent.succ should contain only "x>0".asFormula
  }

  it should "not cut in x>0 if already present in antecedent when adding y'=1 to [x'=2]x>0" in {
    import ODETactics.diffAuxiliariesRule
    val s = Sequent(Nil, immutable.IndexedSeq("x>0".asFormula), immutable.IndexedSeq("[{x'=2}]x>0".asFormula))
    val tactic = diffAuxiliariesRule(Variable("y"), "0".asTerm, "1".asTerm, "y>0 & x*y>0".asFormula)(1)
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 1
    result.openGoals().head.sequent.ante should contain only "y>0 & x*y>0".asFormula
    result.openGoals().head.sequent.succ should contain only "[{x'=2,y'=0*y+1}](y>0 & x*y>0)".asFormula
  }

  it should "not cut in x>0 if already present among other formulas in antecedent when adding y'=1 to [x'=2]x>0" in {
    import ODETactics.diffAuxiliariesRule
    val s = Sequent(Nil, immutable.IndexedSeq("a=2".asFormula, "x>0".asFormula, "b<4".asFormula), immutable.IndexedSeq("[{x'=2}]x>0".asFormula))
    val tactic = diffAuxiliariesRule(Variable("y"), "0".asTerm, "1".asTerm, "y>0 & x*y>0".asFormula)(1)
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 1
    result.openGoals().head.sequent.ante should contain only ("a=2".asFormula, "y>0 & x*y>0".asFormula, "b<4".asFormula)
    result.openGoals().head.sequent.succ should contain only "[{x'=2,y'=0*y+1}](y>0 & x*y>0)".asFormula
  }

  it should "work in a simple context" in {
    import ODETactics.diffAuxiliariesRule
    val s = Sequent(Nil, immutable.IndexedSeq("x>0".asFormula), immutable.IndexedSeq("a=2 -> [{x'=2}]x>0".asFormula))
    val tactic = diffAuxiliariesRule(Variable("y"), "0".asTerm, "1".asTerm, "y>0 & x*y>0".asFormula)(1, 1::Nil)
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 1
    result.openGoals().head.sequent.ante should contain only "y>0 & x*y>0".asFormula
    result.openGoals().head.sequent.succ should contain only "a=2 -> [{x'=2,y'=0*y+1}](y>0 & x*y>0)".asFormula
  }

  it should "when used in context, leave two branches open when P is not in antecedent" in {
    import ODETactics.diffAuxiliariesRule
    val s = Sequent(Nil, immutable.IndexedSeq(), immutable.IndexedSeq("a=2 -> [{x'=2}]x>0".asFormula))
    val tactic = diffAuxiliariesRule(Variable("y"), "0".asTerm, "1".asTerm, "y>0 & x*y>0".asFormula)(1, 1::Nil)
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 2
    result.openGoals()(0).sequent.ante should contain only "y>0 & x*y>0".asFormula
    result.openGoals()(0).sequent.succ should contain only "a=2 -> [{x'=2,y'=0*y+1}](y>0 & x*y>0)".asFormula
    result.openGoals()(0).tacticInfo.infos should contain ("branchLabel", "Diff. Aux. Result")
    result.openGoals()(1).sequent.ante shouldBe empty
    result.openGoals()(1).sequent.succ should contain only "x>0".asFormula
    result.openGoals()(1).tacticInfo.infos should contain ("branchLabel", "Diff. Aux. P Initially Valid")
  }

  it should "work in a complicated context" in {
    import ODETactics.diffAuxiliariesRule
    val s = Sequent(Nil, immutable.IndexedSeq("x>0".asFormula), immutable.IndexedSeq("a=2 -> [b:=3;]<?c=5;{c'=2}>[{x'=2}]x>0".asFormula))
    val tactic = diffAuxiliariesRule(Variable("y"), "0".asTerm, "1".asTerm, "y>0 & x*y>0".asFormula)(1, 1::1::1::Nil)
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 1
    result.openGoals().head.sequent.ante should contain only "y>0 & x*y>0".asFormula
    result.openGoals().head.sequent.succ should contain only "a=2 -> [b:=3;]<?c=5;{c'=2}>[{x'=2,y'=0*y+1}](y>0 & x*y>0)".asFormula
  }

  it should "not mess up when the context binds the differential auxiliary" in {
    //@todo should we disallow applicability? result doesn't make much sense anyway...
    import ODETactics.diffAuxiliariesRule
    val s = Sequent(Nil, immutable.IndexedSeq("x>0".asFormula), immutable.IndexedSeq("[y:=2;][{x'=2}]x>0".asFormula))
    val tactic = diffAuxiliariesRule(Variable("y"), "0".asTerm, "1".asTerm, "y>0 & x*y>0".asFormula)(1, 1::Nil)
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 1
    result.openGoals().head.sequent.ante should contain only "y_0>0 & x*y_0>0".asFormula
    result.openGoals().head.sequent.succ should contain only "[y:=2;][{x'=2,y'=0*y+1}](y>0 & x*y>0)".asFormula
  }

  it should "add y'=-a() to [x'=2]x>0" in {
    import ODETactics.diffAuxiliariesRule
    val s = Sequent(Nil, immutable.IndexedSeq("a()>0".asFormula, "x>0".asFormula), immutable.IndexedSeq("[{x'=2}]x>0".asFormula))
    val tactic = diffAuxiliariesRule(Variable("y"), "0".asTerm, "-a()".asTerm, "x>0 & y<0".asFormula)(1)
    val result = helper.runTactic(tactic, new RootNode(s))

    result.openGoals() should have size 1
    result.openGoals()(0).sequent.ante should contain only ("a()>0".asFormula, "x>0 & y<0".asFormula)
    result.openGoals()(0).sequent.succ should contain only "[{x'=2,y'=0*y+-a()}](x>0 & y<0)".asFormula
  }

  "Differential introduce constants" should "replace a with a() in v'=a" in {
    val s = sucSequent("[{v'=a}]v=v0()+a*t()".asFormula)
    val result = helper.runTactic(locateSucc(ODETactics.diffIntroduceConstantT), new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[{v'=a()}]v=v0()+a()*t()".asFormula
  }

  it should "not self-replace a() with a() in v'=a()" in {
    val s = sucSequent("[{v'=a()}]v=v0()+a()*t()".asFormula)
    val result = helper.runTactic(locateSucc(ODETactics.diffIntroduceConstantT), new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[{v'=a()}]v=v0()+a()*t()".asFormula
  }

  it should "not replace a with a() when a is not free in p" in {
    val s = sucSequent("[{v'=a}]v>0".asFormula)
    val result = helper.runTactic(locateSucc(ODETactics.diffIntroduceConstantT), new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[{v'=a}]v>0".asFormula
  }

  it should "replace every free occurrence of a with a() everywhere in the sequent" in {
    val s = sequent(Seq(),
      Seq("v>=0".asFormula, "a=0".asFormula, "\\forall a a<0".asFormula),
      Seq("[{v'=a}]v=v0()+a*t()".asFormula, "a>=0".asFormula, "[a:=2;]v>0".asFormula))
    val result = helper.runTactic(locateSucc(ODETactics.diffIntroduceConstantT), new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) should contain only ("v>=0".asFormula, "a()=0".asFormula, "\\forall a a<0".asFormula)
    result.openGoals().flatMap(_.sequent.succ) should contain only ("[{v'=a()}]v=v0()+a()*t()".asFormula, "a()>=0".asFormula, "[a:=2;]v>0".asFormula)
  }

  it should "replace every free occurrence of b (only in p) with b() everywhere in the sequent" in {
    val s = sequent(Seq(),
      Seq("v>=0".asFormula, "a=0".asFormula, "b=2".asFormula, "\\forall b b<0".asFormula),
      Seq("[{v'=a}](v>0 & b<0)".asFormula, "a>=0".asFormula, "[a:=2;]v>0".asFormula))
    val result = helper.runTactic(locateSucc(ODETactics.diffIntroduceConstantT), new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) should contain only ("v>=0".asFormula, "a=0".asFormula, "b()=2".asFormula, "\\forall b b<0".asFormula)
    result.openGoals().flatMap(_.sequent.succ) should contain only ("[{v'=a}](v>0& b()<0)".asFormula, "a>=0".asFormula, "[a:=2;]v>0".asFormula)
  }

  it should "work as part of ODE solve" in {
    val s = sequent(Seq(), Seq("v>0".asFormula, "a=0".asFormula), Seq("[{v'=a}]v>0".asFormula))
    val odeSolved = helper.runTactic(locateSucc(diffSolution(None)), new RootNode(s))
    odeSolved.openGoals() should have size 1
    val result = helper.runTactic(TacticLibrary.arithmeticT, odeSolved.openGoals().head)
    result shouldBe 'closed
  }

  "Differential effect" should "introduce a differential assignment" in {
    val s = sucSequent("[{x'=5 & x>2}]x>0".asFormula)
    val tactic = locateSucc(ODETactics.diffEffectT)
    val result = helper.runTactic(tactic, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[{x'=5 & x>2}][x':=5;]x>0".asFormula
  }

  it should "introduce a differential assignment when the postcondition is primed" in {
    val s = sucSequent("[{x'=5 & x>2}](x>0)'".asFormula)
    val tactic = locateSucc(ODETactics.diffEffectT)
    val result = helper.runTactic(tactic, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[{x'=5 & x>2}][x':=5;](x>0)'".asFormula
  }

  it should "introduce differential assignments when the postcondition is primed" in {
    val s = sucSequent("[{x'=5, y'=2 & x>2}](x>0)'".asFormula)
    val tactic = locateSucc(ODETactics.diffEffectT)*2
    val result = helper.runTactic(tactic, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[{x'=5, y'=2 & x>2}][y':=2;][x':=5;](x>0)'".asFormula
  }

  it should "introduce a differential assignment in context" in {
    val s = sucSequent("[x:=0;][{x'=5 & x>2}]x>0".asFormula)
    val tactic = ODETactics.diffEffectT(SuccPosition(0, PosInExpr(1::Nil)))
    val result = helper.runTactic(tactic, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[x:=0;][{x'=5 & x>2}][x':=5;]x>0".asFormula
  }

  it should "alpha rename if necessary" in {
    val s = sucSequent("[{y'=5 & y>2}]y>0".asFormula)
    val tactic = locateSucc(ODETactics.diffEffectT)
    val result = helper.runTactic(tactic, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[{y'=5 & y>2}][y':=5;]y>0".asFormula
  }

  it should "alpha rename with remaining ODEs mentioning original x from axiom" in {
    val s = sucSequent("[{y'=5,x'=2 & y>2 & x>0}]y>0".asFormula)
    val tactic = locateSucc(ODETactics.diffEffectT)
    val result = helper.runTactic(tactic, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[{x'=2, y'=5 & y>2 & x>0}][y':=5;]y>0".asFormula
  }

  it should "introduce a differential assignment for the first ODE in a system" in {
    val s = sucSequent("[{x'=5, y'=2 & x>2}]x>0".asFormula)
    val tactic = locateSucc(ODETactics.diffEffectT)
    val result = helper.runTactic(tactic, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[{y'=2, x'=5 & x>2}][x':=5;]x>0".asFormula
  }

  it should "introduce a differential assignment for the first ODE in a system repeatedly (even loop if asked to)" in {
    val s = sucSequent("[{x'=5, y'=2 & x>2}]x>0".asFormula)
    val tactic = ODETactics.diffEffectT(SuccPosition(0)) * 3
    val result = helper.runTactic(tactic, new RootNode(s))
    result.openGoals() should have size 1
    result.openGoals().flatMap(_.sequent.ante) shouldBe empty
    result.openGoals().flatMap(_.sequent.succ) should contain only "[{y'=2, x'=5 & x>2}][x':=5;][y':=2;][x':=5;]x>0".asFormula
  }
}
