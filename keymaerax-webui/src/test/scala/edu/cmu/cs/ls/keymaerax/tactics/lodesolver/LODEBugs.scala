package edu.cmu.cs.ls.keymaerax.tactics.lodesolver

import edu.cmu.cs.ls.keymaerax.core.SuccPos
import edu.cmu.cs.ls.keymaerax.tactics.{TacticTestSuite, SearchTacticsImpl, SuccPosition, ODETactics}
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
import edu.cmu.cs.ls.keymaerax.tags.ObsoleteTest

/**
 * These tests reproduce bugs (both current and historical) in the Logical ODE Solver
 * Created by nfulton on 9/11/15.
 */
@ObsoleteTest
class LODESolverBugs extends TacticTestSuite {
  /**
   * @note this was never a bug.
   */
  "diffSolveConstraintT" should "work for z' = 0" in {
    val formula = "[{z' = 1 & 2=2}]1=1".asFormula
    val node = helper.formulaToNode(formula)

    val tactic = ODETactics.diffSolveConstraintT(SuccPos(0))
    helper.runTactic(tactic, node)

    val s = helper.remainingSequent(node)

    s.ante.length shouldBe 0
    s.succ.length shouldBe 1
    s.succ.head shouldBe "\\forall T (T>=0->\\forall S (0<=S&S<=T->2=2)->[z:=z+1*T;]1=1)".asFormula
  }


  /**
   * @note causes a clash b/c z occurs on the RHS of z'.
   * @note wontfix -- instead, use a ODETactics.rewriteConstantTime phase to rewrite 0*z+1.
   *       See next test case.
   * @note related to rewriteConstantTime which we don't use anymore.
   */
  ignore should "work for z' = 0*z + 1" in {
    val formula = "[{z' = 0*z+1 & 2=2}]1=1".asFormula
    val node = helper.formulaToNode(formula)

    val tactic = ODETactics.diffSolveConstraintT(SuccPos(0))
    helper.runTactic(tactic, node)

    val s = helper.remainingSequent(node)

    s.ante.length shouldBe 0
    s.succ.length shouldBe 1
    fail("wontfix")
  }

  /** @note related to rewriteConstantTime, which we aren't using anymore. */
  ignore should "get correct input from the workaround tactic" in {
    val formula = "[{z' = 0*z+1 & 2=2}]1=1".asFormula
    val node = helper.formulaToNode(formula)

    val tactic = SearchTacticsImpl.locateTerm(ODETactics.rewriteConstantTime)
    helper.runTactic(tactic, node)

    val s = helper.remainingSequent(node)

    s.ante.length shouldBe 0
    s.succ.length shouldBe 1
    s.succ.head   shouldBe "[{z' = 1 & 2=2}]1=1".asFormula
  }

  /** @note related to rewriteConstantTime, which we aren't using anymore. */
  ignore should "handle output of workwaround tactic" in {
    val formula = "[{z' = 1 & 2=2}]1=1".asFormula
    val node = helper.formulaToNode(formula)

    val tactic = ODETactics.diffSolveConstraintT(SuccPos(0))
    helper.runTactic(tactic, node)

    val s = helper.remainingSequent(node)

    s.ante.length shouldBe 0
    s.succ.length shouldBe 1
    s.succ.head shouldBe "\\forall T (T>=0->\\forall S (0<=S&S<=T->2=2)->[z:=z+1*T;]1=1)".asFormula
  }
}