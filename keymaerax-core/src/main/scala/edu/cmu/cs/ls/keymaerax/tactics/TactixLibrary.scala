/**
 * Copyright (c) Carnegie Mellon University. CONFIDENTIAL
 * See LICENSE.txt for the conditions of this license.
 */
package edu.cmu.cs.ls.keymaerax.tactics

import edu.cmu.cs.ls.keymaerax.core.{Sequent, Formula, Term, Variable, Expression}
import edu.cmu.cs.ls.keymaerax.tactics.Tactics.{PositionTactic, Tactic}

/**
 * Tactix: Main tactic library with simple interface.
 *
 * This library features all main tactic elements for most common cases, except sophisticated tactics.
 *
 * @author aplatzer
 * @see Andre Platzer. [[http://www.cs.cmu.edu/~aplatzer/pub/usubst.pdf A uniform substitution calculus for differential dynamic logic]].  In Amy P. Felty and Aart Middeldorp, editors, International Conference on Automated Deduction, CADE'15, Berlin, Germany, Proceedings, LNCS. Springer, 2015.
 * @see Andre Platzer. [[http://arxiv.org/pdf/1503.01981.pdf A uniform substitution calculus for differential dynamic logic.  arXiv 1503.01981]], 2015.
 */
object TactixLibrary {
  /** step: makes one proof step to simplify the formula at the indicated position (unless @invariant needed) */
  def step                    : PositionTactic = TacticLibrary.step

  /** Normalize to sequent form */
  //@todo ensure to keep branching factor down by favoring alpha rules over beta rules
  def normalize               : Tactic = l(step)*
  /** exhaust propositional logic */
  def prop                    : Tactic = TacticLibrary.propositional
  /** master: master tactic that tries hard to prove whatever it could */
  def master                  : Tactic = TacticLibrary.master(new NoneGenerate(), true, "Mathematica")


  def onBranch(s1: (String, Tactic), spec: (String, Tactic)*): Tactic = SearchTacticsImpl.onBranch(s1, spec:_*)

  // Locating applicable positions for PositionTactics

  /** Locate applicable position in succedent */
  def ls(tactic: PositionTactic): Tactic = TacticLibrary.locateSucc(tactic)
  /** Locate applicable position in antecedent */
  def la(tactic: PositionTactic): Tactic = TacticLibrary.locateAnte(tactic)
  /** Locate applicable position in antecedent or succedent */
  def l(tactic: PositionTactic): Tactic  = TacticLibrary.locateAnteSucc(tactic)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Propositional tactics
  def hide                    : PositionTactic = TacticLibrary.hideT
  /** Hide left: weaken a formula to drop it from the antecedent */
  def hideL                   : PositionTactic = TacticLibrary.hideT
  /** Hide right: weaken a formula to drop it from the succcedent */
  def hideR                   : PositionTactic = TacticLibrary.hideT
  /** !L Not left: move an negation in the antecedent to the succedent */
  def notL                    : PositionTactic = TacticLibrary.NotLeftT
  /** !R Not right: move an negation in the succedent to the antecedent */
  def notR                    : PositionTactic = TacticLibrary.NotRightT
  /** &L And left: split a conjunction in the antecedent into separate assumptions */
  def andL                    : PositionTactic = TacticLibrary.AndLeftT
  /** &R And right: prove a conjunction in the succedent on two separate branches */
  def andR                    : PositionTactic = TacticLibrary.AndRightT
  /** |L Or left: use a disjunction in the antecedent by assuming each option on separate branches */
  def orL                     : PositionTactic = TacticLibrary.OrLeftT
  /** |R Or right: split a disjunction in the succedent into separate formulas to show alternatively */
  def orR                     : PositionTactic = TacticLibrary.OrRightT
  /** ->L Imply left: use an implication in the antecedent by proving its left-hand side on one branch and using its right-hand side on the other branch */
  def implyL                  : PositionTactic = TacticLibrary.ImplyLeftT
  /** ->R Imply right: prove an implication in the succedent by assuming its left-hand side and proving its right-hand side */
  def implyR                  : PositionTactic = TacticLibrary.ImplyRightT
  /** <->L Equiv left: use an equivalence by considering both true or both false cases */
  def equivL                  : PositionTactic = TacticLibrary.EquivLeftT
  /** <->R Equiv right: prove an equivalence by proving both implications */
  def equivR                  : PositionTactic = TacticLibrary.EquivRightT

  /** cut a formula in to prove it on one branch and then assume it on the other. Or to perform a case distinction on whether it holds */
  def cut(cut : Formula)      : Tactic         = TacticLibrary.cutT(Some(cut))

  // quantifiers
  /** all right: Skolemize a universal quantifier in the succedent */
  def allR                    : PositionTactic = TacticLibrary.skolemizeT
  /** all left: instantiate a universal quantifier in the antecedent by a concrete instance */
  def allL(x: Variable, inst: Term) : PositionTactic = TacticLibrary.instantiateQuanT(x, inst)
  def allL(inst: Term)        : PositionTactic = TacticLibrary.instantiateQuanT(???, inst)
  /** exists left: Skolemize an existential quantifier in the antecedent */
  def existsL                 : PositionTactic = TacticLibrary.skolemizeT
  /** exists right: instantiate an existential quantifier in the succedwent by a concrete instance as a witness */
  def existsR(x: Variable, inst: Term) : PositionTactic = TacticLibrary.instantiateQuanT(x, inst)
  def existsR(inst: Term)     : PositionTactic = TacticLibrary.instantiateQuanT(???, inst)

  // modalities
  //  def SpecificMaster(toolId : String) : Tactic = TacticLibrary.master(new NoneGenerate(), true, toolId)
  /** assignb: [:=] simplify assignment by substitution or equation */
  def assignb                 : PositionTactic = TacticLibrary.boxAssignT
  /** randomb: [:*] simplify nondeterministic assignment to universal quantifier */
  def randomb                 : PositionTactic = TacticLibrary.boxNDetAssign
  /** testb: [?] simplifies test to an implication */
  def testb                   : PositionTactic = TacticLibrary.boxTestT
  /** diffSolve: solve a differential equationb */
  def diffSolve               : PositionTactic = TacticLibrary.diffSolutionT
  /** choiceb: [++] handles both cases of a nondeterministic choice separately */
  def choiceb                 : PositionTactic = TacticLibrary.boxChoiceT
  /** composeb: [;] handle both parts of a sequential composition one at a time */
  def composeb                : PositionTactic = TacticLibrary.boxSeqT
  /** iterateb: [*] prove a property of a loop by unrolling it once */
  def iterateb                : PositionTactic = ???
  /** I: prove a property of a loop by induction with the given loop invariant (hybrid systems) */
  def I(invariant : Formula)  : PositionTactic = TacticLibrary.inductionT(Some(invariant))
  def loop(invariant: Formula) = I(invariant)
  /** K: modal modus ponens (hybrid systems) */
  def K                       : PositionTactic = PropositionalTacticsImpl.kModalModusPonensT
  /** V: vacuous box will be discarded (unless it changes values of the postcondition) (hybrid systems) */
  def V                       : PositionTactic = HybridProgramTacticsImpl.boxVacuousT

  // differential equations
  /** DW: Differential Weakening to use evolution domain constraint (equivalence form) */
  def DW                      : PositionTactic = TacticLibrary.diffWeakenT
  /** DC: Differential Cut a new invariant for a differential equation */
  def DC(invariant: Formula)  : PositionTactic = TacticLibrary.diffCutT(invariant)
  /** DE: Differential Effect exposes the effect of a differential equation on its differential symbols */
  def DE                      : PositionTactic = ODETactics.diffEffectT
  /** DI: Differential Invariant proves a formula to be an invariant of a differential equation */
  def DI                      : PositionTactic = TacticLibrary.diffInvariant
  /** DG: Differential Ghost add auxiliary differential equations with extra variables y'=a*y+b */
  def DG(y:Variable, a:Term, b:Term) : PositionTactic = ODETactics.diffAuxiliaryT(y,a,b)
  /** DS: Differential Solution solves a differential equation */
  def DS                      : PositionTactic = ???
  /** Dassignb: Substitute a differential assignment */
  def Dassignb                : PositionTactic = HybridProgramTacticsImpl.boxDerivativeAssignT
  /** Dplus: +' derives a sum */
  def Dplus                   : PositionTactic = SyntacticDerivationInContext.AddDerivativeT
  /** Dminus: -' derives a difference */
  def Dminus                  : PositionTactic = SyntacticDerivationInContext.SubtractDerivativeT
  /** Dtimes: *' derives a product */
  def Dtimes                  : PositionTactic = SyntacticDerivationInContext.MultiplyDerivativeT
  /** Dcompose: o' derives a function composition by chain rule */
  def Dcompose                : PositionTactic = ???

  /** Prove the given list of differential invariants in that order by DC+DI */
  //@todo could change type to invariants: Formula* if considered more readable
  def diffInvariant(invariants: List[Formula]): PositionTactic = ODETactics.diffInvariant(invariants)

  // rules

  /** G: Goedel rule proves the postcondition of a box in isolation (hybrid systems) */
  def G                       : Tactic         = AxiomaticRuleTactics.goedelT
  /** allG: all generalization rule proves the formula after a universal quantifier in isolation */
  def allG                    : Tactic         = AxiomaticRuleTactics.forallGeneralizationT
  /** CT: Term Congruence: Contextual Equivalence of terms proves an equality */
  def CT(inEqPos: PosInExpr)  : Tactic         = ???
  /** CQ: Equation Congruence: Contextual Equivalence of terms proves an equivalence */
  def CQ(inEqPos: PosInExpr)  : Tactic         = AxiomaticRuleTactics.equationCongruenceT(inEqPos)
  /** CE: Congruence: Contextual Equivalence proves an equivalence */
  def CE(inEqPos: PosInExpr)  : Tactic         = AxiomaticRuleTactics.equivalenceCongruenceT(inEqPos)


  /** QE: Quantifier Elimination to decide arithmetic */
  def QE                      : Tactic         = TacticLibrary.arithmeticT

  /** close: closes the branch when the same formula is in the antecedent and succedent or true or false close */
  def close                   : Tactic         = TacticLibrary.closeT
  /** closeId: closes the branch when the same formula is in the antecedent and succedent */
  def closeId                 : Tactic         = TacticLibrary.AxiomCloseT
  /** closeT: closes the branch when true is in the succedent */
  def closeT                  : PositionTactic = TacticLibrary.CloseTrueT
  /** closeF: closes the branch when false is in the antecedent */
  def closeF                  : PositionTactic = TacticLibrary.CloseFalseT

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Bigger Tactics.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // Utility Tactics
  /** nil: skip is a no-op that has no effect */
  def nil : Tactic = Tactics.NilT
  def skip : Tactic = nil

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Contract Tactics and Debugging Tactics
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Tactic contracts
  /** Assert that the given condition holds for the goal's sequent. */
  def assert(cond : Sequent=>Boolean, msg:String = ""): Tactic = Tactics.assertT(cond, msg)
  /** Assertion that the sequent has the specified number of antecedent and succedent formulas, respectively. */
  def assert(antecedents: Int, succedents: Int): Tactic = Tactics.assertT(antecedents, succedents)
  /** Assert that the given formula is present at the given position in the sequent that this tactic is applied to. */
  def assert(expected: Formula, pos: Position, msg:String): Tactic = Tactics.assertT(expected, pos, msg)

  // PositionTactic contracts
  /** Assert that the given condition holds for the sequent at the position where the tactic is applied */
  def assert(cond : (Sequent,Position)=>Boolean, msg:String): PositionTactic = Tactics.assertPT(cond, msg)
  /** Assert that the given expression is present at the position in the sequent where this tactic is applied to. */
  def assert(expected: Expression, msg:String): PositionTactic = expected match {
    case t: Term => Tactics.assertPT(t, msg)
    case f: Formula => Tactics.assertPT(f, msg)
  }

  def debug(s: => Any): Tactic = TacticLibrary.debugT(s)
  def debugAt(s: => Any): PositionTactic = TacticLibrary.debugAtT(s)

}
