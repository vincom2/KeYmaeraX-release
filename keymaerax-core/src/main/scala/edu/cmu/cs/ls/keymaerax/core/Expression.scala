/**
* Copyright (c) Carnegie Mellon University.
* See LICENSE.txt for the conditions of this license.
*/
/**
 * Differential Dynamic Logic expression data structures.
 * @author Andre Platzer
 * @see Andre Platzer. [[http://www.cs.cmu.edu/~aplatzer/pub/usubst.pdf A uniform substitution calculus for differential dynamic logic]].  In Amy P. Felty and Aart Middeldorp, editors, International Conference on Automated Deduction, CADE'15, Berlin, Germany, Proceedings, LNCS. Springer, 2015. [[http://arxiv.org/pdf/1503.01981.pdf arXiv 1503.01981]]
 * @see Andre Platzer. [[http://dx.doi.org/10.1145/2817824 Differential game logic]]. ACM Trans. Comput. Log. 17(1), 2015. [[http://arxiv.org/pdf/1408.1980 arXiv 1408.1980]]
 * @see Andre Platzer. [[http://dx.doi.org/10.1109/LICS.2012.64 The complete proof theory of hybrid systems]]. ACM/IEEE Symposium on Logic in Computer Science, LICS 2012, June 25–28, 2012, Dubrovnik, Croatia, pages 541-550. IEEE 2012
 * @note Code Review: 2016-03-09
 */
package edu.cmu.cs.ls.keymaerax.core

// require favoring immutable Seqs for soundness

import scala.collection.immutable
import scala.math._

/**
  * Kinds of expressions (term, formula, program).
  */
sealed abstract class Kind
/** All expressions that are neither terms nor formulas nor programs nor functions are of kind ExpressionKind */
object ExpressionKind extends Kind { override def toString = "Expression" }
/** All terms are of kind TermKind */
object TermKind extends Kind { override def toString = "Term" }
/** All formulas are of kind FormulaKind */
object FormulaKind extends Kind { override def toString = "Formula" }
/** All programs are of kind ProgramKind */
object ProgramKind extends Kind { override def toString = "Program" }
/** All differential programs are of kind DifferentialProgramKind */
object DifferentialProgramKind extends Kind/*ProgramKind.type*/ { override def toString = "DifferentialProgram" }
/** Function/predicate symbols that are not themselves terms or formulas are of kind FunctionKind */
object FunctionKind extends Kind { override def toString = "Function" }

/**
 * Sorts
 */
sealed abstract class Sort
/** Unit type of [[edu.cmu.cs.ls.keymaerax.core.Nothing Nothing]] */
object Unit extends Sort { override def toString = "Unit" }
/** Sort of booleans: [[edu.cmu.cs.ls.keymaerax.core.True True]], [[edu.cmu.cs.ls.keymaerax.core.False False]]. */
object Bool extends Sort { override def toString = "Bool" }
/** Sort of real numbers: 0, 1, 2.5 */
object Real extends Sort { override def toString = "Real" }
/** Sort of units of measure: m, s, etc */
object UnitOfMeasure extends Sort { override def toString = "UnitOfMeasure" }
/** Sort of state transformations (i.e. programs) */
object Trafo extends Sort { override def toString = "Trafo" }
/** Tuple sort for [[edu.cmu.cs.ls.keymaerax.core.Pair Pair]]. */
case class Tuple(left: Sort, right: Sort) extends Sort { override def toString = "(" + left + "," + right + ")" }
/** User-defined object sort */
case class ObjectSort(name : String) extends Sort { override def toString = name }

/**
  * Units of measure
  */
sealed abstract class MeasureUnit
/** General unit type, where the int is a unique identifier (icky, but hopefully enough for now) */
case class UnitUnit(id: String) extends MeasureUnit { override def toString = "UnitOfMeasure " + id }
/** Unit type of products; products are simply stored as a "multiset" of the units being multiplied together */
case class ProductUnit(units: Map[String, Int]) extends MeasureUnit { override def toString = "Units[" + units.foldLeft(""){ case (s, (u, e)) => s + "," + u + "**" + e.toString } }
/** The unit of measure of any variable without an annotation in the ProgramVariables section. Matches any other unit. */
object AnyUnit extends MeasureUnit
/** Dimensionless units; only matches other dimensionless things */
object NoUnit extends MeasureUnit

/**
 * Expressions of differential dynamic logic.
 * Expressions are categorized according to the syntactic categories of the grammar of differential dynamic logic:
 *
 * 1. terms are of type [[edu.cmu.cs.ls.keymaerax.core.Term]] of kind [[edu.cmu.cs.ls.keymaerax.core.TermKind]]
 *
 * 2. formulas are of type [[edu.cmu.cs.ls.keymaerax.core.Formula]] of kind [[edu.cmu.cs.ls.keymaerax.core.FormulaKind]]
 *
 * 3. hybrid programs are of type [[edu.cmu.cs.ls.keymaerax.core.Program]] of kind [[edu.cmu.cs.ls.keymaerax.core.ProgramKind]]
 *
 * See [[http://arxiv.org/pdf/1503.01981.pdf Section 2.1]]
 * @author Andre Platzer
 * @see Andre Platzer. [[http://arxiv.org/pdf/1503.01981.pdf A uniform substitution calculus for differential dynamic logic.  arXiv 1503.01981]], 2015.
 * @see [[edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXParser#apply]]
 */
sealed trait Expression {
  def kind : Kind
  def sort : Sort
  //override def toString : String = "(" + prettyString + ")@" + canonicalString
  override def toString : String = prettyString
  /** Pretty-printed string representing this expression */
  def prettyString : String = PrettyPrinter.printer(this)
  private[core] def canonicalString : String = super.toString
}

/** Atomic expressions */
sealed trait Atomic extends Expression
/** Composite expressions that are composed of subexpressions */
sealed trait Composite extends Expression

/** Unary composite expressions that are composed of one subexpression */
sealed trait UnaryComposite extends Composite {
  def child: Expression
}

/** Binary composite expressions that are composed of two subexpressions */
sealed trait BinaryComposite extends Composite {
  def left: Expression
  def right: Expression
}

/** Function/predicate/predicational application */
sealed trait ApplicationOf extends Expression {
  insist(child.sort == func.domain, "expected argument sort " + child.sort + " to match domain sort " + func.domain + " when applying " + func + " to " + child)
  insist(sort == func.sort, "sort of application is the sort of the function")
  def func : Function
  def child : Expression
}

/**
 * A named symbol such as a variable or function symbol or predicate symbol.
 * @note User-level symbols should not use underscores, which are reserved for the core.
 */
sealed trait NamedSymbol extends Expression with Ordered[NamedSymbol] {
  require(!name.isEmpty && !name.substring(0, name.length-1).contains("_"),
    "non-empty names without underscores (except at end for internal names): " + name)
  //@note the above requires conditions imply that !name.endsWith("__")
  require(!name.contains("'"), "names cannot mention primes, not even the names of differential symbols: " + name)
//  require(name.matches("""\\\_|\\?([a-zA-Z])*|([a-zA-Z][a-zA-Z0-9]*\_?)"""), "alphanumerical identifier without primes and without underscores " +
//    "(internal names allow _ at the end, \\_ at the beginning, and \\ followed by letters only): " + name)
  //@note \\ part of the names for Nothing and Anything objects
  require((name.charAt(0).isLetter || name.charAt(0)=='_' || name.charAt(0)=='\\') && name.forall(c=> c.isLetterOrDigit || c=='_' || c=='\\' || c=='$'), "alphabetical name: " + name)
  require(index.getOrElse(0)>=0, "nonnegative index if any " + this)

  def name: String
  def index: Option[Int]

  /** Compare named symbols lexicographically: by name, index, category. */
  def compare(other: NamedSymbol): Int = {
    val cmp = name.compare(other.name)
    if (cmp != 0) cmp else {
      val cmp2 = index.getOrElse(-1) - other.index.getOrElse(-1)
      //@note .getCanonicalName would cause no collisions if same class name in different packages, but expressions are sealed in core.
      if (cmp2 != 0) cmp2 else getClass.getSimpleName.compareTo(other.getClass.getSimpleName)
    }
  } ensuring(r => r!=0 || this==other, "no different categories of symbols with same name " + this + " compared to " + other)

  /** Get name with index of this NamedSymbol. */
  def asString: String = index match {
    case None => name
    case Some(idx) => name + "_" + idx
  }

  /** Full string with names and full types */
  def fullString: String = asString + ":" + sort

  override def toString: String = asString + "@" + getClass.getSimpleName
}

/*********************************************************************************
  * Terms of differential dynamic logic
  *********************************************************************************
  */

/**
 * Terms of differential dynamic logic.
 * @author Andre Platzer
  * @see [[edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXParser#termParser]]
 */
sealed trait Term extends Expression {
  final def kind: Kind = TermKind
}

/** Atomic terms */
sealed trait AtomicTerm extends Term with Atomic

/** Real terms */
private[core] sealed trait RTerm extends Term {
  final def sort: Sort = Real
}

/** Variable called name with an index of a fixed sort */
/* @todo a better thing to do would probably be to add a unit field to a variable,
 * but since that would necessitate many changes to the entire codebase,
 * we'll avoid it for now
 */
sealed case class Variable(name: String, index: Option[Int] = None, sort: Sort = Real)
  extends NamedSymbol with AtomicTerm

/** Differential symbol x' for variable x */
sealed case class DifferentialSymbol(x: Variable)
  extends NamedSymbol with AtomicTerm with RTerm {
  insist(x.sort == Real, "differential symbols expect real sort")
  def name: String = x.name
  def index: Option[Int] = x.index
  override def asString: String = x.asString + "'"
  override def toString: String =  x.asString + "'" + "@" + getClass.getSimpleName
}

/** Number literal */
case class Number(value: BigDecimal) extends AtomicTerm with RTerm

/** Function symbol or predicate symbol or predicational symbol name_index:domain->sort */
sealed case class Function(name: String, index: Option[Int] = None, domain: Sort, sort: Sort)
  extends Expression with NamedSymbol {
  def kind: Kind = FunctionKind
  /** Full string with names and full types */
  override def fullString: String = asString + ":" + domain + "->" + sort
}

/** •: Placeholder for terms in uniform substitutions. Reserved nullary function symbol \\cdot for uniform substitutions are unlike ordinary function symbols */
object DotTerm extends NamedSymbol with AtomicTerm with RTerm {
  def name: String = "\\cdot"
  def index: Option[Int] = None
}

/** The empty argument of Unit sort (as argument for arity 0 function/predicate symbols) */
object Nothing extends NamedSymbol with AtomicTerm {
  def sort: Sort = Unit
  def name: String = "\\nothing"
  def index: Option[Int] = None
}

/** The list of all variables as arguments \bar{x} (axioms that allow any variable dependency) */
object Anything extends NamedSymbol with AtomicTerm with RTerm {
  def name: String = "\\anything"
  def index: Option[Int] = None
  //@note Direct prettyString implementation bypasses pretty printer contracts, which fail since Anything can't be parsed standalone.
  override def prettyString: String = "??"
}

/** Function symbol applied to argument child func(child) */
case class FuncOf(func: Function, child: Term) extends AtomicTerm with ApplicationOf {
  /** The sort of an ApplicationOf is the sort of func */
  def sort: Sort = func.sort
}

/** Composite terms */
sealed trait CompositeTerm extends Term with Composite

/** Unary Composite Terms, i.e. terms composed of one real term. */
sealed trait UnaryCompositeTerm extends UnaryComposite with CompositeTerm {
  /** Create a term of this constructor but with the given argument as child instead. (copy)
    * @example {{{
    *         Neg(Number(77)).reapply(Number(99)) == Neg(Number(99))
    *         Neg(Variable("x")).reapply(Plus(Number(42),Number(69))) == Neg(Plus(Number(42),Number(69)))
    *         }}}
    */
  def reapply: Term=>Term
  def child: Term
}

/** Unary Composite Real Terms, i.e. real terms composed of one real term. */
private[core] sealed trait RUnaryCompositeTerm extends UnaryCompositeTerm with RTerm {
  insist(child.sort == Real, "expected argument sort real: " + child.sort)
}

/** Binary Composite Terms, i.e. terms composed of two terms. */
sealed trait BinaryCompositeTerm extends BinaryComposite with CompositeTerm {
  /** Create a term of this constructor but with the give left and right arguments instead. (copy)
    * @example {{{
    *         Times(Number(7), Variable("v")).reapply(Variable("a"), Number(99)) == Times(Variable("a"), Number(99))
    *         }}}
    */
  def reapply: (Term,Term)=>Term
  def left: Term
  def right: Term
}

/** Binary Composite Real Terms, i.e. real terms composed of two real terms. */
private[core] sealed trait RBinaryCompositeTerm extends BinaryCompositeTerm with RTerm {
  insist(left.sort == Real && right.sort == Real, "expected argument sorts real: " + left + " and " + right)
  def left: Term
  def right: Term
}

/** - unary negation: minus */
case class Neg(child: Term) extends RUnaryCompositeTerm { def reapply = copy }
/** + binary addition */
case class Plus(left: Term, right: Term) extends RBinaryCompositeTerm { def reapply = copy }
/** - binary subtraction */
case class Minus(left: Term, right: Term) extends RBinaryCompositeTerm { def reapply = copy }
/** * binary multiplication*/
case class Times(left: Term, right: Term) extends RBinaryCompositeTerm { def reapply = copy }
/** / real division */
case class Divide(left: Term, right: Term) extends RBinaryCompositeTerm { def reapply = copy }
/** real exponentiation or power: left^right^ */
//@note axiom("^' derive power") needs right to be a Term not just a Number
case class Power(left: Term, right: Term) extends RBinaryCompositeTerm { def reapply = copy }

/** ' differential of a term */
case class Differential(child: Term) extends RUnaryCompositeTerm { def reapply = copy }

/** Pairs (left,right) for binary Function and FuncOf and PredOf */
case class Pair(left: Term, right: Term) extends BinaryCompositeTerm {
  def reapply = copy
  def sort: Sort = Tuple(left.sort, right.sort)
}

/*********************************************************************************
  * Formulas of differential dynamic logic
  *********************************************************************************
  */

/**
 * Formulas of differential dynamic logic.
 * @author Andre Platzer
 * @see [[edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXParser#formulaParser]]
 */
sealed trait Formula extends Expression {
  final def kind: Kind = FormulaKind
  final def sort: Sort = Bool
}

/** Atomic formulas */
sealed trait AtomicFormula extends Formula with Atomic

/** Atomic comparison formula composed of two terms. */
sealed trait ComparisonFormula extends AtomicFormula with BinaryComposite {
  /** Create a formula of this constructor but with the give left and right arguments instead. (copy)
    * @example {{{
    *         GreaterEqual(Number(7), Variable("v")).reapply(Variable("a"), Number(99)) == GreaterEqual(Variable("a"), Number(99))
    *         }}}
    */
  def reapply: (Term,Term)=>Formula
  def left: Term
  def right: Term
}

/** Real comparison formula composed of two real terms. */
private[core] sealed trait RComparisonFormula extends ComparisonFormula {
  insist(left.sort == Real && right.sort == Real, "expected argument sorts real: " + left + " and " + right)
}

/** Verum formula true */
object True extends AtomicFormula
/** Falsum formula false */
object False extends AtomicFormula

/** ``=`` equality left = right */
case class Equal(left: Term, right: Term) extends ComparisonFormula {
  insist(left.sort == right.sort, "expected identical argument sorts: " + left + " and " + right)
  def reapply = copy
}
/** != disequality left != right */
//@todo DisEqual or UnEqual might have been better name to not confuse with Not(Equal()) even though equivalent
case class NotEqual(left: Term, right: Term) extends ComparisonFormula {
  insist(left.sort == right.sort, "expected identical argument sorts: " + left + " and " + right)
  def reapply = copy
}

/** >= greater or equal comparison left >= right */
case class GreaterEqual(left: Term, right: Term) extends RComparisonFormula { def reapply = copy }
/** > greater than comparison left > right */
case class Greater(left: Term, right: Term) extends RComparisonFormula { def reapply = copy }
/** < less or equal comparison left <= right */
case class LessEqual(left: Term, right: Term) extends RComparisonFormula { def reapply = copy }
/** <= less than comparison left < right */
case class Less(left: Term, right: Term) extends RComparisonFormula { def reapply = copy }

/** ⎵: Placeholder for formulas in uniform substitutions. Reserved nullary predicational symbol _ for substitutions are unlike ordinary predicational symbols */
object DotFormula extends NamedSymbol with AtomicFormula {
  def name: String = "\\_"
  def index: Option[Int] = None
}

/** Predicate symbol applied to argument child func(child) */
case class PredOf(func: Function, child: Term) extends AtomicFormula with ApplicationOf {
  //@note redundant requires since ApplicationOf.sort and Formula.requires will check this already.
  insist(func.sort == Bool, "expected predicate sort Bool found " + func.sort + " in " + this)
}
/** Predicational or quantifier symbol applied to argument formula child */
case class PredicationalOf(func: Function, child: Formula) extends AtomicFormula with ApplicationOf {
  //@note redundant requires since ApplicationOf.sort and Formula.requires will check this already.
  insist(func.sort == Bool, "expected argument sort Bool: " + this)
  insist(func.domain == Bool, "expected domain simplifies to Bool: " + this)
}

/** Composite formulas */
sealed trait CompositeFormula extends Formula with Composite

/** Unary Composite Formulas, i.e. formulas composed of one formula. */
sealed trait UnaryCompositeFormula extends UnaryComposite with CompositeFormula {
  /** Create a formula of this constructor but with the given argument as child instead. (copy)
    * @example {{{
    *         Not(GreaterEqual(Variable("x"),Number(0))).reapply(UnEqual(Number(7),Number(9))) == Not(UnEqual(Number(7),Number(9)))
    *         Not(True).reapply(False) == Not(False)
    *         }}}
    */
  def reapply: Formula=>Formula
  def child: Formula
}

/** Binary Composite Formulas, i.e. formulas composed of two formulas. */
sealed trait BinaryCompositeFormula extends BinaryComposite with CompositeFormula {
  /** Create a formula of this constructor but with the give left and right arguments instead. (copy)
    * @example {{{
    *         Or(GreaterEqual(Variable("x"),Number(0)), False).reapply(True, UnEqual(Number(7),Number(9))) == Or(True, UnEqual(Number(7),Number(9)))
    *         }}}
    */
  def reapply: (Formula,Formula)=>Formula
  def left: Formula
  def right: Formula
}

/** ! logical negation: not */
case class Not(child: Formula) extends UnaryCompositeFormula { def reapply = copy }
/** & logical conjunction: and */
case class And(left: Formula, right:Formula) extends BinaryCompositeFormula { def reapply = copy }
/** | logical disjunction: or */
case class Or(left: Formula, right:Formula) extends BinaryCompositeFormula { def reapply = copy }
/** -> logical implication: implies */
case class Imply(left: Formula, right:Formula) extends BinaryCompositeFormula { def reapply = copy }
/** <-> logical biimplication: equivalent */
case class Equiv(left: Formula, right:Formula) extends BinaryCompositeFormula { def reapply = copy }

/** Quantified formulas */
sealed trait Quantified extends /*Unary?*/CompositeFormula {
  insist(vars.length==1, "quantifiers bind exactly one variable at the moment")
//  require(vars.nonEmpty, "quantifiers bind at least one variable")
//  insist(vars.distinct.size == vars.size, "no duplicates within one quantifier block")
//  insist(vars.forall(x => x.sort == vars.head.sort), "all vars have the same sort")
  /** Create a formula of this constructor but with the given variable list and child as argument instead. (copy)
    * @example {{{
    *         Forall(immutable.Seq(Variable("x")), PredOf(Func("p",None,Real,Bool),Variable("x")).reapply(
    *                immutable.Seq(Variable("y")), PredOf(Func("q",None,Real,Bool),Variable("y")))
    *         == Forall(immutable.Seq(Variable("y")), PredOf(Func("q",None,Real,Bool),Variable("y"))
    *         }}}
    */
  def reapply: (immutable.Seq[Variable],Formula)=>Formula
  /** The variables quantified here */
  def vars: immutable.Seq[Variable]
  def child: Formula
}
/** \forall vars universally quantified formula */
case class Forall(vars: immutable.Seq[Variable], child: Formula) extends Quantified { def reapply = copy }
/** \exists vars existentially quantified formula */
case class Exists(vars: immutable.Seq[Variable], child: Formula) extends Quantified { def reapply = copy }

/** Modal formulas */
sealed trait Modal extends CompositeFormula {
  /** Create a formula of this constructor but with the given program and formula child as argument instead. (copy) */
  def reapply: (Program,Formula)=>Formula
  def program: Program
  def child: Formula
}
/** box modality all runs of program satisfy child [program]child */
case class Box(program: Program, child: Formula) extends Modal { def reapply = copy }
/** diamond modality some run of program satisfies child ⟨program⟩child */
case class Diamond(program: Program, child: Formula) extends Modal { def reapply = copy }

/** Differential formula are differentials of formulas in analogy to differential terms (child)' */
case class DifferentialFormula(child: Formula) extends UnaryCompositeFormula { def reapply = copy }

/*********************************************************************************
  * Programs of differential dynamic logic
  *********************************************************************************
  */

/**
 * Hybrid programs of differential dynamic logic.
 * @author Andre Platzer
 * @see [[edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXParser#programParser]]
 */
sealed trait Program extends Expression {
  /*final*/ def kind: Kind = ProgramKind
  final def sort: Sort = Trafo
}

/** Atomic programs */
sealed trait AtomicProgram extends Program with Atomic

/** Uninterpreted program constant */
sealed case class ProgramConst(name: String) extends NamedSymbol with AtomicProgram {
  def index: Option[Int] = None
}

/** x:=e assignment */
case class Assign(x: Variable, e: Term) extends AtomicProgram {
  insist(e.sort == x.sort, "assignment of compatible sort " + this)
}
/** x':=e differential assignment */
case class DiffAssign(xp: DifferentialSymbol, e: Term) extends AtomicProgram {
  insist(e.sort == Real, "differential assignment of real sort " + this)
}
/** x:=* nondeterministic assignment */
case class AssignAny(x: Variable) extends AtomicProgram
/** ?cond test a formula as a condition on the current state */
case class Test(cond: Formula) extends AtomicProgram

/** composite programs */
sealed trait CompositeProgram extends Program with Composite

/** Unary Composite Programs, i.e. programs composed of one program. */
sealed trait UnaryCompositeProgram extends UnaryComposite with CompositeProgram {
  /** Create a program of this constructor but with the given argument as child instead. (copy)
    * @example {{{
    *         Loop(ProgramConst("alpha")).reapply(Assign(Variable("x"),Number(42))) == Loop(Assign(Variable("x"),Number(42)))
    *         }}}
    */
  def reapply: Program=>Program
  def child: Program
}

/** Binary Composite Programs, i.e. programs composed of two programs. */
sealed trait BinaryCompositeProgram extends BinaryComposite with CompositeProgram {
  /** Create a program of this constructor but with the give left and right arguments instead. (copy)
    * @example {{{
    *         Choice(ProgramConst("alpha"), ProgramConst("beta")).reapply(ProgramConst("gamma"), ProgramConst("delta")) == Choice(ProgramConst("gamma"), ProgramConst("delta"))
    *         }}}
    */
  def reapply: (Program,Program)=>Program
  def left: Program
  def right: Program
}


/** left++right nondeterministic choice */
case class Choice(left: Program, right: Program) extends BinaryCompositeProgram { def reapply = copy }
/** left;right sequential composition */
case class Compose(left: Program, right: Program) extends BinaryCompositeProgram { def reapply = copy }
/** child* nondeterministic repetition */
case class Loop(child: Program) extends UnaryCompositeProgram { def reapply = copy }
/** `child^d` dual program */
case class Dual(child: Program) extends UnaryCompositeProgram { def reapply = copy
  require(false, "Hybrid games are currently disabled")
}

/**
 * Differential programs
 * @author Andre Platzer
 * @see [[edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXParser#differentialProgramParser]]
 */
sealed trait DifferentialProgram extends Program {
  override def kind: Kind = DifferentialProgramKind
}
/** Atomic differential programs */
sealed trait AtomicDifferentialProgram extends AtomicProgram with DifferentialProgram
/** Differential equation system ode with given evolution domain constraint */
//@note could say that ODESystem is no differential program since not to be nested within DifferentialProduct.
case class ODESystem(ode: DifferentialProgram, constraint: Formula = True)
  extends Program {
  override def kind: Kind = ProgramKind
}
/** Uninterpreted differential program constant */
sealed case class DifferentialProgramConst(name: String) extends NamedSymbol with AtomicDifferentialProgram {
  def index: Option[Int] = None
}
/** x'=e atomic differential equation */
case class AtomicODE(xp: DifferentialSymbol, e: Term) extends AtomicDifferentialProgram {
  insist(e.sort == Real, "expected argument sort real " + this)
  /* @NOTE Soundness: AtomicODE requires explicit-form so f(?) cannot verbatim mention differentials/differential symbols,
     which is required for soundness of axiom "DE differential effect (system)" */
  //@note avoid toString call, which could cause an infinite loop coming from contracts checking in pretty printer. But should probably be taken care of.
  insist(!StaticSemantics.isDifferential(e), "Explicit-form differential equations expected, without any differentials on right-hand side: " + xp + "=" + e)
}

/**
 * left,right parallel product of differential programs.
 * This data structure automatically reassociates to list form
 * DifferentialProduct(AtomicDifferentialProgram, DifferentialProduct(AtomicDifferentialProgram, ....))
 * @note This is a case class except for an override of the apply function.
 * @note Private constructor so only [[DifferentialProduct.apply]] can ever create this, which will re-associate et al.
 */
final class DifferentialProduct private(val left: DifferentialProgram, val right: DifferentialProgram)
  extends DifferentialProgram with BinaryComposite {

  override def equals(e: Any): Boolean = e match {
    case a: DifferentialProduct => left == a.left && right == a.right
    case _ => false
  }

  override def hashCode: Int = 31 * left.hashCode() + right.hashCode()
}

object DifferentialProduct {
  /**
   * Construct an ODEProduct in reassociated normal form, i.e. as a list such that left will never be an ODEProduct in
   * the data structures.
   * @note Completeness: reassociate needed in DifferentialProduct data structures for
   *       axiom "DE differential effect (system)" so as not to get stuck after it.
   */
  def apply(left: DifferentialProgram, right: DifferentialProgram): DifferentialProduct = {
    require(!left.isInstanceOf[ODESystem], "Left should not be its own ODESystem: " + left + " with " + right)
    require(!right.isInstanceOf[ODESystem], "Right should not be its own ODESystem: " + left + " with " + right)
    insist(differentialSymbols(left).intersect(differentialSymbols(right)).isEmpty, "No duplicate differential equations when composing differential equations " + left + " and " + right)
    reassociate(left, right)
  } ensuring(r => differentialSymbols(r).length == differentialSymbols(r).distinct.length,
    "No undetected duplicate differential equations when composing differential equations " + left + " and " + right + " to form " + reassociate(left, right))

  def unapply(e: Any): Option[(DifferentialProgram, DifferentialProgram)] = e match {
    case a: DifferentialProduct => Some(a.left, a.right)
    case _ => None
  }

  //@tailrec
  private def reassociate(left: DifferentialProgram, right: DifferentialProgram): DifferentialProduct = (left match {
    // properly associated cases
    case l: AtomicODE => new DifferentialProduct(l, right)
    case l: DifferentialProgramConst => new DifferentialProduct(l, right)
    // reassociate so that there's no more differential product on the left
    case DifferentialProduct(ll, lr) =>
      assert(ll.isInstanceOf[AtomicDifferentialProgram], "reassociation has succeeded on the left")
      reassociate(ll, reassociate(lr, right))
  }) ensuring(r => listify(r) == listify(left) ++ listify(right),
    "reassociating DifferentialProduct does not change the list of atomic ODEs")

  /** Turn differential program ode along its DifferentialProduct into a list */
  private def listify(ode: DifferentialProgram): immutable.List[DifferentialProgram] = ode match {
    case p: DifferentialProduct => listify(p.left) ++ listify(p.right)
    case a: AtomicDifferentialProgram => a :: Nil
  }

  /** The list of all literal differential symbols in ode */
  //@note Differential symbols can only occur on the left of AtomicODEs anyhow.
  private def differentialSymbols(ode: DifferentialProgram): immutable.List[DifferentialSymbol] = {ode match {
    case p: DifferentialProduct => differentialSymbols(p.left) ++ differentialSymbols(p.right)
    case AtomicODE(xp, _) => xp :: Nil
    case a: DifferentialProgramConst => Nil
  }} ensuring(r => r.toSet==StaticSemantics.symbols(ode).filter(x=>x.isInstanceOf[DifferentialSymbol]),
    "StaticSemantics should agree since differential symbols only occur on the left-hand side of differential equations " + StaticSemantics.symbols(ode).toList.filter(x=>x.isInstanceOf[DifferentialSymbol]))

}