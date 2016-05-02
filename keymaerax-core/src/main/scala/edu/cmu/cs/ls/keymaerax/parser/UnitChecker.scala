package edu.cmu.cs.ls.keymaerax.parser

import edu.cmu.cs.ls.keymaerax.core._

/**
  * Created by vincenth on 30/04/16.
  */
case class UnitChecker(r : KeYmaeraXProblemParserResult) {
  val (varMap, unitMap) = (r.variableMap, r.unitMap)

  def unitsEqual(u1 : MeasureUnit, u2 : MeasureUnit) : Boolean = {
    (u1, u2) match {
      case (AnyUnit, _) => true
      case (_, AnyUnit) => true
      case (NoUnit, NoUnit) => true
      case (UnitUnit(s1), UnitUnit(s2)) => s1 == s2
      case (UnitUnit(s), ProductUnit(m)) =>
        m.foldLeft(true)({ case (acc, (s1, e)) => !((s1 != s && e != 0) || (s1 == s && e != 1)) })
      case (ProductUnit(m), UnitUnit(s)) =>
        m.foldLeft(true)({ case (acc, (s1, e)) => !((s1 != s && e != 0) || (s1 == s && e != 1)) })
      case (ProductUnit(m1), ProductUnit(m2)) => m1 == m2
      case _ => false
    }
  }

  def getUnitOfVariable(x : Variable) : MeasureUnit = {
    varMap((x.name, x.index))._3
  }

  def getUnitOfTerm(tm : Term) : Option[MeasureUnit] = {
    tm match {
      case Variable(n, i, _) => varMap.get((n, i)) match {
        case Some((_, _, u)) => Some(u)
        case None => None
      }
      /* @todo time is implicitly a unit in all programs, because it's necessary for this */
      case DifferentialSymbol(Variable(n, i, _)) => varMap.get((n, i)) match {
        case Some((_, _, u)) => Some(u) // @todo this is wrong, it should be divided by time
        case None => None
      }
      case Number(_) => Some(AnyUnit)
      case FuncOf(_, _) => Some(AnyUnit) // @todo this is not right, but it's not like we've handled functions elsewhere either...
      // a bunch of internal (I think) cases omitted
      case Neg(c) => getUnitOfTerm(c)
      case Plus(l, r) => {
        (getUnitOfTerm(l), getUnitOfTerm(r)) match {
          case (Some(lu), Some(ru)) => if(unitsEqual(lu, ru)) Some(lu) else None
          // @todo suboptimal?: if the left unit is less specific (AnyUnit), the code takes it as the unit anyway
          // actually, I think the other way of doing it rejects too many valid programs... think harder about this
          case _ => None
        }
      }
      case Minus(l, r) => {
        (getUnitOfTerm(l), getUnitOfTerm(r)) match {
          case (Some(lu), Some(ru)) => if(unitsEqual(lu, ru)) Some(lu) else None
          case _ => None
        }
      }
      case Times(l, r) => {
        (getUnitOfTerm(l), getUnitOfTerm(r)) match {
          case (Some(u1), Some(u2)) => {
            Some(multiplyUnits(u1, u2))
          }
          case _ => None
        }
      }
      case Divide(l, r) => {
        (getUnitOfTerm(l), getUnitOfTerm(r)) match {
          case (Some(u1), Some(u2)) => Some(divideUnits(u1, u2))
          case _ => None
        }
      }
      // I'm making the assumption that r is always a Number : Term here, and also that the Number can be truncated to an Int
      // the comment at Expression.scala:164 makes this seem ok to do. Hopefully that's right!
      // @todo upon further consideration, this assumption is bad and wrong
      case Power(l, r) => {
        getUnitOfTerm(l) match {
          case Some(u) => exponentiateUnit(u, r)
          case None => None
        }
      }
      case Differential(c) => getUnitOfTerm(c) match {
        case None => None
        case Some(u) => Some(divideUnits(u, UnitUnit("s")))
      }
      case Pair(l, r) => None // oh man what do we do here? can you write keymaerax formulas on pairs?
      case _ => None // wat
    }
  }

  def exponentiateUnit(u : MeasureUnit, exp : Term) : Option[MeasureUnit] = {
    exp match {
      case Number(n) => u match {
        // XXX intValue??? probably wrong, see above
        case UnitUnit(s) => Some(ProductUnit(Map().withDefaultValue(0) + (s -> n.intValue())))
        case ProductUnit(m) => Some(ProductUnit(m.mapValues(e => e * n.intValue())))
        case AnyUnit => Some(AnyUnit) // really?
        case NoUnit => Some(NoUnit) // is it an error to try and exponentiate the unit of a dimensionless quantity? THINK HARDER
      }
      case _ => None
    }
  }

  /**
    * Multiplies units and returns the new unit
    * @param u1 the first unit
    * @param u2 the second unit
    * @return the product of the units
    */
  def multiplyUnits(u1 : MeasureUnit, u2 : MeasureUnit) : MeasureUnit = {
    (u1, u2) match {
      case (AnyUnit, _) => u2 // actually what does it mean to be multiplied by any unit? I don't know, so I'm going to say nothing happens
      case (_, AnyUnit) => u1
      case (NoUnit, _) => u2
      case (_, NoUnit) => u1
      case (UnitUnit(s1), UnitUnit(s2)) => {
        // XXX is this a good idea? what if someone else creates a ProductUnit map without using withDefaultValue(0)?
        // maybe we should just use getOrElse instead?
        val result1 = Map().withDefaultValue(0) + (s1 -> 1)
        ProductUnit(result1 + (s2 -> (result1.getOrElse(s2, 0)+1)))
      }
      case (ProductUnit(m), UnitUnit(s)) => ProductUnit(m + (s -> (m.getOrElse(s, 0)+1)))
      case (UnitUnit(s), ProductUnit(m)) => ProductUnit(m + (s -> (m.getOrElse(s, 0)+1)))
      case (ProductUnit(m1), ProductUnit(m2)) => {
        ProductUnit(m1.map({ case (s, e) => (s, e + m2.getOrElse(s, 0)) }))
      }
    }
  }

  /**
    * Divides units and returns the new unit
    * @param u1 the first unit
    * @param u2 the second unit
    * @return the quotient of the units
    */
  def divideUnits(u1 : MeasureUnit, u2 : MeasureUnit) : MeasureUnit = {
    (u1, u2) match {
      case (AnyUnit, _) => u2 // actually what does it mean to be multiplied by any unit? I don't know, so I'm going to say nothing happens
      case (_, AnyUnit) => u1
      case (NoUnit, _) => u2
      case (_, NoUnit) => u1
      case (UnitUnit(s1), UnitUnit(s2)) => {
        // XXX is this a good idea? what if someone else creates a ProductUnit map without using withDefaultValue(0)?
        // maybe we should just use getOrElse instead?
        val result1 = Map().withDefaultValue(0) + (s1 -> 1)
        ProductUnit(result1 + (s2 -> (result1.getOrElse(s2, 0)-1)))
        // "weed out" units that lookup to 0?
      }
      case (ProductUnit(m), UnitUnit(s)) => ProductUnit(m + (s -> (m(s)-1)))
      case (UnitUnit(s), ProductUnit(m)) => {
        val m2 = m.mapValues(e => -e)
        ProductUnit(m2 + (s -> (m2.getOrElse(s, 0)+1)))
      }
      case (ProductUnit(m1), ProductUnit(m2)) => {
        ProductUnit(m1.map({ case (s, e) => (s, e - m2.getOrElse(s, 0)) }))
      }
    }
  }

  /* unit-checking formulas after parsing
   * @todo this probably needs more parameters, like formula variables, passed in,
   * because we need to take set differences when within the scope of a quantifier
   * or hey, we can just call fmlVars
   * but we still need a typing context to tell us the unit of variables
   * XXX see comment at Expression.scala:181
   */
  def unitAnalysis(e : Formula) : Option[String] = {
    e match {
      case True => None // always fine, right???
      case False => None
      case Equal(l, r) => (getUnitOfTerm(l), getUnitOfTerm(r)) match {
        case (Some(lu), Some(ru)) => if(unitsEqual(lu, ru)) None else Some("unit error in = expression")
        case (Some(_), None) => Some("unit error in term on RHS of =")
        case (None, Some(_)) => Some("unit error in term on LHS of =")
        case _ => Some("unit error on both sides of =")
      }
      case NotEqual(l, r) => (getUnitOfTerm(l), getUnitOfTerm(r)) match {
        case (Some(lu), Some(ru)) => if(unitsEqual(lu, ru)) None else Some("unit error in != expression")
        case (Some(_), None) => Some("unit error in term on RHS of !=")
        case (None, Some(_)) => Some("unit error in term on LHS of !=")
        case _ => Some("unit error on both sides of !=")
      }
      case GreaterEqual(l, r) => (getUnitOfTerm(l), getUnitOfTerm(r)) match {
        case (Some(lu), Some(ru)) => if(unitsEqual(lu, ru)) None else Some("unit error in >= expression")
        case (Some(_), None) => Some("unit error in term on RHS of >=")
        case (None, Some(_)) => Some("unit error in term on LHS of <=\n Problematic term is " + l.toString)
        case _ => Some("unit error on both sides of >=")
      }
      case Greater(l, r) => (getUnitOfTerm(l), getUnitOfTerm(r)) match {
        case (Some(lu), Some(ru)) => if(unitsEqual(lu, ru)) None else Some("unit error in > expression")
        case (Some(_), None) => Some("unit error in term on RHS of >")
        case (None, Some(_)) => Some("unit error in term on LHS of >")
        case _ => Some("unit error on both sides of >")
      }
      case LessEqual(l, r) => (getUnitOfTerm(l), getUnitOfTerm(r)) match {
        case (Some(lu), Some(ru)) => if(unitsEqual(lu, ru)) None else {
          Some("unit error in <= expression\nLHS has unit " + lu.toString + "\nRHS has unit " + ru.toString)
        }
        case (Some(_), None) => Some("unit error in term on RHS of <=")
        case (None, Some(_)) => Some("unit error in term on LHS of <=\n Problematic term is " + l.toString)
        case _ => Some("unit error on both sides of <=")
      }
      case Less(l, r) => (getUnitOfTerm(l), getUnitOfTerm(r)) match {
        case (Some(lu), Some(ru)) => if(unitsEqual(lu, ru)) None else Some("unit error in < expression")
        case (Some(_), None) => Some("unit error in term on RHS of <")
        case (None, Some(_)) => Some("unit error in term on LHS of <")
        case _ => Some("unit error on both sides of <")
      }
      case PredOf(_, _) => None // Some("PredOf???")
      case PredicationalOf(_, _) => None // Some("PredicationalOf???")
      case Not(f) => unitAnalysis(f)
      case And(l, r) => unitAnalysis(l) match {
        case None => unitAnalysis(r)
        case err => err
      }
      case Or(l, r) => unitAnalysis(l) match {
        case None => unitAnalysis(r)
        case err => err
      }
      case Imply(l, r) => unitAnalysis(l) match {
        case None => unitAnalysis(r)
        case err => err
      }
      case Equiv(l, r) => unitAnalysis(l) match {
        case None => unitAnalysis(r)
        case err => err
      }
      case Forall(_, _) => None /* @todo jeez you can also bind variables with units here... */
      case Exists(_, _) => None /* @todo see above comment */
      case Box(p, f) => programAnalysis(p) match {
        case None => unitAnalysis(f)
        case Some(error) => Some(error)
      }
      case Diamond(p, f) => programAnalysis(p) match {
        case None => unitAnalysis(f)
        case Some(error) => Some(error)
      }
      case DifferentialFormula(f) => unitAnalysis(f) // I think this is correct?
    }
  }

  def programAnalysis(p : Program) : Option[String] = {
    p match {
      case Assign(x, e) => getUnitOfTerm(e) match {
        case None => Some("term " + e.toString + " did not have a proper unit!")
        case Some(eu) => if(unitsEqual(getUnitOfVariable(x), eu)) None
          else Some(x.toString + " had unit " + getUnitOfVariable(x).toString + " but " + e.toString + " had unit " + eu.toString)
      }
      case DiffAssign(DifferentialSymbol(x), e) => {
        getUnitOfTerm(e) match {
          case None => Some("term " + e.toString + " did not have a proper unit!")
          case Some(eu) => {
            val xu = divideUnits(getUnitOfVariable(x), UnitUnit("s"))
            if(unitsEqual(eu, xu)) None
            else Some(DifferentialSymbol(x).toString + " had unit " + xu.toString + " but " + e.toString + " had unit " + eu.toString)
          }
        }
      }
      case AssignAny(_) => None // random assignment is basically AnyUnit
      case Test(f) => unitAnalysis(f)
      case Choice(l, r) => programAnalysis(l) match {
        case Some(error) => Some(error)
        case None => programAnalysis(r)
      }
      case Compose(l, r) => programAnalysis(l) match {
        case Some(error) => Some(error)
        case None => programAnalysis(r)
      }
      case Loop(pr) => programAnalysis(pr)
      case Dual(pr) => programAnalysis(pr)
      // Differential programs
      case ODESystem(ode, constraint) => programAnalysis(ode) match {
        case Some(error) => Some(error)
        case None => unitAnalysis(constraint)
      }
      case AtomicODE(DifferentialSymbol(x), e) => {
        getUnitOfTerm(e) match {
          case None => Some("term " + e.toString + " did not have a proper unit!")
          case Some(eu) => {
            val xu = divideUnits(getUnitOfVariable(x), UnitUnit("s"))
            if(unitsEqual(eu, xu)) None
            else Some(DifferentialSymbol(x).toString + " had unit " + xu.toString + " but " + e.toString + " had unit " + eu.toString)
          }
        }
      }
      case DifferentialProduct(l, r) => None // @todo wtf is this???
      case _ => Some("wat")
    }
  }
}
