/**
 * Copyright (c) Carnegie Mellon University. CONFIDENTIAL
 * See LICENSE.txt for the conditions of this license.
 */
package edu.cmu.cs.ls.keymaerax.codegen

import java.io.{FileWriter, File}

import com.wolfram.jlink.Expr
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.launcher.DefaultConfiguration
import edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXPrettyPrinter
import edu.cmu.cs.ls.keymaerax.tools.{KeYmaeraToMathematica, JLinkMathematicaLink, MathematicaToKeYmaera}

import scala.collection.immutable.Map

/**
 * Created by ran on 7/3/15.
 * @author Ran Ji
 */
class Hcol {
  private var constTbl : String = ""
  private var monitor : String  = ""
  private var coefficientHex : String = ""

  def getConstTbl = this.constTbl
  def setConstTbl(ct : String) = this.constTbl = ct
  def getMonitor = this.monitor
  def setMonitor(mnt : String) = this.monitor = mnt
  def getCoefficientHex = this.coefficientHex
  def setCoefficientHex(ceh : String) = this.coefficientHex = ceh
}

class SpiralGenerator extends CodeGenerator {
  def apply(kExpr: Expression): String = apply(kExpr, Nil, "")._1
  def apply(kExpr: Expression, fileName: String) : String = apply(kExpr, Nil, fileName)._1
  def apply(kExpr: Expression, vars: List[Variable], fileName: String): (String, String) = generateSpiralMonitor(kExpr, vars, fileName)

  private val hcol = new Hcol

  // marthematica setting
  private val mathematicaConfig: Map[String, String] = DefaultConfiguration.defaultMathematicaConfig
  private val link : JLinkMathematicaLink= new JLinkMathematicaLink()
  link.init(mathematicaConfig.apply(mathematicaConfig.keys.head), None)

  // name counter used for generating new vector names
  private var vecNameCounter : Int = 0

  // printigs for monitor generation
  private def infoG(fileName: String) =
    "# *************************\n" +
      {if(fileName.nonEmpty) "* " + fileName + ".g\n" + "* \n" else "" } +
      "# Generated by KeYmaera X\n" +
      "# *************************\n"

  private val libs = "# imports\n" +
    "Import(spl, formgen, code, rewrite, transforms, search, compiler, sigma, platforms, platforms.sse, paradigms.common, paradigms.vector, paradigms.cache, libgen, backend);\n" +
    "ImportAll(packages.hacms);\n" +
    "ImportAll(paradigms.vector);\n\n"

  private val monDec = "# monitor\n" +
    "t := \n"

  // printings for coefficientHex generation
  private def infoH(fileName: String) =
    "/**************************\n" +
      {if(fileName.nonEmpty) "* " + fileName + ".h\n" + "* \n" else "" } +
      "* Generated by KeYmaera X\n" +
      "**************************/\n\n"

  private def ifnDef(fileName: String) = "#ifndef _" + {if(fileName.nonEmpty) fileName.toUpperCase else "VECTOR"} + "_H_\n"
  private def define(fileName: String) = "#define _" + {if(fileName.nonEmpty) fileName.toUpperCase else "VECTOR"} + "_H_\n"
  private val endIf = "#endif"


  def generateSpiralMonitorFile(kExpr: Expression, vars: List[Variable], fileName: String) : File = {
    val gFileTempDir = System.getProperty("user.home") + File.separator + ".keymaerax"
    val gFile = new File(gFileTempDir, fileName)
    val writer = new FileWriter(gFile)
    writer.write(generateSpiralMonitor(kExpr, vars, fileName)._1)
    writer.flush()
    writer.close()
    gFile
  }

  def generateSpiralHeaderFile(kExpr: Expression, vars: List[Variable], fileName: String) : File = {
    val hFileTempDir = System.getProperty("user.home") + File.separator + ".keymaerax"
    val hFile = new File(hFileTempDir, fileName)
    val writer = new FileWriter(hFile)
    writer.write(generateSpiralMonitor(kExpr, vars, fileName)._2)
    writer.flush()
    writer.close()
    hFile
  }

  /**
   * generate Spiral monitor
   *
   * @param kExpr KeYmaera X arithmetic expression got from modelPlex
   * @param vars  a list of variables
   * @param fileName  file name
   * @return .g code and .h code
   */
  def generateSpiralMonitor(kExpr: Expression, vars: List[Variable], fileName: String) : (String, String) = {
    val polynomialMode = vars.nonEmpty
    val spiralMonitor = compileToSpiral(kExpr, vars)
    hcol.setMonitor(spiralMonitor)
    val gCode = infoG(fileName) + libs + {if(polynomialMode) "# declare constant table\n" + hcol.getConstTbl else ""} + monDec + hcol.getMonitor + "\n;\n"
    val hCode =
      if(polynomialMode) infoH(fileName) + ifnDef(fileName) + define(fileName) + hcol.getCoefficientHex + endIf
      else ""
    (gCode, hCode)
  }

  private def compileToSpiral(e: Expression, vars: List[Variable]) = e match {
    case t : Term => compilePolynomialTerm(t, vars)
    case f : Formula => compileFormula(f, vars)
    case _ => ???
  }

  private def compileTerm(t: Term) : String = {
    require(t.sort == Real || t.sort == Unit, "can only deal with reals not with sort " + t.sort)
    t match {
      case Neg(c) => "neg(" + compileTerm(c) + ")"
      case Plus(l, r) => "add(" + compileTerm(l) + ", " + compileTerm(r) + ")"
      case Minus(l, r) => "sub(" + compileTerm(l) + ", " + compileTerm(r) + ")"
      case Times(l, r) => "mul(" + compileTerm(l) + ", " + compileTerm(r) + ")"
      case Divide(l, r) => "div(" + compileTerm(l) + ", " + compileTerm(r) + ")"
      case Power(l, r) => "pow(" + compileTerm(l) + ", " + compileTerm(r) + ")"
      // atomic terms
      case Number(n) =>
        assert(n.isValidDouble || n.isValidLong, throw new CodeGenerationException("Term " + KeYmaeraXPrettyPrinter(t) + " contains illegal numbers"))
        "TReal.value(" + n.underlying().toString + ")"
      case t: Variable =>
        if(t.index.isEmpty) t.name
        else t.name + "_" + t.index.get
      case FuncOf(fn, child) =>
        if(child.equals(Nothing)) fn.name
        else fn.name match {
          case "Abs" => "abs(" + compileTerm(child) + ")"
          case "DChebyshev" => "TDistance(TInfinityNorm(" + "2" + "))"  //hack for InfinityNorm of degree 2
          case _ => fn.name + "(" + compileTerm(child) + ")"
        }
      // otherwise exception
      case _ => throw new CodeGenerationException("Conversion of term " + KeYmaeraXPrettyPrinter(t) + " is not defined")
    }
  }

  private def compileFormula(f: Formula, vars: List[Variable]) : String = {
    f match {
      // sub terms are formulas
      case Not(ff) => "logic_neg(" + compileFormula(ff, vars) + ")"
      case And(l, r) => "logic_and(" + compileFormula(l, vars) + ", " + compileFormula(r, vars) + ")"
      case Or(l, r) => "logic_or(" + compileFormula(l, vars) + ", " + compileFormula(r, vars) + ")"
      case Imply(l, r) => "logic_impl(" + compileFormula(l, vars) + ", " + compileFormula(r, vars) + ")"
      case Equiv(l, r) => "logic_equiv(" + compileFormula(l, vars) + ", " + compileFormula(r, vars) + ")"

      // sub terms are arithmetic terms
      case Equal(l, r) => "TEqual(" + compilePolynomialTerm(l, vars) + ", " + compilePolynomialTerm(r, vars) + ")"
      case NotEqual(l, r) => "TNotEqual(" + compilePolynomialTerm(l, vars) + ", " + compilePolynomialTerm(r, vars) + ")"
      case Greater(l,r) => "TLess(" + compilePolynomialTerm(r, vars) + ", " + compilePolynomialTerm(l, vars) + ")"
      case GreaterEqual(l,r) => "TLessEqual(" + compilePolynomialTerm(r, vars) + ", " + compilePolynomialTerm(l, vars) + ")"
      case Less(l,r) => "TLess(" + compilePolynomialTerm(l, vars) + ", " + compilePolynomialTerm(r, vars) + ")"
      case LessEqual(l,r) => "TLessEqual(" + compilePolynomialTerm(l, vars) + ", " + compilePolynomialTerm(r, vars) + ")"
      case True => "true"
      case False => "false"
      case Box(_, _) | Diamond(_, _) => throw new CodeGenerationException("Conversion of Box or Diamond modality is not allowed")
      case _ => throw new CodeGenerationException("Conversion of formula " + KeYmaeraXPrettyPrinter(f) + " is not defined")
    }
  }

  private def compilePolynomialTerm(t: Term, vars: List[Variable]) : String = {
    // only convert to mathVar if the relevant is relevant (appeared) in t
    val allSortedNames = StaticSemantics.symbols(t).toList.sorted
    var mathVarsRelevant = List[Expr]()
    for(i <- vars.indices) {
      if(allSortedNames.contains(vars.apply(i)))
        mathVarsRelevant = KeYmaeraToMathematica.fromKeYmaera(vars.apply(i)) :: mathVarsRelevant
    }
    if(mathVarsRelevant.isEmpty) {    // no relevant variable, then t is not polynomial
      compileTerm(t)
    } else {    // t is polynomial
      assert(mathVarsRelevant.nonEmpty)
      // sort mathVars
      val mathVarsSorted = mathVarsRelevant.sortWith(_.toString < _.toString)
      val mathTerm = KeYmaeraToMathematica.fromKeYmaera(t)
      if(mathVarsSorted.length==1) {   // one relevant variable, t is single polynomial
        compileSinglePoly(mathTerm, mathVarsSorted)
      } else {    // more than one relevant variable, t is multi polynomial
        compileMultiPoly(mathTerm, mathVarsSorted)
      }
    }
  }

  private def compileSinglePoly(mathTerm: Expr, mathVarsSorted: List[Expr]) : String = {
    val resCoeffList = computeCoeff(mathTerm, mathVarsSorted)
    println("resCoeffList is: " + resCoeffList)
    val resCoeffListLength = resCoeffList.length()
    var coeffVector: Array[Expression] = new Array[Expression](resCoeffListLength)
    val coeffVectorDouble: Array[Double] = new Array[Double](resCoeffListLength)
    var typeDouble = true
    for(i <- 0 until resCoeffListLength) {
      coeffVector(i) = MathematicaToKeYmaera.fromMathematica(resCoeffList.args().apply(resCoeffListLength - 1 - i))
      coeffVector(i) match {
        case Number(n) => coeffVectorDouble(i) = n.underlying().doubleValue()
        case _ => typeDouble = false
      }
    }
    val vectorName = getNewVecName
    val ct: String = hcol.getConstTbl.concat(vectorName + " := var(\"" + vectorName + "\", TPtr(T_Real(64)));\n")
    hcol.setConstTbl(ct)
    if(typeDouble) {
      compilePolyCoeffDouble(vectorName, coeffVectorDouble, 1)
    } else compilePolyCoeff(vectorName, coeffVector, 1)
  }

  private def compileMultiPoly(mathTerm: Expr, mathVarsSorted: List[Expr]) : String = {
    // todo
    ???
//    val resCoeffMap = computeCoeff(mathTerm, mathVarsSorted)
//    println("resCoeffMap for mp is " + resCoeffMap)
//    val varNumber = mathVarsSorted.length
//    println("varNumber is " + varNumber)
//    val maxDegree = 2
//    val length = 1 + varNumber*maxDegree + maxDegree*(maxDegree-1)/2 // length of the vector
//    println("multi polynomial coefficient vector length is " + length)
//    ""
  }

  private def computeCoeff(t: Expr, vars: List[Expr]) : Expr = {
    var mathCmd = ""
    if(vars.length==1)
      mathCmd = "CoefficientList[" + t + "," + vars.head + "]"
    else {
      assert(vars.length > 1)
      var varList = ""
      for(i <- vars.indices) {
        varList += vars.apply(i).toString
        if(i != vars.length-1) varList += ", "
      }
      println("varList is " + varList)
      mathCmd = "CoefficientRules[" + t + "," + "{" + varList + "}" + "]"
    }
    link.ml.evaluate(mathCmd)
    link.ml.waitForAnswer()
    link.ml.getExpr
  }

  private def getNewVecName : String = {
    val name = "d_"+vecNameCounter
    vecNameCounter+=1
    name
  }

  private def compilePolyCoeffDouble(vectorName: String, polyCoeff: Array[Double], varNumber: Int) : String = {
    if(varNumber == 1) {
      val degree = polyCoeff.length - 1
      val ceh = hcol.getCoefficientHex.concat("\n__int64 " + vectorName + "[] = {\n" + translateToCoeffsHex(polyCoeff) + "\n};\n")
      hcol.setCoefficientHex(ceh)
      "TEvalPolynomial(" + degree + "," + vectorName + ")"
    } else {
      val degree = 2  // todo
      assert(varNumber>1)
      "TEvalMultiPolynomial(" + degree + "," + varNumber + "," + vectorName + ")"
    }
  }

  private def compilePolyCoeff(vectorName: String, polyCoeff: Array[Expression], varNumber: Int) : String = {
    if(varNumber == 1) {
      val degree = polyCoeff.length - 1
      val ceh = hcol.getCoefficientHex.concat("\n__int64 " + vectorName + "[] = {\n" + translateToCoeffs(polyCoeff) + "\n};\n")
      hcol.setCoefficientHex(ceh)
      "TEvalPolynomial(" + degree + "," + vectorName + ")"
    } else {
      val degree = 2 // todo
      assert(varNumber>1)
      "TEvalMultiPolynomial(" + degree + "," + varNumber + "," + vectorName + ")"
    }
  }

  private def coeffsHex(coeffVecList: List[(String, Array[Double])]) : String = {
    var coeffDec = ""
    for(i <- coeffVecList.indices) {
      val key = coeffVecList(i)._1
      coeffDec += "\n__int64 " + key + "[] = {\n" + translateToCoeffsHex(coeffVecList(i)._2) + "\n};\n"
    }
    coeffDec
  }

  private def translateToCoeffs(coeffVec: Array[Expression]) : String = {
    var coeffsHex = ""
    for(i <- coeffVec.indices) {
      coeffsHex += "  " + KeYmaeraXPrettyPrinter(coeffVec(i))
      if(i != coeffVec.length-1) coeffsHex += ",\n"
    }
    coeffsHex
  }

  private def translateToCoeffsHex(coeffVec: Array[Double]) : String = {
    var coeffsHex = ""
    for(i <- coeffVec.indices) {
      coeffsHex += "  " + translateToHex(coeffVec(i))
      if(i != coeffVec.length-1) coeffsHex += ",\n"
    }
    coeffsHex
  }

  private def translateToHex(value: Double) = "0x" + java.lang.Long.toHexString(java.lang.Double.doubleToRawLongBits(value))
}



