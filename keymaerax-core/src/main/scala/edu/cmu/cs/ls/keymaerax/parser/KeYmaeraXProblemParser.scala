/**
* Copyright (c) Carnegie Mellon University.
* See LICENSE.txt for the conditions of this license.
*/
package edu.cmu.cs.ls.keymaerax.parser

import edu.cmu.cs.ls.keymaerax.core._

import scala.annotation.tailrec

/** The return from a call to KeYmaeraXProblemParser
  * @param variableMap A map containing a mapping from variable name/index pairs to variable sorts, where the sort is a triple:
  *                        _1: (Optionally) the domain sort of a function
  *                        _2: The sort of a ProgramVariable, or the codomain sort of a function.
  *                        _3: The unit of measure of a ProgramVariable
  * @param unitMap A map containing a mapping from unit names to a map containing further information about the unit, viz
  *                  * units the unit named is "related to", in the sense that they will match on opposite sides of an = sign
  *                    (this is stored as a MeasureUnit, but we expect everything here to be a UnitUnit or a ProductUnit)
  *                    @todo I don't know how these `insist` things work, but could we use that here?
  *                  * conversion factors corresponding to said unit
  */
case class KeYmaeraXProblemParserResult (variableMap : Map[(String, Option[Int]), (Option[Sort], Sort, MeasureUnit)]
                                        ,unitMap     : Map[String, (Map[MeasureUnit, Int])])
/**
 * Parses .kyx files.
 * @todo check sorts
 * Created by nfulton on 6/12/15.
 * Modified by vincenth on 4/26/16.
 */
object KeYmaeraXProblemParser {
  def apply(input : String): (Formula, KeYmaeraXProblemParserResult) =
    try {
      firstNonASCIICharacter(input) match {
        case Some(pair) => throw ParseException(s"Input string contains non-ASCII character ${pair._2}", pair._1)
        case None => {
          val (m, f) = parseProblem(KeYmaeraXLexer.inMode(input, ProblemFileMode()))
          (f, m)
        }
      }
    }
    catch {case e: ParseException => throw e.inInput(input)}

  /** Returns the location and value of the first non-ASCII character in a string. */
  private def firstNonASCIICharacter(s : String) : Option[(Location, Char)] = {
    val pattern = """([^\p{ASCII}])""".r
    val matches = pattern.findAllIn(s).matchData

    if(matches.nonEmpty) {
      val nonAsciiCharacter : Char = {
        if(matches.hasNext) matches.next().group(0).toCharArray.last
        else throw new Exception("Expected at least one match but matchData.hasNext returned false when matches.nonEmpty was true!")
      }
      val prefix = s.split(nonAsciiCharacter).head
      val lines = prefix.split("\n")
      val lineNumber = lines.length
      val columnNumber = lines.last.length + 1
      Some(new Region(lineNumber, columnNumber, lineNumber, columnNumber), nonAsciiCharacter)
    }
    else {
      assert(s.matches("\\A\\p{ASCII}*\\z"))
      None
    }
  }

  protected def parseProblem(tokens: List[Token]) :  (KeYmaeraXProblemParserResult, Formula) = {
    val parser = KeYmaeraXParser
    // @todo modify the typeAnalysis to deal with the units too?
    val (KeYmaeraXDeclarationsParserResult(decls, units), remainingTokens) = KeYmaeraXDeclarationsParser(tokens)
    checkInput(remainingTokens.nonEmpty, "Problem. block expected", UnknownLocation, "kyx problem input problem block")
    checkInput(remainingTokens.head.tok.equals(PROBLEM_BLOCK), "Problem. block expected", remainingTokens.head.loc, "kyx reading problem block")

    if(decls.keySet.nonEmpty && remainingTokens.head.loc.line <= 1)
      println("WARNING: There were declarations in this file but the non-declaration portion of the file starts at line 0 or line 1. There may be off-by-n errors in location messages.")

    val (theProblem, eof) = remainingTokens.span(x => !x.tok.equals(END_BLOCK))
    checkInput(eof.length == 2 && eof.head.tok.equals(END_BLOCK),
      "Expected Problem block to end with 'End.' but found " + eof,
      if (eof.nonEmpty) eof.last.loc else theProblem.last.loc, "kyx problem end block parser")
    checkInput(eof.length == 2 && eof.last.tok.equals(EOF),
      "Expected Problem block to be last in file, but found " + eof,
      if (eof.nonEmpty) eof.last.loc else theProblem.last.loc, "kyx problem end block parser")

    val problem : Formula = parser.parse(theProblem.tail :+ Token(EOF, UnknownLocation)) match {
      case f : Formula => f
      case expr : Expression => throw ParseException("problem block" + ":" + "Expected problem to parse to a Formula", expr)
    }

    parser.semanticAnalysis(problem) match {
      case None =>
      case Some(error) => throw ParseException("Semantic analysis error\n" + error, problem)
    }

    KeYmaeraXDeclarationsParser.typeAnalysis(decls, problem) //throws ParseExceptions.

    val declsWithoutStartToks = decls.mapValues(v => (v._1, v._2, v._3))
    val result = KeYmaeraXProblemParserResult(declsWithoutStartToks, units)
    val checker = UnitChecker(result)
    checker.unitAnalysis(problem) match {
      case None =>
      case Some(error) => throw ParseException("Unit analysis error\n" + error, problem)
    }
    (result, problem)
  }
}

/** The return from a call to KeYmaeraXDeclarationsParser
  * @param variableMap A map containing a mapping from variable name/index pairs to variable sorts, where the sort is a triple:
  *                        _1: (Optionally) the domain sort of a function
  *                        _2: The sort of a ProgramVariable, or the codomain sort of a function.
  *                        _3: The unit of measure of a ProgramVariable
  *                        _4: The token that starts the declaration.
  * @param unitMap A map containing a mapping from unit names to a map containing further information about the unit, viz
  *                  * units the unit named is "related to", in the sense that they will match on opposite sides of an = sign
  *                    (this is stored as a MeasureUnit, but we expect everything here to be a UnitUnit or a ProductUnit)
  *                  * conversion factors corresponding to said unit
  */
case class KeYmaeraXDeclarationsParserResult (variableMap : Map[(String, Option[Int]), (Option[Sort], Sort, MeasureUnit, Token)]
                                             ,unitMap     : Map[String, (Map[MeasureUnit, Int])])

/**
 * Parses the declarations in .kyx and .alp files.
 */
object KeYmaeraXDeclarationsParser {
  /**
   *
   * @param tokens The tokens to parse
   * @return A pair containing:
   *          _1: A mapping from variable name/index pairs to variable sorts, where the sort is a triple:
   *              _1: (Optionally) the domain sort of a function
   *              _2: The sort of a ProgramVariable, or the codomain sort of a function.
   *              _3: The unit of measure of a ProgramVariable @todo split this stuff off into a separate map (also important for namespacing units and variables)
   *              _4: The token that starts the declaration.
   *          _2: The list of remaining tokens.
   */
  def apply(tokens : List[Token]) : (KeYmaeraXDeclarationsParserResult, List[Token]) =
    parseDeclarations(tokens)

  def parseDeclarations(tokens: List[Token]): (KeYmaeraXDeclarationsParserResult, List[Token]) = {
    if(tokens.head.tok.equals(PROGRAM_UNITS_BLOCK)) {
      val (programUnits, remainder1) = processProgramUnits(tokens)
      val (programVariables, remainder2) = processProgramVariables(remainder1)
      val (functions, finalRemainder) = processFunctionSymbols(remainder2)
      (KeYmaeraXDeclarationsParserResult(programVariables ++ functions, programUnits), finalRemainder)
    }
    else if(tokens.head.tok.equals(PROGRAM_VARIABLES_BLOCK)) {
      val (programVariables, remainder) = processProgramVariables(tokens)
      val (functions, finalRemainder) = processFunctionSymbols(remainder)
      (KeYmaeraXDeclarationsParserResult(programVariables ++ functions, Map()), finalRemainder)
    }
    else if(tokens.head.tok.equals(FUNCTIONS_BLOCK)) {
      val (functions, remainder) = processFunctionSymbols(tokens)
      val (programVariables, finalRemainder) = processProgramVariables(remainder)
      (KeYmaeraXDeclarationsParserResult(programVariables ++ functions, Map()), finalRemainder)
    }
    else if(tokens.head.tok.equals(VARIABLES_BLOCK)) {
      val (vars, remainder) = processVariables(tokens)
      (KeYmaeraXDeclarationsParserResult(vars, Map()), remainder)
    }
    else {
      (KeYmaeraXDeclarationsParserResult(Map(), Map()), tokens)
    }
  }

  /**
   * Type analysis of expression according to the given type declarations decls
   * @param decls the type declarations known from the context
   * @param expr the expression parsed
   * @throws [[edu.cmu.cs.ls.keymaerax.parser.ParseException]] if the type analysis fails.
   */
  //@todo now this takes MeasureUnits, maybe it should check some of them too?
  def typeAnalysis(decls: Map[(String, Option[Int]), (Option[Sort], Sort, MeasureUnit, Token)], expr: Expression): Boolean = {
    StaticSemantics.signature(expr).forall(f => f match {
      case f:Function =>
        val (declaredDomain, declaredSort, declaredUnit, declarationToken) = decls.get((f.name,f.index)) match {
          case Some(d) => d
          case None => throw ParseException("type analysis" + ": " + "undefined symbol " + f, f)
        }
        if(f.sort != declaredSort) throw ParseException(s"type analysis: ${f.prettyString} declared with sort ${declaredSort} but used where sort ${f.sort} was expected.", declarationToken.loc)
        else if (f.domain != declaredDomain.get) {
          (f.domain, declaredDomain) match {
            case (l, Some(r)) => throw ParseException(s"type analysis: ${f.prettyString} declared with domain ${r} but used where domain ${f.domain} was expected.", declarationToken.loc)
            case (l, None) => throw ParseException(s"type analysis: ${f.prettyString} declared as a non-function but used as a function.", declarationToken.loc)
            //The other cases can't happen -- we know f is a function so we know it has a domain.
          }
        }
        else true
      case _ => true
    }) &&
    StaticSemantics.vars(expr).symbols.forall(x => x match {
      case x: Variable =>
        val (declaredSort, declarationToken) = decls.get((x.name,x.index)) match {
          case Some((None,d,u,token)) => (d, token)
          case None => throw ParseException("type analysis" + ": " + "undefined symbol " + x + " with index " + x.index, x)
        }
        if(x.sort != declaredSort) throw ParseException(s"type analysis: ${x.prettyString} declared with sort ${declaredSort} but used where a ${x.sort} was expected.", declarationToken.loc)
        x.sort == declaredSort
      case _ => true
    })
  }


  /**
    *
    * @param tokens Tokens in the parsed file.
    * @return A pair:
    *           _1: A map containing declared units
    *           _2: The remainder of the file
    * @note The unit "s" (seconds) is implicitly always there, even if the user doesn't declare it
    * @todo Should we now be concerned about the `t` variable implicitly introduced by KeYmaera X?
    */
  def processProgramUnits(tokens: List[Token]) : (Map[String, Map[MeasureUnit, Int]], List[Token]) = {
    val initial = Map() + ("s" -> Map())
    if(tokens.head.tok.equals(PROGRAM_UNITS_BLOCK)) {
      val (progUnitsSection, remainder) = tokens.span(x => !x.tok.equals(END_BLOCK))
      val progUnitsTokens = progUnitsSection.tail
      // dirty hack to avoid having to rewrite processDeclarations
      val tempMap = processDeclarations(progUnitsTokens, Map())
      val progUnitsMap = tempMap.map({ case ((u, _), (_, UnitOfMeasure, AnyUnit, _)) => (u, Map() : Map[MeasureUnit, Int]) })
      (progUnitsMap + ("s" -> Map()), remainder.tail)
    }
    else (Map() + ("s" -> Map()), tokens)
  }

  /**
    *
    * @param tokens Tokens in the parsed file.
    * @return A pair:
    *          _1: The list of Named Symbols.
    *          _2: The remainder of the file.
    */
  def processProgramVariables(tokens : List[Token]): (Map[(String, Option[Int]), (Option[Sort], Sort, MeasureUnit, Token)], List[Token]) = {
    if(tokens.head.tok.equals(PROGRAM_VARIABLES_BLOCK)) {
      val(progVarsSection, remainder) = tokens.span(x => !x.tok.equals(END_BLOCK))
        val progVarsTokens = progVarsSection.tail
        (processDeclarations(progVarsTokens, Map()), remainder.tail)
    }
    else (Map(), tokens)

  }

  def processFunctionSymbols(tokens: List[Token]) : (Map[(String, Option[Int]), (Option[Sort], Sort, MeasureUnit, Token)], List[Token]) = {
    if(tokens.head.tok.equals(FUNCTIONS_BLOCK)) {
      val(funSymbolsTokens, remainder) = tokens.span(x => !x.tok.equals(END_BLOCK))
      val funSymbolsSection = funSymbolsTokens.tail
      (processDeclarations(funSymbolsSection, Map()), remainder.tail)
    }
    else (Map(), tokens)
  }

  def processVariables(tokens: List[Token]) : (Map[(String, Option[Int]), (Option[Sort], Sort, MeasureUnit, Token)], List[Token]) = {
    if(tokens.head.tok.equals(VARIABLES_BLOCK)) {
      val(funSymbolsTokens, remainder) = tokens.span(x => !x.tok.equals(END_BLOCK))
      val funSymbolsSection = funSymbolsTokens.tail
      (processDeclarations(funSymbolsSection, Map()), remainder.tail)
    }
    else (Map(), tokens)
  }


  /*
   *                                              parseFunctionDomainSort
   *                                 ++=========================================++
   *                                 ||                  +-------------+        ||
   *                                 ||                 \/             |        ||
   * InitS --> SortS ---> NameS ---> || OpenParenS ---> ArgSortS ---> CommaS    ||
   *                        |        ||   |                            |        ||
   *                        |        ||   |                            \/       ||
   *                        |        ||   +---------------------- > CloseParen  ||
   *                        |        ++=========================================++
   *                        \/                                         |
   *                     PeriodS <-------------------------------------+
   *
   * And if the machine halts in a non-PeriodS state then it errors.
   */
  private def parseDeclaration(ts : List[Token]) : (((String, Option[Int]), (Option[Sort], Sort, MeasureUnit, Token)), List[Token]) = {
    val sortToken = ts.head
    val sort = parseSort(sortToken, sortToken)

    val nameToken = ts.tail.head
    val nameTerminal : IDENT = ts.tail.head.tok match {
      case i : IDENT => i
      case x => throw new Exception("Expected an identifier but found " + x.getClass().getCanonicalName())
    }

    val afterName = ts.tail.tail //skip over IDENT and REAL/BOOL/UNIT tokens.
    if(afterName.head.tok.equals(LPAREN)) {
      val (domainSort, remainder) = parseFunctionDomainSort(afterName, nameToken)

      checkInput(remainder.last.tok.equals(PERIOD),
        "Expected declaration to end with . but found " + remainder.last, remainder.last.loc, "Reading a declaration")

      (( (nameTerminal.name, nameTerminal.index), (Some(domainSort), sort, AnyUnit, nameToken)), remainder.tail) //@todo this should not be AnyUnit
    }
    else if(afterName.head.tok.equals(PERIOD)) {
      (( (nameTerminal.name, nameTerminal.index) , (None, sort, AnyUnit, nameToken) ), afterName.tail)
    }
    else if(afterName.head.tok.equals(COLON)) {
      val (u, remainder) = parseUnitAnnotation(afterName.tail)
      (( (nameTerminal.name, nameTerminal.index) , (None, sort, u, nameToken) ), remainder)
    }
    else {
      throw new ParseException("Variable/unit declarations should end with a period.", afterName.head.loc, afterName.head.tok.img, ".", "", "declaration parse")
    }
  }

  private def parseUnitAnnotation(tokens : List[Token]) : (MeasureUnit, List[Token]) = {
    /* @todo we really should check whether the unit identifier was declared in ProgramUnits, but that would require lots of changes to pass in the map,
     * so I'm not going to do that - I guess it can be checked at some later stage? I hope... */
    val parser = KeYmaeraXParser // just use this to parse unit annotations, since they can have exponents/products
    val (unitTemp, remainderTemp) = tokens.span(x => !x.tok.equals(PERIOD))
    // numbers are lexed with the final period included when they're at the end of a unit annotation,
    // so this dirty hack is to work around that
    val (unit : List[Token], remainder : List[Token]) = if(remainderTemp.isEmpty) {
      val (ok, List(Token(NUMBER(weird), _))) = unitTemp.splitAt(unitTemp.length-1)
      (ok :+ Token(NUMBER(weird.dropRight(1))), List(PERIOD))
    } else (unitTemp, remainderTemp)
    // make the unit expression into a "formula" so we can use the KeYmaeraXParser to parse it for us
    val hack = unit ++ (Token(EQ) :: unit) :+ Token(EOF)
    val Equal(ut, _) = parser.formulaTokenParser(hack)
    parsedUnitAnnotationToUnit(ut) match {
      case None => throw new ParseException("Invalid unit annotation ", UnknownLocation, "", ".", "", "declaration parse")
      case Some(u) => (u, remainder.tail)
    }
  }

  private def parsedUnitAnnotationToUnit(annot : Term) : Option[MeasureUnit] = {
    // just to use some of the utility functions that don't depend on the result
    // @todo maybe move those utility functions into a separate class
    val checker = UnitChecker(KeYmaeraXProblemParserResult(Map(), Map()))
    annot match {
      case Times(l, r) => (parsedUnitAnnotationToUnit(l), parsedUnitAnnotationToUnit(r)) match {
        case (Some(lu), Some(ru)) => Some(checker.multiplyUnits(lu, ru))
        case _ => None
      }
      case Divide(l, r) => (parsedUnitAnnotationToUnit(l), parsedUnitAnnotationToUnit(r)) match {
        case (Some(lu), Some(ru)) => Some(checker.divideUnits(lu, ru))
        case _ => None
      }
      // @todo again the bad assumption that the exponent is a number...
      case Power(l, e) => parsedUnitAnnotationToUnit(l) match {
        case Some(u) => checker.exponentiateUnit(u, e)
        case None => None
      }
      case Variable(name, _, _) => Some(UnitUnit(name))
      case _ => None
    }
  }

  /**
   *
   * @param tokens A list of tokens that begins like: (R|B, ...).
    *@param of A meaningful token representing the thing whose type is being parsed.
   * @return A pair:
   *          _1: The sort of the entire function,
   *          _2: The reamining tokens.
   */
  private def parseFunctionDomainSort(tokens : List[Token], of: Token) : (Sort, List[Token]) = {
    // Parse the domain sort.
    checkInput(tokens.length > 1, "domain sort expected in declaration", if (tokens.isEmpty) UnknownLocation else tokens.head.loc, "parsing function domain sort")
    checkInput(tokens.head.tok.equals(LPAREN), "function argument parentheses expected", tokens.head.loc, "parsing function domain sorts")

    val splitAtRparen = tokens.tail.span(x => !x.tok.equals(RPAREN))
    val domainElements = splitAtRparen._1

    checkInput(splitAtRparen._2.head.tok.equals(RPAREN),
      "unmatched LPAREN at end of function declaration. Intead, found: " + splitAtRparen._2.head, splitAtRparen._2.head.loc, "parsing function domain sorts")
    val remainder = splitAtRparen._2.tail

    val domain = domainElements.foldLeft(List[Sort]())((list, token) =>
      if(token.tok.equals(COMMA)) list
      else list :+ parseSort(token, of))

    if(domain.isEmpty) {
      (Unit, remainder)
    }
    else {
      val fstArgSort = domain.head
      val domainSort = domain.tail.foldLeft(fstArgSort)( (tuple, next) => Tuple(tuple, next) )
      (domainSort, remainder)
    }
  }

  private def parseSort(sortToken : Token, of: Token) : Sort = sortToken.tok match {
    case edu.cmu.cs.ls.keymaerax.parser.IDENT("R", _) => edu.cmu.cs.ls.keymaerax.core.Real
    case edu.cmu.cs.ls.keymaerax.parser.IDENT("U", _) => edu.cmu.cs.ls.keymaerax.core.UnitOfMeasure
    case edu.cmu.cs.ls.keymaerax.parser.IDENT("P", _) => edu.cmu.cs.ls.keymaerax.core.Trafo
    //@todo do we need a cont. trafo sort to do well-formedness checking?
    case edu.cmu.cs.ls.keymaerax.parser.IDENT("CP", _) => edu.cmu.cs.ls.keymaerax.core.Trafo
    case edu.cmu.cs.ls.keymaerax.parser.IDENT("F", _) => edu.cmu.cs.ls.keymaerax.core.Bool
    case edu.cmu.cs.ls.keymaerax.parser.IDENT("B", _) => edu.cmu.cs.ls.keymaerax.core.Bool
    case edu.cmu.cs.ls.keymaerax.parser.IDENT("T", _) => edu.cmu.cs.ls.keymaerax.core.Real
    case edu.cmu.cs.ls.keymaerax.parser.ANYTHING => edu.cmu.cs.ls.keymaerax.core.Real //Anything extends RTerm
    case edu.cmu.cs.ls.keymaerax.parser.TERM => edu.cmu.cs.ls.keymaerax.core.Real //@todo deprecated -- should be handled by T identifier.
    case edu.cmu.cs.ls.keymaerax.parser.PROGRAM => edu.cmu.cs.ls.keymaerax.core.Trafo //@todo
    case edu.cmu.cs.ls.keymaerax.parser.CP => edu.cmu.cs.ls.keymaerax.core.Trafo //@todo
    case edu.cmu.cs.ls.keymaerax.parser.MFORMULA => edu.cmu.cs.ls.keymaerax.core.Bool //@todo
    case edu.cmu.cs.ls.keymaerax.parser.TEST => edu.cmu.cs.ls.keymaerax.core.Bool //@todo this is getting stupid
    case _ => throw ParseException(s"Error in type declaration statement for ${identTerminalToString(of.tok)}" + ": " + "Expected a sort but found " + sortToken.toString, of.loc)
  }

  /** Turns an IDENT into a string for the sake of error messages only. This is probably replicated in the parser. */
  private def identTerminalToString(terminal: Terminal) = terminal match {
    case i : IDENT => i.name + {i.index match {case Some(x) => "_x" case None => ""}}
    case _ => throw new Exception(s"Expected an IDENT terminal but found ${terminal}")
  }

  private def isSort(terminal: Terminal) = terminal match {
    case REAL => true
    case BOOL => true
    case UNIT => true
    case _    => false
  }

  /**
   * Takes a list of tokens and produces a mapping form names to sorts.
   * @param ts A list of tokens
   * @return A map from variable names and indices to a pair of:
   *          _1: The (optional) domain sort (if this is a function declaration
   *          _2: The sort of the variable, or the codomain sort of a function.
    *         _3: The unit of measure of the variable
   */
  @tailrec
  private def processDeclarations(ts: List[Token],
                                  sortDeclarations: Map[(String, Option[Int]), (Option[Sort], Sort, MeasureUnit, Token)]) : Map[(String, Option[Int]), (Option[Sort], Sort, MeasureUnit, Token)] =
    if(ts.nonEmpty) {
      val (nextDecl, remainders) = parseDeclaration(ts)
      processDeclarations(remainders, sortDeclarations.updated(nextDecl._1, nextDecl._2))
    }
    else {
      sortDeclarations
    }

}