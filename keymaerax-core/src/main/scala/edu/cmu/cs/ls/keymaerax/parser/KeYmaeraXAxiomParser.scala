package edu.cmu.cs.ls.keymaerax.parser

import edu.cmu.cs.ls.keymaerax.core.Formula
import edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXLexer.TokenStream

/**
 * Created by nfulton on 6/11/15.
 */
object KeYmaeraXAxiomParser extends (String => List[(String, Formula)]) {
  def apply(s: String) = parse(KeYmaeraXLexer.lexAxiomFile(s))


  /**
   * Very simple -- just read until AXIOM_END.
   * @param input Token string for the axiom file.
   * @return A list of axiom names and the associated formulas.
   */
  def parse(input: TokenStream): List[(String, Formula)] = {
    require(input.endsWith(List(Token(EOF))), "token streams have to end in " + EOF)
    require(input.head.tok.equals(AXIOM_BEGIN), "expected ALP file to begin with Axion block.")
    val (nextAxiom, nextFormula, remainder) = parseNextAxiom(input)
    if(remainder.length == 1 && remainder.head.tok.equals(EOF))
      List((nextAxiom, nextFormula))
    else
      (nextAxiom, nextFormula) +: parse(remainder)
  }

  def parseNextAxiom(input: TokenStream): (String, Formula, TokenStream) = {
    require(input.head.tok.equals(AXIOM_BEGIN), "expected ALP file to begin with Axion block.")
    require(input.tail.head.tok.isInstanceOf[AXIOM_NAME])

    val name = input.tail.head match {
      case Token(AXIOM_NAME(x),_) => x
      case _ => throw new Exception("Require should have failed.")
    }
    //Find the End. token and exclude it.
    val (axiomTokens, remainderTokens) =
      input.tail.tail.span(x => !x.tok.equals(AXIOM_END)) //1st element is AXIOM_BEGIN, 2nd is AXIOM_NAME.

    val axiom = KeYmaeraXParser.parse(axiomTokens :+ Token(EOF, UnknownLocation)) match {
      case axiomAsFormula : Formula => axiomAsFormula
      case _ => throw new Exception("Parsed a non-formula in an Axiom context.")
    }

    (name, axiom, remainderTokens.tail)
  }

}