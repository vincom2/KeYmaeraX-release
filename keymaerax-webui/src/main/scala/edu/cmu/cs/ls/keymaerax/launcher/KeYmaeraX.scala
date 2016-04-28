package edu.cmu.cs.ls.keymaerax.launcher

import java.io.{FilePermission, File, PrintWriter}
import java.lang.reflect.ReflectPermission
import java.security.Permission

import edu.cmu.cs.ls.keymaerax.bellerophon.{BelleExpr, BTacticParser}
import edu.cmu.cs.ls.keymaerax.btactics._
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser._
import edu.cmu.cs.ls.keymaerax.tools.{ToolEvidence, Mathematica}
import edu.cmu.cs.ls.keymaerax.codegen.{CseCGenerator, CGenerator, SpiralGenerator}


import scala.collection.immutable
import scala.compat.Platform
import scala.reflect.runtime._
import scala.tools.reflect.{ToolBoxError, ToolBox}
import scala.util.Random

/**
 * Command-line interface for KeYmaera X.
 * @author Stefan Mitsch
 * @author Andre Platzer
 * @author Ran Ji
 */
object KeYmaeraX {

  private type OptionMap = Map[Symbol, Any]

  /** Usage -help information, formatted to 80 characters width. */
  private val usage = "KeYmaera X Prover" + " " + VERSION +
    """
      |
      |Usage: java -Xss20M -jar KeYmaeraX.jar
      |  -prove filename -tactic filename [-out filename] |
      |  -modelplex filename [-vars var1,var2,..,varn] [-out filename] |
      |  -codegen filename -format C|Spiral [-vars var1,var2,..,varn] [-out file] |
      |  -ui [filename] [web server options] |
      |  -parse [filename] |
      |  -bparse [filename]
      |
      |Actions:
      |  -prove     run KeYmaera X prover on given file with given tactic
      |  -modelplex synthesize monitor from given file with ModelPlex prover tactic
      |  -codegen   generate executable code from given file for given target language
      |  -ui        start web user interface with optional file (if any) and arguments
      |  -parse     return error code !=0 if the input model file does not parse
      |  -bparse    return error code !=0 if bellerophon tactic file does not parse
      |
      |Additional options:
      |  -mathkernel MathKernel(.exe) path to the Mathematica kernel executable
      |  -jlink path/to/jlinkNativeLib path to the J/Link native library directory
      |  -verify   generate and check the final proof certificate (recommended)
      |  -noverify skip checking proof certificates after proof search
      |  -interval guard reals by interval arithmetic in floating point (recommended)
      |  -nointerval  skip interval arithmetic presuming no floating point errors
      |  -interactive start a simple command-line prover if -prove fails
      |  -cse      use common subexpression elimination in C code (not recommended)
      |  -dnf      use disjunctive normal form in Spiral code
      |  -vars     use ordered list of variables, treating others as constant functions
      |  -kind     kind of monitor to generate, one of ctrl or model
      |  -lax      enable lax mode with more flexible parser, printer, prover etc.
      |  -strict   enable strict mode with no flexibility in prover
      |  -security enable security manager imposing some security restrictions
      |  -debug    enable debug mode with more exhaustive messages
      |  -nodebug  disable debug mode to suppress intermediate messages
      |  -help     Display this usage information
      |  -license  Show license agreement for using this software
      |
      |Web server options:
      |  -tool mathematica|z3
      |
      |Copyright (c) Carnegie Mellon University.
      |Use option -license to show the license conditions.
      |""".stripMargin

  private def launched() {
    LAUNCH = true;
    println("Launch flag was set.");
  }
  var LAUNCH : Boolean = false;

  def main (args: Array[String]): Unit = {
    println("KeYmaera X Prover " + VERSION + "\n" +
      "Use option -help for usage and license information")
    if (args.length == 0) return launchUI(args)
    if (args.length > 0 && (args(0)=="-help" || args(0)=="--help" || args(0)=="-h")) {println(usage); exit(1)}
    else {
      def makeVariables(varNames: Array[String]): Array[Variable] = {
        varNames.map(vn => KeYmaeraXParser(vn) match {
          case v: Variable => v
          case v => throw new IllegalArgumentException("String " + v + " is not a valid variable name")
        })
      }

      def nextOption(map: OptionMap, list: List[String]): OptionMap = {
        list match {
          case Nil => map
          case "-help" :: _ => {println(usage); exit(1)}
          case "-license" :: _ => {println(license); exit(1)}
          // actions
          case "-parse" :: value :: tail =>
            parseProblemFile(value); ???
          case "-bparse" :: value :: tail =>
            parseBelleTactic(value); ???
          case "-prove" :: value :: tail =>
            if(value.nonEmpty && !value.toString.startsWith("-")) nextOption(map ++ Map('mode -> "prove", 'in -> value), tail)
            else optionErrorReporter("-prove")
          case "-modelplex" :: value :: tail =>
            if(value.nonEmpty && !value.toString.startsWith("-")) nextOption(map ++ Map('mode -> "modelplex", 'in -> value), tail)
            else optionErrorReporter("-modelPlex")
          case "-codegen" :: value :: tail =>
            if(value.nonEmpty && !value.toString.startsWith("-")) nextOption(map ++ Map('mode -> "codegen", 'in -> value), tail)
            else optionErrorReporter("-codegen")
          case "-ui" :: kyxFilePath :: "-tactic" :: tacticPath :: tail =>
            launchUIWithTactic(kyxFilePath, tacticPath, tail.toArray); map ++ Map('mode -> "ui")
          case "-ui" :: tail => launchUI(tail.toArray); map ++ Map('mode -> "ui")
          // action options
          case "-out" :: value :: tail =>
            if(value.nonEmpty && !value.toString.startsWith("-")) nextOption(map ++ Map('out -> value), tail)
            else optionErrorReporter("-out")
          case "-vars" :: value :: tail =>
            if(value.nonEmpty && !value.toString.startsWith("-")) nextOption(map ++ Map('vars -> makeVariables(value.split(","))), tail)
            else optionErrorReporter("-vars")
          case "-format" :: value :: tail =>
            if(value.nonEmpty && !value.toString.startsWith("-")) nextOption(map ++ Map('format -> value), tail)
            else optionErrorReporter("-format")
          case "-tactic" :: value :: tail =>
            if(value.nonEmpty && !value.toString.startsWith("-")) nextOption(map ++ Map('tactic -> value), tail)
            else optionErrorReporter("-tactic")
          case "-interactive" :: tail => nextOption(map ++ Map('interactive -> true), tail)
          // aditional options
          case "-mathkernel" :: value :: tail =>
            if(value.nonEmpty && !value.toString.startsWith("-")) nextOption(map ++ Map('mathkernel -> value), tail)
            else optionErrorReporter("-mathkernel")
          case "-jlink" :: value :: tail =>
            if(value.nonEmpty && !value.toString.startsWith("-")) nextOption(map ++ Map('jlink -> value), tail)
            else optionErrorReporter("-jlink")
          case "-verify" :: tail => require(!map.contains('verify)); nextOption(map ++ Map('verify -> true), tail)
          case "-noverify" :: tail => require(!map.contains('verify)); nextOption(map ++ Map('verify -> false), tail)
          case "-interval" :: tail => require(!map.contains('interval)); nextOption(map ++ Map('interval -> true), tail)
          case "-nointerval" :: tail => require(!map.contains('interval)); nextOption(map ++ Map('interval -> false), tail)
          case "-cse" :: tail => require(!map.contains('cse)); nextOption(map ++ Map('cse -> true), tail)
          case "-dnf" :: tail => require(!map.contains('dnf)); nextOption(map ++ Map('dnf -> true), tail)
          // global options
          case "-lax" :: tail => System.setProperty("LAX", "true"); nextOption(map, tail)
          case "-strict" :: tail => System.setProperty("LAX", "false"); nextOption(map, tail)
          case "-debug" :: tail => System.setProperty("DEBUG", "true"); nextOption(map, tail)
          case "-nodebug" :: tail => System.setProperty("DEBUG", "false"); nextOption(map, tail)
          case "-security" :: tail => activateSecurity(); nextOption(map, tail)
          case "-launch" :: tail => launched(); nextOption(map, tail)
          case option :: tail => optionErrorReporter(option)
        }
      }


      //@note 'commandLine is only passed in to preserve evidence of what generated the output.
      val options = nextOption(Map('commandLine -> args.mkString(" ")), args.toList)
      require(options.contains('mode), usage + "\narguments: " + args.mkString("  "))

      if (options.get('mode) == Some("codegen") && options.getOrElse('format, "C")=="C")
        //@note no MathKernel initialization needed for C generation
        codegen(options)
      else if (options.get('mode) != Some("ui") ) {
        try {
          initializeProver(options)

          //@todo allow multiple passes by filter architecture: -prove bla.key -tactic bla.scal -modelplex -codegen -format C
          options.get('mode) match {
            case Some("prove") => prove(options)
            case Some("modelplex") => modelplex(options)
            case Some("codegen") => codegen(options)
            case Some("ui") => assert(false, "already handled above since no prover needed"); ???
          }
        }
        finally {
          shutdownProver()
        }
      }
    }
  }

  private def parseProblemFile(fileName: String) = {
    try {
      val fileContents = scala.io.Source.fromFile(fileName).getLines().reduce(_ + "\n" + _)
      val formula = KeYmaeraXProblemParser(fileContents)._1;
      println(KeYmaeraXPrettyPrinter(formula))
      println("Parsed file successfully");
      System.exit(0)
    }
    catch {
      case e : Exception => {
        if (System.getProperty("DEBUG", "false")=="true") e.printStackTrace()
        println(e);
        println("Failed to parse file");
        System.exit(-1)
      }
    }
  }

  private def parseBelleTactic(fileName: String) = {
    val fileContents : String = scala.io.Source.fromFile(fileName).getLines().reduce(_ + "\n" + _)
    BTacticParser(fileContents) match {
      case Some(_) => {
        println("Parsed file successfully");
        System.exit(0)
      }
      case None => {
        println("Failed to parse file.");
        System.exit(-1)
      }
    }
  }

  private def optionErrorReporter(option: String) = {
    val noValueMessage = "[Error] No value specified for " + option + " option. "
    option match {
      case "-prove" => println(noValueMessage + "Please use: -prove FILENAME.key\n\n" + usage); exit(1)
      case "-modelPlex" => println(noValueMessage + "Please use: -modelPlex FILENAME.key\n\n" + usage); exit(1)
      case "codegen" => println(noValueMessage + "Please use: -codegen FILENAME.mx\n\n" + usage); exit(1)
      case "-out" => println(noValueMessage + "Please use: -out FILENAME.proof | FILENAME.mx | FILENAME.c | FILENAME.g\n\n" + usage); exit(1)
      case "-vars" => println(noValueMessage + "Please use: -vars VARIABLE_1,VARIABLE_2,...\n\n" + usage); exit(1)
      case "-format" => println(noValueMessage + "Please use: -format C | Spiral\n\n" + usage); exit(1)
      case "-tactic" =>  println(noValueMessage + "Please use: -tactic FILENAME.scala\n\n" + usage); exit(1)
      case "-mathkernel" => println(noValueMessage + "Please use: -mathkernel PATH_TO_" + DefaultConfiguration.defaultMathLinkName._1 + "_FILE\n\n" + usage); exit(1)
      case "-jlink" => println(noValueMessage + "Please use: -jlink PATH_TO_DIRECTORY_CONTAINS_" +  DefaultConfiguration.defaultMathLinkName._2 + "_FILE\n\n" + usage); exit(1)
      case _ =>  println("[Error] Unknown option " + option + "\n\n" + usage); exit(1)
    }
  }

  def initializeProver(options: OptionMap) = {
    assert((options.contains('mathkernel) && options.contains('jlink)) || (!options.contains('mathkernel) && !options.contains('jlink)),
      "\n[Error] Please always use command line option -mathkernel and -jlink together," +
        "and specify the Mathematica link paths with:\n" +
        " -mathkernel PATH_TO_" + DefaultConfiguration.defaultMathLinkName._1 + "_FILE" +
        " -jlink PATH_TO_DIRECTORY_CONTAINS_" +  DefaultConfiguration.defaultMathLinkName._2 + "_FILE \n\n" + usage)

    val mathematicaConfig =
      if (options.contains('mathkernel) && options.contains('jlink)) Map("linkName" -> options.get('mathkernel).get.toString,
                                                                         "libDir" -> options.get('jlink).get.toString)
      else DefaultConfiguration.defaultMathematicaConfig

    val linkNamePath = mathematicaConfig.get("linkName") match {
      case Some(path) => path
      case _ => ""
    }
    val libDirPath = mathematicaConfig.get("libDir") match {
      case Some(path) => path
      case _ => ""
    }
    assert(linkNamePath!="" && libDirPath!="",
      "\n[Error] The paths to MathKernel file named " + DefaultConfiguration.defaultMathLinkName._1 + " and jlinkLibDir file named " + DefaultConfiguration.defaultMathLinkName._2 + " are not specified! " +
        "(On your system, they could look like: " + {if(DefaultConfiguration.defaultMathLinkPath._1!="") DefaultConfiguration.defaultMathLinkPath._1 else DefaultConfiguration.exemplaryMathLinkPath._1} +
        " and " + {if(DefaultConfiguration.defaultMathLinkPath._2!="") DefaultConfiguration.defaultMathLinkPath._2 else DefaultConfiguration.exemplaryMathLinkPath._2} + ")\n" +
        "Please specify the paths to " + DefaultConfiguration.defaultMathLinkName._1 + " and " + DefaultConfiguration.defaultMathLinkName._2 + " with command line option:" +
        " -mathkernel PATH_TO_" + DefaultConfiguration.defaultMathLinkName._1 + "_FILE" +
        " -jlink PATH_TO_DIRECTORY_CONTAINS_" +  DefaultConfiguration.defaultMathLinkName._2 + "_FILE\n" +
        "[Note] Please always use command line option -mathkernel and -jlink together. \n\n" + usage)
    assert(linkNamePath.endsWith(DefaultConfiguration.defaultMathLinkName._1) && new java.io.File(linkNamePath).exists(),
      "\n[Error] Cannot find MathKernel file named " + DefaultConfiguration.defaultMathLinkName._1 + " in path: " + linkNamePath+ "! " +
        "(On your system, it could look like: " + {if(DefaultConfiguration.defaultMathLinkPath._1!="") DefaultConfiguration.defaultMathLinkPath._1 else DefaultConfiguration.exemplaryMathLinkPath._1} + ")\n" +
        "Please specify the correct path that points to " + DefaultConfiguration.defaultMathLinkName._1 + " file with command line option:" +
        " -mathkernel PATH_TO_" + DefaultConfiguration.defaultMathLinkName._1 + "_FILE\n" +
        "[Note] Please always use command line option -mathkernel and -jlink together. \n\n" + usage)
    assert(new java.io.File(libDirPath + File.separator + DefaultConfiguration.defaultMathLinkName._2).exists(),
      "\n[Error] Cannot find jlinkLibDir file named " + DefaultConfiguration.defaultMathLinkName._2 + " in path " + libDirPath+ "! " +
        "(On your system, it could look like: " + {if(DefaultConfiguration.defaultMathLinkPath._2!="") DefaultConfiguration.defaultMathLinkPath._2 else DefaultConfiguration.exemplaryMathLinkPath._2} + ")\n" +
        "Please specify the correct path that points to the directory contains " + DefaultConfiguration.defaultMathLinkName._2 + " file with command line option:" +
        " -jlink PATH_TO_DIRECTORY_CONTAINS_" +  DefaultConfiguration.defaultMathLinkName._2 + "_FILE\n" +
        "[Note] Please always use command line option -mathkernel and -jlink together. \n\n" + usage)

    PrettyPrinter.setPrinter(KeYmaeraXPrettyPrinter.pp)

    val generator = new ConfigurableGenerate[Formula]()
    KeYmaeraXParser.setAnnotationListener((p: Program, inv: Formula) => generator.products += (p->inv))
    TactixLibrary.invGenerator = generator

    val mathematica = new Mathematica()
    mathematica.init(DefaultConfiguration.defaultMathematicaConfig)
    DerivedAxioms.qeTool = mathematica
    TactixLibrary.tool = mathematica
  }

  //@todo Runtime.getRuntime.addShutdownHook??
  def shutdownProver() = {
    if (DerivedAxioms.qeTool != null) {
      DerivedAxioms.qeTool match { case m: Mathematica => m.shutdown() }
      DerivedAxioms.qeTool = null
    }
    if (TactixLibrary.tool != null) {
      TactixLibrary.tool match { case m: Mathematica => m.shutdown() }
      TactixLibrary.tool = null
      TactixLibrary.invGenerator = new NoneGenerate()
    }
  }

  /** Exit gracefully */
  private def exit(status: Int): Nothing = {shutdownProver(); sys.exit(status)}

  /** Generate a header stamping the source of a generated file */
  //@todo Of course this has a security attack for non-letter characters like end of comments from command line
  private def stampHead(options: OptionMap): String = "/* @evidence: generated by KeYmaeraX " + VERSION + " " + nocomment(options.getOrElse('commandLine, "<unavailable>").asInstanceOf[String]) + " */\n\n"

  /** Require that s has no C-style line-comments */
  private def nocomment(s: String): String = {require(!s.contains("/*") && !s.contains("*/")); s}

  /**
   * Prove given input file (with given tactic) to produce a lemma.
   * {{{KeYmaeraXLemmaPrinter(Prover(tactic)(KeYmaeraXProblemParser(input)))}}}
   * @param options
   * @todo tactic should default to master and builtin tactic names at least from ExposedTacticsLibrary should be accepted (without file extension)
   */
  def prove(options: OptionMap) = {
    require(options.contains('in), usage)
    require(options.contains('tactic), usage)

    val tacticFileNameDotScala = options.get('tactic).get.toString
    //assert(tacticFileNameDotScala.endsWith(".scala"),
    //  "\n[Error] Wrong file name " + tacticFileNameDotScala + " used for -tactic! KeYmaera X only reads .scala tactic file. Please use: -tactic FILENAME.scala")
    val tacticSource = scala.io.Source.fromFile(tacticFileNameDotScala).mkString

    val cm = universe.runtimeMirror(getClass.getClassLoader)
    val tb = cm.mkToolBox()
    val foo = tb.parse(tacticSource)
    val tacticGenerator = tb.eval(tb.parse(tacticSource)).asInstanceOf[() => BelleExpr]

    val tactic = tacticGenerator()

    // KeYmaeraXLemmaPrinter(Prover(tactic)(KeYmaeraXProblemParser(input)))
    val inputFileNameDotKey = options.get('in).get.toString
    assert(inputFileNameDotKey.endsWith(".key"),
      "\n[Error] Wrong file name " + inputFileNameDotKey + " used for -prove! KeYmaera X only proves .key file. Please use: -prove FILENAME.key")
    val input = scala.io.Source.fromFile(inputFileNameDotKey).mkString
    val inputModel = KeYmaeraXProblemParser(input)._1
    val inputSequent = Sequent(Nil, immutable.IndexedSeq[Formula](), immutable.IndexedSeq(inputModel))
    val inputFileName = inputFileNameDotKey.dropRight(4)
    var outputFileName = inputFileName
    if(options.contains('out)) {
      val outputFileNameDotProof = options.get('out).get.toString
      assert(outputFileNameDotProof.endsWith(".proof"),
        "\n[Error] Wrong file name " + outputFileNameDotProof + " used for -out! KeYmaera X only produces proof evidence as .proof file. Please use: -out FILENAME.proof")
      outputFileName = outputFileNameDotProof.dropRight(6)
    }

    val pw = new PrintWriter(outputFileName + ".proof")

    val proofStart: Long = Platform.currentTime
    //@todo turn the following into a transformation as well. The natural type is Prover: Tactic=>(Formula=>Provable) which however always forces 'verify=true. Maybe that's not bad.
    val witness = TactixLibrary.proveBy(inputSequent, tactic)
    val proofDuration = Platform.currentTime - proofStart
    Console.println("[proof time " + proofDuration + "ms]")

    if (witness.isProved) {
      assert(witness.subgoals.isEmpty)
      if (options.getOrElse('verify, false).asInstanceOf[Boolean]) {
        val witnessStart: Long = Platform.currentTime
        val proved = witness.proved
        //@note assert(witness.isProved, "Successful proof certificate") already checked in line above
        assert(inputSequent == proved, "Proved the original problem and not something else")
        val witnessDuration = Platform.currentTime - witnessStart
        Console.println("[proof time       " + proofDuration + "ms]")
        Console.println("[certificate time " + witnessDuration + "ms]")
        println("Proof certificate: Passed")
      } else {
        println("Proof certificate: Skipped extraction of proof certificate from proof search\n (use -verify to generate and check proof certificate)")
      }

      //@note printing original input rather than a pretty-print of proved ensures that @invariant annotations are preserved for reproves.
      val evidence = ToolEvidence(Map(
        "tool" -> "KeYmaera X",
        "model" -> input,
        "tactic" -> scala.io.Source.fromFile(tacticFileNameDotScala).mkString,
        "proof" -> "" //@todo serialize proof
      )) :: Nil

      //@note pretty-printing the result of parse ensures that the lemma states what's actually been proved.
      assert(KeYmaeraXParser(KeYmaeraXPrettyPrinter(inputModel)) == inputModel, "parse of print is identity")
      //@note check that proved conclusion is what we actually wanted to prove
      assert(witness.conclusion == inputSequent && witness.proved == inputSequent,
        "proved conclusion deviates from input")

      assert(inputFileName.lastIndexOf(File.separatorChar) < inputFileName.length, "Input file name is not an absolute path")
      val lemma = Lemma(witness, evidence,
        Some(inputFileName.substring(inputFileName.lastIndexOf(File.separatorChar)+1)))

      //@see[[edu.cmu.cs.ls.keymaerax.core.Lemma]]
      assert(lemma.fact.conclusion.ante.isEmpty && lemma.fact.conclusion.succ.size == 1, "Illegal lemma form")
      assert(KeYmaeraXExtendedLemmaParser(lemma.toString) == (lemma.name, lemma.fact.conclusion::Nil, lemma.evidence.head),
        "reparse of printed lemma is not original lemma")

      pw.write(stampHead(options))
      pw.write("/* @evidence: parse of print of result of a proof */\n\n")
      pw.write(lemma.toString)
      pw.close()
    } else {
      assert(!witness.isProved)
      assert(witness.subgoals.nonEmpty)
      //@note PrintWriter above has already emptied the output file
      pw.close()
      println("==================================")
      println("Tactic did not finish the proof    open goals: " + witness.subgoals.size)
      println("==================================")
      printOpenGoals(witness)
      println("==================================")
      if (options.getOrElse('interactive,false)==true) {
        interactiveProver(witness)
      } else {
        System.err.println("Incomplete proof: tactic did not finish the proof")
        exit(-1)
      }
    }
  }

  /**
   * ModelPlex monitor synthesis for the given input files
   * {{{KeYmaeraXPrettyPrinter(ModelPlex(vars)(KeYmaeraXProblemParser(input))}}}
   * @param options in describes input file name, vars describes the list of variables, out describes the output file name.
   */
  def modelplex(options: OptionMap) = {
    require(options.contains('in), usage)

    // KeYmaeraXPrettyPrinter(ModelPlex(vars)(KeYmaeraXProblemParser(input))
    val inputFileNameDotKey = options.get('in).get.toString
    assert(inputFileNameDotKey.endsWith(".key"),
      "\n[Error] Wrong file name " + inputFileNameDotKey + " used for -modelplex! ModelPlex only handles .key file. Please use: -modelplex FILENAME.key")
    val input = scala.io.Source.fromFile(inputFileNameDotKey).mkString
    val inputModel = KeYmaeraXProblemParser(input)._1
    val verifyOption = options.getOrElse('verify, true).asInstanceOf[Boolean]
    val inputFileName = inputFileNameDotKey.dropRight(4)
    var outputFileName = inputFileName
    if(options.contains('out)) {
      val outputFileNameDotMx = options.get('out).get.toString
      assert(outputFileNameDotMx.endsWith(".mx"),
        "\n[Error] Wrong file name " + outputFileNameDotMx + " used for -out! ModelPlex only generates .mx file. Please use: -out FILENAME.mx")
      outputFileName = outputFileNameDotMx.dropRight(3)
    }

    val pw = new PrintWriter(outputFileName + ".mx")

    val kind =
      if (options.contains('kind)) options.get('kind).get.asInstanceOf[Symbol]
      else 'model

    val outputFml = if (options.contains('vars))
      ModelPlex(options.get('vars).get.asInstanceOf[Array[Variable]].toList, kind, verifyOption)(inputModel)
    else
      ModelPlex(inputModel, kind, verifyOption)
    val output = KeYmaeraXPrettyPrinter(outputFml)

    val reparse = KeYmaeraXParser(output)
    assert(reparse == outputFml, "parse of print is identity")
    pw.write(stampHead(options))
    pw.write("/* @evidence: parse of print of ModelPlex proof output */\n\n")
    pw.write("/************************************\n * Generated by KeYmaera X ModelPlex\n ************************************/\n\n")
    pw.write("/**\n * @param variables are for the state before the controller run\n * @param post() function symbols are for the state after the controller run\n * @param other function symbols are constant\n */\n\n")
    pw.write(output)
    pw.close()
  }



  /**
   * Code generation
   * {{{CGenerator()(input)}}}
   */
  def codegen(options: OptionMap) = {
    require(options.contains('in), usage)
    require(options.contains('format), usage)

    val inputFileNameDotMx = options.get('in).get.toString
    assert(inputFileNameDotMx.endsWith(".mx"),
      "\n[Error] Wrong file name " + inputFileNameDotMx + " used for -codegen! Code generator only handles .mx file. Please use: -codegen FILENAME.mx")
    val input = scala.io.Source.fromFile(inputFileNameDotMx).mkString
    val inputFormula = KeYmaeraXParser(input)
    val inputFileName = inputFileNameDotMx.dropRight(3)

    if (options.getOrElse('interval, true).asInstanceOf[Boolean]) {
      //@todo check that when assuming the output formula as an additional untrusted lemma, the Provable isProved.
      System.err.println("Cannot yet augment compiled code with interval arithmetic to guard against floating-point roundoff errors\n(use -nointerval instead)")

      println("Interval arithmetic: unfinished")
      System.err.println("Interval arithmetic: unfinished")
      //@todo wipe out output file PrintWriter above has already emptied the output file
      //@todo pw.close()
      exit(-1)
      // TODO what to to when proof cannot be checked?
    } else {
      println("Interval arithmetic: Skipped interval arithmetic generation\n(use -interval to guard against floating-point roundoff errors)")
    }

    if(options.get('format).get.toString == "C") {
      var outputFileName = inputFileName
      if(options.contains('out)) {
        val outputFileNameDotC = options.get('out).get.toString
        assert(outputFileNameDotC.endsWith(".c"),
          "\n[Error] Wrong file name " + outputFileNameDotC + " used for -out! C generator only generates .c file. Please use： -out FILENAME.c")
        outputFileName = outputFileNameDotC.dropRight(2)
      }
      val vars: List[Variable] =
        if(options.contains('vars)) options.get('vars).get.asInstanceOf[Array[Variable]].toList
        else StaticSemantics.vars(inputFormula).symbols.map((x:NamedSymbol)=>x.asInstanceOf[Variable]).toList.sortWith((x, y)=>x<y)
      val cseMode = options.contains('cse)
      val codegenStart = Platform.currentTime
      val output = if(cseMode) CseCGenerator(inputFormula, vars, outputFileName) else CGenerator(inputFormula, vars, outputFileName)
      Console.println("[codegen time " + (Platform.currentTime - codegenStart) + "ms]")
      val pw = new PrintWriter(outputFileName + ".c")
      pw.write(stampHead(options))
      pw.write("/* @evidence: print of CGenerator of input */\n\n")
      pw.write(output)
      pw.close()
    } else if(options.get('format).get.toString == "Spiral") {
      var outputFileName = inputFileName
      if(options.contains('out)) {
        val outputFileNameDotG = options.get('out).get.toString
        assert(outputFileNameDotG.endsWith(".g"),
          "\n[Error] Wrong file name " + outputFileNameDotG + " used for -out! Spiral generator only generates .g file and the .h file when necessary. Please use： -out FILENAME.g")
        outputFileName = outputFileNameDotG.dropRight(2)
      }
      val dnfMode = options.contains('dnf)
      var outputG = ""
      if (options.contains('vars)) {
        val output = SpiralGenerator(inputFormula, options.get('vars).get.asInstanceOf[Array[Variable]].toList, outputFileName, dnfMode)
        outputG = output._1
        val outputH = output._2
        val pwG = new PrintWriter(outputFileName + ".g")
        pwG.write(stampHead(options))
        pwG.write(outputG)
        pwG.close()
        val pwH = new PrintWriter(outputFileName + ".h")
        pwH.write(stampHead(options))
        pwH.write(outputH)
        pwH.close()
      } else {
        outputG = SpiralGenerator(inputFormula, outputFileName)
        val pwG = new PrintWriter(outputFileName + ".g")
        pwG.write(stampHead(options))
        pwG.write(outputG)
        pwG.close()
      }
    } else throw new IllegalArgumentException("-format C or -format Spiral should be specified as a command line argument")
  }


  /** Launch the web user interface */
  def launchUI(args: Array[String]): Unit = {
    if(this.LAUNCH) Main.main("-launch" +: args)
    else Main.main(args)
  }

  def launchUIWithTactic(kyxPath: String, tacticPath: String, uiArgs: Array[String]) : Unit = {
    throw new Exception("This feature is not currently implemented")
//    println("Launching UI and trying to prove " + kyxPath + " with tactic " + tacticPath)
//    // Launch the web server if it's not already running, and then wait until the server is started.
//    //@todo we need a much cleaner way of checking if the stack size is correct. Until then, calling Main.main from here is very spaghetti.
////    if(!serverIsRunning())
////      throw new IllegalStateException("Server must be running in order to execute a tactic on a .key file.")
//
//    //@todo we're assuming that the stack size is correct.
//    Main.startServer()
//    while(!serverIsRunning()) {
//      this.synchronized({ this.wait(100) })
//    }
//
//    //@todo is this ok...?
//
//    val db = DBAbstractionObj.defaultDatabase //@todo ???
//    val username = "commandLineInterface"
//
//    // Create a new user; ignore any errors.
//    new CreateUserRequest(db, username, "password").getResultingResponses()
//
//
//    //Create the model
//    val modelId = {
//      val randomName = java.util.UUID.randomUUID().toString
//      val kyxFile         = new File(kyxPath)
//      val kyxFileContents = Files.readAllLines(Paths.get(kyxPath)).toArray().toList.mkString("\n")
//      val modelRequest    = new CreateModelRequest(db, username, kyxFile.getName + "(noise: " + randomName + ")", kyxFileContents)
//
//      //The file should parse.
//      try { KeYmaeraXProblemParser(kyxFileContents) }
//      catch { case e: Throwable => System.err.println(".key file should parse!"); throw e }
//
//      modelRequest.getResultingResponses()
//      modelRequest.getModelId
//    }
//
//    // Create a new proof
//    val proofId = {
//      val createPrfRequest = new CreateProofRequest(db, username, modelId, s"$modelId -- Proof launched from CLI -- will not reload!", "")
//
//      createPrfRequest.getResultingResponses()
//      createPrfRequest.getProofId
//    }
//
//    // Run the scala file on the created proof -- blocking!
//    new RunScalaFileRequest(db, proofId, new File(tacticPath)).getResultingResponses()
//
//    // Send the user's browser to the correct location.
//    val host: String = "localhost" //@todo ???
//    val port: Int    = 8090 //@todo ???
//    val path: String = s"dashboard.html?#/proofs/$proofId"
//    SystemWebBrowser(s"http://$host:$port/$path")
//  }
//  private def serverIsRunning() : Boolean = {
//    try {
//      new Socket("localhost", 8090)
//      true
//    }
//    catch {
//      case e : Exception => false
//    }
  }

  // helpers

  /** Print brief information about all open goals in the proof tree under node */
  def printOpenGoals(node: Provable): Unit = node.subgoals.foreach(g => printNode(g))

  def printNode(node: Sequent): Unit = node.toString + "\n"

//  /** Print brief information about the given node */
//  def printNode(node: Sequent): Unit =
//    println("=== " + node.tacticInfo.infos.getOrElse("branchLabel", "<none>") + " ===\n  " +
//      (if (node.isProved) "Closed Goal: " else if (node.children.isEmpty) "Open Goal: " else "Inner Node: ") +
//      node.toString() + "\n" +
//      "  \tdebug: " + node.tacticInfo.infos.getOrElse("debug", "<none>") + "\n")
//
//  /** Print elaborate information about the given node */
//  def elaborateNode(node: ProofNode): Unit = {
//    println("=== " + node.tacticInfo.infos.getOrElse("branchLabel", "<none>") + " ===  " +
//      (if (node.isClosed) "Closed Goal: " else if (node.children.isEmpty) "Open Goal: " else "Inner Node: ") +
//      "\tdebug: " + node.tacticInfo.infos.getOrElse("debug", "<none>") +
//      "\n" + node.sequent.prettyString + "\n")
//  }

  /** Implements the security policy for the KeYmaera X web server.
    *
    * Preferably we would heavily restrict uses of reflection (to prevent, for example, creating fake Provables),
    * but we know of no way to do so except relying on extremely fragile methods such as crawling the call stack.
    * The same goes for restricting read access to files.
    *
    * Instead we settle for preventing people from installing less-restrictive security managers and restricting
    * all writes to be inside the .keymaerax directory. */
  class KeYmaeraXSecurityManager extends SecurityManager {
    val homeDir = System.getProperty("user.home")
    val keymaerax = homeDir + "/.keymaerax"

    override def checkPermission(perm: Permission): Unit = {
      perm match {
        case _:ReflectPermission => ()
        case _:RuntimePermission =>
          if ("setSecurityManager".equals(perm.getName))
            throw new SecurityException()
        case _:FilePermission =>
          val filePermission = perm.asInstanceOf[FilePermission]
          val name = filePermission.getName
          val actions = filePermission.getActions
          if ((actions.contains("write") || actions.contains("delete"))
            && !name.startsWith(keymaerax)) {
            throw new SecurityException("KeYmaera X security manager forbids writing to files outside ~/.keymaerax")
          }
      }
    }
  }

  private def activateSecurity(): Unit = {
    System.setSecurityManager(new KeYmaeraXSecurityManager())
  }

  private val interactiveUsage = "Type a tactic command to apply to the current goal.\n" +
    "skip - ignore the current goal for now and skip to the next goal.\n" +
    "goals - list all open goals.\n" +
    "goal i - switch to goal number i\n" +
    "exit - quit the prover (or hit Ctrl-C any time).\n" +
    "help - will display this usage information.\n" +
    "Tactics will be reported back when a branch closes but may need cleanup.\n"
  /** KeYmaera C: A simple interactive command-line prover */
  private def interactiveProver(root: Provable): Provable = {
    val commands = io.Source.stdin.getLines()
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    val tb = cm.mkToolBox()
    println("KeYmaera X Interactive Command-line Prover\n" +
      "If you are looking for the more convenient web user interface,\nrestart with option -ui\n\n")
    println(interactiveUsage)

    while (!root.isProved) {
      assert(root.subgoals.nonEmpty, "proofs that are not closed must have open goals")
      println("Open Goals: " + root.subgoals.size)
      var node = root.subgoals.head
      var current = root
      //println("=== " + node.tacticInfo.infos.getOrElse("branchLabel", "<none>") + " ===\n")
      var tacticLog = ""
      assert(!current.isProved, "open goals are not closed")
      while (!current.isProved) {
        printNode(node)
        System.out.flush()
        commands.next().trim match {
          case "" =>
          case "help" => println(interactiveUsage)
          case "exit" => exit(5)
          case "goals" => val open = root.subgoals
            (1 to open.length).map(g => {println("Goal " + g); printNode(open(g-1))})
          case it if it.startsWith("goal ") => try {
            val g = it.substring("goal ".length).toInt
            if (1<=g&&g<=root.subgoals.size) node = root.subgoals(g-1)
            else println("No such goal: "+ g)
          } catch {case e: NumberFormatException => println(e)}
          case "skip" =>
            if (root.subgoals.size >= 2) {
              //@todo skip to the next goal somewhere on the right of node, not to a random goal
              //@todo track this level skipping by closing and opening parentheses in the log
              var nextGoal = new Random().nextInt(root.subgoals.length)
              assert(0 <= nextGoal && nextGoal < root.subgoals.size, "random in range")
              node = if (root.subgoals(nextGoal) != node)
                root.subgoals(nextGoal)
              else {
                val otherGoals = root.subgoals diff List(node)
                assert(otherGoals.length == root.subgoals.length - 1, "removing one open goal decreases open goal count by 1")
                nextGoal = new Random().nextInt(otherGoals.length)
                assert(0 <= nextGoal && nextGoal < otherGoals.size, "random in range")
                otherGoals(nextGoal)
              }
            } else {
              println("No other open goals to skip to")
            }
          case command: String =>
            try {
              //@note security issue since executing arbitrary input unsanitized
              val tacticGenerator = tb.eval(tb.parse(tacticParsePrefix + command + tacticParseSuffix)).asInstanceOf[() => BelleExpr]
              val tactic = tacticGenerator()
              tacticLog += "& " + command + "\n"
              current = TactixLibrary.proveBy(node, tactic)
              // walk to the next open subgoal
              // continue walking if it has leaves
              while (!current.isProved && current.subgoals.nonEmpty)
                node = current.subgoals.head
              //@todo make sure to walk to siblings ultimately
            }
            catch {
              case e: ToolBoxError => println("Command failed: " + e + "\n"); System.out.flush()
            }
        }
      }
      assert(current.isProved)
//      println("=== " + node.tacticInfo.infos.getOrElse("branchLabel", "<none>") + " === CLOSED")
      println(tacticLog)
    }
    root
  }

  //@todo import namespace of the user tactic *object* passed in -tactic
  private val tacticParsePrefix =
    """
      |import edu.cmu.cs.ls.keymaerax.tactics.TactixLibrary._
      |import edu.cmu.cs.ls.keymaerax.tactics.Tactics.Tactic
      |import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
      |import edu.cmu.cs.ls.keymaerax.tactics.BranchLabels._
      |import edu.cmu.cs.ls.keymaerax.tactics._
      |class InteractiveLocalTactic extends (() => Tactic) {
      |  def apply(): Tactic = {
      |
    """.stripMargin

  private val tacticParseSuffix =
    """
      |  }
      |}
      |new InteractiveLocalTactic()
    """.stripMargin


  private val license: String =
    """
      |KeYmaera X
      |SOFTWARE LICENSE AGREEMENT
      |ACADEMIC OR NON-PROFIT ORGANIZATION NONCOMMERCIAL RESEARCH USE ONLY
      |
      |BY USING THE SOFTWARE, YOU ARE AGREEING TO THE TERMS OF THIS LICENSE
      |AGREEMENT. IF YOU DO NOT AGREE WITH THESE TERMS, YOU MAY NOT USE OR
      |DOWNLOAD THE SOFTWARE.
      |
      |This is a license agreement ("Agreement") between your academic
      |institution or non-profit organization or self (called "Licensee" or
      |"You" in this Agreement) and Carnegie Mellon University (called
      |"Licensor" in this Agreement). All rights not specifically granted
      |to you in this Agreement are reserved for Licensor.
      |
      |RESERVATION OF OWNERSHIP AND GRANT OF LICENSE:
      |
      |Licensor retains exclusive ownership of any copy of the Software (as
      |defined below) licensed under this Agreement and hereby grants to
      |Licensee a personal, non-exclusive, non-transferable license to use
      |the Software for noncommercial research purposes, without the right
      |to sublicense, pursuant to the terms and conditions of this
      |Agreement. As used in this Agreement, the term "Software" means (i)
      |the executable file made accessible to Licensee by Licensor pursuant
      |to this Agreement, inclusive of backups, and updates permitted
      |hereunder or subsequently supplied by Licensor, including all or any
      |file structures, programming instructions, user interfaces and
      |screen formats and sequences as well as any and all documentation
      |and instructions related to it.
      |
      |CONFIDENTIALITY: Licensee acknowledges that the Software is
      |proprietary to Licensor, and as such, Licensee agrees to receive all
      |such materials in confidence and use the Software only in accordance
      |with the terms of this Agreement. Licensee agrees to use reasonable
      |effort to protect the Software from unauthorized use, reproduction,
      |distribution, or publication.
      |
      |COPYRIGHT: The Software is owned by Licensor and is protected by
      |United States copyright laws and applicable international treaties
      |and/or conventions.
      |
      |PERMITTED USES: The Software may be used for your own noncommercial
      |internal research purposes. You understand and agree that Licensor
      |is not obligated to implement any suggestions and/or feedback you
      |might provide regarding the Software, but to the extent Licensor
      |does so, you are not entitled to any compensation related thereto.
      |
      |BACKUPS: If Licensee is an organization, it may make that number of
      |copies of the Software necessary for internal noncommercial use at a
      |single site within its organization provided that all information
      |appearing in or on the original labels, including the copyright and
      |trademark notices are copied onto the labels of the copies.
      |
      |USES NOT PERMITTED: You may not modify, translate, reverse engineer,
      |decompile, disassemble, distribute, copy or use the Software except
      |as explicitly permitted herein. Licensee has not been granted any
      |trademark license as part of this Agreement and may not use the name
      |or mark "KeYmaera X", "Carnegie Mellon" or any renditions thereof
      |without the prior written permission of Licensor.
      |
      |You may not sell, rent, lease, sublicense, lend, time-share or
      |transfer, in whole or in part, or provide third parties access to
      |prior or present versions (or any parts thereof) of the Software.
      |
      |ASSIGNMENT: You may not assign this Agreement or your rights
      |hereunder without the prior written consent of Licensor. Any
      |attempted assignment without such consent shall be null and void.
      |
      |TERM: The term of the license granted by this Agreement is from
      |Licensee's acceptance of this Agreement by clicking "I Agree" below
      |or by using the Software until terminated as provided below.
      |
      |The Agreement automatically terminates without notice if you fail to
      |comply with any provision of this Agreement. Licensee may terminate
      |this Agreement by ceasing using the Software. Upon any termination
      |of this Agreement, Licensee will delete any and all copies of the
      |Software. You agree that all provisions which operate to protect the
      |proprietary rights of Licensor shall remain in force should breach
      |occur and that the obligation of confidentiality described in this
      |Agreement is binding in perpetuity and, as such, survives the term
      |of the Agreement.
      |
      |FEE: Provided Licensee abides completely by the terms and conditions
      |of this Agreement, there is no fee due to Licensor for Licensee's
      |use of the Software in accordance with this Agreement.
      |
      |DISCLAIMER OF WARRANTIES: THE SOFTWARE IS PROVIDED "AS-IS" WITHOUT
      |WARRANTY OF ANY KIND INCLUDING ANY WARRANTIES OF PERFORMANCE OR
      |MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE OR PURPOSE OR OF
      |NON-INFRINGEMENT. LICENSEE BEARS ALL RISK RELATING TO QUALITY AND
      |PERFORMANCE OF THE SOFTWARE AND RELATED MATERIALS.
      |
      |SUPPORT AND MAINTENANCE: No Software support or training by the
      |Licensor is provided as part of this Agreement.
      |
      |EXCLUSIVE REMEDY AND LIMITATION OF LIABILITY: To the maximum extent
      |permitted under applicable law, Licensor shall not be liable for
      |direct, indirect, special, incidental, or consequential damages or
      |lost profits related to Licensee's use of and/or inability to use
      |the Software, even if Licensor is advised of the possibility of such
      |damage.
      |
      |EXPORT REGULATION: Licensee agrees to comply with any and all
      |applicable U.S. export control laws, regulations, and/or other laws
      |related to embargoes and sanction programs administered by the
      |Office of Foreign Assets Control.
      |
      |SEVERABILITY: If any provision(s) of this Agreement shall be held to
      |be invalid, illegal, or unenforceable by a court or other tribunal
      |of competent jurisdiction, the validity, legality and enforceability
      |of the remaining provisions shall not in any way be affected or
      |impaired thereby.
      |
      |NO IMPLIED WAIVERS: No failure or delay by Licensor in enforcing any
      |right or remedy under this Agreement shall be construed as a waiver
      |of any future or other exercise of such right or remedy by Licensor.
      |
      |GOVERNING LAW: This Agreement shall be construed and enforced in
      |accordance with the laws of the Commonwealth of Pennsylvania without
      |reference to conflict of laws principles. You consent to the
      |personal jurisdiction of the courts of this County and waive their
      |rights to venue outside of Allegheny County, Pennsylvania.
      |
      |ENTIRE AGREEMENT AND AMENDMENTS: This Agreement constitutes the sole
      |and entire agreement between Licensee and Licensor as to the matter
      |set forth herein and supersedes any previous agreements,
      |understandings, and arrangements between the parties relating hereto.
      |
    """.stripMargin
}
