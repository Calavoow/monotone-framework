package analyzer

import org.scalatest.{FlatSpec, Matchers}
import parser.AST._
import parser.{AstUtils, WhileParser}

class AnalysesSpec extends FlatSpec with Matchers{

	"Available Expresssions" should "analyze an assignment" in {
		val ast = Assig("x", BinOp("+", Ref("a"), INT(1)))
		AstUtils.labelNodes(ast)
		val expectedIn = List(
			Set[BinOp]()
		)
		val expectedOut = List(
			Set(BinOp("+", Ref("a"), INT(1)))
		)

		val (in, out) = Analyses.availableExpressions(ast)
		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	it should "analyze the example 2.5" in {
		//Note: assume correct parse (checked manually)
		val program = "{x:= a+b y:=a*b while(y>a+b) {a:=a+1 x:=a+b}}"
		val ast = WhileParser.parseAll(WhileParser.statement, program).get
		AstUtils.labelNodes(ast)

		val expectedIn = List(
			Set[BinOp]()
			, Set(BinOp("+", Ref("a"), Ref("b")))
			, Set(BinOp("+", Ref("a"), Ref("b")))
			, Set(BinOp("+", Ref("a"), Ref("b")))
			, Set[BinOp]()
		)
		val expectedOut = List(
			Set(BinOp("+", Ref("a"), Ref("b")))
			, Set(BinOp("+", Ref("a"), Ref("b")), BinOp("*", Ref("a"), Ref("b")))
			, Set(BinOp("+", Ref("a"), Ref("b")))
			, Set[BinOp]()
			, Set(BinOp("+", Ref("a"), Ref("b")))
		)
		val(in, out) = Analyses.availableExpressions(ast)

		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	"Reaching definitions" should "analyze an assignment" in {
		val ast = Assig("x", BinOp("+", Ref("a"), INT(1)))
		AstUtils.labelNodes(ast)
		val expectedIn = List(
			Set(("a", -1)) // Only a occurs as a FreeVar
		)
		val expectedOut = List(
			Set(("x", 0), ("a", -1)) // x is set at label 0
		)

		val (in, out) = Analyses.reachingDefinitions(ast)
		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	it should "analyze example 2.7" in {
		val program = "{x:=5 y:=1 while(x>1){y:=x*y x:=x-1}}"
		val ast = WhileParser.parseAll(WhileParser.statement, program).get
		AstUtils.labelNodes(ast)

		val expectedIn = List(
			Set(("x", -1),("y", -1))
			, Set(("x", 0),("y", -1))
			, Set(("x", 0),("y", 1),("x", 4),("y", 3))
			, Set(("x", 0),("y", 1),("x", 4),("y", 3))
			, Set(("x", 0),("y", 3),("x", 4))
		)
		val expectedOut = List(
			Set(("x", 0),("y", -1))
			, Set(("x", 0),("y", 1))
			, Set(("x", 0),("y", 1),("x", 4),("y", 3))
			, Set(("x", 0),("x", 4),("y", 3))
			, Set(("y", 3),("x", 4))
		)

		val (in, out) = Analyses.reachingDefinitions(ast)
		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	"Very Busy" should "analyze an assignment" in {
		val ast = Assig("x", BinOp("+", Ref("a"), INT(1)))
		AstUtils.labelNodes(ast)
		val expectedIn = List(
			Set(BinOp("+", Ref("a"), INT(1)))
		)
		val expectedOut = List(
			Set()
		)

		val (in, out) = Analyses.veryBusy(ast)

		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	it should "analyze example 2.9" in {
		val program = "if a>b then {x:=b-a y:=a-b} else {y:=b-a x:=a-b}"
		val ast = WhileParser.parseAll(WhileParser.statement, program).get
		AstUtils.labelNodes(ast)

		val expectedIn = List(
			Set(BinOp("-", Ref("a"), Ref("b")), BinOp("-", Ref("b"), Ref("a")))
			, Set(BinOp("-", Ref("a"), Ref("b")), BinOp("-", Ref("b"), Ref("a")))
			, Set(BinOp("-", Ref("a"), Ref("b")))
			, Set(BinOp("-", Ref("a"), Ref("b")), BinOp("-", Ref("b"), Ref("a")))
			, Set(BinOp("-", Ref("a"), Ref("b")))
		)
		val expectedOut = List(
			Set(BinOp("-", Ref("a"), Ref("b")), BinOp("-", Ref("b"), Ref("a")))
			, Set(BinOp("-", Ref("a"), Ref("b")))
			, Set()
			, Set(BinOp("-", Ref("a"), Ref("b")))
			, Set()
		)
		val(in, out) = Analyses.veryBusy(ast)

		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	"Live Variables" should "analyze an assignment" in {
		val ast = Assig("x", BinOp("+", Ref("a"), INT(1)))
		AstUtils.labelNodes(ast)
		val expectedIn = List(
			Set("a")
		)
		val expectedOut = List(
			Set()
		)

		val (in, out) = Analyses.liveVariables(ast)
		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	it should "analyze example 2.11" in {
		val program = "{x:=2 y:=4 x:=1 if y>x then z:=y else z:=y*y x:=z}"
		val ast = WhileParser.parseAll(WhileParser.statement, program).get
		AstUtils.labelNodes(ast)

		val expectedIn = List(
			Set()
			, Set()
			, Set("y")
			, Set("y","x")
			, Set("y")
			, Set("y")
			, Set("z")
		)
		val expectedOut = List(
			Set()
			, Set("y")
			, Set("y","x")
			, Set("y")
			, Set("z")
			, Set("z")
			, Set()
		)
		val(in, out) = Analyses.liveVariables(ast)

		in should equal(expectedIn)
		out should equal(expectedOut)
	}
}
