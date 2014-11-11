package analyzer

import org.scalatest.{FlatSpec, Matchers}
import parser.AST._
import parser.{AstUtils, WhileParser}

class AnalysesSpec extends FlatSpec with Matchers{

	"Available Expresssions" should "analyze an assignment" in {
		val ast = Program(List(),Assig("x", BinOp("+", Ref("a"), INT(1))))
		AstUtils.labelNodes(ast)
		val expectedIn = List(
			Set[BinOp]()
		)
		val expectedOut = List(
			Set(BinOp("+", Ref("a"), INT(1)))
		)

		val (in, out) = Analyses.availableExpressions(ast, 2)
		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	it should "analyze the example 2.5" in {
		//Note: assume correct parse (checked manually)
		val program = "begin {x:= a+b y:=a*b while(y>a+b) {a:=a+1 x:=a+b}} end"
		val ast = WhileParser.parseAll(WhileParser.program, program).get
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
		val(in, out) = Analyses.availableExpressions(ast, 2)

		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	it should "analyze a function call" in {
		val ast = Program(
			List(Procedure("p", List("x"), "y", Assig("y", Ref("x")))),
			Sequence(List(
				Assig("x", BinOp("+", Ref("a"), INT(1)))
				,Call("p", List("x"), "y")
			))
		)
		AstUtils.labelNodes(ast)

		val expectedIn = List(
			Set(BinOp("+",Ref("a"),INT(1)))
			, Set(BinOp("+",Ref("a"),INT(1)))
			, Set(BinOp("+",Ref("a"),INT(1)))
			, Set() // Only before the assignment there expression 'a+1' is not available.
			, Set(BinOp("+",Ref("a"),INT(1)))
			, Set(BinOp("+",Ref("a"),INT(1)))
		)
		val expectedOut = List(
			Set(BinOp("+",Ref("a"),INT(1)))
			, Set(BinOp("+",Ref("a"),INT(1)))
			, Set(BinOp("+",Ref("a"),INT(1)))
			, Set(BinOp("+",Ref("a"),INT(1)))
			, Set(BinOp("+",Ref("a"),INT(1)))
			, Set(BinOp("+",Ref("a"),INT(1)))
		)

		val (in, out) = Analyses.availableExpressions(ast, 2)
		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	it should "be a context-sensitive function call" in {
		val ast = Program(
			List(Procedure("p", List("x"), "y", Assig("y", Ref("x")))),
			Sequence(List(
				Assig("x", BinOp("+", Ref("a"), INT(1)))
				, Call("p", List("x"), "y")
				, Assig("a", BinOp("+", Ref("a"), INT(2)))
				, Call("p", List("x"), "y")
			))
		)
		AstUtils.labelNodes(ast)

		val expectedIn = List(
			Set(BinOp("+",Ref("a"),INT(1)), BinOp("+", Ref("a"), INT(2))),
			Set(BinOp("+",Ref("a"),INT(1)), BinOp("+", Ref("a"), INT(2))),
			Set(BinOp("+",Ref("a"),INT(1)), BinOp("+", Ref("a"), INT(2))),
			Set(),
			Set(BinOp("+",Ref("a"),INT(1))), // call p(x; y)^4
			Set(BinOp("+",Ref("a"),INT(1))), // call p(x; y)_5
			Set(BinOp("+",Ref("a"),INT(1))),
			Set(),
			Set()
		)
		val expectedOut = List(
			Set(BinOp("+",Ref("a"),INT(1)), BinOp("+", Ref("a"), INT(2))),
			Set(BinOp("+",Ref("a"),INT(1)), BinOp("+", Ref("a"), INT(2))),
			Set(BinOp("+",Ref("a"),INT(1)), BinOp("+", Ref("a"), INT(2))),
			Set(BinOp("+",Ref("a"),INT(1))), // x := a + 1
			Set(BinOp("+",Ref("a"),INT(1))), // call p(x;y)^4
			Set(BinOp("+",Ref("a"),INT(1))), // call p(x;y)_5
			Set(),
			Set(),
			Set()
		)


		val (in, out) = Analyses.availableExpressions(ast, 2)
		println(out)
		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	"Reaching definitions" should "analyze an assignment" in {
		val ast = Program(List(),Assig("x", BinOp("+", Ref("a"), INT(1))))
		AstUtils.labelNodes(ast)
		val expectedIn = List(
			Set(("a", -1)) // Only a occurs as a FreeVar
		)
		val expectedOut = List(
			Set(("x", 0), ("a", -1)) // x is set at label 0
		)

		val (in, out) = Analyses.reachingDefinitions(ast, 2)
		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	it should "analyze example 2.7" in {
		val program = "begin {x:=5 y:=1 while(x>1){y:=x*y x:=x-1}} end"
		val ast = WhileParser.parseAll(WhileParser.program, program).get
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

		val (in, out) = Analyses.reachingDefinitions(ast, 2)
		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	"Very Busy" should "analyze an assignment" in {
		val ast = Program(List(),Assig("x", BinOp("+", Ref("a"), INT(1))))
		AstUtils.labelNodes(ast)
		val expectedIn = List(
			Set(BinOp("+", Ref("a"), INT(1)))
		)
		val expectedOut = List(
			Set()
		)

		val (in, out) = Analyses.veryBusy(ast, 2)

		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	it should "analyze example 2.9" in {
		val program = "begin if a>b then {x:=b-a y:=a-b} else {y:=b-a x:=a-b} end"
		val ast = WhileParser.parseAll(WhileParser.program, program).get
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
		val(in, out) = Analyses.veryBusy(ast, 2)

		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	"Live Variables" should "analyze an assignment" in {
		val ast = Program(List(), Assig("x", BinOp("+", Ref("a"), INT(1))))
		AstUtils.labelNodes(ast)
		val expectedIn = List(
			Set("a")
		)
		val expectedOut = List(
			Set()
		)

		val (in, out) = Analyses.liveVariables(ast, 2)
		in should equal(expectedIn)
		out should equal(expectedOut)
	}

	it should "analyze example 2.11" in {
		val program = "begin {x:=2 y:=4 x:=1 if y>x then z:=y else z:=y*y x:=z} end"
		val ast = WhileParser.parseAll(WhileParser.program, program).get
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
		val(in, out) = Analyses.liveVariables(ast, 2)

		in should equal(expectedIn)
		out should equal(expectedOut)
	}
}
