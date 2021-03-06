package parser

import org.scalatest.{FlatSpec, Matchers}
import parser.AST._

class WhileParserSpec extends FlatSpec with Matchers {

	"The While Parser" should "parse a statement" in {
		val toParse = "a := 2"
		val expected = Assig("a", INT(2))
		val parsed = WhileParser.parseAll(WhileParser.statement, toParse)

		parsed.get should be (expected)
	}

	it should "parse an if-else" in {
		val toParse = "if true && true || false then x:=1 else { x:= 1 * 2 y:=3 }"
		val expected = IfElse(
			BinBExp("||",
				BinBExp("&&",
					True(),True()
				),False()
			),Assig("x",INT(1)),
			Sequence(List(
				Assig("x",BinOp("*",INT(1),INT(2))),
				Assig("y", INT(3))
			))
		)
		val parsed = WhileParser.parseAll(WhileParser.statement, toParse)

		parsed.get should be (expected)
	}

	it should "not accept a sequence of less than 2" in {
		val toParse = "{a:=1}"
		val parsed = WhileParser.parseAll(WhileParser.statement, toParse)

		parsed.isInstanceOf[WhileParser.NoSuccess] should be(true)
	}

	it should "parse a sequence of 2" in {
		val toParse = "{a:=1 b:=2}"
		val expected = Sequence(List(Assig("a", INT(1)), Assig("b", INT(2))))
		val parsed = WhileParser.parseAll(WhileParser.statement, toParse)

		parsed.get should be (expected)
	}

	it should "parse a basic program" in {
		val toParse = "begin a:=1 end"
		val expected = Program(Nil, Assig("a", INT(1)))
		val parsed = WhileParser.parseAll(WhileParser.program, toParse)

		parsed.get should be (expected)
	}

	it should "parse a program with a procedure call" in {
		val toParse = "begin proc p(val x; res y) is y:=x end call p(x;y) end"
		val expected = Program(List(Procedure("p", List("x"), "y", Assig("y", Ref("x")))), Call("p", List("x"), "y"))
		val parsed = WhileParser.parseAll(WhileParser.program, toParse)
		println(parsed)

		parsed.get should be (expected)
	}
}
