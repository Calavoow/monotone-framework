package parser

import org.scalatest.{FlatSpec, Matchers, FunSuite}
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
			Seq(List(
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
		val expected = Seq(List(Assig("a", INT(1)), Assig("b", INT(2))))
		val parsed = WhileParser.parseAll(WhileParser.statement, toParse)

		parsed.get should be (expected)
	}
}
