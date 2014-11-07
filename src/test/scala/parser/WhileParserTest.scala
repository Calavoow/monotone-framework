package parser

import org.scalatest.{FlatSpec, Matchers, FunSuite}
import parser.AST._


class WhileParserTest extends FlatSpec with Matchers {

	"The While Parser" should "parse a statement" in {
		val toParse = "a := 2"
		val expected = Assig("a", INT(2))
		val parsed = WhileParser.parseAll(WhileParser.statement, toParse)

		parsed.get should be (expected)
	}

	it should "parse an if-else" in {
		val toParse = "if true && true || false then x:=1 else { x:= 1 * 2 + 3 }"
		val expected = IfElse(
			BinBExp("||",
				BinBExp("&&",
					True,True
				),False
			),Assig("x",INT(1)),
			Block(List(
				Assig("x",BinOp("+",BinOp("*",INT(1),INT(2)),INT(3)))
			))
		)
		val parsed = WhileParser.parseAll(WhileParser.statement, toParse)

		parsed.get should be (expected)
	}
}
