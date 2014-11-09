package analyzer

import org.scalatest.{FlatSpec, Matchers}
import parser.AST._

class MonotoneSpec extends FlatSpec with Matchers{

	def allLabelsEqual(in: AstNode, expected: AstNode) {
		println(in + " " + expected)
		in.label should be(expected.label)
		in.children should have size(expected.children.size)
		val children = in.children zip expected.children
		children.foreach { case (c1, c2) => allLabelsEqual(c1, c2) }
	}

	"labelNodes" should "label a simple assignment correctly" in {
		val ast = Assig("a", INT(2))
		val labeledAst = Monotone.labelNodes(ast)

		val expected = Assig("a", INT(2))
		expected.label = 1
		expected.exp.label = 2

		allLabelsEqual(ast, expected)
	}

	"flow calculation" should "give a correct flow set for an if-else" in {
		// If(1 < 2) x:=1 else x:=2
		val ast = IfElse(RelationalExp("<", INT(1), INT(2)), Assig("x", INT(1)), Assig("x", INT(2)))
		Monotone.labelNodes(ast)
		val expected = Set(
			(1,2) //If -> conditional
			,(2,5) // conditional -> stmt 1
			,(2,7) // conditional -> stmt 2
		)
		ast.flow should equal(expected)
	}

	"blocks" should "calculate blocks for a while" in {
		val ast = While(BinBExp("&&", True(), False()), Skip())
		val expected = Set(
			BinBExp("&&", True(), False())
			, Skip()
		)

		ast.blocks should equal(expected)
	}
}
