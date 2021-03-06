package parser

import org.scalatest.{FlatSpec, Matchers}
import parser.AST._

class AstUtilsSpec extends FlatSpec with Matchers {
	def allLabelsEqual(in: AstNode, expected: AstNode) {
		(in, expected) match {
			case (x: LabeledNode, y: LabeledNode) =>
				x.label should be(y.label)
			case _ =>
		}
		in.children should have size(expected.children.size)
		val children = in.children zip expected.children
		children.foreach { case (c1, c2) => allLabelsEqual(c1, c2) }
	}

	"labelNodes" should "label a simple assignment correctly" in {
		val ast = Assig("a", INT(2))
		val labeledAst = AstUtils.labelNodes(ast)

		val expected = Assig("a", INT(2))
		expected.label = 0

		allLabelsEqual(ast, expected)
	}

	"flow calculation" should "give a correct flow set for an if-else" in {
		// If(1 < 2) x:=1 else x:=2
		val ast = IfElse(RelationalExp("<", INT(1), INT(2)), Assig("x", INT(1)), Assig("x", INT(2)))
		AstUtils.labelNodes(ast)
		val expected = Set(
			(0,1) // conditional -> stmt 1
			,(0,2) // conditional -> stmt 2
		)
		ast.flow(Map()) should equal(expected)
	}

	"blocks" should "calculate blocks for a while" in {
		val ast = While(BinBExp("&&", True(), False()), Skip())
		val expected = Set(
			BinBExp("&&", True(), False())
			, Skip()
		)

		ast.blocks should equal(expected)
	}

	"interflow" should "calculate the interflow for 1 procedure and call" in {
		val ast = Program(List(Procedure("p", List("x"), "y", Assig("y", Ref("x")))), Call("p", List("x"), "y"))
		AstUtils.labelNodes(ast)
		val expected = Set(
			(3,0,2,4)
		)

		ast.interFlow should equal(expected)
	}
}
