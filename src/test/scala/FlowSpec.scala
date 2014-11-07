import org.scalatest.{Matchers, FlatSpec}
import parser.AST._

class FlowSpec extends FlatSpec with Matchers{

	def allLabelsEqual(in: AstNode, expected: AstNode) {
		println(in + " " + expected)
		in.label should be(expected.label)
		in.children should have size(expected.children.size)
		val children = in.children zip expected.children
		children.foreach { case (c1, c2) => allLabelsEqual(c1, c2) }
	}
	"labelNodes" should "label a simple assignment correctly" in {
		val ast = Assig("a", INT(2))
		val labeledAst = Flow.labelNodes(ast)

		val expected = Assig("a", INT(2))
		expected.label = 1
		expected.exp.label = 2

		allLabelsEqual(ast, expected)
	}
}
