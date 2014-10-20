import parser.AST._
import shapeless._
import poly._

object Flow {

	var counter = -1
	def labelNodes(node: astNode) : astNode = {
//		counter += 1
//		node match {
//			case block: Block => block.label = counter
//				block.statements.foreach(labelNodes)
//			case w@While(cond, stmt) =>
//				w.label = counter
//				labelNodes(cond)
//				labelNodes(stmt)
//			case a@Assig(_, exp) =>
//				a.label = counter
//				labelNodes(exp)
//			case ie@IfElse(cond, s1, s2) =>
//				ie.label = counter
//				labelNodes(cond)
//				labelNodes(s1)
//				labelNodes(s2)
//
//			case e => e.label = counter
//		}
		node
	}

	def labelNode(node: astNode) = {
		counter += 1
		node.label = counter
	}
//
//
//	def flowGraph(ast: astNode): List[(Int, Int)] = {
//
//	}

}
