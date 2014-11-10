package parser

import AST._

object AstUtil {

	def mapProcedures(node: AstNode) : Map[String, Procedure] = node match {
		case p@Procedure(name, _, _, _) => Map(name -> p)
		case n => n.children.map(mapProcedures).foldLeft(Map[String, Procedure]())(_ ++ _)
	}

	/**
	 * Labels relevant nodes with a unique number top-down.
	 *
	 * Nodes are relevant when they have the LabeledNode trait.
	 * @param node The AST node and all its children to label
	 * @return A counter for the next (unique) label.
	 */
	def labelNodes(node: AstNode, counter: Int =  0) : Int = {
		node match {
			case n: LabeledNode =>
				n.label = counter
				counter + 1
			case _ =>
				// Then label all children of the node
				node.children.foldLeft(counter)( (currentCounter, node) => {
					labelNodes(node, currentCounter)
				})
		}
	}

	/**
	 * labelToNode calculates a Map from label -> AST node.
	 * @param node The node and all its children to analyze.
	 * @return
	 */
	def labelToNode(node: AstNode): Map[Int, AstNode] = {
		node match {
			case n: LabeledNode => Map(n.label -> n)
			case _ => node.children.map(labelToNode).foldLeft(Map[Int,AstNode]())(_ ++ _)
		}
	}
}
