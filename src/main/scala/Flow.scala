import parser.AST._

object Flow {

	/**
	 * Labels nodes with a number top-down.
	 * @param node The node to label
	 * @return
	 */
	def labelNodes(node: AstNode, counter: Int = 0) : Int = {
		val newCounter = counter + 1
		node.label = newCounter

		// Then label all children of the node
		node.children.foldLeft(newCounter)( (currentCounter, node) => {
			labelNodes(node, currentCounter)
		})
	}

	def flow(node: AstNode) : Map[Statement, Set[(Int, Int)]] = node match {
		case _ => Map()
	}
}
