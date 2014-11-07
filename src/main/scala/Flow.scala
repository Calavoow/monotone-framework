import parser.AST._

object Flow {

	/**
	 * Labels nodes with a number top-down.
	 * @param node The node to label
	 * @return
	 */
	def labelNodes(node: AstNode, counter: Int = 0) : AstNode = {
		val newCounter = counter + 1
		node.label = newCounter

		node.children.foldLeft(newCounter)( (currentCounter, node) => {
			val c = currentCounter + 1
			labelNodes(node, currentCounter)
			c
		})
		node
	}
}
