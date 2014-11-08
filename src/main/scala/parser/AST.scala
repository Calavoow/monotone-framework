package parser

object AST {

	sealed trait AstNode {
		var label: Int = -1
		def children : List[AstNode]
	}

	sealed trait BExp extends AstNode
	case object True extends BExp {
		override def children = Nil
	}
	case object False extends BExp {
		override def children = Nil
	}
	case class Not(e1: BExp) extends BExp {
		override def children = List(e1)
	}
	case class BinBExp(operator: String, e1: BExp, e2: BExp) extends BExp {
		override def  children = List(e1, e2)
	}
	case class RelationalExp(operator: String, e1: AExp, e2: AExp) extends BExp {
		override def children = List(e1, e2)
	}

	sealed trait AExp extends AstNode
	case class BinOp(operator: String, e1: AExp, e2: AExp) extends AExp {
		override def children = List(e1, e2)
	}
	case class INT(value: Int) extends AExp {
		override def children = Nil
//		override def toString = s"INT($value)^$label"
	}
	case class Ref(id: String) extends AExp {
		override def children = Nil
	}

	/**
	 * The Statements of the While language
	 */
	sealed trait Statement extends AstNode {
		def initLabel : Int
		def finalLabel: Set[Int]
	}

	case class Block(statements: List[Statement]) extends Statement {
		override def children = statements
		override def toString = s"Block($statements)^$label"

		/**
		 * Returns the label of the head of the statements.
		 *
		 * statements must be non-empty, because of the syntax.
		 * @return
		 */
		override def initLabel = statements.head.label

		override def finalLabel = statements.last.finalLabel
	}

	case class While(conditional: BExp, stmt: Statement) extends Statement {
		override def children = List(conditional, stmt)
		override def initLabel = conditional.label
		override def finalLabel = Set(conditional.label)
	}

	case class Assig(ref: String, exp: AExp) extends Statement {
		override def children = List(exp)
		override def toString = s"Assig($ref, $exp)^$label"
		override def initLabel = label
		override def finalLabel = Set(label)
	}

	case class IfElse(condition: BExp, s1: Statement, s2: Statement) extends Statement {
		override def children = List(condition, s1, s2)
		override def initLabel = condition.label
		override def finalLabel: Set[Int] = s1.finalLabel ++ s2.finalLabel
	}

	case object Skip extends Statement {
		override def children = Nil
		override def initLabel = label
		override def finalLabel: Set[Int] = Set(label)
	}

}