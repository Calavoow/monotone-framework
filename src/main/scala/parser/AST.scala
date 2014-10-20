package parser

object AST {

	sealed trait AstNode {
		def label: Int
		def children : List[AstNode]
	}

	sealed trait BExp extends AstNode
	case class True(label: Int = -1) extends BExp {
		override def children = Nil
	}
	case class False(label: Int = -1) extends BExp {
		override def children = Nil
	}
	case class Not(e1: BExp, label: Int = -1) extends BExp {
		override def children = List(e1)
	}
	case class BinBExp(operator: String, e1: BExp, e2: BExp, label: Int = -1) extends BExp {
		override def  children = List(e1, e2)
	}
	case class RelationalExp(operator: String, e1: AExp, e2: AExp, label: Int = -1) extends BExp {
		override def children = List(e1, e2)
	}

	sealed trait AExp extends AstNode
	case class BinOp(operator: String, e1: AExp, e2: AExp, label: Int = -1) extends AExp {
		override def children = List(e1, e2)
	}
	case class INT(value: Int, label: Int = -1) extends AExp {
		override def children = Nil
	}
	case class Ref(id: String, label: Int = -1) extends AExp {
		override def children = Nil
	}

	sealed trait Statement extends AstNode
	case class Block(statements: List[Statement], label: Int = -1) extends Statement {
		override def children = statements
		override def toString = s"Block($statements)^$label"
	}
	case class While(conditional: BExp, stmt: Statement, label: Int = -1) extends Statement {
		override def children = List(conditional, stmt)
	}
	case class Assig(ref: String, exp: AExp, label: Int = -1) extends Statement {
		override def children = List(exp)
	}
	case class IfElse(condition: BExp, s1: Statement, s2: Statement, label: Int = -1) extends Statement {
		override def children = List(condition, s1, s2)
	}
	case class Skip(label: Int = -1) extends Statement {
		override def children = Nil
	}

}