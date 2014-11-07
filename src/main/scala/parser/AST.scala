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

	sealed trait Statement extends AstNode
	case class Block(statements: List[Statement]) extends Statement {
		override def children = statements
		override def toString = s"Block($statements)^$label"
	}
	case class While(conditional: BExp, stmt: Statement) extends Statement {
		override def children = List(conditional, stmt)
	}
	case class Assig(ref: String, exp: AExp) extends Statement {
		override def children = List(exp)
		override def toString = s"Assig($ref, $exp)^$label"
	}
	case class IfElse(condition: BExp, s1: Statement, s2: Statement) extends Statement {
		override def children = List(condition, s1, s2)
	}
	case object Skip extends Statement {
		override def children = Nil
	}

}