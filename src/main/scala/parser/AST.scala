package parser

object AST {

	sealed trait AstNode {
		def children : List[AstNode]
		def pp: String
	}

	sealed trait LabeledNode extends AstNode {
		var label = -1
	}

	sealed trait Block extends AstNode

	sealed trait Exp extends AstNode {
		def children: List[Exp]
	}

	/**
	 * The trait for binary expressions.
	 *
	 * These are all labeled, because any could occur as conditional in a Statement.
	 */
	sealed trait BExp extends Exp with Block with LabeledNode
	case class True() extends BExp {
		override def children = Nil
		override def pp = s"true^$label"
	}
	case class False() extends BExp {
		override def children = Nil
		override def pp = s"false^$label"
	}
	case class Not(e1: BExp) extends BExp {
		override def children = List(e1)
		override def pp = s"!^$label(${e1.pp})"
	}
	case class BinBExp(operator: String, e1: BExp, e2: BExp) extends BExp {
		override def  children = List(e1, e2)
		override def pp = s"${e1.pp} $operator^$label ${e2.pp}"
	}
	case class RelationalExp(operator: String, e1: AExp, e2: AExp) extends BExp {
		override def children = List(e1, e2)
		override def pp = s"${e1.pp} $operator^$label ${e2.pp}"
	}

	sealed trait AExp extends Exp
	case class BinOp(operator: String, e1: AExp, e2: AExp) extends AExp {
		override def children = List(e1, e2)
		override def pp = s"BinOp($operator, ${e1.pp}, ${e2.pp})"
	}
	case class INT(value: Int) extends AExp {
		override def children = Nil
		override def pp = s"INT($value)"
	}
	case class Ref(id: String) extends AExp {
		override def children = Nil
		override def pp = s"Ref($id)"
	}

	/**
	 * The Statements of the While language
	 */
	sealed trait Statement extends AstNode {
		def initLabel : Int
		def finalLabel: Set[Int]
		def blocks: Set[Block]
		def flow: Set[(Int, Int)]
		def reverseFlow: Set[(Int, Int)] = flow.map { case (l1, l2) => (l2, l1) }
	}

	case class Sequence(statements: List[Statement]) extends Statement {
		override def children = statements
		override def pp = s"{\n${statements.map(_.pp).mkString("\n")}\n}"

		/**
		 * Returns the label of the head of the statements.
		 *
		 * statements must be non-empty, because of the syntax.
		 * @return
		 */
		override def initLabel = statements.head.initLabel
		override def finalLabel = statements.last.finalLabel
		override def blocks = statements.map(_.blocks).reduce(_ ++ _)
		override def flow = {
			val stmtsFlows = statements.map(_.flow).reduce(_ ++ _)
			val seqFlow = for(s <- statements.sliding(2)) yield {
				s match {
					case List(s1, s2) => s1.finalLabel.map((_, s2.initLabel))
				}
			}

			stmtsFlows ++ seqFlow.reduce(_ ++ _)
		}
	}

	case class While(conditional: BExp, stmt: Statement) extends Statement {
		override def children = List(conditional, stmt)
		override def initLabel = conditional.label
		override def finalLabel = Set(conditional.label)
		override def blocks = stmt.blocks + conditional
		override def flow = {
			val backFlow = for(l <- stmt.finalLabel) yield (l, conditional.label)
			stmt.flow ++ backFlow.+((conditional.label, stmt.initLabel))
		}
		override def pp = s"while(${conditional.pp})\n${stmt.pp}"
	}

	case class Assig(ref: String, exp: AExp) extends Statement with Block with LabeledNode {
		override def children = List(exp)
		override def pp = s"[$ref := ${exp.pp}]^$label"
		override def initLabel = label
		override def finalLabel = Set(label)
		override def blocks = Set(this)
		override def flow = Set()
	}

	case class IfElse(condition: BExp, s1: Statement, s2: Statement) extends Statement {
		override def children = List(condition, s1, s2)
		override def initLabel = condition.label
		override def finalLabel: Set[Int] = s1.finalLabel ++ s2.finalLabel
		override def blocks = s1.blocks ++ s2.blocks + condition
		override def flow = {
			s1.flow ++ s2.flow ++ Set((condition.label, s1.initLabel), (condition.label, s2.initLabel))
		}
		override def pp = s"if(${condition.pp})\n${s1.pp}\nelse\n${s2.pp}"
	}

	case class Skip() extends Statement with Block with LabeledNode {
		override def children = Nil
		override def initLabel = label
		override def finalLabel: Set[Int] = Set(label)
		override def blocks = Set(this)
		override def flow = Set()
		override def pp = s"skip^$label"
	}

}