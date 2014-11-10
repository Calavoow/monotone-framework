package parser

object AST {

	sealed trait AstNode {
		def children : List[AstNode]
		def pp: String
	}

	/**
	 * A trait for all nodes that should be labeled.
	 */
	sealed trait LabeledNode extends AstNode {
		var label = -1
	}

	/**
	 * A trait for all nodes that should be in the set of Blocks.
	 */
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
		override def pp = s"[${e1.pp} $operator ${e2.pp}]^$label"
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
		def flow(procs: Map[String, Procedure]): Set[(Int, Int)]
		def reverseFlow(procs: Map[String, Procedure]): Set[(Int, Int)] = flow(procs).map { case (l1, l2) => (l2, l1) }
		def interFlow(procs: Map[String, Procedure]) : Set[(Int, Int, Int, Int)]
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
		override def flow(procs: Map[String, Procedure]) = {
			val stmtsFlows = statements.map(_.flow(procs)).reduce(_ ++ _)
			val seqFlow = for(s <- statements.sliding(2)) yield {
				s match {
					case List(s1, s2) => s1.finalLabel.map((_, s2.initLabel))
				}
			}

			stmtsFlows ++ seqFlow.reduce(_ ++ _)
		}
		override def interFlow(procs: Map[String, Procedure]) : Set[(Int, Int, Int, Int)] = {
			statements.map(_.interFlow(procs)).foldLeft(Set[(Int,Int,Int,Int)]())(_ ++ _)
		}
	}

	case class While(conditional: BExp, stmt: Statement) extends Statement {
		override def children = List(conditional, stmt)
		override def initLabel = conditional.label
		override def finalLabel = Set(conditional.label)
		override def blocks = stmt.blocks + conditional
		override def flow(procs: Map[String, Procedure]) = {
			val backFlow = for(l <- stmt.finalLabel) yield (l, conditional.label)
			stmt.flow(procs) ++ backFlow.+((conditional.label, stmt.initLabel))
		}
		override def pp = s"while(${conditional.pp})\n${stmt.pp}"
		override def interFlow(procs: Map[String, Procedure]) : Set[(Int, Int, Int, Int)] = {
			stmt.interFlow(procs)
		}
	}

	case class Assig(ref: String, exp: AExp) extends Statement with Block with LabeledNode {
		override def children = List(exp)
		override def pp = s"[$ref := ${exp.pp}]^$label"
		override def initLabel = label
		override def finalLabel = Set(label)
		override def blocks = Set(this)
		override def flow(procs: Map[String, Procedure]) = Set()
		override def interFlow(procs: Map[String, Procedure]) : Set[(Int, Int, Int, Int)] = Set()
	}

	case class IfElse(condition: BExp, s1: Statement, s2: Statement) extends Statement {
		override def children = List(condition, s1, s2)
		override def initLabel = condition.label
		override def finalLabel: Set[Int] = s1.finalLabel ++ s2.finalLabel
		override def blocks = s1.blocks ++ s2.blocks + condition
		override def flow(procs: Map[String, Procedure]) = {
			s1.flow(procs) ++ s2.flow(procs) ++ Set((condition.label, s1.initLabel), (condition.label, s2.initLabel))
		}
		override def pp = s"if(${condition.pp})\n${s1.pp}\nelse\n${s2.pp}"
		override def interFlow(procs: Map[String, Procedure]) : Set[(Int, Int, Int, Int)] = {
			s1.interFlow(procs) ++ s2.interFlow(procs)
		}
	}

	case class Skip() extends Statement with Block with LabeledNode {
		override def children = Nil
		override def initLabel = label
		override def finalLabel: Set[Int] = Set(label)
		override def blocks = Set(this)
		override def flow(procs: Map[String, Procedure]) = Set()
		override def pp = s"skip^$label"
		override def interFlow(procs: Map[String, Procedure]) : Set[(Int, Int, Int, Int)] = Set()
	}

	case class Call(procedureName: String, variables: List[String], result: String) extends Statement with Block {
		val entry = ProcedureCall()
		val exit = ProcedureCall()
		override def initLabel = entry.label
		override def finalLabel = Set(exit.label)
		override def blocks = Set(this)
		override def flow(procs: Map[String, Procedure]) = {
			val p = procs(procedureName)
			Set(ProcFlow(entry.label, p.entry.label), ProcFlow(p.exit.label, exit.label))
		}
		override def children = List(entry, exit)
		override def pp = s"[call $procedureName(${variables.mkString(",")}, $result)]^${entry.label}_${exit.label}"
		override def interFlow(procs: Map[String, Procedure]) : Set[(Int, Int, Int, Int)] = {
			val p = procs(procedureName)
			Set((entry.label, p.entry.label, p.exit.label, exit.label))
		}
	}

	case class Program(procedures: List[Procedure], statement: Statement) extends AstNode {
		override def children: List[Statement] = procedures :+ statement
		override def pp = s"begin\n${procedures.map(_.pp).mkString("\n")}\n${statement.pp}\nend"
		def flow: Set[(Int, Int)] = {
			val procs = AstUtils.mapProcedures(this)
			children.map(_.flow(procs)).foldLeft(Set[(Int, Int)]())(_ ++ _)
		}
		def reverseFlow: Set[(Int, Int)] = flow.map { case (l1, l2) => (l2, l1) }
		def interFlow: Set[(Int, Int, Int, Int)] = {
			val procs = AstUtils.mapProcedures(this)
			children.map(_.interFlow(procs)).foldLeft(Set[(Int, Int, Int, Int)]())(_ ++ _)
		}
	}

	case class ProcedureCall() extends LabeledNode with Block {
		def children = Nil
		def pp = s"ProcedureCall()^$label"
	}

	case class Procedure(name: String, variables: List[String], result: String, statement: Statement) extends Statement {
		val entry = ProcedureCall()
		val exit = ProcedureCall()
		override def children = List(entry, statement, exit)
		override def pp = s"proc $name(val ${variables.mkString(",")}, res $result) is^${entry.label}\n${statement.pp}\nend^${exit.label}"

		override def initLabel = entry.label
		override def finalLabel = Set(exit.label)
		override def flow(procs: Map[String, Procedure]) = {
			statement.flow(procs).+((entry.label,statement.initLabel)) ++ statement.finalLabel.map((_, exit.label))
		}
		override def blocks = statement.blocks.+(entry).+(exit)
		override def interFlow(procs: Map[String, Procedure]) : Set[(Int, Int, Int, Int)] = {
			statement.interFlow(procs)
		}
	}


	object ProcFlow{
		def apply(s: Int, e: Int) = new ProcFlow(s,e)
		def unapply(pf: ProcFlow): Option[(Int, Int)] = Some((pf.start, pf.end))
	}
	/**
	 * Represents a special case of program flow. (start; end)
	 * @param start The entry or exit label
	 * @param end The entry or exit label
	 */
	class ProcFlow(val start: Int, val end: Int) extends Tuple2(start, end)
}