package analyzer

import parser.AST._

object Analyses {
	def aExp(program: Statement) : (Seq[Set[BinOp]], Seq[Set[BinOp]]) = {
		type L = Set[BinOp]

		val labelMap = labelToNode(program)

		// s1 intersect s2
		val lub = (s1: L, s2: L) => s1 intersect s2

		// l1 superset l2
		def partialOrd(l1: L, l2: L): Boolean = {
			l2.subsetOf(l1)
		}

		val F = program.flow
		val E = Set(program.initLabel)

		// Calculate AExp_*
		val aExpStar = findAExp(program)

		val iota = (Set[BinOp](), aExpStar)

		def killAE(label: Int) : L= {
			val node = labelMap(label)
			node match {
				case Assig(x, exp) =>
					for(aExp <- aExpStar if FV(aExp).contains(x)) yield aExp
				case _ => Set()
			}
		}
		def genAE(label: Int) : L = {
			val node = labelMap(label)
			node match {
				case Assig(x, exp) =>
					for(aExp <- findAExp(exp) if !FV(aExp).contains(x)) yield aExp
				case r@RelationalExp(op, e1, e2) => findAExp(r)
				case _ => Set()
			}
		}

		val f = f_l[BinOp](killAE, genAE) _

		Monotone.MFP[L](lub, partialOrd, F, E, iota, f)
	}

	def rDef(program: Statement) : (Seq[Set[(String, Int)]], Seq[Set[(String, Int)]]) = {
		type L = Set[(String, Int)]

		val labelMap = labelToNode(program)

		// s1 union s2
		val lub = (s1: L, s2: L) => s1 ++ s2

		// l1 subset l2
		def partialOrd(l1: L, l2: L): Boolean = {
			l1.subsetOf(l2)
		}

		val F = program.flow
		val E = Set(program.initLabel)

		val iota = (FV(program).map((_, -1)), Set[(String, Int)]())

		def findAssig(x: String, node: AstNode) : Set[Int] = node match {
			case a@Assig(v, _) if x == v => Set(a.label)
			case n => n.children.map(findAssig(x, _)).foldLeft(Set[Int]())(_ ++ _)
		}
		def killRD(label: Int) : L = {
			val node = labelMap(label)
			node match {
				case Assig(x, exp) =>
					findAssig(x, program).+(-1).map((x, _))
				case _ => Set[(String, Int)]()
			}
		}
		def genRD(label: Int) : L = {
			val node = labelMap(label)
			node match {
				case Assig(x, exp) => Set((x, label))
				case _ => Set[(String, Int)]()
			}
		}

		val f = f_l[(String, Int)](killRD, genRD) _

		Monotone.MFP[L](lub, partialOrd, F, E, iota, f)
	}

	def veryBusy(program: Statement) : (Seq[Set[BinOp]], Seq[Set[BinOp]]) = {
		type L = Set[BinOp]
		val emptySet = Set[BinOp]()

		val labelMap = labelToNode(program)

		// s1 intersect s2
		val lub = (s1: L, s2: L) => s1 intersect s2

		// l1 superset l2
		def partialOrd(l1: L, l2: L): Boolean = {
			l2.subsetOf(l1)
		}

		val F = program.reverseFlow
		val E = program.finalLabel

		val aExpStar = findAExp(program)
		val iota = (emptySet, aExpStar)


		def killVB(label: Int) : L = {
			val node = labelMap(label)
			node match {
				case Assig(x, exp) =>
					for(aExp <- aExpStar if FV(aExp).contains(x)) yield aExp
				case _ => emptySet
			}
		}
		def genVB(label: Int) : L = {
			val node = labelMap(label)
			node match {
				case a@Assig(_, _) => findAExp(a)
				case r@RelationalExp(_, _, _) => findAExp(r)
				case _ => emptySet
			}
		}

		val f = f_l[BinOp](killVB, genVB) _

		// Note: We have to switch in and out set, because of reversed flow.
		val (mfp_in, mfp_out) = Monotone.MFP[L](lub, partialOrd, F, E, iota, f)
		(mfp_out, mfp_in)
	}

	def f_l[T](kill: Int => Set[T], gen: Int => Set[T])(label: Int, currAnalysis: Set[T]) : Set[T] = {
		(currAnalysis -- kill(label)) ++ gen(label)
	}

	/**
	 * Find all AExp that are this node or children of it.
	 * @param node The node to start looking from
	 * @return A set of BinOp that are signficant AExp.
	 */
	def findAExp(node: AstNode) : Set[BinOp] = node match {
		case b@BinOp(_, e1, e2) => findAExp(e1) ++ findAExp(e2) + b
		case n => n.children.map(findAExp).foldLeft(Set[BinOp]())(_ ++ _)
	}

	def FV(exp: AstNode) : Set[String] = exp match {
		case Ref(v) => Set(v)
		case _ => exp.children.map(FV).foldLeft(Set[String]())(_ ++ _)
	}

	/**
	 * Labels nodes with a number top-down.
	 * @param node The node to label
	 * @return
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

	def labelToNode(node: AstNode): Map[Int, AstNode] = {
		node match {
			case n: LabeledNode => Map(n.label -> n)
			case _ => node.children.map(labelToNode).foldLeft(Map[Int,AstNode]())(_ ++ _)
		}
	}
}
