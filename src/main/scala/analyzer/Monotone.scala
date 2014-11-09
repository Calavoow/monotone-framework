package analyzer

import parser.AST._
import scala.collection.mutable

object Monotone {

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

//	/**
//	 * A generalisation of each monotone framework analysis.
//	 * @param lub The least upper bound
//	 * @param F The flow set
//	 * @param E The entry point
//	 * @param iota The Initial or final analysis information
//	 * @param f The analysis transfer function
//	 * @tparam T The type of the analysis function
//	 */
//	def analysis[T](lub : (Set[T], Set[T]) => Set[T]
//	                ,F: Set[(Int, Int)]
//	                ,E: Set[Int]
//	                ,iota: (Set[T], Set[T])
//	                ,f: Int => Set[T] => Set[T]) : Set[T] = {
//		def iotaNoIsolated(l: Int) = if(E.contains(l)) iota._1 else iota._2
//		def in(l: Int) : Set[T] = {
//			val outRes = for(flow <- F if flow._2 == l) yield flow match {
//				case (k, _) => out(k)
//			}
//			lub(outRes.reduce(lub), iotaNoIsolated(l))
//		}
//
//		def out(l: Int) : Set[T] = f(l)(in(l))
//
//		Set()
//	}

	def MFP[L](lub : (L, L) => L
			,partialOrd: (L,L) => Boolean
	        ,F: Set[(Int, Int)]
	        ,E: Set[Int]
	        ,iota: (L, L)
	        ,f: (Int, L) => L): (Seq[L], Seq[L]) = {
		// Step 1: Initialisation of W and analysis
		// Sort labels from small to large.
		var W = F.toList.sorted
		// Make sure that there is at least one element in the Set.
		val numLabels = F.flatMap((el) => Set(el._1, el._2)).++(E).max + 1
		val analysis = mutable.IndexedSeq.fill(numLabels)(iota._2)
		for(l <- E) analysis(l) = iota._1

		// Step 2: Iteration, updating W and analysis
		while(W.nonEmpty) {
			val (l, m) = W.head
			W = W.tail
			val fResult = f(l, analysis(l))
			if(!partialOrd(fResult, analysis(m))) {
				analysis(m) = lub(analysis(m), fResult)
				for(followLabel <- F if followLabel._1 == m) W = followLabel :: W
			}
		}

		// Return MFP_in and MFP_out
		val MFP_out = analysis.zipWithIndex.map { case (analysisResult, label) => f(label, analysis(label)) }
		(analysis, MFP_out)
	}

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
		def findAExp(node: AstNode) : Set[BinOp] = node match {
			case b@BinOp(_, e1, e2) => findAExp(e1) ++ findAExp(e2) + b
			case n => n.children.map(findAExp).foldLeft(Set[BinOp]())(_ ++ _)
		}
		val aExpStar = findAExp(program)

		val iota = (Set[BinOp](), aExpStar)

		def killAE(label: Int) : Set[BinOp] = {
			val node = labelMap(label)
			node match {
				case Assig(x, exp) =>
					for(aExp <- aExpStar if FV(aExp).contains(x)) yield aExp
				case _ => Set()
			}
		}
		def genAE(label: Int) : Set[BinOp] = {
			val node = labelMap(label)
			node match {
				case Assig(x, exp) =>
					for(aExp <- findAExp(exp) if !FV(aExp).contains(x)) yield aExp
				case r@RelationalExp(op, e1, e2) => findAExp(r)
				case _ => Set()
			}
		}

		val f = f_l[BinOp](killAE, genAE) _

		MFP[L](lub, partialOrd, F, E, iota, f)
	}

	def f_l[T](kill: Int => Set[T], gen: Int => Set[T])(label: Int, currAnalysis: Set[T]) : Set[T] = {
		(currAnalysis -- kill(label)) ++ gen(label)
	}

	def FV(exp: Exp) : Set[String] = exp match {
		case Ref(v) => Set(v)
		case _ => exp.children.map(FV).foldLeft(Set[String]())(_ ++ _)
	}

	def labelToNode(node: AstNode): Map[Int, AstNode] = {
		node match {
			case n: LabeledNode => Map(n.label -> n)
			case _ => node.children.map(labelToNode).foldLeft(Map[Int,AstNode]())(_ ++ _)
		}
	}
}
