package analyzer

import parser.AST._
import scala.collection.mutable

object Monotone {

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
	        ,f: L => L): Seq[L] = {
		// Step 1: Initialisation of W and analysis
		// Sort labels from small to large.
		var W = F.toList.sorted
		val numLabels = F.flatMap((el) => Set(el._1, el._2)).max
		val analysis = mutable.IndexedSeq.fill(numLabels)(iota._2)
		for(l <- E) analysis(2) = iota._1

		// Step 2: Iteration, updating W and analysis
		while(W.nonEmpty) {
			val (l, m) = W.head
			W = W.tail
			val fResult = f(analysis(l))
			if(!partialOrd(fResult, analysis(m))) {
				analysis(m) = lub(analysis(m), fResult)
				for(followLabel <- F if followLabel._1 == m) W = followLabel :: W
			}
		}

		analysis
	}

	def aExp(program: Statement) : Set[Exp] = {
		type L = Exp
		val lub = (s1: Set[L], s2: Set[L]) => s1.intersect(s2)
		val F = program.flow
		val E = Set(program.initLabel)

		def findAllAExp(node: AstNode) : Set[BinOp] = node match {
			case b@BinOp(_, e1, e2) => findAllAExp(e1) ++ findAllAExp(e2) + b
			case n => n.children.map(findAllAExp).reduce(_ ++ _)
		}
		val aExpStar = findAllAExp(program)

		val iota = (Set(), aExpStar)

		def f = ???
		analysis(lub, F, E, iota, f)
	}

	def FV(exp: Exp) : Set[String] = exp match {
		case Ref(v) => Set(v)
		case _ => exp.children.map(FV).reduce(_ ++ _)
	}
}
