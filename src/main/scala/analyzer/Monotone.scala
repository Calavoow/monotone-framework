package analyzer

import scala.collection.mutable

object Monotone {
	/**
	 * The Maximal Fixed Point algorithm (MFP) calculates various program analyses according to the Monotone Framework.
	 * @param lub The Least Upper Bound function ⊔
	 * @param partialOrd A partial ordering ⊑
	 * @param F The program flow. Either forward or reverse flow.
	 * @param E The entry point of the program for this analyses.
	 * @param iota The extremal values of the analysis. A tuple of the form (ι,⊥)
	 * @param f The analysis transfer function from label×L -> L.
	 * @tparam L The type of the analysis function.
	 * @return The (entry-,exit-)set of a all nodes. (Analysis_○, Analysis_●)
	 */
	def MFP[L](lub : (L, L) => L
			,partialOrd: (L,L) => Boolean
	        ,F: Set[(Int, Int)]
			,IF: Set[(Int, Int, Int, Int)]
	        ,E: Set[Int]
	        ,iota: (L, L)
	        ,f: (Int, L) => L
			,k : Int): (Seq[L], Seq[L]) = {
		// Step 1: Initialisation of W and analysis
		// Sort labels from small to large.
		var W = F.toList.sorted.map((_, List[Int]()))
		// Make sure that there is at least one element in the Set.
		val numLabels = F.flatMap((el) => Set(el._1, el._2)).++(E).max + 1
		// Delta -> calling context
		val analysis = mutable.Map[List[Int], mutable.IndexedSeq[L]]().withDefault(_ => mutable.IndexedSeq.fill(numLabels)(iota._2))
		for(l <- E) {
			val seq = analysis(List())
			seq(l) = iota._1
			analysis(List()) = seq
		}

		val interFlowEntry = IF.map { case (a,b,_,_) => (a,b) }
		val interFlowExit = IF.map { case (_,_,c,d) => (c,d) }
		// Step 2: Iteration, updating W and analysis
		while(W.nonEmpty) {
			val ((l, m), delta) = W.head
			W = W.tail
			// Calculate the new calling context
			val curDelta = if(interFlowEntry.contains((l,m))) {
				val newDelta = (l :: delta).take(k)
				// Do the lub between the current state of the context and the incoming state
				val seq = analysis(newDelta)
				seq(l) = lub(seq(l), analysis(delta)(l))
				analysis(newDelta) = seq
				newDelta
			} else if(interFlowExit.contains((l,m)) && delta != Nil) {
				val newDelta = delta.tail
				// Do the lub between the current state of the context and the exiting state
				val seq = analysis(newDelta)
				seq(l) = lub(seq(l), analysis(delta)(l))
				analysis(newDelta) = seq
				newDelta
			} else delta
			val activeAnalysis = analysis(curDelta)
			val fResult = f(l, activeAnalysis(l))
			if(!partialOrd(fResult, activeAnalysis(m))) {
				activeAnalysis(m) = lub(activeAnalysis(m), fResult)
				// Only allow the current context to flow back to its originating context
				val contextExit = interFlowExit.filter{
					case _ if delta.isEmpty => false
					case (a,b) => b == delta.head + 1
				}
				for(followLabel <- (F -- interFlowExit) ++ contextExit if followLabel._1 == m) W = (followLabel, curDelta) :: W
			}
			// Make sure the map is updated
			analysis(curDelta) = activeAnalysis
		}

		// Return MFP_in and MFP_out
		val MFP_out = analysis(Nil).zipWithIndex.map { case (analysisResult, label) => f(label, analysis(Nil)(label)) }
		(analysis(Nil), MFP_out)
	}
}
