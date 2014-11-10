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
}
