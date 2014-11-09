package analyzer

import scala.collection.mutable

object Monotone {
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
