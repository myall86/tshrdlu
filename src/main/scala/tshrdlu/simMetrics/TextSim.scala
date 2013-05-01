package tshrdlu.simMetrics

import tshrdlu.util.SimpleTokenizer

object TextSim {

	def LexicalOverlap(dTokens1: Seq[String], dTokens2: Seq[String]): Double =
	{
		val numOverlapTokens = dTokens1.filter(dTokens2.contains)
						.size

		numOverlapTokens.toDouble / math.max(dTokens1.size, dTokens2.size)
	}
	
	def LexicalOverlap(text1: String, text2: String): Double =
	{
		val dTokens1 = SimpleTokenizer(text1)
			.map(_.toLowerCase)
			.filter(_.length > 2)
			.distinct	

		val dTokens2 = SimpleTokenizer(text2)
			.map(_.toLowerCase)
			.filter(_.length > 2)
			.distinct

		LexicalOverlap(dTokens1, dTokens2)
	}
}
