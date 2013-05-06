package tshrdlu.simMetrics

import tshrdlu.util.{POSTagger, SimpleTokenizer}
import tshrdlu.util.POSTagger.Token

object TextSim {

	def LexicalOverlap(tokens1: Seq[String], tokens2: Seq[String]): Double =
	{
		val numOverlapTokens = tokens1.filter(tokens2.contains)
						.size

		numOverlapTokens.toDouble / math.max(tokens1.size, tokens2.size)
	}
	
	def LexicalOverlap(text1: String, text2: String): Double =
	{
		val tokens1 = SimpleTokenizer(text1)
			.map(_.toLowerCase)
			.filter(_.length > 2)
			//.distinct	

		val tokens2 = SimpleTokenizer(text2)
			.map(_.toLowerCase)
			.filter(_.length > 2)
			//.distinct

		LexicalOverlap(tokens1, tokens2)
	}

	def TopicOverlap(topics1: Seq[String], topics2: Seq[String]): Double =
	{
		val numOverlapTopics = topics1.filter(topics2.contains)
						.size

		numOverlapTopics.toDouble / math.max(topics1.size, topics2.size)
	}

	def POSTokenOverlap(dPOSTokens1: Seq[Token], dPOSTokens2: Seq[Token]): Double =
	{
		val numOverlapTokens = dPOSTokens1.filter(dPOSTokens2.contains)
						.size

		numOverlapTokens.toDouble / math.max(dPOSTokens1.size, dPOSTokens2.size)
	}
}
