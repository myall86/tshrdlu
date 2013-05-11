package tshrdlu.simMetrics

import tshrdlu.util.{POSTagger, SimpleTokenizer}
import tshrdlu.util.POSTagger.Token

/**
 * A helper object for computing textual similarity between two texts.
 */
object TextSim {

	/**
 	 * Compute the lexical overlap between two sequences of words
 	 */
	def LexicalOverlap(tokens1: Seq[String], tokens2: Seq[String]): Double =
	{
		val numOverlapTokens = tokens1.filter(tokens2.contains)
						.size

		val maxNumTokens = math.max(tokens1.size, tokens2.size)

		if(maxNumTokens > 0)
			numOverlapTokens.toDouble / maxNumTokens
		else 0
	}
	
	/**
 	 * Compute the lexical overlap between two texts
 	 */
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

	/**
 	 * Compute the topic overlap between two sequences of topics
 	 */
	def TopicOverlap(topics1: Seq[String], topics2: Seq[String]): Double =
	{
		val numOverlapTopics = topics1.filter(topics2.contains)
						.size

		val maxNumTopics = math.max(topics1.size, topics2.size)

		if(maxNumTopics > 0)
			numOverlapTopics.toDouble / maxNumTopics
		else 0
	}

	/**
 	 * Compute the Token overlap between two sequences of Tokens,
	 * where each Token is a pair of <word_form, POS>
 	 */
	def POSTokenOverlap(dPOSTokens1: Seq[Token], dPOSTokens2: Seq[Token]): Double =
	{
		val numOverlapTokens = dPOSTokens1.filter(dPOSTokens2.contains)
						.size

		val maxNumTokens = math.max(dPOSTokens1.size, dPOSTokens2.size)

		if(maxNumTokens > 0)
			numOverlapTokens.toDouble / maxNumTokens
		else 0
	}
}
