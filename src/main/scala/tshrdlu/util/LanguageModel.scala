package tshrdlu.util

import collection.mutable.HashMap

/**
 * A companion object for constructing unigram and bigram language model.
 */
object LanguageModel {

  /**
   * Construct a unigram language model from text
   */
  def createUnigramModel(text: String) = {
    val unigramCounts = HashMap[String,Double]().withDefaultValue(0.0)
    var numTokens = 1.0
    getTokens(text).foreach { word => { 
      unigramCounts(word) += 1
      numTokens += 1
    }}
    val smoothedTotal = numTokens + unigramCounts.size
    unigramCounts
      .mapValues(c => (c+1.0)/smoothedTotal)
      .toMap
      .withDefaultValue(1.0/smoothedTotal)
  }

  val BOUNDARY = "[###]"

  /**
   * Construct a bigram language model from text
   */
  def createBigramModel(text: String) = {
    val unigramModel: Map[String,Double] = createUnigramModel(text)

    val bigramCounts = HashMap[(String,String),Double]().withDefaultValue(0.0)
    val pairs = pad(getTokens(text))
      .sliding(2)
      .map{ case Array(prev, curr) => { (prev,curr) }}
      .toIndexedSeq

    val numTokens = pairs.length

    // The default used below is not a valid backoff, but cheap and dirty
    // and works well enough for present purposes.
    pairs
      .groupBy(_._1)
      .mapValues { listOfPrevCurrPairs => {
	val counts = listOfPrevCurrPairs
	  .map(_._2)
	  .groupBy(x=>x)
	  .mapValues(_.length.toDouble)

	val total = counts.values.sum
	val numWords = counts.size
	val denominator = total+numWords
	counts
	  .mapValues(c => (c+1.0)/(total+numWords))
	  .withDefaultValue(1.0/denominator)
      }}
      .withDefaultValue(Map[String,Double]().withDefault(x => unigramModel(x)))
  }

  // Inefficient, but works
  def pad(tokens: Array[String]) = 
    (BOUNDARY :: tokens.toList ::: List(BOUNDARY)).toArray
  
  def getTokens(text: String) = text
//    .replaceAll("""[^a-zA-Z\s]""","")
    .replaceAll("\\s+"," ")
    .split(" ")

}
