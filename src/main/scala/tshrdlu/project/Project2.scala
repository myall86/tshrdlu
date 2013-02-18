package tshrdlu.project

import twitter4j._
import tshrdlu.twitter._
import tshrdlu.util.{English,SimpleTokenizer}

object Dict {
  // Found the utils later... :-/
  def fromFile(path: String): Set[String] = {io.Source.fromInputStream(new java.util.zip.GZIPInputStream(new java.io.FileInputStream(path))).getLines.map(_.toLowerCase).toSet}
}

/**
 * Show only tweets that appear to be English.
 */
object EnglishStatusStreamer extends BaseStreamer with EnglishStatusListener

/**
 * Debug the English detector.
 */
object EnglishStatusStreamerDebug extends BaseStreamer with EnglishStatusListenerDebug

/**
 * Output only tweets detected as English.
 */
trait EnglishStatusListener extends StatusListenerAdaptor {

  /**
   * If a status' text is English, print it.
   */
  override def onStatus(status: Status) { 
    val text = status.getText
    if (isEnglish(text)) 
      println(text)
  }

  /**
   * Test whether a given text is written in English.
   */
  lazy val words = Dict.fromFile("src/main/resources/lang/eng/lexicon/words.gz")
  def isEnglish(text: String): Boolean = {
    val tokens = text.toLowerCase.split(" ").map(_.filter((('a' to 'z') :+ '\'').toSet)).filter(_.length > 3).filter("^(?:@|#|http)".r.findFirstMatchIn(_).isEmpty)
                                                                                                                    (tokens.count(words).toFloat / tokens.length) > 0.5 && ! (tokens.length == 0)
  }

}

/**
 * Output both English and non-English tweets in order to improve
 * the isEnglish method in EnglishStatusListener.
 */
trait EnglishStatusListenerDebug extends EnglishStatusListener {

  /**
   * For each status, print it's text, prefixed with the label
   * [ENGLISH] for those detected as English and [OTHER] for
   * those that are not.
   */
  override def onStatus(status: Status) { 
    val text = status.getText
    val prefix = if (isEnglish(text)) "[ENGLISH]" else "[OTHER]  "
    println(prefix + " " + text)
  }
}


/**
 * Output polarity labels for every English tweet and output polarity
 * statistics at default interval (every 100 tweets). Done for 
 * tweets from the Twitter sample.
 */
object PolarityStatusStreamer extends BaseStreamer with PolarityStatusListener

/**
 * Output polarity labels for every English tweet and output polarity
 * statistics at default interval (every 100 tweets). Filtered by provided
 * query terms.
 */
object PolarityTermStreamer extends FilteredStreamer with PolarityStatusListener with TermFilter

/**
 * Output polarity labels for every English tweet and output polarity
 * statistics at an interval of every ten tweets. Filtered by provided
 * query locations.
 */
object PolarityLocationStreamer extends FilteredStreamer with PolarityStatusListener with LocationFilter { override val outputInterval = 10 }


/**
 * For every tweet detected as English, compute its polarity based
 * on presence of positive and negative words, and output the label
 * with the tweet. Output statistics about polarity at specified
 * intervals (as given by the outputInterval variable).
 */
trait PolarityStatusListener extends EnglishStatusListener {

  import tshrdlu.util.DecimalToPercent

  val outputInterval = 100

  var numEnglishTweets = 0

  // Indices: 0 => +, 1 => -, 2 => ~
  val polaritySymbol = Array("+","-","~")
  var polarityCounts = Array(0.0,0.0,0.0)

  override def onStatus(status: Status) {
    val text = status.getText
    if (isEnglish(text)) {
      val polarityIndex = getPolarity(text)
      val polarityMagic = polarityIndex match {
        case x if (x > 0) => 0
        case x if (x < 0) => 1
        case x if (x == 0) => 2
      }
      polarityCounts(polarityMagic) += 1

      numEnglishTweets += 1

      println(polaritySymbol(polarityMagic) + ": " + text)

      if ((numEnglishTweets % outputInterval) == 0) {
	    println("----------------------------------------")
	    println("Number of English tweets processed: " + numEnglishTweets)
	    println(polaritySymbol.mkString("\t"))
	    println(polarityCounts.mkString("\t"))
	    println(polarityCounts.map(_/numEnglishTweets).map(DecimalToPercent).mkString("\t"))
	    println("----------------------------------------")
      }
    }
    
  }


  lazy val negativeWords = Dict.fromFile("src/main/resources/lang/eng/lexicon/negative-words.txt.gz")
  lazy val positiveWords = Dict.fromFile("src/main/resources/lang/eng/lexicon/positive-words.txt.gz")
  /**
   * Given a text, return its polarity:
   *   x > 0 for positive
   *   x < 0 for negative
   *   0 for neutral
   */
  def getPolarity(text: String) = {
    val tokens = text.toLowerCase.split(" ")
    tokens.count(positiveWords) compare tokens.count(negativeWords)
  }

}
