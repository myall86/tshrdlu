package tshrdlu.twitter

import akka.actor._
import twitter4j._

/**
 * An actor that constructs replies to a given status.
 */
trait BaseReplier extends Actor with ActorLogging {
  import Bot._
  import TwitterRegex._
  import tshrdlu.simMetrics.TextSim
  import tshrdlu.util.{English, LanguageModel, POSTagger, SimpleTokenizer}
  import tshrdlu.util.POSTagger.Token

  import context.dispatcher
  import scala.concurrent.Future
  import akka.pattern.pipe
  import collection.JavaConversions._
  import nak.util.CollectionUtil._

	lazy val random = new scala.util.Random
	lazy val BEGIN_BOUNDARY = "[<b>]"
	lazy val END_BOUNDARY = "[<\\b>]"

  def receive = {
    case ReplyToStatus(status) => 
      val replyName = status.getUser.getScreenName
	val maxLength = 138-replyName.length
	val twitter = new TwitterFactory().getInstance
	val botName = twitter.getScreenName

	// Collect the 15 most recent tweets sent to the Bot by the user named replyName
	val texts: Seq[String] = twitter.search(new Query("@" + botName + " from:" + replyName))
					.getTweets
					.toSeq
					.map{tweet => 
						val StripLeadMentionRE(withoutMention) = tweet.getText
						withoutMention
					}
					.filter(English.isEnglish)				
				
	val topTfIdfTokens = getTopTfIdfTokens(texts, 5)
/*
	val preContext = SimpleTokenizer(texts.mkString(" "))
				.map(_.toLowerCase)
				.filter(_.length > 2)
				.distinct

	val curContext = SimpleTokenizer(status.getText)
				.map(_.toLowerCase)
				.filter(_.length > 2)
				.distinct
*/
	val preContext = texts.flatMap { text =>
				POSTagger(text.toLowerCase)
					.filter {case Token(token, tag) => 
						token.length > 1
					}
				}
				.distinct

	val curContext = POSTagger(status.getText.toLowerCase)
				.filter {case Token(token, tag) => 
					token.length > 1
				}
				.distinct
		
      val candidatesFuture = getReplies(status, topTfIdfTokens, maxLength)
      candidatesFuture.map { candidates =>
	val bigramProb = createBigramModelFromTweets(candidates)

	val bestTweetFromBigram = createBestTweetFromBigram(bigramProb, maxLength)
	val randomTweetsFromBigram = 
      			for (_ <- 1 to 200) 
			yield createRandomTweetFromBigram(bigramProb, maxLength)

	val candidateResponses = (bestTweetFromBigram +: randomTweetsFromBigram.toSeq)
			.map { response =>
				val responseTokens = POSTagger(response.toLowerCase)
							.filter {case Token(token, tag) => 
								token.length > 1
							}
							.toSeq
							.distinct
		
				val score = (TextSim.POSTokenOverlap(responseTokens, curContext) + 
						TextSim.POSTokenOverlap(responseTokens, preContext)
					) / 2

/*
				val responseTokens = SimpleTokenizer(response)
							.map(_.toLowerCase)
							.filter(_.length > 2)
							.distinct

				val score = (TextSim.LexicalOverlap(responseTokens, curContext) + 
						TextSim.LexicalOverlap(responseTokens, preContext)
					) / 2
*/
				(response, score)
			}
			.sortBy(-_._2)

	println("********** Top 10 responses **********")
	for(i <- 0 to 9) println(candidateResponses(i) + "\n")
	//candidateResponses.foreach(response => println(response + "\n"))
	println("*********** Best response **********")
	println(candidateResponses(0) + "\n")
	println("************************************")
POSTagger(candidateResponses(0)._1).foreach(println)
        val reply = "@" + replyName + " " + candidateResponses(0)._1
        log.info("Candidate reply: " + reply)
        new StatusUpdate(reply).inReplyToStatusId(status.getId)
      } pipeTo sender
  }

  def getReplies(status: Status, topTfIdfTokens: Set[String], maxLength: Int): Future[Seq[String]]

	def getTopTfIdfTokens(texts: Seq[String], maxNumTokens: Int = 5): Set[String] =
	{
		val countsPerText = texts.map { text => {
      			SimpleTokenizer(text)
        		.map(_.toLowerCase)
			.filter(_.length > 2)
			.filter(English.isSafe)
			.filterNot(English.stopwords)
			.counts
    			}
		}

    		val totalCounts = countsPerText.foldLeft(Map[String, Int]()) {
      			(dfs, tcounts) =>
        			dfs ++ tcounts.map { 
	  				case (k, v) => k -> (v + dfs.getOrElse(k, 0)) 
				}
    		}

    		val docFreqs = countsPerText.foldLeft(Map[String, Int]()) { 
      			(dfs, tcounts) =>
        			dfs ++ tcounts.map { 
	  				case (k, v) => k -> (1 + dfs.getOrElse(k, 0)) 
				}
      		}

    		val tfidf = 
      			for ((k,v) <- totalCounts) 
			yield (k,v.toDouble/docFreqs(k))

    		val topTfIdfTokens = tfidf
      			.toSeq
      			.sortBy(_._2)
      			.takeRight(maxNumTokens)
      			.map(_._1)
      			.toSet

		topTfIdfTokens
	}

	def createBigramModelFromTweets(tweets: Seq[String]): Map[String,Map[String,Double]] =
	{
		println("\nNumber of returned tweets: " + tweets.size + "\n")

		// Add BEGIN_BOUNDARY & END_BOUNDARY to each tweet
		val data = tweets.map(tweet => BEGIN_BOUNDARY + " " + Tokenize(tweet).mkString(" ") + " " + END_BOUNDARY)
				.mkString(" ")

		// Construct the bigram model
      		LanguageModel.createBigramModel(data)

	}

	def createBestTweetFromBigram(bigramProb: Map[String,Map[String,Double]], maxLength: Int = 140): String =
	{
		var bigramUsed = Set[String]()
		val initialTokenMap = bigramProb(BEGIN_BOUNDARY)
		val numInitialTokens = initialTokenMap.size

		// Select randomly the first token for the response tweet from a list of words 
		// following the BEGIN_BOUNDARY token in the constructed bigram model.
		var currentToken = if(numInitialTokens > 0) initialTokenMap.toSeq(random.nextInt(numInitialTokens))._1
				else END_BOUNDARY
		var text = currentToken

		// Iteratively choose the best unused next token to append into the response tweet
		while(currentToken != END_BOUNDARY && text.length <= maxLength)
		{
			val candidates = bigramProb(currentToken)
				.filterNot{case (token, prob) => bigramUsed.contains(currentToken + " " + token)}
				.toSeq

			val nextToken = if (candidates.isEmpty) END_BOUNDARY
					else candidates.sortBy(_._2).last._1

			if (nextToken != END_BOUNDARY) text += " " + nextToken
			bigramUsed += (currentToken + " " + nextToken)
			currentToken = nextToken
		}
		text.replaceAll("""\s([\?!()\";\|\[\].,':])\s""", "$1").take(maxLength)
	}

	def createRandomTweetFromBigram(bigramProb: Map[String,Map[String,Double]], maxLength: Int = 140): String =
	{
		val initialTokenMap = bigramProb(BEGIN_BOUNDARY)
		val numInitialTokens = initialTokenMap.size

		// Select randomly the first token for the response tweet from a list of words 
		// following the BEGIN_BOUNDARY token in the constructed bigram model.
		var currentToken = if(numInitialTokens > 0) initialTokenMap.toSeq(random.nextInt(numInitialTokens))._1
				else END_BOUNDARY
		var text = currentToken

		// Iteratively choose the random next token to append into the response tweet
		while(currentToken != END_BOUNDARY && text.length <= maxLength)
		{
			val candidates = bigramProb(currentToken)
			val numNextTokens = candidates.size

			val nextToken = if (numNextTokens > 0) candidates.toSeq(random.nextInt(numNextTokens))._1
					else END_BOUNDARY


			if (nextToken != END_BOUNDARY) text += " " + nextToken
			currentToken = nextToken
		}
		text.replaceAll("""\s([\?!()\";\|\[\].,':])\s""", "$1").take(maxLength)
	}


	def Tokenize(text: String): IndexedSeq[String] =
	{
    		val starts = """(?:[#@])|\b(?:http)"""
   		 text.replaceAll("""([\?!()\";\|\[\].,:])""", " $1 ")
    			.trim
    			.split("\\s+")
    			.toIndexedSeq
    			.filterNot(x => x.startsWith(starts))
  	}

}

/**
 * An actor that constructs replies to a given status.
 */
class SynonymReplier extends BaseReplier {
  import Bot._ 
  import tshrdlu.util.English.synonymize
  import TwitterRegex._

  import context.dispatcher
  import scala.concurrent.Future

  def getReplies(status: Status, topTfIdfTokens: Set[String], maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply synonym")
    val text = stripLeadMention(status.getText).toLowerCase
    val synTexts = (0 until 100).map(_ => Future(synonymize(text))) 
    Future.sequence(synTexts).map(_.filter(_.length <= maxLength))
  }

}

/**
 * An actor that constructs replies to a given status.
 */
class StreamReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)

  /**
   * Produce a reply to a status.
   */
  def getReplies(status: Status, topTfIdfTokens: Set[String], maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply stream")

    val text = stripLeadMention(status.getText).toLowerCase
    
    // Get a sequence of futures of status sequences (one seq for each query)
    val statusSeqFutures: Seq[Future[Seq[Status]]] = SimpleTokenizer(text)
    .filter(_.length > 3)
    .filter(_.length < 10)
    .filterNot(_.contains('/'))
    .filter(tshrdlu.util.English.isSafe)
    .sortBy(- _.length)
    .take(3)
    .map(w => (context.parent ? SearchTwitter(new Query(w))).mapTo[Seq[Status]])

    // Convert this to a Future of a single sequence of candidate replies
    val statusesFuture: Future[Seq[Status]] =
      Future.sequence(statusSeqFutures).map(_.flatten)

    // Filter statuses to their text and make sure they are short enough to use.
    statusesFuture.map(_.flatMap(getText).filter(_.length <= maxLength))
  }


  /**
   * Go through the list of Statuses, filter out the non-English ones and
   * any that contain (known) vulgar terms, strip mentions from the front,
   * filter any that have remaining mentions or links, and then return the
   * head of the set, if it exists.
   */
  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    
    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else None
  }

}


/**
 * An actor that constructs replies to a given status based on synonyms.
 */
class SynonymStreamReplier extends StreamReplier {
  import Bot._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future

  import tshrdlu.util.English._
  import TwitterRegex._
  override implicit val timeout = Timeout(10000)


  override def getReplies(status: Status, topTfIdfTokens: Set[String], maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to do synonym search")
    val text = stripLeadMention(status.getText).toLowerCase

    // Get two words from the tweet, and get up to 5 synonyms each (including the word itself).
    // Matched tweets must contain one synonym of each of the two words.

    val query:String = SimpleTokenizer(text)
      .filter(_.length > 3)
      .filter(_.length < 10)
      .filterNot(_.contains('/'))
      .filter(tshrdlu.util.English.isSafe)
      .filterNot(tshrdlu.util.English.stopwords(_))
      .take(2).toList
      .map(w => synonymize(w, 5))
      .map(x=>x.mkString(" OR ")).map(x=>"("+x+")").mkString(" AND ")

    log.info("searched for: " + query)

    val futureStatuses = (context.parent ? SearchTwitter(new Query(query))).mapTo[Seq[Status]]

    futureStatuses.map(_.flatMap(getText).filter(_.length <= maxLength))
 }

}


/**
 * An actor that constructs replies to a given status.
 */
class BigramReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)

  /**
   * Produce a reply to a status using bigrams
   */
  lazy val stopwords = tshrdlu.util.English.stopwords_bot
  def getReplies(status: Status, topTfIdfTokens: Set[String], maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply stream")

    val text = stripLeadMention(status.getText).toLowerCase
    
    // Get a sequence of futures of status sequences (one seq for each query)

    val bigram = Tokenize(text)
      .sliding(2)
      .filterNot(z => (stopwords.contains(z(0))||stopwords.contains(z(1))))
      .flatMap{case Vector(x,y) => List(x+" "+y)}
      .toList
      .sortBy(-_.length)

    val statusSeqFutures: Seq[Future[Seq[String]]] = bigram
      .takeRight(5)
      .map(w => (context.parent ? SearchTwitter(new Query("\""+w+"\""))).mapTo[Seq[Status]].map(_.flatMap(getText).toSeq))
    
    //statusSeqFutures.foreach(println)
    // Convert this to a Future of a single sequence of candidate replies
    val statusesFuture: Future[Seq[String]] =
      extractText(statusSeqFutures,bigram.toList)

    //statusesFuture.foreach(println)
    // Filter statuses to their text and make sure they are short enough to use.
    statusesFuture.filter(_.length <= maxLength)
  }

  def extractText(statusList: Seq[Future[Seq[String]]],bigram:List[String]): Future[Seq[String]] = {
    val bigramMap = Future.sequence(statusList).map(_.flatten)
    //bigramMap.foreach(println)
    val sortedMap = bigramMap.map { tweet => {
      tweet.flatMap{ x => { 
        Tokenize(x)
          .sliding(2)
          .filterNot(z => (stopwords.contains(z(0))||stopwords.contains(z(1))))
          .map(bg => bg.mkString(" ") -> x) toMap
      }}.filter { case (p,q) => bigram.contains(p)}
    }}

    val bigramSeq = sortedMap.map(_.map(_._2))
    bigramSeq
  }

  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    
    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else None
  }

}

/**
 * An actor that constructs replies to a given status.
 */
class LuceneReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.{English, Lucene, SimpleTokenizer}

  import context.dispatcher
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future

  def getReplies(status: Status, topTfIdfTokens: Set[String], maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to do search replies via Lucene")
    val text = status.getText.toLowerCase
	  val StripLeadMentionRE(withoutMention) = text
	  val query = (topTfIdfTokens ++ (SimpleTokenizer(withoutMention)
	    					.filter(_.length > 2)
						.filter(English.isSafe)
	    					.filterNot(English.stopwords)
	    					.toSet
					)
			).mkString(" ")
			.replaceAll("""[^a-zA-Z\s]""","")

	println("\n****************")
	println("<Top tf-idf tokens> " + topTfIdfTokens.mkString(" "))
	println("\n<Query> " + query)

      val replyLucene = Lucene.read(query)
    Future(replyLucene).map(_.filter(_.length <= maxLength))
  }

}

