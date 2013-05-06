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
  import chalk.lang.eng.PorterStemmer
  import nak.util.CollectionUtil._

	lazy val stemmer = new PorterStemmer
	lazy val random = new scala.util.Random
	lazy val BEGIN_BOUNDARY = "[<b>]"
	lazy val END_BOUNDARY = "[<\\b>]"

  def receive = {
    case ReplyToStatus(status) => 
      val replyName = status.getUser.getScreenName
	val maxLength = 138-replyName.length
	val twitter = new TwitterFactory().getInstance
	val botName = twitter.getScreenName

	lazy val modeler = new TopicModeler("topic-keys.txt")

	// Collect the 15 most recent tweets sent to the Bot by the user named replyName
	val texts: Seq[String] = twitter.search(new Query("@" + botName + " from:" + replyName))
					.getTweets
					.toSeq
					.map{tweet => stripLeadMention(tweet.getText)}
					.filter(English.isEnglish)				
				
	// Top 5 tf tokens in previous context
	val topTokens = getTopTfTokens(texts, 5)

	// The sequence of <word_form, POS> pairs in the previous context
	val preContext = texts.flatMap { text =>
				POSTagger(text.toLowerCase)
					.filter {case Token(token, tag) => 
						token.length > 1
					}
				}
				//.distinct
				.map {case Token(token, tag) => Token(stemmer(token), tag)}

	// The sequence of <word_form, POS> pairs in the current context
	val curContext = POSTagger(status.getText.toLowerCase)
				.filter {case Token(token, tag) => 
					token.length > 1
				}
				//.distinct
				.map {case Token(token, tag) => Token(stemmer(token), tag)}
				
	// The sequence of topics in the previous context
	val preTopics = texts.flatMap {text => 
					val words = SimpleTokenizer(text.toLowerCase).toSeq
					words.flatMap(word => modeler.wordTopicsMap.get(word))
						.flatten				
				}

	// The sequence of topics in the current context
	val curTopics = SimpleTokenizer(status.getText.toLowerCase).toSeq
				.flatMap(word => modeler.wordTopicsMap.get(word))
				.flatten

	// Get retrieved tweets		
      val candidatesFuture = getReplies(status, topTokens, maxLength)
      candidatesFuture.map { candidates =>
	// Build a bigram model from returned tweets
	val bigramProb = createBigramModelFromTweets(candidates)

	val bestTweetFromBigram = createBestTweetFromBigram(bigramProb, maxLength)

	// Randomly sample 200 tweets from the bigram model
	val randomTweetsFromBigram = 
      			for (_ <- 1 to 200) 
			yield createRandomTweetFromBigram(bigramProb, maxLength)

	// Rank response candidates
	val responseCandidates = rankedResponses(
					// Add top 100 retrieved tweets to response candidates 
					((bestTweetFromBigram +: randomTweetsFromBigram.toSeq) ++ candidates.take(100)),
					curContext,
					preContext,
					curTopics,
					preTopics
				)
	
	// Print out top 10 response candidates
	println("********** Top 10 responses **********")
	for(i <- 0 to 9) println(responseCandidates(i) + "\n")
	
	println("*********** Best response **********")
	println(responseCandidates(0) + "\n")
	println("************************************")

        val reply = "@" + replyName + " " + responseCandidates(0)._1
        log.info("Candidate reply: " + reply)
        new StatusUpdate(reply).inReplyToStatusId(status.getId)
      } pipeTo sender
  }

  /**
   * Produce returned tweets from a status and a set of top tokens
   * in previous context.
   * @param status the status is sent to the Bot.
   * @param topTokens the top tokens in previous context.
   * @param maxLength the maximum length of each returned tweet.
   * @return A sequence of returned tweets.
   */
  def getReplies(status: Status, topTokens: Set[String], maxLength: Int): Future[Seq[String]]

	/**
          * Ranking the response candidates.
 
          * @param responseCandidate the response candidates.
   	  * @param curContext the sequence of <word_form, POS> pairs in the current context. 
   	  * @param preContext the sequence of <word_form, POS> pairs in the previous context. 
   	  * @param curTopics the sequence of topics in the current context. 
   	  * @param preTopics the sequence of topics in the previous context. 
   	  * @return A sequence of <response, score> pairs is sorted by score in decending order.
   	  */
	def rankedResponses(
		responseCandidate: Seq[String], 
		curContext: Seq[Token],
		preContext: Seq[Token],
		curTopics: Seq[String],
		preTopics: Seq[String]
	): Seq[(String, Double)] =
	{
		lazy val modeler = new TopicModeler("topic-keys.txt")
		responseCandidate.map { response =>
				val responseTokens = POSTagger(response.toLowerCase)
							.filter {case Token(token, tag) => 
								token.length > 1
							}
							.toSeq
							//.distinct
							.map {case Token(token, tag) => Token(stemmer(token), tag)}

				val responseTopics = SimpleTokenizer(response.toLowerCase).toSeq
							.flatMap(word => modeler.wordTopicsMap.get(word))
							.flatten
		
				// The final score is the sum of individual scores
				val score = TextSim.POSTokenOverlap(responseTokens, curContext) + 
						TextSim.POSTokenOverlap(responseTokens, preContext) +
						TextSim.TopicOverlap(responseTopics, curTopics) + 
						TextSim.TopicOverlap(responseTopics, preTopics) 
					
				(response, score)
			}
			.sortBy(-_._2)
	}

	/**
          * Computing the top tf tokens from a sequence of texts.
 
          * @param texts the sequence of input texts.
   	  * @param maxNumTokens the maximum number of returned top tokens. Default = 5. 
   	  * @return A set of top tf tokens.
   	  */
	def getTopTfTokens(texts: Seq[String], maxNumTokens: Int = 5): Set[String] =
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

    		val topTfTokens = totalCounts
      			.toSeq
      			.sortBy(_._2)
      			.takeRight(maxNumTokens)
      			.map(_._1)
      			.toSet

		topTfTokens
	}

	/**
          * Computing the top tf-idf tokens from a sequence of texts.
 
          * @param texts the sequence of input texts.
   	  * @param maxNumTokens the maximum number of returned top tokens. Default = 5. 
   	  * @return A set of top tf-idf tokens.
   	  */
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

	/**
 	 * Create the best tweet from a bigram model.
 	 */
	def createBestTweetFromBigram(bigramProb: Map[String,Map[String,Double]], maxLength: Int = 140): String =
	{
		var bigramUsed = Set[String]()
		var currentToken = BEGIN_BOUNDARY
		var text = ""

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

	/**
 	 * Create a random tweet from a bigram model.
 	 */
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
  def getReplies(status: Status, topTokens: Set[String], maxLength: Int = 140): Future[Seq[String]] = {
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
 * An actor that constructs replies to a given status and a previous context
 * using Lucene
 */
class LuceneReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.{English, Lucene, SimpleTokenizer}

  import context.dispatcher
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future

  lazy val modeler = new TopicModeler("topic-keys.txt")

  /**
   * Produce returned tweets from a status and a set of top tokens
   * in previous context. Using a topic model to extend the query.
   * @param status the status is sent to the Bot.
   * @param topTokens the top tokens in previous context.
   * @param maxLength the maximum length of each returned tweet. Default = 140.
   * @return A sequence of returned tweets.
   */
  def getReplies(status: Status, topTokens: Set[String], maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to do search replies via Lucene")
    val text = status.getText.toLowerCase
	  val StripLeadMentionRE(withoutMention) = text
	  val contextString = (topTokens ++ (SimpleTokenizer(withoutMention)
	    					.filter(_.length > 2)
						.filter(English.isSafe)
	    					.filterNot(English.stopwords)
	    					.toSet
					)
			).mkString(" ")
			.replaceAll("""[^a-zA-Z\s]""","")

	// Compute the topic list in contextString
	val topicList = contextString.split(" ")
				.toList
				.flatMap(word => modeler.wordTopicsMap.get(word))
				.flatten

	// Select the first 3 topics, then pick 3 words from each topic
	val topicWords:Set[String] = topicList.take(3)
						.map(topic =>
							modeler.topicWordsMap
								.getOrElse(topic, Set(" "))
								.filter(_.length > 2)
								.take(3)
						)
						.flatten
						.toSet

	val query = (contextString.split(" ").toSet ++ topicWords).mkString(" ")

	println("\n****************")
	println("<Top previous tokens> " + topTokens.mkString(" "))
	println("\n<Query> " + query)

      val replyLucene = Lucene.read(query)
    Future(replyLucene).map(_.filter(_.length <= maxLength))
  }

}

