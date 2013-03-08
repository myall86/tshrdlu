package tshrdlu.twitter

/**
 * Copyright 2013 Jason Baldridge, Simon Hafner
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import twitter4j._
import collection.JavaConversions._

import akka.actor._
import akka.event.Logging
import scala.concurrent.duration._
import akka.util.Timeout

import tshrdlu.Lucene
import tshrdlu.util.{English,SimpleTokenizer}
import bridge._

/**
 * Base trait with properties default for Configuration.
 * Gets a Twitter instance set up and ready to use.
 */
trait TwitterInstance {
  val twitter = new TwitterFactory().getInstance
}

/**
 * Companion object for ReactiveBot with main method.
 */
object ReactiveBot {

  def main(args: Array[String]) {

    // Set timeout to 5 seconds. Should be plenty of time to process a
    // tweet.
    implicit val timeout = Timeout(5 seconds)

    val system = ActorSystem("Botty")

    val sample = new Streamer(system.actorOf(Props[Sampler], name = "sampler"))
    sample.stream.sample
    val reply = new Streamer(system.actorOf(Props[Replier], name = "replier"))
    reply.stream.user

  }
}

class Sampler extends Actor {
  var collector, writer: ActorRef = null

  override def preStart = {
    collector = context.actorOf(Props[Collector], name="collector")
    writer = context.actorOf(Props[LuceneWriter], name="lucenewriter")
  }

  def receive = {
    case status: Status => collector ! status
    case batch: List[Status] => writer ! batch
  }
}

class Receiver extends Actor {
  var replier : ActorRef = null

  override def preStart = {
    replier = context.actorOf(Props[Replier], name="replier")
  }

  // Recognize a follow command
  lazy val FollowRE = """(?i)(?<=follow)(\s+(me|@[a-z]+))+""".r

  def receive = {
    case status: Status => {
      val text = status.getText
      text match {
        case FollowRE() => {
          val FollowRE(follow) = text
          // TODO: Follow code - do we need that one? Given we use
          // sample.json
        }
        // case ... other behaviour
        case _ => replier ! status
      }
    }
  }
}

class Replier extends Actor {
	val twitter = new TwitterFactory().getInstance

  // Recognize a follow command
  lazy val FollowRE = """(?i)(?<=follow)(\s+(me|all_appliednlp|@[A-Za-z\d_]+))+""".r

  // Pull just the lead mention from a tweet.
  lazy val StripLeadMentionRE = """(?:)^@[A-Za-z\d_]+\s(.*)$""".r

  def receive = {
    // If this becomes a bottleneck, create subworkers.
    case status: Status => {
      // TODO: Reply n stuff.
	val username = twitter.getScreenName
	println("New status: " + status.getText)
    	val replyName = status.getInReplyToScreenName
    	if (replyName == username) {
      		println("*************")
      		println("New reply: " + status.getText)
      		val text = "@" + status.getUser.getScreenName + " " + doActionGetReply(status)
      		println("Replying: " + text)
      		val reply = new StatusUpdate(text).inReplyToStatusId(status.getId)
      		twitter.updateStatus(reply)
    	}
    }
  }

   /**
   * A method that possibly takes an action based on a status
   * it has received, and then produces a response.
   */
  def doActionGetReply(status: Status) = {
    val text = status.getText.toLowerCase
    val followMatches = FollowRE.findAllIn(text)
	if (!followMatches.isEmpty) {
      		val followSet = followMatches
				.next
				.drop(1)
				.split("\\s")
				.map {
	  				case "me" => status.getUser.getScreenName
					case "all_appliednlp" =>
	  				{
						twitter.getFollowersIDs("appliednlp", -1)
							.getIDs
							.map(id => twitter.showUser(id).getScreenName)
							.filter(screenName => screenName.endsWith("_anlp") && screenName != twitter.getScreenName)
							.mkString(" ")
	  				}
	  				case screenName => screenName.drop(1)
				}
				.toSet
      		followSet.foreach(follow =>
					if (follow.contains("_anlp") && follow.contains(" "))
						follow.split(" ").foreach(twitter.createFriendship)
					else twitter.createFriendship(follow)
				)
      		"OK. I FOLLOWED " + followSet.map(x => if(x.contains("_anlp") && x.contains(" ")) "all_appliednlp"
							else "@" + x
						).mkString(" ") + "."  
    	} else {
     		try {
			val StripLeadMentionRE(withoutMention) = text
			val query = SimpleTokenizer(withoutMention)
	    				.filter(_.length > 2)
	    				.toList
	    				.mkString(" ")
			Lucene.read(query)
      		} catch { 
			case _: Throwable => "???"
      		}
  
  	}
  }

}

class Collector extends Actor {
  val collected = scala.collection.mutable.ListBuffer[Status]()
  def receive = {
    case tweet: Status => {
      collected.append(tweet)
      if (collected.length == 100) {
        sender ! collected.toList
        collected.clear
      }
    }
  }
}

class LuceneWriter extends Actor {
  // Pull the RT and mentions from the front of a tweet.
  lazy val StripMentionsRE = """(?:)(?:RT\s)?(?:(?:@[A-Za-z\d_]+\s))+(.*)$""".r

  def receive = {
    case batch: List[Status] => {
	 val useableTweets = batch
      .map(_.getText)
      .map {
        case StripMentionsRE(rest) => rest
        case x => x
      }
      .filterNot(_.contains('@'))
      .filterNot(_.contains('/'))
      .filter(tshrdlu.util.English.isEnglish)
      .filter(tshrdlu.util.English.isSafe)
	
      Lucene.write(useableTweets)
      println(Lucene.writer.maxDoc)
    }
  }
}
