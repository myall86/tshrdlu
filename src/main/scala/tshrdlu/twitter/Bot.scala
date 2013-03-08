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
    val reply = new Streamer(system.actorOf(Props[Receiver], name = "replier"))
    reply.stream.user

  }
}

object FollowAnlp extends TwitterInstance {
  def main(args: Array[String]) {
    twitter.getFollowersIDs("appliednlp", -1)
      .getIDs
      .filter({id =>
        val screenName = twitter.showUser(id).getScreenName
        screenName.endsWith("_anlp") && screenName != twitter.getScreenName})
      .toSet
      .foreach(twitter.createFriendship(_:Long))
  }
}

// The Sampler collects possible responses. Does not implement a
// filter for bot requests, so it should be connected to the sample
// stream. Batches tweets together using the collector so we don't
// need to add every tweet to the index on its own.
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

/**
 * Base trait with properties default for Configuration.
 * Gets a Twitter instance set up and ready to use.
 */
trait TwitterInstance {
  val twitter = new TwitterFactory().getInstance
  val FollowRE = """(?i)(?<=follow)(\s+(me|@[A-Za-z\d_]+))+""".r
}

// Holds the reply the bot should send.
case class Reply(reply: String, status: Status)

// Receives request to the bot and forwards them accordingly. Also
// sends replies back.
class Receiver extends Actor with TwitterInstance {
  var replier, follower : ActorRef = null

  override def preStart = {
    replier = context.actorOf(Props[Replier], name="replier")
    follower = context.actorOf(Props[Follower], name="follower")
  }

  def receive = {
    case status: Status => {
	  val username = twitter.getScreenName
	  println("New status: " + status.getText)
      val replyName = status.getInReplyToScreenName
      if (replyName == username) {
      	println("*************")
      	println("New reply: " + status.getText)
        val text = status.getText
        text match {
          case FollowRE() => follower ! status
          // case ... other behaviour
          case _ => replier ! status
        }
      }
    }
    case Reply(response, status) => {
      val text = "@" + status.getUser.getScreenName + " " + response
      println("Replying: " + text)
      val reply = new StatusUpdate(text).inReplyToStatusId(status.getId)
      twitter.updateStatus(reply)
    }
  }

}

// Processes a follow request.
class Follower extends Actor with TwitterInstance {
  def receive = {
    case status: Status => {
      val followSet = FollowRE.findAllIn(status.getText)
	    .next
	    .drop(1)
	    .split("\\s")
	    .map {
          case "me" => status.getUser.getScreenName
	      case screenName => screenName.drop(1)
	    }
	    .toSet

      followSet.foreach(twitter.createFriendship(_))
      val reply = "OK. I FOLLOWED " + followSet.map("@" + _).mkString(" ") + "."  
      sender ! Reply(reply, status)
    }
  }
}


// The Replier is responsible for responding to messages sent to the
// Bot. The way it responds is to choose the best tweet retrieved from
// Lucene database. The query issued is a string of tokens that
// have at least a length of 3.
class Replier extends Actor {
  val twitter = new TwitterFactory().getInstance

  // Pull just the lead mention from a tweet.
  lazy val StripLeadMentionRE = """(?:)^@[A-Za-z\d_]+\s(.*)$""".r

  def receive = {
    // If this becomes a bottleneck, create subworkers.
    case status: Status => {
      val text = status.getText.toLowerCase
	  val StripLeadMentionRE(withoutMention) = text
	  val query = SimpleTokenizer(withoutMention)
	    .filter(_.length > 2)
	    .toList
	    .mkString(" ")
      val reply = Lucene.read(query)
      if (reply.nonEmpty) {sender ! Reply(reply.get, status)}
      else {println("Found no witty reply :-(")}
    }
  }
}

// Collects until it reaches 100 and then sends them back to the
// sender and the cycle begins anew.
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

// The LuceneWriter actor extracts the content of each tweet, removes
// the RT and mentions from the front and select only tweets
// classified as not vulgar for indexing via Lucene.
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
    }
  }
}
