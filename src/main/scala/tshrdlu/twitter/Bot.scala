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

/**
 * Base trait with properties default for Configuration.
 * Gets a Twitter instance set up and ready to use.
 */
trait TwitterInstance {
  val twitter = new TwitterFactory().getInstance
}

case class ScrubGeo(userId: Long, upToStatusId: Long)
case class TrackLimitationNotice(numberOfLimitedStatuses: Int)

case class Block(source: User, blockedUser: User)
case class DeletionNotice(directMessageId: Long, userId: Long)
case class Favorite(source: User, target: User, favoritedStatus: Status)
case class Follow(source: User, followedUser: User)
case class Unblock(source: User, unblockedUser: User)
case class Unfavorite(source: User, target: User, unfavoritedStatus: Status)
case class UserListCreation(listOwner: User, list: UserList)
case class UserListDeletion(listOwner: User, list: UserList)
case class UserListMemberAddition(addedMember: User, listOwner: User, list: UserList)
case class UserListMemberDeletion(deletedMember: User, listOwner: User, list: UserList)
case class UserListSubscription(subscriber: User, listOwner: User, list: UserList)
case class UserListUnsubscription(subscriber: User, listOwner: User, list: UserList)
case class UserListUpdate(listOwner: User, list: UserList)
case class ProfileUpdate(user: User)

// Streaming the statuses to the actors.
class Streamer(actor: ActorRef) extends StreamInstance {
  class Listener extends UserStreamListener {
    // StatusListener
    def onStatus(status: Status) = actor ! status
    def onDeletionNotice(notice: StatusDeletionNotice) = actor ! notice
    def onScrubGeo(userId: Long, upToStatusId: Long) = actor ! ScrubGeo(userId, upToStatusId)
    def onStallWarning(warning: StallWarning) = actor ! warning
    def onTrackLimitationNotice(int: Int) = actor ! TrackLimitationNotice(int)
    def onException(ex: Exception) = actor ! akka.actor.Status.Failure(ex)
    // UserStreamListener
    def onBlock(source: User, blockedUser: User) = actor ! Block(source, blockedUser)
    def onDeletionNotice(directMessageId: Long, userId: Long) = actor ! DeletionNotice(directMessageId, userId)
    def onDirectMessage(directMessage: DirectMessage) = actor ! directMessage
    def onFavorite(source: User, target: User, favoritedStatus: Status) = actor ! Favorite(source, target, favoritedStatus)
    def onFollow(source: User, followedUser: User) = actor ! Follow(source, followedUser)
    def onFriendList(friendIds: Array[Long]) = actor ! friendIds
    def onUnblock(source: User, unblockedUser: User) = actor ! Unblock(source: User, unblockedUser: User)
    def onUnfavorite(source: User, target: User, unfavoritedStatus: Status) = actor ! Unfavorite(source, target, unfavoritedStatus)
    def onUserListCreation(listOwner: User, list: UserList) = actor ! UserListCreation(listOwner, list)
    def onUserListDeletion(listOwner: User, list: UserList) = actor ! UserListDeletion(listOwner, list)
    def onUserListMemberAddition(addedMember: User, listOwner: User, list: UserList) = actor ! UserListMemberAddition(addedMember, listOwner, list)
    def onUserListMemberDeletion(deletedMember: User, listOwner: User, list: UserList) = actor ! UserListMemberDeletion(deletedMember, listOwner, list)
    def onUserListSubscription(subscriber: User, listOwner: User, list: UserList) = actor ! UserListSubscription(subscriber, listOwner, list)
    def onUserListUnsubscription(subscriber: User, listOwner: User, list: UserList) = actor ! UserListUnsubscription(subscriber, listOwner, list)
    def onUserListUpdate(listOwner: User, list: UserList) = actor ! UserListUpdate(listOwner, list)
    def onUserProfileUpdate(updatedUser: User) = actor ! ProfileUpdate(updatedUser)
  }
  stream.addListener(new Listener)
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
  // Recognize a follow command
  lazy val FollowRE = """(?i)(?<=follow)(\s+(me|@[A-Za-z\d_]+))+""".r

  // Pull just the lead mention from a tweet.
  lazy val StripLeadMentionRE = """(?:)^@[A-Za-z\d_]+\s(.*)$""".r

  def receive = {
    // If this becomes a bottleneck, create subworkers.
    case status: Status => {
      // TODO: Reply n stuff.
	val username = "ckcuong_anlp" // TODO: need to get the real username
	println("New status: " + status.getText)
    	val replyName = status.getInReplyToScreenName
    	if (replyName == username) {
      		println("*************")
      		println("New reply: " + status.getText)
      		val text = "@" + status.getUser.getScreenName + " " + doActionGetReply(status)
      		println("Replying: " + text)
      		val reply = new StatusUpdate(text).inReplyToStatusId(status.getId)
//      		twitter.updateStatus(reply)
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
     try {
	val StripLeadMentionRE(withoutMention) = text
	val query = SimpleTokenizer(withoutMention)
	    .filter(_.length > 2)
//	    .toSet
//	    .take(3)
	    .toList
	    .mkString(" ")
	Lucene.read(query)
      }	catch { 
	case _: Throwable => "???"
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
