package tshrdlu.util



object POSTagger {
	import cmu.arktweetnlp.Tagger
      	import cmu.arktweetnlp.Tagger._
      	import scala.collection.JavaConversions._

	case class Token(token: String, tag: String)

      	lazy val tagger = new Tagger()
      	tagger.loadModel("/cmu/arktweetnlp/model.20120919")

      	def apply(tweet: String): List[Token] =
		asScalaBuffer(tagger.tokenizeAndTag(tweet))
			.toList
			.map(token => Token(token.token, token.tag))
}
