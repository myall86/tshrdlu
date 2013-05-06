package tshrdlu.util

import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search._
import org.apache.lucene.store._
import org.apache.lucene.analysis.en._
import org.apache.lucene.util.Version
import twitter4j.Status
import scala.collection.JavaConversions._

/**
 * A companion object handles reading and writing to Lucene. 
 * We use a specific EnglishAnalyzer which also adds synonyms and other magic.
 * The initial tweets are also stored directly in Lucene.
 * This object is threadsave, but you might find a slightly outdated version.
 */
object Lucene {
  val index = new RAMDirectory()
  val analyzer = new EnglishAnalyzer(Version.LUCENE_41)
  val config = new IndexWriterConfig(Version.LUCENE_41, analyzer)
  val writer = new IndexWriter(index, config)
  val parser = new QueryParser(Version.LUCENE_41, "text", analyzer)

  /**
   * Write a bag of tweets into Lucene database
   */
  def write(tweets: Iterable[String]) {
    val documents = asJavaIterable(tweets.map({tweet =>
      val doc = new Document()
      doc.add(new TextField("text", tweet, Field.Store.YES))
      doc
    }))
    writer.addDocuments(documents)
    writer.commit()
  }

  /**
   * Read tweets satisfying the query from Lucene database
   */
  def read(query: String): Seq[String] = {
    val reader = DirectoryReader.open(index)
    val searcher = new IndexSearcher(reader)
    // The maximum number of returned tweets is set to 1,000
    val collector = TopScoreDocCollector.create(1000, true)
    searcher.search(parser.parse(query), collector)
    collector.topDocs().scoreDocs.toSeq.map(_.doc).map(searcher.doc(_).get("text"))
  }
}
