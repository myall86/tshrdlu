package tshrdlu

import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search._
import org.apache.lucene.store._
import org.apache.lucene.util.Version
import twitter4j.Status
import scala.collection.JavaConversions._

object Lucene {
  val index = new RAMDirectory()
  val analyzer = new StandardAnalyzer(Version.LUCENE_40)
  val config = new IndexWriterConfig(Version.LUCENE_40, analyzer)
  val writer = new IndexWriter(index, config)
  val parser = new QueryParser(Version.LUCENE_40, "text", analyzer)

  def write(tweets: Iterable[String]) {
    val documents = asJavaIterable(tweets.map({tweet =>
      val doc = new Document()
      doc.add(new TextField("text", tweet, Field.Store.YES))
      doc
    }))
    writer.addDocuments(documents)
    writer.commit()
  }

  def read(query: String): String = {
    val reader = DirectoryReader.open(index)
    val searcher = new IndexSearcher(reader)
    val collector = TopScoreDocCollector.create(1, true)
    searcher.search(parser.parse(query), collector)
    searcher.doc(collector.topDocs().scoreDocs(0).doc).get("text")
  }
}
