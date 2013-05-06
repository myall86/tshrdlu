package tshrdlu.twitter

import io.Source

/**
 * A class for creating a topic modeler 
 * @param inputFile the input file contains a list of word-topic map
 */
class TopicModeler(val inputFile: String) {
  def inputIterator = Source.fromFile(inputFile).getLines.toList

  // Each element is a <word, topic> pair
  lazy val tuples = inputIterator.map { line =>
    val split = line.split(" ")
    (split(0) -> split(1))
  }

  // A map between each word and a set of topics to which the word belongs
  lazy val wordTopicsMap = tuples.groupBy(e => e._1).mapValues(e => e.map(x => x._2).toSet)

  // Each element is a <topic, word> pair
  lazy val tuplesRev = inputIterator.map { line =>
    val split = line.split(" ")
    (split(1) -> split(0))
  }

  // A map between each topic and a set of words that the topic contains
  lazy val topicWordsMap = tuplesRev.groupBy(e => e._1).mapValues(e => e.map(x => x._2).toSet)
}

