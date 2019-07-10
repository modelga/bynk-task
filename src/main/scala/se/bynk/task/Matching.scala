package se.bynk.task

trait Matcher {
  def apply(needle: String): List[(String, Int)]
  def info: String
}

object Matching {
  def apply(dataSet: List[String], classifier: Classifier) = new Matcher {

    def apply(needle: String) = {
      val classify = classifier(needle, Some(dataSet))

      dataSet
        .map(fileName => (fileName, classify(fileName)))
        .sortWith(_._2 > _._2)
    }

    def info = classifier.getClass.getSimpleName
  }

}
