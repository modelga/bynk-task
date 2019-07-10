package se.bynk.task

trait Matcher {
  def apply(needle: String): List[(String, Int)]
  def info: String
}

object Matching {
  def apply(dataSet: List[String], classifier: Classifier) = new Matcher {
    def apply(needle: String) =
      dataSet
        .map(fileName => (fileName, classifier(needle, fileName)))
        .sortWith(_._2 > _._2)

    def info = classifier.getClass.getSimpleName
  }

}
