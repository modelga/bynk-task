package se.bynk.task

trait Matcher {
  def apply(needle: String): List[(String, Either[ReadFileError, Int])]
  val info: String
}

object Matching {
  def apply(dataSet: List[Content], classifier: Classifier) = new Matcher {

    def apply(needle: String) = {
      val classify = classifier(needle)

      val result = dataSet
        .map(
          content =>
            content.content.fold(
              error => (content.name, Left(error)),
              lines => (content.name, Right(classify(lines)))
            )
        )

      result.sortWith {
        case ((_, Right(a)), (_, Right(b))) => a > b
        case ((_, Right(_)), (_, Left(_)))  => true
        case ((_, Left(_)), (_, Right(_)))  => false
        case ((a, Left(_)), (b, Left(_)))   => a > b
      }
    }

    val info = classifier.getClass.getSimpleName
  }

}
