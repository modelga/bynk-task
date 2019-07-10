package se.bynk.task

trait Classifier {
  type Classify = String => Int
  def apply(haystack: String, dataSet: Option[List[String]] = None): Classify
}

object SimpleClassifier extends Classifier {
  def apply(needle: String, dataSet: Option[List[String]] = None) = {
    case hayStack if hayStack contains needle => 100
    case _                                    => 0
  }
}

object CaseInsensitiveClassifier extends Classifier {
  def apply(needle: String, dataSet: Option[List[String]] = None) = {
    val prepared = needle.toLowerCase()

    {
      case hayStack if hayStack.toLowerCase() contains prepared => 100
      case _                                                    => 0
    }
  }
}

object Classifier {
  val default = SimpleClassifier
  val getClassifier: String => Classifier = {
    case "simple"    => SimpleClassifier
    case "simple-ci" => CaseInsensitiveClassifier
    case _           => default
  }
}
