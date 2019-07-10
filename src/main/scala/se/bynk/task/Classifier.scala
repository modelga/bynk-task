package se.bynk.task

trait Classifier {

  def apply(haystack: String, needle: String): Int
}

object SimpleClassifier extends Classifier {
  def apply(hayStack: String, needle: String): Int = {
    if (hayStack contains needle) 100
    else 0
  }
}

object CaseInsensitiveClassifier extends Classifier {
  def apply(hayStack: String, needle: String): Int = {
    if (hayStack.toLowerCase() contains needle.toLowerCase()) 100
    else 0
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
