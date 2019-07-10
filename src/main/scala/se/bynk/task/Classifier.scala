package se.bynk.task

trait Classifier {
  def apply(needle: String, haystack: String): Int
}

object SimpleClassifier extends Classifier {
  def apply(needle: String, hayStack: String) = {
    if (hayStack contains needle) 100
    else 0
  }
}

object CaseInsensitiveClassifier extends Classifier {
  def apply(needle: String, hayStack: String) = {
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
