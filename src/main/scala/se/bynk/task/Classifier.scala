package se.bynk.task

trait Classifier {
  type Classify = List[String] => Int
  def apply(needle: String): Classify
}

object SimpleClassifier extends Classifier {
  def apply(needle: String) = {
    case hayStack if hayStack.exists(_ contains needle) => 100
    case _                                              => 0
  }
}

object CaseInsensitiveClassifier extends Classifier {
  def apply(needle: String) = {
    val prepared = needle.toLowerCase()

    {
      case hayStack if hayStack.exists(_.toLowerCase() contains prepared) => 100
      case _                                                              => 0
    }
  }
}

object SplitStringClassifier extends Classifier {
  def apply(needle: String) = {
    val needles = needle.toLowerCase().split(" ")
    haystack => {
      val haystacks = haystack.flatMap(_.split(" "))
      val matchingParts = needles.intersect(haystacks).length
      (100 * matchingParts) / Math.min(needles.length, haystack.length)
    }
  }
}

object LevenshteinStringClassifier extends Classifier {
  /*
      Copied from https://oldfashionedsoftware.com/2009/11/19/string-distance-and-refactoring-in-scala/
   */
  def stringDistance(s1: String, s2: String): Int = {
    val memo = scala.collection.mutable.Map[(List[Char], List[Char]), Int]()
    def min(a: Int, b: Int, c: Int) = Math.min(Math.min(a, b), c)
    def sd(s1: List[Char], s2: List[Char]): Int = {
      if (!memo.contains((s1, s2)))
        memo((s1, s2)) = (s1, s2) match {
          case (_, Nil)             => s1.length
          case (Nil, _)             => s2.length
          case (c1 :: t1, c2 :: t2) => min(sd(t1, s2) + 1, sd(s1, t2) + 1, sd(t1, t2) + (if (c1 == c2) 0 else 1))
        }
      memo((s1, s2))
    }

    sd(s1.toList, s2.toList)
  }

  def apply(needles: String) = { haystacks =>
    {
      val tokensCount = haystacks.flatMap(_.split(" ")).length

      val distances: List[Int] = for {
        tokens <- haystacks
        token <- tokens.toLowerCase().split(" ")
        needle <- needles.toLowerCase().split(" ")
        dist = stringDistance(token, needle)
      } yield dist
      List(10000, 5000, 3000, 1000, 500).zipWithIndex
        .map { case (factor, distance) => factor * distances.count(_ == distance) }
        .map(_ / tokensCount)
        .reduce(Math.addExact)
    }
  }
}

object Classifier {
  val default = SimpleClassifier
  val getClassifier: String => Classifier = {
    case "simple"      => SimpleClassifier
    case "simple-ci"   => CaseInsensitiveClassifier
    case "split"       => SplitStringClassifier
    case "levenshtein" => LevenshteinStringClassifier
    case _             => default
  }
}
