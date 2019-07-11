import org.scalatest.FlatSpec
import se.bynk.task.{Classifier, Content, Matching, ReadFileError}

class MatchingSpec extends FlatSpec {
  case class StringContent(value: String) extends Content {
    val name: String = value
    val content: Either[ReadFileError, List[String]] = Right(value.toCharArray.map(_.toString).toList)
  }
  trait FakeClassifier {
    object FakeClassifier extends Classifier {
      override def apply(needle: String): Classify = ???
    }
    val classifier = FakeClassifier
  }

  "A Matching object" should "be able to return class name of Classifier" in new FakeClassifier {
    assert(Matching(List(), classifier).info === "FakeClassifier$")
  }
  trait LengthClassifier {
    val classifier = new Classifier {
      override def apply(needle: String): Classify = (v1: List[String]) => v1.length
    }
  }

  it should "be able to sort the results based on return" in new LengthClassifier {
    val result = Matching(List("a", "bb", "ccc").map(StringContent), classifier).apply("not relevant")
    assume(result.length === 3, "Output list has to have the same long")
    assert(result(0)._2 === Right(3) && result(0)._1 === "ccc")
    assert(result(1)._2 === Right(2) && result(1)._1 === "bb")
    assert(result(2)._2 === Right(1) && result(2)._1 === "a")

  }

}
