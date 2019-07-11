import org.scalatest.FlatSpec
import se.bynk.task.{Classifier, Matching}

class MatchingSpec extends FlatSpec {
  trait FakeClassifier {
    object FakeClassifier extends Classifier {
      override def apply(haystack: String, dataSet: Option[List[String]]): Classify = ???
    }
    val classifier = FakeClassifier
  }

  "A Matching object" should "be able to return class name of Classifier" in new FakeClassifier {
    assert(Matching(List(), classifier).info === "FakeClassifier$")
  }
  trait LengthClassifier {
    val classifier = new Classifier {
      override def apply(haystack: String, dataSet: Option[List[String]]): Classify = _.length
    }
  }

  it should "be able to sort the results based on return" in new LengthClassifier {
    val result = Matching(List("a", "bb", "ccc"), classifier).apply("not relevant")
    assume(result.length === 3, "Output list has to have the same long")
    assert(result(0)._2 === 3 && result(0)._1 === "ccc")
    assert(result(2)._2 === 1 && result(2)._1 === "a")

  }

}
