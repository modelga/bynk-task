import org.scalatest.FlatSpec
import se.bynk.task._

class ClassifierSpec extends FlatSpec {
  trait SimpleClassifier {
    val classifier = SimpleClassifier
  }

  "A getClassifier" should "return SimpleClassifier by default" in {
    assert(Classifier.getClassifier("") === SimpleClassifier)
  }

  it should "return SimpleClassifier for given 'simple'" in {
    assert(Classifier.getClassifier("simple") === SimpleClassifier)
  }

  it should "return CaseInsensitiveClassifier for given 'simple-cu'" in {
    assert(Classifier.getClassifier("simple-ci") === CaseInsensitiveClassifier)
  }

  "A SimpleClassifier" should "return 100 for same string" in new SimpleClassifier {
    assert(classifier("Match", "Match") === 100)
  }

  it should "return 100 for being part of string" in new SimpleClassifier {
    assert(classifier("Match", "A Match") === 100)
  }

  it should "return 0 for not being related to haystack string" in new SimpleClassifier {
    assert(classifier("Match", "not related") === 0)
  }

  it should "return 0 for lowercase haystack" in new SimpleClassifier {
    assert(classifier("Match", "match") === 0)
  }

  trait CaseInsensitiveClassifier {
    val classifier = CaseInsensitiveClassifier
  }

  "A CaseInsensitiveClassifier"
  it should "return 100 for lowercase haystack" in new CaseInsensitiveClassifier {
    assert(classifier("Match", "match") === 100)
  }
  it should "return 100 for not being related to haystack string" in new CaseInsensitiveClassifier {
    assert(classifier("Match", "not related") === 0)
  }
}
