import org.scalatest.FlatSpec
import se.bynk.task._

class ClassifierSpec extends FlatSpec {
  def simplify(classifier: Classifier) = (needle: String, hayStack: String) => {
    classifier(needle, Some(List(needle, hayStack)))(hayStack)
  }
  trait SimpleClassifier {
    val classifier = simplify(SimpleClassifier)
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
    assert(classifier("match", "Match") === 0)
  }

  trait CaseInsensitiveClassifier {
    val classifier = simplify(CaseInsensitiveClassifier)
  }

  "A CaseInsensitiveClassifier" should "return 100 for lowercase haystack" in new CaseInsensitiveClassifier {
    assert(classifier("match", "Match") === 100)
  }
  it should "return 100 for not being related to haystack string" in new CaseInsensitiveClassifier {
    assert(classifier("Match", "not related") === 0)
  }

  trait SplitClassifiers {
    val classifier = simplify(SplitStringClassifier)
  }

  "A SplitStringClassifier" should "return positive value if part matches" in new SplitClassifiers {
    assert(classifier("m a t c h", "to sma t c h") > 0)
  }
  it should "return 0 for two irrelevant strings" in new SplitClassifiers {
    assert(classifier("not matching", "irr relevant") === 0)
  }

  trait LevenshteinClassifier {
    val classifier = simplify(LevenshteinStringClassifier)
  }

  "A LevenshteinClassifier" should "return almost maximum value" in new LevenshteinClassifier {
    assert(classifier("metch", "match") === 4)
  }
  it should "return 0 for two irrelevant strings" in new LevenshteinClassifier {
    assert(classifier("bbbbb", "aaaaa") === 0)
  }
}
