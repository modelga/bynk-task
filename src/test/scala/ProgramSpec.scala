import org.scalactic.Prettifier
import org.scalactic.source.Position
import org.scalatest.FlatSpec
import se.bynk.task.{FileNotFound, Matcher, MissingPathArg, NotDirectory, Program, ReadFileError}

import scala.collection.mutable

class ProgramSpec extends FlatSpec {
  trait FakeMatcher {
    val matcher = new Matcher {
      def apply(needle: String): List[(String, Either[ReadFileError, Int])] = List()
      val info: String = "Fake"
    }
  }
  trait PreparedResultsMatcher {
    val matcher = new Matcher {
      def apply(needle: String) = List(("Item", Right(100)), ("Another", Right(50)))
      val info: String = "PreparedResults"
    }
  }

  trait LongerResultsMatcher {
    val results: List[(String, Either[ReadFileError, Int])] = (1 to 20).reverse.map(i => ("Item", Right(i))).toList
    val matcher = new Matcher {
      def apply(needle: String): List[(String, Either[ReadFileError, Int])] = results
      val info: String = "LongerResultsMatcher"
    }
  }
  trait ResultsWithFailuresMatcher {
    val results: List[(String, Either[ReadFileError, Int])] =
      (1 to 10).reverse.map(i => ("Item", if (i > 5) Right(i) else Left(MissingPathArg))).toList
    val matcher = new Matcher {
      def apply(needle: String): List[(String, Either[ReadFileError, Int])] = results
      val info: String = "LongerResultsMatcher"
    }
  }

  trait Streams {
    val readQueue = mutable.Queue[String]()
    val writeQueue = mutable.Queue[Any]()
    def read(): String = if (readQueue.isEmpty) ".exit" else readQueue.dequeue()
    def write(any: Any): Unit = writeQueue.enqueue(any)
  }

  "A Program" should "be able to read cwd" in {
    val result = Program.readFile(Array("./"))
    assume(result.isRight, "A readFile cannot get into cwd")
    result match {
      case Right(value) => assert(value.getPath === ".")
      case _            => assert(true)
    }
  }

  it should "throw exception for non directory path " in {
    val result = Program.readFile(Array("build.sbt"))
    assume(result.isLeft, "A readFile should throw a handled exception")
    result match {
      case Left(value) => assert(value === NotDirectory("Path [build.sbt] is not a directory"))
      case _           => assert(true)
    }
  }

  it should "throw exception if no path defined" in {
    val result = Program.readFile(Array())
    assume(result.isLeft, "A readFile should throw a handled exception")
    result match {
      case Left(value) => assert(value === MissingPathArg)
      case _           => assert(true)
    }
  }

  it should "be able to operate on fake streams" in new FakeMatcher with Streams {
    Program.iterate(matcher, read, write)
    assert(writeQueue.dequeue() === "search using Fake> ")
    assert(writeQueue.dequeue() === "Done.\n")
  }

  it should "be able to list results" in new PreparedResultsMatcher with Streams {
    readQueue.enqueue("Listing")
    Program.iterate(matcher, read, write)
    assert(writeQueue.dequeue() === "search using PreparedResults> ")
    assert(writeQueue.dequeue() === "1. Item score: 100\n")
    assert(writeQueue.dequeue() === "2. Another score: 50\n")
    assert(writeQueue.dequeue() === "search using PreparedResults> ")
    assert(writeQueue.dequeue() === "Done.\n")
  }

  it should "be able to use specific matcher" in new PreparedResultsMatcher with Streams {
    readQueue.enqueue("Listing")
    Program.iterate(matcher, read, write)
    assert(writeQueue.dequeue() === "search using PreparedResults> ")
    assert(writeQueue.dequeue() === "1. Item score: 100\n")
    assert(writeQueue.dequeue() === "2. Another score: 50\n")
    assert(writeQueue.dequeue() === "search using PreparedResults> ")
    assert(writeQueue.dequeue() === "Done.\n")
  }
  def expectedOutput(x: ((String, Either[ReadFileError, Int]), Int)): String = x match {
    case ((item, Right(score)), index) =>
      s"${index + 1}. $item score: $score\n"
    case ((item, Left(_)), index) =>
      s"${index + 1}. $item failed to load\n"
  }

  it should "be able to produce the output" in new LongerResultsMatcher with Streams {
    readQueue.enqueue("Listing")
    Program.iterate(matcher, read, write)
    assert(writeQueue.dequeue() === "search using LongerResultsMatcher> ")
    results
      .take(10)
      .zipWithIndex
      .foreach(result => assert(writeQueue.dequeue() === expectedOutput(result)))
    assert(writeQueue.dequeue() === "search using LongerResultsMatcher> ")
    assert(writeQueue.dequeue() === "Done.\n")
  }

  it should "be able to reduce the contains failures" in new ResultsWithFailuresMatcher with Streams {
    readQueue.enqueue("Listing")
    Program.iterate(matcher, read, write)
    assert(writeQueue.dequeue() === "search using LongerResultsMatcher> ")
    results
      .take(10)
      .zipWithIndex
      .foreach(result => assert(writeQueue.dequeue() === expectedOutput(result)))
    assert(writeQueue.dequeue() === "search using LongerResultsMatcher> ")
    assert(writeQueue.dequeue() === "Done.\n")
  }
}
