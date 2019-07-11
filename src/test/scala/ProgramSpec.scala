import org.scalatest.FlatSpec
import se.bynk.task.{Matcher, MissingPathArg, NotDirectory, Program}

import scala.collection.mutable

class ProgramSpec extends FlatSpec {
  trait FakeMatcher {
    val matcher = new Matcher {
      def apply(needle: String): List[(String, Int)] = List()
      val info: String = "Fake"
    }
  }
  trait PreparedResultsMatcher {
    val matcher = new Matcher {
      def apply(needle: String): List[(String, Int)] = List(("Item", 100), ("Another", 50))
      val info: String = "PreparedResults"
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
    }
  }

  it should "throw exception for non directory path " in {
    val result = Program.readFile(Array("build.sbt"))
    assume(result.isLeft, "A readFile should throw a handled exception")
    result match {
      case Left(value) => assert(value === NotDirectory("Path [build.sbt] is not a directory"))
    }
  }

  it should "throw exception if no path defined" in {
    val result = Program.readFile(Array())
    assume(result.isLeft, "A readFile should throw a handled exception")
    result match {
      case Left(value) => assert(value === MissingPathArg)
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
}
