import java.io.{File, FileNotFoundException}

import org.scalatest.FlatSpec
import se.bynk.task.{FileContent, FileNotFound}

class FileContentSpec extends FlatSpec {
  "A FileContent.content" should "has FileNotFound for directory" in {
    val content = FileContent(new File("./")).content
    content match {
      case Left(FileNotFound(value)) => assert(value.isInstanceOf[FileNotFoundException])
      case Left(e)                   => assert(false, e)
      case Right(_)                  => assert(false, "got access to directory content")
    }
  }
  "A FileContent.content" should "has FileNotFound for non-existing path" in {
    val content = FileContent(new File("./non-existing")).content
    content match {
      case Left(FileNotFound(value)) => assert(value.isInstanceOf[FileNotFoundException])
      case Left(e)                   => assert(false, e)
      case Right(_)                  => assert(false, "got access to non-existing path")
    }
  }
  "A FileContent.content" should "access content" in {
    val content = FileContent(new File("./build.sbt")).content
    content match {
      case Left(_)  => assert(false, "couldn't access file")
      case Right(_) => assert(true)
    }
  }
}
