package se.bynk.task

import java.io.{File, FileNotFoundException, IOException}

import scala.io.Source

trait Content {
  val name: String
  val content: Either[ReadFileError, List[String]]
}

case class FileContent(path: File) extends Content {
  val name = path.getName
  lazy val content: Either[ReadFileError, List[String]] = {
    try {
      val handler = Source.fromFile(path)
      val lines = handler.getLines()
      Right(lines.toList)
    } catch {
      case e: FileNotFoundException => Left(FileNotFound(e))
      case e: IOException           => Left(IO(e))
    }
  }
}
