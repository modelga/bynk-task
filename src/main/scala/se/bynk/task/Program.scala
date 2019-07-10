package se.bynk.task

sealed trait ReadFileError
case object MissingPathArg extends ReadFileError
case class NotDirectory(error: String) extends ReadFileError
case class FileNotFound(t: Throwable) extends ReadFileError

import java.io.File
import scala.util.Try
import scala.language.postfixOps
import scala.io.StdIn.readLine

object Program {

  type Read = () => String
  type Write = Any => Unit

  def readFile(args: Array[String]): Either[ReadFileError, File] = {
    for {
      path <- args.headOption.toRight(MissingPathArg)
      file <- Try(new java.io.File(path))
        .fold(
          throwable => Left(FileNotFound(throwable)),
          file =>
            if (file.isDirectory) Right(file)
            else Left(NotDirectory(s"Path [$path] is not a directory"))
        )
    } yield file
  }

  def prepareData(file: File, classifier: Classifier): Matcher = {
    val dataSet = file.list().filter(_.endsWith(".txt")).toList
    Matching(dataSet, classifier)
  }

  def iterate(matcher: Matcher, read: Read = readLine, write: Write = print): Unit = {
    write(s"search using ${matcher.info}> ")
    val searchString = read()
    if (searchString == ".exit") {
      write("Done.\n")
      return
    }

    val results = for {
      result <- matcher(searchString) take 10 zipWithIndex
    } yield result

    results foreach {
      case ((fileName, score), index) =>
        write(s"${index + 1}. $fileName score: $score\n")
    }

    iterate(matcher, read, write)
  }
}
