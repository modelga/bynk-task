package se.bynk.task

object Application extends App {
  Program
    .readFile(args)
    .fold(
      err => println(err),
      file => {
        val classifier = if (args.length > 1) args.lastOption.map(Classifier.getClassifier) else None
        val matcher = Program.prepareData(file, classifier.getOrElse(Classifier.default))
        Program.iterate(matcher)
      }
    )
}
