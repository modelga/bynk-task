package se.bynk.task

object Application extends App {
  Program
    .readFile(args)
    .fold(
      err => println(err),
      file => {
        val classifier = args.lastOption.map(Classifier.getClassifier)
        val matcher = Program.prepareData(file, classifier.getOrElse(Classifier.default))
        Program.iterate(matcher)
      }
    )
}
