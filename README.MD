## Requirements
* Scala 2.13
* Java 8
* SBT 1.2.8

## Build/Assembly (fat-jar)

To build the application run `sbt assembly` 
Then jar will be accessible under `./target/scala-2.13/search.jar`
## Run

To run the application you have to build it first or run via sbt `sbt run`

First argument - directory is required
Last argument (not necessarily second) - override default Classifier

Rich example:
`java -jar ./target/scala-2.13/search.jar ./dataset simple-ci` 

To exit from the app type `.exit` in prompt.

## Run tests

To run tests type `sbt tests`
