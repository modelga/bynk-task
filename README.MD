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


### Accessible algoritgms

- simple      - Classifier that matches if query is a part of the file name
- simple-ci   - Same as above but case insensitive
- split       - Splits the query and filenames by " " then compare it as simple-ci
- levenshtein - levenshtein distance calculation 
