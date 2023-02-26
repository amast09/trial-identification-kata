# trial-identification-kata
Identify patients who are eligible for drug trials based on provided data

## How to run using Docker

* Install [Docker](https://docs.docker.com/get-docker/)
* Run the application

```
# may take a minute to complete on first run
docker run --rm -it $(docker build -q .)
```

* Test the application

```
# may take a minute to complete on first run
docker run --rm -it $(docker build -q .) sbt test
```

## How to run using sbt

* Install [sbt](https://www.scala-sbt.org/1.x/docs/Setup.html)
* Run the application

```
sbt compile
sbt --error run
```

* Test the application

```
sbt test
```
