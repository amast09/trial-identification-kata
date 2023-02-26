FROM sbtscala/scala-sbt:eclipse-temurin-19_36_1.8.0_3.2.1

COPY . .

RUN ls -al

RUN sbt compile

CMD ["sbt", "--error", "run"]