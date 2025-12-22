#!/bin/zsh

sbt assembly
java -jar target/scala-3.7.4/buckshot-roulette-assistant.jar
