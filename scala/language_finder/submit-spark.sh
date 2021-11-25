#!/bin/bash

echo "Compiling and assembling application..."
sbt -mem 2048 clean package


# JAR containing a simple hello world
JARFILE=`pwd`/target/scala-2.11/languagefinder_2.11-0.1.0.jar

# Run it locally
spark-submit --class LanguageFrequency --master local $JARFILE "books/*"
