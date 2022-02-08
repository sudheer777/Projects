#!/bin/bash

echo "Compiling and assembling application..."
sbt -mem 2048 clean assembly


# JAR containing a simple hello world
JARFILE=`pwd`/target/scala-2.11/InstacartAnalytics-assembly-0.1.0.jar

# Run it locally
spark-submit --class TimeFormatConversion --master local $JARFILE
