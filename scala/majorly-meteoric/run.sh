#!/bin/bash

echo "Compiling and assembling application..."
sbt compile assembly

JARFILE=`pwd`/target/scala-2.11/majorly-meteoric-assembly-0.1.jar

# Run it locally
spark-submit --class Main --master local $JARFILE target/quarentine/