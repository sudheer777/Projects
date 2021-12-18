#!/bin/bash

echo "Compiling and assembling application..."
sbt -mem 2048 clean assembly


JARFILE=`pwd`/target/scala-2.11/Customer-assembly-0.1.0.jar

# Run it locally
spark-submit --class Customer --master local $JARFILE "Attachment_1639279522.csv"
