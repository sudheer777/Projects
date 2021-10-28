#!/bin/bash

echo "Compiling and assembling application..."
sbt assembly

# Directory where spark-submit is defined
# Install spark from https://spark.apache.org/downloads.html
SPARK_HOME=/Users/${USER}/spark-2.4.2-bin-hadoop2.7

# JAR containing a simple hello world
JARFILE=`pwd`/target/scala-2.11/Chess_analysis-assembly-1.0.jar

# Run it locally
spark-submit --class ChessAnalysisSparkFinal --master local $JARFILE
#spark-submit --class ChessAnalysisSpark2 --master local $JARFILE
#spark-submit --class CoronaAnalysis --master local $JARFILE file:///Users/sudheer/Downloads/markmarkoh-coronavirus-data/full_data.csv
