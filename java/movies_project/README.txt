Prerequisites:
java version "1.8.0_112" should be installed

How to use this software:

Make sure you are in inside the directory where all the files are available.
To generate all classes: `javac *.java` (This generates all required classes for running)
To run nearest theatre finder application: `java TheatreFinder graph.csv locations.csv a1`
To run movie lister application: `java MovieLister shows.csv "2020-01-01 00:00:00"  "2020-01-01 02:40:00"`
To run theatre path finder with minimum cost: `java TheatrePathFinder graph.csv locations.csv a1 a4 135`

Using user interface
To generate all classes: `javac *.java` (This generates all required classes for running)
To start user interface: `UserInterface`
You can choose any of the functionality by choosing one of them
Based on each functionality it will ask to enter required parameters

Required parameters for Theatre finder:
locations file which has all locations information (sample is available at locations.csv)
graph file which has information between locations (sample is available at graph.csv. More samples are available at tests directory)
User location name

Required parameters for Theatre path finder:
locations file which has all locations information (sample is available at locations.csv)
graph file which has information between locations (sample is available at graph.csv. More samples are available at tests directory)
User location name
Theatre location name
Duration in seconds in which you want to reach theatre

Required parameters for Movie lister:
shows list file (sample is available at shows.csv)
start time you are available from (sample: "2020-01-01 00:00:00")
end time you are available until (sample: "2020-01-01 02:40:00")


Performance tests:

To generate all classes: `javac *.java` (This generates all required classes for running)
To run performance tests for all 3 functionalities: `java PerformanceTest`