import java.io.IOException;
import java.util.List;

public class PerformanceTest {
    public static void main(String[] args) throws IOException {
        System.out.println("---- Theatre finder test started -------");

        long t11 = theatreFinderTest("tests/test-f1-1.txt", "locations.csv", "a1");
        System.out.println("Test 1 took(5 edges) " + t11 + " ms");

        long t12 = theatreFinderTest("tests/test-f1-2.txt", "locations.csv", "a1");
        System.out.println("Test 2 took(201 edges) " + t12 + " ms");

        long t13 = theatreFinderTest("tests/test-f1-3.txt", "locations.csv", "a1");
        System.out.println("Test 3 took(1001 edges) " + t13 + " ms");

        long t14 = theatreFinderTest("tests/test-f1-4.txt", "locations.csv", "a1");
        System.out.println("Test 4 took(10001 edges) " + t14 + " ms");

        long t15 = theatreFinderTest("tests/test-f1-5.txt", "locations.csv", "a1");
        System.out.println("Test 5 took(15001 edges) " + t15 + " ms");

        long avg1 = (t11 + t12 + t13 + t14 + t15)/5;

        System.out.println("Average time for all test cases: " + avg1 + "ms");
        System.out.println("---- Theatre finder test ended -------");

        System.out.println("---- Theatre path finder test started -------");

        long t21 = theatrePsthFinderTest("tests/test-f2-1.txt", "locations.csv", "a1", "a4", 3000L);
        System.out.println("Test 1 took(5 edges) " + t21 + " ms");

        long t22 = theatrePsthFinderTest("tests/test-f2-2.txt", "locations.csv", "a1", "a4", 3000L);
        System.out.println("Test 2 took(201 edges) " + t22 + " ms");

        long t23 = theatrePsthFinderTest("tests/test-f2-3.txt", "locations.csv", "a1", "a10", 3000L);
        System.out.println("Test 3 took(1001 edges) " + t23 + " ms");

        long t24 = theatrePsthFinderTest("tests/test-f2-4.txt", "locations.csv", "a1", "a20", 1400L);
        System.out.println("Test 4 took(10001 edges) " + t24 + " ms");

        long t25 = theatrePsthFinderTest("tests/test-f2-5.txt", "locations.csv", "a1", "a78", 1000L);
        System.out.println("Test 5 took(15001 edges) " + t25 + " ms");

        long avg2 = (t21 + t22 + t23 + t24 + t25)/5;

        System.out.println("Average time for all test cases: " + avg2 + "ms");
        System.out.println("---- Theatre path finder test ended -------");

        System.out.println("---- Movie Lister test started -------");
        long t31 = movieListerTest("tests/test-f3-1.txt", "2020-01-01 00:00:00", "2020-01-01 02:40:00");
        System.out.println("Test 1 took(9 records) " + t31 + " ms");

        long t32 = movieListerTest("tests/test-f3-2.txt", "2020-01-01 00:00:00", "2020-01-01 23:59:00");
        System.out.println("Test 2 took(1001 records) " + t32 + " ms");

        long t33 = movieListerTest("tests/test-f3-3.txt", "2020-01-01 00:00:00", "2020-01-01 23:59:00");
        System.out.println("Test 3 took(3001 records) " + t33 + " ms");

        long t34 = movieListerTest("tests/test-f3-4.txt", "2020-01-01 00:00:00", "2020-01-01 23:59:00");
        System.out.println("Test 4 took(5001 records) " + t34 + " ms");

        long t35 = movieListerTest("tests/test-f3-5.txt", "2020-01-01 00:00:00", "2020-01-01 23:59:00");
        System.out.println("Test 5 took(10001 records) " + t35 + " ms");

        long avg3 = (t31 + t32 + t33 + t34 + t35)/5;

        System.out.println("Average time for all test cases: " + avg3 + "ms");
        System.out.println("---- Movie Lister test ended -------");

    }

    public static long movieListerTest(String showTimingsFilename, String startTime, String endTime) throws IOException {
        long s = System.currentTimeMillis();
        List<ShowTime> maxPOssibleShows = MovieLister.getMaximumMovies(showTimingsFilename, startTime, endTime);
        long r = System.currentTimeMillis() - s;
        System.out.println("Maximum number of movies user can watch " + maxPOssibleShows.size());
        //System.out.println("Shows list:");
        //maxPOssibleShows.forEach(k -> System.out.println(k));
        return r;
    }

    public static long theatreFinderTest(String graphInputFilename, String nodesInputFilename, String currentNode) throws IOException {
        long s = System.currentTimeMillis();
        TheatreFinder.findNearestTheatre(graphInputFilename, nodesInputFilename, currentNode);
        return System.currentTimeMillis() - s;
    }

    public static long theatrePsthFinderTest(String graphInputFilename, String nodesInputFilename, String currentNode,
                                             String theatreNode, Long reachDuration) throws IOException {
        long s = System.currentTimeMillis();
        TheatrePathFinder.VertexPath res = TheatrePathFinder.findMinimumCostPathForTheatre(graphInputFilename, nodesInputFilename, currentNode, theatreNode, reachDuration);
        long r = System.currentTimeMillis() - s;
        System.out.println("Minimum path and cost within given duration: " + res);
        return r;
    }
}
