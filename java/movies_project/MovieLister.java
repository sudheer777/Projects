import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

class MovieLister {
    public static void main(String[] args) throws IOException {
        if (args.length != 3) {
            System.out.println("usage: MovieLister <show_timing_file_name> <start time format: yyyy-MM-dd HH:mm:ss> <end time format: yyyy-MM-dd HH:mm:ss>");
        }
        List<ShowTime> maxPOssibleShows = getMaximumMovies(args[0], args[1], args[2]);
        System.out.println("Maximum number of movies user can watch " + maxPOssibleShows.size());
        System.out.println("Shows list:");
        maxPOssibleShows.forEach(s -> System.out.println(s));
    }

    /**
     * @param showTimingsFilename Filename which has entire information on show startTime and duration of each show
     *                            Header of the CSV file "Movie name,Start time,Duration"
     * @param startTime Timestamp of user when he arrives at theatre (format: yyyy-MM-dd HH:mm:ss)
     * @param endTime Timestamp of user till he can spend at theatre (format: yyyy-MM-dd HH:mm:ss)
     * @return Maximum
     */
    public static List<ShowTime> getMaximumMovies(String showTimingsFilename, String startTime, String endTime) throws IOException {
        List<ShowTime> shows = getShowsList(showTimingsFilename);
        Long startTimestamp = ShowTime.getTimestampInSecs(startTime);
        Long endTimestamp = ShowTime.getTimestampInSecs(endTime);
        List<ShowTime> maxPOssibleShows = new ArrayList<>();
        HashSet<String> seenMovies = new HashSet<>();

        Comparator<ShowTime> customComparator = new Comparator<ShowTime>() {
            @Override
            public int compare(ShowTime s1, ShowTime s2) {
                if (s1.endTimestamp > s2.endTimestamp) return 1;
                return -1;
            }
        };

        // min heap implementation on endTimestamp
        PriorityQueue<ShowTime> queue = new PriorityQueue<ShowTime>(customComparator);

        for (ShowTime s : shows) {
            if (s.startTimestamp >= startTimestamp && s.endTimestamp <= endTimestamp) {
                // System.out.println("Show - " + s);
                while (!queue.isEmpty() && queue.peek().endTimestamp <= s.startTimestamp) {
                    if (!seenMovies.contains(queue.peek().movieName)) {
                        maxPOssibleShows.add(queue.peek());
                        seenMovies.add(queue.peek().movieName);
                        queue.clear();
                    } else {
                        queue.remove();
                    }
                }
                queue.add(s);
                // System.out.println(queue);
            }
        }
        while (!queue.isEmpty() && queue.peek().endTimestamp <= endTimestamp) {
            if (!seenMovies.contains(queue.peek().movieName)) {
                maxPOssibleShows.add(queue.peek());
                queue.clear();
            } else {
                queue.remove();
            }
        }
        return maxPOssibleShows;
    }

    private static List<ShowTime> getShowsList(String showTimingsFile) throws IOException {
        List<ShowTime> shows = new ArrayList<>();

        BufferedReader reader = new BufferedReader(new FileReader(showTimingsFile));
        reader.readLine(); // header line
        String line;
        while((line = reader.readLine()) != null) {
            String[] l = line.split(",");
            shows.add(new ShowTime(l[0], l[1], Long.parseLong(l[2])));
        }
        reader.close();
        return shows;
    }
}
