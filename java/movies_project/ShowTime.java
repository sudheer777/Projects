import java.text.SimpleDateFormat;
import java.util.Date;

class ShowTime {
    public String movieName;

    private String startTime;

    public Long startTimestamp;

    private String endTime;

    public Long endTimestamp;

    private Long durationInSeconds;

    private static SimpleDateFormat dateFormat = new SimpleDateFormat ("yyyy-MM-dd HH:mm:ss");

    public ShowTime(String movieName, String startTime, Long durationInSeconds) {
        this.movieName = movieName;
        this.startTime = startTime;
        this.startTimestamp = getTimestampInSecs(startTime);
        this.durationInSeconds = durationInSeconds;
        this.endTimestamp = this.startTimestamp + durationInSeconds;
        this.endTime = getDateFormat(this.endTimestamp);
    }

    public String getDateFormat(Long time) {
        return dateFormat.format(new Date(time * 1000));
    }

    public static Long getTimestampInSecs(String time) {
        try {
            return dateFormat.parse(time).getTime() / 1000;
        } catch(Exception e) {
            return null;
        }
    }

    @Override
    public String toString() {
        return "{ 'movieName': " + movieName +
                ", 'startTime': " + startTime +
                ", 'duration': " + durationInSeconds +
                ", 'endTime': " + endTime + " }";
    }
}
