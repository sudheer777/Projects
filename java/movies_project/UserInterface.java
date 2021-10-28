import java.io.IOException;
import java.util.*;

public class UserInterface {
    public static void main(String[] args) throws IOException {
        while (true) {
            Scanner sc= new Scanner(System.in);
            System.out.print("Enter number to select any of the functionality\n1) Theatre finder\n2) Theatre path finder\n3) Movie lister\n4) exit\n");
            int ty = sc.nextInt();

            if (ty == 1) {
                System.out.print("Enter full path for locations file\n");
                String locationFile = sc.next();
                System.out.print("Enter full path for graph file\n");
                String graphFile = sc.next();
                System.out.print("User current location name\n");
                String userLocation = sc.next();
                String[] ars = new String[3];
                ars[0] = graphFile;
                ars[1] = locationFile;
                ars[2] = userLocation;
                TheatreFinder.main(ars);
            } else if (ty == 2) {
                System.out.print("Enter full path for locations file\n");
                String locationFile = sc.next();
                System.out.print("Enter full path for graph file\n");
                String graphFile = sc.next();
                System.out.print("User current location name\n");
                String userLocation = sc.next();
                System.out.print("theatre location name which needs to be visited\n");
                String theatreLocation = sc.next();
                System.out.print("Duration time in seconds in which user wants to reach theatre\n");
                String duration = sc.next();
                String[] ars = new String[5];
                ars[0] = graphFile;
                ars[1] = locationFile;
                ars[2] = userLocation;
                ars[3] = theatreLocation;
                ars[4] = duration;
                TheatrePathFinder.main(ars);
            } else if (ty == 3) {
                System.out.print("Enter full path for show_timing file\n");
                String showsFile = sc.next();
                System.out.print("Start time format: yyyy-MM-dd HH:mm:ss\n");
                String startTime = sc.next() + " " + sc.next();
                System.out.print("End time format: yyyy-MM-dd HH:mm:ss\n");
                String endTime = sc.next() + " " + sc.next();
                String[] ars = new String[3];
                ars[0] = showsFile;
                ars[1] = startTime;
                ars[2] = endTime;
                MovieLister.main(ars);
            } else if (ty == 4) {
                System.out.print("Exiting..");
                break;
            } else {
                System.out.print("Please enter valid input\n");
            }

        }
    }
}
