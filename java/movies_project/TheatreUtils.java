import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TheatreUtils {
    public static Map<String, Node> constructNodes(String nodesInputFilename) throws IOException {
        Map<String, Node> nodeMap = new HashMap<String, Node>();

        BufferedReader reader = new BufferedReader(new FileReader(nodesInputFilename));
        reader.readLine(); // header line
        String line;
        while((line = reader.readLine()) != null) {
            String[] l = line.split(",");
            Node n = new Node(l[0], Boolean.parseBoolean(l[1]));
            nodeMap.put(l[0], n);
        }
        reader.close();
        return nodeMap;
    }

    public static Map<Node, List<Vertex>> constructGraph(String graphInputFilename, Map<String, Node> nodeMap) throws IOException {
        Map<Node, List<Vertex>> graph = new HashMap<Node, List<Vertex>>();

        BufferedReader reader = new BufferedReader(new FileReader(graphInputFilename));
        reader.readLine(); // header line
        String line;
        while((line = reader.readLine()) != null) {
            String[] l = line.split(",");
            Node n1 = nodeMap.get(l[0]);
            Node n2 = nodeMap.get(l[1]);

            List<Vertex> n1List = graph.getOrDefault(n1, new ArrayList<Vertex>());
            n1List.add(new Vertex(n2, Long.parseLong(l[2]), Long.parseLong(l[3])));
            graph.put(n1, n1List);
            List<Vertex> n2List = graph.getOrDefault(n2, new ArrayList<Vertex>());
            n2List.add(new Vertex(n1, Long.parseLong(l[2]), Long.parseLong(l[3])));
            graph.put(n2, n2List);
        }
        reader.close();
        return graph;
    }
}
