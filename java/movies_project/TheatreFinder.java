import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Arrays;

class TheatreFinder {
    public static void main(String[] args) throws IOException {
        if (args.length != 3) {
            System.out.println("usage: TheatreFinder <graph_input_filename> <locations_input_filename> <user location node name>");
            return;
        }
        findNearestTheatre(args[0], args[1], args[2]);
    }

    public static Node findNearestTheatre(String graphInputFilename, String nodesInputFilename, String currentNode) throws IOException {
        Map<String, Node> nodeMap = TheatreUtils.constructNodes(nodesInputFilename);
        Map<Node, List<Vertex>> graph = TheatreUtils.constructGraph(graphInputFilename, nodeMap);
        // System.out.println(graph);

        String[] nodes = nodeMap.keySet().toArray(new String[nodeMap.size()]);
        Boolean[] visited = new Boolean[nodes.length];
        Arrays.fill(visited, Boolean.FALSE);
        Long[] distance = new Long[nodes.length];
        Arrays.fill(distance, Long.MAX_VALUE);
        int curInd = getIndexOf(nodes, currentNode);
        visited[curInd] = true;
        distance[curInd] = 0L;
        String prevNode = currentNode;
        for (int i = 0; i < nodes.length; i++) {
            Node n = nodeMap.get(prevNode);
            int ind = getIndexOf(nodes, prevNode);
            List<Vertex> vertices = graph.get(n);
            if (vertices != null) {
                for (int j = 0; j < vertices.size(); j++) {
                    String ln = vertices.get(j).node.name;
                    int lind = getIndexOf(nodes, ln);
                    if (!visited[lind]) {
                        Long newDis = distance[ind] + vertices.get(j).timeInSeconds;
                        if (newDis < distance[lind]) {
                            distance[lind] = newDis;
                        }
                    }

                }
            }

            Long curMin = Long.MAX_VALUE;
            int cuMinInd = 0;

            for (int j = 0; j < distance.length; j++) {
                if (!visited[j]) {
                    if (curMin > distance[j]) {
                        curMin = distance[j];
                        cuMinInd = j;
                    }
                }
            }
            prevNode = nodes[cuMinInd];
            visited[cuMinInd] = true;
        }

        Node nearestTheatre = null;
        Long shortestTravel = Long.MAX_VALUE;

        for (int i = 0; i < distance.length; i++) {
            if (i != curInd) {
                Node n = nodeMap.get(nodes[i]);
                if (n.isTheatre) {
                    if (shortestTravel > distance[i]) {
                        shortestTravel = distance[i];
                        nearestTheatre = n;
                    }
                }
            }
        }

        // System.out.println("Distance array: " + Arrays.toString(distance));
        System.out.println("Nearest theatre is " + nearestTheatre + ". Travel time is: " + shortestTravel);

        return nearestTheatre;
    }

    private static int getIndexOf(String[] strings, String item) {
        for (int i = 0; i < strings.length; i++) {
            if (item.equals(strings[i])) return i;
        }
        return -1;
    }

}