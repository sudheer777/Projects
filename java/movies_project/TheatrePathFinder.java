import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TheatrePathFinder {
    public static void main(String[] args) throws IOException {
        if (args.length != 5) {
            System.out.println("usage: TheatrePathFInder <graph_input_filename> <locations_input_filename> <user location node name> <theatre location node name> <reach duration in seconds>");
            return;
        }
        VertexPath res = findMinimumCostPathForTheatre(args[0], args[1], args[2], args[3], Long.parseLong(args[4]));
        System.out.println("Minimum path and cost within given duration: " + res);
    }

    public static VertexPath findMinimumCostPathForTheatre(String graphInputFilename, String nodesInputFilename, String currentNode,
                                                           String theatreNode, Long reachDuration) throws IOException {
        Map<String, Node> nodeMap = TheatreUtils.constructNodes(nodesInputFilename);
        Map<Node, List<Vertex>> graph = TheatreUtils.constructGraph(graphInputFilename, nodeMap);
        // System.out.println(graph);
        List<Node> visited = new ArrayList<Node>();
        visited.add(nodeMap.get(currentNode));
        Map<NodeDuration, VertexPath> cache = new HashMap<>();
        return find(nodeMap, graph, currentNode, theatreNode, reachDuration, visited, cache);
    }

    public static class NodeDuration {
        private String node;

        private Long duration;

        public NodeDuration(
                String node,
                Long duration
        ) {
            this.node = node;
            this.duration = duration;
        }

        @Override
        public boolean equals(Object obj) {
            NodeDuration o = (NodeDuration) obj;
            return o.node.equals(node) && o.duration.equals(duration);
        }
    }

    public static class VertexPath {
        private String path;

        private Long duration;

        public VertexPath(
                String path,
                Long duration
        ) {
            this.path = path;
            this.duration = duration;
        }

        @Override
        public String toString() {
            return "{ 'path': " + path + ", 'duration': " + duration + "}";
        }
    }

    private static VertexPath find(Map<String, Node> nodeMap, Map<Node, List<Vertex>> graph, String currentNode,
                                     String theatreNode, Long reachDuration, List<Node> visited,
                                     Map<NodeDuration, VertexPath> cache) {
        if (currentNode.equals(theatreNode)) return new VertexPath(theatreNode, 0L);
        NodeDuration nd = new NodeDuration(currentNode, reachDuration);
        if (cache.containsKey(nd)) return cache.get(nd);
        Long minDurarion = Long.MAX_VALUE;
        String path = null;
        // System.out.println("reachDuration: " + reachDuration);
        List<Vertex> vertices = graph.get(nodeMap.get(currentNode));
        for (Vertex v : vertices) {
            if (!visited.contains(v.node)) {
                if (reachDuration - v.timeInSeconds >= 0) {
                    visited.add(v.node);
                    VertexPath d = find(nodeMap, graph, v.node.name, theatreNode, reachDuration - v.timeInSeconds, visited, cache);
                    visited.remove(v.node);
                    if (d.duration != Long.MAX_VALUE) {
                        Long f = v.costInDollars + d.duration;
                        // System.out.println("current node:" + currentNode + "|vertex: " + v + "|cost: " + f);
                        if (f < minDurarion) {
                            minDurarion = f;
                            path = currentNode + " -> " + d.path;
                        }
                    }
                }
            }
        }
        VertexPath res = new VertexPath(path, minDurarion);
        cache.put(nd, res);
        return res;
    }
}
