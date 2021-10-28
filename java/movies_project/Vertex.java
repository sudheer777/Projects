class Vertex {
    public Node node;

    public Long costInDollars;

    public Long timeInSeconds;

    public Vertex(Node node, Long costInDollars, Long timeInSeconds) {
        this.node = node;
        this.costInDollars = costInDollars;
        this.timeInSeconds = timeInSeconds;
    }

    @Override
    public String toString() {
        return "{ 'node': " + node + ", 'cost_in_dollars': " + costInDollars + ", 'time_in_seconds': " + timeInSeconds + " }";
    }
}