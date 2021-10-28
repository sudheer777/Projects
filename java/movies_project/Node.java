class Node {
    public String name;

    public Boolean isTheatre;

    public Node(String name, Boolean isTheatre) {
        this.name = name;
        this.isTheatre = isTheatre;
    }

    @Override
    public boolean equals(Object obj) {
        return ((Node) obj).name.equals(name);
    }

    @Override
    public String toString() {
        return "{ 'name': " + name + ", 'is_theatre': " + isTheatre + "}";
    }
}