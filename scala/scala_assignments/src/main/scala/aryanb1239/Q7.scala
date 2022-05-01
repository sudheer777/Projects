package aryanb1239


case class Item(title: String, description: String) {
  def viewDescription(): Unit = println(s"title: $title, description: $description")
}


case class Book(author: String, isbn: String) {
  def viewDescription(): Unit = println(s"author: $author, isbn: $isbn")
}

case class DVD(director: String, certificate: String) {
  def viewDescription(): Unit = println(s"director: $director, certificate: $certificate")
}

case class CD(artist: String, genere: String, numberOfTracks: Int) {
  def viewDescription(): Unit = println(s"artist: $artist, genere: $genere, numberOfTracks: $numberOfTracks")
}

object Q7 {
  def main(args: Array[String]): Unit = {

  }
}
