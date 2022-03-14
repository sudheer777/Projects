package vishvarajlakhan

object Parents {
  val royalParent = Map("George" -> ("m", "William", "Catherine"),
    "Charlotte" -> ("f", "William", "Catherine"), "Louis" -> ("m", "William",
      "Catherine"), "Archie" -> ("m", "Harry", "Meghan"), "Lilibet" -> ("f",
      "Harry", "Meghan"), "Savannah" -> ("f", "Autumn", "Peter"), "Isla" -> ("f",
      "Autumn", "Peter"), "Mia" -> ("f", "Zara", "Mike"), "Lena" -> ("f", "Zara",
      "Mike"), "Lucas" -> ("m", "Zara", "Mike"), "Sienna" -> ("f", "Beatrice",
      "Edoardo"), "August" -> ("m", "Eugenie", "Jack"), "Beatrice" -> ("f",
      "Andrew", "Sarah"), "Eugenie" -> ("f", "Andrew", "Sarah"), "Louise" ->
      ("f", "Edward", "Sophie"), "James" -> ("m", "Edward", "Sophie"), "Peter" -> ("m", "Mark", "Anne"), "Zara" -> ("f", "Mark", "Anne"), "William" ->
      ("m", "Diana", "Charles"), "Harry" -> ("m", "Diana", "Charles"), "Charles" -> ("m", "Elizabeth", "Philip"), "Anne" -> ("f", "Elizabeth", "Philip"),
    "Andrew" -> ("m", "Elizabeth", "Philip"), "Edward" -> ("m", "Elizabeth",
      "Philip"), "Elizabeth" -> ("f", "", ""), "Philip" -> ("m", "", ""), "Diana" -> ("f",
      "", ""), "Mark" -> ("m", "", ""), "Sophie" -> ("f", "", ""), "Sarah" -> ("f", "",
      ""), "Mike" -> ("m", "", ""), "Autumn" -> ("f", "", ""), "Meghan" -> ("f", "",
      ""), "Catherine" -> ("f", "", ""), "Timothy" -> ("m", "", ""), "Jack" -> ("m",
      "", ""), "Camilla" -> ("f", "", ""), "Edoardo" -> ("m", "", ""))


  def parents(p: String): Option[(String, String)] = {
    val par = royalParent.get(p).map(x => (x._2, x._3))
    if (par.isEmpty) {
      return None
    }
    if (par.get._1 == "") {
      return None
    }
    par
  }

  def grandparents(p: String): Option[List[String]] = {
    val par = parents(p)
    if (par.isEmpty) {
      return None
    }
    val (f, m) = par.get
    val gpf = parents(f).map(x => List(x._1, x._2)).getOrElse(List[String]())
    val gpm = parents(m).map(x => List(x._1, x._2)).getOrElse(List[String]())
    val gp = gpf ++ gpm
    if (gp.isEmpty) None else Some(gpf ++ gpm)
  }

  private def sibblings(p: String): Option[List[(String, String)]] = {
    val par = parents(p)
    if (par.isEmpty) {
      return None
    }
    val (f, m) = par.get
    val sibs = royalParent
      .filter(x => x._2._2 == f && x._2._3 == m)
      .filter(x => x._1 != p)
      .map(x => (x._1, x._2._1))
    Some(if (sibs.isEmpty) Nil else sibs.toList)
  }

  private def sisters(p: String): Option[List[String]] = {
    val sibs =  sibblings(p)
    if (sibs.isEmpty) return None
    Some(sibs.get.filter(x => x._2 == "f").map(_._1))
  }

  def brothers(p: String): Option[List[String]] = {
    val sibs =  sibblings(p)
    if (sibs.isEmpty) return None
    Some(sibs.get.filter(x => x._2 == "m").map(_._1))
  }

  def firstCousins(p: String): Option[List[String]] = {
    val par = parents(p)
    if (par.isEmpty) {
      return None
    }
    val (f, m) = par.get
    val fSibs = sibblings(f).getOrElse(List[(String, String)]()).map(_._1)
    val mSibs = sibblings(m).getOrElse(List[(String, String)]()).map(_._1)
    val sibs = fSibs ++ mSibs
    Some(royalParent
      .filter(x => sibs.contains(x._2._2) || sibs.contains(x._2._3))
      .map(_._1).toList)
  }

  def aunts(p: String): Option[List[String]] = {
    val par = parents(p)
    if (par.isEmpty) {
      return None
    }
    val (f, m) = par.get
    val autsF = sisters(f).getOrElse(List[String]())
    val auntsM = sisters(m).getOrElse(List[String]())
    Some(autsF ++ auntsM)
  }

  def main(args: Array[String]): Unit = {
    println(parents("George"))
    println(grandparents("George"))
    println(brothers("George"))
    println(firstCousins("George"))
    println(aunts("William"))
  }
}