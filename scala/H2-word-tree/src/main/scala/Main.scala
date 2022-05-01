abstract class WTree extends WTreeInterface {
  override def filter(pred: Token => Boolean): WTree = ???
  def filterAux(pred: Token => Boolean, acc: WTree): WTree

}

case object Empty extends WTree {
  override def isEmpty = true
  override def ins(w: Token): WTree = Node(w, Empty, Empty)
  override def filterAux(pred: Token => Boolean, acc: WTree): WTree = ???
  override def size = 0
  override def contains(s: String): Boolean = false
}

case class Node(word: Token, left: WTree, right: WTree) extends WTree {
  override def isEmpty = false

  override def ins(w: Token): WTree =
    if (w.freq > word.freq) Node(word, left, right.ins(w))
    else Node(word, left.ins(w), right)

  override def contains(s: String): Boolean = {
    word.word == s || left.contains(s) || right.contains(s)
  }

  override def size: Int = {
    1 + left.size + right.size
  }

  def filterAux(pred: Token => Boolean, acc: WTree): WTree = ???
}


object Main {

  def profileID:Int = 1

  val scalaDescription: String = "Scala is a strong statically typed general-purpose programming language which supports both object-oriented programming and functional programming designed to be concise many of Scala s design decisions are aimed to address criticisms of Java Scala source code can be compiled to Java bytecode and run on a Java virtual machine. Scala provides language interoperability with Java so that libraries written in either language may be referenced directly in Scala or Java code like Java, Scala is object-oriented, and uses a syntax termed curly-brace which is similar to the language C since Scala 3 there is also an option to use the off-side rule to structure blocks and its use is advised martin odersky has said that this turned out to be the most productive change introduced in Scala 3 unlike Java, Scala has many features of functional programming languages like Scheme, Standard ML, and Haskell, including currying, immutability, lazy evaluation, and pattern matching it also has an advanced type system supporting algebraic data types, covariance and contravariance, higher-order types (but not higher-rank types), and anonymous types other features of Scala not present in Java include operator overloading optional parameters named parameters and raw strings conversely a feature of Java not in Scala is checked exceptions which has proved controversial"

  /* Split the text into chunks */
  def split(text: List[Char]): List[List[Char]] = {
    def aux(text: List[Char]): List[List[Char]] = {
      text match {
        case Nil => Nil
        case ' ' :: t =>
          val p = aux(t)
          if (p == List(Nil))
            List(Nil)
          else
            Nil :: aux(t)
        case h :: Nil => List(List(h))
        case h :: t =>
          val p = aux(t)
          p match {
            case h1 :: t1 => (h +: h1)  +: t1
          }
      }
    }

    // if we have only void chunks, we return the empty list
    val l = aux(text)
    if (l == List(Nil)) Nil
    else l
  }

  /* compute the frequency of each chunk */
  def computeTokens(words: List[String]): List[Token] = {
    def dropToken(acc: List[Token], word: String): List[Token] = {
      acc match {
        case Nil => Nil
        case h :: t if h.word == word => dropToken(t, word)
        case h :: t => h :: dropToken(t, word)
      }
    }

    def findToken(acc: List[Token], word: String): Option[Token] = {
      acc match {
        case Nil => None
        case h :: t if h.word == word => Some(h)
        case h :: t => findToken(t, word)
      }
    }

    /* insert a new string in a list of tokens */
    def insWord(s: String, acc: List[Token]): List[Token] = {
      val f = findToken(acc, s)
      if (f.isEmpty)
        acc :+ Token(s, 1)
      else {
        dropToken(acc, s) :+ Token(s, f.get.freq + 1)
      }
    }

    /* tail-recursive implementation of the list of tokens */
    def aux(rest: List[String], acc: List[Token]): List[Token] = {
      rest.reverse match {
        case Nil => acc
        case h :: t => aux(t, insWord(h, acc))
      }
    }

    aux(words, Nil)
  }

  def tokensToTree(tokens: List[Token]): WTree = {
    val empty: WTree = Empty
    tokens.foldLeft(empty)((x, y) => x.ins(y))
  }

  /* Using the previous function, which builds a tree from a list of tokens,
  *  write a function which takes a string,
  *  splits it into chunks, computes frequencies and constructs a tree.
  *  Use the function _.toList to construct a list of characters from a String.
  *
  *  A much cleaner implementation can be achieved by "sequencing" functions using
  *  andThen.
  * */

  def makeTree(s:String): WTree = {
    def mkStr(s: List[Char]): String = {
      s match {
        case Nil => ""
        case h :: t => h + mkStr(t)
      }
    }

    def splitAux(c: List[List[Char]]): List[String] = {
      c match {
        case Nil => Nil
        case h :: t => mkStr(h) :: splitAux(t)
      }
    }

    val s1 = split(s.toList)
    val s2 = splitAux(s1)
    val tokens = computeTokens(s2)
    tokensToTree(tokens)
  }

  /* build a tree with the words and frequencies from the text in the scalaDescription text */
  def wordSet: WTree = makeTree(scalaDescription)

  /* find the number of occurrences of the keyword "Scala" in the scalaDescription text */
  def scalaFreq: Int = ???

  /* find how many programming languages are referenced in the text.
     A PL is a keyword which starts with an uppercase
     You can reference a character from a string using (0) and you can
     also use the function isUpper

  */
  def progLang: Int = ???

  /* find how many words which are not prepositions or conjunctions appear in the text (any word whose size is larger than 3). */

  def wordCount : Int = ???


}

