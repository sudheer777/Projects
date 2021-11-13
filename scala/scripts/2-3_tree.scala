//a 2-3-tree
//contains(e: X): Booleanthat returns true if and only if the tree con-tains the argumente.  This method is abstract and must be implementedby subclasses.
//ins(e: X): Tree[X]that  returns  the  tree  node  after  inserting  the  ar-gumente.   This  method  is  abstract  and  must  be  implemented  by  thesubclasses.  This method may return a 4-node.
//insert(e: X): Tree[X]that  returns  the  tree  node  after  inserting  theargumenteby callingins(e)and ifinsreturns a 4-node, it converts the4-node into a 2-node with two 2-nodes as subtrees.
//heightthat returns the height of the tree (which is the max height of thesubtrees plus 1).  This method is abstract and must be implemented bythe subclasses.

trait Tree[X <: Ordered[X]] {
//
  def contains(e: X): Boolean 
  def insert(e: X) = {
    ins(e) match {
      case FourNode(t1, a, t2, b, t3, c, t4) => TwoNode(TwoNode(t1, a, t2), b, TwoNode(t3, c, t4))
      case n => n
    }
  }
  def ins(e: X): Tree[X] 
  def height: Int
}

case class Leaf[X <: Ordered[X]]() extends Tree[X] {
  // TODO
  ////a leaf node with no data or subtree.
  //definetoStringmethod in each case class to convert thetree to a string.
  def contains(e: X): Boolean = false

  def ins(e: X): Tree[X] = TwoNode[X](Leaf(), e, Leaf())

  def height: Int = 0

  override def toString: String = "L"
}

case class TwoNode[X <: Ordered[X]](var left: Tree[X], x:X, var right: Tree[X]) extends Tree[X] {
  // TODO
//definetoStringmethod in each case class to convert thetree to a string.
////a 2-node with a left subtree, a data valuex, and a right subtree, where allvalues in left subtree are less thanx, which is less than all values in rightsubtree.

  def contains(e: X): Boolean = {
    if (e == x) return true
    if (e > x) {
      right.contains(e)
    } else {
      left.contains(e)
    }
  }

  def ins(e: X): Tree[X] = {
    if (left == Leaf() && right == Leaf()) {
      val min = if (e > x) x else e
      val max = if (e > x) e else x
      return ThreeNode(Leaf(), min, Leaf(), max, Leaf())
    }
    if (e < x) {
      left = left.ins(e)
    } else right = right.ins(e)
    left match {
      case FourNode(t1, a, t2, b, t3, c, t4) =>
        return ThreeNode(TwoNode(t1, a, t2), b, TwoNode(t3, c, t4), x, right)
      case _ =>
    }
    right match {
      case FourNode(t1, a, t2, b, t3, c, t4) =>
        return ThreeNode(left, x, TwoNode(t1, a, t2), b, TwoNode(t3, c, t4))
      case _ =>
    }
    this
  }

  def height: Int = {
    1 + (left.height max right.height)
  }

  override def toString: String = s"{${left.toString}, $x, ${right.toString})"
}

case class ThreeNode[X <: Ordered[X]](var left: Tree[X], x1:X, var middle:Tree[X], x2:X, var right: Tree[X]) extends Tree[X] {
  // TODO
//definetoStringmethod in each case class to convert thetree to a string.
////a  3-node  with  a  left  subtree,  a  data  valuex1,  a  middle  subtree,  a  datavaluex2, a right subtree.  The data values in the subtrees are ordered thesame way as those of 2-node.

  def contains(e: X): Boolean = {
    if (e == x1 || e == x2) return true
    if (e < x1) {
      left.contains(e)
    } else if (e < x2) {
      middle.contains(e)
    } else {
      right.contains(e)
    }
  }

  def ins(e: X): Tree[X] = {
    if (left == Leaf() && middle == Leaf() && right == Leaf()) {
      val min = if (e < x1) e else x1
      val middle = if (e > x1 && e < x2) e else if (e < x1) x1 else x2
      val max = if (e > x2) e else x2
      return FourNode(Leaf(), min, Leaf(), middle, Leaf(), max, Leaf())
    } else {
      if (e < x1) {
        left = left.ins(e)
        left match {
          case FourNode(t1, a, t2, b, t3, c, t4) =>
            return FourNode(TwoNode(t1, a, t2), b, TwoNode(t3, c, t4), x1, middle, x2, right)
          case _ =>
        }
      } else if (e < x2) {
        middle = middle.ins(e)
        middle match {
          case FourNode(t1, a, t2, b, t3, c, t4) =>
            return FourNode(left, x1, TwoNode(t1, a, t2), b, TwoNode(t3, c, t4), x2, right)
          case _ =>
        }
      } else {
        right = right.ins(e)
        right match {
          case FourNode(t1, a, t2, b, t3, c, t4) =>
            return FourNode(left, x1, middle, x2, TwoNode(t1, a, t2), b, TwoNode(t3, c, t4))
          case _ =>
        }
      }
    }
    this
  }

  def height: Int = {
    1 + Array(left.height, middle.height, right.height).max
  }

  override def toString: String = s"{${left.toString}, $x1, ${middle.toString}, $x2, ${right.toString})"
}

case class FourNode[X<:Ordered[X]](t1:Tree[X], a:X, t2:Tree[X], b:X, t3:Tree[X], c:X, t4:Tree[X]) extends Tree[X] {
  def contains(e: X): Boolean = throw new Exception()
  def ins(e: X): Tree[X] = throw new Exception()
  def height: Int = throw new Exception()
}
 
case class Num(i: Int) extends Ordered[Num] {
  def compare(that: Num) = i - that.i
  override def toString = i.toString
}
case class Alpha(c: Char) extends Ordered[Alpha] {
  def compare(that: Alpha) = c - that.c
  override def toString = c.toString
}

object TwoThreeTree {
//Do not change the driver
    def main(arg: Array[String]) {
	  val input = List(3, 4, 2, 10, 9, 1, 5, 6, 11, 12, 13, 14, 15).map(i=>Num(i))
	  
	  val t = input.foldLeft[Tree[Num]](Leaf())((l, i) => { val x = l.insert(i); println(x.height + " " + x); x }) 
	  
	  println(t.contains(Num(5)))
	  println(t.contains(Num(14)))
	  println(t.contains(Num(7))) 
	  println(t.contains(Num(17))) 
	  
	  val input2 = List('a', 'c', 'd', 'g', 'e', 'z', 'r', 'k', 'l', 'p', 'y').map(i => Alpha(i))
	  
	  val t2 = input2.foldLeft[Tree[Alpha]](Leaf())((l, i) => { val x = l.insert(i); println(x.height + " " + x); x }) 
	  println(t2.contains(Alpha('d')))
	  println(t2.contains(Alpha('z')))
	  println(t2.contains(Alpha('m'))) 
	  println(t2.contains(Alpha('s'))) 
  }
}
