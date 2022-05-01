package context
import expression._
import value._
object alu:

  def execute(opcode: Identifier, args: List[Value]): Value =
    opcode.name match
      case "add" => add(args)            // n-ary
      case "mul" => mul(args)            // n-ary
      case "sub" => sub(args)            // n-ary
      case "div" => div(args)            // n-ary
      case "less" => less(args)          // binary
      case "equals" => same(args)        // binary
      //case "more" => more(args)          // binary
      //case "unequals" => unequals(args)  // binary
      //case "not" => not(args)            // unary
      case "write" => write(args)
  // TBC


  private def add(args: List[Value]): Value =
    def helper(result: Addable, unseen: List[Value]): Addable =
      if(unseen.isEmpty) result
      else unseen.head match
        case h: Addable => helper(result + h, unseen.tail)
        case _ => throw TypeException("Inputs to + must be addable")

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match
      case n: Addable => helper(n, args.tail )
      case _ => throw new TypeException("Inputs to + must be addable")

  private def sub(args: List[Value]): Value =
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if(unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result - h, unseen.tail)
        case _ => throw TypeException("Inputs to - must be numeric")

    if(args.size < 2) throw new TypeException("2 or more inputs required by -")
    args(0) match
      case n: value.Numeric => helper(n, args.tail )
      case _ => throw new TypeException("Inputs to - must be numeric")

  private def div(args: List[Value]): Value =
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if(unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result / h, unseen.tail)
        case _ => throw TypeException("Inputs to / must be numeric")

    if(args.size < 2) throw new TypeException("2 or more inputs required by /")
    args(0) match
      case n: value.Numeric => helper(n, args.tail )
      case _ => throw new TypeException("Inputs to / must be numeric")

  private def mul(args: List[Value]): Value =
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if(unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result * h, unseen.tail)
        case _ => throw TypeException("Inputs to * must be numeric")

    if(args.size < 2) throw new TypeException("2 or more inputs required by *")
    args(0) match
      case n: value.Numeric => helper(n, args.tail )
      case _ => throw new TypeException("Inputs to * must be numeric")

  private def less(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("2 inputs required by <")
    args(0) match
      case x: Ordered[Value] => Boole(x < args(1))
      case _ => throw TypeException("Inputs to < must be orderable")

  private def same(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("2 inputs required by =")
    args(0) match
      case x: Ordered[Value] => Boole(x == args(1))
      case _ => throw TypeException("Inputs to == must be orderable")

  private def write(args: List[Value]): Value =
    println(args(0))
    Notification.DONE





// etc.