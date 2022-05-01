package edu.colorado.csci3155.project2

object Interpreter {

    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
        val v1 = evalExpr(expr, env)
        val v2 = evalExpr(expr1, env)
        fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case ConstBool(b) => BoolValue(b)
        case Ident(s) => env.lookup(s)

        case Line(l) => evalExpr(l, env) match {
            case NumValue(len) => { // TODO: Create a polygon with two vertices (0,0) and (len, 0)
                  // Make sure to return a FigValue(canvas).
                val p = Polygon(List((0, 0), (len, 0)))
                FigValue(new MyCanvas(List(p)))
            }
            case _ => throw new IllegalArgumentException("Cannot create line with non numerical length")
        }
        case EquiTriangle(sideLength) => evalExpr(sideLength, env) match {
            case NumValue(len) => {
                val p = Polygon(List((0, 0), (len, 0), (len/2, 2 * len * math.sqrt(3) / 4 )))
                FigValue(new MyCanvas(List(p)))
            }
            case _ => throw new IllegalArgumentException("Cannot create eqi traingle with non numerical length")
        }
        case Rectangle(sideLength) => evalExpr(sideLength, env) match {
            case NumValue(len) => {
                val p = Polygon(List((0, 0), (len, 0), (len, len), (0, len)))
                FigValue(new MyCanvas(List(p)))
            }
            case _ => throw new IllegalArgumentException("Cannot create eqi traingle with non numerical length")
        }
        case Circle(rad) => evalExpr(rad, env) match {
            case NumValue(r) => {
                // Make sure to return a FigValue(canvas).
                val p = MyCircle((0, 0), r)
                FigValue(new MyCanvas(List(p)))
            }
            case _ => throw new IllegalArgumentException("Cannot create line with non numerical length")
        }
        case Plus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.plus) // TODO: Handle addition of numbers or figures
        case Minus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.minus)
        case Mult(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.mult) // TODO: Handle multiplication of numbers or figures
        case Div(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.div) // TODO: Handle division

        case Geq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.geq)
        case Gt(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.gt)
        case Eq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.equal)
        case Neq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.notEqual)
        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
            }
        }

        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
                    }
                }
                case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
            }
        }

        case Not(e) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
            }
        }

        case IfThenElse(e, e1, e2) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(true) => evalExpr(e1, env)
                case BoolValue(false) => evalExpr(e2,env)
                case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
            }
        }


        case Let(x, e1, e2) => {
            val v1 = evalExpr(e1, env)
            val env2 = Extend(x, v1, env)
            evalExpr(e2, env2)
        }

        case FunDef(x, e) => Closure(x, e, env)
        case LetRec(f, x, e1, e2) => {
            val env1 = ExtendREC(f, x, e1, env)
            evalExpr(e2, env1)
        }
        case FunCall(fCallExpr, arg) => {
            val v1 = evalExpr(fCallExpr, env)
            v1 match {
                case Closure(p, eBody, envClosure) => {
                    val v2 = evalExpr(arg, env)
                    val env2 = Extend(p, v2, envClosure)
                    evalExpr(eBody, env2)
                }
                case _ => throw new IllegalArgumentException("Runtime function call type mismatch")
            }
        }
    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
