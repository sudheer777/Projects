package edu.colorado.csci3155.project1


class RuntimeError(msg: String) extends Exception {
    override def toString(): String = {
        s"Error: $msg"
    }
}


object Interpreter {


    type Environment = Map[String, Value]



    /*--
        TODO: Complete the evalExpr Function below.
        Please write refactored code and use ValueOps.plus, ValueOps.minus,..
        defined in Value.scala for evaluating the expressions.

        If you encounter error, you should throw a RuntimeError exception defined above.
        Please do not use other exception types.
     */
    def evalExpr(e: Expr, env: Environment) : Value = {
        e match {
            case Const (f) => NumValue(f)
            case Ident (str) => env.getOrElse(str, throw new RuntimeError(s"Environment does not contain mapping for $str"))
            case Plus(e1, e2) =>
                ValueOps.plus(evalExpr(e1, env), evalExpr(e2, env))
            case Minus(e1, e2) =>
                ValueOps.minus(evalExpr(e1, env), evalExpr(e2, env))
            case Mult(e1, e2) =>
                ValueOps.mult(evalExpr(e1, env), evalExpr(e2, env))
            case Div(e1, e2) =>
                val d = evalExpr(e2, env)
                d match {
                    case NumValue(d) if d == 0 => throw new RuntimeError("Division by zero error.")
                    case _ => ValueOps.div(evalExpr(e1, env), d)
                }
            case Gt(e1, e2) =>
                ValueOps.gt(evalExpr(e1, env), evalExpr(e2, env))
            case Eq(e1, e2) =>
                ValueOps.eq(evalExpr(e1, env), evalExpr(e2, env))
            case Geq(e1, e2) =>
                ValueOps.geq(evalExpr(e1, env), evalExpr(e2, env))
            case And(e1, e2) =>
                val ev1 = evalExpr(e1, env)
                ev1 match {
                    case BoolValue(b) =>
                        if (!b) {
                            BoolValue(false)
                        } else {
                            val ev2 = evalExpr(e2, env)
                            if (ValueOps.isBoolean(ev2)) {
                                ev2
                            } else {
                                throw new RuntimeError("Unexpected type error. e2 should be boolean for 'and'")
                            }
                        }
                    case _ => throw new RuntimeError("Unexpected type error. e1 should be boolean for 'and'")
                }
            case Or(e1, e2) =>
                val ev1 = evalExpr(e1, env)
                ev1 match {
                    case BoolValue(b) =>
                        if (b) {
                            BoolValue(true)
                        } else {
                            val ev2 = evalExpr(e2, env)
                            if (ValueOps.isBoolean(ev2)) {
                                ev2
                            } else {
                                throw new RuntimeError("Unexpected type error. e2 should be boolean for 'or'")
                            }
                        }
                    case _ => throw new RuntimeError("Unexpected type error. e1 should be boolean for 'or'")
                }
            case Not(e1) =>
                evalExpr(e1, env) match {
                    case BoolValue(b) => BoolValue(!b)
                    case _ => throw new RuntimeError("Unexpected type error. operation for 'not' should be boolean")
                }
            case IfThenElse(e1, e2, e3) =>
                evalExpr(e1, env) match {
                    case BoolValue(b) =>
                        if (b) {
                            evalExpr(e2, env)
                        } else {
                            evalExpr(e3, env)
                        }
                    case _ => throw new RuntimeError("Unexpected type error. operation for 'not' should be boolean")
                }
        }
    }

    /*--
    TODO: Implement a function evalVarDefine that given a identifier x,
    expression e and environment env,
       a) evaluates e under env: let result be v
       b) yields new environment that updates env with {x -> v }
     For your convenience the RuntimeError exception has been handled for you.
     */
    def evalVarDefine(x: String, e: Expr, env: Environment): Environment = {
        try {
            val r = evalExpr(e, env)
            env ++ Map(x -> r)
        } catch {
            case _:RuntimeError =>  env
        }
    }

    /*-- TODO: Complete the evalCommand Function Below --*/
    // Function evalCommand
    // Evaluate a command under an environment.
    //  Returns the new environment as a result of executing the command.
    //  If the command is of the form Define(x, e), the environment is updated by evaluating
    //  e under the "old" environment and updating the old environment to now bind x to the result.
    // If the command is of the form Display(e), the environment returned is just the
    // same as the environment that was passed in as an argument.
    //
    def evalCommand( env: Environment, cmd: Cmd): Environment = {
        cmd match {
            case Display(e) =>
                val v = evalExpr(e, env)
                println(s"Displaying result: $v")
                env
            case Define(s, e) => evalVarDefine(s, e, env)
        }
    }

    /*-- TODO: Implement evalProgram function below.
       Careful: Do not use for/while loops. Instead you should be using
       pattern matching on `prog` and then using lst foldLeft function.
       A tail recursive solution is also acceptable but please try to use pattern matching.
     */
    def evalProgram(prog: CalcProgram, env0: Environment = Map.empty): Environment = {
        prog match {
            case TopLevel(l) => l.foldLeft(env0)((env, cmd) => evalCommand(env, cmd))
            case _ => throw new RuntimeError("Unsupported")
        }
    }

}
