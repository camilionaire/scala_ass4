//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22) [Partially based on A. Tolmach's code]
//-------------------------------------------------------------------------

// FuncLang Interpreter
//
// Usage: linux> scala FLInterp <source file>
//

// Edited by:Camilo Schaser-Hughes
// Date November 30, 2022
import FuncLang._

object FLInterp {
  case class InterpException(string: String) extends RuntimeException

  // Value represenation
  sealed abstract class Value
  case class NumV(n:Int) extends Value
  case class ClosureV(x:String,b:Expr,env:Env) extends Value

  // Storage represenation
  type Index = Int
  sealed abstract class Store {
    case class UndefinedContents(string: String) extends RuntimeException
    private val contents = collection.mutable.Map[Index,Value]()
    def get(i:Index) = contents.getOrElse(i, throw UndefinedContents("" + i))
    def set(i:Index,v:Value) = contents += (i->v)
    override def toString: String = contents.toString
  }
  // Heap
  class HeapStore extends Store {
    private var nextFreeIndex:Index = 0
    def allocate(n:Int): Addr = {
      val i = nextFreeIndex
      nextFreeIndex += n
      HeapAddr(i)
    }
    // there is no mechanism for deallocation
    override def toString: String = "[next=" + nextFreeIndex + "] " + super.toString
  }
  // Stack
  class StackStore extends Store {
    private var stackPointer:Index = 0;
    def push(): Addr = {
      val i = stackPointer
      stackPointer += 1
      StackAddr(i)
    }
    def pop() = {
      if (stackPointer > 0)
        stackPointer -= 1
      else
        throw InterpException("stack storage is empty")
    }
    def isEmpty(): Boolean = stackPointer == 0
    override def toString: String = "[sp=" + stackPointer + "] " + super.toString
  }

  // Address to storage
  sealed abstract class Addr() {
    def +(offset:Int): Addr
  }
  case class HeapAddr(index:Int) extends Addr {
    def +(offset:Int) = HeapAddr(index+offset)
  }
  case class StackAddr(index:Int) extends Addr {
    def +(offset:Int) = StackAddr(index+offset)
  }

  type Env = Map[String,Addr]

  // Main interpreter function
  // . useHeap - flag for choosing heap storage
  // . callByName - flag for choosing call-by-name param passing mode
  def interp(p:Expr,useHeap:Boolean=false,callByName:Boolean=false,
               debug:Int=0): Int = {
    val heap = new HeapStore()
    val stack = new StackStore()
    val env: Env = Map[String,Addr]() // initial env (empty)

    def get(a:Addr) = a match {
      case HeapAddr(i)  => heap.get(i)
      case StackAddr(i) => stack.get(i)
    }

    def set(a:Addr,v:Value) = a match {
      case HeapAddr(i)  => heap.set(i,v)
      case StackAddr(i) => stack.set(i,v)
    }

    def interpVar(env:Env,x:String): Addr =
      env.getOrElse(x, throw InterpException("undefined variable:" + x))

    def interpBop(env:Env, l: Expr, r:Expr, op:(Int,Int)=>Int) = {
      val lv = interpE(env,l)
      val rv = interpE(env,r)
      (lv,rv) match {
        case (NumV(ln),NumV(rn)) => NumV(op(ln,rn))
        case _ => throw InterpException("non-numeric argument to numeric operator")
      }   
    }

    def interpE(env:Env,e:Expr): Value = {
      if (debug > 1) {
        println("expr = "+ e)
        println("env = " + env)
        println("stack = " + stack)
        println("heap = " + heap)
      } 
      e match {
        case Num(n) => NumV(n)
        case Var(x) => get(interpVar(env,x))
        case Add(l,r) => interpBop(env,l,r,(lv,rv)=>lv+rv)
        case Sub(l,r) => interpBop(env,l,r,(lv,rv)=>lv-rv)  
        case Mul(l,r) => interpBop(env,l,r,(lv,rv)=>lv*rv)  
        case Div(l,r) => interpBop(env,l,r,(lv,rv)=> 
               if (rv!=0) lv/rv else throw InterpException("divide by zero"))
        case Rem(l,r) => interpBop(env,l,r,(lv,rv)=> 
               if (rv!=0) lv%rv else throw InterpException("divide by zero"))
        case Lt(l,r) => interpBop(env,l,r,(lv,rv)=> if (lv<rv) 1 else 0) 
        case Gt(l,r) => interpBop(env,l,r,(lv,rv)=> if (lv>rv) 1 else 0) 
        case Eq(l,r) => interpBop(env,l,r,(lv,rv)=> if (lv==rv) 1 else 0)
        // just the same old if statement
        case If(c,t,e) => {
          val cv = interpE(env, c)
          cv match {
            case NumV(n) => {
              if (n != 0) {
                interpE(env, t)
              } else {
                interpE(env, e)
              }
            }
            case _ => throw InterpException("conditionals have to evaluate to a numV")
          }
        } // end of If
        // same of let statement
        case Let(x,b,e) => {
          // does same thing except doesn't pop for heap
          if (useHeap) {
            val cb = interpE(env, b)
            val addy:Addr = heap.allocate(1)
            set(addy, cb)
            val ne = env + (x -> addy)
            interpE(ne, e)
          } else {
            val cb = interpE(env, b)
            val addy:Addr = stack.push()
            set(addy, cb)
            val ne = env + (x -> addy)
            val ev = interpE(ne, e)
            stack.pop()
            ev
          }
        } // end of Let
        case LetRec(x,b,e) => {
          // does the same things except don't pop
          if (useHeap) {
            val addy:Addr = heap.allocate(1)
            val ne = env + (x -> addy)
            val vb = interpE(ne, b)
            vb match {
              case ClosureV(what,ev,er) => {
                set(addy, vb)
                interpE(ne, e)
              }
              case _ => throw InterpException("second terms gotta be a closure...")
            }
          } else {
            val addy:Addr = stack.push()
            val ne = env + (x -> addy)
            val vb = interpE(ne, b)
            vb match {
              case ClosureV(what,ev,er) => {
                set(addy, vb)
                val ve = interpE(ne, e)
                stack.pop()
                ve
              }
              case _ => throw InterpException("second terms gotta be a closure...")
            }
          }
        } // end of LetRec
        // just returns a closure, I think that's all it needs
        case Fun(x,b) => {
          ClosureV(x, b, env)
        } // end of Fun



        case Apply(f,e) => {
          interpE(env, f) match {
            // first we make sure it's a closure
            case ClosureV(x, b, cl_env) => {
              // then if it's call by name
              if (callByName) {
                // super long recursive helper function
                def substitute(e:Expr, repX:String, y:Expr):Expr = {
                  e match {
                    case Num(n) => e
                    case Var(x) => if (x == repX) y else e
                    case Add(l,r) => Add(substitute(l,repX,y), substitute(r,repX,y))
                    case Sub(l,r) => Sub(substitute(l,repX,y), substitute(r,repX,y))
                    case Mul(l,r) => Mul(substitute(l,repX,y), substitute(r,repX,y))
                    case Div(l,r) => Div(substitute(l,repX,y), substitute(r,repX,y))
                    case Rem(l,r) => Rem(substitute(l,repX,y), substitute(r,repX,y))
                    case Lt(l,r) => Lt(substitute(l,repX,y), substitute(r,repX,y))
                    case Gt(l,r) => Gt(substitute(l,repX,y), substitute(r,repX,y))
                    case Eq(l,r) => Eq(substitute(l,repX,y), substitute(r,repX,y))
                    case If(c,t,f) => If(substitute(c,repX,y), substitute(t,repX,y), substitute(f,repX,y))
                    case Let(w,b,f) => if (w == repX) e else Let(w, substitute(b,repX,y), substitute(f,repX,y))
                    case LetRec(w,b,f) => if (w == repX) e else LetRec(w, substitute(b,repX,y), substitute(f,repX,y))
                    case Fun(w,b) => if (w == repX) e else Fun(w, substitute(b,repX,y))
                    case Apply(f, b) => Apply(f, substitute(b,repX,y))                      
                  }
                } // end of substitute rec function...
                // subsittute and then interpret
                val vs:Expr = substitute(b, x, e)
                interpE(cl_env, vs)
              } // end of if call by name 

              else { // if it's not a call by name:
              // interp and then do the same thing stack-pop heap no
                val ve = interpE(env, e)
                if (useHeap) {
                  val addy = heap.allocate(1);
                  set(addy, ve)
                  val ne = cl_env + (x -> addy)
                  interpE(ne, b)
                } else {
                  val addy = stack.push()
                  set(addy, ve)
                  val ne = cl_env + (x -> addy)
                  val vb = interpE(ne, b)
                  stack.pop
                  vb
                }
              }
            }
            case _ => throw InterpException("can't apply to a non closure.")
          }
        } // end of Apply, i think?
      } // end of e match
    } // end of interpE

    // process the top-level expression
    val v = interpE(env,p)
    if (debug > 0) println("Expression evaluates to: " + v)
    v match {
      case NumV(n) => n
      case _ => throw InterpException("top-level expr returns non-integer")
    }
  }

  def apply(s:String,useHeap:Boolean=false,callByName:Boolean=false,
            debug:Int=0): Int = {
    if (debug > 0) println("Input:  " + s)
    val p = FLParse(s)
    if (debug > 0) println("AST:    " + p)
    interp(p,useHeap,callByName,debug)
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    try {
      val s = Source.fromFile(argv(0)).getLines.mkString("\n")
      var heapFlag = false
      var cbnFlag = false
      var debugFlag = 0
      for (arg <- argv) {
        if (arg == "heap") heapFlag = true
        if (arg == "cbn") cbnFlag = true
        if (arg == "1") debugFlag = 1
        if (arg == "2") debugFlag = 2
      }
      val v = apply(s,heapFlag,cbnFlag,debugFlag)
      println(v)
    } catch {
      case ex: ParseException =>  println("Parser Error: " + ex.string)
      case ex: InterpException => println("Interp Error: " + ex.string)
    }
  }
}

//
