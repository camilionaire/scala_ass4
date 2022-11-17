//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22)
//-------------------------------------------------------------------------

// FuncLang - A toy functional language
//
// Grammar:
//   Expr -> <num>
//        |  <id>                    
//        |  (Op Expr Expr)
//        |  (if Expr Expr Expr)  
//        |  (let <id> Expr Expr)      // local scope
//        |  (let* <id> Expr Expr)     // recursive version of let
//        |  (fun <id> Expr)           // lambda func
//        |  (@ Expr Expr)             // func application
//   Op   -> + | - | * | / | %         
//        |  < | > | ==                
//   <num>: integer  // "wholeNumber"                  
//   <id>:  string   // "ident"
//
import scala.util.parsing.combinator._

object FuncLang {
  sealed abstract class Expr
  case class Num(i:Int) extends Expr
  case class Var(id:String) extends Expr
  case class Add(l:Expr,r:Expr) extends Expr
  case class Sub(l:Expr,r:Expr) extends Expr
  case class Mul(l:Expr,r:Expr) extends Expr
  case class Div(l:Expr,r:Expr) extends Expr
  case class Rem(l:Expr,r:Expr) extends Expr
  case class Lt(l:Expr,r:Expr) extends Expr
  case class Gt(l:Expr,r:Expr) extends Expr
  case class Eq(l:Expr,r:Expr) extends Expr
  case class If(c:Expr,t:Expr,e:Expr) extends Expr
  case class Let(id:String,b:Expr,e:Expr) extends Expr
  case class LetRec(id:String,b:Expr,e:Expr) extends Expr
  case class Fun(x:String,b:Expr) extends Expr
  case class Apply(f:Expr,e:Expr) extends Expr

  case class ParseException(string: String) extends RuntimeException

  object FLParse extends JavaTokenParsers {
    // input program is in s-expression form
    def expr: Parser[Expr] = atm | lst
    def atm: Parser[Expr] = 
          wholeNumber ^^ {s => Num(s.toInt)} |
          ident ^^ {s => Var(s)}
    def lst: Parser[Expr] = 
          "(+" ~> expr~expr <~ ")"  ^^ {case l~r => Add(l,r)} |
          "(-" ~> expr~expr <~ ")"  ^^ {case l~r => Sub(l,r)} |
          "(*" ~> expr~expr <~ ")"  ^^ {case l~r => Mul(l,r)} |
          "(/" ~> expr~expr <~ ")"  ^^ {case l~r => Div(l,r)} |
          "(%" ~> expr~expr <~ ")"  ^^ {case l~r => Rem(l,r)} |
          "(<" ~> expr~expr <~ ")"  ^^ {case l~r => Lt(l,r)}  |
          "(>" ~> expr~expr <~ ")"  ^^ {case l~r => Gt(l,r)}  |
          "(==" ~> expr~expr <~ ")" ^^ {case l~r => Eq(l,r)}  |
          "(if" ~> expr~expr~expr <~ ")"   ^^ {case c~l~r => If(c,l,r)}  |
          "(let" ~> ident~expr~expr <~ ")" ^^ {case x~b~e => Let(x,b,e)} |
          "(let*" ~> ident~expr~expr <~ ")" ^^ {case x~b~e => LetRec(x,b,e)} |
          "(fun" ~> ident~expr <~ ")" ^^ {case x~b => Fun(x,b)} |
          "(@" ~> expr~expr <~ ")"    ^^ {case f~e => Apply(f,e)} 

    def apply(text: String): Expr = {
      val result = parseAll(expr, text)
      result match {
        case Success(r, _) => r
        case Failure(msg, n) =>
          throw ParseException(msg +
            " (input left: \"" + n.source.toString.drop(n.offset) + "\")")
        case Error(msg, n) =>
          throw ParseException(msg +
            " (input left: \"" + n.source.toString.drop(n.offset) + "\")")
      }
    }
  }

  // Test driver
  // Test driver (two options)
  // 1. Input from StdIn:  linux> scala FuncLang -
  //                       ...program (in single line)...
  // 2. Input from file:   linux> scala FuncLang <filename>
  //
  import scala.io._
  def main(argv: Array[String]) = {
    try {
      val s = if (argv(0) == "-") StdIn.readLine()
              else Source.fromFile(argv(0)).getLines.mkString("\n")
      val p = FLParse(s)
      println("Input: " + s)
      println("AST:   " + p)
    } catch {
      case ex: ParseException => { println("Parser Error:" + ex.string) }
    }
  }
}    
//
