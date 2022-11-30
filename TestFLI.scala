//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22) [Partially based on A. Tolmach's code]
//-------------------------------------------------------------------------

// Testing FuncLang interpreter
//
import org.scalatest.FunSuite
import FLInterp._

class TestFLI extends FunSuite {

  test("test let" ) {
    assertResult(3) { FLInterp("(let x 1 (let y 2 (+ x y)))") }
    assertResult(4) { FLInterp("(let x 1 (let x 2 (+ x x)))") }
    intercept[InterpException] { FLInterp("(let* x 1 (+ x 1))") }
  }

  test("simple functions" ) {
    assertResult(6) { FLInterp("(@ (fun x (+ x 1)) 5)") }
    assertResult(6) { FLInterp("(let f (fun x (+ x 1)) (@ f 5))") }
    assertResult(26) { FLInterp("(let f (fun x (+ (* x x) 1)) (@ f 5))") }
    assertResult(4) { FLInterp("(@ (fun x (let x 3 (+ x 1))) 4)") }
  }
                        // these are the answers I got on the callbyName the are wrong
  val example1 = """(@ (@ (fun x (fun y (+ x y))) 2) 3)""" // got 4 sposed to be 5

  val example2 = """(let f (let y 4 (fun x y)) (@ f 1))""" // got 1 // sposed to be 4

  val example3 = """(let f (fun x (fun y x)) (@ (@ f 2) 1))"""  // got 2

  val example4 = """(let f (let z 0 (let y 3 (fun x y))) (@ f 1))""" // got 1 sposed to be 3
    
  val facCode = """(let* fac 
                         (fun n (if (< n 1)
                                1 
                                (* n (@ fac (- n 1)))))
                         (@ fac 5))"""

  test("naive impl of first-class func examples 1-4") {
    assertResult(6) { FLInterp(example1) }
    intercept[InterpException] { FLInterp(example2) }
    assertResult(1) { FLInterp(example3) }
    assertResult(1) { FLInterp(example4) }
  }

  test("correct impl of first-class func examples 1-4") {
    assertResult(5) { FLInterp(example1,useHeap=true) }
    assertResult(4) { FLInterp(example2,useHeap=true) }
    assertResult(2) { FLInterp(example3,useHeap=true) }
    assertResult(3) { FLInterp(example4,useHeap=true) }
  }

  test("call-by-name impl of first-class func examples 1-4") {
    assertResult(5) { FLInterp(example1,useHeap=true,callByName=true) } // got 4
    assertResult(4) { FLInterp(example2,useHeap=true,callByName=true) } // got 1
    assertResult(2) { FLInterp(example3,useHeap=true,callByName=true) } // passed i guess?
    assertResult(3) { FLInterp(example4,useHeap=true,callByName=true) } // got 1
  }

  test("factorial function") {
    assertResult(120) { FLInterp(facCode) }
  }

  test("factorial function (useHeap)") {
    assertResult(120) { FLInterp(facCode,useHeap=true) }
  }

  test("factorial function (useHeap,CBN)") {
    assertResult(120) { FLInterp(facCode,useHeap=true,callByName=true) }
  }

  test("more call-by-name") {
    assertResult(6) { FLInterp("(@ (fun x (+ x 1)) (+ 2 3))", true,true) }
    assertResult(4) { FLInterp("(@ (fun x (let x 3 (+ x 1))) 4)",true,true) }
    assertResult(5) { 
      FLInterp("(let y 4 (@ (fun x (let z 3 (+ x 1))) y))",true,true) }
    assertResult(4) { 
      FLInterp("(let y 4 (@ (fun x (let x 3 (+ x 1))) y))",true,true) }
  }

  test("lazy-eval") {
    intercept[InterpException] { FLInterp("(let f (fun x 1) (@ f (/ 5 0)))") }
    assertResult(1) { FLInterp("(let f (fun x 1) (@ f (/ 5 0)))",true,true) }
  }

}
