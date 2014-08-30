package quickcheck

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._

import org.scalatest.exceptions.TestFailedException

object QuickCheckBinomialHeap extends QuickCheckHeap with BinomialHeap

@RunWith(classOf[JUnitRunner])
class QuickCheckSuite extends FunSuite with Checkers {
  def checkBogus(p: Prop) {
    var ok = false
    try {
      check(p)
    } catch {
      case e: TestFailedException =>
        ok = true
    }
    println(ok)
    assert(ok, "A bogus heap should NOT satisfy all properties. Try to find the bug!")
  }

  test("Binomial heap satisfies properties.") {
    println("Running good")
    check(new QuickCheckHeap with BinomialHeap)
  }

  test("Bogus (1) binomial heap does not satisfy properties.") {
    println("Running 1")
    checkBogus(new QuickCheckHeap with Bogus1BinomialHeap)
  }

  test("Bogus (2) binomial heap does not satisfy properties.") {
    println("Running 2")
    checkBogus(new QuickCheckHeap with Bogus2BinomialHeap)
  }

  test("Bogus (3) binomial heap does not satisfy properties.") {
    println("Running 3")
    checkBogus(new QuickCheckHeap with Bogus3BinomialHeap)
  }

  test("Bogus (4) binomial heap does not satisfy properties.") {
    println("Running 4")
    checkBogus(new QuickCheckHeap with Bogus4BinomialHeap)
  }
  
  test("Bogus (5) binomial heap does not satisfy properties.") {
    println("Running 5")
    checkBogus(new QuickCheckHeap with Bogus5BinomialHeap)
  }
}
