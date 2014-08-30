package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("demux1 example") {
    val in = new Wire
    val c0 = new Wire
    val out0 = new Wire
    val out1 = new Wire

    demux(in, List(c0), List(out1, out0))
    c0.setSignal(false)
    in.setSignal(false)
    run
    assert(out0.getSignal === false, "demux1 1")
    assert(out1.getSignal === false, "demux1 2")

    in.setSignal(true)
    run
    println(out0)
    println(out1)
    assert(out0.getSignal === true, "demux1 3")
    assert(out1.getSignal === false, "demux1 4")

    c0.setSignal(true)
    run
    assert(out0.getSignal === false, "demux1 5")
    assert(out1.getSignal === true, "demux1 6")
  }

  test("demux medium") {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    val c = c1 :: c0 :: Nil
    val o = o0 :: o1 :: o2 :: o3 :: Nil
    demux(in, c, o)

    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)

    in.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === true, 3)

    in.setSignal(true)
    c0.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === true, 2)
    assert(o3.getSignal === false, 3)
  }

  test("demux large") {
    val in, c0, c1, c2, c3, o0, o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15 = new Wire
    val c = c0 :: c1 :: c2 :: c3 :: Nil
    val o = o0 :: o1 :: o2 :: o3 :: o4 :: o5 :: o6 :: o7 :: o8 :: o9 :: o10 :: o11 :: o12 :: o13 :: o14 :: o15 :: Nil
    demux(in, c, o)

    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    assert(o4.getSignal === false, "o4")
    assert(o5.getSignal === false, "o5")
    assert(o6.getSignal === false, "o6")
    assert(o7.getSignal === false, "o7")
    assert(o8.getSignal === false, "o8")
    assert(o9.getSignal === false, "o9")
    assert(o10.getSignal === false, "o10")
    assert(o11.getSignal === false, "o11")
    assert(o12.getSignal === false, "o12")
    assert(o13.getSignal === false, "o13")
    assert(o14.getSignal === false, "o14")
    assert(o15.getSignal === false, "o15")

    in.setSignal(true)
    run

    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    assert(o4.getSignal === false, "o4")
    assert(o5.getSignal === false, "o5")
    assert(o6.getSignal === false, "o6")
    assert(o7.getSignal === false, "o7")
    assert(o8.getSignal === false, "o8")
    assert(o9.getSignal === false, "o9")
    assert(o10.getSignal === false, "o10")
    assert(o11.getSignal === false, "o11")
    assert(o12.getSignal === false, "o12")
    assert(o13.getSignal === false, "o13")
    assert(o14.getSignal === false, "o14")
    assert(o15.getSignal === true, "o15")

    in.setSignal(true)
    c0.setSignal(true)
    c3.setSignal(true)
    run

    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    assert(o4.getSignal === false, "o4")
    assert(o5.getSignal === false, "o5")
    assert(o6.getSignal === true, "o6")
    assert(o7.getSignal === false, "o7")
    assert(o8.getSignal === false, "o8")
    assert(o9.getSignal === false, "o9")
    assert(o10.getSignal === false, "o10")
    assert(o11.getSignal === false, "o11")
    assert(o12.getSignal === false, "o12")
    assert(o13.getSignal === false, "o13")
    assert(o14.getSignal === false, "o14")
    assert(o15.getSignal === false, "o15")
  }

}  
  