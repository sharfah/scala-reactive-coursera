package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
  
  @Override
  override def toString(): String= {
    "Wire " + getSignal
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    // a1|a2 = ~(~a1 & ~a2)
    val notA1, notA2, andNotA1A2 = new Wire
    inverter(a1, notA1)
    inverter(a2, notA2)
    andGate(notA1, notA2, andNotA1A2)
    inverter(andNotA1A2, output)
  }
  
  def demux(in: Wire, c: List[Wire], out: List[Wire]) {

    // think of demux as a tree...

    // builds a single node
    def buildNode(in: Wire, c: Wire): (Wire, Wire) = {
      val outC, out1, out2 = new Wire
      inverter(c, outC)
      andGate(in, outC, out1)
      andGate(in, c, out2)
      (out1, out2)
    }

    // builds the whole tree recursively
    def buildTree(in: Wire, c: List[Wire], out: List[Wire]) {
      if (c.isEmpty) {
        // just join the input to the output
        andGate(in, in, out.head)
      } else {
        val (out1, out2) = buildNode(in, c.head)
        val (list1, list2) = out.splitAt(out.size / 2)
        buildTree(out1, c.tail, list1)
        buildTree(out2, c.tail, list2)
      }
    }
    buildTree(in, c, out.reverse)
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
