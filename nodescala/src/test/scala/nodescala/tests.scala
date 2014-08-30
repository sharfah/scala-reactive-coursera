package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.util.Date
import org.junit.rules.ExpectedException

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }
  /*
  test("any") {
    val list = List(Future { Thread.sleep(2000);1 }, Future { Thread.sleep(1000);2 }, Future { Thread.sleep(1500); throw new Exception("I FAILED") })
    val any = Future.any(list)
    val result = Await.result(any, 5 seconds)
    println("ANY")
    println(result)
  }  

  
  test("all") {
    val list = List(Future { Thread.sleep(1000);1 }, Future { Thread.sleep(2000);2 }, Future { Thread.sleep(1000);3 })
    println(new Date)
    val all = Future.all(list)
    val result = Await.result(all, 5 seconds)
    println("ALL")
    println(result)
    println(new Date)
  }  
  
  test("delay") {
    println("DELAY")
    println(new Date)
    val delay = Future.delay(3 second)
    val result = Await.result(delay, 5 seconds)
    println(new Date)
  }

  test("now throws NoSuchElementException") {
    try {
      Future.never.now
      assert(false)
    } catch {
      case t: NoSuchElementException => // ok!
    }
  }

  test("now throws IllegalStateException") {
    try {
      Future.failed(new IllegalStateException).now
      assert(false)
    } catch {
      case t: IllegalStateException => // ok!
    }
  }

  test("now successful") {
    assert(Future.successful(2).now == 2)
  }  

  test("continuewith") {
    val f = future(1)
    val s = f.continueWith(_ => throw new IllegalStateException("HELLO"))
    try {
      Await.result(s, Duration("100 ms"))
    } catch {
      case t: IllegalStateException => // OK!
    }
  } 

  test("continue") {
    val f = future(1)
    val s = f.continueWith(_ => throw new IllegalStateException("FOO"))
    try {
      Await.result(s, Duration("100 ms"))
    } catch {
      case t: IllegalStateException => // OK!
    }
  }  */

/*  test("Future should run until cancelled") {
    val working = Future.run() { ct =>
      Future {
        println(ct.nonCancelled)
        while (ct.nonCancelled) {
          println("working")
          Thread.sleep(1000)
        }
        println("done")
      }
    }
    Future.delay(5 seconds) onSuccess {
      case _ => working.unsubscribe()
    }
  }*/
    
  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




