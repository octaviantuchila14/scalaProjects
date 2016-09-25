package nodescala

import java.util.NoSuchElementException

import org.junit.Test
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span, Millis}

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

import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite with ScalaFutures {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be completed") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("Now terminates future that's done") {
    val testFuture: Future[Int] = Future{
      10
    }
    assert(testFuture.now == 10)
  }

  test("Any returns the future that completes first") {
    val f1: Future[Int] = Future{
      Thread.sleep(1000)
      1
    }
    val f2: Future[Int] = Future{
      Thread.sleep(2000)
      2
    }
    val f3: Future[Int] = Future{
      Thread.sleep(3000)
      3
    }

    val resF = Future.any(List(f1, f2, f3))
    implicit val defaultPatience = PatienceConfig(timeout = Span(5, Seconds), interval = Span(500, Millis))
    whenReady(resF) { r =>
      assert(r == 2, "Any should return the future that terminates first")
    }
//    resF.onComplete(r => assert(r.get == 2, "Any should return the future that terminates first"))

//    Await.ready(resF, Duration(5000, MILLISECONDS))
  }

//  @Test{val expected= classOf[NoSuchElementException]}
//  test("Now doesn't terminate future that's not done") {
//    val testFuture: Future[Int] = Future{
//      wait(1000)
//      10
//    }
//    assertThrows[NoSuchElementException]{
//      testFuture.now
//    }
//  }

//  def continueWith[S](cont: Future[T] => S): Future[S] = {
//    //      Await.ready(f)
//    Future{cont(f)}
//  }

//  test("continues with all the values") {
//    val cont = { Future{ println("first future") } => 1}
//  }

//  def continueWith[S](cont: Future[T] => S): Future[S] = Future{cont(f)}

  test("continueWith prints everything") {
    val f = Future{Thread.sleep(10000.toLong)}

    val t0 = System.currentTimeMillis()
    val res = f.continueWith(f => {
      "Finished!"
    })//.onComplete(r => assert(r.get == "Finished!"))
//    res.onComplete(r => assert(r.get == "Finished!"))
    val t1 = System.currentTimeMillis()
    assert(t1 - t0 <= 10000, "The second part of the code was not executed")
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




