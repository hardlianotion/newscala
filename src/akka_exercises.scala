/**
 * Created with IntelliJ IDEA.
 * User: etuka
 * Date: 24/06/2012
 * Time: 18:40
 * To change this template use File | Settings | File Templates.
 */
package akka.tutorial.first.scala

import akka.actor._
import akka.routing.RoundRobinRouter
import akka.util.Duration
import akka.util.duration._
import scala.util.Random
import scala.math.pow

sealed trait PiMessage

case object Calculate extends PiMessage

case class Work(start: Int, nrOfElements: Int) extends PiMessage

case class Result(value: (Double, Double)) extends PiMessage

case class PiApproximation(pi: Double, duration: Duration)

class Worker extends Actor {

  def calculatePiFor(start: Int, numElements: Int): (Double, Double) = {
    val random = new Random(2 * start + 1)
    var result = 0.0
    for (i <- 0 to numElements) {
      val x = random.nextDouble
      val y = random.nextDouble
      if (x*x + y*y < 1.0)
        result += 1.0
    }
    (result, numElements.toDouble)
  }

  def receive = {
    case Work(start, numElements) =>
      sender ! Result(calculatePiFor(start, numElements)) // perform the work
  }
}

class Master(numWorkers: Int, numMessages: Int, numElements: Int, listener: ActorRef)
  extends Actor {

  var cumulants: Double = _
  var sims: Double = _
  var numResults: Int = _
  val start: Long = System.currentTimeMillis

  val workerRouter = context.actorOf(
    Props[Worker].withRouter(RoundRobinRouter(numWorkers)), name = "workerRouter")

  def receive = {
    case Calculate =>
      for (i <- 0 until numMessages) workerRouter ! Work(i * numElements, numElements)
    //case Result((value, count)) =>
    case Result((value,count)) =>
      cumulants += value
      sims += count
      numResults += 1

      println("count: " + numResults.toString)

      if (numResults == numMessages) {
        // Send the result to the listener
        listener ! PiApproximation(4*cumulants/sims, duration = (System.currentTimeMillis - start).millis)
        // Stops this actor and all its supervised children
        context.stop(self)
      }
  }

}

class Listener extends Actor {
  def receive = {
    case PiApproximation(pi, duration) =>
      println("\n\tPi approximation: \t\t%s\n\tCalculation time: \t%s"
        .format(pi, duration))
      context.system.shutdown()
  }
}

object Pi extends App {

  calculate(nrOfWorkers = 8, nrOfElements = 10000, nrOfMessages = 10000)

  // actors and messages ...

  def calculate(nrOfWorkers: Int, nrOfElements: Int, nrOfMessages: Int) {
    // Create an Akka system
    val system = ActorSystem("PiSystem")

    // create the result listener, which will print the result and shutdown the system
    val listener = system.actorOf(Props[Listener], name = "listener")

    // create the master
    val master = system.actorOf(Props(new Master(
      nrOfWorkers, nrOfMessages, nrOfElements, listener)),
      name = "master")

    // start the calculation
    master ! Calculate

  }
}