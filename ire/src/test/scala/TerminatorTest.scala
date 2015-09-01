import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActorRef, TestActors, TestKit}
import akka.util.Timeout
import hu.bme.mit.IQDcore._
import scala.concurrent.duration._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * Created by janosmaginecz on 10/05/15.
 */
class TerminatorTest (_system: ActorSystem) extends TestKit(_system) with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {
  def this() = this(ActorSystem("MySpec"))
  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }
  "alpha nodes" must {
    "propagate terminator messages" in {
      val echoActor = system.actorOf(TestActors.echoActorProps)
      val production = system.actorOf(Props(new Production("")))
      val intermediary = system.actorOf(Props(new Checker(production ! _, c => true)))
      val input = system.actorOf(Props(new Checker(intermediary ! _, c => true)))
      input ! ChangeSet(positive = Vector(Vector(15)))
      input ! ChangeSet(positive = Vector(Vector(19)))
      val terminator = Terminator(List(input ! _), production)
      val future = terminator.send
      val expected = Set(Vector(15),Vector(19))
      Await.result(future, Duration(1,SECONDS)) == expected
    }
  }
  "beta nodes" must {
    "propagate terminator messages" in {
      val echoActor = system.actorOf(TestActors.echoActorProps)
      val production = system.actorOf(Props(new Production("")))
      val intermediary = system.actorOf(Props(new HashJoiner(production ! _,1,Vector(0),1,Vector(0))))
      val input = system.actorOf(Props(new HashJoiner(intermediary ! Secondary(_),1,Vector(0), 1,Vector(0))))
      input ! Primary(ChangeSet(positive = Vector(Vector(15))))
      input ! Secondary(ChangeSet(positive = Vector(Vector(15))))
      intermediary ! Primary(ChangeSet(positive = Vector(Vector(15))))
      val terminator = Terminator(List(input ! _), production)
      val expected = Set(Vector(15))
      val future = terminator.send
      Await.result(future, Duration(1,SECONDS)) == expected
    }
  }
}
