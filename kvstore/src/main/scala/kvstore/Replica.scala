package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import akka.event.LoggingReceive
import akka.actor.Cancellable
import collection.mutable.{ HashMap, MultiMap, Set }
import scala.language.postfixOps
import akka.actor.TypedActor.Supervisor
import akka.actor.TypedActor.Supervisor
import akka.actor.OneForOneStrategy

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  override val supervisorStrategy = OneForOneStrategy() {
    case e:Exception => {
      println("Restarting " + e.getMessage())
      Restart
    }
  }
  
  val persistor = context.system.actorOf(persistenceProps)
  

  var counter = 0

  var primaryPersistingAcks = Map.empty[Long, (ActorRef, Cancellable)]
  var replicationAcks = Map.empty[Long, (ActorRef, Long)]
  var replicatorAcks = new HashMap[ActorRef, Set[Long]] with MultiMap[ActorRef, Long]

  arbiter ! Join

  def receive = LoggingReceive {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = LoggingReceive {
    case Insert(key, value, id) => {
      kv += key -> value

      if (!replicators.isEmpty) {
        replicationAcks += id -> (sender, replicators.size)
        replicators foreach { r =>
          replicatorAcks.addBinding(r, id)
          r ! Replicate(key, Some(value), id)
        }
      }

      primaryPersistingAcks += id -> (sender, context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, persistor, Persist(key, Some(value), id)))

      context.system.scheduler.scheduleOnce(1 second) {
        primaryPersistingAcks get id match {
          case Some((s, c)) => {
            c.cancel
            primaryPersistingAcks -= id
            s ! OperationFailed(id)
          }
          case None => {
            replicationAcks get id match {
              case Some((s, c)) => {
                replicationAcks -= id
                s ! OperationFailed(id)
              }
              case None =>
            }
          }
        }
      }
    }
    case Remove(key, id) => {
      kv -= key

      if (!replicators.isEmpty) {
        replicationAcks += id -> (sender, replicators.size)
        replicators foreach { r =>
          replicatorAcks.addBinding(r, id)
          r ! Replicate(key, None, id)
        }
      }

      primaryPersistingAcks += id -> (sender, context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, persistor, Persist(key, None, id)))

      context.system.scheduler.scheduleOnce(1 second) {
        primaryPersistingAcks get id match {
          case Some((s, c)) => {
            c.cancel
            primaryPersistingAcks -= id
            s ! OperationFailed(id)
          }
          case None => {
            replicationAcks get id match {
              case Some((s, c)) => {
                replicationAcks -= id
                s ! OperationFailed(id)
              }
              case None =>
            }
          }
        }
      }
    }
    case Get(key, id) => {
      val value: Option[String] = kv get key
      sender ! GetResult(key, value, id)
    }
    case Replicas(replicas) => {

      val secs = replicas - self
      val newJoiners = secs -- secondaries.keySet
      val leavers =  secondaries.keySet -- secs

      newJoiners foreach { nj =>
        val r = context.system.actorOf(Replicator.props(nj))
        secondaries += nj -> r
        replicators += r
        kv foreach { e =>
          r ! Replicate(e._1, Some(e._2), counter)
          counter += 1
        }
      }

      leavers foreach { l =>
        secondaries get l match {
          case Some(r) => {
            context.stop(r)
            secondaries -= l
            replicators -= r

            replicatorAcks get r match {
              case Some(outstandingAcks) => {
                outstandingAcks foreach { a =>
                  self ! Replicated("", a)
                }
                replicatorAcks -= r
              }
              case None =>
            }
          }
          case None =>
        }
      }
    }

    case Replicated(key, id) => {
      replicatorAcks get sender match {
        case Some(s) => {
          s-= id
        }
        case None =>
      }
      replicationAcks get id match {
        case Some((s, v)) => {
          val newValue = v - 1
          if (newValue == 0) {
            replicationAcks -= id
            if (!(primaryPersistingAcks contains id)) {
              s ! OperationAck(id)
            }
          } else {
            replicationAcks += id -> (s, newValue)
          }
        }
        case None =>
      }
    }

    case Persisted(key, id) => {
      primaryPersistingAcks get id match {
        case Some((s, c)) => {
          c.cancel
          primaryPersistingAcks -= id
          if (!(replicationAcks contains id)) {
            s ! OperationAck(id)
          }
        }
        case None =>
      }
    }
    case _ => println("Got unknown in leader")
  }

  var expectedSeq = 0L
  var secondaryPersistingAcks = Map.empty[Long, (ActorRef, String, Cancellable)]

  /* TODO Behavior for the replica role. */
  val replica: Receive = LoggingReceive {
    case Get(key, id) => {
      val value: Option[String] = kv get key
      sender ! GetResult(key, value, id)
    }
    case Snapshot(key, valueOption, seq) => {
      if (seq < expectedSeq) {
        sender ! SnapshotAck(key, seq)
      }
      else if (seq == expectedSeq) {
        valueOption match {
          case Some(value) => {
            kv += key -> value
            secondaryPersistingAcks += seq -> (sender, key, context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, persistor, Persist(key, Some(value), seq)))
          }
          case None => {
            kv -= key
            secondaryPersistingAcks += seq -> (sender, key, context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, persistor, Persist(key, None, seq)))
          }
        }
        expectedSeq += 1
      }
    }
    case Persisted(key, id) => {
      secondaryPersistingAcks get id match {
        case Some((replicator, k, c)) => {
          c.cancel
          secondaryPersistingAcks -= id
          replicator ! SnapshotAck(key, id)
        }
        case None =>
      }
    }
  }
}
