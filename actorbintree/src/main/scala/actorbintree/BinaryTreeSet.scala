/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import akka.event.LoggingReceive

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /**
   * Request with identifier `id` to insert an element `elem` into the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to check whether an element `elem` is present
   * in the tree. The actor at reference `requester` should be notified when
   * this operation is completed.
   */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to remove the element `elem` from the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /**
   * Holds the answer to the Contains request with identifier `id`.
   * `result` is true if and only if the element is present in the tree.
   */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply
 
}

class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true), name = "root")
  var count= 0
  def createNewRoot: ActorRef = {
    count = count+1
    context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true), name = s"newRoot$count")
  }

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = waiting

  def running(): Receive = LoggingReceive {
    case OperationFinished(id) =>
      val head = pendingQueue.head
      head match {
        case Insert(requester, id, elemToInsert) => requester ! OperationFinished(id)
        case Remove(requester, id, elemToInsert) => requester ! OperationFinished(id)
      }
      pendingQueue = pendingQueue.tail
      context.become(runNext)

    case ContainsResult(id, result) =>
      val head = pendingQueue.head
      head match {
        case Contains(requester, id, elemToCheck) => requester ! ContainsResult(id, result)
      }
      pendingQueue = pendingQueue.tail
      context.become(runNext)

    case op: Operation => context.become(enqueueJob(op))

    case GC =>
      val newRoot = createNewRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
  }

  def runNext(): Receive = LoggingReceive {
    if (pendingQueue.isEmpty) waiting
    else {
      root ! pendingQueue.head
      running
    }
  }

  def enqueueJob(op: Operation): Receive = LoggingReceive {
    pendingQueue :+= op
    running
  }

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val waiting: Receive = LoggingReceive {
    case op: Operation =>
      pendingQueue :+= op
      context.become(runNext)
      
    case GC =>
      val newRoot = createNewRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
  }

  // optional
  /**
   * Handles messages while garbage collection is performed.
   * `newRoot` is the root of the new binary tree where we want to copy
   * all non-removed elements into.
   */
  def garbageCollecting(newRoot: ActorRef): Receive = LoggingReceive {
    case GC =>
    case CopyFinished => {
      println("GC COMPLETE - shutting down")
      //      context.watch(root)
      //      root ! PoisonPill
      context.stop(root)
      Thread.sleep(400)
      root = newRoot
      context.become(runNext)
    }
    /*    case Terminated(_) => {
      println("root terminated")
      context.unwatch(root)
      root = newRoot
      context.become(runNext)
    }*/
    case OperationFinished(id) => {
//      println("op finished in garbage collecting " + id)
    }
    case op: Operation =>
      pendingQueue :+= op
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  
/*  override def postStop(): Unit = {
    println("Stopping " + elem)
  } */ 
  
  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = LoggingReceive {
    case Insert(requester, id, elemToInsert) => {
      if (elemToInsert < elem) {
        subtrees.get(Left) match {
          case Some(actorRef) => {
            actorRef ! Insert(requester, id, elemToInsert)
          }
          case None => {
            subtrees += Left -> context.actorOf(props(elemToInsert, false), name = "node-" + elemToInsert)

            if (id < 0) requester ! OperationFinished(id) else context.parent ! OperationFinished(id)
          }
        }
      } else if (elemToInsert > elem) {
        subtrees.get(Right) match {
          case Some(actorRef) => {
            actorRef ! Insert(requester, id, elemToInsert)
          }
          case None => {
            subtrees += Right -> context.actorOf(props(elemToInsert, false), name = "node-" + elemToInsert)
            if (id < 0) requester ! OperationFinished(id) else context.parent ! OperationFinished(id)
          }
        }
      } else if (elemToInsert == elem && removed) {
        removed = false
        if (id < 0) requester ! OperationFinished(id) else context.parent ! OperationFinished(id)
      } else {
        if (id < 0) requester ! OperationFinished(id) else context.parent ! OperationFinished(id)
      }
    }
    case Remove(requester, id, elemToRemove) => {
      if (elemToRemove == elem) {
        removed = true
        context.parent ! OperationFinished(id)
      } else if (elemToRemove < elem) {
        subtrees.get(Left) match {
          case Some(actorRef) => actorRef ! Remove(requester, id, elemToRemove)
          case None => context.parent ! OperationFinished(id)
        }
      } else if (elemToRemove > elem) {
        subtrees.get(Right) match {
          case Some(actorRef) => actorRef ! Remove(requester, id, elemToRemove)
          case None => context.parent ! OperationFinished(id)
        }
      }
    }
    case Contains(requester, id, elemToCheck) => {
      if (elemToCheck == elem) {
        context.parent ! ContainsResult(id, !removed)
      } else if (elemToCheck < elem) {
        subtrees.get(Left) match {
          case Some(actorRef) => actorRef ! Contains(requester, id, elemToCheck)
          case None => context.parent ! ContainsResult(id, false)
        }
      } else if (elemToCheck > elem) {
        subtrees.get(Right) match {
          case Some(actorRef) => actorRef ! Contains(requester, id, elemToCheck)
          case None => context.parent ! ContainsResult(id, false)
        }
      } 
    }

    case ContainsResult(id, result) => context.parent ! ContainsResult(id, result)
    case OperationFinished(id) => context.parent ! OperationFinished(id)

   case CopyTo(treeNode) => {
      if (subtrees.isEmpty && removed) {
        context.parent ! CopyFinished
      } else {
        val actorRefs = subtrees.values.toSet
        context.become(copying(actorRefs, removed))
        
        actorRefs foreach (_ ! CopyTo(treeNode))
        if (!removed) {
          treeNode ! Insert(self, -1, elem)
        }
      }
    }  
  }

  // optional
  /**
   * `expected` is the set of ActorRefs whose replies we are waiting for,
   * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
   */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = LoggingReceive {
    case OperationFinished(id) => {
      if (expected.isEmpty) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(expected, true))
      }
    }
    case CopyFinished => {
      val newSet = expected - sender
      if (newSet.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(newSet, insertConfirmed))
      }
    }
  }
}
