/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

  case object GCFinished

  case class GCollecting(newRoot: ActorRef)

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true, "set"))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case GC => {
      val newRoot: ActorRef = createRoot
      println("Received garbage collection message.")
      root ! GCollecting(newRoot)
      context.become(garbageCollecting(newRoot))
    }
    case op => root ! op
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case op: Operation => {
      println("Pending: " + op)
      pendingQueue :+= op
    }
    case GCFinished => {
      println("In TreeSet receive GCFinished!")
      if(sender == root) {
        pendingQueue.foreach(op => {
          println("Sending: " + op)
          newRoot ! op
        })
        pendingQueue = Queue.empty[Operation]
        root ! PoisonPill
        root = newRoot
        println("Switched root")
        context.become(normal)
      }
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean, parent: String) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved, parent)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean, parent: String) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  def addNodeOrSend(Child: BinaryTreeNode.Position, requester: ActorRef, id: Int, elem: Int): Unit = {
    if(!subtrees.contains(Child)) {
      subtrees += Child -> context.actorOf(props(elem, false, "node"))
      println("Inserted node: " + elem + " with id " + id)
      requester ! OperationFinished(id)
    } else {
      subtrees(Child) ! Insert(requester, id, elem)
    }
  }

    // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, elem) => {
      if(elem == this.elem && removed == false) {
        println("insert: " + elem + " in node with elem: " + this.elem)
        println("Operation finished: " + id)
        requester ! OperationFinished(id)
      }
      else if(elem < this.elem) {
        addNodeOrSend(Left, requester, id, elem)
      }
      else if(elem >= this.elem) {
        addNodeOrSend(Right, requester, id, elem)
      }
    }
    case Contains(requester, id, elem) => {
//      println("Contains: " + id)
      if (elem == this.elem && removed == false) {
        requester ! ContainsResult(id, true)
      }
      else if (elem < this.elem && subtrees.contains(Left)) {
        subtrees(Left) ! Contains(requester, id, elem)
      }
      else if (elem >= this.elem && subtrees.contains(Right)) {
        subtrees(Right) ! Contains(requester, id, elem)
      }
      else {
        requester ! ContainsResult(id, false)
      }
    }
    case Remove(requester, id, elem) => {
      if (elem == this.elem && removed == false) {
        removed = true
        requester ! OperationFinished(id)
      }
      else if (elem < this.elem && subtrees.contains(Left)) {
        subtrees(Left) ! Remove(requester, id, elem)
      }
      else if (elem >= this.elem && subtrees.contains(Right)) {
        subtrees(Right) ! Remove(requester, id, elem)
      }
      else {
        requester ! OperationFinished(id)
      }
    }
    case GCollecting(newRoot) => {
      println("Garbage collecting in: " + elem + " in node " + self, "removed: " + removed)
//      println("Removed: " + removed)
      if(!removed) {
        newRoot ! Insert(self, elem, elem)
      }
      subtrees.map(subtree => subtree._2 ! GCollecting(newRoot))
      context.become(copying(subtrees.values.toSet, removed))
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(_) => {
//      println("Inserted new node: " + elem)
      checkIfDone(expected, true)
    }
    case GCFinished => {
      println("Node " + elem + " received garbage collection finished message")
////      println("expected.size: " + expected.size)
      println("(expected - sender).size: " + (expected - sender).size)
      println("InsertConfirmed: " + insertConfirmed)
      checkIfDone(expected - sender, insertConfirmed)
    }
  }

  def checkIfDone(expected: Set[ActorRef], insertConfirmed: Boolean): Unit = {
      if(expected.isEmpty && insertConfirmed == true) {
        println("Garbage collection finished in: " + elem)
        println("Sending to: " + parent)
        context.parent ! GCFinished
        context.become(normal)
      }
      else {
        context.become(copying(expected, insertConfirmed))
      }
  }

}
