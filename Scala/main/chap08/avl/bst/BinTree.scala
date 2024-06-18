package chap08.avl.bst

import chap08.avl.bst.ImbalanceMagnitude.{ONE, TWO, ZERO}
import chap08.avl.bst.RightBiased

import java.util.NoSuchElementException
import scala.annotation.targetName
import scala.math.abs

enum ImbalanceMagnitude {
   case ZERO, ONE, TWO
}

sealed trait Imbalance {
   val magnitude: ImbalanceMagnitude
}

case object NoImbalance extends Imbalance {
   override val magnitude: ImbalanceMagnitude = ZERO
}

sealed trait LeftBiased extends Imbalance

sealed trait RightBiased extends Imbalance

case object LeftBiasedOne extends LeftBiased {
   override val magnitude: ImbalanceMagnitude = ONE
}

case object LeftBiasedTwo extends LeftBiased {
   override val magnitude: ImbalanceMagnitude = TWO
}

case object RightBiasedOne extends RightBiased {
   override val magnitude: ImbalanceMagnitude = ONE
}

case object RightBiasedTwo extends RightBiased {
   override val magnitude: ImbalanceMagnitude = TWO
}

object Imbalance {

   def apply(rightHeight: Int, leftHeight: Int): Imbalance = (rightHeight - leftHeight) match {
      case 0 => NoImbalance
      case -1 => LeftBiasedOne
      case -2 => LeftBiasedTwo
      case 1 => RightBiasedOne
      case 2 => RightBiasedTwo
      case unexpectedHeightDiff =>
         throw AssertionError(s"Height difference should never be great than +/- 2 (got $unexpectedHeightDiff)")
   }

}

sealed abstract class BinTree:
   def isEmpty: Boolean
   def contains(k: Int): Boolean

   def size: Int
   def height: Int

   def min: Int
   def max: Int

   @targetName("plus")
   def +(k: Int): BinTree

   @targetName("minus")
   def -(k: Int): BinTree

   def toList: List[Int] = makeList(List.empty)

   private[bst] def makeList(list: List[Int]): List[Int]

   private[bst] def imbalance: Imbalance
   private[bst] def rotateRight: Node
   private[bst] def rotateLeft: Node

   private[bst] def minRemoved: (Int, BinTree)
   private[bst] def maxRemoved: (Int, BinTree)

end BinTree

private case object Empty extends BinTree:
   
   override def isEmpty          = true
   override def contains(k: Int) = false

   override def size   = 0
   override def height = 0

   override def min: Int = throw NoSuchElementException("Empty.min")
   override def max: Int = throw NoSuchElementException("Empty.max")

   @targetName("plus")
   override def +(k: Int): BinTree = Node(k, Empty, Empty)

   @targetName("minus")
   override def -(k: Int): BinTree = this

   override def makeList(list: List[Int]): List[Int] = list

   override def imbalance: Imbalance = NoImbalance

   override def rotateRight: Node = throw AssertionError("empty trees are never rotated")
   override def rotateLeft: Node = throw AssertionError("empty trees are never rotated")
   override def minRemoved: (Int, BinTree) = throw AssertionError("empty trees are never reduced")
   override def maxRemoved: (Int, BinTree) = throw AssertionError("empty trees are never reduced")

end Empty

private case class Node(key: Int, left: BinTree, right: BinTree) extends BinTree:
   override def isEmpty = false

   override def contains(k: Int): Boolean =
      if k < key then left.contains(k)
      else if k > key then right.contains(k)
      else true

   override def size: Int = 1 + left.size + right.size
   val height: Int = 1 + (left.height max right.height) // use val to improve avl performance

   override def min: Int = if left.isEmpty then key else left.min
   override def max: Int = if right.isEmpty then key else right.max

   @targetName("plus")
   override def +(k: Int): Node =
      if k < key then Node(key, left + k, right).avl
      else if k > key then Node(key, left, right + k).avl
      else this

   override def minRemoved: (Int, BinTree) =
      if left.isEmpty then (key, right)
      else
         val (min, othersLeft) = left.minRemoved
         (min, Node(key, othersLeft, right).avl)

   override def maxRemoved: (Int, BinTree) =
      if right.isEmpty then (key, left)
      else
         val (max, othersRight) = right.maxRemoved
         (max, Node(key, left, othersRight).avl)

   @targetName("minus")
   override def -(k: Int): BinTree =
      if k < key then Node(key, left - k, right).avl
      else if k > key then Node(key, left, right - k).avl
      else if left.isEmpty then right
      else if right.isEmpty then left
      else
         val (minRight, othersRight) = right.minRemoved
         Node(minRight, left, othersRight).avl

   override def makeList(list: List[Int]): List[Int] = left.makeList(key :: right.makeList(list))

   override def imbalance: Imbalance = Imbalance(right.height, left.height)

   override def rotateRight: Node = left match {
      case leftNode: Node =>
         Node(
            key = leftNode.key,
            left = leftNode.left,
            right = Node(key, left = leftNode.right, right)
         )
      case Empty =>
         throw AssertionError("We should never call rotateRight with an empty left node")
   }

   override def rotateLeft: Node = right match {
      case rightNode: Node =>
         Node(
            key = rightNode.key,
            left = Node(key, left, right = rightNode.left),
            right = rightNode.right
         )
      case Empty =>
         throw AssertionError("We should never call rotateLeft with an empty right node")
   }

   def avl: Node = imbalance match {
      case LeftBiasedTwo => left.imbalance match {
         case NoImbalance | _: LeftBiased => rotateRight
         case _ : RightBiased => Node(key, left.rotateLeft, right).rotateRight
      }
      case RightBiasedTwo => right.imbalance match {
         case NoImbalance | _: RightBiased => rotateLeft
         case _ : LeftBiased => Node(key, left, right.rotateRight).rotateLeft
      }
      case _ => this
   }

end Node

object BinTree:
   def empty: BinTree = Empty

   def fromSet(keys: Set[Int]): BinTree =
      val keySeq = keys.toIndexedSeq.sorted

      def makeTree(from: Int, to: Int): BinTree =
         if from > to then Empty
         else
            val mid = (from + to) / 2
            Node(keySeq(mid), makeTree(from, mid - 1), makeTree(mid + 1, to))

      makeTree(0, keySeq.length - 1)

   end fromSet

end BinTree
