/**
  Authors: Joe Schlessinger and Matthew Yuan
  Date Created: 21 February, 2018
  Version: v0.0.1
  Description: This is a baseline implementation 
  of a Splay Tree that only holds Ints.

**/

object SplayTree {

  trait MSet { //characteristic functions of a mutable set
    def isEmpty: Boolean
    def contains(x: Int): Boolean
    def add(x: Int): Unit
    def remove(x: Int): Unit
  }

  case class Node(item: Int, left: MSet, right: MSet) extends MSet{
    def isEmpty: Boolean = {
      ???
    }

    def contains(x: Int): Boolean = {
      ???
    }

    def add(x: Int): Unit = {
      ???
    }

    def remove(x: Int): Unit = {
      ???
    }
  }

  case object Empty extends MSet {
    def isEmpty = true
    def contains(x: Int) = false
    def add(x: Int) = Node(x, Empty, Empty)
    def remove(x: Int) = throw new Exception("Cannot remove from an empty set")
  }
}
