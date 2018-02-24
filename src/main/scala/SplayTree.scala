/**
  Authors: Joe Schlessinger and Matthew Yuan
  Date Created: 21 February, 2018
  Version: v0.0.1
  Description: This is a baseline implementation 
  of a Splay Tree that only holds Ints.

**/


//Set object holds SplayTree


class MSet { //characteristic functions of a mutable set
  private var tree = new SplayTree
  def isEmpty: Boolean = {
    (tree == Empty)
  }

  def contains(x: Int): Boolean
  def add(x: Int): this.type
  def remove(x: Int): this.type

}

abstract class SplayTree {
  def splay(right: Boolean): SplayTree
  
}

case class Node(item: Int,var left: SplayTree, var right: SplayTree) extends SplayTree{
  def splay(right: Boolean) = this
}

case object Empty extends SplayTree {
  def splay(right: Boolen) = this
}

