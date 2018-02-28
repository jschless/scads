/**
  Authors: Joe Schlessinger and Matthew Yuan
  Date Created: 21 February, 2018
  Version: v0.0.1
  Description: This is a baseline implementation 
  of a Splay Tree that only holds Ints.

**/


//Set object holds SplayTree


class MSet { //characteristic functions of a mutable set
  private var tree: SplayTree = Empty
  def isEmpty: Boolean = {
    (tree == Empty)
  }

  def contains(x: Int): Boolean = {
    tree.contains(x, "")._1
  }
  def add(x: Int): this.type = {
    ???
  }
  def remove(x: Int): this.type = {
    ???
  }

}

abstract class SplayTree() {
  //def splay(): SplayTree
  def rotateLeft: Node
  def rotateRight: Node
  def help(x: Int): String
  def contains(x: Int, str: String): (Boolean, String, Boolean)
  def splay(str: String): SplayTree
}

case class Node(var item: Int,var left: SplayTree, var right: SplayTree) extends SplayTree{

  def splay(str: String): Node.this.type = str match {
    case "LL" => this.rotateRight.rotateRight
    case "RR" => this.rotateLeft.rotateLeft
    case "LR" => {
      (this.right.asInstanceOf[Node.this.type].rotateRight)
      this.rotateLeft
      //this
    }
    case "RL" => {
      this.left.asInstanceOf[Node.this.type].rotateLeft
      this.rotateRight
      //this
    }
    case "L" => this.rotateRight
    case "R" => this.rotateLeft
    case _ => this
  }

  def contains(x: Int, str: String): (Boolean, String, Boolean) = {
    var bool = false //in set
    var f = false //splay
    var ans = ""
    if (x == item){
      //println("String at bottom: " + str)
      return (true, str, false)
    }
    else if (x < item) {
      println("hello")
      val (a, b, c) = left.contains(x, 'L' +: str)
      bool = a
      ans = b
      f = c
    }
    else {
      val (a, b, c) = right.contains(x, 'R' +: str)
      bool = a
      ans = b
     
      f = c
    }

    if (!bool) return (bool, str, f) //not in tree
      //in set
    if (!f && ans.length == 1) {
      splay(ans)
      (bool, "", f)
    }
    else if (!f)
      (bool, ans, !f) //don't need to edit
    else {
      //println("SPLAY")
      //println("String: " + ans)
      splay(ans.take(2)) //splay on first two
      (bool, ans.drop(2), !f)
     }
    }
  


  def rotateLeft: Node.this.type  = right match {
    case Empty => throw new Exception("trying to rotate an empty node")
    case Node(x, a, b) => {
      val cloneItem = item
      val cloneLeft = left
      this.left = Empty
      this.right = b
      this.item = x
      this.left = Node(cloneItem, cloneLeft, a)
      this
    }
  }
  def rotateRight: Node.this.type = left match {
    case Empty => throw new Exception("trying to rotate an empty node")
    case Node(x, a, b) => {
      val cloneItem = item
      val cloneRight = right
      this.right = Empty
      this.left = a
      this.item = x
      this.right = Node(cloneItem, b, cloneRight)
      this
    }
  }
  override def toString: String = {
    help(0)
  }
  def help(spaces: Int): String = {
    (" " * spaces + item + "\n") + left.help(spaces+1) + right.help(spaces+1)
  }
  
}

case object Empty extends SplayTree {
  def rotateLeft: Node = throw new Exception("Can't rotate empty tree")
  def rotateRight: Node = throw new Exception("Can't rotate empty tree")
  def contains(x: Int, str: String): (Boolean, String, Boolean) = (false, "", false)
  def splay(str: String): SplayTree = Empty
  def help(spaces: Int): String = " " * spaces + "X\n"
  override def toString: String = help(0)
}

def test(): Unit = {
  val test: SplayTree = Node(2, Node(1, Empty, Empty), Node(6, Node(4, Empty, Empty), Node(8,Empty,Empty)))
  val test2: SplayTree = Node(2, Node(1, Node(0, Empty, Empty), Node(1, Empty, Empty)), Node(6, Node(4, Node(-5, Empty, Empty), Node(-20, Empty, Empty)), Node(8,Node(-40, Empty, Empty), Node(-55,Empty, Empty))))
  val test3: SplayTree = Node(12, Node(5, Empty,Empty), Node(25, Node(20, Node(15, Node(13, Empty, Empty), Node(18, Node(16, Empty, Empty), Empty)), Node(24, Empty, Empty)), Node(30, Empty, Empty))) 
  val test4: SplayTree = Node(8, Node(7, Node(6, Node(5, Node(4, Node(3, Node(2, Node(1, Empty, Empty), Empty), Empty), Empty), Empty), Empty), Empty), Empty)
}
