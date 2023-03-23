package rimp

/**
 * A class that represents a node in a binary tree.
 *
 * @constructor Creates a new node with the given value and
 *              optional parent (default is None),
 *              left child (default is None),
 *              and right child (default is None).
 * @param value The integer value stored in the node.
 * @param parent An optional reference to the parent of the node.
 * @param left An optional reference to the left child of the node.
 * @param right An optional reference to the right child of the node.
 */
class Node(var value: Int, var parent: Option[Node] = None, var left: Option[Node] = None, var right: Option[Node] = None) {

  /**
   * Returns true if this node is a terminal node (it has no left or right child nodes).
   * Otherwise, returns false.
   *
   * @return true if this node is a terminal node, false otherwise.
   */
  def isTerminal: Boolean = left.isEmpty && right.isEmpty

  /**
   * Returns true if this node is a left child of its parent. Otherwise returns false.
   *
   * @return true if this node is a left child. false otherwise.
   */
  def isLeftChild: Boolean = this == parent.get.left.get

  /**
   * Returns a string representation of the tree rooted at this node.
   *
   * If the node is null, returns the string "Empty Node". Otherwise, returns a string that represents the
   * tree rooted at this node. If the node is a left child, the string will contain only the node's value.
   * If the node is terminal, the string will be "+(0, 0)".
   * Otherwise, the string will be "+(leftSubtree, rightSubtree)", where leftSubtree and rightSubtree are the
   * string representations of the left and right subtrees, respectively.
   *
   * @return A string representation of the tree rooted at this node.
   */
  override def toString: String = {
    if(this == null) "Empty Node"
    else if (isLeftChild)
      s"$value"
    else if (isTerminal)
      "+(0, 0)"
    else
    s"+(${left.get.toString}, ${right.get.toString})"
  }
}

