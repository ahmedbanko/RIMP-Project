package rimp

class Node(var value: Int, var parent: Option[Node] = None, var left: Option[Node] = None, var right: Option[Node] = None) {
  def isTerminal: Boolean = left.isEmpty && right.isEmpty
  def isLeftChild: Boolean = this == parent.get.left.get

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

