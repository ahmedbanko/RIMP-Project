package rimp

class RVar(var root_in: Node = new Node(0)) {
  private var size = 1
  private var root = root_in

  def top: Node = root

  def push(newValue: Int): Unit = {
    val newRoot = new Node(newValue, right=Some(this.root))
    val newRoot_left = new Node(newValue-this.top.value, parent=Some(newRoot))
    newRoot.left = Some(newRoot_left)

    root.parent = Some(newRoot)
    size+=1
    root = newRoot
  }

  def pop: Int = {
    val out = root
    if (size == 1) {
      size -= 1
      0
    } else if(size > 1) {
      root = root.right.get
      size-=1
      out.value
    }
    else throw new Exception("Empty stack")
  }

  override def toString: String = {
    if (size > 1)
    s"(${root.value}, +(${root.left.get.toString}, ${root.right.get.toString})"
    else if (size == 1)s"(${root.value}, +(0, 0))"
    else "None"
  }


}
