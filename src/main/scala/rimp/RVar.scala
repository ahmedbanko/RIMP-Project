package rimp

class RVar(var root_in: Node = new Node(0)) {
  // default 0 means all variables in RIMP initially have the value 0
  private var stackSize = 1
  private var root = root_in

  def top: Node = root
  def size: Int = stackSize

  def push(newValue: Int): RVar = {
    val newRoot = new Node(newValue, right=Some(this.root))
    val newRoot_left = new Node(newValue-this.top.value, parent=Some(newRoot))
    newRoot.left = Some(newRoot_left)

    root.parent = Some(newRoot)
    stackSize+=1
    root = newRoot
    this
  }

  def pop: Int = {
    val out = root
   if(stackSize > 1) {
      root = root.right.get
      root.parent = None
      stackSize-=1
      out.value
    }
    else throw new Exception("Empty stack (RIMP stacks with only 0 are considered empty)")
  }

  override def toString: String = {
    if (stackSize > 1)
    s"(${root.value}, +(${root.left.get.toString}, ${root.right.get.toString})"
    else if (stackSize == 1)s"(${root.value}, +(0, 0))"
    else "None"
  }

  override def equals(obj: Any): Boolean = {
    (this.size == obj.asInstanceOf[RVar].size) &&
      (this.top.value == obj.asInstanceOf[RVar].top.value) &&
      (obj.toString == this.toString)
  }


}
