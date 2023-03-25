package rimp

/**
 * A class that represent variables in RIMP designed using a
 * tree data structure-based stack. All variables in RIMP are
 * initialised to have a value of 0 by default, and their initial size is 1.
 */

class RVar {
  // All variables in RIMP initially have the value 0.
  private var stackSize = 1
  private var root = new Node(0)

  /**
   * Returns the top node in the stack (root of the tree)
   * which stores the latest value of the variable.
   *
   * @return The root node of the tree which represents the top of the stack.
   */
  def top: Node = root

  /**
   * Returns the size of the stack.
   *
   * @return The size of the stack.
   */
  def size: Int = stackSize

  /**
   * Pushes a new node to the stack (new root for the tree).
   *
   * @param newValue The new node to be added onto the stack (new root for the tree).
   * @return The stack to which the new node was pushed.
   */
  def push(newValue: Int): RVar = {
    val newRoot = new Node(newValue, right=Some(this.root))
    val newRoot_left = new Node(newValue-this.top.value, parent=Some(newRoot))
    newRoot.left = Some(newRoot_left)

    root.parent = Some(newRoot)
    stackSize+=1
    root = newRoot
    this
  }

  /**
   * Removes the top node (root of the tree) and returns its value.
   * Replaces the root by its right child, and decrements the size of the stack.
   *
   * @throws Exception if the stack is empty. (only has a terminal node with value 0).
   * @return The value stored in the removed node.
   *
   */
  def pop: Int = {
    val out = root
   if(stackSize > 1) {
      root = root.right.get
      root.parent = None
      stackSize-=1
      out.value
    }
    else throw new Exception("Empty stack: (RIMP stacks with only 0 are considered empty)")
  }

  /**
   * Returns a string representation of the stack in the form
   * (root_value, +(left_value, right_value)) if the stack is not empty,
   * otherwise it returns '(0, +(0, 0))'.
   *
   * @return The string representation of the stack.
   */
  override def toString: String = {
    if (stackSize > 1)
    s"(${root.value}, +(${root.left.get.toString}, ${root.right.get.toString}))"
    else if (stackSize == 1)s"(0, +(0, 0))"
    else "None"
  }

  /**
   * Returns true if the given RVar object has the same size,
   * the same value and the same string representation with this RVar object.
   * Otherwise it returns false.
   *
   * @param obj The given RVar object to compare with this object.
   * @return true if the given RVar object has the same size,
   *         the same value and the same string representation with this RVar object.
   *         Otherwise it returns false.
   */
  override def equals(obj: Any): Boolean = {
    (this.size == obj.asInstanceOf[RVar].size) &&
      (this.top.value == obj.asInstanceOf[RVar].top.value) &&
      (obj.toString == this.toString)
  }


}
