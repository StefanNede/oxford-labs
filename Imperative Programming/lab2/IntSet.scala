// A class of objects to represent a set

// Use a dummy header node: yes so adding to the set is easier
// Avoid repetitions as this is a set
// Won't store the elements in increasing order as sets aren't sorted and order for the toString method doesn't matter
// Include anything else in your state: 

class IntSet {
  // State: S : P(Int) (where "P" represents power set)

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)

  // Init: S = {dummy header node}
  private var theSet : Node = Node(0, null) // or however empty set is represented

  /** Convert the set to a string.
    * (The given implementation is not sufficient.) */
  override def toString : String = {
    // O(n) where n is the size of the set
    var cardinality = size
    if (cardinality == 0) {
      "{}"
    } 
    else {
      var res: String = "{"
      var currentNode: Node = theSet.next // exclude dummy header
      // Invariant: res includes all nodes in the set excluding dummy header node
      while (currentNode.next != null) {
        res += currentNode.datum + ", "
        currentNode = currentNode.next
      }
      // res includes all nodes but the last one
      res + currentNode.datum + "}" // res includes all nodes
    }
  }

  /** Return the node before the one containing int.
    * Post: set = set_0 && returns n s.t. n in L(list) &&
    * (n.next.datum = el or n.next=null if no such Node exists)*/
  private def find(el:Int) : Node = {
    // O(n)
    var n = theSet
    // Invariant: el does not appear in the nodes up to and  
    // including n; i.e., 
    // for all n1 in L(theSet.next, n.next), n1.datum != el 
    while(n.next != null && n.next.datum != el) n = n.next
    n
  }

  /** Add element e to the set
    * Post: S = S_0 U {e} */
  def add(e: Int) : Unit = {
    // O(n) due to find
    // need to check if element e already in set
    val n = find(e)
    if (n.next == null) { // store new info in current list header
      theSet.datum = e 
      var newHead: Node = Node(0, theSet)
      theSet = newHead
    }
  }

  /** Length of the list
    * Post: S = S_0 && returns #S */
  def size : Int = {
    // O(n)
    var n = 0
    var previousNode: Node = theSet.next
    while (previousNode != null) {
      n += 1
      previousNode = previousNode.next
    }
    n
  }

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
  // O(n) due to find
  def contains(e: Int) : Boolean = find(e).next != null

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  def any : Int = {
    require(size != 0)
    theSet.next.datum // return first element
  }

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  override def equals(that: Any) : Boolean = that match {
    // O(n^2) due to calling contains in the loop
    case s: IntSet => {
      if (size != s.size) { false }
      else if (size == 0 && s.size == 0) { true }
      else {
        var previousNode: Node = theSet.next
        var found = true 
        // Invariant: datum appears in other set in the nodes up to but not including previousNode
        while (previousNode!= null) {
          found = found && s.contains(previousNode.datum)
          previousNode = previousNode.next
        }
        found
      }
    }
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: Int) : Boolean = {
    // O(n)
    val n = find(e)
    if (n.next == null) { // element doesn't exist
      false
    } else {
      n.next = n.next.next // node to be deleted
      true
    }
  }
    
  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  def subsetOf(that: IntSet) : Boolean = {
    // O(n^2) due to calling contains in the loop
    // S needs to have a size <= that.S to be a subset
    if (size == 0) {
      true
    } else if (size > that.size) {
      false
    } else {
      // go over each element in S and check that that.S contains it
      var flag = true 
      var previousNode: Node = theSet.next
      // Invariant: datum appears in other set in the nodes up to but not including previousNode
      while (previousNode != null) {
        flag = flag && that.contains(previousNode.datum)
        previousNode = previousNode.next
      }
      flag 
    }
  }

  // ----- optional parts below here -----

  /** return union of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  def union(that: IntSet) : IntSet = {
    // O(n^2) due to add calling find
    val s = this
    var previousNode: Node = that.theSet.next // skip dummy node
    // Invariant: s contains all nodes up to but not including previousNode
    while (previousNode != null) {
      // add previousNode to s 
      s.add(previousNode.datum) // won't share nodes due to add function
      previousNode = previousNode.next
    }
    s
  }

  /** return intersection of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersect(that: IntSet) : IntSet = {
    // O(n^2) 
    val s = new IntSet
    // go over nodes in S and check if they are in that and then add them to result set
    var previousNode = theSet.next // skip dummy header node
    // Invariant: s contains previousNode if that.contains(previousNode) for all previousNode from theSet(start, previousNode) (not including previousNode)
    while (previousNode != null) {
      if (that.contains(previousNode.datum)) {
        s.add(previousNode.datum)
      }
      previousNode = previousNode.next
    }
    s
  }

  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  def map(f: Int => Int) : IntSet = {
    // O(n^2) due to add taking O(n) time
    val s = new IntSet
    var previousNode = theSet.next // skip dummy node
    // Invariant: s contains f(x) for all x belonging to s from start up to but not including previousNode
    while (previousNode != null) {
      s.add(f(previousNode.datum))
      previousNode = previousNode.next
    }
    s
  }

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : Int => Boolean) : IntSet = {
    // O(n^2) due to add taking O(n) time
    val s = new IntSet
    var previousNode = theSet.next // skip dummy node
    // Invariant: s contains x for all x belonging to s from start up to but not including previousNode
    //            if p(x) satisfied
    while (previousNode != null) {
      if (p(previousNode.datum)) s.add(previousNode.datum)
      previousNode = previousNode.next
    }
    s
  }
}


// The companion object
object IntSet{
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node)

  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined 
    * the main constructor and the add operation. 
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }
}
