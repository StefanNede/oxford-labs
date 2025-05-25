import org.scalatest.funsuite.AnyFunSuite
// 37 tests with 52 individual checks

class TestTest extends AnyFunSuite{ 
  var s: IntSet = new IntSet
  test("1. empty set toString") {
    s = new IntSet
    assert(s.toString == "{}") 
  }

  test("2. non-empty set toString") {
    // output in reverse order to how added
    s = IntSet(1,2,3)
    assert(s.toString == "{3, 2, 1}")
  }

  test("3. no duplicate adding function") {
    // output in reverse order to how added
    // no duplicates
    s = new IntSet; s.add(1); s.add(2); s.add(3)
    assert(s.toString == "{3, 2, 1}")
  }

  test("4. duplicate adding function") {
    // duplicates
    s = new IntSet; s.add(1); s.add(1)
    assert(s.toString == "{1}")
  }

  test("5. no duplicate size function") {
    // no duplicates
    s = IntSet(1,2,3,4,5,6,7,8,9,10)
    assert(s.size == 10)
  }

  test("6. duplicate size function") {
    // duplicates
    s = IntSet(1,1,1,1,1,1,2)
    assert(s.size == 2)
  }

  test("7. contains function") {
    s = IntSet(1,2,3,4,5,6,7,8,9,10)
    assert(s.contains(1) == true)
  }

  test("8. does not contain function") {
    s = IntSet(1,2,3,4,5,6,7,8,9,10)
    assert(s.contains(11) == false)
  }

  test("9. any function on non-empty set") {
    s = IntSet(1,2,3)
    assert(s.any == 1 || s.any == 2 || s.any == 3)
  }

  test("10. any function on empty set") {
    // empty set
    s = new IntSet
    intercept[IllegalArgumentException] { s.any }
  }

  test("11. equals on empty sets") {
    val s1 = new IntSet
    val s2 = new IntSet
    assert((s1 == s2) == true)
  }

  test("12. equals on non-empty sets that are equal") {
    val s3 = IntSet(1,2,3)
    val s4 = IntSet(3,2,1)
    assert((s3 == s4) == true)
  }

  test("13. equals on non-empty sets that are not equal") {
    val s5 = IntSet(1,2,3)
    val s6 = IntSet(4,2,1)
    assert((s5 == s6) == false )
  }

  test("14. equals on non equal size test") {
    val s7 = IntSet(1,2,3)
    val s8 = IntSet(1)
    assert((s7 == s8) == false)
  }

  test("15. remove an element that was not in the set") {
    s = IntSet(1,2,3,4,5)
    val initialSet = IntSet(1,2,3,4,5)
    assert(s.remove(6) == false)
    assert(s == initialSet)
  }

  test("16. remove an element that was in the set") {
    s = IntSet(1,2,3,4,5)
    val augmentedSet = IntSet(2,3,4,5)
    assert(s.remove(1) == true)
    assert(s == augmentedSet)
  }

  test("17. subset on empty set and non empty set") {
    s = new IntSet
    var superset = IntSet(1,2,3,4)
    assert(s.subsetOf(superset) == true)
    assert(superset.subsetOf(s) == false)
  }

  test("18. subset on 2 empty sets") {
    s = new IntSet
    var superset = new IntSet
    assert(s.subsetOf(superset) == true)
    assert(superset.subsetOf(s) == true)
  }

  test("19. subset on bigger set and smaller set") {
    s = IntSet(1,2,3,4,5)
    var smallerset = IntSet(1,2,3)
    assert(s.subsetOf(smallerset) == false)
  }

  test("20. subset on subset and superset") {
    s = IntSet(1,2)
    var superset = IntSet(1,2,3,5)
    assert(s.subsetOf(superset) == true)
  }

  test("21. subset on 2 equal sets") {
    s = IntSet(1,2,3)
    var superset = IntSet(1,2,3)
    assert(s.subsetOf(superset) == true)
    assert(superset.subsetOf(s) == true)
  }

  test("22. union on 2 equal sets") {
    s = IntSet(1)
    var otherset = IntSet(1)
    assert(s.union(otherset) == otherset.union(s))
    assert(s.union(otherset) == s)
  }

  test("23. union on empty set and non empty set") {
    s = IntSet(1,2,3)
    var emptySet = new IntSet
    assert(s.union(emptySet) == emptySet.union(s))
    assert(s.union(emptySet) == s)
  }

  test("24. union on 2 empty sets") {
    s = new IntSet
    var emptySet = new IntSet
    assert(s.union(emptySet) == emptySet.union(s))
    assert(s.union(emptySet) == s)
    assert(s.union(emptySet).toString == "{}")
  }

  test("25. union on 2 disjoint sets") {
    s = IntSet(1,2,3)
    var otherset = IntSet(4,5,6)
    var unionset = IntSet(1,2,3,4,5,6)
    assert(s.union(otherset) == otherset.union(s))
    assert(s.union(otherset) == unionset)
  }

  test("26. union on 2 different sets which are not disjoint") {
    s = IntSet(1,2,3)
    var otherset = IntSet(2,3,4)
    var unionset = IntSet(1,2,3,4)
    assert(s.union(otherset) == otherset.union(s))
    assert(s.union(otherset) == unionset)
  }

  test("27. intersect on 2 disjoint sets") {
    s = IntSet(1,2,3)
    var otherset = IntSet(4,5,6)
    var intersectset = new IntSet
    assert(s.intersect(otherset) == otherset.intersect(s))
    assert(s.intersect(otherset) == intersectset)
    assert(s.intersect(otherset).toString == "{}" )
  }

  test("28. intersect on 2 empty sets") {
    s = new IntSet
    var emptyset = new IntSet
    assert(s.intersect(emptyset) == emptyset.intersect(s))
    assert(s.intersect(emptyset).toString == "{}" )
  }

  test("29. intersect on 2 sets with overlap") {
    s = IntSet(1,2,3)
    var otherset = IntSet(2,3,4)
    var intersectset = IntSet(2,3)
    assert(s.intersect(otherset) == otherset.intersect(s))
    assert(s.intersect(otherset) == intersectset)
    assert(s.intersect(otherset).toString == "{2, 3}")
  }

  test("30. identity map on set") {
    s = IntSet(1,2,3,4)
    def f(x : Int) : Int = x
    assert(s.map(f) == s)
  }

  test("31. multiply values by 2 in a set") {
    s = IntSet(1,2,3,4)
    var targetset = IntSet(2,4,6,8)
    def f(x : Int) : Int = x*2
    assert(s.map(f) == targetset)
  }

  test("32. change all values to 0 in a set") {
    s = IntSet(1,2,3,4)
    var targetset = IntSet(0)
    def f(x : Int) : Int = 0
    assert(s.map(f) == targetset)
  }

  test("33. map on empty set") {
    s = new IntSet
    var targetset = new IntSet
    def f(x : Int) : Int = x+2
    assert(s.map(f) == targetset)
  }
  test("map to same number") {
    s = IntSet(1,2,3)
    def f(x : Int) = 2
    assert(s.map(f) == IntSet(2))
  }

  test("34. filter even numbers") {
    s = IntSet(1,2,3,4,5,6,7,8,9,10)
    var targetSet = IntSet(2,4,6,8,10)
    def p(x : Int) : Boolean = x%2 == 0
    assert(s.filter(p) == targetSet)
  }

  test("35. filter with constantly false predicate") {
    // should return an empty set
    s = IntSet(1,2,3,4,5,6,7,8,9,10)
    var emptySet = new IntSet
    def p(x : Int) : Boolean = false
    assert(s.filter(p) == emptySet)
  }

  test("36. filter with constantly true predicate") {
    // should return the initial set
    s = IntSet(1,2,3,4,5,6,7,8,9,10)
    def p(x : Int) : Boolean = true
    assert(s.filter(p) == s)
  }

  test("37. filter on empty set") {
    // should return empty set
    s = new IntSet
    var emptySet = new IntSet
    def p(x : Int) : Boolean = x == 1
    assert(s.filter(p) == emptySet)
  }
}
