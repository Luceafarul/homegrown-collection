import homegrown.collections._
import org.scalatest._

class SetSuite extends FunSuite with Matchers {
  test("apply on an empty Set should yield false") {
    Set.empty(randomString) shouldBe false
    Set.empty.size shouldBe 0
  }

  test("add on an empty Set should yield a new Set with one element") {
    val first = randomString
    val second = randomString

    val set = Set.empty.add(first)

    set(first) shouldBe true
    set(second) shouldBe false
  }

  test("add on a non empty Set should yield a new Set with two elements") {
    val first = randomString
    val second = randomString

    first should not be second

    val set = Set.empty.add(first).add(second)

    set(first) shouldBe true
    set(second) shouldBe true
  }

  test("remove on an empty Set should yield an empty Set") {
    val emlement = randomString
    val stillEmpty = Set.empty.remove(randomString)

    stillEmpty(randomString) shouldBe false
  }

  test("remove on a non empty Set should yield a new Set without the element") {
    val element = randomString
    val setWithElement = Set.empty.add(element)

    setWithElement(element) shouldBe true

    val setWithoutElement = setWithElement.remove(element)

    setWithElement(element) shouldBe true
    setWithoutElement(element) shouldBe false
  }

  test("remove removes only the element in question") {
    val first = randomString
    val second = randomString

    val setWithElements = Set.empty.add(first).add(second)

    setWithElements(first) shouldBe true
    setWithElements(second) shouldBe true

    val setWithoutFirstElement = setWithElements.remove(second)

    setWithoutFirstElement(second) shouldBe false
    setWithoutFirstElement(first) shouldBe true
  }

  test("remove removes only the element in question 2") {
    val first = randomString
    val second = randomString

    val setWithElements = Set.empty.add(first).add(second)

    setWithElements(first) shouldBe true
    setWithElements(second) shouldBe true

    val setWithoutFirstElement = setWithElements.remove(first)

    setWithoutFirstElement(first) shouldBe false
    setWithoutFirstElement(second) shouldBe true
  }

  test("add/remove combo should ensure that all elements are distinct") {
    val element = randomString

    val set = Set.empty.add(element).add(element).remove(element)

    set(element) shouldBe false
  }

  test("union on empty Set should yield an empty Set") {
    Set.empty.union(Set.empty) shouldBe Set.empty
  }

  test("union on a non empty Set with an empty Set should yeild the original Set untouched") {
    val first = randomString
    val second = randomString

    first should not be second

    val emptySet = Set.empty
    val nonEmptySet = emptySet.add(first).add(second)

    emptySet.union(nonEmptySet)(first) shouldBe true
    emptySet.union(nonEmptySet)(second) shouldBe true

    nonEmptySet.union(emptySet)(first) shouldBe true
    nonEmptySet.union(emptySet)(second) shouldBe true
  }

  test("union on two no empty Sets should yield their union") {
    val a = randomString
    val b = randomString
    val d = randomString
    val c = randomString

    val left = Set.empty.add(a).add(b)
    val right = Set.empty.add(c).add(d)

    left.union(right) shouldBe Set.empty.add(a).add(b).add(c).add(d)
    right.union(left) shouldBe Set.empty.add(a).add(b).add(c).add(d)
  }

  test("intersection on empyt Set should yield an empty Set") {
    Set.empty.intersection(Set.empty)(randomString) shouldBe false
  }

  test("intersection on a non empty Set with an empty Set should yeild an empty Set") {
    val first = randomString
    val second = randomString

    first should not be second

    val emptySet = Set.empty
    val nonEmptySet = emptySet.add(first).add(second)

    emptySet.intersection(nonEmptySet)(first) shouldBe false
    emptySet.intersection(nonEmptySet)(second) shouldBe false

    nonEmptySet.intersection(emptySet)(first) shouldBe false
    nonEmptySet.intersection(emptySet)(second) shouldBe false
  }

  test("intersection on two no empty Sets should yield their intersection") {
    val a = randomString
    val b = randomString
    val d = randomString
    val c = randomString

    val left = Set.empty.add(a).add(b).add(c)
    val right = Set.empty.add(c).add(d).add(b)

    left.intersection(right) shouldBe Set.empty.add(c).add(b)
    right.intersection(left) shouldBe Set.empty.add(c).add(b)
  }

  test("difference on emtpy Set should yield an empty Set") {
    Set.empty.difference(Set.empty)(randomString) shouldBe false
  }

  test("differene on a non empty Set with an empty Set should yeild an empty Set") {
    val first = randomString
    val second = randomString

    first should not be second

    val emptySet = Set.empty
    val nonEmptySet = emptySet.add(first).add(second)

    emptySet.difference(nonEmptySet)(first) shouldBe false
    emptySet.difference(nonEmptySet)(second) shouldBe false

    nonEmptySet.difference(emptySet)(first) shouldBe true
    nonEmptySet.difference(emptySet)(second) shouldBe true
  }

  test("difference on two no empty Sets should yield their difference") {
    val a = randomString
    val b = randomString
    val d = randomString
    val c = randomString

    val left = Set.empty.add(a).add(b).add(c)
    val right = Set.empty.add(c).add(d).add(b)

    left.difference(right) shouldBe Set.empty.add(a)
    right.difference(left) shouldBe Set.empty.add(d)
  }

  test("isSubsetOf on empty Set should yield true") {
    Set.empty.isSubsetOf(Set.empty) shouldBe true
    Set.empty.isSubsetOf(Set.empty.add(randomString)) shouldBe true
  }

  test("isSubsetOf on itself Set should yield true") {
    val set = Set.empty.add(randomString)

    set.isSubsetOf(set) shouldBe true
  }

  test("isSubsetOf on non empty Set should yield true if subset exist") {
    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val set = Set.empty.add(a).add(b).add(c).add(d)
    val subSet = Set.empty.add(b).add(c)

    subSet.isSubsetOf(set) shouldBe true
    set.isSubsetOf(subSet) shouldBe false
  }

  test("isSubsetOf on non empty Set should yield false if subset non exist") {
    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val set = Set.empty.add(a).add(b).add(c)
    val subSet = Set.empty.add(d).add(d)

    set.isSubsetOf(subSet) shouldBe false
    subSet.isSubsetOf(set) shouldBe false
  }

  test("isSupersetOf on empty Set should yield true") {
    Set.empty.isSupersetOf(Set.empty) shouldBe true
    Set.empty.isSupersetOf(Set.empty.add(randomString)) shouldBe true
  }

  test("isSupersetOf on itself Set should yield true") {
    val set = Set.empty.add(randomString)

    set.isSupersetOf(set) shouldBe true
  }

  test("isSupersetOf on non empty Set should yield true if subset exist") {
    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val set = Set.empty.add(a).add(b).add(c).add(d)
    val superSet = Set.empty.add(b).add(c)

    superSet.isSupersetOf(set) shouldBe false
    set.isSupersetOf(superSet) shouldBe true
  }

  test("isSupersetOf on non empty Set should yield false if subset non exist") {
    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val set = Set.empty.add(a).add(b).add(c)
    val superSet = Set.empty.add(d).add(d)

    set.isSupersetOf(superSet) shouldBe false
    superSet.isSupersetOf(set) shouldBe false
  }

  test("hashCode on an empty Set should not be random") {
    Set.empty.hashCode shouldBe Set.empty.hashCode
    val element = randomString
    Set.empty.add(element) shouldBe Set.empty.add(element)
    Set.empty.add(randomString) should not be Set.empty.add(randomString)
  }

  test("hashCode on an empty Set should not be 0") {
    Set.empty should not be 0
  }

  test("hashCode on a non empty Set should be the sum of all the hashCodes and the hashCode of the empty Set") {
    val empty = Set.empty
    val first = randomString
    val second = randomString

    val expected = empty.hashCode + first.hashCode + second.hashCode
    val actual = empty.add(first).add(second).hashCode

    expected shouldBe actual
  }

  test("size on an empty Set should be 0") {
    Set.empty.size shouldBe 0
  }

  test("size on a non emtpy Set should be 1") {
    Set.empty.add(randomString).size shouldBe 1
  }

  test("size on a non empty Set with 2 distinct elements should be 2") {
    val first = randomString
    val second = randomString

    first should not be second

    Set.empty.add(first).add(second).size shouldBe 2
  }

  test("size on a non empty Set with 2 equals element should be 1") {
    val element = randomString

    Set.empty.add(element).add(element).size shouldBe 1
  }

  test("isEmpty on an empty Set should yield true") {
    Set.empty.isEmpty shouldBe true
    Set.empty.nonEmpty shouldBe false
  }

  test("isEmpty on a non empty Set should yield false") {
    Set.empty.add(randomString).isEmpty shouldBe false
    Set.empty.add(randomString).nonEmpty shouldBe true
  }

  test("isSingleton on an empty Set should yield false") {
    Set.empty.isSingleton shouldBe false
  }

  test("isSingleton on a Set with more than one element should yield false") {
    val first = randomString
    val second = randomString

    first should not be second

    Set.empty.add(first).add(second).isSingleton shouldBe false
  }

  test("isSingleton on a Set with a single element should yield true") {
    Set.empty.add(randomString).isSingleton shouldBe true
  }

  test("sample should yield a random element from the Set") {
    Set.empty.sample shouldBe None

    val a = randomString

    Set.empty.add(a).sample shouldBe Some(a)

    val b = randomString

    Set.empty.add(a).add(b).sample should contain oneOf (a, b)
  }

  private def randomString: String = scala.util.Random.alphanumeric.take(5).mkString
}
