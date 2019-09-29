import homegrown.collections._
import org.scalatest._

class SetSuite extends FunSuite with Matchers {
  test("apply on an empty Set should yield false") {
    Set.empty(randomString) shouldBe false
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

  test("union on empty Set should yield an empty Set") {
    Set.empty.union(Set.empty)(randomString) shouldBe false
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

    val leftUnion = left.union(right)

    leftUnion(a) shouldBe true
    leftUnion(b) shouldBe true
    leftUnion(d) shouldBe true
    leftUnion(c) shouldBe true

    val rightUnion = right.union(left)

    rightUnion(a) shouldBe true
    rightUnion(b) shouldBe true
    rightUnion(d) shouldBe true
    rightUnion(c) shouldBe true
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

    val leftIntersection = left.intersection(right)

    leftIntersection(a) shouldBe false
    leftIntersection(d) shouldBe false

    leftIntersection(c) shouldBe true
    leftIntersection(b) shouldBe true

    val rightIntersection = right.intersection(left)

    rightIntersection(a) shouldBe false
    rightIntersection(d) shouldBe false

    rightIntersection(c) shouldBe true
    rightIntersection(b) shouldBe true
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

    val leftDiffrence = left.difference(right)

    leftDiffrence(a) shouldBe true

    leftDiffrence(b) shouldBe false
    leftDiffrence(c) shouldBe false
    leftDiffrence(d) shouldBe false

    val rightDiffrence = right.difference(left)

    rightDiffrence(d) shouldBe true

    rightDiffrence(a) shouldBe false
    rightDiffrence(b) shouldBe false
    rightDiffrence(c) shouldBe false
  }

  test("isSubsetOf on empty Set should yield true") {
    pending

    Set.empty.isSubsetOf(Set.empty) shouldBe true
    Set.empty.isSubsetOf(Set.empty.add(randomString)) shouldBe true
  }

  private def randomString: String = scala.util.Random.alphanumeric.take(5).mkString
}
