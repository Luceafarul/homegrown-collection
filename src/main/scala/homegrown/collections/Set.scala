package homegrown.collections

trait Set extends (String => Boolean) {
  def add(input: String): Set

  def remove(input: String): Set

  def union(that: Set): Set

  def intersection(that: Set): Set

  def difference(that: Set): Set

  def isSubsetOf(that: Set): Boolean

  def isSupersetOf(that: Set): Boolean

  def size: Int

  def isEmpty: Boolean = this == Set.empty

  def nonEmpty: Boolean = !isEmpty

  def isSingleton: Boolean

  def sample: Option[String]

  override def equals(other: Any): Boolean = other match {
    case that: Set => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _         => false
  }
}

object Set {
  private final case class NonEmpty(element: String, otherElements: Set) extends Set {
    final override def apply(input: String): Boolean = input == element || otherElements(input)

    def add(input: String): Set = if (input == element) this else NonEmpty(input, otherElements.add(element))

    def remove(input: String): Set = if (input == element) otherElements else NonEmpty(element, otherElements.remove(input))

    def union(that: Set): Set = otherElements.union(that.add(element))

    def intersection(that: Set): Set = {
      // if (that(element)) NonEmpty(element, otherElements.intersection(that))
      if (that(element)) otherElements.intersection(that).add(element)
      else otherElements.intersection(that)
    }

    def difference(that: Set): Set = {
      if (that(element)) otherElements.difference(that)
      else otherElements.difference(that).add(element)
    }

    def isSubsetOf(that: Set): Boolean = that(element) && otherElements.isSubsetOf(that)

    def isSupersetOf(that: Set): Boolean = that.isSubsetOf(this)

    def size: Int = 1 + otherElements.size

    def isSingleton: Boolean = otherElements.isEmpty

    def sample: Option[String] = Some(element)

    override def hashCode = element.hashCode + otherElements.hashCode
  }

  private object Empty extends Set {
    def apply(input: String): Boolean = false

    def add(input: String): Set = NonEmpty(input, Empty)

    def remove(input: String): Set = this

    def union(that: Set): Set = that

    def intersection(that: Set): Set = this

    def difference(that: Set): Set = this

    def isSubsetOf(that: Set): Boolean = true

    def isSupersetOf(that: Set): Boolean = true

    def size: Int = 0

    def isSingleton: Boolean = false

    def sample: Option[String] = None
  }

  val empty: Set = Empty
}
