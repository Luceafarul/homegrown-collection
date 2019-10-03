package homegrown.collections

trait OldSet extends (String => Boolean) {
  final def add(input: String): Set = element => input == element || this(element)

  final def remove(input: String): Set = element => input != element && this(element)

  final def union(that: Set): Set = element => this(element) || that(element)

  final def intersection(that: Set): Set = element => this(element) && that(element)

  final def difference(that: Set): Set = element => this(element) && !that(element)

  final def isSubsetOf(that: Set): Boolean = ???
}

object OldSet {
  val empty: Set = input => false

  // val empty: Set = new Set {
  // override final def apply(input: String) = false
  // }
}
