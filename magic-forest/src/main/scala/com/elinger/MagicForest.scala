package com.elinger

import scala.annotation.tailrec
import scala.collection.Set

object MagicForest {

  case class State(pre: Option[State], goats: Int, wolfs: Int, lions: Int) {

    override def equals(obj: Any): Boolean =
      obj match {
        case o: State => o.goats == this.goats && o.wolfs == this.wolfs && o.lions == this.lions
        case _ => false
      }

    override def hashCode(): Int = {
      var hash = 7
      hash = 31 * hash + this.goats.hashCode()
      hash = 31 * hash + this.wolfs.hashCode()
      hash = 31 * hash + this.lions.hashCode()
      hash
    }

    override def toString: String = s"($goats, $wolfs, $lions)"
  }

  def evolution(initialState: State): State = {

    def wolfEeatsGoat(state: State): Option[State] =
      state match {
        case State(_, g, w, l) if g > 0 && w > 0 => Some(State(Some(state), g - 1, w - 1, l + 1))
        case _ => None
      }

    def lionEeatsGoat(state: State): Option[State] =
      state match {
        case State(_, g, w, l) if g > 0 && l > 0 => Some(State(Some(state), g - 1, w + 1, l - 1))
        case _ => None
      }

    def lionEeatsWolf(state: State): Option[State] =
      state match {
        case State(_, g, w, l) if l > 0 && w > 0 => Some(State(Some(state), g + 1, w - 1, l - 1))
        case _ => None
      }

    @tailrec
    def play(states: Set[State]): Set[State] = {
      // All final states will be kept so that it can be determined which scenario has the largest population
      // at the end.
      val finalStates = states.filter(isFinalState)
      // Multiple scenarios might lead to the same state. Since hash sets are used, duplicated states will be
      // eliminated to speed up computation. See also how hashCode and equals are defined for State.
      val newStates = states.flatMap(nextStates)
      if (newStates.nonEmpty) play(finalStates ++ newStates) else states
    }

    def nextStates(state: State): Set[State] =
      Set(
        wolfEeatsGoat(state),
        lionEeatsWolf(state),
        lionEeatsGoat(state)
      ).flatten

    def isFinalState(state: State): Boolean =
      state match {
        case State(_, 0, 0, _) => true
        case State(_, 0, _, 0) => true
        case State(_, _, 0, 0) => true
        case _ => false
      }

    val scenarios = play(Set(initialState))
    scenarios.maxBy(l => l.goats + l.wolfs + l.lions)
  }

  def recreateHistory(state: State): List[String] =
    state.pre match {
      case Some(State(_, g, w, _)) if g == state.goats + 1 && w == state.wolfs + 1 =>
        s"Wolf eats goat. $state" :: state.pre.map(recreateHistory).getOrElse(List())
      case Some(State(_, g, _, l)) if g == state.goats + 1 && l == state.lions + 1 =>
        s"Lion eats goat. $state" :: state.pre.map(recreateHistory).getOrElse(List())
      case Some(State(_, _, w, l)) if w == state.wolfs + 1 && l == state.lions + 1 =>
        s"Lion eats wolf. $state" :: state.pre.map(recreateHistory).getOrElse(List())
      case _ => List()
    }

  def main(args: Array[String]): Unit = {
    val s = System.currentTimeMillis()
    val initialState = State(None, 17, 55, 6)
    println(s"Initial state = Goats: ${initialState.goats}, Wolfs: ${initialState.wolfs}, Lions: ${initialState.lions}")
    val state = evolution(initialState)
    println(s"Time: ${System.currentTimeMillis() - s} ms")
    recreateHistory(state).reverse.zipWithIndex.foreach(t => println(s"${t._2 + 1} ${t._1}"))
    println(s"Final state = Goats: ${state.goats}, Wolfs: ${state.wolfs}, Lions: ${state.lions}")
  }

}
