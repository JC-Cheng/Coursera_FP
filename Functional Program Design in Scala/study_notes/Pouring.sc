class Pouring(capacity: Vector[Int]){

  //state
  type State = Vector[Int]
  val initialState: Vector[Int] = capacity map(_ => 0)

  //move
  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State): State = state updated(glass, 0)
    override def toString: String = s"Empty(${capacity(glass)})"
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State): State = state updated(glass, capacity(glass))
    override def toString: String = s"Fill(${capacity(glass)})"
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
    override def toString: String = s"Pour(${capacity(from)} -> ${capacity(to)})"
  }

  val glasses: Seq[Int] = capacity.indices
  val moves: Seq[Move]  =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  //path
  class Path(history: List[Move], val endState: State) {
    //def endState: State = (history foldRight initialState) (_ change  _)
    /*def endState: State = trackState(history)
    private def trackState(xs: List[Move]): State = xs match {
      case Nil => initialState
      case move::xs1 => move.change(trackState(xs1))
    }
    */
    def extend(move: Move) = new Path(move::history, move change endState)
    override def toString: String = (history.reverse mkString " ") + "=> " + endState
  }

  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
    if (paths.isEmpty) Stream.Empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next

      paths #:: from(more, explored ++ (more map(_.endState)))
    }
  }

  val pathSets = from(Set(initialPath), Set(initialState))

  def solution(target: Int): Stream[Path] = {
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
  }
}

val prob = new Pouring(Vector(4, 9))

prob.moves
prob.pathSets.take(3).toList
prob.solution(6)
prob.solution(13)


