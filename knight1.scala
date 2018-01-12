// Part 1 about finding and counting Knight's tours
//==================================================

object CW7a {

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1a) Complete the function that tests whether the position 
//     is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path)(x: Pos) : Boolean = {
  !(x._1 < 0 || x._1 >= dim || x._2 < 0 || x._2 >= dim || path.contains(x))
}

//(1b) Complete the function that calculates for a position 
//     all legal onward moves that are not already in the path. 
//     The moves should be ordered in a "clockwise" manner.
 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val operations = List((1,2), (2,1), (2,-1), (1,-2), (-1, -2), (-2,-1), (-2,1), (-1,2))
  for(n <- operations
      if(is_legal(dim, path)((x._1 + n._1), (x._2 + n._2)))) yield ((x._1 + n._1), (x._2 + n._2)).asInstanceOf[Pos]
}

//some test cases
//
assert(legal_moves(8, Nil, (2,2)) ==
  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
assert(legal_moves(8, List((4,1), (1,0)), (2,2)) ==
  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


//(1c) Complete the two recursive functions below. 
//     They exhaustively search for knight's tours starting from the 
//     given path. The first function counts all possible tours, 
//     and the second collects all tours in a list of paths.

  def count_tours(dim: Int, path: Path): Int = {
    val moves = legal_moves(dim, path, path.head)
    val counts = for (n <- moves) yield count_tours(dim, n :: path)
    if(path.size == dim*dim) 1
    else if(moves.size == 0) 0
    else counts.foldLeft(0)(_ + _)
  }

  def enum_tours(dim: Int, path: Path) : List[Path] = {
    val moves = legal_moves(dim, path, path.head)
    val tours = for (n <- moves) yield enum_tours(dim, n :: path)
    if (path.size == dim*dim) List(path)
    else if(moves.size == 0) Nil
    else tours.flatten
  }


}
