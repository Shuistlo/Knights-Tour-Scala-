
// Part 2 about finding a single tour for a board
//================================================

// copy any function you need from file knight1.scala

object CW7b {

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions


//(2a) Implement a first-function that finds the first 
//     element, say x, in the list xs where f is not None. 
//     In that case Return f(x), otherwise None. If possible,
//     calculate f(x) only once.
def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
  if(xs == Nil) None
  else if(f(xs.head) != None) f(xs.head)
  else first(xs.drop(1), f)
}

//(2b) Implement a function that uses the first-function for
//     trying out onward moves, and searches recursively for a
  //     knight tour on a dim * dim-board.

def is_legal(dim: Int, path: Path)(x: Pos) : Boolean = {
  !(x._1 < 0 || x._1 >= dim || x._2 < 0 || x._2 >= dim || path.contains(x))
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val operations = List((1,2), (2,1), (2,-1), (1,-2), (-1, -2), (-2,-1), (-2,1), (-1,2))
  for(n <- operations
      if(is_legal(dim, path)((x._1 + n._1), (x._2 + n._2)))) yield ((x._1 + n._1), (x._2 + n._2)).asInstanceOf[Pos]
}

  def first_tour(dim: Int, path: Path): Option[Path] = {
    def f(pos: Pos): Option[Path] = {
      first_tour(dim, pos :: path)
    }
    if (path.size == dim*dim){
      Some(path)
    } else{
      val moves = legal_moves(dim, path, path.head)
      if(moves.size==0) None
      else first(moves,f)
    }
  }
 

}
