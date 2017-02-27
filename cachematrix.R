#creates a matrix
makeMatrix <- function(x = matrix(), ...){
  SolvedCache <- NULL

  setMatrix <- function(y = matrix()){
    x <<- y
    SolvedCache <<- NULL
  }

  getMatrix <- function() {x}
  setSolvedMatrix <- function(solve) {
    SolvedCache <- solve
  }
  getSolvedMatrix <- function(){
    SolvedCache
  } 
    list(set = setMatrix, get = getMatrix, setSolve = setSolvedMatrix, getSolve = getSolvedMatrix)
}
#cache an exisiting solved inverse matrix, or solves the inverse from an input matrix.
cacheSolvedMatrix <- function(x, ...){
  SolvedCache <- x$getSolve()
  if(!is.null(SolvedCache))
  {
    message("getting cached matrix")
    return(SolvedCache)
  }
  data <- x$get()
  SolvedCache <- solve(data, ...)
  x$setSolve(SolvedCache)
  SolvedCache
}