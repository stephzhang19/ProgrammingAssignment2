## List with a function that sets the value of a matrix 
## and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y){
    x<<-y
    inv <<-NULL
  }
  get <- function()x
  setInv<- function(inverse) inv <<- inverse
  getInv <-function() inv
  list(set = set, get= get, setInv=setInv, getInv=getInv)

}

## Checks if inverse has been calculated, otherwise calculates
## and caches the inverse value

cacheSolve <- function(x, ...) {
  inv<-x$getInv()
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setInv(inv)
  inv
}
