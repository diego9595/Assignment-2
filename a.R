makeCacheMatrix <- function(x = matrix()){
      a <- NULL
    set <- function(y){
            x <<- y 
            a <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {a <<- inverse}
      getInverse <- function() {a} 
      list(set =set, get = get,
           setInverse = setInverse,
           getInverse =getInverse)
}


cachesolve <- function(x, ...){
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}
