## 1.  set the value of the Matrix
## 2.  get the value of the Matrix
## 3.  set the value of the Inverse
## 4.  get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  
            inv <- NULL
            set <- function(y){
              x <<- y
              inv <<- NULL
            }
            get <- function() x
            setinv <- function(solve) inv <<- solve
            getinv <- function() inv
            list(set = set, get =get, 
                 setinv = setinv, getinv = getinv)
}

## Function that inverse the cached matrix

cacheSolve <- function(x, ...) {
            inv <- x$getinv()
            if(!is.null(inv)){
                  message("Getting cached data")
                  return(inv)
            }
            data <- x$get()
            inv <- solve(data)
            x$setinv(inv)
            inv
            
}
