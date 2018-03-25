## This function creates a special "matrix" object that can its inverse
## Important: The matrix that will be inputted in this function should be invertible
## otherwise the function will not work.

makeCacheMatrix <- function(x = matrix()){    # initializing the function
  inv <- NULL                                 # is set to null, because it will be used by later code
  set <- function(y){                         # function to assign later different values to matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x                         # defining the get function which will take the argument of the matrix                  
  setinverse<- function(inverse) inv <<- inverse #assigns the input argument to the value of inv in the parent environment
  getinverse <- function() inv                # retrieves the value of inv
  list(set = set, get = get ,                 # naming the the elements so later we can use $ form of the extract operator
       setinverse = setinverse ,
       getinverse = getinverse)
}

## The function that solves for the inverse of the matrix

cacheSolve <- function(x, ...){                    
  inv <-x$getinverse()                         # calls the getinverse() function on the input objetct
  if(!is.null(inv)){                           # if it is not equal to null, it takes the cached mean and returns it to parent environment
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                              # if it is false it calculates the inverse
  inv <- solve(data, ...)                      # clculates the inver of the function
  x$setinverse(inv)
  inv
}


