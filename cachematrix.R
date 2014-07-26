## These functions calculate and store a matrix and it's inverse

## makeCacheMatrix creates a list of four functions that set the value
## of a matrix, get the value of a matrix, set the inverse value of a 
## a matrix and get the inverse value of a matrix

makeCacheMatrix <- function(x = matrix()) {  # creates function 'makeCacheMatrix' that takes a matrix argument x
        m <- NULL  # sets an object m equal to null
        set <- function(y) {  # creates a funtion 'set' which stores value of 
               x <<- y        # of matrix x and sets object m = null
               m <<- NULL
        }
        get <- function() x  # creates a function 'get' which returns matrix x
        setinverse <- function(solve) m <<- solve  # creates a function 'setinverse' which stores value m (from cacheSolve)
        getinverse <- function() m  # creates a function 'getinverse' which returns m
        list(set = set, get = get,   # creates a named list of functions set, get, setinverse & getinverse
             setinverse = setinverse, 
             getinverse = getinverse)
}

## cacheSolve uses the 'solve' function in R to calculate the inverse 
## of a matrix but first it checks to see if the inverse has already
## been calculated

cacheSolve <- function(x, ...) {  # creates a function 'cacheSolve' which takes at least one argument x
        m <- x$getinverse()  # sets object 'm' equal to the value of function 'getinverse' with argument x
        if(!is.null(m)) {  # checks to see if m is not equal to null
                message("getting cached data")  # if not then returns the message 'getting cached data'
                return(m)  # returns value of m
        }
        data <- x$get()  # sets object 'data' equal to the value of function 'get' with argument x
        m <- solve(data, ...)  # sets object 'm' equal to the inverse of 'data'
        x$setinverse(m)  # uses the function 'setinverse' to store m in 'makeCacheMatrix'
        m  # returns m, an inverse matrix of x
}
