## These two functions implement a caching mechanism for the calculation of the inverse of a matrix

## This function implements gets and sets functions for a matrix object and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function returns a matrix that is the inverse of 'x' using the caching mechanism implemented
## in the above function (the cache is sent as arg in this function). First it checks whether the inverse of the matrix already exists (getinverse).
## If it exists return the obtained valu thus avoiding the calculation. If the getInverse() returns a null 
## value, then the function calculates the inverse, stores the result using the setinverse function and 
## returns the inverse of the original matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


####To test this submission, source the functions above and run the following code

# Creating a squared matrix
seq1 <- seq(1:4)
mat1 <- matrix(seq1, 2)
mat1
#Calculating the inverse without the caching mechanism
solve(mat1)

#Checking get functions before calling the cacheSolve function
cache <- makeCacheMatrix(mat1)
#to get the matrix
cache$get()
#to get the cached inverse (null at this time)
cache$getinverse()

#Invoking the cacheSolve method to obtain the inverse using the caching mechanism
inversemat1 <- cacheSolve(cache)
inversemat1
