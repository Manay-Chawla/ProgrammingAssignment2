## It's always beneficial to cache the results which help us reduce the computation cost.
## Following functions are an attempt to cache the inverse of a matrix.

## makeCacheMatrix Function takes a parameter x that is the input matrix whose inverse has to be returned.
# It has various functions to
# -> set value of the matrix
# -> get value of the matrix
# -> set value of inverse of the matrix
# -> getvalue of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve Function
## NOTE-> This function assumes that the matrix is invertible.
# returns the inverse of the matrix.
# It is first checked whether the inverse of the given matrix is already computed.
# If there's a cache value it will return it and if not computation is carried out and the value is setin the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
         if(!is.null(inv)) {
        message("getting the cached inverse matrix.")
        return(inv)
         }
    output <- x$get()
    inv <- solve(output)
    x$setinverse(inv)
    inv
}

## Example
# > x = rbind(c(1, 2), c(3, 4))
# > m = makeCacheMatrix(x)
# > m$get()
#       [,1]  [,2]
# [1,]   1     2
# [2,]   3     4  

## No cache in the first run
# > cacheSolve(m)
#           [,1]      [,2]
# [1,]     -2.0         1.0
# [2,]      1.5        -0.5

# Retrieving from the cache in the second run
# > cacheSolve(m)
# getting the cached inverse matrix.
#           [,1]       [,2]
# [1,]      -2.0        1.0
# [2,]       1.5       -0.5
