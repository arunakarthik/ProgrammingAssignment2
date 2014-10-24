# Operations like Matrix inversion are computation intensive and their may be some
# benefit to caching the inverse of a matrix rather than computing it repeatedly.
# The lexical scoping rules of R are used here to implement the caching functioanlity.


# makeCacheMatrix function returns a special "matrix" object that can cache its inverse.
# The returned matrix object is actually a list containing functions to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the matrix inverse
# 4. Get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve function returns the inverse of the special matrix object
# created using the above makeCacheMatrix function. 
# First checks whether the matrix inverse has already been calcualted or not. 
# If found, cached result is returned. Otherwise, it calculates the inverse,
# stores the result in the cache using setInverse function & returns the result

cacheSolve <- function(x, ...) 
{
    inv <- x$getInverse()
    
    if(is.null(inv))
    {
        message("setting up cache")
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
    }
    else
    {
        message("data served from cache")
    }
    
    inv
}
