## The two functions below enable the caching of the inverse matrix for an invertible matrix.
##
## A square matrix A is called invertible if a square matrix B exists such that:
##            AB = BA = I   ,  with I the identity matrix
##
## It is assumed that the matrix A is invertible, no checks are performed to ensure this.
##
## The first function, makeCacheMatrix, takes a matrix as input and creates a vector containing 
## four functions:
## - makeCacheMatrix$get : get value of matrix
## - makeCacheMatrix$set : set value of matrix
## - makeCacheMatrix$getinverse : get value of inverse matrix
## - makeCacheMatrix$setinverse : set value of inverse matrix
##
## The second function, cacheSolve, takes the vector created with makeCacheMatrix function as input,
## calculates the inverse of the matrix contained in this vector and stores this inverse into the vector.
## In case the input argument already has an inverse cached, the function will return this cached value and 
## the calculation will not be performed.


## makeCacheMatrix receives a matrix x as input (defaults to an empty matrix) and returns a list containing
## four functions. Within the scope of the function are two values:
## x : containing the matrix (initiated to argument passed to the function
## inverse: containing the inverse matrix (initiated to NULL)
##
## The four functions of the returned list have the following properties:
## $set(y) : sets the matrix x to y and resets inverse to NULL.
## $get()  : returns the value of the contained matrix x
## $setinverse(inv) : sets the value of inverse to inv
## $getinverse() : returns the value of inverse
##
## Both "set" functions use the <<- operator to set the x and inverse variables in the scope
## of the function (makeCacheMatrix) in which the two setters are defined.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y = matrix()){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse<<-inv
    getinverse <- function() inverse
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve receives a list object (as returned by the makeCacheMatrix) and calculates the inverse matrix
## of the matrix contained in this object. 
## First the function will check if the inverse is already known, in which case this value is returned, and
## a message shown stating that a cached value is used.
## If no inverse is known (the $getinverse function in the input object's list returns a NULL value), 
## the inverse is calculated using solve(). Optional arguments to the solve() function can be passed to cacheSolve().
## The calculated inverse matrix is stored in the object sent as argument to the cachSolve function. This will
## ensure the next time the function is called on the same object, the cached value is returned and no additional
## calculations are required.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
