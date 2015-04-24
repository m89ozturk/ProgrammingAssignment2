## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## inv_x is the calculated inverse of matrix x by solve() function
## setInv//getInv functions are generated in order to store//retrieve the inverse of matrix x
## finally new matrix is generated
makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) inv_x <<- Inv
        getInv <- function() inv_x
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

## if the inverse of matrix x is already calculated first caches and returns it
## otherwise inverse of x gets calculated, set as its reverse and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getInv()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setInv(inv_x)
        inv_x
}

