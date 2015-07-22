## these 2 functions are used to avoid repeating inverse matrices calculations.
## Once the inverse is calculated it is stored in cache memory so it is just called instead of calculated again

## This 1st function creates and stores 4 functions. As they are nested they store objects in different environments
## To be able to access the same objects the operator <<- is used to search for them in parent environments.
## The functions are stored in a list
## inv variable will store the inverse matrix; set function will change the matrix stored in the main function 
## it will delete any previous inv.
## get function returns the vector stored in the main function(the one passed as an argument to makeCacheMatrix)
## setinverse function stores the input variable in the main function
## getinverse returns the input variable stored in the main function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if the inverse matrix has already been calculated (and stored) by means of calling the 1st function.
## If so, it returns it (and prints a message). Otherwise it calculates it.
## the functions stored in the previous function now are called as elements of a list called x(the input variable for this function)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
