## This file has two functions, one which generates a list of functions
## with a matrix, and the other which generates an inverse matrix
## if one doesn't exist. It searches for it before solving.
## The functions share the "m" variable, storing the cached solve matrix.

## This function generates a list of functions, for a matrix
## This matrix has the function to
## set - set itself
## get - print itself
## setsolve - set the solved inverse matrix
## getsolve - print the solved inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y = matrix(), ...) {
          x <<- matrix(y,...)
          m <<- NULL
     }
     get <- function() x      #prints the x matrix
     setsolve <- function(solve) m <<- solve #stores m
     getsolve <- function() m #prints m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## This function will see if an inverse exists of a matrix
## If it does, then it just prints that,
## if it doesn't, it calculates the inverse.

cacheSolve <- function(x, ...) {
     m <- x$getsolve()   #retrieve m
     if(!is.null(m)) {   #If m isn't NULL then report it and skip process
          message("getting cached data")
          return(m)
     }
     data <- x$get()     #if emtpy, calculate inverse
     m <- solve(data)    #put solve to m
     x$setsolve(m)       #set solve m of x
     m                   # print it
               ## Return a matrix that is the inverse of 'x'
}