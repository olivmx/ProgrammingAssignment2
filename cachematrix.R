## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## will return a list with 4 functions: get set getinverse setinverse
## get: returns the matrix to inverse
## set: sets the matrix to inverse
## getinverse: return the inverse matrix of matrix
## sets the inverse matrix of matrix


makeCacheMatrix <- function(x = matrix()) {
        #the inverse is set to null
        i<- NULL
        ## when using the set function it sets the matrix to the recieved parameter and erases the inverse
        set <- function (y){
            x <<-y
            i <<- NULL
        }
        ## when using the get function it returns the value of the matrix
        get <- function() x
        ## when using the setinverse sets the inverse to the function solve
        setinverse <-function(solve) i <<- solve
        ## when using the getinverse returns the previusly calculated inverse
        getinverse <-function() i
        ## now return a list with the four functions: set, get, setinverse, getinverse
        list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## first get the inverse of the parameter
        i <- x$getinverse()
        ## then check if its null. if it is not return it and exit function
        if (!is.null(i)){
            message ("getting inverse from cache")
            return (i)
        }
        ## but if it is null, call the setinverse to calculate the inverse
        data <- x$get()
        i <- solve (data,...)
        x$setinverse(i)
        i
}


#solve: R function to get the inverse of the matrix
#det: R function to get the determinant of the matrix (if determinant is zero matrix is not inversable)

#tested with:
#mtr <- matrix (c(3,3.2,3.5,3.6), nrow=2, ncol=2)  ## creates a 2x2 inversable matrix
#funlist <-makeCacheMatrix(mtr)                    ## creates a funlist with the result of the first function
#cacheSolve(funlist)                               ## calculate and return the inverse of mtr

