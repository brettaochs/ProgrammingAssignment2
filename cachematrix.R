## These functions will create a matrix using passed information
## then calculate inverse of matrix and cache for future reference to
## reduce processor use unless matrix content is changed

## makeCacheMatrix creates a matrix based on arguments passed to function
makeCacheMatrix <- function(x = matrix()) {    
    ## Initialize variables
    matrix_inverse <- NULL
    set <- function(y){
        x <<- y
        matrix_inverse <<- NULL
    }
    get <- function(){
        x
    }
    setinverse <- function(solve){
        matrix_inverse <<- solve
    } 
    getinverse <- function(){
        matrix_inverse
    } 
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse previously calculated and matrix hasn't changed then 
## cacheSolve should retrieve the cached Inverse instead of calculating again.

cacheSolve <- function(x,...){
    matrix_inverse <- x$getinverse()
    if(!is.null(matrix_inverse)){
        message("getting cached inversed matrix")
        return(matrix_inverse)
    }
    data <- x$get()
    ##if(dim(data)[1] != dim(data)[2]) stop("matrix must be square!\n")
    matrix_inverse <- solve(data)
    x$setinverse(matrix_inverse)
    matrix_inverse
}