## Programming Assignment 2

# This function creates a special "matrix" object that  
# can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
    # Initializing
    Inverse_Matrix <- NULL
        
    #Creating the matrix
    set <- function(y = m) {
        #check if it is a square matrix
        if (nrow(y) == ncol(y)) {
            Inverse_Matrix <<- solve(y)
        } else {
            message("It is not a square matrix")
            Inverse_Matrix <- NULL
        }
     }
    
    #Cache the Inverse matrix
    getInverse <- function() Inverse_Matrix
    
    #return a list
    list(set = set, getInverse = getInverse)
}



## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    #Get cached inverse matrix
    Original <- x$getInverse()
    
    #if cached matrix is not null then return it
    if(!is.null(Original)) {
        #Retrieve original matrix
        Original <- solve(Original)
        message("getting cached data")
        
        #Return original Matrix
        return(Original)
    } else {
        message("There is no Matrix to Retrieve")
    }    
}


