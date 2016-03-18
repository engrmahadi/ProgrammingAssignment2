# cache the inverse of a matrix

 makeCacheMatrix <- function( given_mat = matrix() ) 
  {
  
    # Initialize
    initial_value <- NULL
    
    # set the matrix
    set <- function( matrix ) { given_mat <<- matrix; initial_value <<- NULL }
    
    #get the matrix
    get <- function() { given_mat }
    
    ##  matrix inverse
    setInverse <- function(inverse) { initial_value <<- inverse }
    
    # get the inverse
    getInverse <- function() { initial_value }
    
    #list of the methods
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
   
 }


cacheSolve <- function(x, ...) 
  {
    # inverse of 'x' 
    xinv_mat <- x$getInverse()
    
    # return the inverse
    if( !is.null(xinv_mat) ) { message("cached data");   return(xinv_mat)  }
    
    # Get matrix 
    given_data <- x$get()
    
    # Estimate inverse 
    xinv_mat <- solve(given_data) %*% given_data
    
    # inverse to the object
    x$setInverse(xinv_mat)
    
    # Return the matrix
    xinv_mat
 }
