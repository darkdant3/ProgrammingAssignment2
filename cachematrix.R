## The 'makeCacheMatrix' function creates a special matrix object more like an OOP object
#with matrix and inverse as its properties/attributes with setters and getters methods
#
# The 'cacheSolve' takes the 'special' matrix object and solves for inverse of a 'data' matrix 
#returns that inverse

## creates a special matrix object with setters and getters methods for caching and retrieving
# a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #work on matrices
  if( !is.matrix( x ) ){
    message("variable <x> is not a matrix")
    #set inverse to NULL
    inverse <<- NULL
    x <<- NULL
    return( NULL )
  }
  #properties
  inverse <- NULL
  x <- NULL
  
  # setters and getters
  get<- function( ) x
  set<- function ( m ){
    x <<- m
  } 
  setinverse <- function ( i ) inverse <<- i
  getinverse <- function ( ) inverse
  
  list( get = get , set = set , setinverse = setinverse , getinverse = getinverse )
}


## returns inverse of a 'data' matrix from the global environment and NULL on error

cacheSolve <- function(x, ...) {
  
  if( is.null( x ) ){
    message( "'Special' matrix is null")
    return (NULL)
  }
  if( ! is.matrix( data ) ){
    message( '<data> variable is not assigned')
    return ( NULL )
  }
  #check for cached inverse
  if( ! is.null(x$getinverse() )){
    #check if matrix was changed
    if( identical( x$get() , data ) ){
      #no changes return cached inverse
      message( 'No matrix changes.. returning cached inverse')
      return (x$getinverse())
    }
    #matrix was changed.. resolve inverse
    message( 'Matrix changed.. resolving inverse')
    x$setinverse( solve( data ), ...)
    x$set( data )
    return (x$getinverse())
  }else{
    #an inverse was cached
    #check if matrix was changed
    if( identical( x$get(), data ) && ! is.null( x$getinverse())){
      message( 'Getting cached inverse' )
      return ( x$getinverse())
    }
    
    #matrix was changed
    #set inverse to null and resolve inverse with new matrix
    x$setinverse( solve( data ) )
    x$set( data )
    x$getinverse()
  }
  
}
