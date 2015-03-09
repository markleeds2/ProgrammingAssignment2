# GENERAL DESCRIPTION:
# THE FUNCTIONS BELOW ILLUSTRATE THE ADVANTAGES OF THE LEXICAL
# SCOPING PARADIGM USED IN R. DATA CAN BE ASSOCIATED WITH
# A FUNCTION"S ENVIRONMENT. IN THIS MANNER,  BURDENSOME 
# COMPUTATIONS ASSOCIATED WITH THE DATA ONLY NEED TO BE DONE 
# ONCE BECAUSE THE COMPUTATION CAN BE SAVED IN THE FUNCTION"S 
# ENVIRONMENT AND RETURNED WHEN NEEDED.

#============================================================
## AUTHOR: MARK LEEDS
## DATE: MARCH 08, 2015
## DESCRIPTION: FUNCTION TAKES A SQUARE MATRIX AS INPUT
## THEN WRITE LOCAL FUNCTIONS WHOSE CLOSING ENVIRONMENT IS
## IS THE FUNCTION'S ENVIRONMENT. THIS WAY, THE 
## MATRIX INPUT IS "ATTACHED" TO THE FUNCTION.
## THERE ARE FUNCTIONS FOR SETTING THE MATRIX, GETTING
## ITS MATRIX, SETTING THE INVERSE AND GETTING THE INVERSE

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
#============================================================
## AUTHOR: MARK LEEDS
## DATE: MARCH 08, 2015
## DESCRIPTION: FUNCTION TAKES A SQUARE MATRIX
## AND EITHER 
## A) CALCULATES ITS INVERSE IF IT
## HAS NOT ALREADY BEEN COMPUTED AND STORED IN
## makeCacheMatrix OR 
## B) RETURNS THE PREVIOUSLY SAVED RESULT FROM 
## makeCacheMatrix

cacheinverse <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
#=================================================================
# CODE FOR CHECKING THAT THE FUNCTIONS
# ABOVE ARE WORKING CORRECTLY
#=================================================================

# CREATE MATRIX
testmat <- matrix(c(1,2,3,4), nr = 2)
print(testmat)

# CACHE THE MATRIX 
cachetest <- makeCacheMatrix(testmat)
# GET THE MATRIX
cachetest$get()

# GET THE INVERSE. NOT PREVIOUSLY COMPUTED
cacheinverse(cachetest)
# GET THE PREVIOUSLY COMPUTED INVERSE
cacheinverse(cachetest)

# SET THE MATRIX TO A NEW VALUE
cachetest$set(matrix(c(2,4,6,8), nr = 2))
cachetest$get()

# GET THE INVERSE. NOT PREVIOUSLY COMPUTED
cacheinverse(cachetest)
# GET THE PREVIOUSLY COMPUTED INVERSE
cacheinverse(cachetest)















