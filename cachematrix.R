## In the following functions, the inverse of a matrix is determined and stored (cache).
#In the case where the function is called again with the same parameters, the inverse is 
##retrived from cache. Avoiding doing the same calculation again 
##and thus saving computational time.

makeCacheMatrix <- function(x = numeric()) {   
  Invers <- NULL  # sets the value of Invers to NULL 
  set <- function(Invers) { #set the value of the matrix
    x <<- Invers
    Invers <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) Invers <<- solve
  getsolve <- function() Invers
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##The following function checks whether matrix inverse has already been calculated. 
## When this is the case it retrieves the stored result (cache). If this not the case it
## calculates the inverse value.

cacheSolve <- function(x, ...) {
  Invers <- x$getsolve()
  if(!is.null(Invers)) {
    message("getting cached data.")
    return(Invers)
  }
  data <- x$get()
  Invers <- solve(data)
  x$setsolve(Invers)
  Invers
}

##Test  

##Invers <- makeCacheMatrix(matrix(c(1,4,6,7,10,12,14,16,18), nrow= 3, ncol=3))
## Invers$get()

##[,1] [,2] [,3]
##[1,]    1    7   14
##[2,]    4   10   16
##[3,]    6   12   18

#cacheSolve(Invers)

##[,1] [,2]      [,3]
##[1,]    1 -3.5  2.333333
##[2,]   -2  5.5 -3.333333
##[3,]    1 -2.5  1.500000

##Invers$getsolve()

##[,1] [,2]      [,3]
##[1,]    1 -3.5  2.333333
##[2,]   -2  5.5 -3.333333
##[3,]    1 -2.5  1.500000

##cacheSolve(Invers)
##getting cached data.
##[,1] [,2]      [,3]
##[1,]    1 -3.5  2.333333
##[2,]   -2  5.5 -3.333333
##[3,]    1 -2.5  1.500000

## Invers$set(matrix(c(3,4,6,8,10,15,11,16,17), nrow= 3, ncol=3))

## cacheSolve(Invers)

##[,1]       [,2]       [,3]
##[1,]   -5  2.0714286  1.2857143
##[2,]    2 -1.0714286 -0.2857143
##[3,]    0  0.2142857 -0.1428571

## Invers$get()

##[,1] [,2] [,3]
##[1,]    3    8   11
##[2,]    4   10   16
##[3,]    6   15   17

## Invers$getsolve()

##[,1]       [,2]       [,3]
##[1,]   -5  2.0714286  1.2857143
##[2,]    2 -1.0714286 -0.2857143
##[3,]    0  0.2142857 -0.1428571
