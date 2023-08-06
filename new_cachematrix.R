makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## makecachematrix is a function that creates a special matrix and 
## caches the inverse matrix object 

makeCacheMatrix <- function(x = matrix()) {
        temp <- NULL

        set <- function(y) {
                x <<- y
                temp <<- NULL
        }
  
        get <- function() 
                x
        setinver <- function(inverse) 
                temp <<- inverse
        getinver <- function() 
                temp
        
        list(set=set, get=get, setinver=setinver, 
                getinver=getinver)
}

## the function cacheSolve returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  
        temp <- x$getinver()
        
        if(is.null(temp) == FALSE) {
                return(temp)
        }
        
        result <- x$get()
        temp <- solve(result,...)
        x$setinver(temp)
        
        temp
}

