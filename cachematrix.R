##This function creates a special "matrix" object
##that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    cacheInv <- NULL
    set <- function(uVal = matrix()) {
        x <<-  uVal
        cacheInv <<- NULL
    }
    get <- function(){
        x
    }
    setInv <- function(iVal) {
        cacheInv <<- iVal 
        return(cacheInv)
    }
    getInv  <- function() {
        cacheInv
    }
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}
##This function will get the inverse of the matrix
##returned by makeCacheMatrix. If inverse has already been 
##calculated then cacheSolve will retrieve inverse from cache.
cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { 
    calInv <- x$getInv() 
    if(!is.null(calInv) && is.matrix(calInv)) { 
        message("Cached data found.")
        return(calInv)
    }
    newMatrix <- x$get()  
    calInv <- tryCatch({ 
        solve(newMatrix)
   
    })
    x$setInv(calInv)
}
