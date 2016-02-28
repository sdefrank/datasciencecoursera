##This function creates a special "matrix" object
##that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    CI <- NULL
    set <- function(UV = matrix()) {
        x <<- UV 
        CI <<- NULL
    }
    get <- function(){
        x
    }
    setInv <- function(IV) {
        CI <<- IV 
        return(CI)
    }
    
    getInv  <- function() {
        CI
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
        return(calIn)
    }
    newMatrix <- x$get()  
    calInv <- tryCatch({ 
        solve(newMatrix)
   
    })
    x$setInv(calInv)
}
