## Functions set the inverse of a matrix and retrieve it from the cached memory if it 
## is already calculated

## This function sets and retrieves the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  
        i<-NULL  
        set<- function(y){
              x<<-y
              i<<-NULL          
        }
        
        get <- function() x
        setinv<- function(inv) i<<- inv
        getinv<- function() i
        
        list(set=set,get=get,setinv=setinv,getinv=getinv) 
        
}


## This function computes the inverse of matrix if it has not been computed earlier
## If the inverse has been computed then the function returns the cached value


cacheSolve <- function(x, ...) {
  
        i<- x$getinv()
        if(!is.null(i)){
                  message("Message: Getting cached data")
                  return(i)
        }
        data<- x$get()
        i<- solve(data,...)
        x$setinv(i)
        i
        ## Returns a matrix that is the inverse of 'x'
}
