#The following two functions create a Matrix, calculate its inverse, and cache
#the result for quicker computations. 


#This function, makeCacheMatrix creates a special Matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function(x) 
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

#This function computes the inverse of the matrix returned by makeCacheMatrix.  
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    } else {
    m<-solve(x$get())
    x$setmatrix(m)
    m
    }
}