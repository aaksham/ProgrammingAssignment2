## These functions will enable caching of the inverse of a matrix to save time and 
## unnecessary computation


## makeCacheMatrix is a function that defines/creates other functions which will 
## coordinate the making of an inverse matrix or return a cached matrix. This
## is equivalent to a class in C++/Python which will then be instantiated when
## needed.

makeCacheMatrix <- function(x = matrix()) {
        print("in cache")
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinv<-function(sinv)inv<<-sinv
        getinv<-function()inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function checks if an inverse of the matrix exists in the 
## cache and returns it or calculates the inverse if it doesn't 
## exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat)
        x$setinv(inv)
        inv
}
