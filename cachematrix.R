## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){        ## set the value of the matrix
        x<<-y
        inv<<-NULL
    }
    get<-function() x       ## get the value of the matrix
    setinv<-function(inver) inv<<-inver     ## set the value of inverse
    getinv<-function() inv                  ## get the value of inverse
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()               
    if(!is.null(inv)){             ## check cached data
        message("getting cached data")
        return(inv)
    }
    data<-x$get()                 ## get the value of matrix
    if(det(data)){                ## check the invertibility of matrix
        inv<-solve(data)          ## to inverse the matrix
    }else {
        message("matrix is not invertible")          
        return
    }
    x$setinv(inv)                ## set the value of inverse 
    inv
}
