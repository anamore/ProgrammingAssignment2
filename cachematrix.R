## These functions working together are able to cache 
## a potentially time-consuming computation, like 
## matrix inversion, so that if it has to be computed
## repeatedly, it can be looked up in the cache (not
## recomputed)


## Function 'makeCacheVector' will create a special object, 
## which is really a list containing functions to set and get 
## the values of the original matrix and its inverse

# 'm' will be the matrix we want to inverse.
# 'inv' will be its inverse and it is reset to NULL
#       every time 'makeCacheMatrix' is called.
makeCacheMatrix <-function(m=matrix()){
  inv<-NULL
  
  # 'set' is a function that sets the values for 'm' and 'inv' 
  # using superassignment 
  set<-function(y){
    m<<-y
    inv<<-NULL
  }
  
  # 'get' is a function that returns the original matrix
  get<-function(){
    m
  }
  
  # 'setinv is a function that stores inverse in 'inv' 
  # using superassignment
  setinv<-function(inverse){
    inv<<-inverse
  }
  
  # 'getinv' is a function that returns the cached matrix 
  # on subsequent accesses
  getinv<-function(){
    inv
  }
  
  # this is the list returned by 'makeCacheMatrix ', which is 
  # really a list of the internal functions accessed by the 
  # function 'cacheSolve'
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}




## Function 'cacheSolve' will return the inverse of  
## the matrix passed to function ''makeCacheMatrix'.  
## It first checks if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse and sets
## its value in the cache via 'setinv'


# the input 'y' is an object created by 'makeCacheMatrix'. 
# 'inv' will be the inverse. it is initizlized by accessing 
#      the object 'y' getting its value (NULL or cached value)
cacheSolve<-function(y,...){
  inv<-y$getinv()
  
  # if the inverse was already cached (not NULL), 'cacheSolve' 
  # send the message "getting cached data" to the console 
  # and return the inverse
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # if the inverse was not previously cached, then 'cacheSolve': 
  
  # gets the original matrix from the object created by 'makeCacheMatrix'
  data<-y$get()
  # calculates the inverse
  inv<-solve(data,...)
  # stores the calculated inverse in the object created by 'makeCacheMatrix' 
  y$setinv(inv)
  # and finally returns the inverse matrix
  inv
}
