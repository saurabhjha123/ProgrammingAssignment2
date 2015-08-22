## Created two functions to "cache" inverse of a matrix to avoid multiple computations for same matrix 
## References :-
### 1.) Example for Assignment 2 in R programming course at Coursera (rprog-031)
### 2.) github explanation by DanieleP : https://github.com/DanieleP/PA2-clarifying_instructions

## Function makeCacheMatrix stores 4 other fuctions: set, get, getinv, setinv

makeCacheMatrix <- function(x = matrix()) {
  
  inv=NULL
  
  # To change input matrix and by default it is changing inv to Null as well
  set<-function(y=matrix()) 
    {
      x<<-y
      inv<<-NULL
    }
  
  # To get input matrix
  get<-function() x
  
  # To get inverse of input matrix
  getinv<-function() inv
  
  # To set inverse of input matrix
  setinv<-function(inv_inp) inv<<-inv_inp
  
  # To store/return input matix and all 4 functions when makeCacheMatrix is called
  list(inv=inv,set=set,get=get,getinv=getinv,setinv=setinv)
  
}


## To solve/cache inverse of input matrix given(as input)/set to makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_c<-x$getinv()
  
  # if inverse in makeCacheMatrix is not null it will be cached and retured
  # if inverse in makeCacheMatrix is null then only inverse is calculated using Solve function
  
  if(!is.null(inv_c))
    {
       message("getting cached data")
       return(inv_c)
    }
  else
   {
       mat<-x$get()
       inv_c<-solve(mat)
       x$setinv(inv_c)
   }
  
}
