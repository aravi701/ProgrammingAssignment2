##PART 1##
##makeCacheMatrix: This function creates a special "matrix" object that
##can cache its inverse.##
makeCacheMatrix<-function(mat=matrix()){
inv<-NULL
##set function. Can be used to change the given matrix##
set<-function(mat2) {
mat<<-mat2
inv<<-NULL
 }
##get function. Can be used to print the set matrix##
get<-function()  {
mat
  }
setinverse<-function(inverse)   {
inv<<-inverse
   }
getinverse<-function()    {
inv
    }
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
##PART 2##
##cacheSolve: This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.##
cacheSolve<-function(mat,...){
inv<-mat$getinverse()
if(!is.null(inv)){
message("getting cached data")
return(inv)
}
data<-mat$get()
##getting the inverse of the matrix##
inv<-solve(data,...)
mat$setinverse(inv)
inv
}
##example:
##x<-makeCacheMatrix(matrix(c(1,11,21,31),2,2))##A non singular matrix should be given!!! ##
##x$get() : calling this function gives us the set matrix##
##cacheSolve(x) :computes inverse and gives the cached data
##x$getinverse() : calling this function after running cacheSolve() function gives inverse of the matrix##
