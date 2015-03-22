## The makeCacheMatrix() function takes the "special" matrix we want to cache its inverse as a parameter and returns 
## a list containing the following functions:
## 1- setMatrix(matrix()) that sets the value of the special matrix
## 2- getMatrix() returns the special matrix 
## 3- setInv(matrix()) sets the value of the inverse
## 4- getInv() returns the inverse of the matrix


## Once we have calculated the inverse of a matrix (and if this operation is time consuming, especially 
## with very large matrices), it makes sense to store the value of the inverse so we can use it afterward as often as we 
## need without unnecessarry repetitive calculation. 
## Following is an example of the function's utilization:
## A <- matrice(rnorm(25,5,5)) ##creating a matrix of 5 by 5 elements from a random distribution
## inverse <- solve(A) ## calculating the inverse of A
## cc <- makeCacheMatrix(A) ## calling the function makeCacheMatrix with the matrix A as a parameter
## cc$setInv(inverse) ## caching the value of A's inverse 
## cc$getMatrix() ## returning the value of the matrix
## cc$getInv() ## returns the value of the inverse
## If we want to change the special matrix A, we can call the function setMatrix() that will update the value of the parameter x and 
## set the inverse to NULL: cc$setMatrix(B <- matrix(runif(49),7,7))

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL   ## initializing the inverse to NULL
  
  ## the setMatrix() function sets the parameter x to the value of the new matrix and sets the inv variable to NULL 
  ## hence preventing the case where we change the "special" matrix and forget to update the inverse
  
  setMatrix <- function(M)
    
  {
     x <<- M
     
     inv <<- NULL
  }

  
  getMatrix <- function() x

  
  setInv <- function(M_Inv)
  {
    inv <<- M_Inv 
  }
  
  
  
  getInv <- function() inv 
  
  list(setMatrix=setMatrix,getMatrix = getMatrix,setInv = setInv,getInv =getInv)
  
}


## The cacheSolve() function computes the inverse of the matrix returned by the makeCacheMatrix (via the getMatrix() 
## inner function)
## It takes as a parameter the list created by the makeCacheMatrix. First, it tests if the special matrix is still the same 
## (by looking at the value of the inverse which is set to NULL if the makeCacheMatrix() is called or if the setMatrix() 
## function is called). In this case, the function cacheSolve() just returns the cached inverse through the inner function 
## getInv(), otherwise, it calculates the inverse using the function solve() and caches its value.
## The "special" matrix must be invertible, otherwise an error is thrown

cacheSolve <- function(x, ...) {
   
   inv <- x$getInv()
   
   if(!is.null(inv))
   {
     print("The inverse of the cached matrix is:")
     
     return(inv)
     
   }
   
   new_matrix <- x$getMatrix()
   
   print("Calculating the new inverse...")
   
   new_inv <- solve(new_matrix)
   
   print("Caching the new inverse...")
   
   x$setInv(new_inv)
   
   print("The new inverse is:")
   
   new_inv
}
