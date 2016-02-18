## The following two functions makeCacheMatrix and cacheSolve together 
##creates a special matrix,store it in the cache
##and then calculates the inverse matrix and store 
##the inverse matrix in the cache and returns the inverse matrix 

##makeCacheMatrix is a function which returns a list of functions
##The functions returned are 
##a. setMatrix -- set the value of a matrix
##b. getMatrix -- get the value of the set Matrix
##c. setInverse -- set the inverse of the set Matrix
##d. getInverse -- get the inverse of the set Matrix

##makecacheMatrix stores a matrix and a cached value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL				##initially nothing in the cache so inv(inverse of the matrix)is set to null
	setMatrix<-function(y) {	
		x<<-y				##set the value of the matrix to y and store it
		inv<<-NULL			##for a new matrix inv is set to NULL 	
	}
	getMatrix <- function() x	##returns the stored matrix 
	setInverse <-function(solve) inv<<-solve ##cache the inverse of the stored matrix
	getInverse <-function() inv		     ##returns the cached Inverse
	#the following command returns a list where each element in the list is a function
	list(seMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse,
		getInverse = getInverse)

}


##cacheSolve function first checks if the inverse of the input matrix
##is stored in the cache, if yes, then returns it else it calculates
##the inverse of the input matrix, stores it in the cache
## and returns the value of the inverse matrix

cacheSolve <- function(x, ...) {
	inv<-x$getInverse()		##checks for the presence of the inverse matrix in the cache
	if(!is.null(inv)){
		message ("getting cached data") ## if yes (not NULL), then returns the value of the cached inverse matrix
		return(inv)
 		}      				
data <- x$getMatrix()			##else, it calculates the inverse of the input matrix
inv<-solve(data, ...)			
x$setInverse(inv)				##stores it in the cach
inv						
}