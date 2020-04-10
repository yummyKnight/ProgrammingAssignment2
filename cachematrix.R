## Write a short comment describing this function
#The first function, makeCacheMatrix creates a special "matrix", which is
#list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        set_inverse <- function(inv_matrix) inverse_matrix <<- inv_matrix
        get_inverse <- function() inverse_matrix
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function
#This cacheSolve function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the 
#cachesolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
}
# Example
# new_m <- matrix([1,2,3,4],2,2)
# tmp <- makeCacheMatrix(new_m)
# cacheSolve(tmp)
# output:      
#               [,1] [,2]
#       [1,]   -2  4.5
#       [2,]    2 -2.0
# cacheSolve(tmp)
# output:
# getting cached data
# You can test what matrix returned matrix is inverted using this rule:
# AB = BA = I, where A - original matrix, B - inverted A matrix and I is identity matrix
