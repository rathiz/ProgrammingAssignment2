## This R script creates a special matrix object for a given
## matrix object. The special matrix object contains input
## matrix, its inverse and various functions to get and set
## these components. Input matrix is assumed to be invertible.

## makeCacheMatrix function takes an input matrix object and
## returns a list object containing functions to get and set
## the given input matrix, and get/set functions for its inverse

makeCacheMatrix <- function(x = matrix()) {
		vSolve <- NULL
		set <- function(y) {
			x <<- y
			vSolve <<- NULL
		}
		get <- function() x
		setSolve <- function(mInverse) vSolve <<- mInverse
		getSolve <- function() vSolve
		list(set = set, get = get,
					setSolve = setSolve, getSolve = getSolve)
}

## cacheSolve takes special matrix object, created by makeCacheMatrix
## and returns the cached inverse if it exists. In case inverse doesn't
## exist, it calculates the inverse, caches it and returns the inverse.

cacheSolve <- function(x, ...) {
		vSolve <- x$getSolve()
		if(!is.null(vSolve)) {
			message("getting cached inverse matrix")
			return(vSolve)
		}
		vMatrix <- x$get()
		vSolve <- solve(vMatrix)
		x$setSolve(vSolve)
		vSolve
}

################################################################
## Unit Test# 1
vMatrix = matrix()
lstMatrix = makeCacheMatrix(vMatrix)
cacheSolve(lstMatrix) ## will return 1x1 NA as output
cacheSolve(lstMatrix) ## will return cached 1x1 NA as output

## Unit Test# 2
vMatrix = diag(1, 3)
lstMatrix = makeCacheMatrix(vMatrix)
cacheSolve(lstMatrix) ## will return diag(1, 3) as output
cacheSolve(lstMatrix) ## will return cached diag(1, 3) as output

## Unit Test# 3
vMatrix = matrix(1:4, 2, 2)
lstMatrix = makeCacheMatrix(vMatrix)
cacheSolve(lstMatrix) ## will return the following output
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

cacheSolve(lstMatrix) ## will return the following output
#getting cached inverse matrix
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

vMatrix %*% cacheSolve(lstMatrix) ## will pull the cached matrix and a 2x2 Identity matrix
