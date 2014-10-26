## Function cacheMatrix calclates the inverse matrix for x.
## If the inverse matrix has been calculated already the cashed meanig returns.

## Function makeMatrix makes an object for matrix x
## Function cacheMatrix calclates the inverse matrix for x

makeMatrix <- function(x = vector()) {
        i <- NULL

        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        get <- function() x

        setinv <- function(inv) i <<- inv

        getinv <- function() i

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheMatrix <- function(x, ...) {
        i <- x$getinv()

        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}