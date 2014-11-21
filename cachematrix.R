# It receives a matrix 'x' which is always invertible.
# It returns a special 'matrix' object that can cache its inverse.
# The retrieved object contains the following functions: 
#  'get'
#       return the matrix value
#  'setCachedInversed'
#       set the cache value of the inversed matrix
#  'getCachedInversed'
#       get the inversed matrix cached
makeCacheMatrix <- function(x = matrix()) {
	# set cache variable to null
	cache <- NULL
	# define the 'set' function which is used to the matrix value and reset its cache
	set <- function(y) {
			x <<- y
			cache <<- NULL
	}
	# define the 'get' function which return the matrix value
	get <- function() x
	# define the 'setCachedInversed' function which set the cache value of the inversed matrix
	setCachedInversed <- function(i) cache <<- i
	# define the 'getCachedInversed' function which get the inversed matrix cached
	getCachedInversed <- function() cache
	# return a vector of the defined functions above
	list(set = set, get = get,
		setCachedInversed = setCachedInversed,
		getCachedInversed = getCachedInversed)
}

# It receives a matrix 'x' which is always invertible.
# It returns the inversed matrix of 'x'. 
# This value will be calculated whether 'x' do not has it cached
# (and then it'll save this value into the 'x' cache). 
# Otherwise, it'll return the cached value.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	cache <- x$getCachedInversed()
	# verify if there is an inversed matrix cached
	if(!is.null(cache)) {
		message("getting inversed matrix cached")
		#return inversed matrix cached
		return(cache)
	}
	# get matrix value
	matrix <- x$get()
	# calculate the inversed matrix
	i <- solve(matrix, ...)
	# save the inversed matrix into cache
	x$setCachedInversed(i)
	# return inversed matrix
	i
}