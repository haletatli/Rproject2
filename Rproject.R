test = 1:10
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
makeVector(test)
cachemean(makeVector(test))
mean(test)
test2 <- makeVector(test)
test2
test$getmean()
test2$getmean()
test2$get()
mean(1:10,...)
mean(test2$get(), ...)
test2$set
test2$set()
test2$set(1:3)
x