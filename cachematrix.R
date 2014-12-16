## Notes will be written in both Chinese and English to be understood by more people
## 为了让更多的人理解，注释用中文和英文两种语言书写

## A pair of functions following cache the inverse of a matrix.
## 下面是一对用来缓存逆矩阵的程序


## Set, get the matrix to be solved
## Set, get the inverse of the matrix given
## 设置，获取将要求解的矩阵
## 设置，获取给定矩阵的逆矩阵
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y  ## Set the matrix as y 将矩阵设为参数y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, 
         get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## Retrieve the cache of the inverse of the matrix
## Or solve to get the inverse of the matrix
## 从缓存中获取矩阵的逆矩阵
## 或者通过求解（Solve函数）得到矩阵的逆矩阵
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)){
        message("Getting cached inverse")
        return(inverse)
        ## If the inverse has already been calculated then retrieve the inverse from the cache.
        ## 如果逆矩阵已经计算出来了，那么直接从缓存中获取
    }
    ## If there's no inverse existing, calculate as following
    ## 如果逆矩阵还不存在，通过下面计算得到
    data <- x$get()
    inverse <- solve(data, ...) ## Get the inverse of the given matrix 获得给定矩阵的逆矩阵
    x$setInverse(inverse) ## Cache the inverse 缓存求得的逆矩阵
    inverse
}
