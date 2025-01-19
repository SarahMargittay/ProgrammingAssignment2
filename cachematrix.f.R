## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## Armazena a inversa da matriz (em cache)
  set <- function(y) {
    x <<- y
    m <<- NULL  ## Se a matriz for alterada, a inversa deve ser redefinida
  }
  ## Obtém a matriz
  get <- function() x
  
  ## Define a inversa da matriz
  setmean <- function(inverse) m <<- inverse
  
  ## Obtém a inversa da matriz
  getmean <- function() m
  
  ## Retorna uma lista de funções para manipular a matriz e sua inversa
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getmean()  ## Obtém a inversa armazenada, se existir
  
  ## Se a inversa já foi calculada, retorna ela diretamente do cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Caso contrário, calcula a inversa
  data <- x$get()  ## Obtém a matriz original
  m <- solve(data, ...)  ## Calcula a inversa da matriz
  
  ## Armazena a inversa no cache
  x$setmean(m)
  
  ## Retorna a inversa calculada
  m
}

