## "MakeCacheMatrix" will first examine if a matirx is invertible,
## and then restore the matrix and corresponding inverse matrix in a list
## when "CacheSolve" gets called, it will first see if the matrix is restored in matrix list,
## and the return the cooresponding element of the inverse list
## if the matrix is not in the matrix list, then "CacheSolve" will calculate the inverse,
## and restore the value to the matrix list by calling "MakeCacheMatrix"

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  Matrix_list<- list()                                       ##create Matrix list
  Inverse_Matrix<- list()                                    ##Create corresponding Inverse list
  if (class(try(solve(x),silent = TRUE)) == "matrix"){       ##examine if x is invertible
    Matrix_list <<- c(Matrix_list, list(x))                  ##if it is invertible, restore x in Matrix list
    Inverse_Matrix <<- c(Inverse_Matrix, list(solve(x)))     ## restore the inverse of x in Inverse list
  }
  if(class(try(solve(x),silent = TRUE)) != "matrix"){        ##if x is not invertible,
    message("This Matrix is not invertible")}                ## error message is printed
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if (list(x) %in% Matrix_list){                            ##check if x is in the matrix list
    Inverse_Matrix[[match(list(x),Inverse_Matrix)]]         ## if it is in the matrix list, get the index of x in matrix list, subset the corresponding element in the inverse list
    print(Inverse_Matrix[length(Inverse_Matrix)])           ## print out the answer
  }
  if (is.element(list(x), Matrix_list) == FALSE){           ## if x is not in the matrix list
    makeCacheMatrix(x)                                      ## call makeCacheMatrix to restore x into the "database" and calculate the inverse of x, restore that in inverse list
    print(Inverse_Matrix[length(Inverse_Matrix)])           ## print out the answer
  }
}
