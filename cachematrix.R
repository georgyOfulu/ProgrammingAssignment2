## The makeCacheMatrix function can be thought of as a factory that produces data and functions that can be 
## assessed by other functions. Similar to the R6 and Object Orienting Programming(OOP) concept.

## 1. defining the makeCacheMatrix aka our function factory that returns  a list of functions.

makeCacheMatrix <- function(x = matrix()) {
        inverse_data_cache <- NULL
        #defining a method to set the input matrix data
        set_matrix <- function(new_matrix_input) {
                x <<- new_matrix_input
                inverse_data_cache <<- NULL
        }
        #defining a method to get the matrix
        get_matrix <- function()
                x
        #defining a method to manually set the inverse of the matrix
        set_inverse <- function(new_inverse_input) {
                inverse_data_cache <<- new_inverse_input
        }
        #defining a method to get the inverse of the matrix
        get_inverse <- function()
                inverse_data_cache
        
        #enclosing the entire function to return a list of functions.
        #each method defined earlier would be an element of the list and
        #can be referenced using the $ subset operator.
        
        list(
                set_matrix = set_matrix,
                get_matrix = get_matrix,
                set_inverse = set_inverse,
                get_inverse = get_inverse
        )
}


## 2. Defining the cachesolve function that computes the inversion of the input matrix and stores it
##    in memory OR returns an already cached inversion matrix previously computed

cacheSolve <- function(list_data_input, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_data_cache <- list_data_input$get_inverse()
        if(!is.null(inverse_data_cache)){
                message("getting cached inverse matrix")
                return(inverse_data_cache)
        }
        else {
                data <- list_data_input$get_matrix()
                inverse_data_cache <- solve(data)
                list_data_input$set_inverse(inverse_data_cache)
                inverse_data_cache
        }
}
