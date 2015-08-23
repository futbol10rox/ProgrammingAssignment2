## Programming Assignment #2
## By: Jorge Revuelta referenced ex provided by Dr. Peng
## 8/23/2015

## The program accepts a matrix, calculates its inverse, and then caches the inverse

##____________________________________________________________________________________
## makeCacheMatrix Function
## input: a single matrix
## output: creates an object that has various functions
## Functions inside include: set(), get(), setInv(), getInv()
## set(): allows user to input new matrix
## get(): allows user to get matrix value
## setInv(): allows user to cache inverse
## getInv(): allows user to get cached inverse
##____________________________________________________________________________________

makeCacheMatrix <- function(x) 
{
    
    i <- NULL
    
    set <- function(y) ##input a new matrix
    {
        x <<- y     ##lexical scoping; 
                    ##make sure values are set within entire function
        
        i <<- NULL
    }
    
    get <- function() ##get current matrix
    {
        x
    }
    
    setInv <- function(inv) ##cache calculated inverse
    {
        i <<- inv
    }
    
    getInv <- function()  ##get cached inverse
    {
        i
    }
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

##___________________________________________________________________________________
## cacheSolve Function
## input: object created from makeCacheMatrix()
## output: a single matrix(calculated inverse)
## Function checks to see if there is a cached inverse
## If cached inverse exits it outputs it
## Else it calculates a new inverse with solve(), caches it, and retruns it
##___________________________________________________________________________________

cacheSolve <- function(x, ...) {
    
    i <- x$getInv()
    
    if(!is.null(i))  ##Check to see if there is a cached inverse
    {
        message("Getting cached inverse...")
        
        return(i)
    }
    
    d <- x$get()  ##get stored matrix
    
    i <- solve(d) ##calculate the inverse
    
    x$setInv(i)  ##cache the inverse
    
    i
}