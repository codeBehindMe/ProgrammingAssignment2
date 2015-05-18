## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function will make an object which contains 4 elements. 2 of the will return a value and 2 of them will set a property

makeCacheMatrix <- function(x = matrix()) {

      inv_mtx <-NULL ## initialize the matrix as null
      
      set <-function(mtx){ #this will initialize the object when created. we will pass in the matrix and it will be set
                              # set in the parent environment
            x <<- mtx 
            inv_mtx <<- NULL
      }
      
      get <-function() x # this will simply return the matrix that was passed in 
      
      setInv <-function(solve)inv_mtx <<- solve #since we call for the solve function to set the inverse 
      
      getInv <-function() inv_mtx #simply return the inverse of the matrix
      
      #create a list so that we can call by name
      
      list(set=set,get=get,setInv=setInv,getInv=getInv)
      
}


## Write a short comment describing this function
# This function will provide the inverse of a particular matrix and if we have alraedy caclulated the inverse, it will 
# simply retrieve the inverse from the cache. the function takes args of an object which is of type makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inv_mtx <- x$getInv() #we call this to get the inverse of the matrix thats stored in the cache
      
      if(!is.null(inv_mtx)){ #check if this is null to see if the inverse has been already caclulated if it is then ::
            
            message("you have already calculated the inverse, fetching it now!")
            return(inv_mtx) # :: show the inverse and return out of the function
      }
      
      mx <- x$get() #well since we havent got the inverse, get the matrix so we can calculate it
      inv_mtx <- solve(mx) # call on the solve function to get the inverse
      
      x$setInv(inv_mtx) #set the mean so we can call for it in the future
      
      inv_mtx # show the inverse
      
}
