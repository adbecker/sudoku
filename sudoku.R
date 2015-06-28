## this is a sudoku solver written by Alex Becker (adbecker@princeton.edu)
## i wrote this in an afternoon with no power so not the most efficent code
## please let me know if you use it or have any suggestions
## last editted 6/25/2015
rm(list=ls())

## load number 1 easy in book
S <- unname(as.matrix(read.table('mattest.txt')))

## load number 200 hard in book 
#S <- unname(as.matrix(read.table('sodukuhard1.txt')))

## load paramters
itmax <- 5
## empty vector for number of options/which options at each position
opts <- rep(0,9)
## we use this 3D array to track progress, i.e. when to make a guess
opts.mat <- (array(0,dim=c(9,9,itmax)))

## write function to generate squares on board
square <- function(i,j){
  i <- i - 1; j <- j - 1
  i <- i-((i%%3)-1); j <- j-((j%%3)-1)
  sq <- as.vector(S[i:(i+2),j:(j+2)])
  return(sq)
}

## solve the system
for(it in 1:itmax){
  for(nx in 1:9){
    for(ny in 1:9){
      ## if number is a given or already filled in skip it
      if(S[nx,ny] != 0){
        S[nx,ny] <- S[nx,ny]
      }
      else{
        ## run through numbers 1,...,9 and test if they meet the three conditions
        ## 1) no reps in row
        ## 2) no reps in col
        ## 3) no reps in square -- call square function here 
        for(i in 1:9){
          is.valid.choice <- c(is.element(i,S[nx,]), is.element(i,S[,ny]), is.element(i,square(nx,ny)))
          ## if all FALSE, sum(val.choice)=0
          ## this is quick and dirty way to tell what our options are for each square
          if(sum(is.valid.choice) == 0) opts[i] <- i
          else opts[i] <- 0
        }
        ## remove zeros
        opts <- opts[opts != 0] 
        ## calculate how many options are in each spot
        opts.mat[nx,ny,it] <- length(opts)
        #print(length(opts))
        #print(c('opts:', opts, 'loc', nx,ny))
        ## if length(opts) == 1 then you only have one choice, which we use
        if(length(opts) == 1 ){
          S[nx,ny] = opts
          #print(c('opts:', opts, 'loc', nx,ny))
          }
        else{}  
        }
    }
  }
  ## check if puzzle is solved yet
  is.finished.yet <- sum(is.element(S,0))
  ## if puzzle is not solved then run some diagnostics
  if(is.finished.yet != 0){
    print(sprintf('iteration %d failed, starting iteration number %d',it, it+1))
    ## now if the first iteration failed we want to make sure were actually solving something
    ## so now we use the opts.mat array 
    ## create error array
    if(it >=2) diff <- opts.mat[,,it] - opts.mat[,,(it-1)]
    ## if matrix sum is zero, no change, i.e. you aren't solving anything
    if(it >= 2 && sum(sum(diff)) == 0){
      stop(sprintf('need to make a guess. failed at %d iterations', it))
    }  
  }
  else{
    print(sprintf('complete! needed %d iterations', it))
    print( S )
    stop()
  }
}