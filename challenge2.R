# Data incubator application challenge, task 2
#
# https://www.thedataincubator.com/challenge.html
#
# sbuergers@gmail.com
# 01/11/2019

rm(list = ls())

library(ggplot2)
library(GGally)
library(tidyverse)
library(stringr)
library(forecast)
library(pheatmap)
library(pracma)
library(data.table)
library(plyr)
library(psych)
library(mgcv)

options(digits=11)



# Consider a grid in d-dimensional space. There are n grid lines in each dimension, 
# spaced one unit apart. We will consider a walk of m steps from grid intersection 
# to grid intersection. Each step will be a single unit movement in any one of the 
# dimensions, such that it stays on the grid. We wish to look at the number of possible 
# paths from a particular starting location on this grid.


# Q1 ---
# Consider the case where d=4, n=10, and m=10.
# How many valid walks start from the corner (0, 0, 0, 0)?


destinations <- function(x, nvisit, wall) {
  ## takes a coordinate as input and gives out all possible destinations
  ## in a matrix containing coordinates. Coordinate x is of the form
  ## c(0,0,0) for a three dimensional coordinate system
  ## wall denotes the maximum number of steps in one direction (n)
  ## nvisit is a multiplyer denoting how often this node
  ## could be reached to avoid unnecessary redundant computations
  ## it is appended as additional column in the output
  
  # hypothetically possible steps (e.g. valid if in middle of coordinate system)
  steps <- rbind(diag(length(x)), -1*diag(length(x)))
  potential.paths <- repmat(x,2*length(x),1) + steps
  # discard impossible paths and append number paths to get here
  misstep <- apply(potential.paths < 0 | potential.paths > wall, 1, any) 
  possible.paths <- cbind(potential.paths[!misstep,],
                          as.matrix(rep(nvisit,sum(!misstep))))
  return(possible.paths)
}





takewalks <- function(d=2,n=3,m=3,start.pts=t(matrix(c(0,0)))) {
  for (i1 in c(1:size(start.pts,1))) {
    print(start.pts[i1,])
    path <- list()
    nvisits <- list() # keep track of how often I visited a node 
    path[[1]] <- t(as.matrix(start.pts[i1,]))
    nvisits[[1]] <- as.matrix(rep(1,d/d))
    temp.list <- list()
    # loop through steps
    for (istep in c(1:m)) {
      print(istep)
      # loop through origins to find all destination from this step depth
      for (iorig in c(1:size(path[[istep]],1))) {
        # get all possible destination coordinates, and add how often we jumped here
        temp.list[[iorig]] <- destinations(path[[istep]][iorig,], 
                                           nvisits[[istep]][iorig], 
                                           wall=n-1)
      }
      # track how often we visit every node (keep only one for speed's sake)
      mdt <- as.data.table(do.call(rbind, temp.list))
      res <- mdt[,.N, by=names(mdt)]
      res <- data.frame(res)
      res.sorted <- as.matrix(res[,1:(d+1)])
      nvisits[[istep+1]] <- res.sorted[,d+1] * res$N
      res.sorted[,d+1] <- nvisits[[istep+1]]
      res.sorted[,1:d] <- t(apply(res[,1:d],1,sort))
      # collapse over permutations (e.g. 1,2 is equal to 2,1)
      combos <- uniquecombs(res.sorted)
      multiplier <- tabulate(attr(combos, "index"))
      combos <- as.matrix(combos)
      if (size(combos,2) < (d+1)){
        combos <- t(combos)
      }
      path[[istep+1]] <- matrix(combos[,1:d], nrow = size(combos,1), ncol = d)
      nvisits[[istep+1]] <- matrix(combos[,d+1] * multiplier)
      temp.list <- list()
    }
    # remember the total number of possible paths for this starting point
    # and all possible permutations of the starting point
    npaths[i1] <- sum(nvisits[[m+1]])
    nperms[i1] <- size(unique(perms(start.pts[i1,])),1)
  }
  return(list(npaths, nperms))
}






destinations2 <- function(x, wall) {
  ## takes a coordinate as input and gives out all possible destinations
  ## in a matrix containing coordinates. Coordinate x is of the form
  ## c(0,0,0) for a three dimensional coordinate system
  ## wall denotes the maximum number of steps in one direction (n)
  ## nvisit is a multiplyer denoting how often this node
  ## could be reached to avoid unnecessary redundant computations
  ## it is appended as additional column in the output
  
  # hypothetically possible steps (e.g. valid if in middle of coordinate system)
  lenx <- size(x,2)-1
  nvisit <- x[length(x)]
  steps <- rbind(diag(lenx), -1*diag(lenx))
  potential.paths <- repmat(x[1:lenx],2*(lenx),1) + steps
  # discard impossible paths and append number paths to get here
  misstep <- apply(potential.paths < 0 | potential.paths > wall, 1, any) 
  possible.paths <- cbind(potential.paths[!misstep,],
                          as.matrix(rep(nvisit,sum(!misstep))))
  return(possible.paths)
}


# parameters of the question
d = 4  # dimensions
n = 10 # grid lines
m = 9 # steps

# compute all possible paths from c(0,0,0,0)
start_time <- Sys.time() # time execution
path <- list()
nvisits <- list() # keep track of how often I visited a node 
sp <- as.matrix(diag(d)[1:(d/d),]) # starting point 
if (size(sp,1) > size(sp,2)){
  sp <- t(sp)
}
path[[1]] <- sp
nvisits[[1]] <- as.matrix(rep(1,d/d))
temp.list <- list()
# path[[istep+1]] contains the positions we can end up after istep steps
for (istep in c(1:m)) {
  temp.list <- alply(cbind(path[[istep]],nvisits[[istep]]), 1, destinations2, wall=n-1)
  # track how often we visit every node (keep only one for speed's sake)
  mdt <- as.data.table(do.call(rbind, temp.list))
  res <- mdt[,.N, by=names(mdt)]
  nvisits[[istep+1]] <- as.matrix(res$V5 * res$N)
  path[[istep+1]] <- as.matrix(res[,1:d])
  temp.list <- list()
}
## number of valid walks from starting position c(0,0,0,0):
4*sum(nvisits[[m+1]]) ## if I want to know it for m=4, I run it until m=3
end_time <- Sys.time()
end_time - start_time

abc <- cbind(path[[3]],nvisits[[3]])
check <- apply(abc, 1, destinations2, wall=n-1)





# Q2 ---
# Consider the count of valid walks associated with each possible starting position. 
# What is the ratio of the highest count of valid walks to smallest count of valid walks?
start_time <- Sys.time() # time execution
start.pts <- t(as.matrix(c(5,5,5,5)))
npaths <- rep(0,size(start.pts,1))
m = 10
for (i1 in c(1:size(start.pts,1))){
  print(start.pts[i1,])
  path <- list()
  nvisits <- list() # keep track of how often I visited a node 
  path[[1]] <- t(as.matrix(start.pts[i1,]))
  nvisits[[1]] <- as.matrix(rep(1,d/d))
  temp.list <- list()
  # path[[istep+1]] contains the positions we can end up after istep steps
  for (istep in c(1:m)) {
    print(istep)
    for (iorig in c(1:size(path[[istep]],1))){
      # get all possible destination coordinates, and add how often we jumped here
      temp.list[[iorig]] <- destinations(path[[istep]][iorig,], 
                                         nvisits[[istep]][iorig], 
                                         wall=n-1)
    }
    # track how often we visit every node (keep only one for speed's sake)
    mdt <- as.data.table(do.call(rbind, temp.list))
    res <- mdt[,.N, by=names(mdt)]
    nvisits[[istep+1]] <- res$V5 * res$N
    path[[istep+1]] <- as.matrix(res[,1:d])
    temp.list <- list()
  }
  npaths[i1] <- sum(nvisits[[m+1]])
}
end_time <- Sys.time()
end_time - start_time

max.paths <- 1061298332
min.paths <- 44569724
max.paths / min.paths



start_time <- Sys.time() # time execution
lout <- takewalks(d=4,n=10,m=10,start.pts=t(matrix(c(0,0,0,0))))
end_time <- Sys.time()
end_time - start_time
lout




# Q3 ---
# Consider the case where d=4, n=10, and m=10.
# Consider the count of valid walks associated with each possible starting position. 
# What is the ratio of the standard deviation of the number of valid walks to the mean 
# of the number of valid walks?

# There actually aren't all that many starting points I need to check
# If I consider points in a 5x5x5x5 grid, I can simply multiply the result by
# 8, because there are 8 such parts in the entire object
# Moreover, each possible permutation of dimensions, e.g. 1,2,3,4 or 4,3,2,1 
# gives the same value as any other permutation, so I can only check unique
# permutations and remember the number of possible permutations

start.pts <- as.matrix(expand.grid(c(1:(n/2)), c(1:(n/2)), c(1:(n/2)), c(1:(n/2))))
# delete redundant permutations
start.pts <- unique(t(apply(start.pts,1,sort)))
npaths <- rep(0,size(start.pts,1))
nperms <- npaths
m = 10
# loop through points (starting points)
for (i1 in c(1:size(start.pts,1))){
  print(start.pts[i1,])
  path <- list()
  nvisits <- list() # keep track of how often I visited a node 
  path[[1]] <- t(as.matrix(start.pts[i1,]))
  nvisits[[1]] <- as.matrix(rep(1,d/d))
  temp.list <- list()
  # loop through steps
  for (istep in c(1:m)) {
    print(istep)
    # loop through origins to find all destination from this step depth
    for (iorig in c(1:size(path[[istep]],1))){
      # get all possible destination coordinates, and add how often we jumped here
      temp.list[[iorig]] <- destinations(path[[istep]][iorig,], 
                                         nvisits[[istep]][iorig], 
                                         wall=n-1)
    }
    # track how often we visit every node (keep only one for speed's sake)
    mdt <- as.data.table(do.call(rbind, temp.list))
    res <- mdt[,.N, by=names(mdt)]
    nvisits[[istep+1]] <- res$V5 * res$N
    path[[istep+1]] <- as.matrix(res[,1:d])
    temp.list <- list()
  }
  # remember the total number of possible paths for this starting point
  # and all possible permutations of the starting point
  npaths[i1] <- sum(nvisits[[m+1]])
  nperms[i1] <- size(unique(perms(start.pts[i1,])),1)
}
end_time <- Sys.time()
end_time - start_time

# to get the distribution of paths over starting values, repeat by permutations
# of starting values
path.distribution <- rep(npaths, nperms)


# What is the ratio of the standard deviation of the number of valid walks to the mean 
# of the number of valid walks?
sd(path.distribution) / mean(path.distribution)




# Q4 ---
# Now, let's consider the case where d=8, n=12, and m=12.
# How many valid walks start from one of the corners?

# My god... my script is useless!!!

# Maybe I should guess-timate it? 


#start.pts <- as.matrix(expand.grid(c(1:(n/2)), c(1:(n/2)), c(1:(n/2)), c(1:(n/2)), 
#                                         c(1:(n/2)), c(1:(n/2)), c(1:(n/2)), c(1:(n/2))))
# delete redundant permutations
#start.pts <- unique(t(apply(start.pts,1,sort)))


d = 8
n = 12
m = 12
start.pts <- t(as.matrix(c(6,6,6,6,6,6,6,6)))
npaths <- rep(0,size(start.pts,1))
nperms <- npaths
# loop through points (starting points)
start_time <- Sys.time() # time execution
for (i1 in c(1:size(start.pts,1))){
  print(start.pts[i1,])
  path <- list()
  nvisits <- list() # keep track of how often I visited a node 
  path[[1]] <- t(as.matrix(start.pts[i1,]))
  nvisits[[1]] <- as.matrix(rep(1,d/d))
  temp.list <- list()
  # loop through steps
  for (istep in c(1:m)) {
    print(istep)
    # loop through origins to find all destination from this step depth
    for (iorig in c(1:size(path[[istep]],1))){
      # get all possible destination coordinates, and add how often we jumped here
      temp.list[[iorig]] <- destinations(path[[istep]][iorig,], 
                                         nvisits[[istep]][iorig], 
                                         wall=n-1)
    }
    # track how often we visit every node (keep only one for speed's sake)
    mdt <- as.data.table(do.call(rbind, temp.list))
    res <- mdt[,.N, by=names(mdt)]
    nvisits[[istep+1]] <- res$V9 * res$N  ################ change to V(d-1)
    res.sorted <- as.matrix(res[,1:(d+1)])
    res.sorted[,d+1] <- nvisits[[istep+1]]
    res.sorted[,1:d] <- t(apply(res[,1:d],1,sort))
    # collapse over permutations (e.g. 1,2 is equal to 2,1)
    combos <- uniquecombs(res.sorted)
    multiplier <- tabulate(attr(combos, "index"))
    combos <- as.matrix(combos)
    if (size(combos,2) < (d+1)){
      combos <- t(combos)
    }
    path[[istep+1]] <- matrix(combos[,1:d], nrow = size(combos,1), ncol = d)
    nvisits[[istep+1]] <- matrix(combos[,d+1] * multiplier)
    temp.list <- list()
  }
  # remember the total number of possible paths for this starting point
  # and all possible permutations of the starting point
  npaths[i1] <- sum(nvisits[[m+1]])
  nperms[i1] <- size(unique(perms(start.pts[i1,])),1)
}
end_time <- Sys.time()
end_time - start_time

print(npaths)

# to get the distribution of paths over starting values, repeat by permutations
# of starting values
path.distribution <- rep(npaths, nperms)


max.paths <- 281407110705872## inputting c(6,6,6,6,6,6,6,6)
min.paths <- 2479092118264 ## inputting c(0,0,0,0,0,0,0,0)
max.paths / min.paths













