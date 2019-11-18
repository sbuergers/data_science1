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
  ## Input: d = number of dimensions
  ##        n = number of grid lines
  ##        m = number of steps
  ##        start.pts = matrix of starting coordinates (ncoord x ndim)
  ## Output: list(npaths, nperms)
  ##        npaths = the number of possible paths of m steps in this grid
  ##        nperms = the number of permutations of those coordinates in the grid
  ##                 this number can be seen as the number of equivalent starting points
  ##                 rotated in some way
  npaths <- rep(0,size(start.pts,1))
  nperms <- npaths
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







# Q1 ---
# Consider the case where d=4, n=10, and m=10.
# How many valid walks start from the corner (0, 0, 0, 0)?
start_time <- Sys.time() # time execution
lout <- takewalks(d=4,n=10,m=10,start.pts=t(matrix(c(0,0,0,0))))
end_time <- Sys.time()
end_time - start_time
lout





# Q2 ---
# Consider the count of valid walks associated with each possible starting position. 
# What is the ratio of the highest count of valid walks to smallest count of valid walks?

# most central point
start_time <- Sys.time() # time execution
lout <- takewalks(d=4,n=10,m=10,start.pts=t(as.matrix(c(5,5,5,5))))
end_time <- Sys.time()
end_time - start_time
lout

# corner (from before)
start_time <- Sys.time() # time execution
lout <- takewalks(d=4,n=10,m=10,start.pts=t(matrix(c(0,0,0,0))))
end_time <- Sys.time()
end_time - start_time
lout

max.paths <- 1061298332
min.paths <- 44569724
max.paths / min.paths



# Q3 ---
# Consider the case where d=4, n=10, and m=10.
# Consider the count of valid walks associated with each possible starting position. 
# What is the ratio of the standard deviation of the number of valid walks to the mean 
# of the number of valid walks?

# There actually aren't all that many starting points I need to check
# If I consider points in a 4x4x4x4 grid, I can simply multiply the result by
# 8, because there are 8 such parts in the entire object 
# Moreover, each possible permutation of dimensions, e.g. 1,2,3,4 or 4,3,2,1 
# gives the same value as any other permutation, so I can only check unique
# permutations and remember the number of possible permutations
d = 4
n = 10
m = 10
start.pts <- as.matrix(expand.grid(c(0:(n/2-1)), c(0:(n/2-1)), c(0:(n/2-1)), c(0:(n/2-1))))
# delete redundant permutations
start.pts <- unique(t(apply(start.pts,1,sort)))

start_time <- Sys.time() # time execution
lout <- takewalks(d=d,n=n,m=m,start.pts=start.pts)
end_time <- Sys.time()
end_time - start_time
lout

# to get the distribution of paths over starting values, repeat by permutations
# of starting values
path.distribution <- rep(lout[[1]], lout[[2]])

# What is the ratio of the standard deviation of the number of valid walks to the mean 
# of the number of valid walks?
sd(path.distribution) / mean(path.distribution)




# Q4 ---
# Now, let's consider the case where d=8, n=12, and m=12.
# How many valid walks start from one of the corners?

# And

# Q5 --- 
# Consider the count of valid walks associated with each 
# possible starting position. What is the ratio of the highest 
# count of valid walks to smallest count of valid walks?


#start.pts <- as.matrix(expand.grid(c(1:(n/2)), c(1:(n/2)), c(1:(n/2)), c(1:(n/2)), 
#                                         c(1:(n/2)), c(1:(n/2)), c(1:(n/2)), c(1:(n/2))))
# delete redundant permutations
#start.pts <- unique(t(apply(start.pts,1,sort)))


d = 8
n = 12
m = 12

# most central point
start_time <- Sys.time() # time execution
lout <- takewalks(d=d,n=n,m=m,start.pts=t(as.matrix(c(6,6,6,6,6,6,6,6))))
end_time <- Sys.time()
end_time - start_time
lout

# corner 
start_time <- Sys.time() # time execution
lout <- takewalks(d=d,n=n,m=m,start.pts=t(matrix(c(0,0,0,0,0,0,0,0))))
end_time <- Sys.time()
end_time - start_time
lout

max.paths <- 281407110705872## inputting c(6,6,6,6,6,6,6,6)
min.paths <- 2479092118264 ## inputting c(0,0,0,0,0,0,0,0)
max.paths / min.paths







# Q6 ---
# Consider the count of valid walks associated with each possible starting position. 
# What is the ratio of the standard deviation of the number of valid walks to the mean 
# of the number of valid walks?

# clear memory
# rm(list = ls())

d = 8
n = 12
m = 12

# This is going to be rough... it already took approximately 10 minutes to get
# the path for the central point (c(6,6,6,6,6,6,6,6))
start.pts <- as.matrix(expand.grid(c(0:(n/2-1)), c(0:(n/2-1)), c(0:(n/2-1)), c(0:(n/2-1)),
                                   c(0:(n/2-1)), c(0:(n/2-1)), c(0:(n/2-1)), c(0:(n/2-1))))
# delete redundant permutations
start.pts <- unique(t(apply(start.pts,1,sort)))


# I do not have enough computing time for all starting points,
# and I quickly get memory errors, so simply estimate approximately
# using trends from smaller to larger step sizes

sample_ids <- round(linspace(1, size(start.pts,1), n=50))
start_time <- Sys.time() # time execution
npaths <- rep(-1, size(start.pts,1))
nperms <- rep(-1, size(start.pts,1))
# loop through coordinates
for (isp in c(1:length(sample_ids))) {
  lout <- takewalks(d=d,n=n,m=7,start.pts=t(as.matrix(start.pts[sample_ids[isp],])))
  npaths[sample_ids[isp]] <- lout[[1]] 
  nperms[sample_ids[isp]] <- lout[[2]]
}
end_time <- Sys.time()
end_time - start_time
npaths[npaths!=-1]
nperms[nperms!=-1]

# to get the distribution of paths over starting values, repeat by permutations
# of starting values
path.distribution <- rep(npaths[npaths!=-1], nperms[nperms!=-1])

# What is the ratio of the standard deviation of the number of valid walks to the mean 
# of the number of valid walks?
sd(path.distribution) / mean(path.distribution)



vm <- rep(-1,10)
vm[1] <- 0.13415871763
vm[2] <- 0.18916184721
vm[3] <- 0.24143252252
vm[4] <- 0.28486226144
vm[5] <- 0.32400130322 # 6steps
vm[6] <- 0.35954532056 # 7steps

tm <- c(2,3,4,5,6,7)

# fit exponential model to forecast ratio at m=12
# Prepare a good inital state
y <- vm[vm!=-1]
x <- tm
data.df <- data.frame(x,y)
theta.0 <- 1.1
model.0 <- lm(log(- y + theta.0) ~ x, data=data.df)
alpha.0 <- -exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)

# Fit the model
model <- nls(y ~ alpha * exp(beta * x) + theta , data = data.df, start = start)

# plot original data and predicted data
xpred <- c((max(tm)):12)
plot(c(data.df$x,xpred), c(data.df$y,predict(model, list(x = xpred))))
lines(data.df$x, predict(model, list(x = data.df$x)), col = 'skyblue', lwd = 3)
lines(xpred, predict(model, list(x = xpred)), col = 'red', lwd = 3)

predict(model, list(x = 12))



# eof








