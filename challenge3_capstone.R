# Capstone project for the Data incubator
#
# sbuergers@gmail.com
# created on 04/11/2019

# data from https://osf.io/s46pr/
# You can read more about it in the associated paper: https://psyarxiv.com/h8tju
#
# It is a large collection of scientific studies investigating confidence, a measure 
# of metacognition, or the ability to assess one's own cognitive activity in a variety of domains
# including memory, perception and reasoning. 
#
# While this data set is a treasure trough for academic research, it also provides potentially
# powerful insights for businesses, policy makers and educational institutions. 
# 
# For example, evidence suggests that the brain has dedicated processing resources for both
# domain general and domain specific metacognition. This dataset might help identify the boundaries
# of these distinct systems - i.e. is there a designated system for memory, perception and reasoning? 
# How is metacognition affected by different task parameters, e.g. task difficulty? Are these factors
# constant over different task domains? 
#
# How is confidence affected as a function of time on task? And how does a person's metacognitive ability
# relate to performance - is it accurate or are participants over- or underconfident? What factors affect
# this metacognitive efficiency? 
#
# Understanding the generality of metacognition, and what factors influence confidence can have broad 
# implications for school curricula and educational policy, the design of training programs and motivational
# psychology. 



#
# 1.) Pre-process and explore the data
#
rm(list())

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


# get data from all experiments
datdir <- "D:/data science/confidence/Confidence Database/Confidence Database/"
files <- list.files(datdir, pattern = "data")
dlist <- list()
for (i in 1:length(files)) {
  fprintf("Loading dataset  %i\n", i  )
  dlist[[i]] <- read.csv(paste(datdir, files[[i]], sep = ""))
}


# show column labels for each dataset
lapply(dlist, colnames)

# A little bit messy, do not have time to clean it right now, have to do this 
# properly and calmly anyway...
# But let's make two nice figures using a subset of data to show that my dataset
# is actually interesting

# the authors of the pre-print make it easier for us, they provide some R scripts
# on the OSF website
# Use their code to merge datasets quickly

library(here);library(pwr)

fastmerge <- function(d1, d2) {
  d1.names <- names(d1)
  d2.names <- names(d2)
  
  # columns in d1 but not in d2
  d2.add <- setdiff(d1.names, d2.names)
  
  # columns in d2 but not in d1
  d1.add <- setdiff(d2.names, d1.names)
  
  # add blank columns to d2
  if(length(d2.add) > 0) {
    for(i in 1:length(d2.add)) {
      d2[d2.add[i]] <- NA
    }
  }
  
  # add blank columns to d1
  if(length(d1.add) > 0) {
    for(i in 1:length(d1.add)) {
      d1[d1.add[i]] <- NA
    }
  }
  
  return(rbind(d1, d2))
}


#Get the names of the files that will be read in
data_files <- list.files(path=datdir,pattern = "^data_*", recursive = FALSE)
readme_files <- list.files(path=datdir,pattern = "^readme_*", recursive = FALSE)

#pre-allocate empty variables 
rt_conf <- rep(NA,0);rtconf_conf <- rep(NA,0);p_outlier1 <- rep(NA,0);p_outlier2 <- rep(NA,0);which_data_included <- rep(NA,0)
counter <- 1 #participant counter
counter2 <- 1 #study counter

conf.list <- list()
RTconf.list <- list()
RT.list <- list()
npoints <- 10 # downsample time series to 15 points for every participant (walk through
# experiment in npoints steps no matter how long it took actually)
#Loop over all files
for(i in 1:length(data_files)){
  Data <- read.csv(paste0(datdir,data_files[i]),fileEncoding="UTF-8-BOM")
  
  #Only include data with the variables RT_conf, RT_dec and Confidence
  if(any(names(Data)=="RT_conf")&any(names(Data)=="RT_dec"&any(names(Data)=="Confidence"))){
    
    #loop over subjects in this dataset
    subs <- unique(Data$Subj_idx)  
    for(j in 1:length(subs)){
      temp <- subset(Data,Subj_idx==subs[j])
      
      #compute standardized RTs
      temp$RT_dec_z <- scale(temp$RT_dec)
      temp$RT_conf_z <- scale(temp$RT_conf)
      
      #exclude trials with NaNs for each of these variables
      n1 <- dim(temp)[1]
      temp <- temp[complete.cases(temp$Confidence),]
      temp <- temp[complete.cases(temp$RT_dec),]
      temp <- temp[complete.cases(temp$RT_conf),]
      #exclude trials with RT==0 and slower than 5s
      temp <- subset(temp, c(RT_dec > 0.01 & RT_dec < 5))
      temp <- subset(temp, c(RT_conf > 0.01 & RT_conf < 5))
      p_outlier1[counter] <- dim(temp)[1]/n1 #how many data is retained after 100ms < RT < 5s
      
      #exclude trials exceeding + or - 3sds from the grand average
      n2 <- dim(temp)[1]
      temp <- subset(temp, c(RT_dec_z < 3 & RT_dec_z > -3))
      temp <- subset(temp, c(RT_conf_z < 3 & RT_conf_z > -3))
      p_outlier2[counter] <- dim(temp)[1]/n2 #how many data is retained after excluding 3sds
      
      #Only compute correlations for participants with variation in confidence
      if(length(unique(temp$Confidence))>1){
        #An (absolute) minimum of 3 trials is needed to compute correlations
        if(dim(temp)[1]>20){
          
          #Compute correlations Confidence/Choice RT, and confidence
          temp$Confidence <- as.numeric(temp$Confidence)
          rt_conf[counter] <- cor(temp$RT_dec,temp$Confidence)
          rtconf_conf[counter] <- cor(temp$RT_conf,temp$Confidence)
          
          # Compute confidence, RT and RTconf as a function of time
          # for simplicity sake take moving average to get 25 datapoints for each ppt
          trlid <- c(1:dim(temp)[1])
          ma.conf <- movavg(temp$Confidence, n=round(length(trlid)/npoints), type = "s")
          conf.list[[counter]] <- ma.conf[round(linspace(1,length(trlid), npoints))]
          ma.RTconf <- movavg(temp$RT_conf, n=round(length(trlid)/npoints), type = "s")
          RTconf.list[[counter]] <- ma.RTconf[round(linspace(1,length(trlid), npoints))]
          ma.RT <- movavg(temp$RT_dec, n=round(length(trlid)/npoints), type = "s")
          RT.list[[counter]] <- ma.RT[round(linspace(1,length(trlid), npoints))]
          
          counter <- counter+1
        }
      }
      j<-j+1
    }
    which_data_included[counter2] <- i; counter2 <- counter2+1
  }
  print(paste('Computing dataset ',i,'from ',length(data_files)))
}

#Which data are included
data_files[which_data_included]
length(which_data_included) #studies included
length(data_files) #studies in the database
length(rtconf_conf) #final N


# Plot
conf.list[sapply(conf.list, is.null)] <- NULL
confidence <- sapply(conf.list, rbind)
RTconf.list[sapply(RTconf.list, is.null)] <- NULL
confidenceRT <- sapply(RTconf.list, rbind)
time.in.percent <- linspace(0,100,10)
plot(time.in.percent,apply(confidence, 1, mean), xlab="Time on task (in %)", ylab = "Confidence")
plot(time.in.percent,apply(confidenceRT, 1, mean), xlab="Time on task (in %)", ylab = "Confidence RT")



# eof
