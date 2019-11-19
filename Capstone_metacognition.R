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
library(knitr)
library(summarytools)
library(rccmisc)
library(readxl)


options(digits=11)


# get data from all experiments
datdir_raw <- "D:/data science/confidence/Confidence Database/Confidence Database/"
datdir_sav <- "D:/data science/confidence/Confidence Database/"
files <- list.files(datdir_raw, pattern = "data")
dlist <- list()
for (i in 1:length(files)) {
  fprintf("Loading dataset  %i\n", i  )
  dlist[[i]] <- read.csv(paste(datdir_raw, files[[i]], sep = ""))
  # add exp_idx column to map to dinfo df
  dlist[[i]]$exp_idx <- as.factor(rep(i,dim(dlist[[i]])[1]))
}

# get database information (task type for example)
dinfo <- read_excel(paste(datdir_sav, "Database_Information.xlsx", sep = ""))

# add exp_idx to merge with trial df as needed
dinfo$exp_idx <- as.factor(1:dim(dinfo)[1])



## First, we need to clean the data a little bit: Make sure column names are 
## matched between studies and variables are scaled similarly


# All datasets should have the following columns:
# >> Subj_idx (the subject index, preferably in 1:N format; do not use participant initials)
# >> Stimulus (this could be numeric or string; for 2AFC designs, it may be necessary to include two fields 
#    [e.g., Stim1 and Stim2] corresponding to the two stimuli presented)
# >> Response (this should have the same format -numeric or string -as the Stimulus field)
# >> Confidence
# >> RT_dec (RT of decision in seconds; if decision and confidence are given at the same time, 
#    this should instead be named RT_decConf)
# >> RT_conf (RT of the confidence in seconds; this should be the time taken after the decision is made 
#    in experiments where decision and confidence were given separately; if RT for confidence was not recorded, 
#    do not include this column but make a note about it in the readme file)
#
# In addition, if applicable, submissions should have the following extra columns:
# >> Contrast/coherence/noise level/difficulty (an indication of the difficulty in the given trial; 
#    this could be on an abstract scale [e.g. 1-3 if there are 3 contrast levels] or the raw contrast/coherence/etc 
#    can be reported)
# >> Condition (if more than one condition is present [e.g., TMS vs. no-TMS], then the condition can be indicated here)
# >> Accuracy (this can usually be inferred from the Stimulus and Response columns but not always)
# >> Training (if training data are included, then this field can be used to indicate whether each trial is from the 
#    training or the real experiment; if this field is not included, then it is assumed that none of the trials 
#    reported are training trials)
# >> Stim alternatives (if different stimulus alternatives are present on different trials)

# show column labels used over all datasets and their respective frequency
dlist <- dlist %>% lapply(lownames)
dlist %>% lapply(colnames) %>% unlist(recursive = FALSE) %>% table()

# Check for mandatory columns one by one:

col_names <- dlist %>% lapply(colnames) %>% unlist(recursive = FALSE) %>% unique

# Find fuzzy matched to column names of interest and replace them
# 1.) Find matches
req_cols <- list("subj_idx", "stimulus", "response", "confidence")
candidate_strings <- list()
for (i in 1:length(req_cols)) {
  candidate_strings[[i]] <- col_names[agrepl(req_cols[[i]], col_names)]
}
print(candidate_strings)
# For subj ID this looks very reasonable, for the others not so much, so let's
# only fill in subj_idx

# 2.) replace column names
d <- dlist
for (i in 1) {                 # loop through columns
  for (j in 1:length(candidate_strings[[i]])) { # and potential fuzzy string matches
    for (k in 1:length(dlist)) {                # for each data frame replace with unique proper label
      names(d[[k]])[names(d[[k]]) == candidate_strings[[i]][j]] <- req_cols[[i]]
    }
  }
}
col_names2 <- d %>% lapply(colnames) %>% unlist(recursive = FALSE) %>% unique
d %>% lapply(colnames) %>% unlist(recursive = FALSE) %>% table()

# This solved it for subj_idx for all 145 datasets


# I only have 139 datasets with a confidence column, but loosing 6 for the sake of time
# seems reasonable. 
has_conf_column <- logical(length(d))
for (i in 1:length(d)){
  has_conf_column[i] <- is.element("confidence", lapply(d, colnames)[[i]])
}
d[!has_conf_column] <- NULL
length(d)


# For simplicity I will also delete datasets that miss response or stimulus
has_resp_column <- logical(length(d))
has_stim_column <- logical(length(d))
for (i in 1:length(d)){
  has_resp_column[i] <- is.element("response", lapply(d, colnames)[[i]])
  has_stim_column[i] <- is.element("stimulus", lapply(d, colnames)[[i]])
}
sum(has_resp_column)
sum(has_stim_column)
d[!has_resp_column | !has_stim_column] <- NULL
length(d)

# I still have 138 datasets, so pretty good!



# Let's explore RT. When there are two responses, one for the main decision and one for
# confidence we should have 2 columns: rt_dec, rt_conf. When the response to both
# questions is made at the same time we should have one column: rt_decconf
list.RT <- list()
for (i in 1:length(d)) {
  list.RT[[i]] <- d[[i]] %>% select(starts_with("rt_"))
}
list.RT %>% lapply(colnames) %>% unlist(recursive = FALSE) %>% table()

# replace rt_confidence with rt_conf
# replace rt_decision with rt_dec
for (i in 1:length(list.RT)) {
  list.RT[[i]] <- rename_all(list.RT[[i]], recode, rt_confidence = "rt_conf", rt_decision = "rt_dec")
}
# add rt_dec and rt_conf for datasets with rt_decconf (they will be identical)
# and add column rt_numresp which is either 1 (for rt_decconf) or 2 (rt_dec and rt_conf)
for (i in 1:length(list.RT)) {
  if (is.element("rt_decconf", colnames(list.RT[[i]]))){
    list.RT[[i]] <- mutate(list.RT[[i]], rt_dec = rt_decconf, rt_conf = rt_decconf)
  } else {
    list.RT[[i]] <- mutate(list.RT[[i]], rt_decconf = rep(NA, dim(list.RT[[i]])[1]))
  }
}


# drop all other columns from RT data frames (keep rt_dec, rt_conf, rt_decconf) and
# drop whole data frame when these columns are not all available
del.df <- logical(length(list.RT))
for (i in 1:length(list.RT)) {
  if (any(!c("rt_dec", "rt_conf", "rt_decconf") %in% colnames(list.RT[[i]]))) {
    del.df[i] <- TRUE
  } else {
    list.RT[[i]] <- list.RT[[i]] %>% select(c("rt_dec", "rt_conf", "rt_decconf"))
  }
}
list.RT[del.df] <- NULL
list.RT %>% lapply(colnames) %>% unlist(recursive = FALSE) %>% table()
size(list.RT)


d[!del.df] %>% lapply(colnames) %>% unlist(recursive = FALSE) %>% table()


# get data to work with, using only basic columns:
# subj_idx, stimulus, response, confidence, rt_dec, rt_conf, rt_decconf, nresp
list.fin <- d[!del.df]
for (i in 1:length(list.RT)) {
  list.fin[[i]] <- cbind(subset(list.fin[[i]], select = c("exp_idx", "subj_idx", "stimulus", "response", "confidence")), 
                         list.RT[[i]])
}


# 
# scaled.list <- list()
# for(i in 1:length(list.fin)){
#   # loop over subjects in this dataset
#   subs <- unique(list.fin[[i]]$subj_idx)  
#   subj.list <- list()
#   for(j in 1:length(subs)){
#     
#     # for current experiment and subject,
#     # choose RT trials between 0.01 and 5s for both dec and conf
#     # and accumulate in list (subj.list)
#     subj.list[[j]] <- subset(list.fin[[i]], 
#                  (subj_idx == subs[j]) &
#                  (rt_dec > 0.01 & rt_dec < 5) & 
#                  (rt_conf > 0.01 & rt_conf < 5 ))
#     
#     # delete rows with missing values for response, confidence, rt_dec, rt_conf
#     subj.list[[j]] <- subj.list[[j]][complete.cases(subj.list[[j]][ , 3:6]),]
#     
#     # then compute standardized RTs and Confidence. Use median rather than mean for centering
#     subj.list[[j]]$rt_dec <- scale(subj.list[[j]]$rt_dec)
#     subj.list[[j]]$rt_conf <- scale(subj.list[[j]]$rt_conf)
#     subj.list[[j]]$rt_decconf <- scale(subj.list[[j]]$rt_decconf)
#     subj.list[[j]]$confidence <- scale(subj.list[[j]]$confidence)
#   }
#   print(paste('Computing dataset ',i,'from ',length(exps)))
#   scaled.list[[i]] <- rbindlist(subj.list)
# }
# df.new <- rbindlist(scaled.list)


# Finally, merge all dataframe into one
df <- rbindlist(list.fin)
dim(df)
head(df)


# Add column for number of responses
df$nresp <- rep(1,dim(df)[1])
df$nresp[is.na(df$rt_decconf)] <- rep(2,sum(is.na(df$rt_decconf)))


# delete rows with missing values for response, confidence, rt_dec, rt_conf
df <- df[complete.cases(df[ , 3:6]),]
dim(df)
head(df)


# convert to numeric values where sensible (instead of factor)
lapply(df, class)
levels(df$confidence)
levels(df$response)
df$confidence <- as.numeric(df$confidence)


# delete trials with unrealistic RTs
df <- subset(df, (df$rt_dec > 0.01 & df$rt_dec < 5) | 
          (df$rt_conf > 0.01 & df$rt_conf < 5 ))


# Check confidence scales by experiment
theme_set(theme_bw())
g <- ggplot(df, aes(as.factor(exp_idx), confidence, na.rm = TRUE))
g + geom_boxplot()


# loop through experiments and participants to
# scale confidence equally and normalize RTs
exps <- unique(df$exp_idx) 
for(i in 1:length(exps)){
  # loop over subjects in this dataset
  subs <- unique(df$subj_idx[df$exp_idx == exps[i]])  
  for(j in 1:length(subs)){
    
    # current experiment and subject
    curid <- df$exp_idx == exps[i] & df$subj_idx == subs[j]
    
    # compute standardized RTs and Confidence
    df$rt_dec[curid] <- scale(df$rt_dec[curid])
    df$rt_conf[curid] <- scale(df$rt_conf[curid])
    df$rt_decconf[curid] <- scale(df$rt_conf[curid])
    df$confidence[curid] <- scale(df$confidence[curid])
  }
  print(paste('Computing dataset ',i,'from ',length(exps)))
}
#dfbu <- df


# delete trials with standard deviations more extreme than 3
df <- subset(df, (df$rt_dec > (-3) & df$rt_dec < 3) & 
               (df$rt_conf > (-3) & df$rt_conf < 3 ))


## Save data for convience
write.csv(df, paste(datdir_sav, "data_preproc"))








## --------------------------------------------------------------
# clean up
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
library(knitr)
library(summarytools)
library(rccmisc)
library(TTR)
library(reshape2)
library(ggpubr)




## load preprocessed data
datdir_sav <- "D:/data science/confidence/Confidence Database/"
df_full <- read.csv(paste(datdir_sav, "data_preproc"))
head(df_full)
describe(df_full)

# get database information (task type for example)
dinfo <- read_excel(paste(datdir_sav, "Database_Information.xlsx", sep = ""))
dinfo$exp_idx <- as.factor(1:dim(dinfo)[1])


df <- df_full %>%
  subset(select=c("exp_idx", "subj_idx", "confidence", "rt_dec", "rt_conf", "rt_decconf", "stimulus", "response"))
df$acc <- as.numeric(df$stimulus) == as.numeric(df$response)

# check confidence scales again
g <- ggplot(df, aes(as.factor(exp_idx), confidence, na.rm = TRUE))
g + geom_boxplot()


# delete extreme confidence values
df <- subset(df, (df$confidence > (-3) & df$confidence < 3))
describe(df)


# check confidence scales again
g <- ggplot(df, aes(as.factor(exp_idx), confidence, na.rm = TRUE))
g + geom_boxplot()


# delete studies with few trials per participant - require a minimum of 80
head(dinfo)
plot(dinfo$Min_trials_per_subject)
exp_with_suff_trls <- dinfo$exp_idx[dinfo$Min_trials_per_subject >= 80]
dinfo <- subset(dinfo, exp_idx %in% exp_with_suff_trls)
df <- subset(df, exp_idx %in% exp_with_suff_trls)


# also match dataframes otherwise
exp_in_data <- as.factor(unique(df$exp_idx))
dinfo <- subset(dinfo, exp_idx %in% exp_in_data)

## Collapse data over subjects
df_summary <- df %>%
  group_by(exp_idx,subj_idx) %>% 
  summarise_each(funs(mean))
head(df_summary)

# check confidence at the subject level
g <- ggplot(df_summary, aes(as.factor(exp_idx), confidence, na.rm = TRUE))
g + geom_boxplot()
#g + geom_jitter(shape=1, position=position_jitter(0.2))


# subject level data with exp info
df_subjlvl <- merge(df_summary, subset(dinfo, select=c("exp_idx", "Category", "Name_in_database", "Journal", "Year", "Stimuli", "Num_subjects", "Min_trials_per_subject")), by="exp_idx")
g <- ggplot(df_subjlvl, aes(x=confidence, y=rt_conf, color=Category)) +
  geom_point(size=.1, shape=23) +
  geom_smooth()
g


# experiment level data with exp info
df_explvl <- df_summary %>%
  group_by(exp_idx) %>% 
  summarise_each(funs(mean))
df_explvl <- merge(df_explvl, subset(dinfo, select=c("exp_idx", "Category", "Name_in_database", "Journal", "Year", "Stimuli", "Num_subjects", "Min_trials_per_subject")), by="exp_idx")
head(df_explvl)

g <- ggplot(df_explvl, aes(x=confidence, y=rt_conf, color=Category)) +
  geom_point(size=2, shape=1)
g

# number of ppts per experiment
df_numsubj <- ddply(df,~exp_idx,summarise,nsubj=length(unique(subj_idx)))
ggplot(df_numsubj, aes(as.factor(exp_idx), nsubj, na.rm = TRUE)) +
  geom_bar(stat = "identity")

# it seems somewhat unlikely that experiments have > 100 participants, but 
# I don't really have time to explore this further right now


## What do the RT distributions look like?
# 1. RT decision
g <- ggplot(df_summary, aes(as.factor(exp_idx), rt_dec, na.rm = TRUE))
g + geom_boxplot()

# 2. RT confidence
g <- ggplot(df_summary, aes(as.factor(exp_idx), rt_conf, na.rm = TRUE))
g + geom_boxplot()

# 3. RT decconf
g <- ggplot(df_summary, aes(as.factor(exp_idx), rt_decconf, na.rm = TRUE))
g + geom_boxplot()


# Add unique index (exp_idx x subj_idx)
df$idx <- rep(0, dim(df)[1])
del.rows <- df$idx < 0
counter <- 1
exps <- unique(df$exp_idx) 
for(i in 1:length(exps)){
  data <- subset(df, exp_idx == exps[i])
  subs <- unique(data$subj_idx)  
  for(j in 1:length(subs)){
    # delete subject with only a single confidence response
    curid <- df$exp_idx == exps[i] & df$subj_idx == subs[j]
    if(length(unique(df$confidence[curid]))<=1){
      del.rows[curid] <- TRUE
    } else {
      df$idx[curid] <- rep(counter, sum(curid))
      counter <- counter + 1
    }
  }  
  print(paste('Computing dataset ',i,'from ',length(exps)))
} 

df <- df[!del.rows,]

## Save data for convience
write.csv(df, paste(datdir_sav, "data_preproc2"))






#pre-allocate empty variables 
rt_conf <- rep(NA,0);rtconf_conf <- rep(NA,0);p_outlier1 <- rep(NA,0);p_outlier2 <- rep(NA,0);which_data_included <- rep(NA,0)
counter <- 1 #participant counter
counter2 <- 1 #study counter

## get estimates of confidence and RT over the course of the experiment
conf.list <- list()
rtconf.list <- list()
rt.list <- list()
acc.list <- list()
acc_conf.list <- list()
npoints <- 25 # downsample time series to X points for every participant (walk through
# experiment in npoints steps no matter how long it took actually)
# Loop over all files
idx <- c()
exps <- unique(df$exp_idx) 
for(i in 1:length(exps)){
  data <- subset(df, exp_idx == exps[i])
  
  #loop over subjects in this dataset
  subs <- unique(data$subj_idx)  
  for(j in 1:length(subs)){
    temp <- subset(data, subj_idx == subs[j])
    #Only compute correlations for participants with variation in confidence
    if(length(unique(temp$confidence))>1){
      #An (absolute) minimum of 3 trials is needed to compute correlations
      
      #Compute correlations Confidence/Choice RT, and confidence
      temp$confidence <- as.numeric(temp$confidence)
      temp$acc <- as.numeric(temp$acc)
      rt_conf[counter] <- cor(temp$rt_dec,temp$confidence)
      rtconf_conf[counter] <- cor(temp$rt_conf,temp$confidence)
      
      # Compute confidence, RT and RTconf as a function of time
      # for simplicity sake take moving average to get 25 datapoints for each ppt
      trlid <- c(1:dim(temp)[1])
      ma.conf <- movavg(temp$confidence, n=round(length(trlid)/npoints), type = "s")
      conf.list[[counter]] <- ma.conf[round(linspace(1,length(trlid), npoints))]
      ma.rtconf <- movavg(temp$rt_conf, n=round(length(trlid)/npoints), type = "s")
      rtconf.list[[counter]] <- ma.rtconf[round(linspace(1,length(trlid), npoints))]
      ma.rt <- movavg(temp$rt_dec, n=round(length(trlid)/npoints), type = "s")
      rt.list[[counter]] <- ma.rt[round(linspace(1,length(trlid), npoints))]
      ma.acc <- movavg(temp$acc, n=round(length(trlid)/npoints), type = "s")
      acc.list[[counter]] <- ma.acc[round(linspace(1,length(trlid), npoints))]
      ma.acc_conf <- runCor(temp$acc, temp$conf, n=round(length(trlid)/npoints))
      acc_conf.list[[counter]] <- ma.acc_conf[round(linspace(1,length(trlid), npoints))]
      
      counter <- counter+1
      idx <- c(idx, rep(temp$idx[1],25))
    }
  }
  print(paste('Computing dataset ',i,'from ',length(unique(df$exp_idx))))
}

#Which data are included
length(rtconf_conf) #final N


# Plot
conf.list[sapply(conf.list, is.null)] <- NULL
confidence <- sapply(conf.list, rbind)
rtconf.list[sapply(rtconf.list, is.null)] <- NULL
confidenceRT <- sapply(rtconf.list, rbind)
rt.list[sapply(rt.list, is.null)] <- NULL
decRT <- sapply(rt.list, rbind)
acc.list[sapply(acc.list, is.null)] <- NULL
accuracy <- sapply(acc.list, rbind)
acc_conf.list[sapply(acc_conf.list, is.null)] <- NULL
acc_conf_cor <- sapply(acc_conf.list, rbind)
time.in.percent <- linspace(0,100,25)
plot(time.in.percent,apply(confidence, 1, mean), xlab="Time on task (in %)", ylab = "Confidence")
plot(time.in.percent,apply(confidenceRT, 1, mean), xlab="Time on task (in %)", ylab = "Confidence RT")

## combine data from info df and performance df over time
df.conf <- melt(confidence)
colnames(df.conf) <- c("time", "subj", "conf")
df.confRT <- melt(confidenceRT)
colnames(df.confRT) <- c("time", "subj", "confRT")
df.RT <- melt(decRT)
colnames(df.RT) <- c("time", "subj", "decRT")
df.acc <- melt(accuracy)
colnames(df.acc) <- c("time", "subj", "acc")
df.acc_conf_cor <- melt(acc_conf_cor)
df.time <- cbind(df.conf, df.confRT$confRT, df.RT$decRT, df.acc$acc, df.acc_conf_cor$value)
colnames(df.time) <- c("time", "subj", "conf", "confRT", "decRT", "acc", "acc_conf_cor")
head(df.time)
# add additional column variables
df.full <- merge(df, subset(dinfo, select=c("exp_idx", "Category", "Name_in_database", "Journal", "Year", "Stimuli", "Num_subjects", "Min_trials_per_subject")), by="exp_idx")
df.time <- cbind(data.frame(idx), df.time)
df.full$idx <- as.factor(df.full$idx)
df.catego <- df.full %>% 
  select(idx, Category, Name_in_database, Journal, Year, Stimuli, Num_subjects, Min_trials_per_subject) %>%
  unique()
dim(df.catego)

## finally I can have my temporal data with associated category of task, journal etc.
df.time <- cbind(df.time, df.catego)

# change time to percent
df.time$time <- df.time$time * 4
df.time <- df.time[,2:dim(df.time)[2]]


# save preprocessed temporal data
write.csv(df.time, paste(datdir_sav, "data_preproc_time"))




# ------ Plot as a function of time -------
#
# by CONFIDENCE RESPONSE TIME
#
y <- "confRT"   # confRT, conf
x <- "time"
# overall
g <- ggplot(df.time, aes_string(x=x, y=y)) +
  geom_smooth(se = TRUE, level = 0.67) +
  ylab("Confidence response speed (in SD)") +
  xlab("Time on task (in %)")
g
# Task category
g.cat <- ggplot(df.time, aes_string(x=x, y=y, color="Category")) +
  geom_smooth(se = TRUE, level = 0.67) +
  ylab("Confidence response speed (in SD)") +
  xlab("Time on task (in %)")
g.cat
table(df.time$Category)

# Experiment Idx
g.exp <- ggplot(df.time, aes_string(x=x, y=y, color="Name_in_database")) +
  geom_smooth(se = TRUE, level = 0.67) +
  ylab("Confidence response speed (in SD)") +
  xlab("Time on task (in %)")
g.exp
table(df.time$Name_in_database)

# Experiment Idx
g.year <- ggplot(df.time, aes_string(x=x, y=y, color="Year")) +
  geom_smooth(se = TRUE, level = 0.67) +
  ylab("Confidence response speed (in SD)") +
  xlab("Time on task (in %)")
g.year
table(df.time$Name_in_database)



## -------------------------------------------
## Plot RTdec and RTconf (when both made separately)
g1 <- ggplot(subset(df.time, decRT != confRT), aes_string(x=x, y="confRT")) +
  geom_smooth(se = TRUE, level = 0.67) +
  ylab("Confidence response speed (in SD)") +
  xlab("Time on task (in %)")
g2 <- ggplot(subset(df.time, decRT != confRT), aes_string(x=x, y="decRT")) +
  geom_smooth(se = TRUE, level = 0.67) +
  ylab("Confidence response speed (in SD)") +
  xlab("Time on task (in %)")
g1
g2

# ------ Plot as a function of time -------
#
# by CONFIDENCE RESPONSE TIME
#
y <- "decRT"   # confRT, conf
x <- "time"
# overall
g <- ggplot(df.time, aes_string(x=x, y=y)) +
  geom_smooth(se = TRUE, level = 0.67) +
  ylab("Confidence response speed (in SD)") +
  xlab("Time on task (in %)")
g
# Task category
g.cat <- ggplot(df.time, aes_string(x=x, y=y, color="Category")) +
  geom_smooth(se = TRUE, level = 0.67) +
  ylab("Confidence response speed (in SD)") +
  xlab("Time on task (in %)")
g.cat
table(df.time$Category)

# Experiment Idx
g.exp <- ggplot(df.time, aes_string(x=x, y=y, color="Name_in_database")) +
  geom_smooth(se = TRUE, level = 0.67) +
  ylab("Confidence response speed (in SD)") +
  xlab("Time on task (in %)")
g.exp
table(df.time$Name_in_database)

# Experiment Idx
g.year <- ggplot(df.time, aes_string(x=x, y=y, color="Year")) +
  geom_smooth(se = TRUE, level = 0.67) +
  ylab("Confidence response speed (in SD)") +
  xlab("Time on task (in %)")
g.year
table(df.time$Name_in_database)



#
# by CONFIDENCE RESPONSE
#
y <- "conf"   # confRT, conf
x <- "time"
# overall
g <- ggplot(df.time, aes_string(x=x, y=y)) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Confidence (in SD)") +
  xlab("Time on task (in %)")
g
# Task category
g.cat <- ggplot(df.time, aes_string(x=x, y=y, color="Category")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Confidence (in SD)") +
  xlab("Time on task (in %)")
g.cat
table(df.time$Category)

# Experiment Idx
g.exp <- ggplot(df.time, aes_string(x=x, y=y, color="Name_in_database")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Confidence (in SD)") +
  xlab("Time on task (in %)")
g.exp
table(df.time$Name_in_database)

# Journal
g.j <- ggplot(df.time, aes_string(x=x, y=y, color="Journal")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Confidence (in SD)") +
  xlab("Time on task (in %)")
g.j
table(df.time$Journal)


g.stim <- ggplot(df.time, aes_string(x=x, y=y, color="Stimuli")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Confidence (in SD)") +
  xlab("Time on task (in %)")
g.stim
table(df.time$Stimuli)


#
# by RESPONSE ACCURACY
#
y <- "acc"   # confRT, conf
x <- "time"
# overall
g <- ggplot(df.time, aes_string(x=x, y=y)) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Confidence (in SD)") +
  xlab("Time on task (in %)")
g
# Task category
g.cat <- ggplot(df.time, aes_string(x=x, y=y, color="Category")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Confidence (in SD)") +
  xlab("Time on task (in %)")
g.cat
table(df.time$Category)

# Experiment Idx
g.exp <- ggplot(df.time, aes_string(x=x, y=y, color="Name_in_database")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Confidence (in SD)") +
  xlab("Time on task (in %)")
g.exp
table(df.time$Name_in_database)

# Journal
g.j <- ggplot(df.time, aes_string(x=x, y=y, color="Journal")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Confidence (in SD)") +
  xlab("Time on task (in %)")
g.j
table(df.time$Journal)


g.stim <- ggplot(df.time, aes_string(x=x, y=y, color="Stimuli")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Confidence (in SD)") +
  xlab("Time on task (in %)")
g.stim
table(df.time$Stimuli)




#
# by metacognitive efficiency (acc corr with conf)
#
y <- "acc_conf_cor"   # confRT, conf
x <- "time"
# overall
g <- ggplot(df.time, aes_string(x=x, y=y)) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Pearson correlation accuracy, confidence") +
  xlab("Time on task (in %)")
g
# Task category
g.cat <- ggplot(df.time, aes_string(x=x, y=y, color="Category")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Pearson correlation accuracy, confidence") +
  xlab("Time on task (in %)")
g.cat
table(df.time$Category)

# Experiment Idx
g.exp <- ggplot(df.time, aes_string(x=x, y=y, color="Name_in_database")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Pearson correlation accuracy, confidence") +
  xlab("Time on task (in %)")
g.exp
table(df.time$Name_in_database)

# Journal
g.j <- ggplot(df.time, aes_string(x=x, y=y, color="Journal")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Pearson correlation accuracy, confidence") +
  xlab("Time on task (in %)")
g.j
table(df.time$Journal)


g.stim <- ggplot(df.time, aes_string(x=x, y=y, color="Stimuli")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Pearson correlation accuracy, confidence") +
  xlab("Time on task (in %)")
g.stim
table(df.time$Stimuli)




## Multiplot: RT conf / dec
# Task category
g.decRT <- ggplot(subset(df.time, decRT != confRT & 
  Category %in% c("Cognitive", "Memory", "Motor", "Perception")), 
  aes_string(x=x, y="decRT", color="Category")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Response speed (in SD)") +
  xlab("Time on task (in %)")

g.confRT <- ggplot(subset(df.time, decRT != confRT & 
  Category %in% c("Cognitive", "Memory", "Motor", "Perception")),
  aes_string(x=x, y="confRT", color="Category")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Response speed (in SD)") +
  xlab("Time on task (in %)")

ggarrange(g.decRT, g.confRT, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)



## plot: acc / conf
# Task category
g.accconf <- ggplot(subset(df.time, decRT != confRT & 
                             Category %in% c("Cognitive", "Memory", "Motor", "Perception")), 
                    aes_string(x="acc", y="conf", color="Category")) +
#  geom_point() +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Confidence") +
  xlab("Performance accuracy")
g.accconf




g.accconf <- ggplot(subset(df.time, decRT != confRT & 
                           Category %in% c("Cognitive", "Memory", "Motor", "Perception")), 
                  aes_string(x=x, y="acc_conf_cor", color="Category")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Correlation of accuracy and confidence") +
  xlab("Time on task (in %)")
g.accconf



g.confRT <- ggplot(subset(df.time, decRT != confRT & 
                            Category %in% c("Cognitive", "Memory", "Motor", "Perception")),
                   aes_string(x=x, y="acc_conf_cor", color="Category")) +
  geom_smooth(se = TRUE, level = 0.67)+
  ylab("Correlation of accuracy and confidence") +
  xlab("Time on task (in %)")

ggarrange(g.decRT, g.confRT, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)



df.table <- table(subset(df.time, Category %in% c("Cognitive", "Memory", "Motor", "Perception"))$Category)/25
df.freq <- data.frame(df.table)
colnames(df.freq) <- c("Category", "Count")
ggplot(df.freq, aes(x=Category, y=Count, na.rm = TRUE, color = Category, fill = Category)) +
  geom_bar(stat = "identity")




# eof
