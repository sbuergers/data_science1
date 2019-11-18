# Data incubator application challenge, task 1
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

options(digits=11)

# get arrest data from 
# https://data.lacity.org/A-Safe-City/Arrest-Data-from-2010-to-Present/yru6-6re4
d <- read.csv("~/admin/Applications/data_incubator/Arrest_Data_from_2010_to_Present.csv")

# What variables do we have?
head(d)

# # Sex.Code and other codes are factors
# d <- within(d, {
#   Sex.Code <- factor(Sex.Code)
#   Descent.Code <- factor(Descent.Code)
#   Area.ID <- factor(Area.ID)
#   Reporting.District <- factor(Reporting.District)
#   Charge.Group.Code <- factor(Charge.Group.Code)
#   Arrest.Type.Code <- factor(Arrest.Type.Code)
#   Charge <- factor(Charge)
# })




# Q1 --- How many bookings of arrestees were made in 2018?

# convert Date from character to class Date
d$Date <- as.Date(d$Arrest.Date, '%m/%d/%y')

# get year as a separate variable to group by
d$Year <- as.integer(substring(d$Arrest.Date,7,10))

# plot overview of yearly arrests of all data grouped by year
d %>%
  ggplot(aes(x = Date)) +
  geom_bar(color = "darkorchid4") +
  facet_wrap( ~ Year ) +
  labs(title = "Yearly arrests in LA, CA",
       y = "Arrests",
       x = "Date") + theme_bw(base_size = 15) +
  # adjust the x axis breaks
  scale_x_date(date_breaks = "5 years", date_labels = "%m-%Y")

# Now to answer the question, how many arrests were made in total in 2018?
sum(d$Year == 2018)





# Q2 --- How many bookings of arrestees were made in the area with the most arrests in 2018?

# Compute the frequency
df <- d %>%
  subset(Year == 2018) %>%
  group_by(Area.ID) %>%
  summarise(counts = n())
print(df)

# plot frequency of arrests by area ID
ggplot(df, aes(x = Area.ID, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3)





# Q3 --- What is the 95% quantile of the age of the arrestee in 2018? 
# Only consider the following charge groups for your analysis:
# Vehicle Theft
# Robbery
# Burglary
# Receive Stolen Property

# Check if the names are unique
d %>% subset(Year == 2018) %>% group_by(Charge.Description) %>% summarize(count=n())

# First, figure out which group codes belong to Vehicle theft, robbery, burglary and so on
# Find string values containing 'ab', return indices
d2018 <- d %>% subset(Year == 2018) 
  
vehicle.theft <- agrepl("vehicle theft", tolower(d2018$Charge.Group.Description))
robbery <- agrepl("robbery", tolower(d2018$Charge.Group.Description))
burglary <- agrepl("burglary", tolower(d2018$Charge.Group.Description))
receive.stolen.property <- agrepl("receive stolen property", tolower(d2018$Charge.Group.Description))

# check if the charge group codes are unique for the descriptions I found above
tabulate(d2018$Charge.Group.Code[vehicle.theft])
tabulate(d2018$Charge.Group.Code[robbery])
tabulate(d2018$Charge.Group.Code[burglary])
tabulate(d2018$Charge.Group.Code[receive.stolen.property])

# They are all unique, now check if I might have missed any by searching 
# for the charge codes instead and then comparing to what I got from searching
# the charge descriptions
code.vehicle.theft <- d2018$Charge.Group.Code == 7
sum(d2018$Charge.Group.Code == 7, na.rm = TRUE)
code.vehicle.theft <- d2018$Charge.Group.Code == 3
sum(d2018$Charge.Group.Code == 3, na.rm = TRUE)
code.vehicle.theft <- d2018$Charge.Group.Code == 5
sum(d2018$Charge.Group.Code == 5, na.rm = TRUE)
code.vehicle.theft <- d2018$Charge.Group.Code == 11
sum(d2018$Charge.Group.Code == 11, na.rm = TRUE)

# Combine indeces of the crimes we are interested in 
crimes.of.interest <- vehicle.theft | robbery | burglary | receive.stolen.property
sum(crimes.of.interest)

# At least in terms of number of observations they match, let's say that's good enough for now
quantile(d2018$Age[crimes.of.interest], probs = seq(0, 1, 0.05), na.rm = TRUE)
quantile(as.numeric(d2018$Age[crimes.of.interest]), probs = seq(0, 1, 0.05), na.rm = TRUE)

sum(d2018$Age[crimes.of.interest] < 52, na.rm = TRUE)
sum(d2018$Age[crimes.of.interest] >= 52, na.rm = TRUE)

# Compute the frequency of Age
df <- d[crimes.of.interest,] %>%
  subset(Year == 2018) %>%
  group_by(Age) %>%
  summarise(counts = n())
print(df)

# plot Age distribution in 2018
ggplot(df, aes(x = Age, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3)

# What if I don't consider ages below 12
quantile(d2018$Age[crimes.of.interest & d2018$Age >= 12], probs = seq(0, 1, 0.05), na.rm = TRUE)





#
# Q5 --- There are differences between the average age of an arrestee for the various 
# charge groups. Are these differences statistically significant? For this question, 
# calculate the Z-score of the average age for each charge group. Report the largest 
# absolute value among the calculated Z-scores.
#
# Only consider data for 2018
# Do not consider "Pre-Delinquency" and "Non-Criminal Detention" as these charge groups 
# are reserved for minors
# Exclude any arrests where the charge group description is not known
#


# Get all charge groups except pre delinquency and non criminal detention
charge.groups <- unique(d2018$Charge.Group.Description)
charge.groups <- droplevels(charge.groups[charge.groups != "Pre-Delinquency" & 
                                          charge.groups != "Non-Criminal Detention" &
                                          charge.groups != ""])
charge.grps <- as.character(charge.groups)

# Investigate Age for the different charge groups - get IDs for selection
# each list element contains the IDs for selecting one charge group
chgrp.ids <- list()
for (i in c(1:length(charge.groups))) {
  chgrp.ids[[i]] <- agrepl(charge.grps[i], tolower(d2018$Charge.Group.Description))
}

# calculate the Z-score of the average age for each charge group. Report the largest 
# absolute value among the calculated Z-scores.
age.avg <- c(1:length(charge.groups))
for (i in c(1:length(charge.groups))) {
  age.avg[i] <- mean(d2018$Age[chgrp.ids[[i]]], na.rm = TRUE)
}
age.zscore <- scale(age.avg[is.finite(age.avg)])
max(abs(age.zscore))




#
# Q6 --- Felony arrest incidents have been dropping over the years. Using a trend line 
# (linear estimation) for the data from 2010 and 2018 (inclusive), what is the projected 
# number of felony arrests in 2019? Round to the nearest integer. 
#


# Select fellony data from 2010 and 2018
dfell <- d %>% subset(Year %in% c(2010,2018) & Arrest.Type.Code == "F")

# Predict fellony rate for 2019
# Compute the frequency by month
dfell$Month <- as.integer(substring(dfell$Arrest.Date,1,2))
dfell$time <- dfell$Year + dfell$Month / 13
df <- dfell %>%
  group_by(time) %>%
  summarise(counts = n())
print(df)

# plot fellonies over the months and years
ggplot(df, aes(x = time, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3)

# fit simple linear model to fellonies
lmfell <- lm(counts ~ 1 + time, data = df)
summary(lmfell)

# predict fellonies for 2019
time <- 2019 + c(1:12)/13
df_new <- data.frame(time)
forecast_fellonies <- forecast(lmfell, newdata = df_new)
temp <- summary(forecast_fellonies)

# plot fellonies over the months and years + forecasted fellonies for 2019
counts <- temp[,1]
df_pred <- data.frame(time, counts)
df_all <- rbind(df, df_pred)
ggplot(df_all, aes(x = time, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3)

predicted.fellonies <- round(sum(counts))
print(predicted.fellonies)





#
# Q7 --- How many arrest incidents were made per kilometer on Pico Boulevard during 2018? 
#

pico.id <- grepl("pico", tolower(d2018$Address))
gsub("(,)", "", as.character(d2018$Location), fixed = TRUE)
X <- as.numeric(gsub("[^-[:digit:].]", "",  gsub(" .*$", "", as.character(d2018$Location))))
Y <- as.numeric(gsub("[^-[:digit:].]", "",  gsub(".* ", "", as.character(d2018$Location))))
d2018$X <- X
d2018$Y <- Y

# Scatter plot of coordinates
d2018 %>%
  subset(pico.id & !(X == 0 & Y == 0)) %>%
  ggplot(aes(x=X, y=Y)) +
  geom_point()

# Remove outliers by filtering out locations where either the latitude or 
# longitude is 2 standard deviations beyond the mean of the subset of identified points.
Xoi <- d2018$X[pico.id & !(X == 0 & Y == 0)]
Yoi <- d2018$Y[pico.id & !(X == 0 & Y == 0)]
within2std <- (Xoi < (mean(Xoi) + 2*sd(Xoi)) & Xoi > (mean(Xoi) - 2*sd(Xoi))) &
              (Yoi < (mean(Yoi) + 2*sd(Yoi)) & Yoi > (mean(Yoi) - 2*sd(Yoi)))
plot(Xoi[within2std], Yoi[within2std])

# Oh, actually the first number is north-south, the second number is east-west...
westY <- min(Yoi[within2std])
eastY <- max(Yoi[within2std])

# get west-most and east-most coordinates
westcoord <- c(unique(Xoi[Yoi == westY]), westY)
eastcoord <- c(unique(Xoi[Yoi == eastY]), eastY)

# Use the spherical Earth projected to a plane equation for calculating distances.
R = 6371.009 # radius of earth in km
# convert latitude and logitude to radians
rad.west <- pi/180 * westcoord
rad.east <- pi/180 * eastcoord
delta.sig <- rad.west[1] - rad.east[1]
mean.sig <- (rad.west[1] + rad.east[1]) / 2
delta.lam <- rad.west[2] - rad.east[2]
D = R * sqrt( (delta.sig^2) + ( cos(mean.sig)*delta.lam^2 ) )

# length of Pico boulevard:
print(D)

# Once you have estimated the length of Pico Boulevard, you can proceed to report the number 
# of arrest incidents per kilometer on Pico Boulevard in 2018.
df <- d2018 %>%
  subset(pico.id) %>%
  summarise(counts = n())
print(df)

# Arrests on Pico boulevard per km:
df$counts / D


#
# Q8 --- How many arrest incidents occurred within 2 km from the Bradbury Building in 2018? 
#

R = 6371 # radius of earth in km (according to the question)

# function to check for a given point if it is close to bradbury
close_to_bradbury <- function(from, maxdist=2, to=c(34.050536, -118.247861)) {
  # returns TRUE if the latitude and longitude in c(lat, lon) of "from"
  # are within maxdist km of c(lat, lon) of "to"
  # the distance is computed according to the spherical earth projected to a plane
  # equation (see wikipedia for details)
  B <- c(pi/180 * to[1], pi/180 * to[2])
  A <- c(pi/180 * from[1], pi/180 * from[2])
  delta.sig <- B[1] - A[1]
  mean.sig <- (B[1] + A[1]) / 2
  delta.lam <- B[2] - A[2]
  D = R * sqrt( (delta.sig^2) + ( cos(mean.sig)*delta.lam^2 ) )
  return(maxdist > D)
}

# test all pairwise distances with the bradbury
coords <- cbind(d2018$X, d2018$Y)
d2018$close.to.Bradbury <- apply(coords, 1, close_to_bradbury)

# How many arrests were made close to the bradbury in 2018?
sum(d2018$close.to.Bradbury)





#
# Q9 --- Some types of arrest incidents in certain areas occur at a highly disproportionate 
# rate compared to their frequency city-wide. For example, let's say that the rate of larceny 
# arrests (charge group code 6) is 1% in Devonshire (area ID 17). This rate may appear low but 
# what if larceny arrests constitute 0.1 % city-wide? The ratio between these two probabilities 
# is 10 and we can say that larceny occurs unusually often in Devonshire (Note, these numbers 
# were made up for illustration purposes). Calculate this ratio for all charge group code and 
# area ID pairs. You can view this ratio as the ratio of the conditional probability of an arrest 
# incident of a charge group code given that it occurred in an area ID to the unconditional probability 
# of the arrest incident of a charge group. Report the average of the top 5 of the calculated ratio.
#
# Consider all records prior to January 1, 2019.
# Some arrest incidents don't have a charge group code. 
# These records should not be considered in your analysis.
# Arrest incidents for charge group code 99 should not be considered in your analysis.
# 

dpre2019 <- d %>%
            subset(Year < 2019 & Charge.Group.Code != 99 & !is.na(Charge.Group.Code)) %>%
            droplevels()
          
freq.table <- table(dpre2019$Area.ID,dpre2019$Charge.Group.Code) # A will be rows, B will be columns

# visualize counts
pheatmap(freq.table, display_numbers = T, cluster_rows = F, cluster_cols = F)

# compute counts of arrest codes over the whole city
sz.table <- dim(freq.table)
arrests.by.area <- t(repmat(rowSums(freq.table), sz.table[2],1))
pheatmap(arrests.by.area, display_numbers = T, cluster_rows = F, cluster_cols = F)

# conditional probability of an arrest 
# incident of a charge group code given that it occurred in an area ID
cond.prob.table <- freq.table / arrests.by.area
pheatmap(cond.prob.table, display_numbers = T, cluster_rows = F, cluster_cols = F)

# unconditional probability of the arrest incident of a charge group 
prob.citywide <- repmat(colSums(freq.table / sum(freq.table)),sz.table[1],1)
pheatmap(prob.citywide, display_numbers = T, cluster_rows = F, cluster_cols = F)

# Calculate the ratio of the conditional probability of an arrest 
# incident of a charge group code given that it occurred in an area ID to the 
# unconditional probability of the arrest incident of a charge group.
ratio <- cond.prob.table / prob.citywide
pheatmap(ratio, display_numbers = T, cluster_rows = F, cluster_cols = F)

# Report the average of the top 5 of the calculated ratio.
sorted.ratio <- sort(ratio)
mean(tail(sorted.ratio, n=5))



