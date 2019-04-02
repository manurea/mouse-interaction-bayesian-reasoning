set.seed(171219)
#-----------------------------------------------------------------------------------
# Author:       Manuele Reani
# Date:         03/01/2018
# Institution:  HKUST
# Object:       Analysis of study 4 data (fire website) Cleaning raw data (1)
#-----------------------------------------------------------------------------------

#---------------------------------------------------------------------------------
# FUNCTION:     loadPackages(package.args)
# INPUT:        vector
# OUTPUT:       void
# DESCRIPTION:  Loads required packages.
#                
#---------------------------------------------------------------------------------
loadPackages <- function(package.args)
{ 
  for(i in package.args)
  {
    if(!is.element(i, .packages(all.available = TRUE)))
    {
      cat("\nPackage <", i, "> not found, attempting to add it...")
      install.packages(i)
    }
    library(i, character.only = TRUE)
  }
}
#---------------------------------------------------------------------------------
# FUNCTION:     initialize()
# INPUT:        void
# OUTPUT:       void
# DESCRIPTION:  Set up function for adding packages and other source data
#               
#---------------------------------------------------------------------------------
initialize <- function()
{
  # load packages
  package.args <- c("car","msm", "png","jpeg", "gplots", "dplyr", "plyr","tidyr", 
                    "matrixStats",  "lattice","ggplot2", "gtools", 
                    "dbscan", "stringdist", "utils", "qualV", "stringi", "dplyr", 
                    "stringr", "rjson","lsmeans","multcomp","lme4","nlme","MuMIn",
                    "effsize","heplots","DescTools","irr","reshape","psych","effsize")
  loadPackages(package.args)
}

initialize()
setwd("C:/Users/manuele/Desktop/PhDremote/paper4/FIRE/data&code")

####################### WAVE1: FREQUENCY & PROBABILITY MIXED ###########################################################

# Cleaning Alan/Aitor DB data 
DF_1 <-read.csv(file="Alan_data_W1.csv",header=TRUE,sep=",")
DF_2 <-read.csv(file="Aitor_data.csv",header=TRUE,sep=",")
# keep only the column of interest
DF_2<- DF_2[, c("event","timestampms","sid", "node.id")]

# convert the timestamp in Alan
# convert the class to data and time and conver to epoch 
options(digits.secs=6)
DF_1$date.time <- as.POSIXct(DF_1$date.time, format = "%Y-%m-%d %H:%M:%OS")
DF_1$date.time <- as.numeric(DF_1$date.time)*1000

# subset by event and button
DF_2_1 <- DF_2[DF_2$node.id == 'tree-submit-button' &
                 DF_2$event == 'mousedown',]

# Check for duplicate in Aitor DF (Ps who pressed submit twice) ed eliminate them by 
# keeping the latest event
DF_2_1$error<- rep(0, nrow(DF_2_1))
DF_2_1[duplicated(DF_2_1[3]), 5] <- 1
DF_2_1_temp <- subset(DF_2_1, !(DF_2_1$sid %in% unique(DF_2_1[DF_2_1$error==1,3])))

df_build <- DF_2_1[DF_2_1$sid %in%  unique(DF_2_1[DF_2_1$error==1,3]),]
for (p in unique(df_build$sid)){
  df_build2 <- df_build[df_build$sid==p,]
  DF_2_1_temp <- rbind(DF_2_1_temp,
    (df_build2[df_build2$timestampms == max(df_build2$timestampms),]))
}
# eliminate the error column
DF_2_1_temp <- DF_2_1_temp[,-5]
# check for duplicates in Aitor DF
DF_2_1_temp[duplicated(DF_2_1_temp$sid), ]

# Create a dummy df and get the id in Alan DF
DF_1_temp <- DF_1
DF_1_temp$ucivit_id <- as.character(DF_1_temp$ucivit_id)
for (i in DF_2_1_temp$timestampms){
  for (c in DF_1$date.time){
    if(c > (i-1500)){ # this can be incrased or decrease, but if too large may include duplicate if too narrow may not get all the IDs
      if(c < (i+1500)){ 
        DF_1_temp[DF_1_temp$date.time == c,3] <- 
          as.character(DF_2_1_temp[DF_2_1_temp$timestampms == i,3]) 
      }
    }
  }
}
# check for duplicates (precision of the milliseconds) in Alan DF
DF_1_temp[duplicated(DF_1_temp[3]), 3]

# Create the DFs
Alan_DF_freq_1 <- DF_1_temp[DF_1_temp$numerical_format=='frequency',]

Alan_DF_prob_1 <- DF_1_temp[DF_1_temp$numerical_format=='probability',]

Aitor_DF_freq_1 <- DF_2[DF_2$sid %in% Alan_DF_freq_1$ucivit_id, ]
Aitor_DF_freq_1$n_format <- rep('freq', nrow(Aitor_DF_freq_1))
Aitor_DF_freq_1$wave <- rep(1, nrow(Aitor_DF_freq_1))

Aitor_DF_prob_1 <- DF_2[DF_2$sid %in% Alan_DF_prob_1$ucivit_id, ]
Aitor_DF_prob_1$n_format <- rep('prob', nrow(Aitor_DF_prob_1))
Aitor_DF_prob_1$wave <- rep(1, nrow(Aitor_DF_prob_1))

####################### FREQUENCY 2 ###########################################################

# Cleaning Alan/Aitor DB data 
DF_1 <-read.csv(file="freq_wave1_alan.csv",header=TRUE,sep=",")
DF_2 <-read.csv(file="freq_wave1_aitor.csv",header=TRUE,sep=",")
# keep only the column of interest
DF_2<- DF_2[, c("event","timestampms","sid", "node.id")]
DF_1a <-read.csv(file="freq_wave1_alan2.csv",header=TRUE,sep=",")
DF_2a <-read.csv(file="freq_wave1_aitor2.csv",header=TRUE,sep=",")
# keep only the column of interest
DF_2a<- DF_2a[, c("event","timestampms","sid", "node.id")]
# bind the DFs
DF_1 <- rbind(DF_1, DF_1a)
DF_2 <- rbind(DF_2,DF_2a)


# convert the timestamp in Alan
# convert the class to data and time and conver to epoch 
options(digits.secs=6)
DF_1$date.time <- as.POSIXct(DF_1$date.time, format = "%Y-%m-%d %H:%M:%OS")
DF_1$date.time <- as.numeric(DF_1$date.time)*1000

# subset by event and button
DF_2_1 <- DF_2[DF_2$node.id == 'tree-submit-button' &
                 DF_2$event == 'mousedown',]

# Check for duplicate in Aitor DF (Ps who pressed submit twice) ed eliminate them by 
# keeping the latest event
DF_2_1$error<- rep(0, nrow(DF_2_1))
DF_2_1[duplicated(DF_2_1[3]), 5] <- 1
DF_2_1_temp <- subset(DF_2_1, !(DF_2_1$sid %in% unique(DF_2_1[DF_2_1$error==1,3])))

df_build <- DF_2_1[DF_2_1$sid %in%  unique(DF_2_1[DF_2_1$error==1,3]),]
for (p in unique(df_build$sid)){
  df_build2 <- df_build[df_build$sid==p,]
  DF_2_1_temp <- rbind(DF_2_1_temp,
                       (df_build2[df_build2$timestampms == max(df_build2$timestampms),]))
}
# eliminate the error column
DF_2_1_temp <- DF_2_1_temp[,-5]
# check for duplicates in Aitor DF
DF_2_1_temp[duplicated(DF_2_1_temp$sid), ]

# Create a dummy df and get the id in Alan DF
DF_1_temp <- DF_1
DF_1_temp$ucivit_id <- as.character(DF_1_temp$ucivit_id)
for (i in DF_2_1_temp$timestampms){
  for (c in DF_1$date.time){
    if(c > (i-1500)){ # this can be incrased or decrease, but if too large may include duplicate if too narrow may not get all the IDs
      if(c < (i+1500)){ 
        DF_1_temp[DF_1_temp$date.time == c,3] <- 
          as.character(DF_2_1_temp[DF_2_1_temp$timestampms == i,3]) 
      }
    }
  }
}
# check for duplicates (precision of the milliseconds) in Alan DF
DF_1_temp[duplicated(DF_1_temp[3]), 3]

# Create the DFs
Alan_DF_freq_2 <- DF_1_temp
Aitor_DF_freq_2 <- DF_2 
Aitor_DF_freq_2$n_format <- rep('freq', nrow(Aitor_DF_freq_2))
Aitor_DF_freq_2$wave <- rep(1, nrow(Aitor_DF_freq_2))

############################ PROBABILITY2 #####################################################

# Cleaning Alan/Aitor DB data 
DF_1 <-read.csv(file="alan_probability_wave1.csv",header=TRUE,sep=",")
DF_2 <-read.csv(file="aitor_probability_wave1.csv",header=TRUE,sep=",")
# keep only the column of interest
DF_2<- DF_2[, c("event","timestampms","sid", "node.id")]

# convert the timestamp in Alan
# convert the class to data and time and conver to epoch 
options(digits.secs=6)
DF_1$date.time <- as.POSIXct(DF_1$date.time, format = "%Y-%m-%d %H:%M:%OS")
DF_1$date.time <- as.numeric(DF_1$date.time)*1000

# subset by event and button
DF_2_1 <- DF_2[DF_2$node.id == 'tree-submit-button' &
                 DF_2$event == 'mousedown',]

# Check for duplicate in Aitor DF (Ps who pressed submit twice) ed eliminate them by 
# keeping the latest event
DF_2_1$error<- rep(0, nrow(DF_2_1))
DF_2_1[duplicated(DF_2_1[3]), 5] <- 1
DF_2_1_temp <- subset(DF_2_1, !(DF_2_1$sid %in% unique(DF_2_1[DF_2_1$error==1,3])))

df_build <- DF_2_1[DF_2_1$sid %in%  unique(DF_2_1[DF_2_1$error==1,3]),]
for (p in unique(df_build$sid)){
  df_build2 <- df_build[df_build$sid==p,]
  DF_2_1_temp <- rbind(DF_2_1_temp,
                       (df_build2[df_build2$timestampms == max(df_build2$timestampms),]))
}
# eliminate the error column
DF_2_1_temp <- DF_2_1_temp[,-5]
# check for duplicates in Aitor DF
DF_2_1_temp[duplicated(DF_2_1_temp$sid), ]

# Create a dummy df and get the id in Alan DF
DF_1_temp <- DF_1
DF_1_temp$ucivit_id <- as.character(DF_1_temp$ucivit_id)
for (i in DF_2_1_temp$timestampms){
  for (c in DF_1$date.time){
    if(c > (i-500)){ # this can be incrased or decrease, but if too large may include duplicate if too narrow may not get all the IDs
      if(c < (i+500)){ 
        DF_1_temp[DF_1_temp$date.time == c,3] <- 
          as.character(DF_2_1_temp[DF_2_1_temp$timestampms == i,3]) 
      }
    }
  }
}
# check for duplicates (precision of the milliseconds) in Alan DF
DF_1_temp[duplicated(DF_1_temp[3]), 3] #???????????????????????????????????????????????????

# eliminate those duplicates
DF_1_temp <- DF_1_temp[DF_1_temp$ucivit_id!='387a379abbdaa391',]

# Create the DFs
Alan_DF_prob_2 <- DF_1_temp
Aitor_DF_prob_2 <- DF_2
Aitor_DF_prob_2$n_format <- rep('prob', nrow(Aitor_DF_prob_2))
Aitor_DF_prob_2$wave <- rep(1, nrow(Aitor_DF_prob_2))

#-----------------------------------------------------------------
#      MERGING THE DATAFRAME 
# ---------------------------------------------------------------

# merge the DFs
# DFs: Alan_DF_freq_1, Alan_DF_prob_1, Alan_DF_freq_2, Alan_DF_prob_2

Alan_wave1 <- do.call("rbind", 
                      list(Alan_DF_freq_1, Alan_DF_prob_1, Alan_DF_freq_2,Alan_DF_prob_2))
  
# DFs: Aitor_DF_freq_1, Aitor_DF_prob_1, Aitor_DF_freq_2, Aitor_DF_prob_2

Aitor_wave1 <- do.call("rbind", 
                      list(Aitor_DF_freq_1, Aitor_DF_prob_1, Aitor_DF_freq_2,Aitor_DF_prob_2))


# Add some lables  
Alan_wave1[Alan_wave1$ucivit_id == '',]$ucivit_id <- '0'
Alan_wave1[Alan_wave1$ucivit_id == 'null',]$ucivit_id <- '0'
Alan_wave1$Wave <- rep(1, nrow(Alan_wave1))

# final input 
#View(Alan_wave1)
#View(Aitor_wave1)

#-------------------------------------------------------------------------------
#              SECOND WAVE PREPARATION 
# -----------------------------------------------------------------------------

####################### FREQUENCY ###########################################################

# Cleaning Alan/Aitor DB data 
DF_1 <-read.csv(file="w2_freq_alan.csv",header=TRUE,sep=",")
DF_2 <-read.csv(file="w2_freq_aitor.csv",header=TRUE,sep=",")
# keep only the column of interest
DF_2<- DF_2[, c("event","timestampms","sid", "node.id")]

# convert the timestamp in Alan
# convert the class to data and time and conver to epoch 
options(digits.secs=6)
DF_1$date.time <- as.POSIXct(DF_1$date.time, format = "%Y-%m-%d %H:%M:%OS")
DF_1$date.time <- as.numeric(DF_1$date.time)*1000

# subset by event and button
DF_2_1 <- DF_2[DF_2$node.id == 'tree-submit-button' &
                 DF_2$event == 'mousedown',]

# Check for duplicate in Aitor DF (Ps who pressed submit twice) ed eliminate them by 
# keeping the latest event
DF_2_1$error<- rep(0, nrow(DF_2_1))
DF_2_1[duplicated(DF_2_1[3]), 5] <- 1
DF_2_1_temp <- subset(DF_2_1, !(DF_2_1$sid %in% unique(DF_2_1[DF_2_1$error==1,3])))

df_build <- DF_2_1[DF_2_1$sid %in%  unique(DF_2_1[DF_2_1$error==1,3]),]
for (p in unique(df_build$sid)){
  df_build2 <- df_build[df_build$sid==p,]
  DF_2_1_temp <- rbind(DF_2_1_temp,
                       (df_build2[df_build2$timestampms == max(df_build2$timestampms),]))
}
# eliminate the error column
DF_2_1_temp <- DF_2_1_temp[,-5]
# check for duplicates in Aitor DF
DF_2_1_temp[duplicated(DF_2_1_temp$sid), ]

# Create a dummy df and get the id in Alan DF
DF_1_temp <- DF_1
DF_1_temp$ucivit_id <- as.character(DF_1_temp$ucivit_id)
for (i in DF_2_1_temp$timestampms){
  for (c in DF_1$date.time){
    if(c > (i-1500)){ # this can be incrased or decrease, but if too large may include duplicate if too narrow may not get all the IDs
      if(c < (i+1500)){ 
        DF_1_temp[DF_1_temp$date.time == c,3] <- 
          as.character(DF_2_1_temp[DF_2_1_temp$timestampms == i,3]) 
      }
    }
  }
}
# check for duplicates (precision of the milliseconds) in Alan DF
DF_1_temp[duplicated(DF_1_temp[3]), 3]

# eliminate those duplicates
DF_1_temp <- DF_1_temp[DF_1_temp$ucivit_id!='becc0b26ccad33a7',]
DF_1_temp <- DF_1_temp[DF_1_temp$ucivit_id!='36548f09378bbfe9',]

# Create the DFs
Alan_DF_freq_w2 <- DF_1_temp
Aitor_DF_freq_w2 <- DF_2 
Aitor_DF_freq_w2$n_format <- rep('freq', nrow(Aitor_DF_freq_w2))
Aitor_DF_freq_w2$wave <- rep(2, nrow(Aitor_DF_freq_w2))

# NB: there are more than 150 Ps
# eliminate unreliable Ps who answer clearly with a thoughtless pattern
tempDFbad <- Alan_DF_freq_w2
tempDFbad$badPs <- rep(0,nrow(tempDFbad))
tempDFbad[tempDFbad$id %in% c(71,78,15,18,25,30,42,48,46,118,119),42] <- 1
Alan_DF_freq_w2 <- tempDFbad[tempDFbad$badPs != 1,- 42]

#marci <- tempDFbad[tempDFbad$badPs == 1,]
#print(as.vector(unlist(marci$amazon_codex)))

####################### PROBABILITY ###########################################################

# Cleaning Alan/Aitor DB data 
DF_1 <-read.csv(file="w2_prob_alan.csv",header=TRUE,sep=",")
DF_2 <-read.csv(file="w2_prob_aitor.csv",header=TRUE,sep=",")
# keep only the column of interest
DF_2<- DF_2[, c("event","timestampms","sid", "node.id")]

# convert the timestamp in Alan
# convert the class to data and time and conver to epoch 
options(digits.secs=6)
DF_1$date.time <- as.POSIXct(DF_1$date.time, format = "%Y-%m-%d %H:%M:%OS")
DF_1$date.time <- as.numeric(DF_1$date.time)*1000

# subset by event and button
DF_2_1 <- DF_2[DF_2$node.id == 'tree-submit-button' &
                 DF_2$event == 'mousedown',]

# Check for duplicate in Aitor DF (Ps who pressed submit twice) ed eliminate them by 
# keeping the latest event
DF_2_1$error<- rep(0, nrow(DF_2_1))
DF_2_1[duplicated(DF_2_1[3]), 5] <- 1
DF_2_1_temp <- subset(DF_2_1, !(DF_2_1$sid %in% unique(DF_2_1[DF_2_1$error==1,3])))

df_build <- DF_2_1[DF_2_1$sid %in%  unique(DF_2_1[DF_2_1$error==1,3]),]
for (p in unique(df_build$sid)){
  df_build2 <- df_build[df_build$sid==p,]
  DF_2_1_temp <- rbind(DF_2_1_temp,
                       (df_build2[df_build2$timestampms == max(df_build2$timestampms),]))
}
# eliminate the error column
DF_2_1_temp <- DF_2_1_temp[,-5]
# check for duplicates in Aitor DF
DF_2_1_temp[duplicated(DF_2_1_temp$sid), ]

# Create a dummy df and get the id in Alan DF
DF_1_temp <- DF_1
DF_1_temp$ucivit_id <- as.character(DF_1_temp$ucivit_id)
for (i in DF_2_1_temp$timestampms){
  for (c in DF_1$date.time){
    if(c > (i-1500)){ # this can be incrased or decrease, but if too large may include duplicate if too narrow may not get all the IDs
      if(c < (i+1500)){ 
        DF_1_temp[DF_1_temp$date.time == c,3] <- 
          as.character(DF_2_1_temp[DF_2_1_temp$timestampms == i,3]) 
      }
    }
  }
}
# check for duplicates (precision of the milliseconds) in Alan DF
DF_1_temp[duplicated(DF_1_temp[3]), 3]

# eliminate those duplicates
DF_1_temp <- DF_1_temp[DF_1_temp$ucivit_id!='2698df7c3b11fdf1',]

# Create the DFs
Alan_DF_prob_w2 <- DF_1_temp
Aitor_DF_prob_w2 <- DF_2 
Aitor_DF_prob_w2$n_format <- rep('prob', nrow(Aitor_DF_prob_w2))
Aitor_DF_prob_w2$wave <- rep(2, nrow(Aitor_DF_prob_w2))

# NB: there are more than 150 Ps
# eliminate unreliable Ps who answer clearly with a thoughtless pattern
tempDFbad <- Alan_DF_prob_w2
tempDFbad$badPs <- rep(0,nrow(tempDFbad))
tempDFbad[tempDFbad$id %in% 
            c(93,11,13,45,
              48,148,76,81,
              83,91,101,128,
              141,114,124,105,
              79,2,156,12,
              37,98,172), 42] <- 1
Alan_DF_prob_w2 <- tempDFbad[tempDFbad$badPs != 1,- 42]
#marci <- tempDFbad[tempDFbad$badPs == 1,]
#print(as.vector(unlist(marci$amazon_codex)))




#-----------------------------------------------------------------
#      MERGING THE DATAFRAME 
# ---------------------------------------------------------------
# merge the DFs
Alan_wave2 <- do.call("rbind", list(Alan_DF_freq_w2, Alan_DF_prob_w2))
Aitor_wave2 <- do.call("rbind", list(Aitor_DF_freq_w2, Aitor_DF_prob_w2))

# Add some lables  
Alan_wave2[Alan_wave2$ucivit_id == '',]$ucivit_id <- '0'
Alan_wave2[Alan_wave2$ucivit_id == 'null',]$ucivit_id <- '0'
Alan_wave2$Wave <- rep(2, nrow(Alan_wave2))

# final input 
#View(Alan_wave2)
#View(Aitor_wave2)


#----------------------------------------------------------------
# DESCRIPTIVE STATISTICS W1 and W2
#-------------------------------------------------------------
# do some counting Wave 1
nrow(Alan_wave1[Alan_wave1$numerical_format=='frequency',])
nrow(Alan_wave1[Alan_wave1$numerical_format=='frequency' &
                  Alan_wave1$ucivit_id=='0' ,])
nrow(Alan_wave1[Alan_wave1$numerical_format=='frequency' &
                  Alan_wave1$ucivit_id!='0' ,])

nrow(Alan_wave1[Alan_wave1$numerical_format=='probability',])
nrow(Alan_wave1[Alan_wave1$numerical_format=='probability' &
                  Alan_wave1$ucivit_id =='0' ,])

nrow(Alan_wave1[Alan_wave1$numerical_format=='probability' &
                  Alan_wave1$ucivit_id!='0' ,])

nrow(Aitor_wave1[Aitor_wave1$n_format=='freq',])
nrow(Aitor_wave1[Aitor_wave1$n_format=='prob',])

# do some counting Wave 2
nrow(Alan_wave2[Alan_wave2$numerical_format=='frequency',])
nrow(Alan_wave2[Alan_wave2$numerical_format=='frequency' &
                  Alan_wave2$ucivit_id=='0' ,])
nrow(Alan_wave2[Alan_wave2$numerical_format=='frequency' &
                  Alan_wave2$ucivit_id!='0' ,])

nrow(Alan_wave2[Alan_wave2$numerical_format=='probability',])
nrow(Alan_wave2[Alan_wave2$numerical_format=='probability' &
                  Alan_wave2$ucivit_id =='0' ,])

nrow(Alan_wave2[Alan_wave2$numerical_format=='probability' &
                  Alan_wave2$ucivit_id!='0' ,])

nrow(Aitor_wave2[Aitor_wave2$n_format=='freq',])
nrow(Aitor_wave2[Aitor_wave2$n_format=='prob',])

# check correctness (percentages of correct answers) Wave 1
nrow(Alan_wave1[Alan_wave1$correctness ==1 & Alan_wave1$numerical_format=='frequency',])/
  nrow(Alan_wave1[Alan_wave1$numerical_format=='frequency',])

nrow(Alan_wave1[Alan_wave1$correctness ==1 & Alan_wave1$numerical_format=='probability',])/
  nrow(Alan_wave1[Alan_wave1$numerical_format=='probability',])

# check correctness (percentages of correct answers) Wave 2
nrow(Alan_wave2[Alan_wave2$correctness ==1 & Alan_wave2$numerical_format=='frequency',])/
  nrow(Alan_wave2[Alan_wave2$numerical_format=='frequency',])

nrow(Alan_wave2[Alan_wave2$correctness ==1 & Alan_wave2$numerical_format=='probability',])/
  nrow(Alan_wave2[Alan_wave2$numerical_format=='probability',])

# Descriptive base rate wave 1
median(Alan_wave1$baserate)
IQR(Alan_wave1$baserate) 
median(Alan_wave1[Alan_wave1$numerical_format=='frequency',]$baserate)
IQR(Alan_wave1[Alan_wave1$numerical_format=='frequency',]$baserate)
median(Alan_wave1[Alan_wave1$numerical_format=='probability',]$baserate)
IQR(Alan_wave1[Alan_wave1$numerical_format=='probability',]$baserate)
# Descriptive true_positive wave 1
median(Alan_wave1$true_positive)
IQR(Alan_wave1$true_positive)
median(Alan_wave1[Alan_wave1$numerical_format=='frequency',]$true_positive)
IQR(Alan_wave1[Alan_wave1$numerical_format=='frequency',]$true_positive)
median(Alan_wave1[Alan_wave1$numerical_format=='probability',]$true_positive)
IQR(Alan_wave1[Alan_wave1$numerical_format=='probability',]$true_positive)
# Descriptive false_alarm wave 1
median(Alan_wave1$false_alarm)
IQR(Alan_wave1$false_alarm)
median(Alan_wave1[Alan_wave1$numerical_format=='frequency',]$false_alarm)
IQR(Alan_wave1[Alan_wave1$numerical_format=='frequency',]$false_alarm)
median(Alan_wave1[Alan_wave1$numerical_format=='probability',]$false_alarm)
IQR(Alan_wave1[Alan_wave1$numerical_format=='probability',]$false_alarm)

# Descriptive base rate wave 2
median(Alan_wave2$baserate)
IQR(Alan_wave2$baserate)
median(Alan_wave2[Alan_wave2$numerical_format=='frequency',]$baserate)
IQR(Alan_wave2[Alan_wave2$numerical_format=='frequency',]$baserate)
median(Alan_wave2[Alan_wave2$numerical_format=='probability',]$baserate)
IQR(Alan_wave2[Alan_wave2$numerical_format=='probability',]$baserate)
# Descriptive true_positive wave 2
median(Alan_wave2$true_positive)
IQR(Alan_wave2$true_positive)
median(Alan_wave2[Alan_wave2$numerical_format=='frequency',]$true_positive)
IQR(Alan_wave2[Alan_wave2$numerical_format=='frequency',]$true_positive)
median(Alan_wave2[Alan_wave2$numerical_format=='probability',]$true_positive)
IQR((Alan_wave2[Alan_wave2$numerical_format=='probability',]$true_positive))
# Descriptive false_alarm wave 2
median(Alan_wave2$false_alarm)
IQR(Alan_wave2$false_alarm)
median(Alan_wave2[Alan_wave2$numerical_format=='frequency',]$false_alarm)
IQR(Alan_wave2[Alan_wave2$numerical_format=='frequency',]$false_alarm)
median(Alan_wave2[Alan_wave2$numerical_format=='probability',]$false_alarm)
IQR(Alan_wave2[Alan_wave2$numerical_format=='probability',]$false_alarm)


#----------------------------------------------------------------
#   PLOTTING WAVE 1
#-------------------------------------------------------------

# Plot the data TOTAL
# Baserate P(F)
ggplot(Alan_wave1, aes(x=baserate)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(xintercept=mean(Alan_wave1$baserate), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(Alan_wave1$baserate), 
             color="blue", linetype="solid", size=1)

# true positive P(A|F)
ggplot(Alan_wave1, aes(x=true_positive)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(Alan_wave1$true_positive), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(Alan_wave1$true_positive), 
             color="blue", linetype="solid", size=1)

# false alarm P(A|NF)
ggplot(Alan_wave1, aes(x=false_alarm)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(Alan_wave1$false_alarm), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(Alan_wave1$false_alarm), 
             color="blue", linetype="solid", size=1) 




#----------------------------------------------------------------
#   PLOTTING WAVE 2
#-------------------------------------------------------------

# Plot the data TOTAL
# Baserate P(F)
ggplot(Alan_wave2, aes(x=baserate)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(xintercept=mean(Alan_wave2$baserate), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(Alan_wave2$baserate), 
             color="blue", linetype="solid", size=1)

# true positive P(A|F)
ggplot(Alan_wave2, aes(x=true_positive)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(Alan_wave2$true_positive), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(Alan_wave2$true_positive), 
             color="blue", linetype="solid", size=1)

# false alarm P(A|NF)
ggplot(Alan_wave2, aes(x=false_alarm)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(Alan_wave2$false_alarm), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(Alan_wave2$false_alarm), 
             color="blue", linetype="solid", size=1) 






# ????????????????????????????????????????????????????????????????????????????????????????
# ------------------------------------------------------------------------
# Testing PLOTTING (Wave 1) with subsetting Freq Vs. Prob
# ------------------------------------------------------------------------

# Plot the data Frequency 

# Baserate P(F)
ggplot(Alan_wave1[Alan_wave1$numerical_format=='frequency',], aes(x=baserate)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(xintercept=mean(Alan_wave1[Alan_wave1$numerical_format=='frequency',]$baserate), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(Alan_wave1[Alan_wave1$numerical_format=='frequency',]$baserate), 
             color="blue", linetype="solid", size=1)

# true positive P(A|F)
ggplot(Alan_wave1[Alan_wave1$numerical_format=='frequency',], aes(x=true_positive)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(Alan_wave1[Alan_wave1$numerical_format=='frequency',]$true_positive), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(Alan_wave1[Alan_wave1$numerical_format=='frequency',]$true_positive), 
             color="blue", linetype="solid", size=1)

# false alarm P(A|NF)
ggplot(Alan_wave1[Alan_wave1$numerical_format=='frequency',], aes(x=false_alarm)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(Alan_wave1[Alan_wave1$numerical_format=='frequency',]$false_alarm), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(Alan_wave1[Alan_wave1$numerical_format=='frequency',]$false_alarm), 
             color="blue", linetype="solid", size=1) 

# Plot the data Probability

# Baserate P(F)
ggplot(Alan_wave1[Alan_wave1$numerical_format=='probability',], aes(x=baserate)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(xintercept=mean(Alan_wave1[Alan_wave1$numerical_format=='probability',]$baserate), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(Alan_wave1[Alan_wave1$numerical_format=='probability',]$baserate), 
             color="blue", linetype="solid", size=1)

# true positive P(A|F)
ggplot(Alan_wave1[Alan_wave1$numerical_format=='probability',], aes(x=true_positive)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(Alan_wave1[Alan_wave1$numerical_format=='probability',]$true_positive), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(Alan_wave1[Alan_wave1$numerical_format=='probability',]$true_positive), 
             color="blue", linetype="solid", size=1)

# false alarm P(A|NF)
ggplot(Alan_wave1[Alan_wave1$numerical_format=='probability',], aes(x=false_alarm)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(Alan_wave1[Alan_wave1$numerical_format=='probability',]$false_alarm), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(Alan_wave1[Alan_wave1$numerical_format=='probability',]$false_alarm), 
             color="blue", linetype="solid", size=1) 


# ------------------------------------------------------------------------
# Testing PLOTTING (Wave 1) with subsetting distrustful_ps Vs. trustful_ps
# ------------------------------------------------------------------------

# let's subset the data in distrustful_ps/trustful_ps P(A|F) future prediction 
distrustful_ps <- Alan_wave1[Alan_wave1$true_positive< 0.5,]
trustful_ps <- Alan_wave1[Alan_wave1$true_positive>0.5,]

nrow(distrustful_ps)
nrow(trustful_ps)

nrow(distrustful_ps[distrustful_ps$correctness==1,])
nrow(trustful_ps[trustful_ps$correctness==1,])


ggplot(distrustful_ps, aes(x=baserate)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(xintercept=mean(distrustful_ps$baserate), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(distrustful_ps$baserate), 
             color="blue", linetype="solid", size=1)

ggplot(distrustful_ps, aes(x=true_positive)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(distrustful_ps$true_positive), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(distrustful_ps$true_positive), 
             color="blue", linetype="solid", size=1)

ggplot(distrustful_ps, aes(x=false_alarm)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(distrustful_ps$false_alarm), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(distrustful_ps$false_alarm), 
             color="blue", linetype="solid", size=1) 


ggplot(trustful_ps, aes(x=baserate)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(xintercept=mean(trustful_ps$baserate), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(trustful_ps$baserate), 
             color="blue", linetype="solid", size=1)

ggplot(trustful_ps, aes(x=true_positive)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(trustful_ps$true_positive), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(trustful_ps$true_positive), 
             color="blue", linetype="solid", size=1)

ggplot(trustful_ps, aes(x=false_alarm)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(trustful_ps$false_alarm), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(trustful_ps$false_alarm), 
             color="blue", linetype="solid", size=1)

# let's subset the data in conservativeringing_ps/keepringing_ps P(A|NF) future prediction 
conservativeringing_ps <- Alan_wave1[Alan_wave1$false_alarm< 0.5,]
keepringing_ps <- Alan_wave1[Alan_wave1$false_alarm>0.5,]

nrow(conservativeringing_ps)
nrow(keepringing_ps)

nrow(conservativeringing_ps[conservativeringing_ps$correctness==1,])
nrow(keepringing_ps[keepringing_ps$correctness==1,])


ggplot(conservativeringing_ps, aes(x=baserate)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(xintercept=mean(conservativeringing_ps$baserate), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(conservativeringing_ps$baserate), 
             color="blue", linetype="solid", size=1)

ggplot(conservativeringing_ps, aes(x=true_positive)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(conservativeringing_ps$true_positive), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(conservativeringing_ps$true_positive), 
             color="blue", linetype="solid", size=1)

ggplot(conservativeringing_ps, aes(x=false_alarm)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(conservativeringing_ps$false_alarm), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(conservativeringing_ps$false_alarm), 
             color="blue", linetype="solid", size=1) 


ggplot(keepringing_ps, aes(x=baserate)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(xintercept=mean(keepringing_ps$baserate), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(keepringing_ps$baserate), 
             color="blue", linetype="solid", size=1)

ggplot(keepringing_ps, aes(x=true_positive)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(keepringing_ps$true_positive), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(keepringing_ps$true_positive), 
             color="blue", linetype="solid", size=1)

ggplot(keepringing_ps, aes(x=false_alarm)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.05,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  geom_vline(xintercept=mean(keepringing_ps$false_alarm), 
             color="red", linetype="dashed", size = 1)+
  geom_vline(xintercept=median(keepringing_ps$false_alarm), 
             color="blue", linetype="solid", size=1)
#????????????????????????????????????????????????????????????????????????????????????????????


#----------------------------------------------------------------------------------------
# Merge all the DF and produce two CSV files
#----------------------------------------------------------------------------------------
final_alan_df <- do.call("rbind", list(Alan_wave1, Alan_wave2))
final_aitor_df <- do.call("rbind", list(Aitor_wave1, Aitor_wave2))
#View(Alan_wave1)
#View(Alan_wave2)
#View(Aitor_wave1)
#View(Aitor_wave2)

# find Ps duplicate (participants who repetaed mesure)
Ps_duplicate <- c(final_alan_df[duplicated(final_alan_df$ucivit_id),3])
#eliminate Ps duplicate (repeaters) from Alan
DF_prova <- final_alan_df[!final_alan_df$ucivit_id %in% Ps_duplicate,]


# do some counting 
nrow(DF_prova[DF_prova$Wave==1 & DF_prova$condition==1,])
nrow(DF_prova[DF_prova$Wave==1 & DF_prova$condition==2,])
nrow(DF_prova[DF_prova$Wave==2 & DF_prova$condition==1,])
nrow(DF_prova[DF_prova$Wave==2 & DF_prova$condition==2,])

nrow(DF_prova[DF_prova$Wave==1 & DF_prova$condition==1 & DF_prova$correctness==1,])
nrow(DF_prova[DF_prova$Wave==1 & DF_prova$condition==2 & DF_prova$correctness==1,])
nrow(DF_prova[DF_prova$Wave==2 & DF_prova$condition==1 & DF_prova$correctness==1,])
nrow(DF_prova[DF_prova$Wave==2 & DF_prova$condition==2 & DF_prova$correctness==1,])

# from Aitor 
final_aitor_df$sid <- as.character(final_aitor_df$sid)
DF_prova2 <- final_aitor_df[final_aitor_df$sid %in% DF_prova$ucivit_id,]


write.csv(DF_prova, file = "final_alan_df.csv", row.names=F)
write.csv(DF_prova2, file = "final_aitor_df.csv", row.names=F)

# cros checking with Aitor df for cheaters
#checkingData<- DF_prova[DF_prova$ucivit_id %in% x_ps,c(3,35,42)]
#checkingData <- checkingData[checkingData$Wave==2,]
#checkingData <- checkingData[checkingData$numerical_format == 'frequency',]
#View(DF_prova[DF_prova$ucivit_id %in%  ps_last_cheater,])
