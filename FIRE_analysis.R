set.seed(171219)
#-----------------------------------------------------------------------------------
# Author:       Manuele Reani
# Date:         16/07/2018
# Institution:  The university of Manchester - School of Computer Science
# Object:       This file contain the analysis (n-gram analysis, standard inferential stats,
#               identification of important n-grams, visualization, etc..), for data   
#               collected in a web application which is used to study human reasoning  
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


#----------------------------------------------------
#     DATA CLEANING AND DF PREPARATION 
#----------------------------------------------------

# Load the data and define the inputs
setwd("C:/Users/manuele/Desktop/PhDremote/paper4/FIRE/data&code") 
scanDF <-read.csv(file="analysis_scanpath.csv",header=TRUE,sep=",")


# recalculate some values based on the inputs 

scanDF$Correct_answer <- rep(0,nrow(scanDF))
scanDF$Given_answer <- rep(0,nrow(scanDF))
scanDF$Correct_answer_belief <- rep(0,nrow(scanDF))
scanDF$Bias_GC <- rep(0,nrow(scanDF))


scanDF[scanDF$numerical_format == 'frequency',]$Given_answer <- 
  scanDF[scanDF$numerical_format == 'frequency',]$answer1/
  scanDF[scanDF$numerical_format == 'frequency',]$answer2

scanDF[scanDF$numerical_format == 'probability',]$Given_answer <-
  scanDF[scanDF$numerical_format == 'probability',]$answer1/
  100

scanDF$Given_answer <- round(scanDF$Given_answer,2)

# calculate the correfdt answer according to beliefs 
scanDF$Correct_answer_belief <- (scanDF$baserate * scanDF$true_positive) /
  ((scanDF$baserate * scanDF$true_positive) + ((1-scanDF$baserate)* scanDF$false_alarm))
scanDF$Correct_answer_belief <- round(scanDF$Correct_answer_belief,3)

# add a small prob
scanDF$Given_answer <- scanDF$Given_answer + 0.0001
scanDF$Correct_answer <- scanDF$correct_answer + 0.0001
scanDF$Correct_answer_belief <- scanDF$Correct_answer_belief + 0.0001

# use the approxiamtion to make them the same 
scanDF[scanDF$correctness == 1,]$Given_answer <- 
  scanDF[scanDF$correctness == 1,]$Correct_answer
# calculate the bias
scanDF$Bias_GC <- log10(scanDF$Given_answer/
                        scanDF$Correct_answer)

scanDF[scanDF$Wave==1,]$baserate_avg <- 0
scanDF[scanDF$Wave==1,]$true_positive_avg <- 0 
scanDF[scanDF$Wave==1,]$false_alarm_avg <- 0

scanDF2 <- scanDF

# ----------------- NBB: THIS IS THE ACTUAL NEW DF --------------------------------------- #
# We need to eliminate those ps that have only 1 AOIs (e.e., 100% duration time in one AOI)
# Actually, we should eliminate from the analysis all Ps who do not have at least 3 letters
# and who did not look at teh question , e.g., location "H".
# But this means rerunning the whole regression and scanpath analysis again  
# perhaps only with bigrams 
# --------------------------------------------------------------------------------------- #

# Elimina i casi in cui non il partecipante non e' andato a leggere la domanda H, 
# e in cui il partecimante non ha vistitato almeno e AOIs H compreso (e.g., "HAC" va bene)
scanDF2$errore <- rep(1,nrow(scanDF2))
scanDF2$Scanpath <- as.character(scanDF2$Scanpath)

for(riga in c(1:nrow(scanDF2))){
  if (grepl("H", scanDF2[riga,]$Scanpath)){
    scanDF2[riga,]$errore <- 0
  }
}

scanDF3 <- scanDF2[scanDF2$errore == 0,]
for(riga in c(1:nrow(scanDF3))){
  if (length(unique(strsplit(unique(scanDF3[riga,]$Scanpath), "")[[1]]))<3){
    scanDF3[riga,]$errore <- 1
  }
}

scanDF4 <- scanDF3[scanDF3$errore == 0,]


# take off the last column (errore)
scanDF4 <- scanDF4[,-49]


# -----------------------------------------------------------
# Answers Analysis 
# -----------------------------------------------------------
w1F_responses <- scanDF4[scanDF4$correctness==0 &
          scanDF4$Wave == 1 &
          scanDF4$numerical_format == "frequency"
          ,c(41,42)]

nrow(w1F_responses[w1F_responses$answer2 %in% c(1000,1001),]) / nrow(w1F_responses)

w1P_responses <- scanDF4[scanDF4$correctness==0 &
               scanDF4$Wave == 1 &
               scanDF4$numerical_format == "probability"
             ,c(41,42)]

w2F_responses <- scanDF4[scanDF4$correctness==0 &
               scanDF4$Wave == 2 &
               scanDF4$numerical_format == "frequency"
             ,c(41,42)]

nrow(w2F_responses[w2F_responses$answer2 %in% c(1000,1001),]) / nrow(w2F_responses)

w2P_responses <- scanDF4[scanDF4$correctness==0 &
               scanDF4$Wave == 2 &
               scanDF4$numerical_format == "probability"
             ,c(41,42)]

#------------------------------------------------------------
# End Answers Analysis 
#------------------------------------------------------------




# substitute DF for the following analysis 
# this is the new DF
scanDF2 <- 
  scanDF4[,c(1,2,3,7,8,10,11,28,29,
            30,31,32,33,34,37,39,
            44,45,46,47,48)]

# add a bias of the belief 
scanDF2$Bias_belief <- log10(scanDF2$Correct_answer_belief/
                               scanDF2$Correct_answer)


#------------------------------------------------
#   DESCRIPTIVE STATISTICS
#------------------------------------------------
# count the number of Ps
nrow(scanDF2[scanDF2$Wave == 1,])
nrow(scanDF2[scanDF2$Wave == 2,])

nrow(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'frequency', ]) 
nrow(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'probability', ]) 
nrow(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency', ]) 
nrow(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability', ]) 

# Count percentage of correct by condition
nrow(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'frequency' & 
               scanDF2$correctness==1, ]) / 
  nrow(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'frequency', ]) 
nrow(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'probability' & 
               scanDF2$correctness==1, ]) / 
  nrow(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'probability', ])

nrow(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency' & 
               scanDF2$correctness==1, ]) / 
  nrow(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency', ]) 
nrow(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability' & 
               scanDF2$correctness==1, ]) / 
  nrow(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability', ])

# Count based on individual differences 
nrow(scanDF2[scanDF2$numeracy < 3 & scanDF2$correctness==1, ])
nrow(scanDF2[scanDF2$numeracy == 3 & scanDF2$correctness==1, ])
nrow(scanDF2[scanDF2$numeracy > 3 & scanDF2$correctness==1, ])
            
# median and IQR for Baserate 
x <- c(median(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'frequency', ]$baserate), 
  IQR(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'frequency', ]$baserate),
  median(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'probability', ]$baserate),
  IQR(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'probability', ]$baserate),
  median(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency', ]$baserate),
  IQR(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency', ]$baserate),
  median(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability', ]$baserate),
  IQR(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability', ]$baserate),
  median(scanDF2[scanDF2$alarm < 3, ]$baserate),
  IQR(scanDF2[scanDF2$alarm < 3, ]$baserate),
  median(scanDF2[scanDF2$alarm == 3, ]$baserate),
  IQR(scanDF2[scanDF2$alarm == 3, ]$baserate),
  median(scanDF2[scanDF2$alarm > 3, ]$baserate),
  IQR(scanDF2[scanDF2$alarm > 3, ]$baserate),
  median(scanDF2[scanDF2$fire < 3, ]$baserate),
  IQR(scanDF2[scanDF2$fire < 3, ]$baserate),
  median(scanDF2[scanDF2$fire == 3, ]$baserate),
  IQR(scanDF2[scanDF2$fire == 3, ]$baserate),
  median(scanDF2[scanDF2$fire > 3, ]$baserate), 
  IQR(scanDF2[scanDF2$fire > 3, ]$baserate),
  median(scanDF2[scanDF2$numeracy < 3, ]$baserate),
  IQR(scanDF2[scanDF2$numeracy < 3, ]$baserate),
  median(scanDF2[scanDF2$numeracy == 3, ]$baserate),
  IQR(scanDF2[scanDF2$numeracy == 3, ]$baserate), 
  median(scanDF2[scanDF2$numeracy > 3, ]$baserate),
  IQR(scanDF2[scanDF2$numeracy > 3, ]$baserate))


B <- matrix(x, nrow = 1, ncol = length(x))


# median and IQR for True Positive 
k <- c(median(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'frequency', ]$true_positive), 
       IQR(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'frequency', ]$true_positive),
       median(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'probability', ]$true_positive),
       IQR(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'probability', ]$true_positive),
       median(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency', ]$true_positive),
       IQR(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency', ]$true_positive),
       median(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability', ]$true_positive),
       IQR(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability', ]$true_positive),
       median(scanDF2[scanDF2$alarm < 3, ]$true_positive),
       IQR(scanDF2[scanDF2$alarm < 3, ]$true_positive),
       median(scanDF2[scanDF2$alarm == 3, ]$true_positive),
       IQR(scanDF2[scanDF2$alarm == 3, ]$true_positive),
       median(scanDF2[scanDF2$alarm > 3, ]$true_positive),
       IQR(scanDF2[scanDF2$alarm > 3, ]$true_positive),
       median(scanDF2[scanDF2$fire < 3, ]$true_positive),
       IQR(scanDF2[scanDF2$fire < 3, ]$true_positive),
       median(scanDF2[scanDF2$fire == 3, ]$true_positive),
       IQR(scanDF2[scanDF2$fire == 3, ]$true_positive),
       median(scanDF2[scanDF2$fire > 3, ]$true_positive), 
       IQR(scanDF2[scanDF2$fire > 3, ]$true_positive),
       median(scanDF2[scanDF2$numeracy < 3, ]$true_positive),
       IQR(scanDF2[scanDF2$numeracy < 3, ]$true_positive),
       median(scanDF2[scanDF2$numeracy == 3, ]$true_positive),
       IQR(scanDF2[scanDF2$numeracy == 3, ]$true_positive), 
       median(scanDF2[scanDF2$numeracy > 3, ]$true_positive),
       IQR(scanDF2[scanDF2$numeracy > 3, ]$true_positive))


N <- matrix(k, nrow = 1, ncol = length(k))

# median and IQR for false alarm 
R <- c(median(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'frequency', ]$false_alarm), 
       IQR(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'frequency', ]$false_alarm),
       median(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'probability', ]$false_alarm),
       IQR(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'probability', ]$false_alarm),
       median(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency', ]$false_alarm),
       IQR(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency', ]$false_alarm),
       median(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability', ]$false_alarm),
       IQR(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability', ]$false_alarm),
       median(scanDF2[scanDF2$alarm < 3, ]$false_alarm),
       IQR(scanDF2[scanDF2$alarm < 3, ]$false_alarm),
       median(scanDF2[scanDF2$alarm == 3, ]$false_alarm),
       IQR(scanDF2[scanDF2$alarm == 3, ]$false_alarm),
       median(scanDF2[scanDF2$alarm > 3, ]$false_alarm),
       IQR(scanDF2[scanDF2$alarm > 3, ]$false_alarm),
       median(scanDF2[scanDF2$fire < 3, ]$false_alarm),
       IQR(scanDF2[scanDF2$fire < 3, ]$false_alarm),
       median(scanDF2[scanDF2$fire == 3, ]$false_alarm),
       IQR(scanDF2[scanDF2$fire == 3, ]$false_alarm),
       median(scanDF2[scanDF2$fire > 3, ]$false_alarm), 
       IQR(scanDF2[scanDF2$fire > 3, ]$false_alarm),
       median(scanDF2[scanDF2$numeracy < 3, ]$false_alarm),
       IQR(scanDF2[scanDF2$numeracy < 3, ]$false_alarm),
       median(scanDF2[scanDF2$numeracy == 3, ]$false_alarm),
       IQR(scanDF2[scanDF2$numeracy == 3, ]$false_alarm), 
       median(scanDF2[scanDF2$numeracy > 3, ]$false_alarm),
       IQR(scanDF2[scanDF2$numeracy > 3, ]$false_alarm))


Z <- matrix(R, nrow = 1, ncol = length(R))


# median and IQR for bias
O <- c(median(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'frequency', ]$Bias_GC), 
       IQR(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'frequency', ]$Bias_GC),
       median(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'probability', ]$Bias_GC),
       IQR(scanDF2[scanDF2$Wave == 1 & scanDF2$numerical_format == 'probability', ]$Bias_GC),
       median(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency', ]$Bias_GC),
       IQR(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency', ]$Bias_GC),
       median(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability', ]$Bias_GC),
       IQR(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability', ]$Bias_GC),
       median(scanDF2[scanDF2$alarm < 3, ]$Bias_GC),
       IQR(scanDF2[scanDF2$alarm < 3, ]$Bias_GC),
       median(scanDF2[scanDF2$alarm == 3, ]$Bias_GC),
       IQR(scanDF2[scanDF2$alarm == 3, ]$Bias_GC),
       median(scanDF2[scanDF2$alarm > 3, ]$Bias_GC),
       IQR(scanDF2[scanDF2$alarm > 3, ]$Bias_GC),
       median(scanDF2[scanDF2$fire < 3, ]$Bias_GC),
       IQR(scanDF2[scanDF2$fire < 3, ]$Bias_GC),
       median(scanDF2[scanDF2$fire == 3, ]$Bias_GC),
       IQR(scanDF2[scanDF2$fire == 3, ]$Bias_GC),
       median(scanDF2[scanDF2$fire > 3, ]$Bias_GC), 
       IQR(scanDF2[scanDF2$fire > 3, ]$Bias_GC),
       median(scanDF2[scanDF2$numeracy < 3, ]$Bias_GC),
       IQR(scanDF2[scanDF2$numeracy < 3, ]$Bias_GC),
       median(scanDF2[scanDF2$numeracy == 3, ]$Bias_GC),
       IQR(scanDF2[scanDF2$numeracy == 3, ]$Bias_GC), 
       median(scanDF2[scanDF2$numeracy > 3, ]$Bias_GC),
       IQR(scanDF2[scanDF2$numeracy > 3, ]$Bias_GC))


Y <- matrix(O, nrow = 1, ncol = length(O))

# median and IQR for bias belief 
G <- c(median(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency', ]$Bias_belief),
       IQR(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'frequency', ]$Bias_belief),
       median(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability', ]$Bias_belief),
       IQR(scanDF2[scanDF2$Wave == 2 & scanDF2$numerical_format == 'probability', ]$Bias_belief),
       median(scanDF2[scanDF2$alarm < 3 & scanDF2$Wave == 2, ]$Bias_belief),
       IQR(scanDF2[scanDF2$alarm < 3 & scanDF2$Wave == 2, ]$Bias_belief),
       median(scanDF2[scanDF2$alarm == 3 & scanDF2$Wave == 2, ]$Bias_belief),
       IQR(scanDF2[scanDF2$alarm == 3 & scanDF2$Wave == 2, ]$Bias_belief),
       median(scanDF2[scanDF2$alarm > 3 & scanDF2$Wave == 2, ]$Bias_belief),
       IQR(scanDF2[scanDF2$alarm > 3 & scanDF2$Wave == 2, ]$Bias_belief),
       median(scanDF2[scanDF2$fire < 3 & scanDF2$Wave == 2, ]$Bias_belief),
       IQR(scanDF2[scanDF2$fire < 3 & scanDF2$Wave == 2, ]$Bias_belief),
       median(scanDF2[scanDF2$fire == 3 & scanDF2$Wave == 2, ]$Bias_belief),
       IQR(scanDF2[scanDF2$fire == 3 & scanDF2$Wave == 2, ]$Bias_belief),
       median(scanDF2[scanDF2$fire > 3 & scanDF2$Wave == 2, ]$Bias_belief), 
       IQR(scanDF2[scanDF2$fire > 3 & scanDF2$Wave == 2, ]$Bias_belief),
       median(scanDF2[scanDF2$numeracy < 3 & scanDF2$Wave == 2, ]$Bias_belief),
       IQR(scanDF2[scanDF2$numeracy < 3 & scanDF2$Wave == 2, ]$Bias_belief),
       median(scanDF2[scanDF2$numeracy == 3 & scanDF2$Wave == 2, ]$Bias_belief),
       IQR(scanDF2[scanDF2$numeracy == 3 & scanDF2$Wave == 2, ]$Bias_belief), 
       median(scanDF2[scanDF2$numeracy > 3 & scanDF2$Wave == 2, ]$Bias_belief),
       IQR(scanDF2[scanDF2$numeracy > 3 & scanDF2$Wave == 2, ]$Bias_belief))


J <- matrix(G, nrow = 1, ncol = length(G))

IQR(scanDF2[scanDF2$Wave==1 & scanDF2$numerical_format == "frequency",]$Bias_GC)




# logistic regression for correctness with wave as a predictor 
scanDF2$correctness <- relevel(as.factor(scanDF2$correctness),"0")
scanDF2$Wave <- relevel(as.factor(scanDF2$Wave),"1")
scanDF2$numerical_format <- relevel(as.factor(scanDF2$numerical_format),"frequency")
# chenage the level names for the factor correctness 
levels(scanDF2$correctness) <- c("incorrect", "correct")

# descriptive stats for numeracy accross gropups 
median(scanDF2[scanDF2$correctness == "incorrect" & 
                 scanDF2$numerical_format == "frequency",]$numeracy)
IQR(scanDF2[scanDF2$correctness == "incorrect" & 
              scanDF2$numerical_format == "frequency",]$numeracy)

median(scanDF2[scanDF2$correctness == "incorrect" & 
                 scanDF2$numerical_format == "probability",]$numeracy)
IQR(scanDF2[scanDF2$correctness == "incorrect" & 
              scanDF2$numerical_format == "probability",]$numeracy)

median(scanDF2[scanDF2$correctness == "correct" & 
                 scanDF2$numerical_format == "frequency",]$numeracy)
IQR(scanDF2[scanDF2$correctness == "correct" & 
              scanDF2$numerical_format == "frequency",]$numeracy)

median(scanDF2[scanDF2$correctness == "correct" & 
                 scanDF2$numerical_format == "probability",]$numeracy)
IQR(scanDF2[scanDF2$correctness == "correct" & 
              scanDF2$numerical_format == "probability",]$numeracy)


#-----------------------------------------------------------------
#         PLOTTING
#-----------------------------------------------------------------
# distributions 
ggplot(scanDF2, aes(x=Wave, y=Bias_GC, fill= numerical_format)) + 
  geom_boxplot() +
  labs(x = "Wave", y = "Bias") +
  scale_fill_grey(start = 0.3, end = 0.9, name="Format")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        legend.title = element_text(size = 12), legend.text = element_text(size = 12), 
        legend.key.size = unit(0.5, "cm")) 

# create a categorcial variable for numeracy
scanDF2$Numeracy_cat <- rep("low", nrow(scanDF2))
scanDF2[scanDF2$numeracy > 3,]$Numeracy_cat <- "high"
scanDF2[scanDF2$numeracy == 3,]$Numeracy_cat <- "mid"
# reorder the variable 
scanDF2$Numeracy_cat <- factor(scanDF2$Numeracy_cat , 
                               levels = c("low", "mid", "high"))

ggplot(scanDF2, aes(x=Numeracy_cat, y=abs(Bias_GC), fill= numerical_format)) + 
  geom_boxplot() +
  labs(x = "Numeracy", y = "Bias") +
  scale_fill_grey(start = 0.3, end = 0.9, name="Format")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        legend.title = element_text(size = 12), legend.text = element_text(size = 12), 
        legend.key.size = unit(0.5, "cm")) 

ggplot(scanDF2, aes(x=numeracy, y=abs(Bias_GC))) +
  geom_bar(stat='identity')

ggplot(scanDF2, aes(x=numeracy, y=alarm)) +
  geom_jitter(shape=21, size =2.5)+
  facet_grid(.~correctness)
ggplot(scanDF2, aes(x=numeracy, y=fire)) +
  geom_jitter(shape=21, size =2.5)+
  facet_grid(.~correctness)
          
# Find the mean of each group library(plyr)
cdat <- ddply(scanDF2, c("numerical_format","Wave"), summarise, rating.mean=mean(Bias_GC))

# Draw with black outline, white fill
ggplot(scanDF2, aes(x=Bias_GC)) +
  geom_histogram(binwidth=.3, colour="black", fill="white")+
  facet_grid(Wave~numerical_format) +
  geom_vline(data=cdat, aes(xintercept=rating.mean),
           linetype="dashed", size=1, colour="red")


# Find the mean of each group library(plyr)
cdat2 <- ddply(scanDF2[scanDF2$Wave==2,], 
               c("numerical_format"), summarise, rating.mean=median(Bias_belief))

# Draw with black outline, white fill (change the x name into Log-Belief-Deviation)
ggplot(scanDF2[scanDF2$Wave==2,], aes(x=Bias_belief)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")+
  facet_grid(.~numerical_format) +
  geom_vline(data=cdat2, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")+
  labs(x = "Log-Experience-Deviation") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        strip.text.x = element_text(size = 14))

# Find the mean of each group library(plyr)
#cdat3 <- ddply(scanDF2[scanDF2$Wave==2,], 
 #              c("numerical_format"), summarise, rating.median=median(Correct_answer_belief))

# Draw with black outline, white fill
#ggplot(scanDF2[scanDF2$Wave==2,], aes(x=Correct_answer_belief)) +
 # geom_histogram(binwidth=.1, colour="black", fill="white")+
  #facet_grid(.~numerical_format) +
  #geom_vline(data=cdat3, aes(xintercept=rating.median),
   #          linetype="dashed", size=1, colour="red")


# relationships correclations 
ggplot() + 
  xlim(-0.05,1.05)+
  stat_smooth(data = scanDF2[scanDF2$Wave==2,], 
              aes(x = Correct_answer_belief, y = Given_answer), 
              method = "lm", col = "blue", fullrange = T) +
  stat_smooth(data = scanDF2[scanDF2$Wave==1,], 
              aes(x = Correct_answer_belief, y = Given_answer),
    method = "lm", col = "red", fullrange = T) +
  geom_jitter(data = scanDF2, 
              aes(x = Correct_answer_belief, y = Given_answer, color = Wave),
    width = 0.02, height = 0.02, shape=21, size =2.5)+
  scale_colour_manual(values = c("red","blue")) +
  labs(x = "Believed Probability", y = "Estimated Probability", colour = "Study") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        legend.title = element_text(size = 12), legend.text = element_text(size = 12), 
        legend.key.size = unit(0.5, "cm")) + 
  theme_bw()

ggplot(scanDF2[scanDF2$Wave==2,], 
       aes(x = Bias_belief, y = Bias_GC)) + 
  geom_jitter(width = 0.05, height = 0.5, shape=21, size =2.5) +
  xlim(-3.4,1)+
  stat_smooth(method = "lm", col = "red", fullrange = T) +
  labs(x = "Bias belief", y = "Bias Estimate") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))+ 
  theme_bw()

ggplot(scanDF2, aes(x = numerical_format, y = Bias_GC)) +
  geom_jitter(size = 4, alpha = 0.2, width = 0.2, height = 0)  + 
  geom_jitter(data=scanDF2[scanDF2$Bias_GC==0, ], 
              aes(x=numerical_format, y=Bias_GC), 
              colour="red", size=4, alpha = 0.2)+
  xlab("Information Format")+ 
  ylab("Bias Estimate")+
  facet_grid(.~Wave) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red", size=1)

# Find the mean of each group library(plyr)
cdat3 <- ddply(scanDF2, 
               c("numerical_format","correctness"), summarise, rating.mean=median(numeracy))

# Draw with black outline, white fill
ggplot(scanDF2, aes(x=numeracy, y = ..ncount..)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")+
  facet_grid(correctness~numerical_format) +
  geom_vline(data=cdat3, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")+
  labs(x = "Numeracy") +
  labs(y = "Normalized Count") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14))


#-----------------------------------------------------------------
#         REGRESSION ANALYSIS 
#-----------------------------------------------------------------

correctModel <- glm(correctness ~ 
                      numerical_format + Wave + numeracy, # + alarm + fire, 
                    data = scanDF2, 
                    family=binomial(link="logit"))

summary(correctModel)

# compute Odd ratios and CI 
exp(summary(correctModel)$coefficients[2,1] + 
      qnorm(c(0.025,0.5,0.975)) * 
      summary(correctModel)$coefficients[2,2])
exp(summary(correctModel)$coefficients[3,1] + 
      qnorm(c(0.025,0.5,0.975)) * 
      summary(correctModel)$coefficients[2,2])
exp(summary(correctModel)$coefficients[4,1] + 
      qnorm(c(0.025,0.5,0.975)) * 
      summary(correctModel)$coefficients[2,2])


# linear regression for bias with wave as a predictor 

#biasModel <- glm(Bias_GC ~ 
 #                  numerical_format + Wave + numeracy + alarm + fire, 
  #               data = scanDF2)

#summary(biasModel)

# logistic regression for correctness only for wave 2 with bias_belief as a predictor 
#correctModel2 <- glm(correctness ~ 
 #                     numerical_format + Bias_belief + numeracy,# + alarm + fire, 
  #                  data = scanDF2[scanDF2$Wave==2,], 
   #                 family=binomial(link="logit"))

#summary(correctModel2)

# linear regression for bias only for wave 2 with bias_belief as a predictor 
# max-likelyhood method
biasModel2 <- glm(Bias_GC ~ 
                   numerical_format + Bias_belief, #+ numeracy + alarm + fire, 
                 data = scanDF2[scanDF2$Wave==2,])

summary(biasModel2)
# caluclate confidence intervals 
confint(biasModel2)

# least-square method
#biasModel3 <- lm(Bias_GC ~ 
 #                   numerical_format + Bias_belief, #+ numeracy + alarm + fire, 
  #                data = scanDF2[scanDF2$Wave==2,])

#summary(biasModel3)





# Write the DF for the scanpath analysis to be perfopmed with the R file FIRE_mains_scanpath
write.csv(scanDF2, file = "scanDF2.csv", row.names=F) # NB



#-------------------------------------------------------------------------------
# Produce the analysis for the Manuscript: SCANPATH
# ------------------------------------------------------------------------------
# Load the CSV files produced by the script FIRE_mains_scanpath

# Comparing frequency vs probability, for the 
# two DFs separately, wave 1 and wave 2
freq_prob_df <-read.csv(file="TESToddData_total2.csv",header=TRUE,sep=",")

#write.csv(results_ngram, file = "TESThellData.csv", row.names=F) 
#write.csv(results_odds, file = "TESToddData.csv", row.names=F) 
#write.csv(distributionsDf, file = "TESTdistriData.csv", row.names=F) 
#write.csv(results_odds_total, file = "TESToddData_total.csv", row.names=F) # NEW
#write.csv(dfPROVARE, file = "frequenzeDF.csv", row.names=F)

# Comparing correct vs incorrect, for the 
# two DFs separately, frequency (wave 1 only) and probability (wave1 only) 2)
corr_incorr_df <-read.csv(file="TESToddData_total_corIncor2.csv",header=TRUE,sep=",")

#write.csv(results_ngram, file = "TESThellData_corIncor.csv", row.names=F) 
#write.csv(results_odds, file = "TESToddData_corIncor.csv", row.names=F) 
#write.csv(distributionsDf, file = "TESTdistriData_corIncor.csv", row.names=F) 
#write.csv(results_odds_total, file = "TESToddData_total_corIncor.csv", row.names=F) # NEW
#write.csv(dfPROVARE, file = "frequenzeDF_corIncor.csv", row.names=F)

# calculate the log odds-ratios
freq_prob_df$log_OR <- log(freq_prob_df$OR)
corr_incorr_df$log_OR <- log(corr_incorr_df$OR)



# Correct the wrong data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
freq_prob_df[freq_prob_df$Experiment == "Study1" &
  freq_prob_df$n_gram %in% c("CF", "FB"),]$Sig <- "N"
freq_prob_df[freq_prob_df$Experiment == "Study1" &
               freq_prob_df$n_gram == "CF",]$OR <- 1.1
freq_prob_df[freq_prob_df$Experiment == "Study1" &
               freq_prob_df$n_gram == "FB",]$Frequency <- 10

freq_prob_df[freq_prob_df$Experiment == "Study2" &
               freq_prob_df$n_gram == "BE",]$Sig <- "N"
freq_prob_df[freq_prob_df$Experiment == "Study2" &
               freq_prob_df$n_gram == "BE",]$OR <- 0.95
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 


# Produce a graph showing the discriminative n-garms 
# plot the data looking at thresholds 
ggplot(freq_prob_df, aes(x = OR, y = Frequency)) +
  geom_point(data=freq_prob_df[freq_prob_df$Sig=="N",], 
             aes(x = OR, y = Frequency), shape=1, size = 4)  +
  geom_point(data=freq_prob_df[freq_prob_df$Sig=="Y",], 
             aes(x = OR, y = Frequency),color="red", shape=1, size = 4, stroke = 1.5)  + 
  xlab("Odds-Ratio") + 
  scale_x_continuous(trans='log10') + 
  facet_grid(.~ Experiment)+
  geom_vline(xintercept = 1,linetype = "longdash", size = 0.5, color = 'blue') +
  #geom_text(aes(label=ifelse(OR>=2 & 
  #                             Frequency >= 90,as.character(n_gram),'')),
  #          hjust=-0.5,vjust=-0.4, color = "red")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14))
  #geom_text(data = freq_prob_df[freq_prob_df$Sig=="Y",], 
            #aes(x = log_OR,y = Frequency,label = n_gram),inherit.aes=F,
            #hjust=-0.5,vjust=-0.4, color = "red")


# correct and incorrect comparison 
corr_incorr_df

# Produce a graph showing the discriminative n-garms 
# plot the data looking at thresholds 
ggplot(corr_incorr_df, aes(x = log_OR, y = Frequency)) +
  geom_point(data=corr_incorr_df[corr_incorr_df$Sig=="N",], 
             aes(x = log_OR, y = Frequency), shape=1, size = 4)  +
  geom_point(data=corr_incorr_df[corr_incorr_df$Sig=="Y",], 
             aes(x = log_OR, y = Frequency),color="red", shape=1, size = 4, stroke = 1.5)  + 
  xlab("Log Odds-Ratio") + 
  #scale_y_continuous(trans='log10') + 
  facet_grid(.~ numerical_format)+
  geom_vline(xintercept = 0,linetype = "longdash", size = 0.5, color = 'blue') +
  geom_text(aes(label=ifelse(log_OR>=0.5 & 
                               Frequency >= 90,as.character(n_gram),'')),
            hjust=-0.5,vjust=-0.4, color = "red")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14))




# Create a DF with all n-grams together
# study 1
# frequency 
study1_freq <- freq_prob_df[freq_prob_df$Sig == "Y" & 
                              freq_prob_df$Experiment == "Study1" &
                              freq_prob_df$log_OR > 0,]
# probability
study1_prob <- freq_prob_df[freq_prob_df$Sig == "Y" & 
                              freq_prob_df$Experiment == "Study1" &
                              freq_prob_df$log_OR < 0,]

# study 2
# frequency
study2_freq <- freq_prob_df[freq_prob_df$Sig == "Y" & 
                              freq_prob_df$Experiment == "Study2" &
                              freq_prob_df$log_OR > 0,]
# probability
study2_prob <- freq_prob_df[freq_prob_df$Sig == "Y" & 
                              freq_prob_df$Experiment == "Study2" &
                              freq_prob_df$log_OR < 0,]
# DFs aggragated for the ORs
study1_DF <- freq_prob_df[freq_prob_df$Sig == "Y" & 
                              freq_prob_df$Experiment == "Study1",]
study2_DF <- freq_prob_df[freq_prob_df$Sig == "Y" & 
                              freq_prob_df$Experiment == "Study2",]



# correct and incorrect comparison 
freq_DF <- corr_incorr_df[corr_incorr_df$Sig == "Y" & 
                              corr_incorr_df$numerical_format == "frequency",]
prob_DF <- corr_incorr_df[corr_incorr_df$Sig == "Y" & 
                              corr_incorr_df$numerical_format == "probability",]


#----------------------------------------------
# Analysis Frequency and duration per AOIs
#----------------------------------------------
duration_df <-read.csv(file="analysis_duration.csv",header=TRUE,sep=",")
colnames(duration_df)[3] <- "Ps"

duration_df2 <- merge(duration_df, scanDF2[,c(1,16)], by="Ps")
duration_df2 <- duration_df2[,-c(2,3)]

# compute the duration for each AOIs for Ps
dfTEMP0 <- data.frame(Ps = NULL, 
                      node.id = NULL, 
                      n_format = NULL, 
                      wave = NULL,
                      duration = NULL,
                      correctness = NULL)

for (i in unique(duration_df2$Ps)){ 
  # this is my new dataframe subsetted duration_df2[duration_df2$Ps == i,]
  df <- duration_df2[duration_df2$Ps == i,]
  for (k in levels(duration_df2$node.id)){
    durata <- 0
    for (riga in c(1:nrow(df))){
      if (df[riga, 2] == k){
        durata <- durata + df[riga, 5]
      }
    }
    newrow <- data.frame(Ps = i, 
                       node.id = k, 
                       n_format = df[riga,3], 
                       wave = df[riga,4],
                       duration = durata,
                       correctness = df[riga,6])
    dfTEMP0 <- rbind(dfTEMP0,newrow)
  }
}

# normalize the duration by Ps
dfTEMP0$NormalTime <- rep(0,nrow(dfTEMP0))
for (p in unique(dfTEMP0$Ps)){
  dfTEMP0[dfTEMP0$Ps==p,]$NormalTime <- dfTEMP0[dfTEMP0$Ps==p,]$duration / 
    sum(dfTEMP0[dfTEMP0$Ps==p,]$duration)
}
    
dfTEMP0$NormalTime<-dfTEMP0$NormalTime*100

# change the names of the AOIs ("A" "B" "C" "D" "E" "F" "G" "H")
levels(dfTEMP0$node.id) <- c("T","F","nF","FA","FnA","nFA","nFnA","Q")

# Study 1
# plot the percentage duration by AOI and by n_format
ggplot(dfTEMP0[dfTEMP0$wave==1,], aes(x=node.id, y=NormalTime, fill= n_format)) + 
  geom_boxplot() +
  labs(x = "Buttons (Study 1)", y = "Total Dwell Time (%)") +
  scale_fill_grey(start = 0.5, end = 0.9, name="Numerical Format")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        legend.title = element_text(size = 12), legend.text = element_text(size = 12), 
        legend.key.size = unit(0.5, "cm")) 
# Study 2
# plot the percentage duration by AOI and by n_format
ggplot(dfTEMP0[dfTEMP0$wave==2,], aes(x=node.id, y=NormalTime, fill= n_format)) + 
  geom_boxplot() +
  labs(x = "Buttons (Study 2)", y = "Total Dwell Time (%)") +
  scale_fill_grey(start = 0.5, end = 0.9, name="Numerical Format")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        legend.title = element_text(size = 12), legend.text = element_text(size = 12), 
        legend.key.size = unit(0.5, "cm")) 

# produce the descriptive statistics (using medians) for proportion dwell time (percentage)
W1F_m <- c(median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "T", 7]),
         median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "F", 7]),
         median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nF", 7]),
         median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FA", 7]),
         median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FnA", 7]),
         median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFA", 7]),
         median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFnA", 7]),
         median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "Q", 7]))

W1F_iqr <- c(IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "T", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "F", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nF", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FnA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFnA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "Q", 7]))

W1P_m <- c(median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "T", 7]),
           median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "F", 7]),
           median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nF", 7]),
           median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FA", 7]),
           median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FnA", 7]),
           median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFA", 7]),
           median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFnA", 7]),
           median(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "Q", 7]))


W1P_iqr <- c(IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "T", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "F", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nF", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FnA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFnA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "Q", 7]))

W2F_m <- c(median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "T", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "F", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nF", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FA", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FnA", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFA", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFnA", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "Q", 7]))

W2F_iqr <- c(IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "T", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "F", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nF", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FnA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFnA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "Q", 7]))

W2P_m <- c(median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "T", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "F", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nF", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FA", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FnA", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFA", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFnA", 7]),
           median(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "Q", 7]))


W2P_iqr <- c(IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "T", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "F", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nF", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FnA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFnA", 7]),
             IQR(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "Q", 7]))

matriceX <- cbind(W1F_m,W1F_iqr,W1P_m,W1P_iqr,W2F_m,W2F_iqr,W2P_m,W2P_iqr)  
descriptiveSTATS <- as.data.frame(matriceX)
descriptiveSTATS <- round(descriptiveSTATS,0)
list_of_AOIs <- c(levels(dfTEMP0$node.id)) 
descriptiveSTATS <- cbind(list_of_AOIs,descriptiveSTATS)


# produce the descriptive statistics (using means) for proportion dwell time (percentage)
W1F_m <- c(mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "T", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "F", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nF", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FnA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFnA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "Q", 7]))

W1F_iqr <- c(sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "T", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "F", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nF", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FnA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFnA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "Q", 7]))

W1P_m <- c(mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "T", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "F", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nF", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FnA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFnA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "Q", 7]))


W1P_iqr <- c(sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "T", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "F", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nF", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FnA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFnA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "Q", 7]))

W2F_m <- c(mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "T", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "F", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nF", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FnA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFnA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "Q", 7]))

W2F_iqr <- c(sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "T", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "F", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nF", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "FnA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "nFnA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='freq' & dfTEMP0$node.id == "Q", 7]))

W2P_m <- c(mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "T", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "F", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nF", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FnA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFnA", 7]),
           mean(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "Q", 7]))


W2P_iqr <- c(sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "T", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "F", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nF", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "FnA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "nFnA", 7]),
             sd(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$n_format=='prob' & dfTEMP0$node.id == "Q", 7]))

matriceX <- cbind(W1F_m,W1F_iqr,W1P_m,W1P_iqr,W2F_m,W2F_iqr,W2P_m,W2P_iqr)  
descriptiveSTATS2 <- as.data.frame(matriceX)
descriptiveSTATS2 <- round(descriptiveSTATS2,0)
list_of_AOIs <- c(levels(dfTEMP0$node.id)) 
descriptiveSTATS2 <- cbind(list_of_AOIs,descriptiveSTATS2)




# check some weird data       
#dfTEMP0[dfTEMP0$Ps == "b2a7a53ec0b1d91c",]  
#scanDF2[scanDF2$Ps == 'b2a7a53ec0b1d91c',] 
#dfTEMP0[dfTEMP0$Ps == "c19b08027067b18f",]  
#scanDF2[scanDF2$Ps == 'c19b08027067b18f',] 

# generate density plot

distri_DF <-read.csv(file="TESTdistriData.csv",header=TRUE,sep=",")
hellDF <- read.csv(file="TESThellData.csv",header=TRUE,sep=",")


par(mfrow=c(1,2))
# Study 1
bayes_DISTANCEs_2g <- append(distri_DF[distri_DF$Exp == "Study1",]$DistrVec, 
                             hellDF[hellDF$Experiment == "Study1",]$HellDists)
bayes_density_plot2g <- density(bayes_DISTANCEs_2g)
plot(bayes_density_plot2g, type = "n", main = "", xlab = "Hellinger distance (Study 1)")
polygon(bayes_density_plot2g, col = "lightgray", border = "grey")
#rug(bayes_DISTANCEs_2g, col = ifelse(bayes_DISTANCEs_2g == hellDF[hellDF$Ngrams==2 & hellDF$Experiment == "Bayes",]$HellDists, 'blue', 'red'))
abline(v = hellDF[hellDF$Experiment == "Study1",]$HellDists, col = "red", lwd = 2)
abline(v = hellDF[hellDF$Experiment == "Study1",]$Mhell, col = "black", lty=2)

# Study 2
bayes_DISTANCEs_2g <- append(distri_DF[distri_DF$Exp == "Study2",]$DistrVec, 
                             hellDF[hellDF$Experiment == "Study2",]$HellDists)
bayes_density_plot2g <- density(bayes_DISTANCEs_2g)
plot(bayes_density_plot2g, type = "n", main = "", xlab = "Hellinger distance (Study 2)")
polygon(bayes_density_plot2g, col = "lightgray", border = "grey")
#rug(bayes_DISTANCEs_2g, col = ifelse(bayes_DISTANCEs_2g == hellDF[hellDF$Ngrams==2 & hellDF$Experiment == "Bayes",]$HellDists, 'blue', 'red'))
abline(v = hellDF[hellDF$Experiment == "Study2",]$HellDists, col = "red", lwd = 2)
abline(v = hellDF[hellDF$Experiment == "Study2",]$Mhell, col = "black", lty=2)



# ------------------------------------------
# Check the answers to the questions 
# -------------------------------------------

# check the people who confuse sensitivity with PPV ----------------------------
DF_check <- read.csv(file="scanDF2.csv",header=TRUE,sep=",")
  
DF_check_p <- DF_check[DF_check$numerical_format == "probability" & 
                         DF_check$correctness == "incorrect",]

DF_check_pw1 <- DF_check_p[DF_check_p$Wave == 1,]
DF_check_pw2 <- DF_check_p[DF_check_p$Wave == 2,]

#DF_check_pw1$error <- rep(0, nrow(DF_check_pw1))
#DF_check_pw1[DF_check_pw1$true_positive < DF_check_pw1$Given_answer + 0.01 &
#               DF_check_pw1$true_positive > DF_check_pw1$Given_answer - 0.01,]$error <- 1

nrow(DF_check_pw1[DF_check_pw1$true_positive < DF_check_pw1$Given_answer + 0.01 &
               DF_check_pw1$true_positive > DF_check_pw1$Given_answer - 0.01,])/nrow(DF_check_pw1)

#DF_check_pw2$error <- rep(0, nrow(DF_check_pw2))
#DF_check_pw2[DF_check_pw2$true_positive_avg < DF_check_pw2$Given_answer + 0.01 &
#               DF_check_pw2$true_positive_avg > DF_check_pw2$Given_answer - 0.01,]$error <- 1

nrow(DF_check_pw2[DF_check_pw2$true_positive_avg < DF_check_pw2$Given_answer + 0.01 &
               DF_check_pw2$true_positive_avg > DF_check_pw2$Given_answer - 0.01,])/nrow(DF_check_pw2)



# use the df called scanDF4 for checking the denominator = 1,000 ---------------------
DF_check_f <- scanDF4[scanDF4$numerical_format == "frequency" & 
                        scanDF4$correctness == 0,]

DF_check_fw1 <- DF_check_f[DF_check_f$Wave == 1,]
DF_check_fw2 <- DF_check_f[DF_check_f$Wave == 2,]

nrow(DF_check_fw1[DF_check_fw1$answer2 < 1002 & 
               DF_check_fw1$answer2 > 998,])/nrow(DF_check_fw1)

nrow(DF_check_fw2[DF_check_fw2$answer2 < 1002 & 
                    DF_check_fw2$answer2 > 998,])/nrow(DF_check_fw2)


# some extra descriptive stats 
#Gender
nrow(DF_check[DF_check$gender=="m" & DF_check$Wave==1 & DF_check$numerical_format=="frequency",])
nrow(DF_check[DF_check$gender=="f" & DF_check$Wave==1 & DF_check$numerical_format=="frequency",])

nrow(DF_check[DF_check$gender=="m" & DF_check$Wave==1 & DF_check$numerical_format=="probability",])
nrow(DF_check[DF_check$gender=="f" & DF_check$Wave==1 & DF_check$numerical_format=="probability",])

nrow(DF_check[DF_check$gender=="m" & DF_check$Wave==2 & DF_check$numerical_format=="frequency",])
nrow(DF_check[DF_check$gender=="f" & DF_check$Wave==2 & DF_check$numerical_format=="frequency",])

nrow(DF_check[DF_check$gender=="m" & DF_check$Wave==2 & DF_check$numerical_format=="probability",])
nrow(DF_check[DF_check$gender=="f" & DF_check$Wave==2 & DF_check$numerical_format=="probability",])

#Age
mean(DF_check[DF_check$Wave==1 & DF_check$numerical_format=="frequency",]$age)
sd(DF_check[DF_check$Wave==1 & DF_check$numerical_format=="frequency",]$age)

mean(DF_check[DF_check$Wave==1 & DF_check$numerical_format=="probability",]$age)
sd(DF_check[DF_check$Wave==1 & DF_check$numerical_format=="probability",]$age)

mean(DF_check[DF_check$Wave==2 & DF_check$numerical_format=="frequency",]$age)
sd(DF_check[DF_check$Wave==2 & DF_check$numerical_format=="frequency",]$age)

mean(DF_check[DF_check$Wave==2 & DF_check$numerical_format=="probability",]$age)
sd(DF_check[DF_check$Wave==2 & DF_check$numerical_format=="probability",]$age)

#Numeracy
mean(DF_check[DF_check$Wave==1 & DF_check$numerical_format=="frequency",]$numeracy)
sd(DF_check[DF_check$Wave==1 & DF_check$numerical_format=="frequency",]$numeracy)

mean(DF_check[DF_check$Wave==1 & DF_check$numerical_format=="probability",]$numeracy)
sd(DF_check[DF_check$Wave==1 & DF_check$numerical_format=="probability",]$numeracy)

mean(DF_check[DF_check$Wave==2 & DF_check$numerical_format=="frequency",]$numeracy)
sd(DF_check[DF_check$Wave==2 & DF_check$numerical_format=="frequency",]$numeracy)

mean(DF_check[DF_check$Wave==2 & DF_check$numerical_format=="probability",]$numeracy)
sd(DF_check[DF_check$Wave==2 & DF_check$numerical_format=="probability",]$numeracy)

#PPV
mean(DF_check[DF_check$Wave==1 & DF_check$numerical_format=="frequency",]$Given_answer)
sd(DF_check[DF_check$Wave==1 & DF_check$numerical_format=="frequency",]$Given_answer)

mean(DF_check[DF_check$Wave==1 & DF_check$numerical_format=="probability",]$Given_answer)
sd(DF_check[DF_check$Wave==1 & DF_check$numerical_format=="probability",]$Given_answer)

mean(DF_check[DF_check$Wave==2 & DF_check$numerical_format=="frequency",]$Given_answer)
sd(DF_check[DF_check$Wave==2 & DF_check$numerical_format=="frequency",]$Given_answer)

mean(DF_check[DF_check$Wave==2 & DF_check$numerical_format=="probability",]$Given_answer)
sd(DF_check[DF_check$Wave==2 & DF_check$numerical_format=="probability",]$Given_answer)


#############################################################
# NEw analysis for the thesis ###############################
#############################################################

# paiwise comparison for dwell time 
# wilcox.test(y~A) 
# wilcox.test(y,x)

lista.p1 <- c(wilcox.test(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="T",]$NormalTime ~
              dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="T",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="F",]$NormalTime ~
              dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="F",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="nF",]$NormalTime ~
              dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="nF",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="FA",]$NormalTime ~
              dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="FA",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="FnA",]$NormalTime ~
              dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="FnA",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="nFA",]$NormalTime ~
              dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="nFA",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="nFnA",]$NormalTime ~
              dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="nFnA",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="Q",]$NormalTime ~
              dfTEMP0[dfTEMP0$wave==1 & dfTEMP0$node.id=="Q",]$n_format)$p.value)

lista.p2 <- c(wilcox.test(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="T",]$NormalTime ~
                            dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="T",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="F",]$NormalTime ~
                            dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="F",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="nF",]$NormalTime ~
                            dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="nF",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="FA",]$NormalTime ~
                            dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="FA",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="FnA",]$NormalTime ~
                            dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="FnA",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="nFA",]$NormalTime ~
                            dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="nFA",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="nFnA",]$NormalTime ~
                            dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="nFnA",]$n_format)$p.value,
              wilcox.test(dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="Q",]$NormalTime ~
                            dfTEMP0[dfTEMP0$wave==2 & dfTEMP0$node.id=="Q",]$n_format)$p.value)
