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
#     LOAD THE DATA 
#----------------------------------------------------
# this csv is produced in the middle of the script FIRE_analysis 
# this CSV is used here for the scanpath analysis 

#setwd("C:/Users/manuele/Desktop/PhDremote/paper4/FIRE/data&code")

scanDF2 <- read.csv(file="scanDF2.csv",header=TRUE,sep=",")


# -------------------------------------------------------------------
# SCANPATH ANALYSIS 
# -------------------------------------------------------------------
# separate the DFs
wave1<-scanDF2[scanDF2$Wave==1,]
wave2<-scanDF2[scanDF2$Wave==2,]

# define the alphabets
alphabet_FIRE <- c('A','B','C','D','E','F','G','H')
# load the functions from the file with the algorithms for scanpath analysis 
source("ngram_Analysis.R")

#---------------------------------------------------------------------------------
# FUNCTION:     main2()       (prob vs freq)  
# INPUT:        void
# OUTPUT:       void
# DESCRIPTION:  Main function. 
#               Makes all subsequent function calls.     
#---------------------------------------------------------------------------------

main2 <- function() {
  results_ngram <- data.frame(Experiment = NULL, Ngrams = NULL, AlphaSize = NULL, Mhell = NULL, SDhell = NULL,
                              HellDists = NULL, Pval = NULL)
  results_odds <- data.frame(n_gram = NULL, freqz.x = NULL, probz.x = NULL, Condition.x = NULL, 
                             probzSmooth.x = NULL, freqz.y = NULL, probz.y = NULL, Condition.y = NULL, 
                             probzSmooth.y = NULL, oddsR = NULL, Experiment = NULL, Ngrams = NULL )
  results_odds_total <- data.frame(n_gram = NULL, freqz.x = NULL, probz.x = NULL, Condition.x = NULL, 
                                   probzSmooth.x = NULL, freqz.y = NULL, probz.y = NULL, Condition.y = NULL, 
                                   probzSmooth.y = NULL, oddsR = NULL, Experiment = NULL, Ngrams = NULL ) # NEW
  distributionsDf <- data.frame(Exp = NULL, Ngr = NULL, DistrVec = NULL)
  
  listDF <- list(wave1, wave2)
  numero <- 1 ################################################################ 
  #for (mydata in listDF){ # 
  for (mydata in listDF){  ################################################################
    for (myngram in c(2)){ #},3,4,5)){  ################################################################
      #  for (myngram in c(6)){
      
      numberPermutations <- 10000 #----------------> INPUT number of permutations 
      
      # get the list of ngrams
      if (numero == 1){
        NGRAMlist <- ngramCREA(alphabet_FIRE, myngram, repetitions_elimination = T)} #----------> INPUT alphabet, nngram, repetitions = FALSE (by default)
      else if (numero == 2){
        NGRAMlist <- ngramCREA(alphabet_FIRE, myngram, repetitions_elimination = T)} #----------> INPUT alphabet, nngram, repetitions = FALSE (by default)
      else{stop('Wrong dataframe')}
      
      #get the frequency df
      dfFREQUENCIES1 <- subSeqfreq(NGRAMlist, mydata[mydata$numerical_format=="frequency",], 2) #---> INPUT NGRAMlist, dataframe, columnNumber 
      dfFREQUENCIES1$numerical_format <- rep("frequency", nrow(dfFREQUENCIES1))
      dfFREQUENCIES0 <- subSeqfreq(NGRAMlist, mydata[mydata$numerical_format=="probability",], 2) #---> INPUT NGRAMlist, dataframe, columnNumber 
      dfFREQUENCIES0$numerical_format <- rep("probability", nrow(dfFREQUENCIES1))
      dfFREQUENCIES <- rbind(dfFREQUENCIES0,dfFREQUENCIES1)
      #dfPROVA <<- dfFREQUENCIES ################################################################
      #View(dfPROVA) ################################################################
      
      # RUN THE PERMUTATION TEST 
      # create the comparison vectors
      condition1Vector <- as.numeric(as.vector(dfFREQUENCIES[dfFREQUENCIES$numerical_format=="frequency",]$probz)) #-------------> INPUT THE DF VECTOR FOR CONDITION 1
      condition0Vector <- as.numeric(as.vector(dfFREQUENCIES[dfFREQUENCIES$numerical_format=="probability",]$probz)) #-------------> INPUT THE DF VECTOR FOR CONDITION 0
      # Find the distance between condition1 and condition2
      distCOND <- vecHellDistance(condition1Vector, condition0Vector)
      Hellylabel <- "Hellinger distance"
      # Run permutation test 
      freqNGRAMDISTANCE <- permutationTestVector(mydata, #----> INPUT DF
                                                 NGRAMlist, #-------------> INPUT LISTngrams
                                                 nrow(subset(mydata, numerical_format=="frequency")), #---> INPUT group1_size
                                                 vecHellDistance, #-----> INPUT distanceMeasure
                                                 2, #-------> INPUT ncolumDATA
                                                 numberPermutations)
      
      # generate dot plot
      freqNGRAMDISTANCE <- append(freqNGRAMDISTANCE, distCOND)
      meanHell <- mean(freqNGRAMDISTANCE)
      sdHell <- sd(freqNGRAMDISTANCE)
      
      #plot(freqNGRAMDISTANCE, col = ifelse(freqNGRAMDISTANCE == distCOND, 'blue', 'red'), 
      #main = paste0("n-gram analysis ", " (",Hellylabel,")"), xlab = "Number of permutations", ylab = Hellylabel)
      #abline(h = distCOND, col = "purple")
      # generate density plot
      #Helldensity_plot <- density(freqNGRAMDISTANCE)
      #plot(Helldensity_plot, type = "n", main = paste0("n-gram analysis ", " (",Hellylabel,")"), xlab = "Hellinger distance")
      #polygon(Helldensity_plot, col = "lightgray", border = "grey")
      #rug(freqNGRAMDISTANCE, col = ifelse(freqNGRAMDISTANCE == distCOND, 'blue', 'red'))
      #abline(v = distCOND, col = "purple")
      Hellpvalue <- calculatePvalue(distCOND, freqNGRAMDISTANCE)
      #HellResults <- list("The Hellinger distance is:", distCOND, "The p-value is:", Hellpvalue)
      #print(HellResults)
      
      if (numero == 1){
        newRow <- data.frame(Experiment = "Study1", Ngrams = myngram, AlphaSize = length(alphabet_FIRE), 
                             Mhell = meanHell, SDhell = sdHell,
                             HellDists = distCOND, Pval = Hellpvalue)}
      else if (numero == 2){
        newRow <- data.frame(Experiment = "Study2", Ngrams = myngram, AlphaSize = length(alphabet_FIRE), 
                             Mhell = meanHell, SDhell = sdHell,
                             HellDists = distCOND, Pval = Hellpvalue)}
      
      else{stop('Wrong dataframe')}
      results_ngram <- rbind(results_ngram,newRow)
      
      # get the ngrams with the largest and lowest odds-ratio
      # apply "add-one" smoothing (Laplace smoothing) to the frequencies before transforming into probabilities
      dfFREQUENCIES1$probzSmooth <- (dfFREQUENCIES1$freqz+1)/ (sum(dfFREQUENCIES1$freqz)+nrow(dfFREQUENCIES1)) 
      dfFREQUENCIES0$probzSmooth <- (dfFREQUENCIES0$freqz+1)/ (sum(dfFREQUENCIES0$freqz)+nrow(dfFREQUENCIES0))
      # merge the dataframes 
      dfODDsR <- merge(dfFREQUENCIES1,dfFREQUENCIES0, by="n_gram") 
      # calculate odds scale, i.e. (p/(1-p)) / (q/(1-q)). 
      dfODDsR$oddsR <- (dfODDsR$probzSmooth.x/(1-dfODDsR$probzSmooth.x)) / 
        (dfODDsR$probzSmooth.y/(1-dfODDsR$probzSmooth.y))
      # get the top 2
      top2odds <- dfODDsR[dfODDsR$oddsR %in% tail(sort(dfODDsR$oddsR),2),]
      top2odds <- top2odds[order(-top2odds$oddsR),]
      # get the bottom 2
      bottom2odds <- dfODDsR[dfODDsR$oddsR %in% head(sort(dfODDsR$oddsR),2),] 
      bottom2odds <- bottom2odds[order(-bottom2odds$oddsR),]
      # combine the df for ODDsRatios
      oddRatios <- rbind(top2odds, bottom2odds)
      oddRatios_total <- dfODDsR # NEW
      if (numero == 1){
        oddRatios$Experiment <- rep("Study1", nrow(oddRatios))
        oddRatios_total $Experiment <- rep("Study1", nrow(oddRatios_total))} # NEW
      else if (numero == 2){
        oddRatios$Experiment <- rep("Study2", nrow(oddRatios))
        oddRatios_total$Experiment <- rep("Study2", nrow(oddRatios_total))} # NEW
      else{stop('Wrong dataframe')}
      oddRatios$Ngrams <- rep(myngram, nrow(oddRatios))
      oddRatios_total$Ngrams <- rep(myngram, nrow(oddRatios_total)) # NEW
      results_odds <- rbind(results_odds,oddRatios)
      results_odds_total <- rbind(results_odds_total,oddRatios_total) # NEW
      
      # export the distribution 
      
      if (numero == 1){
        newDistr <- data.frame(Exp = "Study1", Ngr = myngram, DistrVec = freqNGRAMDISTANCE)}
      else if (numero == 2){
        newDistr <- data.frame(Exp = "Study2", Ngr = myngram, DistrVec = freqNGRAMDISTANCE)}
      else{stop('Wrong dataframe')}
      
      distributionsDf <- rbind(distributionsDf,newDistr)
    }
    #View(results_ngram)
    #View(results_odds)
    #stop()
    numero <- numero + 1
  }
  
  write.csv(results_ngram, file = "TESThellData.csv", row.names=F) ################################################################
  write.csv(results_odds, file = "TESToddData.csv", row.names=F) ################################################################
  write.csv(distributionsDf, file = "TESTdistriData.csv", row.names=F) ################################################################
  
  write.csv(results_odds_total, file = "TESToddData_total.csv", row.names=F) # NEW
  #risultati[["ngramsDF"]] <- results_ngram
  #risultati[["oddsDF"]] <- results_odds
  #return(risultati)
} 

# run main2
main2()

# get the frequencies and counts 
#nrow(dfPROVA[dfPROVA$Condition==0,])
#nrow(dfPROVA[dfPROVA$Condition==1,])
#56
#sum(dfPROVA[dfPROVA$Condition==1,]$freqz)
#232
#sum(dfPROVA[dfPROVA$Condition==0,]$freqz)
#455

#---------------------------------------------------------------------------------
# FUNCTION:     main3()
# INPUT:        void
# OUTPUT:       void
# DESCRIPTION:  Main function. 
#               Makes all subsequent function calls.     
#---------------------------------------------------------------------------------

main3 <- function() {
  DFfreqz<- data.frame(Exp = NULL, n_gram = NULL, Freqz = NULL)
  listDF <- list(wave1, wave2)
  numero <- 1 ################################################################ 
  for (mydata in listDF){  ################################################################
    for (myngram in c(2)){ #},3,4,5)){  ################################################################
      # get the list of ngrams
      if (numero == 1){
        NGRAMlist <- ngramCREA(alphabet_FIRE, myngram,repetitions_elimination = T)} #----------> INPUT alphabet, nngram, repetitions = FALSE (by default)
      else if (numero == 2){
        NGRAMlist <- ngramCREA(alphabet_FIRE, myngram,repetitions_elimination = T)} #----------> INPUT alphabet, nngram, repetitions = FALSE (by default)
      else{stop('Wrong dataframe')}
      
      #get the frequency df
      dfFREQUENCIES1 <- subSeqfreq(NGRAMlist, mydata[mydata$numerical_format=="frequency",], 2) #---> INPUT NGRAMlist, dataframe, columnNumber 
      dfFREQUENCIES1$numerical_format <- rep("frequency", nrow(dfFREQUENCIES1))
      dfFREQUENCIES0 <- subSeqfreq(NGRAMlist, mydata[mydata$numerical_format=="probability",], 2) #---> INPUT NGRAMlist, dataframe, columnNumber 
      dfFREQUENCIES0$numerical_format <- rep("probability", nrow(dfFREQUENCIES0))
      dfFREQUENCIES <- rbind(dfFREQUENCIES0,dfFREQUENCIES1)
      
      if (numero == 1){
        newRow<- data.frame(Exp = rep("Study1", nrow(dfFREQUENCIES)), 
                            n_gram = rep(myngram, nrow(dfFREQUENCIES)), 
                            Freqz = dfFREQUENCIES$freqz)}
      else if (numero == 2){
        newRow<- data.frame(Exp = rep("Study2", nrow(dfFREQUENCIES)), 
                            n_gram = rep(myngram, nrow(dfFREQUENCIES)), 
                            Freqz = dfFREQUENCIES$freqz)}
      else{stop('Wrong dataframe')}
      
      DFfreqz <- rbind(DFfreqz,newRow)
    }
    numero <- numero+1
  }
  return(DFfreqz)
}

# run main 3
dfPROVARE <- main3()
write.csv(dfPROVARE, file = "frequenzeDF.csv", row.names=F)



# prepare the DFs for Wave 1 only, freq and prob
freqDF_w1<-scanDF2[scanDF2$numerical_format == "frequency",]
probDF_w1<-scanDF2[scanDF2$numerical_format == "probability",]
#---------------------------------------------------------------------------------
# FUNCTION:     main4()       (correct vs incorrect)  
# INPUT:        void
# OUTPUT:       void
# DESCRIPTION:  Main function. 
#               Makes all subsequent function calls.   
#---------------------------------------------------------------------------------

main4 <- function() {
  results_ngram <- data.frame(Experiment = NULL, Ngrams = NULL, AlphaSize = NULL, Mhell = NULL, SDhell = NULL,
                              HellDists = NULL, Pval = NULL)
  results_odds <- data.frame(n_gram = NULL, freqz.x = NULL, probz.x = NULL, Condition.x = NULL, 
                             probzSmooth.x = NULL, freqz.y = NULL, probz.y = NULL, Condition.y = NULL, 
                             probzSmooth.y = NULL, oddsR = NULL, Experiment = NULL, Ngrams = NULL )
  results_odds_total <- data.frame(n_gram = NULL, freqz.x = NULL, probz.x = NULL, Condition.x = NULL, 
                                   probzSmooth.x = NULL, freqz.y = NULL, probz.y = NULL, Condition.y = NULL, 
                                   probzSmooth.y = NULL, oddsR = NULL, Experiment = NULL, Ngrams = NULL ) # NEW
  distributionsDf <- data.frame(Exp = NULL, Ngr = NULL, DistrVec = NULL)
  
  listDF <- list(freqDF_w1, probDF_w1)
  numero <- 1 ################################################################ 
  #for (mydata in listDF){ # 
  for (mydata in listDF){  ################################################################
    for (myngram in c(2)){ #},3,4,5)){  ################################################################
      #  for (myngram in c(6)){
      
      numberPermutations <- 10000 #----------------> INPUT number of permutations 
      
      # get the list of ngrams
      if (numero == 1){
        NGRAMlist <- ngramCREA(alphabet_FIRE, myngram, repetitions_elimination = T)} #----------> INPUT alphabet, nngram, repetitions = FALSE (by default)
      else if (numero == 2){
        NGRAMlist <- ngramCREA(alphabet_FIRE, myngram, repetitions_elimination = T)} #----------> INPUT alphabet, nngram, repetitions = FALSE (by default)
      else{stop('Wrong dataframe')}
      
      #get the frequency df
      dfFREQUENCIES1 <- subSeqfreq(NGRAMlist, mydata[mydata$correctness=="incorrect",], 2) #---> INPUT NGRAMlist, dataframe, columnNumber 
      dfFREQUENCIES1$correctness <- rep("incorrect", nrow(dfFREQUENCIES1))
      dfFREQUENCIES0 <- subSeqfreq(NGRAMlist, mydata[mydata$correctness=="correct",], 2) #---> INPUT NGRAMlist, dataframe, columnNumber 
      dfFREQUENCIES0$correctness <- rep("correct", nrow(dfFREQUENCIES1))
      dfFREQUENCIES <- rbind(dfFREQUENCIES0,dfFREQUENCIES1)
      #dfPROVA <<- dfFREQUENCIES ################################################################
      #View(dfPROVA) ################################################################
      
      # RUN THE PERMUTATION TEST 
      # create the comparison vectors
      condition1Vector <- as.numeric(as.vector(dfFREQUENCIES[dfFREQUENCIES$correctness=="incorrect",]$probz)) #-------------> INPUT THE DF VECTOR FOR CONDITION 1
      condition0Vector <- as.numeric(as.vector(dfFREQUENCIES[dfFREQUENCIES$correctness=="correct",]$probz)) #-------------> INPUT THE DF VECTOR FOR CONDITION 0
      # Find the distance between condition1 and condition2
      distCOND <- vecHellDistance(condition1Vector, condition0Vector)
      Hellylabel <- "Hellinger distance"
      # Run permutation test 
      freqNGRAMDISTANCE <- permutationTestVector(mydata, #----> INPUT DF
                                                 NGRAMlist, #-------------> INPUT LISTngrams
                                                 nrow(subset(mydata, correctness=="incorrect")), #---> INPUT group1_size
                                                 vecHellDistance, #-----> INPUT distanceMeasure
                                                 2, #-------> INPUT ncolumDATA
                                                 numberPermutations)
      
      # generate dot plot
      freqNGRAMDISTANCE <- append(freqNGRAMDISTANCE, distCOND)
      meanHell <- mean(freqNGRAMDISTANCE)
      sdHell <- sd(freqNGRAMDISTANCE)
      
      #plot(freqNGRAMDISTANCE, col = ifelse(freqNGRAMDISTANCE == distCOND, 'blue', 'red'), 
      #main = paste0("n-gram analysis ", " (",Hellylabel,")"), xlab = "Number of permutations", ylab = Hellylabel)
      #abline(h = distCOND, col = "purple")
      # generate density plot
      #Helldensity_plot <- density(freqNGRAMDISTANCE)
      #plot(Helldensity_plot, type = "n", main = paste0("n-gram analysis ", " (",Hellylabel,")"), xlab = "Hellinger distance")
      #polygon(Helldensity_plot, col = "lightgray", border = "grey")
      #rug(freqNGRAMDISTANCE, col = ifelse(freqNGRAMDISTANCE == distCOND, 'blue', 'red'))
      #abline(v = distCOND, col = "purple")
      Hellpvalue <- calculatePvalue(distCOND, freqNGRAMDISTANCE)
      #HellResults <- list("The Hellinger distance is:", distCOND, "The p-value is:", Hellpvalue)
      #print(HellResults)
      
      if (numero == 1){
        newRow <- data.frame(numerical_format = "frequency", Ngrams = myngram, AlphaSize = length(alphabet_FIRE), 
                             Mhell = meanHell, SDhell = sdHell,
                             HellDists = distCOND, Pval = Hellpvalue)}
      else if (numero == 2){
        newRow <- data.frame(numerical_format = "probability", Ngrams = myngram, AlphaSize = length(alphabet_FIRE), 
                             Mhell = meanHell, SDhell = sdHell,
                             HellDists = distCOND, Pval = Hellpvalue)}
      
      else{stop('Wrong dataframe')}
      results_ngram <- rbind(results_ngram,newRow)
      
      # get the ngrams with the largest and lowest odds-ratio
      # apply "add-one" smoothing (Laplace smoothing) to the frequencies before transforming into probabilities
      dfFREQUENCIES1$probzSmooth <- (dfFREQUENCIES1$freqz+1)/ (sum(dfFREQUENCIES1$freqz)+nrow(dfFREQUENCIES1)) 
      dfFREQUENCIES0$probzSmooth <- (dfFREQUENCIES0$freqz+1)/ (sum(dfFREQUENCIES0$freqz)+nrow(dfFREQUENCIES0))
      # merge the dataframes 
      dfODDsR <- merge(dfFREQUENCIES1,dfFREQUENCIES0, by="n_gram") 
      # calculate odds scale, i.e. (p/(1-p)) / (q/(1-q)). 
      dfODDsR$oddsR <- (dfODDsR$probzSmooth.x/(1-dfODDsR$probzSmooth.x)) / 
        (dfODDsR$probzSmooth.y/(1-dfODDsR$probzSmooth.y))
      # get the top 2
      top2odds <- dfODDsR[dfODDsR$oddsR %in% tail(sort(dfODDsR$oddsR),2),]
      top2odds <- top2odds[order(-top2odds$oddsR),]
      # get the bottom 2
      bottom2odds <- dfODDsR[dfODDsR$oddsR %in% head(sort(dfODDsR$oddsR),2),] 
      bottom2odds <- bottom2odds[order(-bottom2odds$oddsR),]
      # combine the df for ODDsRatios
      oddRatios <- rbind(top2odds, bottom2odds)
      oddRatios_total <- dfODDsR # NEW
      if (numero == 1){
        oddRatios$numerical_format <- rep("frequency", nrow(oddRatios))
        oddRatios_total $numerical_format <- rep("frequency", nrow(oddRatios_total))} # NEW
      else if (numero == 2){
        oddRatios$numerical_format <- rep("probability", nrow(oddRatios))
        oddRatios_total$numerical_format <- rep("probability", nrow(oddRatios_total))} # NEW
      else{stop('Wrong dataframe')}
      oddRatios$Ngrams <- rep(myngram, nrow(oddRatios))
      oddRatios_total$Ngrams <- rep(myngram, nrow(oddRatios_total)) # NEW
      results_odds <- rbind(results_odds,oddRatios)
      results_odds_total <- rbind(results_odds_total,oddRatios_total) # NEW
      
      # export the distribution 
      
      if (numero == 1){
        newDistr <- data.frame(Exp = "frequency", Ngr = myngram, DistrVec = freqNGRAMDISTANCE)}
      else if (numero == 2){
        newDistr <- data.frame(Exp = "probability", Ngr = myngram, DistrVec = freqNGRAMDISTANCE)}
      else{stop('Wrong dataframe')}
      
      distributionsDf <- rbind(distributionsDf,newDistr)
    }
    #View(results_ngram)
    #View(results_odds)
    #stop()
    numero <- numero + 1
  }
  
  write.csv(results_ngram, file = "TESThellData_corIncor.csv", row.names=F) ################################################################
  write.csv(results_odds, file = "TESToddData_corIncor.csv", row.names=F) ################################################################
  write.csv(distributionsDf, file = "TESTdistriData_corIncor.csv", row.names=F) ################################################################
  
  write.csv(results_odds_total, file = "TESToddData_total_corIncor.csv", row.names=F) # NEW
  #risultati[["ngramsDF"]] <- results_ngram
  #risultati[["oddsDF"]] <- results_odds
  #return(risultati)
} 

# run main4
main4()


#---------------------------------------------------------------------------------
# FUNCTION:     main5()
# INPUT:        void
# OUTPUT:       void
# DESCRIPTION:  Main function. 
#               Makes all subsequent function calls.     
#---------------------------------------------------------------------------------

main5 <- function() {
  DFfreqz<- data.frame(Exp = NULL, n_gram = NULL, Freqz = NULL)
  listDF <- list(freqDF_w1, probDF_w1)
  numero <- 1 ################################################################ 
  for (mydata in listDF){  ################################################################
    for (myngram in c(2)){ #},3,4,5)){  ################################################################
      # get the list of ngrams
      if (numero == 1){
        NGRAMlist <- ngramCREA(alphabet_FIRE, myngram, repetitions_elimination = T)} #----------> INPUT alphabet, nngram, repetitions = FALSE (by default)
      else if (numero == 2){
        NGRAMlist <- ngramCREA(alphabet_FIRE, myngram, repetitions_elimination = T)} #----------> INPUT alphabet, nngram, repetitions = FALSE (by default)
      else{stop('Wrong dataframe')}
      
      #get the frequency df
      dfFREQUENCIES1 <- subSeqfreq(NGRAMlist, mydata[mydata$correctness=="incorrect",], 2) #---> INPUT NGRAMlist, dataframe, columnNumber 
      dfFREQUENCIES1$correctness <- rep("incorrect", nrow(dfFREQUENCIES1))
      dfFREQUENCIES0 <- subSeqfreq(NGRAMlist, mydata[mydata$correctness=="correct",], 2) #---> INPUT NGRAMlist, dataframe, columnNumber 
      dfFREQUENCIES0$correctness <- rep("correct", nrow(dfFREQUENCIES0))
      dfFREQUENCIES <- rbind(dfFREQUENCIES0,dfFREQUENCIES1)
      
      if (numero == 1){
        newRow<- data.frame(Exp = rep("frequency", nrow(dfFREQUENCIES)), 
                            n_gram = rep(myngram, nrow(dfFREQUENCIES)), 
                            Freqz = dfFREQUENCIES$freqz)}
      else if (numero == 2){
        newRow<- data.frame(Exp = rep("probability", nrow(dfFREQUENCIES)), 
                            n_gram = rep(myngram, nrow(dfFREQUENCIES)), 
                            Freqz = dfFREQUENCIES$freqz)}
      else{stop('Wrong dataframe')}
      
      DFfreqz <- rbind(DFfreqz,newRow)
    }
    numero <- numero+1
  }
  return(DFfreqz)
}

# run main 5
dfPROVARE <- main5()
write.csv(dfPROVARE, file = "frequenzeDF_corIncor.csv", row.names=F)