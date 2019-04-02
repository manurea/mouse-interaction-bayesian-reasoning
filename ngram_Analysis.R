#-----------------------------------------------------------------------------------
# Author:       Manuele Reani
# Date:         16/07/2018
# Institution:  The university of Manchester - School of Computer Science
# Object:       This file contains the functions for perfoming 
#               the sequence/scanpaths analysis (n-gram analysis), for different ngram lengths 
#-----------------------------------------------------------------------------------

# n-gram analysis 
#---------------------------------------------------------------------------------
# FUNCTION:     ngramCREA()
# INPUT:        alphabet = a vector of letters 
#               e.g. c("B", "C", "D", "E", "F", "G", "H", "I")
#               nngram = number of ngrams (e.g. 2 for 2-gram)
#               repettitions = false if you do not want consecutive repetitions, true otherwise 
# OUTPUT:       List of all possible n-gram combinations
# DESCRIPTION:  This is used to create a vector of possible n-grams to be used 
#               later for counting the n-grams frequency
#---------------------------------------------------------------------------------

ngramCREA <- function(alphabet, nngram, repetitions_elimination = TRUE){
  # Find all the ngram permutations with repetitions 
  permutazioni <- permutations(n = length(alphabet), r = nngram, v = alphabet, repeats.allowed = TRUE)
  
  listaNGRAMs <- apply(permutazioni,1,paste0,collapse="")
  if(repetitions_elimination){
    # remove subsequent repeated characters in the string
    norepNGRAMS <- gsub('([[:alpha:]])\\1+', '\\1', listaNGRAMs)
  }
  else{norepNGRAMS <- listaNGRAMs
  }
  REDUCEDngram <- norepNGRAMS[nchar(norepNGRAMS) == nngram]
  
  return(REDUCEDngram)
}

# examples:
#bigramexample <- ngramCREA(c("B", "C", "D", "E", "F", "G", "H", "I"),2)
#print(bigramexample)
#trigramexample <- ngramCREA(c("B", "C", "D", "E", "F", "G", "H", "I"),3)
#print(trigramexample)
#fourgramexample <- ngramCREA(c("B", "C", "D", "E", "F", "G", "H", "I"),4)
#print(fourgramexample)
# examples with repetitions
#bigramexampleT <- ngramCREA(c("B", "C", "D", "E", "F", "G", "H", "I"),2, repetitions = TRUE) #here repetitions means subsequent
#print(bigramexampleT)
#trigramexampleT <- ngramCREA(c("B", "C", "D", "E", "F", "G", "H", "I"),3, repetitions = TRUE)
#print(trigramexampleT)
#fourgramexampleT <- ngramCREA(c("B", "C", "D", "E", "F", "G", "H", "I"),4, repetitions = TRUE)
#print(fourgramexampleT)

#"BBC" %in% trigramexample
#"BCBC" %in% fourgramexample

# example with repetitions included 
#print(ngramCREA(c("B", "C", "D"),2,F))

#---------------------------------------------------------------------------------
# FUNCTION:     subSeqfreq()
# INPUT:        ngrams = list of possible n-grams - e.g. c("ab", "bc", ...)
#               dataframe = the dataframe with all the scanpaths for a specific group
#               (e.g. uncorrectGroupTree)
#               columnNumber = the column number where the scanpaths are found (e.g. 11)
# OUTPUT:       A wide DF with the list of n-gram and their frequency and probabilities
# DESCRIPTION:  This function produces a datframe where to store, for each group 
#                 (correct and incorrect), the n-gram and their respective frequencies/prob.
# Comments: Here I use the function "stri_count_regex" from the library "stringi".
# The input DFs need to be subsetted before running this fucntion and the output DFs 
# can then be combined later for further analysis.
#---------------------------------------------------------------------------------
subSeqfreq <- function(ngrams, dataframe, columnNumber){
  number_ngram <- length(ngrams)
  number_observ <- nrow(dataframe)
  ngram_frquencylist <- list() 
  freq <- 0
  ngramfreqs <- vector(mode="integer", length=number_ngram)
  for (i in 1:number_ngram){
    scount <-  str_count(dataframe[,columnNumber], ngrams[i])
    freq <- sum(scount)
    ngramfreqs[i] <- freq
    freq <- 0
  }
  ngram_frquencyDF <- data.frame(n_gram=ngrams, freqz=ngramfreqs)
  # calculate probabilities
  ngram_frquencyDF$probz <- ngram_frquencyDF$freqz/sum(ngram_frquencyDF$freqz)
  return(ngram_frquencyDF) 
}

# example:
#freqSubsq_tree_correct <- subSeqfreq(fourgramexample, uncorrectGroupTree, 11)


#---------------------------------------------------------------------------------
# FUNCTION:     permutationTestVector()
#
# INPUT parameters: 
# DF <- A dataframe with two groups (correct Vs incorrect) 
#       e.g., subset(NEWdfANALYSIS, Format == "tree")
# LISTngrams <- A list with all the possible ngrams
#               e.g., REDUCEDtotalBiGrams__tree
# group1_size <- A number, representing the size of a group of interest
#               e.g., nrow(subset(NEWdfANALYSIS, Correct==1 & Format == "tree"))
# distanceMeasure <- a distance measure to compare 2 discrete distribution of ngrams 
#                   e.g.,  vecHellDistance
# ncolumDATA <- the column where the scanpaths are, the numbe rof column in the DF(e.g., 11)
# perms <- the number of permutations
#          e.g., numberPermutations (10000)
#
# OUTPUT: A vector of distances 
#---------------------------------------------------------------------------------

permutationTestVector <- function(DF, LISTngrams, group1_size, distanceMeasure, ncolumDATA, perms = 10){
  distance <- NULL
  for (i in 1:perms){
    GROUP1<- NULL
    GROUP2<- NULL
    freqGROUP1<- NULL
    freqGROUP2<- NULL
    distance_results<-0
    
    # create two DF groups sampling at random 
    GROUP1 <- DF[sample(1:nrow(DF), group1_size, replace=FALSE),]
    GROUP2 <- DF[ !(DF$Ps %in% GROUP1$Ps), ]
    # create a frequency dataframe (frequency of ngrams)
    freqGROUP1 <- subSeqfreq(LISTngrams, GROUP1, ncolumDATA)
    freqGROUP2 <- subSeqfreq(LISTngrams, GROUP2, ncolumDATA)
    # define the distributions to be compared
    vettore1 <- as.numeric(as.vector(freqGROUP1$probz))
    vettore2 <- as.numeric(as.vector(freqGROUP2$probz))
    # caluclate distance between the two distributions
    distance_results <- distanceMeasure(vettore1 , vettore2)
    # add distance to fdistance vector 
    distance <- c(distance, distance_results)
  }
  return(distance) 
}

#---------------------------------------------------------------------------------
# FUNCTION:     vecHellDistance()
# INPUT:        two vectors (e.g., vec1, vec2)
# OUTPUT:       Summed Hellinger Distance
# DESCRIPTION:  Hellinger Distance is defined as: 
#               1/sqrt(2) * sqrt(sum(suqare(sqrt(pi - squrt(qi))))
#---------------------------------------------------------------------------------
vecHellDistance <- function(v1,v2){
  length_of_vector <- length(v1)
  HJ <- 0
  for (i in 1:length_of_vector){
    HJ <- HJ + ((sqrt(v1[i]) - sqrt(v2[i]))^2)
  }
  return((1/sqrt(2))*sqrt(HJ)) 
}

#---------------------------------------------------------------------------------
# FUNCTION:     calculatePvalue()
# INPUT:        a value and a vector of random values (e.g., value, vector_value)
# OUTPUT:       p-value
# DESCRIPTION:  Calculate p-value (% of values > correct/incorrect value)
#---------------------------------------------------------------------------------
calculatePvalue <- function(correct_and_incorrect, shuffled_distances)
{
  gtr <- length(shuffled_distances[shuffled_distances > correct_and_incorrect])
  pvalue <- gtr / length(shuffled_distances) 
  return(pvalue)
}



#############################################


