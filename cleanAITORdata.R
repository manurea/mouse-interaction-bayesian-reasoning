set.seed(171219)
#-----------------------------------------------------------------------------------
# Author:       Manuele Reani
# Date:         03/01/2018
# Institution:  HKUST
# Object:       Analysis of study 4 data (fire website)
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


# -----------------------------------------------------------------------
# Load the data: final_aitor_df.csv, select data, and rename the variable
# -----------------------------------------------------------------------

setwd("C:/Users/manuele/Desktop/PhDremote/paper4/FIRE/data&code") 

AITORdf <- read.csv(file="final_aitor_df.csv",header=TRUE,sep=",")

# keep only two events (mouseout and mouseover)
AITORdf<- AITORdf[AITORdf$event == 'mouseout' | 
                    AITORdf$event == 'mouseover', ]

# rename the node.id for background 
AITORdf$node.id[AITORdf$node.id %in% c('','ans1', 'ans2', 'line1', 'line2', 'line3',
                                       'line4', 'line5', 'line6', 'page-error',
                                       'pageJumboHeader','tree-submit-button')] <- 'tree-container'
# rename the node.id for inner text 
AITORdf$node.id[AITORdf$node.id == 'question-node-inner'] <- "tree-question-node" 
AITORdf$node.id[AITORdf$node.id =="study-node-1"] <- "study-button-node1"  
AITORdf$node.id[AITORdf$node.id =="study-node-2"] <- "study-button-node2"  
AITORdf$node.id[AITORdf$node.id =="study-node-3"] <- "study-button-node3"  
AITORdf$node.id[AITORdf$node.id =="study-node-4"] <- "study-button-node4"
AITORdf$node.id[AITORdf$node.id =="study-node-5"] <- "study-button-node5"    
AITORdf$node.id[AITORdf$node.id =="study-node-6"] <- "study-button-node6"
AITORdf$node.id[AITORdf$node.id =="study-node-7"] <- "study-button-node7"

# change the name of the button to ABCDEFG(H) 
AITORdf$node.id <- revalue(AITORdf$node.id, c("study-button-node1"="A", 
                                              "study-button-node2"="B", 
                                              "study-button-node3" = "C", 
                                              "study-button-node4"= "D", 
                                              "study-button-node5" = "E", 
                                              "study-button-node6"="F", 
                                              "study-button-node7" = "G",
                                              "tree-question-node" = "H")) 

# -----------------------------------------------------------------------
# Data Cleaning 
# -----------------------------------------------------------------------

# crea the error column
AITORdf$Errore <- rep(0,nrow(AITORdf))

# Some Ps too the task twice or even more. We need to eliminate the subsequent trails of
# these Ps and only take the first trial 
# identifica se passano piu di 5 munuti da un evento all'altro
dfTEMP0 <- data.frame(event = NULL, 
                     timestampms = NULL, 
                     sid = NULL, 
                     node.id = NULL,
                     n_format = NULL,
                     wave = NULL,
                     Errore = NULL)

for (i in unique(AITORdf$sid)){ 
  # this is my new dataframe subsetted AITORdf[AITORdf$sid == i,]
  df <- AITORdf[AITORdf$sid == i,]
  # order the datafrae by timestamp
  df <- df[order(df$timestampms),]
  for (riga in c(1:nrow(df))){
    if (riga < nrow(df)){
      if (df[riga+1,2] - df[riga,2] > 60000){ # longer than 1 min, between events 
        df[riga, 7] <- 1
      }
    }
  }
  dfTEMP0 <- rbind(dfTEMP0,df)
}
 
# we need to delete those cheaters who participates more than once     
sum(dfTEMP0$Errore)
# lista of Ps cheater 
x_ps <- unique(dfTEMP0[dfTEMP0$Errore == 1, ]$sid)
# DF cheaters
DF_cheaters <- dfTEMP0[dfTEMP0$sid %in% x_ps,]
 # DF non-cheaters
dfTEMP0 <- dfTEMP0[!dfTEMP0$sid %in% x_ps,]

# then we need to include them only for the trial we have Alan data 
DF_cheaters1 <- DF_cheaters[DF_cheaters$sid == '0d393ce2ce07d5e5' &
                            DF_cheaters$n_format == 'prob'&
                            DF_cheaters$wave== 1,] 
DF_cheaters2 <- DF_cheaters[DF_cheaters$sid == '40fd6c5de6f179e5' &
                            DF_cheaters$n_format == 'freq'&
                            DF_cheaters$wave==1,] 
DF_cheaters3 <- DF_cheaters[DF_cheaters$sid == '70c24dc7f5747722' &
                              DF_cheaters$n_format == 'freq'&
                              DF_cheaters$wave== 1,] 

DF_cheaters4 <- DF_cheaters[DF_cheaters$sid == 'a413dd82051797d1' &
                              DF_cheaters$n_format == 'prob'&
                              DF_cheaters$wave==2,] 
DF_cheaters5 <- DF_cheaters[DF_cheaters$sid == '82a18bc8ee16b9e6' &
                              DF_cheaters$n_format == 'prob'&
                              DF_cheaters$wave== 2,] 
DF_cheaters6 <- DF_cheaters[DF_cheaters$sid == '9d532e664ba5a144' &
                              DF_cheaters$n_format == 'prob'&
                              DF_cheaters$wave==2,] 
DF_cheaters7 <- DF_cheaters[DF_cheaters$sid == 'd0409bdf7ef1b911' &
                              DF_cheaters$n_format == 'prob'&
                              DF_cheaters$wave== 2,] 
DF_cheaters8 <- DF_cheaters[DF_cheaters$sid == '1aca18c8b23a7d30' &
                              DF_cheaters$n_format == 'prob'&
                              DF_cheaters$wave==2,]

DF_cheaters9 <- DF_cheaters[DF_cheaters$sid == '549e3e2c81b4fb56' &
                              DF_cheaters$n_format == 'freq'&
                              DF_cheaters$wave== 2,] 
DF_cheaters10 <- DF_cheaters[DF_cheaters$sid == '1cdd768edf6a100b' &
                              DF_cheaters$n_format == 'freq'&
                              DF_cheaters$wave==2,] 
DF_cheaters11 <- DF_cheaters[DF_cheaters$sid == 'c131c5d4fa86fd74' &
                              DF_cheaters$n_format == 'freq'&
                              DF_cheaters$wave== 2,] 
DF_cheaters12 <- DF_cheaters[DF_cheaters$sid == 'a48587489a0ebe15' &
                              DF_cheaters$n_format == 'freq'&
                              DF_cheaters$wave==2,] 
DF_cheaters13 <- DF_cheaters[DF_cheaters$sid == 'a179a5457d9f1c36' &
                              DF_cheaters$n_format == 'freq'&
                              DF_cheaters$wave== 2,] 
DF_cheaters14 <- DF_cheaters[DF_cheaters$sid == '9ec91bcdd7b6278b' &
                              DF_cheaters$n_format == 'freq'&
                              DF_cheaters$wave==2,] 
DF_cheaters15 <- DF_cheaters[DF_cheaters$sid == 'bb9a98990004465c' &
                              DF_cheaters$n_format == 'freq'&
                              DF_cheaters$wave== 2,] 
DF_cheaters16 <- DF_cheaters[DF_cheaters$sid == '57eb2d4005419a0f' &
                               DF_cheaters$n_format == 'freq'&
                               DF_cheaters$wave== 2,]


dfTEMP0 <- do.call("rbind", 
                      list(dfTEMP0, DF_cheaters1, DF_cheaters2,DF_cheaters3,
                           DF_cheaters4,DF_cheaters5,DF_cheaters6,DF_cheaters7,
                           DF_cheaters8,DF_cheaters9,DF_cheaters10,
                           DF_cheaters11,DF_cheaters12,DF_cheaters13,DF_cheaters14,
                           DF_cheaters15,DF_cheaters16))

# replace the error column
dfTEMP0$Errore <- rep(0,nrow(dfTEMP0))



######## Further chacking for cheaters ###################
# ci sono ancora alcuni che hanno tempi lunghi, probabilmente si tratta di 
# gente che lo ha rifatto poco dopo. Voglio solo la prima trial 
dfTEMPmille <- data.frame(event = NULL, 
                      timestampms = NULL, 
                      sid = NULL, 
                      node.id = NULL,
                      n_format = NULL,
                      wave = NULL,
                      Errore = NULL)

for (i in unique(dfTEMP0$sid)){ 
  # this is my new dataframe subsetted dfTEMP0[dfTEMP0$sid == i,]
  df <- dfTEMP0[dfTEMP0$sid == i,]
  # order the datafrae by timestamp
  df <- df[order(df$timestampms),]
  for (riga in c(1:nrow(df))){
    if (riga < nrow(df)){
      if (df[riga+1,2] - df[riga,2] > 120000){ # longer than 2 min, between events 
        df[riga, 7] <- 1
      }
    }
  }
  dfTEMPmille <- rbind(dfTEMPmille,df)
}

# we need to delete those cheaters who participates more than once     
sum(dfTEMPmille$Errore)

bad_ps <- c('40fd6c5de6f179e5',
            '82a18bc8ee16b9e6',
            '549e3e2c81b4fb56',
            'a179a5457d9f1c36')

temp_mille <- dfTEMPmille[!dfTEMPmille$sid %in% bad_ps,]
temp_mille_wrong <- dfTEMPmille[dfTEMPmille$sid %in% bad_ps,]


# now add a row where there ia 1 
formula_df <- data.frame(event = NULL, 
                      timestampms = NULL, 
                      sid = NULL, 
                      node.id = NULL,
                      n_format = NULL,
                      wave = NULL,
                      Errore = NULL) 
for (i in unique(temp_mille_wrong$sid)){
  # this is my new dataframe subsetted temp_mille_wrong[temp_mille_wrong$sid == i,]
  df <- temp_mille_wrong[temp_mille_wrong$sid == i,]
  # order the dataframe by timestamp
  df <- df[order(df$timestampms),]
  for (riga in c(1:nrow(df))){
    if(df[riga,7] == 0){
      formula_df <- rbind(formula_df, df[riga,]) 
    } else {
      formula_df <- rbind(formula_df, df[riga,]) 
      break
    }
  }
}


dfTEMP <- rbind(temp_mille, formula_df)
dfTEMP0 <- dfTEMP
########## End of further checking ###################



# Identifica il background (tree-container) ripetututo piu volte 
# ed elimina le ripetizioni di background
dfTEMP <- data.frame(event = NULL, 
                     timestampms = NULL, 
                     sid = NULL, 
                     node.id = NULL,
                     n_format = NULL,
                     wave = NULL,
                     Errore = NULL)

for (i in unique(dfTEMP0$sid)){ 
  # this is my new dataframe subsetted dfTEMP0[dfTEMP0$sid == i,]
  df <- dfTEMP0[dfTEMP0$sid == i,]
  # order the datafrae by timestamp
  df <- df[order(df$timestampms),]
  for (riga in c(1:nrow(df))){
    if (riga < nrow(df)){
      if (as.character(df[riga, 4]) == 'tree-container'){
        if (as.character(df[riga+1, 4]) == 'tree-container'){
          df[riga, 7] <- 1
        }
      }
    }
  }
  dfTEMP <- rbind(dfTEMP,df)
}
dfTEMP <- dfTEMP[dfTEMP$Errore==0,]

# It looks like some hovering did not finish but the system says that the event is finished. 
# It's just the text beneith the buttons that intefer with the event, 
# creating lot of repetitions. Here I eliminate these repetitions  
dfTEMP2 <- data.frame(event = NULL, 
                      timestampms = NULL, 
                      sid = NULL, 
                      node.id = NULL,
                      n_format = NULL,
                      wave = NULL,
                      Errore = NULL) 
for (i in unique(dfTEMP$sid)){ 
  # this is my new dataframe subsetted dfTEMP[dfTEMP$sid == i,]
  df <- dfTEMP[dfTEMP$sid == i,]
  # order the datafrae by timestamp
  df <- df[order(df$timestampms),]
  for (riga in c(1:nrow(df))){
    if(riga != 1){
      if (riga < nrow(df)){
        if(df[riga,4] == df[riga-1,4]){
          if(df[riga,4] == df[riga+1,4]){
            df[riga,7] <- 1
          }
        }
      }
    }
  }
  dfTEMP2 <- rbind(dfTEMP2,df)
}
dfTEMP2 <- dfTEMP2[dfTEMP2$Errore==0,]

# ora elimina pure i doppioni (paia) in modo che ci sia 
# solo 1 elemento singolo dietro l'altro
dfTEMP3 <- data.frame(event = NULL, 
                      timestampms = NULL, 
                      sid = NULL, 
                      node.id = NULL,
                      n_format = NULL,
                      wave = NULL,
                      Errore = NULL) 
for (i in unique(dfTEMP2$sid)){
  # this is my new dataframe subsetted dfTEMP2[dfTEMP2$sid == i,]
  df <- dfTEMP2[dfTEMP2$sid == i,]
  # order the datafrae by timestamp
  df <- df[order(df$timestampms),]
  for (riga in c(1:nrow(df))){
    if(riga != 1){ 
      if(as.character(df[riga,4]) == as.character(df[(riga-1),4])){
        df[riga,7] <- 1
      }
    }
  }
  dfTEMP3 <- rbind(dfTEMP3,df)
}
dfTEMP3 <- dfTEMP3[dfTEMP3$Errore==0,] 

# let's add a tree-container line where there are two subsequent letters (e.g., AB)
# first add 1 to the row whih signal the (subsequent) missing tree-container
dfTEMP4 <- data.frame(event = NULL, 
                      timestampms = NULL, 
                      sid = NULL, 
                      node.id = NULL,
                      n_format = NULL,
                      wave = NULL,
                      Errore = NULL) 
for (i in unique(dfTEMP3$sid)){
  # this is my new dataframe subsetted dfTEMP3[dfTEMP3$sid == i,]
  df <- dfTEMP3[dfTEMP3$sid == i,]
  # order the dataframe by timestamp
  df <- df[order(df$timestampms),]
  for (riga in c(1:nrow(df))){
    if(riga == nrow(df)){
      if(as.character(df[riga,4]) != 'tree-container'){
        df[riga,7] <- 1
      }
    }
    if(riga < nrow(df)){
      if(as.character(df[riga,4]) != 'tree-container'){
        if(as.character(df[(riga+1),4]) != 'tree-container'){
          df[riga,7] <- 1
        }
      }
    }
  }
  dfTEMP4 <- rbind(dfTEMP4,df)
}

# now add a row where there ia 1 
dfTEMP5 <- data.frame(event = NULL, 
                      timestampms = NULL, 
                      sid = NULL, 
                      node.id = NULL,
                      n_format = NULL,
                      wave = NULL,
                      Errore = NULL) 
for (i in unique(dfTEMP4$sid)){
  # this is my new dataframe subsetted dfTEMP4[dfTEMP4$sid == i,]
  df <- dfTEMP4[dfTEMP4$sid == i,]
  # order the dataframe by timestamp
  df <- df[order(df$timestampms),]
  for (riga in c(1:nrow(df))){
    if(df[riga,7] == 0){
      dfTEMP5 <- rbind(dfTEMP5, df[riga,]) 
    } else {
      if(riga!=nrow(df)){
        dfTEMP5 <- rbind(dfTEMP5, df[riga,]) 
        newrow2 <- data.frame(event = df[riga+1,1], 
                              timestampms = df[riga+1,2], 
                              sid = df[riga+1,3], 
                              node.id = 'tree-container',
                              n_format = df[riga+1,5],
                              wave = df[riga+1,6],
                              Errore = 0) 
        dfTEMP5 <- rbind(dfTEMP5, newrow2) 
      }
    }
  }
}

# now replace the 1s with 0s

dfTEMP5$Errore <- rep(0, nrow(dfTEMP5))

# The next code (to get the strings) won't work if the first raw is the 
# background (tree-container). Now, let's get rid of the first line if it is the background, 
# for each participant subset.
dfTEMP6 <- data.frame(event = NULL, 
                      timestampms = NULL, 
                      sid = NULL, 
                      node.id = NULL,
                      n_format = NULL,
                      wave = NULL,
                      Errore = NULL) 
for (i in unique(dfTEMP5$sid)){
  # this is my new dataframe subsetted dfTEMP5[dfTEMP5$sid == i,]
  df <- dfTEMP5[dfTEMP5$sid == i,]
  # order the datafrae by timestamp
  df <- df[order(df$timestampms),]
  for (riga in c(1:nrow(df))){
    if(riga==1 & df[riga,4] != 'tree-container'){
      dfTEMP6 <- rbind(dfTEMP6, df[riga,])
    }
    else if(riga!=1){
      dfTEMP6 <- rbind(dfTEMP6, df[riga,])
    }
  }
}

# check whether any participant subdataset starts with mouseout and put a 1
dfTEMP7 <- data.frame(event = NULL, 
                      timestampms = NULL, 
                      sid = NULL, 
                      node.id = NULL,
                      n_format = NULL,
                      wave = NULL,
                      Errore = NULL) 
for (i in unique(dfTEMP6$sid)){
  # this is my new dataframe subsetted dfTEMP6[dfTEMP6$sid == i,]
  df <- dfTEMP6[dfTEMP6$sid == i,]
  # order the dataframe by timestamp
  df <- df[order(df$timestampms),]
  for (riga in c(1:nrow(df))){
    if(riga==1 & df[riga,1] == 'mouseout'){
      #print('zioboia')
      df[riga,7] <- 1
    }
  }
  dfTEMP7 <- rbind(dfTEMP7,df)
  if (1 %in% c(dfTEMP7$Errore)){
    print(as.character(dfTEMP7[1,3]))
    #stop('Some Ps sequences start with mouseout')
  }
}

# there are some Ps who started with mouseout
# this is perhaps because they were already therem in that position when the page loaded. 

# substitute all mouseout with mousover 
dfTEMP7$event[dfTEMP7$event=='mouseout'] <- 'mouseover'

# convert 'mouseover' to 'mouseout' in the 'tree-container' row
# and convert tree-container into the letter that preceeds it
dfTEMP8 <- data.frame(event = NULL, 
                      timestampms = NULL, 
                      sid = NULL, 
                      node.id = NULL,
                      n_format = NULL,
                      wave = NULL,
                      Errore = NULL) 
for (i in unique(dfTEMP7$sid)){
  # this is my new dataframe subsetted dfTEMP7[dfTEMP7$sid == i,]
  df <- dfTEMP7[dfTEMP7$sid == i,]
  # order the datafrae by timestamp
  df <- df[order(df$timestampms),]
  for (riga in c(1:nrow(df))){
    if(as.character(df[riga,4]) == 'tree-container'){
      df[riga,4] <- df[(riga-1),4]
      df[riga,1] <- 'mouseout'
    }
  }
  dfTEMP8 <- rbind(dfTEMP8,df)
}

# substitute the DF (ecluding the Error column)
dfTEMP8 <- dfTEMP8[-7]

# -------------------------------------------------------------------
# Generating the sequences and durations 
# -------------------------------------------------------------------

# compute the duration of the events
dfTEMP8$duration <- c(rep(0, nrow(dfTEMP8)))

# order the rows by timestampms 
dfTEMP9 <- data.frame(event = NULL, 
                       timestampms = NULL, 
                       sid = NULL, 
                       node.id = NULL,
                       n_format = NULL,
                       wave = NULL,
                       duration = NULL) 
for (i in unique(dfTEMP8$sid)){
  # this is my new dataframe subsetted dfTEMP8[dfTEMP8$sid == i,]
  df <- dfTEMP8[dfTEMP8$sid == i,]
  # order the datafrae by timestamp
  df <- df[order(df$timestampms),]
  for (riga in c(1:(nrow(df)-1))){
    if(df[riga,1] == df[riga+1,1]){
      stop(paste('There are two subsequent events of the same type!',
                 'The faulty raw is',riga))
    } else{
      df[riga, 7] <- df[riga+1,2] - df[riga,2]
    }
  }
  dfTEMP9 <- rbind(dfTEMP9,df)
} 

# Just check that the events in a participant scanpath start with mouseover and end with 
# mouseout (you shouldn't get an error)
for (i in unique(dfTEMP9$sid)){
  # this is my new dataframe subsetted dfTEMP9[dfTEMP9$sid == i,]
  df <- dfTEMP9[dfTEMP9$sid == i,]
  # order the datafrae by timestamp
  df <- df[order(df$timestampms),]
  if(df[nrow(df),1] == 'mouseover'){
    print(df)
    stop('There is a Ps which has the last event mouseover')
  }
  else if(df[1,1] == 'mouseout'){
    print(df)
    stop('There is a Ps which has the first event mouseout')
  }
}

# get rid of the events with 'mouseout'
dfTEMP10 <- dfTEMP9[dfTEMP9$event=="mouseover",]

# create a DF with empty space for scanpaths and duration 
dfScanpath <- data.frame(Ps = unique(dfTEMP10$sid), 
                         Scanpath = rep(0, length(c(unique(dfTEMP10$sid)))))

# create scanpaths
for (p in unique(dfTEMP10$sid)){
  scan <- ""
  df <- dfTEMP10[dfTEMP10$sid==p,]
  # order the datafrae by timestamp
  df <- df[order(df$timestampms),]
  for (r in 1:nrow(df)){
    scan <- paste(scan, df[r,4], sep="")
  }
  dfScanpath$Scanpath[dfScanpath$Ps==p] <- scan
  scan <- ""
}

# -------------------------------------------------------------------
# Generating the more complex sequences (of tuples as strings)  
# -------------------------------------------------------------------
# create a DF with empty space for scanpaths 
dfScanpath$Scanpath_complex <- rep('A', length(c(unique(dfTEMP10$sid))))

# create scanpaths
for (p in unique(dfTEMP10$sid)){
  #scan <- ""
  df <- dfTEMP10[dfTEMP10$sid==p,]
  # order the datafrae by timestamp
  df <- df[order(df$timestampms),]
  #View(df)
  #stop()
  for (r in 1:nrow(df)){
    if (r==1){
      scan <- paste(df[r,4],df[r,7], sep=":")  
    }
    else{
      scan <- paste(scan, paste(df[r,4],df[r,7], sep=":"), sep=",")
    }
  }
  dfScanpath$Scanpath_complex[dfScanpath$Ps==p] <- scan
  scan <- ""
}

# eliminate scanpaths of length less than 3
dfScanpath2 <- dfScanpath[nchar(dfScanpath$Scanpath) > 2,]

# -------------------------------------------------------------------
# Now it is time to merge the DFs  with Alan
# -------------------------------------------------------------------

# Dataframe creation: Now I want to merge the scanpath DF with Alan DF
alanDF <-read.csv(file="final_alan_df.csv",header=TRUE,sep=",") 
# change the name of the Ps column
colnames(alanDF)[3] <- 'Ps'
# Merge two data frames by Ps ID
totalDF <- merge(dfScanpath2,alanDF,by="Ps")

# in Aitor df I only want Ps that are present in Alan DF
dfTEMP11 <- dfTEMP10[dfTEMP10$sid %in% totalDF$Ps,]


# There are 2 important dataframes here:
# dfTEMP11 (for the duration in each single location, for ech Ps)
# totalDF (for the simple and complex scanpaths, including all Alan data)

# WRITE THE DATA FILE  
write.csv(totalDF, file = "analysis_scanpath.csv", row.names=F)
write.csv(dfTEMP11, file = "analysis_duration.csv", row.names=F)

#? end

































