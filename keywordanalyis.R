############################################################################
##      @author: Ankit Sharma                                             ##
##    This script does the keyword analysis for the .csv file             ##
############################################################################

#Load libraries
library(tm)
library(RWeka)
library(SnowballC)

#Set working directory


#################################################################
##    Exact Match Data                                         ##
#################################################################
# Create a dataframe for storing values of the matrix
outFrameExact <- data.frame(abortion=integer(),
                            economy=integer(),
                            education=integer(),
                            energy=integer(),
                            environment=integer(),
                            unemployment=integer())

colNamesExact <- c("abortion","economy","education","energy","environment","unemployment")  

#######################################################################################


#####################################################################
##            Partial Match Data                                   ##
#####################################################################
#Create a dataframe for storing values of the matrix
outFramePartial <- data.frame(economy=integer(),
                              unemployment=integer(),
                              tax=integer())

#Set Column names
colNamesPartial <- c("economy","unemployment","tax")

#####################################################################



######################################################################
##        No Match Data                                             ##
######################################################################
# Create a dataframe for storing values of the matrix
outFrameNotMatch <- data.frame(unemployment=integer())

#Column Names
colNamesNotMatch <- c("unemployment")

######################################################################


#Read Data from CSV File
election <- read.csv("election2.csv", header = TRUE, sep = ",")


######################################################################
# Text Data
######################################################################
text_data <- election$text


##Read Keywords from various input files

#Exact Match
myKeywordsExact <- read.csv("keywords_exact.csv", header = TRUE, sep = ",")

#Not Match
myKeywordsNotMatch <- read.csv("keywords_notmatch.csv", header = TRUE, sep = ",")

#Partial Match
myKeywordsPartial <- read.csv("keywords_partial.csv", header = TRUE, sep = ",")

#Read tweets from myData DataFrame into a vector and remove emojis and hyperlinks

election_text <- text_data

#Remove emojis
election_text <- gsub("[^\x20-\x7e]"," ", election_text)

#Remove http links and other special characters
election_text <- gsub("(@|http)[^[:blank:]]*|[[:punct:]]|[[:digit:]]"," ", election_text)
election_text <- gsub("\\s+", " ", election_text)

trim <- function (x) gsub("^\\s+|\\s+$", "", x) #Trim "white spaces" 

###########################################################
# Convert into data corpus for tmap package
###########################################################

myCorpus <- Corpus(VectorSource(election_text))

#Remove stopwords
##########################################################
myStopwords = c(stopwords('english'),'description','null','text','description','url','text','href','rel','nofollow','false','true','rt')
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

#Clean the data corpus, remove punctuation, Numbers, whitespace, words

myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, stripWhitespace)

#Ngram tokenizer function
options(mc.cores=1)
myTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 4))
##The number of keywords (e.g., "jobs," "steve jobs")


#############################################################
# Exact Match function
#############################################################

myExactMatch <- function(myCorpus, myKeywordsExact) {
    
# For loop to go through all the keywords and check all the data

    for (i in 1:ncol(myKeywordsExact)) {
      tweetdtm <- DocumentTermMatrix(myCorpus, control=list(tokenize=myTokenizer, wordLengths= c(1,Inf), dictionary = myKeywordsExact[,i]))
      tweetm <- as.matrix(tweetdtm)
      newmat <- rowSums(tweetm)
      if(nrow(outFrameExact)==0){
        outFrameExact=data.frame(newmat)
      }else{
        outFrameExact <- cbind2(outFrameExact, newmat)
      }
    }
    colnames(outFrameExact) <- colNamesExact
    
    return (outFrameExact)
}

##############################################################################
#Function for partial matches
##############################################################################

myPartialMatch <- function(myCorpus, myKeywordsPartial) {
    
    #Convert corpus back to a dataFrame
    tweetFrame <- data.frame(text=unlist(sapply(myCorpus, `[`, "content")), stringsAsFactors=FALSE)
    
    ##########################################################
    # For loop to go through all the keywords and check all the data
    ##########################################################
    for (i in 1:NROW(tweetFrame)) {
      for (j in 1:NCOL(myKeywordsPartial)) {
        sumMatches <- 0
        for (k in 1:NROW(myKeywordsPartial)) {
       
          tweetStr <- trim(tweetFrame[i,1])
          tweetStr <- strsplit(tweetStr, " ")
          tweetChar <- unlist(tweetStr, recursive = TRUE, use.names = TRUE)
          if (myKeywordsPartial[k,j] != "") {
            partmatchVal <- grep(myKeywordsPartial[k, j], as.character(tweetChar), fixed = TRUE, value = FALSE, invert = FALSE)
            partmatches <- length(partmatchVal)
            sumMatches <- sumMatches + partmatches
          }
        }
        if (sumMatches == 0) {
          outFramePartial[i,j] = 0
        } else {
          outFramePartial[i,j] = 1
        }
      }
    }
    colnames(outFramePartial) <- colNamesPartial
    return (outFramePartial)
}

##############################################################
# Not Match Function
#############################################################

myNotMatch <- function(myCorpus, myKeywordsNotMatch) {
    
    ##########################################################
    # For loop to go through all the keywords and check all the data
    ##########################################################
    for (i in 1:NCOL(myKeywordsNotMatch)) {
      tweetdtm <- DocumentTermMatrix(myCorpus, control=list(tokenize=myTokenizer, wordLengths= c(1,Inf), dictionary = myKeywordsNotMatch[,i]))
      tweetm <- as.matrix(tweetdtm)
      newmat <- rowSums(tweetm)
      if(nrow(outFrameNotMatch)==0){
        outFrameNotMatch = data.frame(newmat)
      } else {
        outFrameNotMatch <- cbind2(outFrameNotMatch, newmat)
      }
    }
    colnames(outFrameNotMatch) <- colNamesNotMatch
    
    return (outFrameNotMatch)
}

##################################################################
#Run all the functions and get all the dataframes for the matches
##################################################################
myExactMatchFrame <- myExactMatch(myCorpus, myKeywordsExact)
myPartialMatchFrame <- myPartialMatch(myCorpus, myKeywordsPartial)
myNotMatchFrame <- myNotMatch(myCorpus, myKeywordsNotMatch)

########################################################################
#Combine all dataframes into one
########################################################################
myMatchesFinal <- function(myExactMatchFrame, myPartialMatchFrame, myNotMatchFrame) {
    #Find common columns by column names
    colNames <- intersect(colnames(myExactMatchFrame), colnames(myPartialMatchFrame)) 
    
    #Add the values in both Exact match and Partial match
    myExactMatchFrame[colNames] <- myExactMatchFrame[colNames] + myPartialMatchFrame[colNames]
    
    #Now find common columns in Exact match and not match
    colComm <- intersect(colnames(myExactMatchFrame), colnames(myNotMatchFrame))
    
    #Subtract the values for not match from Exact match
    myExactMatchFrame[colComm] <- myExactMatchFrame[colComm] - myNotMatchFrame[colComm]
    
    #Combine the dataframes into one
    
    ###########################################################
    # If all partial match columns are in exact match file then
    # use this function
    ###########################################################
    #myMatches <- myExactMatchFrame
    
    #########################################################
    # If partial match keywords contain different columns 
    # than exact match, then use this function.
    ########################################################
    
    myMatches <- cbind(myPartialMatchFrame[setdiff(colnames(myPartialMatchFrame), colNames)], myExactMatchFrame)
    
    #Conditional to convert the dataFrame into a binary vector
    for(i in 1:nrow(myMatches)) {
      for(j in 1:ncol(myMatches)) {
        myMatches[i,j] <- ifelse(myMatches[i,j]>0, 1, 0)
      }
    }
    
    return (myMatches)
}

myMatchesFinalFrame <- myMatchesFinal(myExactMatchFrame, myPartialMatchFrame, myNotMatchFrame)

##############################################################################
# This is the final frame to written to csv file, the first column should be
# changed to what you want it to be
##############################################################################
finalFrameToFile <- cbind(text_data, myMatchesFinalFrame)

#########################################################################
#Write the dataFrame to a CSV file
#########################################################################
write.table(finalFrameToFile, file = "election2output.csv", sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = "double")

