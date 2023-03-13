## Read from SQLite
#require('RSQLite')
library(RSQLite)



## Input table from database; template from the same database

setwd('C:/Users/pyh/Desktop/PastResearch/AMR/PDFTables/Step2(b)_SavingThemIntoDatabase/')
NARMS2015 <- dbConnect(RSQLite::SQLite(), dbname = "NARMS2015.db") #old
#NARMS2015 <- dbConnect(RSQLite::SQLite(), dbname = "NARMS2015_noheader.db") #new
Input <- dbGetQuery(NARMS2015, "SELECT * FROM 'tabula-NARMS2015-0'")

head(Input)
dim(Input)

template <- dbGetQuery(NARMS2015, "SELECT * FROM 'template'")
#head(template)
#dim(template)

## Correct table from database
#setwd('C:/Users/pyh/Desktop/PDFTables/Step3(b)_SavingThemIntoDatabase/')
setwd('C:/Users/pyh/Desktop/PastResearch/AMR/PDFTables/Step3(b)_SavingThemIntoDatabase/')
NARMS2015c <- dbConnect(RSQLite::SQLite(), dbname = "NARMS2015correct.db")
#NARMS2015c <- dbConnect(RSQLite::SQLite(), dbname = "NARMS2015correct_noheader.db")

Correct <- dbGetQuery(NARMS2015c, "SELECT * FROM 'tabula-NARMS2015-0'")
dim(Input)
dim(Correct)
head(Correct)

## Measure if the two tables are similar

# Replace NA with blank (First step in order to recognize the blank parts)
Input[is.na(Input)] <- ''
template[is.na(template)] <- ''
Correct[is.na(Correct)] <- ''

# Measure their similarity *** of schema (input v.s. template) ###Step1: Split column??
Match<-MisMatch<-0
n=dim(Input)[1]
m=dim(template)[2]##num of columns for schema

dim(template)[2] == dim(Input)[2]##check dimension(go check if split is needed if FALSE)






# Measure their similarity ***
Match<-MisMatch<-0
for(j in 1:dim(Correct)[2]){
  #Var1<-NULL
  colMatch <- colMisMatch <- NULL
  for(i in 1:dim(Correct)[1]){
    if(is.na(Input[i,j]) && is.na(Correct[i,j])){colMatch[i] <- 1; colMisMatch[i] <- 0}
    else if(is.na(Input[i,j]) && !is.na(Correct[i,j])){colMatch[i] <- 0; colMisMatch[i] <- 1}
    else if(!is.na(Input[i,j]) && is.na(Correct[i,j])){colMatch[i] <- 0; colMisMatch[i] <- 1}
    else if(Input[i,j] == Correct[i,j]){colMatch[i] <- 1; colMisMatch[i] <- 0}
    else {colMatch[i] <- 0; colMisMatch[i] <- 1}
  }
  Match <- Match + sum(colMatch)
  MisMatch <- MisMatch + sum(colMisMatch)
  #for(i in 1:dim(Correct)[1]){
  #  if(Input[i,j] == Correct[i,j]){
  #  Match <- Match + 1
  #  }
  #  else{MisMatch <- MisMatch + 1}
  #}
} ## ignore the warning 

Match; MisMatch
total <- (dim(Correct)[1])*(dim(Correct)[2])
Similarity <- Match/(total)
Similarity


## Lesser than 70%, then goes back to checking process:
## Setup a rule to see if the column need to seperate

# Check tables dimension
dimension <- dim(Input) == dim(Correct)

#Input <- df
# find first column that is not matching (usually combining together)
findColumnToSplit<-function(inputTable = Input, correctTable = Correct, startingCol = j, misMatchRate = MisMatch){
  #j <- 1
  #MisMatch <- 0
  repeat{
    Var1<-NULL
    for(i in 1:dim(Input)[1]){
      if(is.na(Input[i,j]) && is.na(Correct[i,j])){Var1[i]<-TRUE}
      else if(Input[i,j] == Correct[i,j]){Var1[i]<-TRUE}
      else {Var1[i]<-FALSE}
    }
    
    a <- as.data.frame(table(Var1))
    if(a[a$Var1==TRUE,2]==(dim(Input)[1])){
      MisMatch <- 0
    }
    else{
      MisMatch <- (a[a$Var1==FALSE,2])/(dim(Input)[1])
    }
    if(MisMatch > 0.5){
      break
    }
    j <- j+1
  }
  #print(j)
  print(MisMatch)
  return(j)
  
  #j # determine which column
  #MisMatch # the mismatch rate for that column
}
findColumnToSplit(Input, template, 1, 0)
matchRate(Input, Correct)
## Do the column separation for this column 
install.packages('splitstackshape')
library(splitstackshape)

columnSplit <- function(InputTable = Input, colToSplit = j){
  df <- Input
  # Separate the column we found from previous step
  names(df)[j]
  x <- as.data.frame(df[,j])
  names(x) <- names(df)[j]
  splitcolumns <- cSplit(x, names(df)[j], c(" "))
  ## Check if the other delimiters within the columns
  df <- cbind(df[,1:(j-1)], splitcolumns, df[,(j+1):(dim(Input)[2])])
  return(df)
}

updatedInput <- columnSplit(Input, 4)

## Go back to check the mismatch



findColumnToSplit(updatedInput, Correct, 4, 0.8823529)





write.csv(df, 'df.csv',row.names = FALSE)

#dbReadTable(NARMS2015, 'table-0', value = df)
dbWriteTable(NARMS2015, value = df, name = "MyTable", append = TRUE ) 




matchRate <- function(InputTable = Input, CorrectTable = Correct){
  ## calculate the match rate
  Input <-df1
  Match<-MisMatch<-0
  for(j in 1:dim(Correct)[2]){
    colMatch <- colMisMatch <- NULL
    for(i in 1:dim(Correct)[1]){
      if(identical(as.character(Input[i,j]),Correct[i,j])){colMatch[i] <- 1; colMisMatch[i] <- 0}
      else {colMatch[i] <- 0; colMisMatch[i] <- 1}
    }
    Match <- Match + sum(colMatch)
    MisMatch <- MisMatch + sum(colMisMatch)
    
  } 
  
  #Match; MisMatch
  total <- (dim(Correct)[1])*(dim(Correct)[2])
  Similarity <- Match/(total)
  return(Similarity)
}


findColumnToSplit2<-function(inputTable = Input, correctTable = template, startingCol = j, misMatchRate = MisMatch){
  j <- 1 # Start
  MisMatch <- 0
  repeat{
    # compute the similarity between each cell
    Var1<-NULL 
    for(i in 1:n){
      if(identical(as.character(Input[i,j]), template[i,j])){
        Var1[i]<-1}
      #else if(Input[i,j] == template[i,j]){Var1[i]<-TRUE}
      else if (!is.na(as.character(Input[i,j])) && !is.na(template[i,j])){
        Var1[i]<-stringsim(as.character(Input[i,j]), template[i,j])
      }
      else if (is.na(as.character(Input[i,j])) || is.na(template[i,j])){
        Var1[i]<- 0
      }
    }
    #a <- as.data.frame(table(Var1))
    
    # compute the similarity between each column
    if(sum(Var1==1) == n){ # cells in the whole column are the same 
      MisMatch <- 0
    }
    else{
      MisMatch <- (1 - (sum(Var1)/n))#(1 - min(Var1)) # set the mismatch rate as the largest dissimilarity
      #(sum(Var1==1))/n
    }
    
    if(MisMatch >= 0.3){ # parameter 1: schema mismatch threshold ###
      break
    }
    j <- j+1
  }
  
  print(j) # determine which column
  print(MisMatch) # the mismatch rate for that column
  return(j)
}
findColumnToSplit2(Input, template, 1, 0)
