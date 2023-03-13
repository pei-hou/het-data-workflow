## Read from SQLite
setwd('C:/Users/pyh/Desktop/AMRDB/')
require('RSQLite')

AMR <- dbConnect(RSQLite::SQLite(), dbname = "AMR.db")
Human <- dbGetQuery(AMR, "SELECT * FROM Human")
#Human <- dbReadTable(AMR, "Human")
#write.csv(Human, 'Human.csv',row.names = FALSE)
# Data Cleaning
Human <- Human[1:992,]
head(Human)
tail(Human)

Animal <- dbGetQuery(AMR, "SELECT * FROM Animal")
#Animal <- dbReadTable(AMR, "Animal")
#write.csv(Animal, 'Animal.csv',row.names = FALSE)

head(Animal)
class(Animal)

##


## Execute Query
Human_S.Newport <- dbGetQuery(AMR, "SELECT * FROM Human WHERE Serotype = 'Salmonella Newport' ")


table(Human$Streptomycin.STR.R.S.I)

## Integrate tables
importfileS = list(Animal, Human)
schema<-union(names(Animal),names(Human))
schema<-c('TableSource', schema)

# Create an empty data frame with all the columns(fields) from imported files
df = data.frame(matrix(vector(), 0, length(schema),
                dimnames=list(c(), schema)),
                stringsAsFactors=F)


for(j in 1:length(importfileS)){#multiple files would like to be read
  importfile = importfileS[[j]] #current file
  n = nrow(importfile)
  m = nrow(df) # number of rows in current loop
  for(i in 1:length(importfile)){ 
    if(sum(names(importfile)[i] == schema) == 1){ # to see if any column in file match schema
      k = which(names(importfile)[i] == schema) # find which column
      #if(j == 1){
      #  df[1:n,k] <- importfile[,i]} # put the column into the appointed position
      #else {
      df[(m+1):(m+n),k] <- importfile[,i]  
      #}
    }
    df[(m+1):(m+n), 1] <- rep(j, n)
  }
}

dim(df)
head(df)
tail(df) # Check if the data read as expected

table(df$TableSource)

# Write integrated table into database
#AMR <- dbConnect(RSQLite::SQLite(), dbname = "AMR.db")
#dbWriteTable(AMR, "Integrated", df)

# Seperate the combined columns (Undo)
library(splitstackshape)
#df_MIC_ALL <- df[, grepl("MIC", as.character(names(df)))]
#x1<-cSplit(df_MIC_ALL,names(df_MIC_ALL)[3],  c("/"));x1
#x1<-cSplit(x1, names(df_MIC_ALL)[13],  c("/"));x1
#dim(x1)
#x1<-(as.data.frame(x1))
#NAME<-names(x1)

dim(df)

# Delete compound drugs 
df1 <- df[, !grepl("SXT", as.character(names(df)))]
df1 <- df1[, !grepl("AUG", as.character(names(df1)))]
dim(df1)

df_MIC_ALL <- df1[, grepl("MIC", as.character(names(df1)))]
x1<-df_MIC_ALL

dim(x1)

# Seperate the signs (First omit them)
newMIC_i<-newMIC<-NULL
for(i in 1:length(x1)){
newMIC_i <- gsub(".*<|>","",x1[,i])
newMIC <- cbind(newMIC, newMIC_i)
}
newMIC
newMIC<-as.data.frame(newMIC)
colnames(newMIC)<-names(df_MIC_ALL)




## Revise the compound drugs (combine seems to be incorrect) ****Not do now****
#n=dim(newMIC)[1]
#originValue = newMIC$Amoxicillin.Clavulonicacid.AUG.MIC
#originValue = newMIC$Trimethoprim.Sulfamethoxazole.SXT.MIC

#newValue<-NULL
#for(i in 1:n){
#  newValue[i] <- eval(parse(text=as.character(originValue[i])))
#  }
#table(newValue)


# Replace the MIC with newMIC
df1<-df1[,!grepl("MIC", as.character(names(df1)))]
df1<-cbind(df1,newMIC)
head(df1)

## Make the squashtogram from NARMS
require("tm")
require('binom')
require('lubridate')

table((df1$TableSource == 2)&(df1$Serotype == 'Salmonella Newport')&(year(mdy(df1$Date))=='2010')) #TableSource2:Human

conditions <- (df1$TableSource == 2)&(df1$Serotype == 'Salmonella Newport')&(year(mdy(df1$Date))=='2010') ### Change the serotype here

#Serotype <- df$Serotype
#table(Serotype)
#names(df)
df_MIC <- df1[conditions, grepl("MIC", as.character(names(df1)))]
df_RSI <- df1[conditions, grepl("R.S.I", as.character(names(df1)))]

dim(df_MIC)
dim(df_RSI)

#table(df_MIC[,1])

## Make the table NAME##
Name <- names(Animal)
Name <- Name[grepl("MIC", as.character(Name))]
Name <- Name[!grepl("AUG", as.character(Name))]
Name <- Name[!grepl("SXT", as.character(Name))]

Name <- removeWords(Name, 'MIC')
Name

#x1 <- unlist(strsplit(Name[3],split='/', fixed=TRUE))
#x2 <- unlist(strsplit(Name[13],split='/', fixed=TRUE))

#Name<-c(Name[-c(3,13)], x1, x2)
#Name
#by(df_RSI, Serotype, table) # by serotype

#table(df_RSI[,1])

apply(df_RSI, 2, table)
apply(df_MIC, 2, table)

# %I %R



y <- as.character(c('I_perc', 'R_perc', 'CI_U', 'CI_L')) #target table column
# Create an empty data frame with all the columns(fields) from imported files
ytable = data.frame(matrix(vector(), 0, length(y),
                    dimnames=list(c(), y)),
                    stringsAsFactors=F)

#freq <- apply(df_RSI, 2, table)

freq<-NULL
for(i in 1:dim(df_RSI)[2]){
  freq[[i]]<-table(df_RSI[,i])
}

for(i in 1:dim(df_RSI)[2]){

    x = round(freq[[i]]/sum(freq[[i]])*100, 1)
    xtable_perc = as.data.frame(x, row.names = NULL,
                                responseName = "Freq", stringsAsFactors = TRUE,
                                sep = "", base = list(LETTERS))
    xtable_freq = as.data.frame(freq[[i]], row.names = NULL,
                                responseName = "Freq", stringsAsFactors = TRUE,
                                sep = "", base = list(LETTERS))



perc_R = NA
perc_S = NA
perc_I = NA
CI$lower = NA
CI$upper = NA

if(sum(xtable_perc$Var1 == 'Int')==1){
  perc_I = xtable_perc[xtable_perc$Var1 == 'Int', 2]}
if(sum(xtable_perc$Var1 == 'R')==1){
  perc_R = xtable_perc[xtable_perc$Var1 == 'R', 2]
  
  # CI of R
  freq_R = xtable_freq[xtable_freq$Var1 == 'R', 2]
  freq_all = sum(xtable_freq[, 2])
  CI <- binom.lrt(freq_R, freq_all, conf.level = 0.95, bayes = FALSE, conf.adj = FALSE, plot = FALSE)
}

if(sum(xtable_perc$Var1 == 'S')==1){
  perc_S = xtable_perc[xtable_perc$Var1 == 'S', 2]}


ytable[i,] <- round(c(perc_I, perc_R, CI$lower*100, CI$upper*100),2)

}

tableRSI <- ytable

tableRSI

cbind(names(df_RSI),tableRSI)


# %MIC
y <- as.character(c(0.015, 0.03, 0.06, 0.125, 0.25, 0.5, 2^(0:9))) #target table column
# Create an empty data frame with all the columns(fields) from imported files
ytable = data.frame(matrix(vector(), 0, length(y),
                    dimnames=list(c(), y)),
                    stringsAsFactors=F)
#i = 1
#grepl("AMI", as.character(names(df_MIC)))

for(i in 1:dim(df_MIC)[2]){
x = df_MIC[,i] # i th medicine (column) ##Revise later

x_perc = round(table(x)/sum(table(x))*100,1)

xtable <- as.data.frame(x_perc, row.names = NULL,
                   responseName = "Freq", stringsAsFactors = TRUE,
                   sep = "", base = list(LETTERS))

  for(j in 1:length(xtable$x)){
    if(any(xtable$x[j] == y)){
    ytable[i, which(xtable$x[j] == y)] = xtable$Freq[j]
    }
    else{
      ytable[i, which.min(abs(as.numeric(y)-as.numeric(as.vector(xtable$x[j]))))] = xtable$Freq[j]
    }
  }

}

tableMIC <- ytable

tableMIC 



# Check sum of the percentage
Sum<-NULL
for(i in 1:dim(tableMIC)[1]){
Sum[i]<-rowSums(tableMIC[i,], na.rm = TRUE)
}
Sum
length(Sum)



# Match the row names
cbind(names(df_RSI),tableRSI)

cbind(names(df_MIC),tableMIC)



# Replace NA with " " space

#df <- sapply(tableMIC, as.character)
#df[is.na(df)] <- " "
#tableMIC <- as.data.frame(df)
#tableMIC

# using the format of squashtogram
cbind(Name, names(df_RSI),names(df_MIC))

tableALL <- cbind(Name, tableRSI, tableMIC)
tableALL


# Save it into database

#write.csv(tableALL, 'tableALL.csv')  
setwd('C:/Users/pyh/Desktop/AMRDB/')
AMR_Agg <- dbConnect(RSQLite::SQLite(), dbname = "AMR_Aggregation.db")
dbWriteTable(AMR_Agg, "table_Source2_SalmonellaNewport_2010", tableALL)