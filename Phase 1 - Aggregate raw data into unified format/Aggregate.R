
## Read from SQLite
setwd('C:/Users/pyh/Desktop/AMRDB/')
require('RSQLite')

AMR <- dbConnect(RSQLite::SQLite(), dbname = "AMR.db")
Human <- dbGetQuery(AMR, "SELECT * FROM Human")
#Human <- dbReadTable(AMR, "Human")
#write.csv(Human, 'Human.csv',row.names = FALSE)
head(Human)
tail(Human)
Animal <- dbGetQuery(AMR, "SELECT * FROM Animal")
#Animal <- dbReadTable(AMR, "Animal")
#write.csv(Animal, 'Animal.csv',row.names = FALSE)

head(Animal)
class(Animal)

## Execute Query
Counts <- dbGetQuery(AMR, "SELECT Serotype, strftime('%Y', Date) AS Year, COUNT(*) AS N
                                    FROM Human
                                    GROUP BY Serotype, strftime('%Y', Date);")

Counts


## Using R function to aggregate
require(dplyr)
library(lubridate)

df1 = Human
a=mdy(df1$Date)
#year(a)
x = df1 %>% count(Serotype) %>% print(n = Inf) 
#x = df1 %>% count(Serotype, year(a)) %>% print(n = Inf) 


df2 = Animal
a=mdy(df2$Date)
#year(a)
y = df2 %>% count(Serotype) %>% print(n = Inf) 
y = df2 %>% count(Serotype, year(a)) %>% print(n = Inf) 


## find the rows with errors
df1[df1$Serotype=='Salmonella Typhimurium,B:i:2',]
df1[df1$Serotype=='Salmonella Typhimurium  C',]

df1[is.na(df1$Serotype),]

## Delete the empty rows
df = df1[-c(993:1005),]
tail(df)

## Revise the rows with errors
df[736,names(df1)=='Serotype'] = 'Salmonella Typhimurium B:i:2'
df[719,names(df1)=='Serotype'] = 'Salmonella Typhimurium.C'
df[c(719,736),]

## Check the result now (df)
a=mdy(df$Date)
x = df %>% count(Serotype) %>% print(n = Inf) 
x = df %>% count(Serotype, year(a)) %>% print(n = Inf) 

## Cover the Human, Animal
Human = df
