
setwd('C:/Users/pyh/Desktop/AMRDB/')
AMR_Agg <- dbConnect(RSQLite::SQLite(), dbname = "AMR_Aggregation.db")
LabAgg1 <- dbGetQuery(AMR_Agg, "SELECT * FROM table_Source2_SalmonellaTyphimurium_2009")
LabAgg2 <- dbGetQuery(AMR_Agg, "SELECT * FROM table_Source2_SalmonellaTyphimurium_2010")

DataSource <-  rep('LabData:Human',dim(LabAgg1)[1])
Year<-rep(2009,dim(LabAgg1)[1]); SerotypeName <- rep('Salmonella Typhimurium',dim(LabAgg1)[1])
LabAgg1 <- cbind(DataSource, Year, SerotypeName, LabAgg1)

DataSource <-  rep('LabData:Human',dim(LabAgg2)[1])
Year<-rep(2010,dim(LabAgg2)[1]); SerotypeName <- rep('Salmonella Typhimurium',dim(LabAgg2)[1])
LabAgg2 <- cbind(DataSource, Year, SerotypeName, LabAgg2)


LabAgg <- rbind(LabAgg1,LabAgg2)
dim(LabAgg)

Name<-names(LabAgg)

setwd("C:/Users/pyh/Desktop/PDFTables/Step4(b)_SavingThemIntoDatabase/")
NARMS2015cleaned <- dbConnect(RSQLite::SQLite(), dbname = "NARMS2015cleaned.db")
#dbWriteTable(NARMS2015cleaned, value = Input , name = "CleanedTable2", append = TRUE ) 
PDFTables1 <- dbGetQuery(NARMS2015cleaned, "SELECT * FROM CleanedTable2")

DataSource <-  rep('NARMS2015',dim(PDFTables1)[1])
Year<-rep(2015,dim(PDFTables1)[1]); SerotypeName <- rep('Salmonella Typhimurium',dim(PDFTables1)[1])
PDFTables1 <- cbind(DataSource, Year, SerotypeName, PDFTables1)

PDFTables1 <- PDFTables1[,-c(4,5)]
PDFTables1 <- PDFTables1[-c(1,2),]

#rename the table from PDF
names(PDFTables1)<-Name
dim(PDFTables1)

## Concatenate two tables
Integration <- rbind(LabAgg, PDFTables1)
## Rename the schema for the entire table
names(Integration) <- c('DataSource','Year','SerotypeName','Agent','%I','%R','CI_L','CI_U',
                        '0.15', '0.03','0.06','0.125','0.25','0.5','1','2','4','8','16','32','64','128',
                        '256','512')

Integration

## Save it into database
setwd('C:/Users/pyh/Desktop/AMRDB/')
AMR_Integration <- dbConnect(RSQLite::SQLite(), dbname = "AMR_IntegrationTwoSource.db")
dbWriteTable(AMR_Integration, "Integration_SalmonellaTyphimurium", Integration)

## Data Analysis (Make a plot)

# Ampicillin, Ceftriaxone,
mydata <- Integration[(grepl("Ampicillin",Integration$Agent)),]
plot(mydata$Year, mydata$`%R`, type = 'l')
mydata <- Integration[(grepl("Ceftriaxone",Integration$Agent)),]

mydata <- Integration[(grepl("Gentamicin",Integration$Agent)),]



## Expected collection


