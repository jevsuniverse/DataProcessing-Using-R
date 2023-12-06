#
#
#
# PART ONE (1) 
#
#
# 

# import data 
Data1 <- read.csv("Participant.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors=TRUE, sep=",")

data1table<- table(Data1$Age)
datafreq <- as.vector(data1table)
datalabel<- names(data1table)


pie(datafreq, main="Pie Chart",col= rainbow(length(datalabel)), labels= piepercent)
piepercent<- round((datafreq/sum(datafreq))*100,1)
legend("topleft", datalabel, fill=rainbow(length(datalabel)), cex = 0.5)


barplot(datafreq, main ="Frequent Using Barplot", names.arg= (datalabel),ylim=c(1,10))

#
#
#
# PART TWO (2) 
#
#
#

#Import data from Pokemon.CSV
pokemon <- read.csv("Pokemon.csv", fileEncoding = "UTF-8-BOM", stringsAsFactor=TRUE, na.string = c(""), sep = ",")
pokemontype2 <- pokemon$Type.2

#change to char
pokemontype2 <- as.character(pokemontype2)

# check table whether it has NA or not (True False)
is.na(pokemontype2)

#Change the NA to the requested condition
pokemontype2[is.na(pokemontype2)] <- "-"
pokemontype2[is.na(pokemontype2)] <- c("person 1")

# make data frame from to column (pokemon$Type.1, pokemon$Type.2)
dataframe = data.frame(pokemon$Type.1, pokemon$Type.2)

#remove duplicate data
dataframe<- dataframe[!duplicated(dataframe),]

#delete all colum that has NA or missing value
na.omit(dataframe)

#make dataframe
dataframe <- dataframe[!is.na(dataframe$pokemon.Type.2),]

#select data with certain condition
datapokemon <- subset(pokemon, pokemon$Type.1=="Grass")
datapokemon <- subset(pokemon, pokemon$Type.1=="Grass",c("HP"))


#aggregate 
sumresult <- aggregate(pokemon$HP ~ pokemon$Type.1, pokemon, sum)
sumresult <- aggregate(pokemon$HP ~ pokemon$Type.1, pokemon, mean)

#summary 
summary(pokemon)

#
#
#
# PART THREE(3)
#
#
#

# import data from header, detail, and games
header<- read.csv("Header.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors = TRUE, na.string=c(""))
detail <- read.csv("Detail.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors = TRUE, na.string=c(""))
games <- read.csv("Games.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors = TRUE, na.string=c(""))

#Merge by Primary key 
transaction <- merge(header, detail, by ="TransactionId")
transaction1 <- merge(transaction, games,by = "GameId")

#Choose that have complete data only 
transaction1 <- transaction1[complete.cases(transaction1),]

# Choose 3 Data for analyze
transaction2<- transaction1[c("TransactionId", "Name", "Quantity")]

# change using as factor for each column
transid <- as.factor(transaction2$TransactionId)
transname <- as.factor(transaction2$Name)
transquan <- as.factor(transaction2$Quantity)

# Drop Level for each column
transid <- droplevels(transid)
transname<- droplevels(transname)
transquan<- droplevels(transquan)

library ("arules")

# split using transname and transid
transactiondata <-  split(transname, transid)

# transactions class
transactiondata <- as(transactiondata, "transactions")

#asc_rules
asc_rules <- apriori(transactiondata, parameter=list(support=0.005, target = "frequent itemset"))

#inspect 
inspect(asc_rules)
#growth
dataframe2 <- data.frame(transid, transname)

#check unique & length
unique = unique(dataframe2$transid)
length = length(dataframe2$transid)

# write to CSV
word = ""
for(id in unique){
  vec <- dataframe2[dataframe2$transid==id, c("transname")]
  word<- paste (word, paste(vec), collapse = ";")
  word<- paste(word, '\n', sep="")
}

write(word, file="fileoutput.csv", sep="")
