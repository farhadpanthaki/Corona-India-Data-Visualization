#Just run the file to get the daily value plot.
require(utils)
require(httr)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv(tf)

colnames(data)

length(unique(data[,7])) #the 7th column has the country names (196 countries)

country_list<-unique(data[,7])#as factors ( integers )
country_list<-levels(data[,7])#as characters


#filtering out india data

data_india <- data[data[,7]=="India",] # the cases column gives the cases identified on the date given in the entry

data_china<-data[data[,7]=="China",]
#to data is already sorted by dates

so the time of data entry corresponds to the negative of the entry number. so going in reverse order would correspond to the choronological occurance of the events

entries in chronological order
rownamez<-rownames(data_india)[length(rownames(data_india)):1]
rownamez_china<-rownames(data_china)[length(rownames(data_china)):1]


# 5th col is for the cases and 6th for the deaths


#ordered india data

ordered_india_data<-data_india[rownamez,]
ordered_china_data<-data_china[rownamez_china,]
##plotting the graph for the number of cases

plot.new()
#par(new=TRUE)
 
plot.window(xlim=c(0,length(rownamez)),ylim=c(min(ordered_india_data[,5]),max(ordered_india_data[,5])),asp = 0.0035,xaxs = "r" , yaxs = "r")

axis(1,at = c(0,length(rownamez)))
axis(2,at =c(min(ordered_india_data[,5]),max(ordered_india_data[,6]),max(ordered_india_data[,5])))

for(i in 1:length(rownamez))
	points(i-1,ordered_india_data[i,5],pch=19)

#plot(0:(i-1),ordered_india_data[1:i,5],pch=19,col="blue")

#plot(0:(i-1),ordered_india_data[1:i,5],pch=19,type="l",col="blue")



#plot.new()

 
#plot.window(xlim=c(0,length(rownamez)),ylim=c(min(ordered_india_data[,5]),max(ordered_india_data[,5])),asp = 0.4,xaxs = "r" , yaxs = "r")

#axis(1,at = c(0,length(rownamez)) )
#axis(2,at =c(min(ordered_india_data[,6]),max(ordered_india_data[,6])))

for(i in 1:length(rownamez))
	points(i-1,ordered_india_data[i,6],pch=19,col='red')

#plot(0:(i-1),ordered_india_data[1:i,6],pch=19,col="red")

#plot(0:(i-1),ordered_india_data[1:i,6],pch=19,type="l",col="red")
