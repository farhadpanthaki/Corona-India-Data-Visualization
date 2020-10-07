data<-read.csv('download.csv')
### to get the country listed in the data

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

############################################################################################################################################################################
Prediction.


#1
running_total<-rep(0,nrow(ordered_india_data))
for( i in 1:nrow(ordered_india_data))
running_total[i]<-sum(ordered_india_data[(1:i),5])

#2
running_change<-rep(0,(length(running_total)-1))
for(i in 1:(length(running_total)-1))
	running_change[i]<-running_total[i+1]-running_total[i]

#need to give more importance to the recent times as the number of cases is higer now as compared to when corona started spreading
#a ratio of the number of cases in a particular day to the total cases is one possible measure of importance for that day.
#so that importance measure multiplited by the change is used to find the average change that would occur on the "next" day

#3

running_importance<-ordered_india_data[,5]/running_total[length(running_total)]

#sum(running_importance) is one

so now each day has an importance that is proportional to the number of cases in that day ie a measure of how much the disease has spread in that day

average_change<-sum(running_importance[2:length(running_importance)]*running_change)


##Assuming a linear change with the rate of change being the calculated average_change

i<- 10  		#number of days we need to predict from today

average_change * i + running_total[length(running_total)]


## for exponential change we need to consider the change in the average change as we go forward and we can assume that to be the constant


running_change_2<-rep(0,(length(running_total)-2))

for(i in 1:length(running_change_2))
		running_change_2[i]<-running_change[i+1]-running_change[i]

sum(running_change_2)

#this running_change decreased or increased on a particular day so it was positive or negative

#if we assume that the running change change that happened over the total number of days to be distributed equally we can divide the above sum by the number of days to get a very conservative change in the change per day

#if we incorporate the running_importance we get the average change that will be higher then

average_change_in_change<-sum(running_change_2 * running_importance[3:length(running_importance)])


pred_total<-rep(0,1)
i<-7
pred_total<- i*(average_change + (i * average_change_in_change)) + running_total[length(running_total)]

#for(j in 0:i){
##pred_change<-average_change + (j * average_change_in_change)
#pred_total<-running_total[length(running_total)] + pred_change 

paste("Total cases after",i,"days","are",ceiling(pred_total)
