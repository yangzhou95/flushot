require(ggplot2)
require(compare)
require(dplyr)

tweet<- read.csv("CommonTweetUser.csv")
tweet1<- tweet[tweet[,2]=="RikDaddy" , ]
time<-as.data.frame(substr(tweet1$Created.At,12,13))

freq<-as.data.frame(table(time))
colnames(freq)<-c("hour","numtweets")

freq$hour<-sub("^[0]+", "", freq$hour)

#adding Zeroeth hour because I have truncated the 0 in the above command
freq[1,1]<-0

# Consider another data frame to impute missing values in the user's tweet data frame
numbtweet<-rep(0,24)
tocomp<-data.frame(hour2=0:23,numbtweet)


difference<-as.data.frame(difference<-setdiff(tocomp$hour2,freq$hour))
difference1<-data.frame(difference,rep(0,nrow(difference)))
colnames(difference1)<-c("hour","numtweets")
union<-rbind(freq,difference1)
union$hour<-as.integer(union$hour)
union<-union[order(union$hour),]

#d<-density(x=freq$hour,y=freq$numtweets)
#plot(d)
ggplot(union,aes(x=hour,y=numtweets)) + geom_line()

