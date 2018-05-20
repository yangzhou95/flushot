

require("plyr")
require("dplyr")
require("stringr")
require("tidytext")
require("stringr")
require("tokenizers")

anxiety<-read.csv("/Users/praneeththomas/Downloads/School Stuff/FluShot/untitled folder 2/Gender Classification/CommonTweetUser.csv")
#extract only columns username and tweet
anxiety<-anxiety[,c(2,3)]
colnames(anxiety)<-c("Username","Tweet")


#initialize the username id to the first username in the anxiety dataset
id<-anxiety[1,1]
counter<-0
newanxiety<-NULL

#iterating for all rows in the anxiety data set.
#This takes some time, about 10 mins, for obvious reasons that the dataset is way too big to run through
for(i in 1:nrow(anxiety)){
  #this gets the first 2 tweets of every user and checks if the previous user is as the same as the current one in the loop
  if(counter<20 && id==anxiety[i,1])
 {
    
    newanxiety<-rbind(newanxiety,anxiety[i,])
    counter<-counter+1
  }
  #changes the username and thereby the 'id' to the next one in the tweetdata set
  else{
    
    if(id!=anxiety[i,1]){
      id<-anxiety[i,1]
      counter<-0
    }
  }
  
}

# note to self: DONT MANIPULATE newanxiety 
row.has.na <- apply(newanxiety, 1, function(x){any(is.na(x))})
newanxiety1 <- newanxiety[!row.has.na,]
newanxiety1<- aggregate(Tweet ~ Username, data=newanxiety1, FUN=paste, collapse=" ")

cleaneddata<- function(data){
  data<-gsub("\\W",' ',data )
  data <- trimws(data)
  data <- gsub("\\s+", " ", data)
  data <-gsub("/", " ", data)
  data <- gsub("@", " ", data)
  data <- gsub("\\|", " ", data)
  data <- tolower(data)
  data<-gsub('[0-9]+', '', data)
  #if(str_detect(tokenize_words(data),paste0("s","$")))
  data<-gsub('s$*', '', data)
  return(data)
}
newanxiety1$Tweet<-cleaneddata(newanxiety1$Tweet)

sentiments<-as.data.frame(get_sentiments("nrc"))

uniqsent<-unique(get_sentiments("nrc")$sentiment)
sentimentdata<-cbind(uniqsent,rep(NA,length(uniqsent)))
colnames(sentimentdata)<-c("Sentiment","Value")

sampletweet<-newanxiety1[1,2]


words<-as.data.frame(tokenize_words(sampletweet))
#words1<-words
tempdf<-NULL
tempdf<-as.data.frame(tempdf)
colnames(words)<-"Words"
words<-na.omit(words)
for( i in 1:nrow(words))
{
  for( j in 1:nrow(sentiments))
  {
    if(words[i,1]==sentiments[j,1])
    {
      #words1[i,1]<-sentiments[j,2]
      print(i)
      print(sentiments[j,2])
      tempdf[i,1]<-sentiments[j,2]
      
    }
    else
    {
      words1[i,1]<-NA
    }
  }
  
}



