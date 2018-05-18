require("plyr")
require("janeaustenr")
require("dplyr")
require("stringr")

nxiety<-read.csv("/Users/praneeththomas/Downloads/School Stuff/FluShot/untitled folder 2/Gender Classification/CommonTweetUser.csv")
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
  if(counter<2 && id==anxiety[i,1])
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

#Combines both the tweets of a user into a single rows for readability
anxiety1<-ddply(newanxiety,.(Username),summarize,
                  text=paste(Tweet,collapse = " ")) 

#omitting rows that dont have tweets
newdata<-dplyr::filter(anxiety1,  !is.na(text))
newdata<-na.omit(newdata)

unnest_tokens(newdata$description)


