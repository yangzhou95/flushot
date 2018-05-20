
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
  #this gets the first 20 tweets of every user and checks if the previous user is as the same as the current one in the loop
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
#combining all 20 tweets of a user into one text by using aggregate function
#Above, I have considered first 20 tweets because Rstudio can not handle aggregate function with hundreds of tweets in rows
newanxiety1<- aggregate(Tweet ~ Username, data=newanxiety1, FUN=paste, collapse=" ")


#cleaning data 
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

#loading nrc emotions into 'sentiments' variable
sentiments<-as.data.frame(get_sentiments("nrc"))

#uniqsent<-unique(get_sentiments("nrc")$sentiment)
#sentimentdata<-cbind(uniqsent,rep(NA,length(uniqsent)))
#olnames(sentimentdata)<-c("Sentiment","Value")


#function would start here, for classifying all users

#get_classifier function returns either "anxious" or "not anxious" for all user
get_classifier<-function(x){
  #considering xth user's combined tweet text
sampletweet<-newanxiety1[x,2]

#tokenizing words and converting into a data frame
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

  #only considering the emotions and their count
sentvalues<-as.data.frame(table(tempdf))

#sumall<-sum(sentvalues$Freq)
#sort(sentvalues$Freq,decreasing = TRUE)

  #reorganizing in a descending order so that we can see if the first 2 rows have either 'fear' or 'sadness'
sentvalues<-sentvalues[order(-sentvalues$Freq),]
if(sentvalues[1,1]=="fear" || sentvalues[1,1]=="sadness")
{
  return("anxious") 
}else if(sentvalues[2,1]=="fear" || sentvalues[2,1]=="sadness")
  {
  return("anxious") 
} else{
  return("not anxious") 
}

}

#calling the function get_classifier
for(i in 1:nrow(newanxiety1)){
  finalclassifier[i,1]<-get_classifier(i)
}

#adding the data frame to the actual data set to represent if the person has anxiety or not
finalresult<-cbind(newanxiety1,finalclassifier)
write.csv(finalresult,file="Anxiety_Result.csv")






