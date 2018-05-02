require(ggplot2)
require(compare)
require(dplyr)

tweet<- read.csv("CommonTweetUser.csv")

x<-function(user_id,id_num){
print(user_id)
  print(id_num)
user<-user_id
tweet1<- tweet[tweet[,2]==user, ]
time<-as.data.frame(substr(tweet1$Created.At,12,13))

freq<-as.data.frame(table(time))
colnames(freq)<-c("hour","numtweets")


freq$hour<-as.numeric(freq$hour)
freq$numtweets<-as.numeric(freq$numtweets)
p<-ggplot(freq,aes(x=hour,y=numtweets))+geom_line()+ggtitle(user_id)
#plot_list[[id_num]]<-p
ggsave(p, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

unique<-as.data.frame(table(tweet[,2]))
unique<-unique[,1]

#plot_list<-list()
for(i in 1:length(unique)){
  q<-i
y<-unique[i]
x(y,q)
}

#pdf("plots.pdf")
#for(s in 1:length(unique)){
#  print(plot_list[[s]])
#}
#dev.off()

#x("seanagnew")

