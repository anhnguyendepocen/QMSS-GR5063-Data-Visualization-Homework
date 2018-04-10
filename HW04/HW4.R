#Replication Codes for Data Visualization HW4
#Chutian Zhou 4/6/2018

library(igraph)
library(dplyr)
library(networkD3)
library(DT)
library(widgetframe)
library(ggnetwork)
library(ggplot2)
library(network)
library(ggrepel)
library(tidyr)
library(lubridate)
library(stringr)

#read file
follow<-read.csv("C:/Columbia/2018Spring/DataVisualization/HW4/senators_follow.csv")
twitter<-read.csv("C:/Columbia/2018Spring/DataVisualization/HW4/senators_twitter.csv")

#subset follow df
follow$following<-as.character(follow$following)
follow<-subset(follow,following=="TRUE")

#as_data_frame
follow<-dplyr::as_data_frame(follow)

#create a graph object
follow_gr<-graph_from_data_frame(follow[,c("source", "target")],directed=T)

#identify three senators who have highest in-degree and out-degree
indegree=igraph::degree(follow_gr,mode="in")
sort(indegree,decreasing=T)
outdegree=igraph::degree(follow_gr,mode="out")
sort(outdegree,decreasing=T)

#generate "size"
V(follow_gr)$size<-centralization.degree(follow_gr)$res

#simplify
E(follow_gr)$weight <- 1
follow_g<-simplify(follow_gr, edge.attr.comb="sum")

#fortify
set.seed(2103)
data<-ggnetwork(follow_g, layout="fruchtermanreingold", 
          arrow.gap=0, cell.jitter=0)

#let senators have their party affiliations
acc_aff<-twitter[,c("Official.Twitter","Party.affiliation")]
acc_aff$Official.Twitter<-as.character(acc_aff$Official.Twitter)
acc_aff$Party.affiliation<-as.character(acc_aff$Party.affiliation)

#edit problematic names and import party information
data$vertex.names<-as.character(data$vertex.names)
data$vertex.names<-replace(data$vertex.names,
                             data$vertex.names=="SenMarkey","senmarkey")
data$vertex.names<-replace(data$vertex.names,
                             data$vertex.names=="sendavidperdue","SenDavidPerdue")
data$x<-as.numeric(data$x)
data$y<-as.numeric(data$y)
data$xend<-as.numeric(data$xend)
data$yend<-as.numeric(data$yend)
data<-data%>%left_join(acc_aff,by = c("vertex.names"="Official.Twitter"))
data$Party.affiliation<-as.factor(data$Party.affiliation)

#draw graph
ggplot() +
  geom_edges(data=data, 
             aes(x=x, y=y, xend=xend, yend=yend),
             color="grey50", curvature=0.1, size=0.15, alpha=1/2)+
  geom_nodes(data=data,
             aes(x=x, y=y, xend=xend, yend=yend,color=Party.affiliation, 
                 size=sqrt(size)),
             alpha=1/3)+
  geom_label_repel(data=unique(data[data$size>120,c(1,2,5)]),
                   aes(x=x, y=y, label=vertex.names), 
                   size=2, color="#8856a7")+   
  theme_blank() +
  theme(legend.position="none")+
  ggtitle("Network of Followers")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_color_manual(breaks = c("Democratic Party", "Republican Party","Independent"),
                     values=c("blue","green","red"))
ggsave("Network of Followers.png")

#b) Communities
#find "communities"
wc2<-cluster_walktrap(follow_g)  
members2<-membership(wc2)

#Convert igraph object to object suitable for networkD3
follow_d3<-igraph_to_networkD3(follow_g,group=members2)

#draw graph
forceNetwork(Links=follow_d3$links,Nodes=follow_d3$nodes, 
                    Source='source', Target='target', 
                    NodeID='name', Group='group')

#2. What are they Tweeting about?
#a) Most Common Topics over Time
tweets<-readRDS("C:/Columbia/2018Spring/DataVisualization/HW4/senator_tweets.rds")
tweets<- dplyr::as_data_frame(tweets)

#retain obs that are not retweets
tweets_a<-tweets%>%filter(is_retweet=="FALSE")

#variable "hashtags" is a list
tweets_a$hashtags<-as.character(tweets_a$hashtags)

#retain hashtags column only
tweets_a<-tweets_a[,"hashtags"]

#calculate frequencies
tweets_a<-tweets_a%>%group_by(hashtags)%>%summarise(count=n())%>%arrange(desc(count))

#remove "NA"
tweets_a<-tweets_a[-1,]

#retain the first 10 rows
tweets_a<-tweets_a[1:10,]

#draw graph
ggplot(tweets_a,aes(hashtags,count))+
  stat_summary(fun.y=mean,geom="bar")+
  coord_flip()+
  theme_classic()+
  ggtitle("Most Frequently Used Hashtags among all Hashtags")+
  theme(plot.title=element_text(hjust=0.5))+
  ylab("Count")+xlab("#Hashtags")
ggsave("2a.png")

#b) Democrats vs. Republicans
#remain obs that are in year 2018
tweets_b<-tweets%>%filter(created_at>=as.Date("2018-01-01"))
tweets_b<-tweets_b[,c("screen_name","hashtags")]

#variable "hashtags" is a list
tweets_b$hashtags<-as.character(tweets_b$hashtags)

#senmarkey and sendavidperdue "capitalize problem"
tweets_b$screen_name<-replace(tweets_b$screen_name,
                              tweets_b$screen_name=="SenMarkey","senmarkey")
tweets_b$screen_name<-replace(tweets_b$screen_name,
                              tweets_b$screen_name=="sendavidperdue","SenDavidPerdue")

#import party affiliation data
tweets_b<-tweets_b%>%left_join(acc_aff,by=c("screen_name"="Official.Twitter"))

#dataset transformation
tweets_b<-tweets_b%>%group_by(hashtags,Party.affiliation)%>%
  summarise(count=n())%>%arrange(desc(count))

#remove NA rows
tweets_b<-tweets_b[-c(1,2,4),]

#retain top10 D and top10 R in terms of "count"
tweets_b<-tweets_b[c(1,2,3,6,7,9,10,11,15,16,4,5,8,12,13,14,18,21,22,29),]

#draw graph
ggplot(tweets_b,aes(hashtags,count))+
  stat_summary(fun.y=mean,geom="bar")+
  facet_wrap(~Party.affiliation,scales="free_x")+coord_flip()+
  theme_classic()+
  ggtitle("Different Topics Tweeted by Two Parties in 2018")+
  theme(plot.title=element_text(hjust=0.5))+
  ylab("Number of Appearances")+xlab("#Hashtags")
ggsave("2b.png")

#c) Gun Control I - Dems vs. Reps
tweets$hashtags<-as.character(tweets$hashtags)

#create target
target<-c("gunsafety","GunReform","Enough",
          "NRA","2ndamendment","NRAAM","2A")

#filter
tweets_c<-tweets%>%filter(hashtags%in%target)

#retain screen_name and hashtags only
tweets_c<-tweets_c[,c("screen_name","hashtags")]

#edit SenMarkey
tweets_c$screen_name<-replace(tweets_c$screen_name,
                              tweets_c$screen_name=="SenMarkey","senmarkey")

#import party affiliation data
tweets_c<-tweets_c%>%left_join(acc_aff,by=c("screen_name"="Official.Twitter"))

#data transformation
tweets_c_data<-tweets_c%>%
  group_by(hashtags,screen_name,Party.affiliation)%>%
  summarise(count=n())

#change column name of "Party.affiliation"
colnames(tweets_c_data)[3]<-"Party"

#draw graph
ggplot(tweets_c_data,aes(screen_name,count),group=Party)+
  stat_summary(fun.y=mean,geom="bar",aes(y=count,fill=Party))+
  facet_wrap(~hashtags)+
  theme_classic()+coord_flip()+
  ggtitle("Senators' Hashtag Choice")+
  theme(plot.title=element_text(hjust=0.5))+
  ylab("Count")+xlab("Official Twitter Account")+
  theme(axis.text.y=element_text(angle=0, hjust=1,size=3))+
  scale_fill_manual(breaks=c("Democratic Party", "Republican Party"),
                    values=c("blue","red"))
ggsave("2c.png")

#d) Gun Control II - Parkland Shooting
#read packages
library(tm)
library(wordcloud)
library(qdap)
library(tidytext)
library(parallel)

#filter for obs that only have "Parkland" in the "text" column
tweets_d<-tweets%>%filter(grepl('Parkland',text))
tweets_d<-tweets_d[,c("text","status_id")]
tweets_d$status_id<-as.numeric(tweets_d$status_id)
colnames(tweets_d)[2]<-"doc_id"

tweets_d_source<-DataframeSource(tweets_d)
tweets_d_corpus<-VCorpus(tweets_d_source)

#cleaning function
removeNumPunct<-function(x){gsub("[^[:alpha:][:space:]]*", "", x)}
clean_corpus<-function(corpus){
  corpus<-tm_map(corpus, removePunctuation)
  corpus<-tm_map(corpus, content_transformer(tolower))
  corpus<-tm_map(corpus, content_transformer(replace_symbol))
  corpus<-tm_map(corpus, removeWords, c(stopwords("en")))  
  corpus<-tm_map(corpus, removeNumbers)
  corpus<-tm_map(corpus, content_transformer(removeNumPunct))
  corpus<-tm_map(corpus, stripWhitespace)
  return(corpus)
}

#use clean_corpus function to clean tweets_d_corpus
tweets_d_corpus_clean<-clean_corpus(tweets_d_corpus)

#stemming tweets_d_corpus_clean
tweets_d_stemmed<-tm_map(tweets_d_corpus_clean,stemDocument)

#stemcompletion
stemCompletion2<-function(x,dictionary) {
  x<-unlist(strsplit(as.character(x), " "))
  x<-x[x != ""]
  x<-stemCompletion(x, dictionary=dictionary)
  x<-paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

#complete tweets_d_stemmed
tweets_d_comp_all<-mclapply(tweets_d_stemmed,stemCompletion2, 
                       dictionary=tweets_d_corpus_clean)

tweets_d[]<-lapply(tweets_d,as.character)

for (i in 1:dim(tweets_d)[1]){
  tweets_d_comp_all[[i]]$meta$id<-tweets_d[i,"doc_id"]
}

tweets_d_comp_all<-as.VCorpus(tweets_d_comp_all)

#make a dtm
tweets_d_dtm<-DocumentTermMatrix(tweets_d_comp_all)

#tidying tweets_d_dtm
tweets_d_td<-tidy(tweets_d_dtm)

#calculating frequencies
tweets_d_tf_idf<-tweets_d_td%>%
  bind_tf_idf(term,document,count)%>%  
  arrange(desc(tf)) 

#draw wordcloud
purple_orange<-brewer.pal(10,"PuOr")
purple_orange<-purple_orange[-(1:2)]
set.seed(1)
wordcloud(tweets_d_tf_idf$term,tweets_d_tf_idf$tf_idf, 
          max.words=500,colors=purple_orange)

#3. Are you talking to me?
#a) Identifying Re-Tweets
#retain obs that are only retweets
tweets_3a<-tweets%>%filter(is_retweet=="TRUE")

#subsetting
tweets_3a<-tweets_3a[,c("screen_name","text","mentions_screen_name")]

#unnesting
tweets_3a<-tweets_3a%>%unnest(mentions_screen_name)

#retain the first row of repetitive obs
tweets_3a<-tweets_3a[!duplicated(tweets_3a$text),]

#edit problematic names
tweets_3a$screen_name<-replace(tweets_3a$screen_name,
                               tweets_3a$screen_name=="SenMarkey","senmarkey")
tweets_3a$screen_name<-replace(tweets_3a$screen_name,
                             tweets_3a$screen_name=="sendavidperdue","SenDavidPerdue")
tweets_3a$mentions_screen_name<-replace(tweets_3a$mentions_screen_name,
                               tweets_3a$mentions_screen_name=="SenMarkey","senmarkey")
tweets_3a$mentions_screen_name<-replace(tweets_3a$mentions_screen_name,
                               tweets_3a$mentions_screen_name=="sendavidperdue","SenDavidPerdue")

#let sources and targets have their Party IDs
tweets_3a<-tweets_3a%>%
  inner_join(acc_aff,by=c("mentions_screen_name"="Official.Twitter"))
tweets_3a<-tweets_3a%>%
  left_join(acc_aff,by=c("screen_name"="Official.Twitter"))

#change column names
colnames(tweets_3a)[4]<-"Target.Party"
colnames(tweets_3a)[5]<-"Source.Party"

#screen_name should not be equal to mentions_screen_name
tweets_3a<-tweets_3a[which(tweets_3a[,1]!=tweets_3a[,3]),-2]

#manipulation
tweets_3a_m<-tweets_3a%>%group_by(mentions_screen_name,Source.Party)%>%summarise(count=n())

#import Party ID for mentions_screen_name
tweets_3a_m<-tweets_3a_m%>%
  left_join(acc_aff,by=c("mentions_screen_name"="Official.Twitter"))

#change column names
colnames(tweets_3a_m)[1]<-"Senator"
colnames(tweets_3a_m)[4]<-"Senator.Party"

ggplot(tweets_3a_m,aes(factor(Senator),count,fill=factor(Source.Party)))+
  geom_bar(stat="identity",position = "dodge")+coord_flip()+
  facet_wrap(~Senator.Party,scales="free_x")+
  theme_classic()+xlab("Senator's Official Twitter Account")+
  ylab("Number of Retweets Receives")+
  ggtitle("Retweets that Senators Receive")+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.text.y=element_text(angle=0, hjust=1,size=4))+
  scale_fill_manual(breaks = c("Democratic Party", "Republican Party","Independent"),
                     values=c("blue","green","red"))+
  guides(fill=guide_legend(title="Party ID of the Source"))
ggsave("3a.png")

#b) Identifying mentions
#filter for obs such that is_retweet==FALSE
tweets_3b<-tweets%>%filter(is_retweet=="FALSE")
tweets_3b$mentions_screen_name<-as.character(tweets_3b$mentions_screen_name)

#subsetting
tweets_3b<-tweets_3b[,c("screen_name","mentions_screen_name")]

#retain obs such that screen_name is a part of mentions_screen_name 
tweets_3b<-tweets_3b%>%filter(mentions_screen_name%in%screen_name)

#change column names
colnames(tweets_3b)[1]<-"source"
colnames(tweets_3b)[2]<-"target"

tweets_3b_gr<-graph_from_data_frame(tweets_3b[,c("source", "target")],directed=F)

V(tweets_3b_gr)$size<-centralization.degree(tweets_3b_gr)$res

#simplify
E(tweets_3b_gr)$weight <- 1
tweets_3b_g<-simplify(tweets_3b_gr, edge.attr.comb="sum")

#fortify
set.seed(2103)
data3b<-ggnetwork(tweets_3b_g, layout="fruchtermanreingold", 
                arrow.gap=0, cell.jitter=0)

#edit problematic names and import party information
data3b$vertex.names<-as.character(data3b$vertex.names)
data3b$vertex.names<-replace(data3b$vertex.names,
                             data3b$vertex.names=="SenMarkey","senmarkey")
data3b$vertex.names<-replace(data3b$vertex.names,
                             data3b$vertex.names=="sendavidperdue","SenDavidPerdue")
data3b$x<-as.numeric(data3b$x)
data3b$y<-as.numeric(data3b$y)
data3b$xend<-as.numeric(data3b$xend)
data3b$yend<-as.numeric(data3b$yend)
data3b<-data3b%>%left_join(acc_aff,by = c("vertex.names"="Official.Twitter"))
data3b$Party.affiliation<-as.factor(data3b$Party.affiliation)

#draw graph
ggplot() +
  geom_edges(data=data3b, 
             aes(x=x, y=y, xend=xend, yend=yend),
             color="grey50", curvature=0.1, size=0.15, alpha=1/2)+
  geom_nodes(data=data3b,
             aes(x=x, y=y, xend=xend, yend=yend,color=Party.affiliation,
                 size=sqrt(size)),
             alpha=1/3)+
  geom_label_repel(data=unique(data[data$size>120,c(1,2,5)]),
                   aes(x=x, y=y, label=vertex.names), 
                   size=2, color="#8856a7")+   
  theme_blank()+
  theme(legend.position="none")+
  ggtitle("Network of Mentions")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_color_manual(breaks = c("Democratic Party", "Republican Party","Independent"),
                       values=c("blue","green","red"))
ggsave("Network of Mentions.png")