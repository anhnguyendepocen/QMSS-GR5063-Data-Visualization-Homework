#Replication Codes for Data Visualization HW3
#Chutian Zhou 3/29/2018

library(dplyr)
library(ggplot2)
library(tm)
library(qdap)
library(parallel)
library(pbapply)
library(tidytext)
library(wordcloud)
library(quanteda)
library(plotrix)
library(magrittr)
library(tidyverse)

#1. Identifying Successful Projects
#a) Success by Category

kickstarter<-read.csv("kickstarter_projects.csv",header=TRUE,sep=",")
kickstarter<-select(kickstarter,-source_url)%>%unique()

#generate achievement_ratio
kickstarter$achievement_ratio<-kickstarter$pledged/kickstarter$goal

#remove Inf
kickstarter2<-kickstarter[kickstarter$achievement_ratio!=Inf,]

#calculate mean achievement_ratio for each top category
ks<-kickstarter2%>%group_by(top_category)%>%summarise(mean=mean(achievement_ratio))

#remove the 16th row (NA)
ks<-ks[-16,]

#draw plot
ggplot(ks,aes(top_category,mean))+
  stat_summary(fun.y=mean,geom="bar")+coord_flip()+
  theme_classic()+
  scale_y_continuous(name="Achievement Ratio")+
  scale_x_discrete(name="Category")+
  ggtitle("Average Achievement Ratio for each Category")+
  theme(plot.title = element_text(hjust = 0.5))+coord_flip()
ggsave("Average Achievement Ratio for each Category.png")

#2. Writing Your Success Story

#1000 most successful
success<-kickstarter%>%arrange(desc(achievement_ratio))
success<-success[c(1:1000),c(2,9)]

colnames(success)[1]<-"text"
colnames(success)[2]<-"doc_id"

success$text<-as.character(success$text)
success$doc_id<-as.numeric(success$doc_id)

#1000 most unsucessful
unsuccess<-kickstarter%>%arrange(achievement_ratio)
unsuccess<-unsuccess[c(1:1000),c(2,9)]

colnames(unsuccess)[1]<-"text"
colnames(unsuccess)[2]<-"doc_id"

unsuccess$text<-as.character(unsuccess$text)
unsuccess$doc_id<-as.numeric(unsuccess$doc_id)

suc_source<-DataframeSource(success)
suc_corpus<-VCorpus(suc_source)

#cleaning function
removeNumPunct <- function(x){gsub("[^[:alpha:][:space:]]*", "", x)}
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(replace_symbol))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))  
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(removeNumPunct))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

#use clean_corpus function to clean suc_corpus
suc_corpus_clean<-clean_corpus(suc_corpus)

#stemming suc_corpus_clean
suc_stemmed<-tm_map(suc_corpus_clean,stemDocument)

#stemcompletion
stemCompletion2<-function(x, dictionary) {
  x<-unlist(strsplit(as.character(x), " "))
  x<-x[x != ""]
  x<-stemCompletion(x, dictionary=dictionary)
  x<-paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

#complete suc_stemmed
suc_comp_all<-mclapply(suc_stemmed,stemCompletion2, 
                     dictionary=suc_corpus_clean)

success[]<-lapply(success,as.character)

for (i in 1:dim(success)[1]){
  suc_comp_all[[i]]$meta$id<-success[i,"doc_id"]
}

suc_comp_all<-as.VCorpus(suc_comp_all)

#make a dtm
suc_dtm<-DocumentTermMatrix(suc_comp_all)

#convert suc_dtm to a matrix
suc_m<-as.matrix(suc_dtm)

#tidying suc_dtm
suc_td<-tidy(suc_dtm)

#calculating frequencies
suc_tf_idf<-suc_td%>%
  bind_tf_idf(term,document,count)%>%  
  arrange(desc(tf)) 

#draw wordcloud
purple_orange<-brewer.pal(10,"PuOr")
purple_orange<-purple_orange[-(1:2)]
set.seed(1)
wordcloud(suc_tf_idf$term,suc_tf_idf$tf, 
          max.words=500,colors=purple_orange)

#b) Success in Words
total<-rbind(success,unsuccess)

total_source<-DataframeSource(total)
total_corpus<-VCorpus(total_source)

#clean total_corpus
total_corpus_clean<-clean_corpus(total_corpus)

#stem total_corpus_clean
total_stemmed<-tm_map(total_corpus_clean,stemDocument)

#complete total_stemmed
total_comp_all<-mclapply(total_stemmed,stemCompletion2, 
                       dictionary=total_corpus_clean)

total[]<-lapply(total, as.character)

for (i in 1:dim(total)[1]){
  total_comp_all[[i]]$meta$id<-total[i,"doc_id"]
}

total_comp_all<-as.VCorpus(total_comp_all)

#make a tdm
total_tdm<-TermDocumentMatrix(total_comp_all)

#convert tdm to a matrix
total_m<-as.matrix(total_tdm)

#convert matrix to a df
total_df<-as.data.frame(total_m)

#generate suc_sum, which is the sum of word counts, as well as unsuc_sum
total_df$suc_sum<-rowSums(total_df[,1:1000])
total_df$unsuc_sum<-rowSums(total_df[,1001:2000])

#draw plot
common_words<-subset(total_df,total_df[,2001]>0&total_df[,2002]>0)
# Create difference
difference<-abs(common_words[,2001]-common_words[,2002])
# Combine common_words and difference
common_words<-cbind(common_words, difference)
# Subset common_words
common_words<-common_words[,2001:2003]
# Order the data frame from most differences to least
common_words<-common_words[order(common_words[, 3],decreasing=TRUE),]
# Create top25_df
top20_df<-data.frame(x=common_words[1:20,1], 
                     y=common_words[1:20,2], 
                     labels=rownames(common_words[1:20, ]))
# Create the pyramid plot
pyramid.plot(top20_df$x,top20_df$y,labels=top20_df$labels, 
                  gap=10,top.labels=c("Successful Projects", " ", "Unsuccessful Projects"), 
                  main="Words in Common",laxlab = NULL, 
                  raxlab=NULL,unit=NULL,labelcex=0.5)

#c) Simplicity as a virtue
kickstarter$blurb<-as.character(kickstarter$blurb)
fk<-textstat_readability(kickstarter$blurb, 
                                  measure=c('Flesch.Kincaid'))

#there is a mismatch...
fk$document<-as.numeric(gsub("text", "", fk$document))
kickstarter$document<-1:nrow(kickstarter)
kickstarter<-kickstarter%>%left_join(fk)

#draw plot
ggplot(kickstarter,aes(Flesch.Kincaid,achievement_ratio))+geom_point(alpha=0.5)+
  geom_smooth()+theme_classic()+
  xlab("Flesch-Kincaid Grade Level")+
  ylab("Achievement Ratio")+
  ggtitle("Correlation between the Readability Measure and Achievement Ratio")+
  theme(plot.title=element_text(hjust=0.5))
ggsave("Correlation between Readability Measure and AR.png")

#3. Sentiment
#a) Stay positive

#create subset, which has id and a_r
subset<-kickstarter[,c(9,23)]

#change "id" to "doc_id" 
colnames(subset)[1]<-"doc_id"

#change the format of doc_id in total
total$doc_id<-as.integer(total$doc_id)

#merge total and subset. Now we have text, doc_id and a_r
total2<-total%>%left_join(subset)

#remove duplicates
total2<-total2[!duplicated(total2$text),]

#get polarity score
sentiment<-total2%$%polarity(total2$text,doc_id)

#it's a "Large polarity". Now convert it into a data frame
sentiment2<-data.frame(doc_id=sentiment$all$doc_id,polarity=sentiment$all$polarity)

#merge total2 and sentiment2. Now we have a_r as well as polarity score
total3<-total2%>%left_join(sentiment2)

#draw plot
ggplot(total3,aes(polarity,achievement_ratio))+geom_point(alpha=0.5)+
  geom_smooth()+
  theme_classic()+ylab("Achievement Ratio")+xlab("Polarity Score")+
  ggtitle("Correlation between Tone of the Document and Success")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(caption="Note: The sample is confined to top 1000 and bottom 1000 most successful projects.")
ggsave("Correlation between Polarity and AR.png")

#b) Positive vs negative
#remove all obs that have 0s in polarity score
total4<-total3[apply(total3[c(2:4)],1,function(z)!any(z==0)),] 

#generate positive/negative
total4$set<-ifelse(total4$polarity>0,"positive","negative")

#subset total4 so that we only have text and set
total4<-total4[,c(1,5)]

#aggregate texts based on positive/negative
total4<-aggregate(total4$text, list(total4$set), paste, collapse="")

colnames(total4)[1]<-"doc_id"
colnames(total4)[2]<-"text"
total4$doc_id<-as.factor(total4$doc_id)

total4_source<-DataframeSource(total4)
total4_corpus<-VCorpus(total4_source)

#clean total4_corpus
total4_corpus_clean<-clean_corpus(total4_corpus)

#create a term_document matrix
total4_tdm<-TermDocumentMatrix(total4_corpus_clean)

#convert tdm to a matrix
total4_m<-as.matrix(total4_tdm)

comparison.cloud(total4_m, colors = c("orange", "blue"), 
                 scale=c(0.1,2), title.size= 1, 
                 max.words=150)

#c) Get in their mind

total2_clean<-total2 %>% 
  mutate(h_number=row_number())

total2_tidy<-total2_clean%>% 
  unnest_tokens(word,text)

total2_tidy<-total2_tidy %>% 
  anti_join(stop_words)

sentiment_nrc<-total2_tidy %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(h_number, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  setNames(c(names(.)[1],paste0('nrc_', names(.)[-1]))) %>%
  mutate(score_nrc = nrc_positive - nrc_negative) %>%
  ungroup()

total2_full<-full_join(total2_clean,sentiment_nrc)%>% 
  mutate_each(funs(replace(.,which(is.na(.)),0)),starts_with("score"),starts_with("nrc"))

ggplot(total2_full,aes(score_nrc,achievement_ratio))+geom_point(alpha=0.5)+
  xlab("NRC Score")+ylab("Achievement Ratio")+theme_classic()+
  ggtitle("Correlation between NRC Score and Success")+
  theme(plot.title = element_text(hjust = 0.5))+geom_smooth()
  