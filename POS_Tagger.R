#Load packages
library(plyr)
library(gtools)
library(qdap)
library(tm)
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)
library(Rstem)
#Raw file containing reviews
Review_Raw_without_ID <- read.csv("ABC.csv", stringsAsFactors = F, header = T)
colnames(Reviews_Raw_without_ID)<- c("Reviews")
#Removing duplicate reviews from the file
Unique_Reviews_without_ID <- Reviews_Raw_without_ID[!duplicated(Reviews_Raw_without_ID[,1]),]
Unique_Reviews_without_ID<- as.data.frame(Unique_Reviews_without_ID)
#Creating unique ID for each review
Unique_Reviews_without_ID$ID<- seq.int(nrow(Unique_Reviews_without_ID))
Unique_Reviews_with_ID<- Unique_Reviews_without_ID
#Providing column names
colnames(Unique_Reviews_with_ID)<- c("Review","Review_id")
Unique_Reviews_with_ID<- Unique_Reviews_with_ID[,c(2,1)]
#Numbers removal#
Unique_Reviews_with_ID$Review<- tolower(Unique_Reviews_with_ID$Review)
Unique_Reviews_with_ID$Review <- tm::removeNumbers(Unique_Reviews_with_ID$Review)
library(stringr)
Unique_Reviews_with_ID<- subset(Unique_Reviews_with_ID,nchar(as.character(Unique_Reviews_with_ID$Review))>=10)  ####reviews with stringlength greater than 10######
Clean_Reviews_with_stopwords<- Unique_Reviews_with_ID
#POS Tagging#
library(openNLP)
library(NLP)
library(openNLPmodels.en)
test<- subset(Clean_Reviews_with_stopwords[1:50,])############taking part of data from review file##########
review_pos <- sapply(test$Review, function(ii)
{
  a2<- Annotation(1L, "sentence", 1L, nchar(ii))
  a2<- annotate(ii, Maxent_Word_Token_Annotator(),a2)
  a3<- annotate(ii,Maxent_POS_Tag_Annotator(),a2)
  a3w<- subset(a3, type=="word")
  tags<- sapply(a3w$features, `[[`,"POS")
  sprintf("%s/%s", as.String(ii)[a3w], tags)
})

pos_info<- as.matrix(review_pos)
pos_info_v2<- as.data.frame(pos_info)
library(data.table)
pos_info_v3<-setDT(pos_info_v2, keep.rownames = TRUE)[]
##Reshaping data
#Tag description file containing description for each tag
tag_description<- read.csv("tag-description.csv", header = T, stringsAsFactors = F)
pos_info_v6<- pos_info_v3
rm(pos_info_v3)
pos_info_v6$V2<- gsub("\\(|\\)||\"|","",pos_info_v6$V1)
library(stringi)
pos_info_v6$V2<- stri_sub(pos_info_v6$V2, 2)
#Splitting the tag from word
library(splitstackshape)
pos_info_v7<- cSplit(pos_info_v6,"V2",sep = ",")
pos_info_v8<- melt(pos_info_v7, id.vars = c("rn","V1"))
pos_info_v9<- cSplit(pos_info_v8,"value", sep = "/")
pos_info_v10<- merge(pos_info_v9[,c("rn","value_1","value_2")], tag_description[,c("Tag","Description")], by.x = "value_2", by.y = "Tag", all.x = T)
pos_info_v10$value_1<- as.character(pos_info_v10$value_1)
pos_info_v11<- aggregate(value_1~ rn+Description, data = pos_info_v10, paste, collapse=",")
#casting data to get each POS on columns
pos_info_v12<- dcast(pos_info_v11, rn ~ Description, value.var = "value_1")
pos_info_v13<- merge(pos_info_v12, Clean_Reviews_with_stopwords, by.x = "rn", by.y = "Review", all.x = T)
pos_info_v14<- pos_info_v13 %>% select(Review_id, everything())
noun<- pos_info_v10[grep("NN", pos_info_v10$value_2)]
noun$Description <- NULL
noun$value_2<- NULL
colnames(noun)<- c("Review","Noun")
adjective<- pos_info_v10[grep("JJ", pos_info_v10$value_2)]
adjective$Description <- NULL
adjective$value_2<- NULL
colnames(adjective)<- c("Review","Adjective")
verb<- pos_info_v10[grep("VB", pos_info_v10$value_2)]
verb$Description <- NULL
verb$value_2<- NULL
colnames(verb)<- c("Review","Verb")
Noun_v2<- merge(noun, Clean_Reviews_with_stopwords, by.x = "Review", by.y = "Review", all.x = T)
Adjective_v2<- merge(adjective, Clean_Reviews_with_stopwords, by.x = "Review", by.y = "Review", all.x = T)
Verb_V2<-merge(verb, Clean_Reviews_with_stopwords, by.x = "Review", by.y = "Review", all.x = T)
rm(noun,adjective,verb,pos_info)
library(dplyr)
#Most frequent nouns
freq_noun<- Noun_v2 %>%
  select(Noun)%>%
  group_by(Noun)%>%
  summarise(Frequency=length(Noun))


