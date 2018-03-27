require("XML")
require("plyr")
require("ggplot2")
require("gridExtra")
require("CoreNLP")
require("stringr")
require("openNLP")
require("arules")
require("arulesViz")
require("NLP")
require("tm")
require("textstem")
require("qdap")#for polarity()
require("tidytext")
require("quanteda")
require("pracma")#for strcmp
require("mosaic")# for zscore
require("glm2")#for glm  training


require('e1071')
require('SparseM')
require('tm')


docxml=ldply(xmlToList("Restaurants_Train.xml"), data.frame)
sentence <- as.data.frame(unique(docxml$text))


#concatenate all strings to form 1 sentence
for(i in 1:3038)
  txt <- paste (txt, sentence$Text[i])

#POS tagging
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)}
tagged_str <- tagPOS(txt)

#filter nouns 
grep('NN', tagged_str$POStags)## used this to make vector v1

for(i in 1:9820)##used this to put those words in noun df
  noun <-paste(noun, pos_words[v1[i],])
noun <- as.data.frame(strsplit(noun," "))

#to remove /NN... from our df
noun$word<- as.character(noun$word)
for(i in 1:nrow(noun))
  noun$word[i]<- gsub("/+[[:alpha:]]*","",noun$word[i])

#to remove punct
for (i in 1:nrow(noun))
  noun$word[i]<- gsub("[[:punct:]]", "", noun$word[i])


j<- 3
#Assomatrix contruction      #use try and catch while contructing loops 

for( i in 1: nrow(Assomatrix))
{ k=2
while(isTRUE(grep(noun$word[j],Assomatrix$Text[i])==1))
{ 
  Assomatrix[i,k]<- noun$word[j]
  j=j+1
  k=k+1
}
}

# Remove NA from Assomatrix so no error later
Assomatrix[is.na(Assomatrix)] <- ""

####################################APPROACH # 2#############################################################################################################################
# trying again from assomatrix with less rows

matrix<- as.data.frame(Assomatrix[-c(1000:nrow(Assomatrix)),-c(18:ncol(Assomatrix))])

#has rows = unique aspects , col = potential no. of matches
newmattrix<- as.data.frame(matrix(0,nrow=2794,ncol=142))

#changing row and coloumn names of new matrix

for(i in 1:2794)
  + row.names(newmattrix)[i]<-noun$word[i]

for(i in 1:ncol(newmattrix))
  names(newmattrix)[i]<- i


#need to init this for pasting the sentence number at the correct position
k[1:nrow(newmattrix)]<-1
val<- 0
#to make matrix#2

for(i in 1:nrow(matrix))
  for(j in 2: ncol(matrix))
  {
    if (isTRUE(matrix[i,j]!= ""))
      
    { val <- which(rownames(newmattrix)==matrix[i,j])
    newmattrix[val,k[val]]<- i
    k[val]= k[val]+1
    }
  }



#########################################################APPROACH #1 #################################################################
######to make transaction - aspect dataframe #######

aspects<- as.data.frame(unique(noun))

aspects<- as.data.frame(lemmatize_words(stem_words(aspects$word))) 

transaction<- as.data.frame(t(aspects))

for(i in 1:nrow(Assomatrix))
  Assomatrix[i,]<- stem_strings(Assomatrix$Text[i])


#to change col names of transaction
for(i in 1:nrow(aspects))
  names(transaction)[i]<-paste(aspects$`lemmatize_words(stem_words(copy))`[i])



# to put 1,0 in sentence-aspect matrix
for(i in 1:nrow(tran))
  for(j in 1:ncol(tran))
  {
    if(isTRUE(grep(aspects$`lemmatize_words(stem_words(copy))`[j],lemmatize_strings(stem_strings(Assomatrix$Text)[i]))==1))
    { 
      tran[i,j]<- 1
    }
    else
      tran[i,j]<-0
    
  }
#reducin the size of transction data frame to process faster
tran_trim<- tran[-c(1001:3038),]

# create transact 0 filled data frame to make a new matrix

for(i in 1:ncol(tran))
{ k=2
for(j in 1:nrow(tran))
{ 
  if(tran[j,i]==1)
  {
    transact[j,k]<- j
    k=k+1
  }
}
}

### nake transpose of transact

tran_trim<- transact
transact<- t(transact)
transact<-data.frame(transact)

#######################################################################################
#difference b/w inbuilt coreNLP / qdap library and our custom built function is that when we make governor-dependent pairs , inbuilt is not accurate since it looks at the sentence as a whole and does not 
#check it aspect wise . eg polarity("food was good , but the ambience was poor","food"). 

#store the most frequent itemset which we founf out using apriori  http://michael.hahsler.net/research/arules_RUG_2015/demo/
#store all these pairs in df 
#seperate governor dependant pairs from getdependency() which contain aspects
#


#MATRIX #1
#  bread  butter milk coconut
#1   1     0      0     1
#2   1     1      1     0
#3   0     0      1     1

#MATRIX #2
#### bread 1,2
#### butter 4,7
### milk 3,5,6
####coconut 8,9

trans<- as(newmatrix,"transactions")

itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.03, minlen = 2, maxlen=4))

frequent<- as.data.frame(inspect(head(sort(itemsets), n=1000)))

frequent$items<-as.character(frequent$items)

# store result of GetDependency() for each annotatedstring(Assomatri$Text[i])
# only if either governor or dependent is a noun ,
#other should be in polarity
#save Governor, Dependent , Polarity in a data frame
#                                                  ^
#                                                  |
#                                                  |
#                                                  |
# To Do                                            |
#load polarity in data frame                       |
#make a new empty data frame for  ------------------


initCoreNLP() # need to initialize coreNLp for getDependency()

#rownames(lexicon) <- seq(length=nrow(lexicon))

'%ni%' <- Negate('%in%')  # notin custom function



for(k in 1:nrow(Assomatrix))
{ 
  temp<- as.data.frame(getDependency(annotateString(Assomatrix$Text[k])))#find GD Pairs of sentence k
  
  temp <- temp[which((temp$governor %in% noun$word & temp$dependent %in% lexicon$word) | (temp$dependent %in% noun$word & temp$governor %in% lexicon$word)),]
  
  #for(j in 1:nrow(tempdf))#############################<---Discard useless GOV-DEP Pairs--->#################
  #for(i in 1:nrow(noun))
  #{
  
  #  if(isTRUE(grep(tempdf$governor[j],noun$word[i])==1))
  #   { 
  # for(l in 1:nrow(lexicon))
  # {
  #    if(!isTRUE(grep(lexicon$word[l],tempdf$dependent[j])==1))
  #        {
  #         tempdf<-tempdf[-c(j),]
  #      }   
  # }
  #  if(which(tempdf$dependent[j]==lexicon$word) > 0)
  #               if(!isTRUE(which(tempdf$dependent[j]==lexicon$word)>0))
  #              tempdf<-tempdf[-c(j),]
  #       }
  #   else if(isTRUE(grep(tempdf$dependent[j],noun$word[i])==1))
  #       { 
  #             #if(which(tempdf$governor[j]==lexicon$word) > 0)
  #                if(!isTRUE(which(tempdf$governor[j]==lexicon$word)>0))
  #               tempdf<-tempdf[-c(j),]
  #   }
  #else if(isTRUE(which(tempdf$governor[j]==lexicon$word)>0))
  #   {
  #     if(tempdf$dependent %nin% noun$word)
  #      tempdf<-tempdf[-c(j),]
  #  }
  #    else if(isTRUE(which(tempdf$dependent[j]==lexicon$word)>0))
  #   {
  #    if(tempdf$governor %nin% noun$word)
  #      tempdf<-tempdf[-c(j),]
  #     }
  if(k==1)
    Dependency_final <- temp
  else
    Dependency_final<- rbind(Dependency_final,temp)
}







#
#
#have created tempdf for storing getdependency()
# & temp to compare the results of oother copies with this(removinf gov-dep pairs and will later copy to Dependency)
#& Dependency (adding selected rows directly to dependency)
#
#


#****************************************Trying to append directly to Dependency*******************************************************

#for(j in 1:nrow(tempdf))
#for(i in 1:nrow(noun))
##{
# if(isTRUE(grep(tempdf$governor[j],noun$word[i])==1))
# { 
#
#     if(isTRUE(which(tempdf$dependent[j]==lexicon$word)>0))
#  Dependency<-merge(Dependency,as.data.frame(tempdf[j,]))        
#   
#  }
# else if(isTRUE(grep(tempdf$dependent[j],noun$word[i])==1))
# { 
#
#     if(isTRUE(which(tempdf$governor[j]==lexicon$word)>0))
#     Dependency<-merge(Dependency,as.data.frame(tempdf[j,]))
# }
#}

#Negation final list
for(k in 1:nrow(Assomatrix))
{ 
  temp<- as.data.frame(getDependency(annotateString(Assomatrix$Text[k])))#find GD Pairs of sentence k
  Neg <- temp[which((temp$governor %in% lexicon$word & temp$dependent %in% Negation) | (temp$dependent %in% lexicon$word & temp$governor %in% Negation)),]
  if(k==1)
    Negated_final <- Neg
  else
    Negated_final<- rbind(Negated_final,Neg)
}


#load polarity in Dependency_final dataframe
lexicon$word<- as.character(lexicon$word)

for (i in 1: nrow(Dependency_final))
  Dependency_final$polarity[i]<- lexicon$value[match(Dependency_final[i,which(Dependency_final[i,] %in% lexicon$word)] , lexicon$word)]


#sep sentences to find context

for(i in 1:nrow(sentence))
{
temp_bind <- as.data.frame(unlist(strsplit(sentence$text[i], ",")))
if(i==1)
  sent_split <- temp_bind
else
  sent_split <- rbind(sent_split,temp_bind)
}                    

names(sent_split)[1]<- c("text")


#to find context and string
sent_split$noun <-0

#remove noun which are same as in lexicon
noun<- noun[-which(noun$word %in% lexicon$word),]

#remove stopwords
noun$word = removeWords(noun$word, stopwords("english"))


                                                                      #######Word n-grams Features######
#find context
for(i in 1:nrow(sent_split))
for(j in 1:nrow(noun))
{ 
  if(noun$word[j] %in% tolower(unlist(strsplit(sent_split$Text[i]," "))))
    sent_split$noun[i] <- c(noun$word[j])
  }

sent_split<- sent_split[which(sent_split$noun == 1 ),]
sent_split<- sent_split[-which(sent_split$noun==""),]

#to remove/NN ans so on ....
words$word <- as.character(words$word)
for(i in 1:nrow(words))
  words$word[i]<- gsub("/+[[:alpha:]]*","",words$word[i])

for(i in 1:nrow(words))
  + words$word[i]<- gsub("[[:punct:]]","",words$word[i])

##words<- words[-which(words$word == ""),] # not using this since we need to bigrams of 2 different sentences

#to make ngrams of 2 
library("quanteda")
words2 <- as.data.frame(sent_split$Text %>% 
  tokens(sent_split$Text, what = "sentence") %>% 
  as.character() %>%
  tokens(ngrams = 2, remove_punct = TRUE) %>%
  as.character())

names(words2)[1]<-c("word")


#to replace the " " for"_" 
for(i in 1:nrow(words2))
words2$bigrams[i]<- gsub("_"," ",words2$bigrams[i])


words<- rbind(words,words2)

                                                                        ######Z score Features######
master <- matrix(0, nrow=nrow(words), ncol=nrow(train500))


for(i in 1:nrow(train500))
{ 
  for(j in 1:1791)
  {
    master[j,i] <-  length(which(unlist(strsplit(tolower(train500$Text[i])," "))== words$word[j]))
  }
  for(j in 1792:nrow(words))
  {
    master[j,i] <- str_count(tolower(train500$Text[i]), words$word[j])
  }
 
}



#made head the subset of master since the data is too big
head<- as.data.frame(master[c(1:nrow(master)),c(1:10)])


master <- t(master)

head <- as.data.frame(master)

row.names(master)<-words$word

names(head)<-seq(length=nrow(train500))


for( i in 1:ncol(head))
zscore[i]<-sum(zscore(head[i,]))

#zscore is saved in master$zscore
for(i in 1: nrow(master))
 master$zscore[i]<- zscore[i]

#aspects are saved in master$aspect   #####MIGHT CAUSE PROBLEMS SINCE THIS IS TEXT IN NUMERICAL MATRIX
#for(i in 1: nrow(master))
# master$aspect[i]<- train500$noun[i]

 head <- as.data.frame(t(master))
 
 #main DataFrame is master

                                      ######Sentiment Lexicon-based Features######
 
 #Create a new df sentlex
 sentlex <- matrix(0,500,4)

 names(sentlex)<- c("+","-","+/-","last_polarity")
 
 
 #count +ve words using lexicon
 # for(i in 1:nrow(train500))
 # sentlex[i,1]<- length( which(lexicon$word %in% unlist(strsplit(tolower(train500$Text[i])," "))))
 for(i in 1:nrow(train500))
   sentlex[i,1]<-sum(lexicon$value[which(lexicon$word %in% unlist(strsplit(tolower(train500$Text[i])," ")))] > 0)
 #count -ve words using lexicon
 for(i in 1:nrow(train500))
   sentlex[i,2]<-sum(lexicon$value[which(lexicon$word %in% unlist(strsplit(tolower(train500$Text[i])," ")))] < 0)
 
 #work on master and then transform to head to view the data 
 #+/-
 for(i in 1:nrow(sentlex))
 {
   sentlex[i,3]<-sentlex[i,1]/sentlex[i,2]
   
   if(sentlex[i,3] == "NaN" |sentlex[i,3] == "Inf"  ) 
     sentlex[i,3]<-0
   
 }
 #polarity of the last word
 for(i in 1:nrow(sentlex))
 {  
   sentarr <- unlist(strsplit(tolower(train500$Text[i])," "))
   arr <- which(unlist(strsplit(tolower(train500$Text[i])," ")) %in% lexicon$word)
   last <- sentarr[arr[length(arr)]]
   if(!is.integer0(lexicon$value[which(lexicon$word %in% last)]))
     sentlex[i,4]<- lexicon$value[which(lexicon$word %in% last)]
   else 
     sentlex[i,4]<-0
   
 } 
 
 #polarity of each sentence
 for(i in 1:nrow(sentlex))
 {
   if(sentlex$`+`[i] - sentlex$`-`[i] >0)
     sentlex$polarity[i] <- 1
   else if((sentlex$`+`[i] - sentlex$`-`[i] < 0))
     sentlex$polarity[i]<- -1
 }
 
 #add findings of sentlex into master and then transform to head to view data
####################################### 
 for(i in 1:nrow(master))
   master$'+'[i] <- sentlex$`+`[i]
 
 for(i in 1:nrow(master))
   master$'-'[i] <- sentlex$`-`[i]
 
 for(i in 1:nrow(master))
   master$'+/-'[i] <- sentlex$`+/-`[i]
 
 for(i in 1:nrow(master))
   master$last_polarity[i] <- sentlex$last_polarity[i]
 
 for(i in 1:nrow(master))
   master$polarity[i]<- sentlex$polarity[i]
#############################################
#######################################################################NOT WORKING ################################################################################
 
 #http://blog.thedigitalgroup.com/rajendras/2015/05/28/supervised-learning-for-text-classification/
 
 traindata <- as.data.frame(train500[1:400,c(1,3)])
 testdata <- as.data.frame(train500[401:500,c(1,3)])

 # SEPARATE TEXT VECTOR TO CREATE Source(),
 # Corpus() CONSTRUCTOR FOR DOCUMENT TERM
 # MATRIX TAKES Source() 
 trainvector <- as.vector(traindata$Text)
  testvector <- as.vector(testdata$Text)
  
  
  # CREATE SOURCE FOR VECTORS
  trainsource <- VectorSource(trainvector)
  testsource <- VectorSource(testvector)
 
  # CREATE CORPUS FOR DATA
  traincorpus <- Corpus(trainsource)
  testcorpus <- Corpus(testsource)
  
  # PERFORMING THE VARIOUS TRANSFORMATION on "traincorpus" and "testcorpus" DATASETS #SUCH AS TRIM WHITESPACE, REMOVE PUNCTUATION, REMOVE STOPWORDS.
  traincorpus <- tm_map(traincorpus,stripWhitespace)
  traincorpus <- tm_map(traincorpus,tolower)
  traincorpus <- tm_map(traincorpus, removeWords,stopwords("english"))
  traincorpus<- tm_map(traincorpus,removePunctuation)
  
  testcorpus <- tm_map(testcorpus,stripWhitespace)
  testcorpus <- tm_map(testcorpus,tolower)
  testcorpus <- tm_map(testcorpus, removeWords,stopwords("english"))
  testcorpus<- tm_map(testcorpus,removePunctuation)
 
  # CREATE TERM DOCUMENT MATRIX
  trainmatrix <- t(TermDocumentMatrix(traincorpus))
  testmatrix <- t(TermDocumentMatrix(testcorpus))
  
  # TRAIN NAIVE BAYES MODEL USING trainmatrix DATA AND traindate$Journal_group CLASS VECTOR
  ###model <- naiveBayes(as.matrix(trainmatrix),as.factor(traindata$polarity))
  model2 <- glm(polarity~., data = traindata)
  # PREDICTION
  #results <- predict(model,as.matrix(testmatrix))
  log_predict <- predict(model2,newdata = testdata,type = "response")