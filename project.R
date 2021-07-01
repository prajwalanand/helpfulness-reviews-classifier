library(rjson)
library(stringr)
library(NLP)
library(openNLP)
library(openNLPdata)
library(openNLPmodels.en)
library(magrittr)
library(qdap)
library(class)
library(e1071)
# tfidf, length, sents, words, exc, syll, stars, flesch, ari, helpful
yjjyt_train = file('train_final_final.json', 'r') # input file
rn = 0 # row number
while(TRUE)
{
  line = readLines(yjjyt_train, n=1) # read each review's details one by one
  if(length(line) == 0) # EOF
    break
  jsonline = fromJSON(paste(line, collapse="")) # convert string to JSON
  if((jsonline$helpful[2] > 0) && (jsonline$reviewText != "")) # in case of no data available about helpfulness
  {
    rn = rn + 1 # next row ( indexing starts from 1 )
    text = jsonline$reviewText # the actual review text
    bio = as.String(text)
    word_ann <- Maxent_Word_Token_Annotator() # separated sentences and words
    sent_ann <- Maxent_Sent_Token_Annotator()
    bio_annotations <- annotate(bio, list(sent_ann, word_ann)) # this includes cases like Mr. and Dr. and also multiple !!!s or ???s
    bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)
    vec_tr = c(character_count(bio)) # length of review
    vec_tr = cbind(vec_tr, length(sents(bio_doc))) # number of sentences in the review
    vec_tr = cbind(vec_tr, length(words(bio_doc))) # number of words in the review
    vec_tr = cbind(vec_tr, str_count(text, fixed('!')) + str_count(text, fixed('?'))) # number of !s and ?s in the review
    vec_tr = cbind(vec_tr, syllable_sum(bio)) # number of syllables in the review
    vec_tr = cbind(vec_tr, jsonline$overall)
    vec_tr = cbind(vec_tr, 206.835 - 1.015*vec_tr[3]/vec_tr[2] - 84.6*vec_tr[5]/vec_tr[3]) # Flesch Reading Ease Score - on a scale from 1 to 100, with lower numbers indicating a text that is more difficult to read
    vec_tr = cbind(vec_tr, 4.57*vec_tr[1]/vec_tr[3] + 0.5*vec_tr[3]/vec_tr[2] - 21.43) # Automated Readability Index - how many years of education are needed to understand the text
    vec_tr = cbind(vec_tr, if (jsonline$helpful[1]/jsonline$helpful[2]>=0.6) 1 else 0) # output variable; 1 indicares helpful and 0 indicates not helpful; helpfuless ratings >= 60% are considered as helpful
    train <- if (rn == 1) vec_tr else rbind(train, vec_tr) 
  }
  colnames(train) = c('length', 'sents', 'words', 'exc', 'syll', 'stars', 'flesch', 'ari', 'helpful')
}
close(yjjyt_train)
cn = 0 # row number
yjjyt_test = file('test_final_final.json', 'r')
while(TRUE)
{
  line = readLines(yjjyt_test, n=1) # read each review's details one by one
  if(length(line) == 0) # EOF
    break
  jsonline = fromJSON(paste(line, collapse="")) # convert string to JSON
  if((jsonline$helpful[2] > 0) && (jsonline$reviewText != "")) # in case of no data available about helpfulness
  {
    cn = cn + 1 # next row ( indexing starts from 1 )
    text = jsonline$reviewText # the actual review text
    bio = as.String(text)
    word_ann <- Maxent_Word_Token_Annotator() # separated sentences and words
    sent_ann <- Maxent_Sent_Token_Annotator()
    bio_annotations <- annotate(bio, list(sent_ann, word_ann)) # this includes cases like Mr. and Dr. and also multiple !!!s or ???s
    bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)
    vec_te = c(character_count(bio)) # length of review
    vec_te = cbind(vec_te, length(sents(bio_doc))) # number of sentences in the review
    vec_te = cbind(vec_te, length(words(bio_doc))) # number of words in the review
    vec_te = cbind(vec_te, str_count(text, fixed('!')) + str_count(text, fixed('?'))) # number of !s and ?s in the review
    vec_te = cbind(vec_te, syllable_sum(bio)) # number of syllables in the review
    vec_te = cbind(vec_te, jsonline$overall)
    vec_te = cbind(vec_te, 206.835 - 1.015*vec_te[3]/vec_te[2] - 84.6*vec_te[5]/vec_te[3]) # Flesch Reading Ease Score - on a scale from 1 to 100, with lower numbers indicating a text that is more difficult to read
    vec_te = cbind(vec_te, 4.57*vec_te[1]/vec_te[3] + 0.5*vec_te[3]/vec_te[2] - 21.43) # Automated Readability Index - how many years of education are needed to understand the text
    vec_te = cbind(vec_te, if (jsonline$helpful[1]/jsonline$helpful[2]>=0.6) 1 else 0) # output variable; 1 indicares helpful and 0 indicates not helpful; helpfuless ratings >= 60% are considered as helpful
    test <- if (cn == 1) vec_te else rbind(test, vec_te) 
  }
  colnames(test) = c('length', 'sents', 'words', 'exc', 'syll', 'stars', 'flesch', 'ari', 'helpful')
}
close(yjjyt_test)

#model = svm(helpful~., data = train, type='C-classification', kernel='rbf') # SVM classifier

train_2 = subset(train, select = -helpful) # not including the predicted variable
test_2 = subset(test, select = -helpful) # not including the predicted variable

#model = knn(train_2, test_2, cl=subset(train, select=helpful), k=5)
y = subset(test, select = helpful) # actual values

model = naiveBayes(train_2, subset(train, select=helpful), helpful~., test_2)

print(model)
predY = predict(model, newdata=test_2) # predicted values

#print(100 * sum(as.factor(y) == predY) / length(y)) # Accuracy

print(100 * sum(as.factor(y) == model) / length(y)) # Accuracy

