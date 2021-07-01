library(stringr)
library(stringi)
#this has to be global somwhere:
idf = list()
recNo = 0 #total no of doc, (for idf)



#for each record:
recNo = recNo + 1 #counting no of doc
s = "I am using this with a Nook HD+. It works as described. The HD picture on my Samsung 52&#34; TV is excellent."
s = sapply(s,tolower)[[1]] #review text. all lowercase for comparison (convenient)
wordList = stri_extract_all_words(s)
wordList = wordList[[1]] #list of words in text
n = length(wordList) #total no of words in document (for tf)
tf = list()
for(w in wordList)
{
  if(is.null(tf[[w]]))
  {
    tf[[w]]=1/n
  }
  else
  {
    tf[[w]]=tf[[w]]+1/n
  }
}#calc tf
for(name in names(tf))
{
  if(is.null(idf[[name]]))
  {
    idf[[name]]=1
  }
  else
  {
    idf[[name]]=idf[[name]]+1
  }
}#var idf has count of doc.s that has each word



#somewhere after each record is processed
for(name in names(idf))
{
  idf[[name]] = log10(recNo/idf[[name]])
}#calc idf

