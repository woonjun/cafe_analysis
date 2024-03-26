#이전 wordclud 분석을 진행하지 않았다면 KONLP 실행을 위해서 추가로 불러온 코드입니다.
install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary") 
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch")) 
library(KoNLP) 
extractNoun('이 문장에서 명사만 추출되었다면 성공입니다.') #테스트



install.packages("qgraph")
install.packages("tm")
install.packages("stringr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("tm")
install.packages("ggplot")
install.packages("wordcloud")
install.packages("stringr")

library(wordcloud)
library(ggplot2)
library(tm)
library(readxl)
library(multilinguer)
library(qgraph)
library(tm)
library(stringr)
library(ggplot2)

setwd("c:\\r_temp")
getwd( )

texts<-read_excel("네이버뉴스_카공족.xlsx")

texts

ko.words=function(texts){
  d=str_split(texts,";")
  extracted=tolower(str_match(d,'([가-힣a-zA-Z]+)/[NVO]'))
  keyword=extracted[,2]
  keyword[!is.na(keyword)]
}

cps=Corpus(VectorSource(texts$api_txt_lines))
tdm<-TermDocumentMatrix(cps,control=list(tokenize=ko.words,
                                         removePunctuation=T,
                                         removeNumbers=T,
                                         worldLengths=c(2,Inf)))

Encoding(tdm$dimnames$Terms) ="CP949"

dtm = as.DocumentTermMatrix(tdm)


freq=sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wf=data.frame(word=names(freq), freq=freq)

p=ggplot(subset(wf, freq>50), aes(word, freq))
p=p+geom_bar(stat="identity")
p=p+theme(axis.text.x = element_text(angle=90, hjust=1))
p
wordcloud(names(freq), freq, min.freq=10, color=brewer.pal(6, "Dark2"))

freq
#wordcloud용
#remove_words <- c("카페에서", "때문이다", "으로","따라","도를","되고","따르면")
#freq <- freq[!(names(freq) %in% remove_words)]
#freq <- freq[nchar(names(freq)) > 1]

tdm.matrix=as.matrix(tdm)
word.count=rowSums(tdm.matrix)
word.order=order(word.count,decreasing=T)


# 네트워크 분석에서 추가하고 싶은 단어를 추가한 코드입니다.
additional_words <- c("공부", "콘센트","전기","한잔","카공족","카공")
additional_indexes <- which(rownames(tdm.matrix) %in% additional_words)
additional_indexes
word.order <- c(word.order[1:50], additional_indexes)


freq.word <- tdm.matrix[word.order, ]
rownames(freq.word)
freq.word=tdm.matrix[word.order[1:55],]
rownames(tdm.matrix)[word.order[1:55]] 

word_lengths<-nchar(rownames(freq.word))
freq.word <- freq.word[word_lengths>1,]
freq.word

freq.word

### 네트워크 분석에서 필요없는 항목을 제거하는 코드입니다.
exclude_words <- c("카페에서", "카페","이른바","동안","일컫는","있다","때문에","위해","인한","있습니다","위한","최근","a씨는","대한")
exclude_words <- c("시간","일명","인해","들이","카페를","각종","공부를","가운데","때문이다","한다","커뮤니티에","있는","하는","지난","으로")
exclude_words <- c("오늘의","오늘은","오늘도","카페","오랜만에","어제")
freq.word <- freq.word[!rownames(freq.word) %in% exclude_words, ]
freq.word


co.matrix = freq.word %*% t(freq.word)
co.matrix
data<-co.matrix
data




qg <- qgraph(co.matrix,
             labels=rownames(co.matrix),
             diag=F,
             layout='spring',
             edge.color='black',
             vsize = log(diag(co.matrix))*1.5,
             labels.cex=10)

plot(qg)

