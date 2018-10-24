library(e1071)
library(MASS)
library('tm')


folderdir="C:\\SAProject\\20news-19997\\20_newsgroups"
alldir=DirSource(folderdir, encoding = "UTF-8", recursive=TRUE)

#Read Corpus, and process content to Document Term Matrix
news <- Corpus(alldir, readerControl=list(reader=readPlain,language="en"))
news.p <- tm_map(news,tolower)



#generate DocumentTermMatrix, note its difference from TermDocumentMatrix
dtm <- DocumentTermMatrix(news.p, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                               stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                               bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
#inspect the resulted document term matrix
dim(dtm)
inspect(dtm)

memory.limit(size=58000)
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
#inspect most frequently occurring terms
top5000 <- names(freq[head(ord,n=5000)])
top10000<- names(freq[head(ord,n=10000)])
top5000
top10000
inspect(DocumentTermMatrix(news.p,list(dictionary=top5000)))
dtm5k <- (DocumentTermMatrix(news.p,list(dictionary=top5000)))
inspect(dtm5k)

inspect(DocumentTermMatrix(news.p,list(dictionary=top10000)))
dtm10k <- (DocumentTermMatrix(news.p,list(dictionary=top10000)))
inspect(dtm10k)
head(dtm5k)

# lets learn the news group label##


alt.atheism <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\alt.atheism", encoding = "UTF-8", recursive=TRUE),
                readerControl=list(reader=readPlain,language="en"))
comp.graphics <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\comp.graphics", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
comp.os.ms.windows.misc <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\comp.os.ms-windows.misc", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
comp.sys.ibm.pc.hardware <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\comp.sys.ibm.pc.hardware", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
comp.sys.mac.hardware <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\comp.sys.mac.hardware", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
comp.windows.x <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\comp.windows.x", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
misc.forsale <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\misc.forsale", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
rec.autos <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\rec.autos", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
rec.motorcycles <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\rec.motorcycles", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
rec.sport.baseball <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\rec.sport.baseball", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
rec.sport.hockey <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\rec.sport.hockey", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
sci.crypt <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\sci.crypt", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
sci.electronics <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\sci.electronics", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
sci.med <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\sci.med", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
sci.space <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\sci.space", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
soc.religion.christian <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\soc.religion.christian", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
talk.politics.guns <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\talk.politics.guns", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
talk.politics.mideast <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\talk.politics.mideast", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
talk.politics.misc <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\talk.politics.misc", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))
talk.religion.misc <- Corpus(DirSource("C:\\SAProject\\20news-19997\\20_newsgroups\\talk.religion.misc", encoding = "UTF-8", recursive=TRUE),
                      readerControl=list(reader=readPlain,language="en"))


dtm.alt.atheism <- DocumentTermMatrix(alt.atheism, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                               stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                               bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.alt.atheism))
nbdtm.alt.atheism <- length(counts)


dtm.comp.graphics <- DocumentTermMatrix(comp.graphics, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.comp.graphics))
nbdtm.comp.graphics <- length(counts)

dtm.comp.os.ms.windows.misc <- DocumentTermMatrix(comp.os.ms.windows.misc, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.comp.os.ms.windows.misc))
nbdtm.comp.os.ms.windows.misc <- length(counts)

dtm.comp.sys.ibm.pc.hardware <- DocumentTermMatrix(alt.atheism, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.comp.sys.ibm.pc.hardware))
nbdtm.comp.sys.ibm.pc.hardware <- length(counts)

dtm.comp.sys.mac.hardware <- DocumentTermMatrix(comp.sys.mac.hardware, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.comp.sys.mac.hardware))
nbdtm.comp.sys.mac.hardware <- length(counts)


dtm.comp.windows.x <- DocumentTermMatrix(comp.windows.x, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.comp.windows.x))
nbdtm.comp.windows.x <- length(counts)

dtm.misc.forsale <- DocumentTermMatrix(misc.forsale, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.misc.forsale))
nbdtm.misc.forsale <- length(counts)

dtm.rec.autos <- DocumentTermMatrix(rec.autos, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.rec.autos))
nbdtm.rec.autos <- length(counts)

dtm.rec.motorcycles <- DocumentTermMatrix(rec.motorcycles, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.rec.motorcycles))
nbdtm.rec.motorcycles <- length(counts)

dtm.rec.sport.baseball <- DocumentTermMatrix(rec.sport.baseball, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.rec.sport.baseball))
nbdtm.rec.sport.baseball <- length(counts)


dtm.rec.sport.hockey <- DocumentTermMatrix(rec.sport.hockey, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.rec.sport.hockey))
nbdtm.rec.sport.hockey <- length(counts)

dtm.sci.crypt <- DocumentTermMatrix(sci.crypt, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.sci.crypt))
nbdtm.sci.crypt <- length(counts)

dtm.sci.electronics <- DocumentTermMatrix(sci.electronics, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.sci.electronics))
nbdtm.sci.electronics <- length(counts)

dtm.sci.med <- DocumentTermMatrix(sci.med, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.sci.med))
nbdtm.sci.med <- length(counts)

dtm.sci.space <- DocumentTermMatrix(sci.space, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.sci.space))
nbdtm.sci.space <- length(counts)


dtm.soc.religion.christian <- DocumentTermMatrix(soc.religion.christian, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.soc.religion.christian))
nbdtm.soc.religion.christian <- length(counts)

dtm.talk.politics.guns <- DocumentTermMatrix(talk.politics.guns, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.talk.politics.guns))
nbdtm.talk.politics.guns <- length(counts)

dtm.talk.politics.mideast <- DocumentTermMatrix(talk.politics.mideast, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.talk.politics.mideast))
nbdtm.talk.politics.mideast <- length(counts)

dtm.talk.politics.misc <- DocumentTermMatrix(talk.politics.misc, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.talk.politics.misc))
nbdtm.talk.politics.misc <- length(counts)

dtm.talk.religion.misc <- DocumentTermMatrix(talk.religion.misc, control=list(removePunctuation=TRUE, tolower=TRUE, 
                                                                stopwords=c("english"), stripWhitespace=TRUE, wordLengths=c(3,15), 
                                                                bounds=list(global=c(5,Inf)), weighting=function(x) weightSMART(x,spec="ntc")))
counts <- colSums(as.matrix(dtm.talk.religion.misc))
nbdtm.talk.religion.misc <- length(counts)



dtm.sci.rel <- as.data.frame(dtm.alt.atheism)
rownames(dtm.sci.rel)<- 1:nrow(dtm.mx.sci.rel)

class <- c(rep("sci",591), rep("rel",377), rep("sci",393), rep("rel",251))
class.tr <- c(rep("sci",591), rep("rel",377))
class.ts <- c(rep("sci",393), rep("rel",251))
dtm.sci.rel <- cbind( dtm.sci.rel, class)



#classlabel=basename(dirname(filedir))
#classvec=c(classvec,classlabel)
#newsxx <- cbind(newsx,factor(classvec))
#classvec <- factor(classvec)
#head(classvec)


