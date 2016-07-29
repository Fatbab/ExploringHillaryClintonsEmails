## Explorative Analysis of Hillary Cinton's Emails
## Emails downloaded from Kaggle: https://www.kaggle.com/kaggle/hillary-clinton-emails

####################
#### CLUSTERING ####
####################

require(quanteda)
library(RSQLite)

# Connect to local sqlite db
# This maybe only on my machine but due to credential issues, R could only access sqlite from /tmp/ folder 
 
db <- dbConnect(dbDriver("SQLite"), "/tmp/database.sqlite")
query1 <- dbGetQuery(db, "SELECT 'To: ' || MetaDataTo || ' [' || SUBSTR(MetadataDateSent,1,13) || ']' AS 'TextName', 
    MetaDataTo AS 'To', SUBSTR(MetadataDateSent, 1,10) AS 'Date', 
                     'Subject: ' || ExtractedSubject || ' - Body: ' || ExtractedBodyText AS 'Subject_Body'
                     FROM Emails e INNER JOIN Persons p ON e.SenderPersonId=P.Id WHERE p.Name='Hillary Clinton'  
                     AND e.ExtractedBodyText != '' group by TextName")

query1df <- as.data.frame(query1)
HillEmailCorpus <- corpus(query1df, textField='Subject_Body')
docnames(HillEmailCorpus) <- query1df$TextName 
#summary(query1df)
#summary(HillEmailCorpus)

HillEmailDFM <- dfm(HillEmailCorpus, ignoredFeatures = stopwords("english"), stem=TRUE)
HillEmailDFM <- trim(HillEmailDFM, minCount= 5, minDoc = 5)
HillEmailMat <- dist(as.matrix(weight(HillEmailDFM, "relFreq")))
HillEmailCluster <- hclust(HillEmailMat)
plot(HillEmailCluster)

####  Dendrogram for emails grouped by Subject 
# This query joins email by subject and appends the subject line to the email body
query2 <- dbGetQuery(db, "SELECT Subject As 'Subject', Subject || GROUP_CONCAT(EBody, '  ++  ') As 'Subject_CombEBody' 
    FROM (SELECT 'in: ' || MetadataSubject AS 'Subject', CAST(ExtractedBodyText AS VARCHAR) As 'EBody' FROM Emails e 
    INNER JOIN Persons p ON e.SenderPersonId=p.Id WHERE p.Name='Hillary Clinton'  AND
    MetadataSubject != 'NA') 
    GROUP BY Subject")

query2df <- as.data.frame(query2)
EmailBySubjectCorpus <- corpus(query2df, textField='Subject_CombEBody')
docnames(EmailBySubjectCorpus) <- query2df$Subject

EmailBySubjectDFM <- dfm(EmailBySubjectCorpus, ignoredFeatures = stopwords("english"), stem=TRUE)
EmailBySubjectDFM <- trim(EmailBySubjectDFM,  minCount= 5, minDoc = 5)
# Removing some combination of letters that 'by experience' appear frequntly with little added information
EmailBySubjectDFM <- removeFeatures(EmailBySubjectDFM, "will")
EmailBySubjectDFM <- removeFeatures(EmailBySubjectDFM, "pm")
EmailBySubjectDFM <- removeFeatures(EmailBySubjectDFM, "u.")
EmailBySubjectDFM <- removeFeatures(EmailBySubjectDFM, "re")
EmailBySubjectDFM <- removeFeatures(EmailBySubjectDFM, "h")

######################
##### Word Cloud #####
######################

plot(EmailBySubjectDFM, min.freq = 6, random.order = FALSE) 
plot(EmailBySubjectDFM, min.freq = 20, random.order = FALSE) 
plot(EmailBySubjectDFM, min.freq = 100, random.order = FALSE) 


######################
##### Dendrogram #####
######################
# The emails dendrogram is very huge so this line creates a custom pdf file
pdf("~/Full200.pdf", width=30, height=200)
plot(EmailBySubjectDendro, horiz =TRUE,main ="Full Dendrogram Tree")
dev.off()

######################
##### INTERVIEWS #####
######################
## Save a local copy of interview transcripts from: http://www.state.gov/r/pa/ei/speeches/

interviewsFile <- textfile("~/PATH/TO/INTERVIEW/FILES.txt")
interviewsCorpus = corpus(interviewsFile)
#summary(corpus(interviewsFile))

# Use moral foundation free dictionary for sentiment analysis
# This code works although results from it were later removed from this work 
##########
#mfdict <- dictionary(file = "http://www.moralfoundations.org/sites/default/files/files/downloads/moral%20foundations%20dictionary.dic", 
#                     format = "LIWC")

#interviewsDFM <- dfm(interviewsCorpus, dictionary = mfdict)

## HarmVirtue
#interviewsDFM[, "HarmVirtue"] / sum(interviewsDFM[, "HarmVirtue"])
#interviewsDFM[, "HarmVirtue"] / sum(interviewsDFM[,])

## HarmVice
#interviewsDFM[, "HarmVice"] / sum(interviewsDFM[, "HarmVice"])
#interviewsDFM[, "HarmVice"] / sum(interviewsDFM[,])

## FairnessVirtue
#interviewsDFM[, "FairnessVirtue"] / sum(interviewsDFM[, "FairnessVirtue"])
#interviewsDFM[, "FairnessVirtue"] / sum(interviewsDFM[,])

## FairnessVice
#interviewsDFM[, "FairnessVice"] / sum(interviewsDFM[, "FairnessVice"])
#interviewsDFM[, "FairnessVice"] / sum(interviewsDFM[,])

## AuthorityVirtue
#interviewsDFM[, "AuthorityVirtue"] / sum(interviewsDFM[, "AuthorityVirtue"])
#interviewsDFM[, "AuthorityVirtue"] / sum(interviewsDFM[,])

## AuthorityVice
#interviewsDFM[, "AuthorityVice"] / sum(interviewsDFM[, "AuthorityVice"])
#interviewsDFM[, "AuthorityVice"] / sum(interviewsDFM[,])
##########

############################################
##### Dotchart -- Interview Similarity #####
############################################
t1 <- Sys.time()
interviewsMat <- dist(as.matrix(weight(interviewsDFM, "relFreq")))  #Can take long
t2 <- Sys.time()
t2 - t1 

t3 <- Sys.time()
interviewsCluster <- hclust(interviewsMat) 
t4 <- Sys.time()
t4 - t3 

plot(interviewsCluster, xlab = "", sub = "", main = "Select Interviews as Seceraty of States")


interviews2Dfm <- dfm(interviewsCorpus, 
               ignoredFeatures = stopwords("english"),
               stem=TRUE, verbose=FALSE)
interviewSimil <- similarity(interviews2Dfm, c("NPR-22Jul2009.txt" , "NPR-29Jan2013.txt"), n = NULL, 
                         margin = "documents", method = "cosine", normalize = FALSE)
dotchart(interviewSimil$`NPR-22Jul2009.txt`, xlab = "Cosine Similarity", main = "Baseline: NPR-22Jul2009")

###### Word Cloud for baseline, lease similar and most similar interviews
###### baseline: NPR-22Jul2009, least similar: NPR-30Jun2012, most similar: ABC-29Jan2013

## baseline: NPR-22Jul2009
npr2009file <- textfile("~/MachineLearning/CourseWork3/Data/Interviews/NPR-22Jul2009.txt", 
                        cache = FALSE)
npr2009Corpus <- corpus(npr2009file)
npr2009dfm <- dfm(npr2009Corpus, ignoredFeatures = stopwords("english"), stem=TRUE)
npr2009dfm <- removeFeatures(npr2009dfm, "question")
npr2009dfm <- removeFeatures(npr2009dfm, "secretari")
npr2009dfm <- removeFeatures(npr2009dfm, "clinton")


## least similar: NPR-30Jun2012
npr2012file <- textfile("~/MachineLearning/CourseWork3/Data/Interviews/NPR-30Jun2012.txt", 
                        cache = FALSE)
npr2012Corpus <- corpus(npr2012file)
npr2012dfm <- dfm(npr2012Corpus, ignoredFeatures = stopwords("english"), stem=TRUE)
npr2012dfm <- removeFeatures(npr2012dfm, "question")
npr2012dfm <- removeFeatures(npr2012dfm, "secretari")
npr2012dfm <- removeFeatures(npr2012dfm, "clinton")


## most similar: ABC-29Jan2013
abc2013file <- textfile("~/MachineLearning/CourseWork3/Data/Interviews/ABC-29Jan2013.txt", 
                        cache = FALSE)
abc2013Corpus <- corpus(abc2013file)
abc2013dfm <- dfm(abc2013Corpus, ignoredFeatures = stopwords("english"), stem=TRUE)
abc2013dfm <- removeFeatures(abc2013dfm, "question")
abc2013dfm <- removeFeatures(abc2013dfm, "secretari")
abc2013dfm <- removeFeatures(abc2013dfm, "clinton")


### put three word clouds togther
par(mfrow=c(1,3))
plot(npr2009dfm,random.order = FALSE, main ="NPR-22Jul2009") 
plot(npr2012dfm,random.order = FALSE, main ="NPR-30Jun2012") 
plot(abc2013dfm,random.order = FALSE, main ="ABC-29Jan2013") 

####################
##### SENTIMENT ####
####################

library("syuzhet")
library("rJava") 
library("gplots")

###2009 NPR IRAN ####
nprfile09 <- textfile("~/PATH/TO/INTERVIEW/NPR-22Jul2009.txt", 
                    cache = FALSE)
nprCorpus09 <- corpus(nprfile09)
nprdfm09 <- dfm(nprCorpus09, ignoredFeatures = stopwords("english"), stem=TRUE)
nprdfm09 <- removeFeatures(nprdfm09, "question")
nprdfm09 <- removeFeatures(nprdfm09, "secretari")
nprdfm09 <- removeFeatures(nprdfm09, "clinton")
temp09 = kwic(nprCorpus09, "iran*", window=20)
nprtxt09 = paste(temp09$contextPre, temp09$keyword, temp09$contextPost, sep=" ")

###2010 CNN IRAN ####
cnnfile10 <- textfile("~/PATH/TO/INTERVIEW/CNN-12Mar2010.txt", 
                      cache = FALSE)
cnnCorpus10 <- corpus(cnnfile10)
cnndfm10 <- dfm(cnnCorpus10, ignoredFeatures = stopwords("english"), stem=TRUE)
cnndfm10 <- removeFeatures(cnndfm10, "question")
cnndfm10 <- removeFeatures(cnndfm10, "secretari")
cnndfm10 <- removeFeatures(cnndfm10, "clinton")
temp10 = kwic(cnnCorpus10, "iran*", window=20)
cnntxt10 = paste(temp10$contextPre, temp10$keyword, temp10$contextPost, sep=" ")


###2011 ABC+CNN IRAN####
abcfile11 <- textfile("~/PATH/TO/INTERVIEW/ABC-18Nov2011.txt", 
                      cache = FALSE)
abcCorpus11 <- corpus(abcfile11)
abcdfm11 <- dfm(abcCorpus11, ignoredFeatures = stopwords("english"), stem=TRUE)
abcdfm11 <- removeFeatures(abcdfm11, "question")
abcdfm11 <- removeFeatures(abcdfm11, "secretari")
abcdfm11 <- removeFeatures(abcdfm11, "clinton")
temp111 = kwic(abcCorpus11, "iran*", window=20)
abctxt11 = paste(temp111$contextPre, temp111$keyword, temp111$contextPost, sep=" ")

cnnfile11 <- textfile("~/PATH/TO/INTERVIEW/CNN-02Dec2011.txt", 
                      cache = FALSE)
cnnCorpus11 <- corpus(cnnfile11)
cnndfm11 <- dfm(cnnCorpus11, ignoredFeatures = stopwords("english"), stem=TRUE)
cnndfm11 <- removeFeatures(cnndfm11, "question")
cnndfm11 <- removeFeatures(cnndfm11, "secretari")
cnndfm11 <- removeFeatures(cnndfm11, "clinton")
temp112 = kwic(cnnCorpus11, "iran*", window=20)
cnntxt11 = paste(temp112$contextPre, temp112$keyword, temp112$contextPost, sep=" ")


###2012 CNN IRAN####
cnnfile12 <- textfile("~/PATH/TO/INTERVIEW/CNN-19Apr2012.txt", 
                      cache = FALSE)
cnnCorpus12 <- corpus(cnnfile12)
cnndfm12 <- dfm(cnnCorpus12, ignoredFeatures = stopwords("english"), stem=TRUE)
cnndfm12 <- removeFeatures(cnndfm12, "question")
cnndfm12 <- removeFeatures(cnndfm12, "secretari")
cnndfm12 <- removeFeatures(cnndfm12, "clinton")
temp12 = kwic(cnnCorpus12, "iran*", window=20)
cnntxt12 = paste(temp12$contextPre, temp12$keyword, temp12$contextPost, sep=" ")


###2013 ALL IRAN ####
nprfile <- textfile("~/PATH/TO/INTERVIEW/NPR-29Jan2013.txt", 
                        cache = FALSE)
cnnfile <- textfile("~/PATH/TO/INTERVIEW/CNN-29Jan2013.txt", 
                        cache = FALSE)
abcfile <- textfile("~/PATH/TO/INTERVIEW/ABC-29Jan2013.txt", 
                        cache = FALSE)
foxfile <- textfile("~/PATH/TO/INTERVIEW/FOX-29Jan2013.txt", 
                        cache = FALSE)
nprCorpus <- corpus(nprfile)
cnnCorpus <- corpus(cnnfile)
abcCorpus <- corpus(abcfile)
foxCorpus <- corpus(foxfile)

## Remove redundant words
nprdfm <- dfm(nprCorpus, ignoredFeatures = stopwords("english"), stem=TRUE)
nprdfm <- removeFeatures(nprdfm, "question")
nprdfm <- removeFeatures(nprdfm, "secretari")
nprdfm <- removeFeatures(nprdfm, "clinton")
temp1 = kwic(nprCorpus, "iran*", window=20)
temp2 = kwic(nprCorpus, "ahmadinejad*", window=20)
temp3 = kwic(nprCorpus, "tehran*", window=20)
nprtxt = paste(temp1$contextPre, temp1$keyword, temp1$contextPost, sep=" ")

cnndfm <- dfm(cnnCorpus, ignoredFeatures = stopwords("english"), stem=TRUE)
cnndfm <- removeFeatures(cnndfm, "question")
cnndfm <- removeFeatures(cnndfm, "secretari")
cnndfm <- removeFeatures(cnndfm, "clinton")
temp1 = kwic(cnnCorpus, "iran*", window=20)
temp2 = kwic(cnnCorpus, "ahmadinejad*", window=20)
temp3 = kwic(cnnCorpus, "tehran*", window=20)
cnntxt = paste(temp1$contextPre, temp1$keyword, temp1$contextPost, sep=" ")

foxdfm <- dfm(foxCorpus, ignoredFeatures = stopwords("english"), stem=TRUE)
foxdfm <- removeFeatures(foxdfm, "question")
foxdfm <- removeFeatures(foxdfm, "secretari")
foxdfm <- removeFeatures(foxdfm, "clinton")
temp1 = kwic(foxCorpus, "iran*", window=20)
temp2 = kwic(foxCorpus, "ahmadinejad*", window=20)
temp3 = kwic(foxCorpus, "tehran*", window=20)
foxtxt = paste(temp1$contextPre, temp1$keyword, temp1$contextPost, sep=" ")

abcdfm <- dfm(abcCorpus, ignoredFeatures = stopwords("english"), stem=TRUE)
abcdfm <- removeFeatures(abcdfm, "question")
abcdfm <- removeFeatures(abcdfm, "secretari")
abcdfm <- removeFeatures(abcdfm, "clinton")
temp1 = kwic(abcCorpus, "iran*", window=20)
temp2 = kwic(abcCorpus, "ahmadinejad*", window=20)
temp3 = kwic(abcCorpus, "tehran*", window=20)

iran09txt <- nprtxt09
iran10txt <- cnntxt10
iran11txt <- paste(abctxt11, cnntxt11)
iran12txt <- cnntxt12
iran13txt <- paste(abctxt, cnntxt, foxtxt, nprtxt)

####################
##### BAR PLOT #####
####################

iran_s09 <- get_sentences(iran09txt)
iran_sntm09 <- get_sentiment(iran_s09, method="syuzhet")
iran_nrc09 <- get_nrc_sentiment(iran_s09)

iran_s10 <- get_sentences(iran10txt)
iran_sntm10 <- get_sentiment(iran_s10, method="syuzhet")
iran_nrc10 <- get_nrc_sentiment(iran_s10)

iran_s11 <- get_sentences(iran11txt)
iran_sntm11 <- get_sentiment(iran_s11, method="syuzhet")
iran_nrc11 <- get_nrc_sentiment(iran_s11)

iran_s12 <- get_sentences(iran12txt)
iran_sntm12 <- get_sentiment(iran_s12, method="syuzhet")
iran_nrc12 <- get_nrc_sentiment(iran_s12)

iran_s13 <- get_sentences(iran13txt)
iran_sntm13 <- get_sentiment(iran_s13, method="syuzhet")
iran_nrc13 <- get_nrc_sentiment(iran_s13)

iran09sentiments <- colSums(prop.table(iran_nrc09[, 1:10])) 
iran09sentiments$year <- as.double(2009)

iran10sentiments <- colSums(prop.table(iran_nrc10[, 1:10])) 
iran10sentiments$year <- as.double(2010)

iran11sentiments <- colSums(prop.table(iran_nrc11[, 1:10])) 
iran11sentiments$year <- as.double(2011)

iran12sentiments <- colSums(prop.table(iran_nrc12[, 1:10])) 
iran12sentiments$year <- as.double(2012)

iran13sentiments <- colSums(prop.table(iran_nrc13[, 1:10])) 
iran13sentiments$year <- as.double(2013)
  

iran_anger <- c(iran09sentiments$anger,iran10sentiments$anger,iran11sentiments$anger,iran12sentiments$anger,iran13sentiments$anger) 
iran_anticipation <- c(iran09sentiments$anticipation,iran10sentiments$anticipation,iran11sentiments$anticipation,iran12sentiments$anticipation,iran13sentiments$anticipation) 
iran_disgust <- c(iran09sentiments$disgust,iran10sentiments$disgust,iran11sentiments$disgust,iran12sentiments$disgust,iran13sentiments$disgust) 
iran_fear <- c(iran09sentiments$fear,iran10sentiments$fear,iran11sentiments$fear,iran12sentiments$fear,iran13sentiments$fear) 
iran_joy <- c(iran09sentiments$joy,iran10sentiments$joy,iran11sentiments$joy,iran12sentiments$joy,iran13sentiments$joy) 
iran_sadness <- c(iran09sentiments$sadness,iran10sentiments$sadness,iran11sentiments$sadness,iran12sentiments$sadness,iran13sentiments$sadness) 
iran_surprise <- c(iran09sentiments$surprise,iran10sentiments$surprise,iran11sentiments$surprise,iran12sentiments$surprise,iran13sentiments$surprise) 
iran_trust <- c(iran09sentiments$trust,iran10sentiments$trust,iran11sentiments$trust,iran12sentiments$trust,iran13sentiments$trust) 
iran_negative <- c(iran09sentiments$negative,iran10sentiments$negative,iran11sentiments$negative,iran12sentiments$negative,iran13sentiments$negative) 
iran_positive <- c(iran09sentiments$positive,iran10sentiments$positive,iran11sentiments$positive,iran12sentiments$positive,iran13sentiments$positive) 

layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
matplot(c(2009, 2010, 2011, 2012, 2013), 
        cbind(iran_anticipation, iran_trust, iran_negative, iran_positive), 
        #pch=1234,
        type="l",
        lty=1:4,
        lwd=2,
        main="Iran in Interviews",
        sub= "(ABC, CNN, FOX, NPR)",
        xlab = "Year", 
        ylab= "Emotional Valence"
)

par(mar=c(5,0,4,.6))
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("topright", inset=c(-.5,0) ,legend=c("anticipation","trust","negative","positive"), 
       col=seq_len(5), 
       cex = 0.7,
       lty= 1:4,
       lwd = 2)


## Similar Work on Emails
### Improvement: turn these steps into function call ###
require(quanteda)
library(RSQLite)
db <- dbConnect(dbDriver("SQLite"), "/tmp/database.sqlite")

IranQ09 <- dbGetQuery(db, "SELECT Subject As 'Subject', Subject || GROUP_CONCAT(EBody, '  ++  ') As 'Subject_CombEBody' 
                     FROM (SELECT 'in: ' || MetadataSubject AS 'Subject', CAST(ExtractedBodyText AS VARCHAR) AS EBody 
                      FROM Emails e
                      WHERE (lower(ExtractedBodyText) like '%iran%' OR lower(ExtractedBodyText) like '%tehran%'
                      OR lower(MetadataSubject) like '%ahmadinejad%') 
                      AND ( date(substr(MetadataDateSent,11,-10)) > '20081231' AND date(substr(MetadataDateSent,11,-10)) < '20091231') ) 
                     GROUP BY Subject")

IranQ10 <- dbGetQuery(db, "SELECT Subject As 'Subject', Subject || GROUP_CONCAT(EBody, '  ++  ') As 'Subject_CombEBody' 
                      FROM (SELECT 'in: ' || MetadataSubject AS 'Subject', CAST(ExtractedBodyText AS VARCHAR) AS EBody 
                      FROM Emails e
                      WHERE (lower(ExtractedBodyText) like '%iran%' OR lower(ExtractedBodyText) like '%tehran%'
                      OR lower(MetadataSubject) like '%ahmadinejad%') 
                      AND ( date(substr(MetadataDateSent,11,-10)) > '20091231' AND date(substr(MetadataDateSent,11,-10)) < '20101231') ) 
                      GROUP BY Subject")

IranQ11 <- dbGetQuery(db, "SELECT Subject As 'Subject', Subject || GROUP_CONCAT(EBody, '  ++  ') As 'Subject_CombEBody' 
                     FROM (SELECT 'in: ' || MetadataSubject AS 'Subject', CAST(ExtractedBodyText AS VARCHAR) AS EBody 
                      FROM Emails e
                      WHERE (lower(ExtractedBodyText) like '%iran%' OR lower(ExtractedBodyText) like '%tehran%'
                      OR lower(MetadataSubject) like '%ahmadinejad%') 
                      AND ( date(substr(MetadataDateSent,11,-10)) > '20091231' AND date(substr(MetadataDateSent,11,-10)) < '20111231') ) 
                     GROUP BY Subject")

IranQ12 <- dbGetQuery(db, "SELECT Subject As 'Subject', Subject || GROUP_CONCAT(EBody, '  ++  ') As 'Subject_CombEBody' 
                      FROM (SELECT 'in: ' || MetadataSubject AS 'Subject', CAST(ExtractedBodyText AS VARCHAR) AS EBody 
                      FROM Emails e
                      WHERE (lower(ExtractedBodyText) like '%iran%' OR lower(ExtractedBodyText) like '%tehran%'
                      OR lower(MetadataSubject) like '%ahmadinejad%') 
                      AND ( date(substr(MetadataDateSent,11,-10)) > '20111231' AND date(substr(MetadataDateSent,11,-10)) < '20121231') ) 
                      GROUP BY Subject")

IranQ13 <- dbGetQuery(db, "SELECT Subject As 'Subject', Subject || GROUP_CONCAT(EBody, '  ++  ') As 'Subject_CombEBody' 
                      FROM (SELECT 'in: ' || MetadataSubject AS 'Subject', CAST(ExtractedBodyText AS VARCHAR) AS EBody 
                      FROM Emails e
                      WHERE (lower(ExtractedBodyText) like '%iran%' OR lower(ExtractedBodyText) like '%tehran%'
                      OR lower(MetadataSubject) like '%ahmadinejad%') 
                      AND ( date(substr(MetadataDateSent,11,-10)) > '20121231' AND date(substr(MetadataDateSent,11,-10)) < '20131231') ) 
                      GROUP BY Subject")


IranQDF09 <- as.data.frame(IranQ09)
IranQDF10 <- as.data.frame(IranQ10)
IranQDF11 <- as.data.frame(IranQ11)
IranQDF12 <- as.data.frame(IranQ12)
#IranQDF13 <- as.data.frame(IranQ13)

iran_q09txt <- IranQDF09$Subject_CombEBody
iran_qs09 <- get_sentences(iran_q09txt)
iran_qsntm09 <- get_sentiment(iran_qs09, method="syuzhet")
iran_qnrc09 <- get_nrc_sentiment(iran_qs09)
iranQ09senti <- colSums(prop.table(iran_qnrc09[, 1:10])) 
iranQ09senti$year <- as.integer(2009)

iran_q10txt <- IranQDF10$Subject_CombEBody
iran_qs10 <- get_sentences(iran_q10txt)
iran_qsntm10 <- get_sentiment(iran_qs10, method="syuzhet")
iran_qnrc10 <- get_nrc_sentiment(iran_qs10)
iranQ10senti <- colSums(prop.table(iran_qnrc10[, 1:10])) 
iranQ10senti$year <- as.integer(2010)

iran_q11txt <- IranQDF11$Subject_CombEBody
iran_qs11 <- get_sentences(iran_q11txt)
iran_qsntm11 <- get_sentiment(iran_qs11, method="syuzhet")
iran_qnrc11 <- get_nrc_sentiment(iran_qs11)
iranQ11senti <- colSums(prop.table(iran_qnrc11[, 1:10])) 
iranQ11senti$year <- as.integer(2011)

iran_q12txt <- IranQDF12$Subject_CombEBody
iran_qs12 <- get_sentences(iran_q12txt)
iran_qsntm12 <- get_sentiment(iran_qs12, method="syuzhet")
iran_qnrc12 <- get_nrc_sentiment(iran_qs12)
iranQ12senti <- colSums(prop.table(iran_qnrc12[, 1:10])) 
iranQ12senti$year <- as.integer(2012)

#iran_q13txt <- IranQDF13$Subject_CombEBody
#iran_qs13 <- get_sentences(iran_q13txt)
#iran_qsntm13 <- get_sentiment(iran_qs13, method="syuzhet")
#iran_qnrc13 <- get_nrc_sentiment(iran_qs13)
#iranQ13senti$year <- colSums(prop.table(iran_qnrc13[, 1:10])) 

Iran_Qanticipation <- c(iranQ09senti$anticipation,iranQ10senti$anticipation,iranQ11senti$anticipation,iranQ12senti$anticipation) 
Iran_Qtrust <- c(iranQ09senti$trust,iranQ10senti$trust,iranQ11senti$trust,iranQ12senti$trust)
Iran_Qpositive <- c(iranQ09senti$positive,iranQ10senti$positive,iranQ11senti$positive,iranQ12senti$positive)
Iran_Qnegative <- c(iranQ09senti$negative,iranQ10senti$negative,iranQ11senti$negative,iranQ12senti$negative)

layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
matplot(c('2009', '2010', '2011', '2012'), 
        cbind(Iran_Qanticipation,Iran_Qtrust,Iran_Qpositive,Iran_Qnegative), 
        #pch=1234,
        type="l",
        lty=1:4,
        lwd=2,
        main="Iran in Emails",
        xlab = "Year", 
        ylab= "Emotional Valence"
)

par(mar=c(5,0,4,.6))
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("topright", inset=c(-.5,0) ,legend=c("anticipation","trust","negative","positive"), 
       col=seq_len(5), 
       cex = 0.7,
       lty= 1:4,
       lwd = 2)
