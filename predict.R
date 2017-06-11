#Functions
## Helper- Required libraries
#' @family : Text processing
#' Loading function: Required libraries
#' @return : Sets up the environment.
#'
Lib=function() {
  library(rJava)
  library(knitr)
  library(curl)
  library(shiny)
  library(devtools)
  library(shinydashboard)
  library(plyr)
  library(RSQLite)
  library(DBI)
  library(dplyr)
  library(tidyr)
  library(NLP)
  library(SnowballC)
  library(RColorBrewer)
  library(ngram)
  library(magrittr)
  library(stringi)
  library(stringr)
  library(gsubfn)
  library(e1071)
  library(ANLP)
  library(tm)
  library(quanteda)
  library(qdap)
  library(qdapRegex)
  library(textcat)
  library(RWeka)
  library(qdapDictionaries)
  library(XLConnect)
  library(rsconnect)
  library(DT)
  library(markdown)
  memory.limit(100000000)
  Sys.setenv(WNHOME = "/usr/local/wordnet-3.0")

 #Sys.setenv(WNHOME = "C:/Program Files (x86)/WordNet/2.1/bin")
  library(wordnet)
  setDict(file.path("usr", "share", "dict"))
}
#Functions
## Helper- Loads corpus for processing
#' @family : Text processing
#' Loading function: Loads corpus for processing.
#' @param folder: Destination folder to download files from an external source connected via a URL
#' @param tf: If TRUE recursively loads files in folders and subfolders. Default is false
#' @param zfile: Name of file to download from an external source. Defaults to "Coursera-SwiftKey.zip".
#' @param url: URL to the external source of the file. Defaults to "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
#' @return same class as input
#'
Loading=function(folder,tf,zfile,url) {
  wd=getwd()
  if (missing(tf)) {
    tf=FALSE
  }
  if (missing(zfile)) {
    zfile="Coursera-SwiftKey.zip" }
  if (missing(url)) {
    url="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip" }
  folder=paste(wd,folder,sep="/")
  zfile=paste(wd,zfile,sep="/")
  if (!dir.exists(folder)) {
    download.file(url,zfile)
    unzip(zfile)}
  docs = VCorpus(DirSource(folder,mode="text",recursive=tf),readerControl = list(reader=readPlain,language = "en"))
  docs
}
## Helper- Clean Function
#' @family : Text processing
#' clean function: for emails, emoticons, citation, title, abbreviations, non-ascii characters, URLs, Dates, numbers, punctuations, whitespaces and trims.
#' @param data: Text data to be cleaned
#' @param xwords: profanities list as a sqlite table
#' @return same class as input
#'
clean=function(data) {
  data=as.character(data[,1])
  data=rm_email(data) #remove emails
  data=rm_emoticon(data) #remove emoticons
  data=rm_citation(data) #remove citation
  data=rm_title_name(data) #remove title
  data=rm_abbreviation(data) #remove abbreviations
  data=rm_date(data) #remove dates
  data=rm_non_ascii(data) #remove non-ascii characters
  data=gsubfn("http[^ ]*|www[^ ]*", "",data) #removing URLs
  data=tolower(data) #convert to lower
  data=pclean(data) #remove profanity
  data=removeNumbers(data) #remove numbers
  data=rm_repeated_characters(data) #remove repeititve characters
  data=gsubfn("[][#$%()`*:;\\.\\,\\!\\'?\"\\+\\&\\/<=>@^_|~{}=\\-]","",data) # replacing special characters
  data=ctrim(data)
  data
}
## Helper- Capitalizing I's in text
#' @family : Text processing
#' capclean function: cleans up i-s to I-s. Replaces text such as  "i", "i'm", "i'd" and "i've" to "I", "I'm", "I'd" and "I've" respectively
#' @param data: Text data to be cleaned
#' @return cleaned up text in the same class as input
#'
capclean=function(data) {
  data=gsubfn(" i "," I ",data) #capitalize standalone i
  data=gsubfn(" i've | ive "," I've ",data)#capitalize i've
  data=gsubfn(" i'm | im "," I'm ",data)#capitalize i've
  data=gsubfn(" i'd"," I'd ",data)#capitalize i've
  # data=gsubfn("(^|[.?!][[:space:]])([[:alpha:]])", "\\1\\U\\2", data, perl=TRUE) #capitalize first letter of a sentence or row
}
## Helper- Profanity cleaning
#' @family : Text processing
#' pclean function: cleans text for profanity. Requires profanity list loaded to profanity table in gramdb table. Requires DBI, RSQLite
#' @param data: Text data to be cleaned
#' @return cleaned up text in the same class as input
#'
pclean=function(data) {
  xwords=read.csv(paste(getwd(),"data/words/words.txt",sep="/"),sep="|",header=TRUE)
  names(xwords)="profanity"
  data=removeWords(as.character(data),xwords$profanity) #remove profanity
  data
}
## Helper- Trimming text
#' @family : Text processing
#' ctrim function: cleans text for white spaces and trims the text for leading and trailing spaces.
#' @param data: Text data to be cleaned
#' @return cleaned up text in the same class as input
#'
ctrim=function (data) {
  data=data[!(data=="")]
  data=rm_white_multiple(data) #Remove extra white spaces
  data=str_trim(data) #Trim
  as.data.frame(data)
}
## Helper- Checking for end of sentence
#' @family : Text processing
#' ctrim function: cleans text for white spaces and trims the text for leading and trailing spaces.
#' @param data: Text data to be cleaned
#' @return cleaned up text in the same class as input
#'
cap=function(data) {
  if (str_sub(data, -1,-1) %in% c(".","?","!") || str_sub(data, -2,-1) %in% c(". ","? ","! ")) return(TRUE) #Capitalize after end of sentence
  else return(FALSE)
}
## Helper- Spell check, keystroke prediction
#' @family : Text prediction
#' spellc function: Predicts words based on keystroke
#' @param data: Keystroke as data
#' @param mafil: Wordnet search type to perform. Acceptable values are "ContainsFilter", "EndsWithFilter", "ExactMatchFilter","RegexFilter", "SoundFilter", "StartsWithFilter", and "WildcardFilter".
#' @param nres: Number of results to return. Defaulted to 3 results.
#' @return Word(s) matching keystroke
#'
spellc=function(data,mafil,nres=3) {
  #setDict("C:/Program Files (x86)/WordNet/2.1/dict")
  pos=c("NOUN","VERB","ADJECTIVE","ADVERB")
  len=length(pos)
  i=1
  res=as.character
  while (i <= len) {
    filter=getTermFilter(mafil, data, TRUE)
    terms=getIndexTerms(pos[i],nres,filter)
    rs=sapply(terms, getLemma)
    res=c(res,rs)
    i=i+1 }
  res=as.data.frame(as.character(tail(res, (NROW(res)-1))))
  names(res)="Word"
  if (nrow(res)>0) {
    res=clean(as.data.frame(res))
  }
  res
}
## Helper- Break down large text file to smaller files
#' @family : Text prediction
#' brkdata function: Break down large text file to smaller files
#' @param data: File to be broken down
#' @param folder: Destination folder to drop files
#' @param fn: File name to use
#' @param nr: Number of lines of text per file
#' @return : Smaller text files
#'
brkfile=function(data,folder=dest,fn,nr) {
  x=length(data)
  i=0
  j=0
  folder=paste(getwd(),folder,sep="/")
  while (i < x) {
    j=j+1
    pdata=piececorp(data,nr)
    p=length(pdata)
    i=i+p
    data=data[-(1:p)]
    fn1=paste(fn,"_",j,".txt",sep="")
    fpath=paste(folder,fn,fn1,sep="/")
    write.table(pdata,fpath,row.names=FALSE,quote=FALSE,col.names=FALSE)
  }
}
## Helper- Break down data to usable length
#' @family : Text prediction
#' brkdata function: Breaks string to individual words and returns the word(s) as per request
#' @param data: Data to be broken into usable length
#' @param n: Number of values to return
#' @param ht: Position of the value to return the value from. Valid values are "head", "tail"
#' @return : Word(s) either from the top of the string or end of the string.
#'
brkdata=function(data,n,ht) {
  data=as.data.frame(stri_extract_all_words(data))
  data=ht(data,n)
  data=str_c(data[,1], collapse = " ")
  data
}
## Helper- Break down text coprus to smaller chunks for quicker processing
#' @family : Text cleanup
#' piececorp function: Break down text coprus to smaller chunks for quicker processing
#' @param data: Data to be broken into smaller chunks
#' @param nr: Number of lines in each chunk.
#' @return : Small chunks of text
#'
piececorp=function(data,nr) {
  l=length(data)
  if (l>=nr)
    pdata=data[1:nr]
  else
    pdata=data[1:l]
  pdata
}
## Helper- Break down text in a data frame to smaller chunks for quicker processing
#' @family : Text cleanup
#' piececorp function: Break down text in a data frame to smaller chunks for quicker processing
#' @param data: Data to be broken into smaller chunks
#' @param nr: Number of lines in each chunk.
#' @return : Small chunks of text
#'
piecedf=function(data,nr) {
  l=nrow(data)
  if (l>=nr)
    pdata=data[1:nr,]
  else
    pdata=data[1:l,]
  pdata
}
## Helper- Tokenize corpus for bag of words (n-grams)
#' @family : Text prediction
#' gram function: Create bag of words- n-grams
#' @param data: Corpus
#' @param n : From "n" of n-gram
#' @param n1 : To "n" of n-gram
#' @param wt : Weighting to use for creating the bag of words
#' @return : Returns a matrix of weighted words and frequency
#'
gram=function(data,n=1,n1=3,wt=weightTf) {
  data=VCorpus(VectorSource(data))
  ngram=function(x=data) NGramTokenizer(x, Weka_control(min = n, max = n1))
  ngmatrix=TermDocumentMatrix(data, control=list(tokenize=ngram,weighting=wt))
  Freq=sort(row_sums(ngmatrix, na.rm=TRUE), decreasing=TRUE) #sorting ngram by frequency (desc)
  Phrase=names(Freq) #extracting ngram as a list
  dfgram=as.data.frame(Phrase,stringsAsFactors=FALSE)
  dfgram$Freq=Freq
  dfgram
}
## Helper- Formats n-grams to Phrase, Frequecy, Pre-phase, Word, N-gram token
#' @family : Text prediction
#' gram function: Formats n-grams to Phrase, Frequecy, Pre-phase, Word, N-gram token
#' @param data: Bag of words created using ngram function
#' @return : Returns a dataframe of formatted bag of words
#'
gramformat=function(data) {
  data$PrePh= gsubfn(" [[:alpha:]]*$", replacement = "",as.character(data$Phrase)) #extract pre-phrase of ngram
  data$Word= stri_extract_last_words(data$Phrase) #extract last phrase of ngram
  row.names(data)= NULL #eliminating row names
  data
}
## Helper- Format contractions by adding apostrophe
#' @family : Text prediction
#' addapo function: Formats contractions with apostrophe
#' @param data: Data to add apostrophe to
#' @return : Returns word(s) formatted with apostrophe
#'
addapo=function(data) {
  c=sqlcontsel(data)
  if (nrow(c) ==0) { c=data } #Add apostrpohe for contraction words
  c
}
## Helper- Function that reorders combination of character vectors and single values in a list or dataframe to single value dataframe
#' @family : Text prediction
#' brkdata1 function: Function that reorders combination of character vectors and single values in a list or dataframe to single value dataframe
#' @param data: Data to be formatted
#' @return : Formatted word(s)
#'
brkdata1=function(data) {
  i=1
  r1=character()
  while (i <= ncol(data)) {
    r=data[,i]
    r1=c(r1,r)
    i=i+1
  }
  r1
}

## Helper- Function that calls addapo and capclean
#' @family : Text prediction
#' concapclean function: Formats contractions, capitalizes personal pronoun "I" to capital and capitalizes the first letter of the word when the word follows a period, exclamation or a question mark or is the the beginning of the paragrah.
#' @param data: Data to be formatted
#' @return : Formatted word(s)
#'
contcapclean=function(data,ptf) {
  con=sqlcontallsel()
  c=as.data.frame(data[data$Word %in% con$woApo,])
  if (!(nrow(c)==0)) {
    data=as.data.frame(data[!(data$Word %in% con$woApo),]) #Add apostrophe to results (words such as wont, cant etc)
  }
  rc=as.data.frame(lapply(as.character(c[,1]),addapo))
  r1=brkdata1(rc)
  data=rbind(data,r1)
  data=as.data.frame(as.character(lapply(data[,1],capclean))) #Capitalize I's in results (words such a I, I've)
  if (ptf==TRUE) {
    data=as.data.frame(sapply(data[,1],str_to_title)) #Capitalize first letter after end of a sentence
  }
  names(data)="Word"
  data
}
## Helper- Predicts words from N-Grams
#' @family : Text prediction
#' ngrampred function: Predicts the next word(s) from a list of n-grams
#' @param data: Data that must be predicted
#' @param ng: The N in N-Gram. Indicate 2 if there are only bigrams and uni-grams in you database, 3 if you have tri-grams too and so on.
#' @param nres: Number of predicted words (results) to return
#' @return : Next word(s) predicted for the string.
#'
ngrampred=function(data,ng,nres) {
  res=data.frame()
  i=0
  j=1
  k=ng
  if (data=="" || data== " ") {
    res=sqlnullsel(1,nres)
  }
  else if (str_count(data," ")+1>(ng-1)) {
    while (i < ng && k > 1) {
      data=brkdata(data,ng-j,tail)
      res=sqlwordsel(data,k,nres)
      i=i+1
      j=j+1
      k=k-1
    }
  }
  else if (str_count(data," ")+1==ng-1) { res=sqlwordsel(data,ng,nres) }
  else { res=sqlwordsel(data,ng-1,nres) }
  as.data.frame(unique(res$Word))
}
## Helper- Predicts keystrokes by calling wordnet function
#' @family : Text prediction
#' keypred function: Predicts the word(s) based on keystroke
#' @param data: Data that must be predicted
#' @param nres: Number of predicted words (results) to return
#' @return : Word(s) that match the start of the word
#'
keypred=function(data,nres) {
  res=unique(spellc(data,mafil="StartsWithFilter",nres))
  names(res)="Word"

  if (nrow(res)==0)  {
    exit()
  }
  else {
  as.data.frame(unique(res$Word))
  }
}
## Helper- Executes the functions to produce smaller files of the corpus
#' @family : Text prediction
#' execbreak function: Predicts the word(s) based on keystroke
#' @param sour: Location of the corpus to be broken down
#' @param dest: Location where the files must be stored
#' @param n: Number for the file
#' @param fname: Name of the file in the source folder. The smaller files will be tagged with this original file name.
#' @param tf: TRUE or FALSE value to set recursive mode for pulling files from folders
#' @param nr:  Number of lines in each destination file.
#' @return : Creates smaller size files of the corpus
#'
execbreak=function(sour,dest,n,fname,tf,nr) {
  data=Loading(sour,tf)
  data=data[n][[1]]$content
  cleanmem(docs)
  brkfile(data,dest,fname,nr)
}
## Helper- Executes the corpus cleaning functions and saves the cleaned files in a destination folder
#' @family : Text prediction
#' execclean function: Executes the corpus cleaning functions and saves the cleaned files in a destination folder
#' @param data: Location of the files to be cleaned
#' @param dest: Location where the cleaned files must be stored
#' @param fn: Name to tag the cleaned files.
#' @return : Cleans corpus and stores as small size files for easy sampling
#'
execclean=function(data=docs,dest,fn) {
  n=length(data)
  i=0
  while (i < n) {
    i=i+1
    pdata=data[i][[1]]$content
    fn1=paste(fn,"_",i,".txt",sep="")
    dest0=paste(dest,fn,sep="/")
    fpath=paste(dest0,fn1,sep="/")
    pdata=as.character(lapply(pdata,clean))
    write.table(pdata,fpath,row.names=FALSE,quote=FALSE,col.names=FALSE)
  }
}
## Helper- Executes bag of words (ngram) functions
#' @family : Text prediction
#' execgram function: Executes bag of words (ngram) functions
#' @param data: Location of the cleaned files
#' @param n : From "n" of n-gram
#' @param n1 : To "n" of n-gram
#' @param dest: Location where the cleaned files must be stored
#' @param nr: Number of lines in each chunk.
#' @param sp: Sparsing value
#' @param wt : Weighting to use for creating the bag of words
#' @return : Returns formatted weighted n-grams and loads to a database
#'
execgram=function(data=docs,n=1,n1=3,nr=10000,sp=0,wt) {
  l=length(data)
  i=0
  gdata=data.frame()
  while (i < l) {
    i=i+1
    pdata=data[i][[1]]$content
    if (n>1 && n1 > 1) {
      pdata=cstop(pdata)
    }
    pdata=gram(pdata,n,n1,wt)
    nval=function(x) y=stri_stats_latex(x)[4]
    pdata$NGram=as.integer(unname(sapply(pdata$Phrase,nval)))
    sqltempins(pdata,"temp")
  }
  cleanmem(docs)
  i=0
  while (i < (n1+1)) {
    i=i+1
    tdata=sqlngramsel(tbl,i)
    sqltempins(tdata,"temp1")
    sqltempdel(i)
    grdata=sqlgrpsel()
    grdata=subset(grdata, (grdata$Freq>sp))
    sqltemp1del()
    gdata=rbind(gdata,grdata)
  }
  sqlt(gdata,"temp")
  cleanmem(grdata)
  cleanmem(tdata)
  execgramdb(gdata,nr)
}
## Helper- Executes the formating of ngrams
#' @family : Text prediction
#' execgram function: Executes the formating of ngrams
#' @param data: Bag of words to be formated as a data frame
#' @param nr: Number of lines to be formatted at a time. Smaller number runs faster.
#' @return : Returns formatted weighted n-grams and loads to a data base
#'
execgramdb=function (data,nr) {
  gramdb= dbConnect(RSQLite::SQLite())
  dbSendQuery(conn=gramdb,
              "CREATE TABLE grams
              (Phrase TEXT,
              PrePh TEXT,
              Word TEXT,
              Freq INTEGER,
              NGram INTEGER)")
  dbSendQuery(conn=gramdb,
              "CREATE TABLE temp
              (Phrase TEXT,
              Freq INTEGER)")
  dbSendQuery(conn=gramdb,
              "CREATE TABLE temp1
              (Phrase TEXT,
              Freq INTEGER)")
  l=nrow(data)
  i=0
  gdata=data.frame()
  while (i < l) {
    pdata=piecedf(data,nr)
    n=nrow(pdata)
    i=i+n
    data=data[-(1:n),]
    pdata=gramformat(pdata)
    gdata=rbind(gdata,pdata)
  }
  sqlgramins(gdata)
}
## Helper- Calls ngrampred & keypred, calls word formatting functions
#' @family : Text prediction
#' pred function: Predicts the word(s) based on keystroke and ngram. Formats the results.
#' @param data: Data that must be predicted#' @param ng: The N in N-Gram. Indicate 2 if there are only bigrams and uni-grams in you database, 3 if you have tri-grams too and so on.
#' @param ng: The N in N-Gram. Indicate 2 if there are only bigrams and uni-grams in you database, 3 if you have tri-grams too and so on.
#' @param nres: Number of predicted words (results) to return
#' @param lc: Last keystroke value
#' @return : Formatted next word(s) predicted for the string or word(s) that match the start of the word
#'
pred=function(data,ng,nres,lc) {
  if (data=="") {
    newsent=TRUE
    puntcap=TRUE
  }
  else {
    newsent=FALSE
    puntcap=cap(data)
    data= clean(tail(as.data.frame(strsplit(data,"\\.|\\?|\\!")),1))
    data=as.character(data$data)
  }

  if (lc==" " || newsent==TRUE) {
    res=as.data.frame(ngrampred(data,ng,nres))
  }
  else {
    lastw=brkdata(data,1,tail)
    lastw=clean(as.data.frame(lastw))
    lastw=as.character(lastw$data)
    res=as.data.frame(keypred(lastw,nres))
  }
  res=contcapclean(res,puntcap)
  as.data.frame(unique(res$Word))
}
#Queries for exploration
sqlgramins=function(df) {
  sql="INSERT INTO gramtr VALUES ($Phrase, $PrePh, $Word, $Freq, $NGram)"
  dbins(sql, df)
}
sqltempins=function(df,tbl) {
  sql=paste("INSERT INTO ",tbl," VALUES ($Phrase, $Freq, $NGram)",sep="")
  dbins(sql, df)
}
sqlgramsel=function() {
  sql=paste("SELECT * FROM gramtr", sep="")
  dbGetQuery(gramdb, sql)
}
sqlunisel=function() {
  sql=paste("SELECT * FROM gramtr WHERE NGram=1", sep="")
  dbGetQuery(gramdb, sql)
}
sqlngramsel=function(tbl,g) {
  sql=paste("SELECT * FROM ",tbl," WHERE NGram=",g," ORDER BY Freq DESC LIMIT 20", sep="")
  dbGetQuery(gramdb, sql)
}
sqltempdel=function(i) {
  sql=paste("DELETE FROM temp WHERE NGram=",i,sep="")
  dbGetQuery(gramdb,sql)
}
sqlgrpsel=function() {
  sql=paste("SELECT Phrase As Phrase, Sum(Freq) As Freq,NGram FROM temp1 GROUP BY Phrase", sep="")
  grdata=dbGetQuery(gramdb, sql)
}
sqltemp1del=function() {
  sql=paste("DELETE FROM temp1",sep="")
  dbGetQuery(gramdb,sql)
}
dbins=function(sql, df)  {
  dbBegin(gramdb) #connecting to db
  dbGetQuery(gramdb, sql, bind.data = df)
  dbCommit(gramdb) #write to db
}
dbclose=function() {
  dbRemoveTable(gramdb,"gramtr")
  dbRemoveTable(gramdb,"temp")
  dbRemoveTable(gramdb,"temp1")
  dbDisconnect(gramdb)
}
#Queries for prediction
sqlwordsel=function(data,ng,l) {
  sql=paste("SELECT Word FROM grams WHERE NGram=",ng," AND PrePh LIKE'",data,"' ORDER BY Freq DESC LIMIT ",l,  ";",sep="")
  dbGetQuery(gramdb, sql)
}
sqlnullsel=function(ng,l) {
  sql=paste("SELECT Word FROM grams WHERE NGram=",ng," ORDER BY Freq DESC LIMIT ",l,  ";",sep="")
  dbGetQuery(gramdb, sql)
}
sqlcontsel=function(data) {
  sql=paste("SELECT wApo from contractions WHERE (woApo='",data,"')",  ";",sep="")
  dbGetQuery(gramdb, sql)
}
sqlcontallsel=function() {
  sql=paste("SELECT * from contractions;",sep="")
  dbGetQuery(gramdb, sql)
}
#init=function() {
Lib()
grams=read.table(paste(getwd(),"data/grams.txt",sep="/"),sep="|",header=TRUE)
contractions=read.csv(paste(getwd(),"data/words/contractions.txt",sep="/"),sep="|",header=TRUE)
names(contractions)=c("woApo","wApo")
gramdb<<-dbConnect(RSQLite::SQLite())
dbWriteTable(gramdb,"grams",grams)
dbWriteTable(gramdb,"contractions",contractions)
#}

p=function(data) {
  data=as.data.frame(data)
  naiveBayes(PrePh ~ .,data=data)
}

ui <- dashboardPage(skin = "purple",
 dashboardHeader(title = "Swiftkey Prediction"),
 dashboardSidebar(disable=TRUE),
 dashboardBody(
   tags$head(tags$script(src="js.cookie.js")),
   tags$head(tags$style(HTML('
      .skin-purple .main-header .logo {background-color: #999999;}
      .main-header .logo {font-family: "Georgia", Times, "Times New Roman", serif;
                          font-weight: bold;
                          font-size: 24px;}
                             .content-wrapper,
      .right-side {background-color: #ffffff;}
      .box {
            background-color: #ffffff;
            border-color: #ffffff;
            outline-color: #ffffff;
            outline-style: none
            outline-shadow: "#fffff";}
      .verbatimTextOutput {
            background-color: #ffffff;
            border-color: #ffffff;
            outline-color: #ffffff
            outline-style: none
            outline-shadow: "#fffff"'))),
   fluidRow(column(4, textAreaInput("text", label = "Type Here")),
                column(7, DT:: dataTableOutput("words"))),
                br(),
                br(),
   fluidRow(column(12,box(verbatimTextOutput("urtext",placeholder=TRUE),width=12))),
   br(),
   br(),
   br(),
   br(),
   fluidRow(column(12,includeMarkdown(("help.md"))))
            ))

server<-(function(input,output) {
  observeEvent(input$text, {
    data<-input$text
    lc=str_sub(input$text,-1,-1)
    output$urtext<- renderText({input$text})
    output$words <- renderDataTable({datatable(pred(data,3,10,lc),options = list(dom = 't',echo=FALSE),colnames ="Suggested Next Words")})
 })  })

  shinyApp(ui, server)