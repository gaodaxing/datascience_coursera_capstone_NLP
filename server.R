library(shiny)
library(rJava)
library(SnowballC)
library(tm)
library(data.table)
N_gram=fread("N_gram.csv")
uni_gram=fread("uni_http_N.csv")
uni_gram=uni_gram[,2:3,with=F]
names(uni_gram)=c("uni_gram","Frequency")
uni_gram$Frequency=as.numeric(uni_gram$Frequency)
uni_gram=uni_gram[order(Frequency,decreasing = T)]

fprofane=file("/Users/daxinggao/Desktop/profanity.txt")
profane=VectorSource(readLines(fprofane,warn=F))
close(fprofane)
removecapital<-function(x)gsub('[A-Z]{2,}',"",x)
removeURL <- function(x) gsub("(ht|f)tp[[:alnum:][:punct:]]*", "", x)
removeshortword<-function(x)gsub("(\\s|^)([a-z]{1,2}(\\s|$))+"," ",x)
removeEmail<-function(x)gsub("[[:alnum:][:punct:]]*@[[:alnum:][:punct:]]*","",x)
removeWWW<-function(x)gsub("www\\.[[:alnum:][:punct:]]*","",x)
removehastag<-function(x)gsub("\\#[[:alnum:][:punct:]]*","",x)
removerepeatletter<-function(x)gsub("([[:alpha:]])\\1{2,}","\\1",x)
replacepunctuation<-function(x)gsub("[[:punct:]]+"," ",x)
##shownot<-function(x)gsub("n\\'t"," not",x)

clean_sentence<-function(sentence){
  docs=Corpus(VectorSource(c(sentence)))
  docs=tm_map(docs,content_transformer(removecapital))
  docs=tm_map(docs,content_transformer(tolower))
  docs=tm_map(docs, content_transformer(removeURL))
  docs=tm_map(docs, content_transformer(removeEmail))
  docs=tm_map(docs, content_transformer(removehastag))
  docs=tm_map(docs, content_transformer(removeWWW))
  
  docs=tm_map(docs,stripWhitespace)
  docs=tm_map(docs, content_transformer(removerepeatletter))
  docs=tm_map(docs,removeWords,c(stopwords("english"),"can","will"))
  docs=tm_map(docs,removeWords,profane$content[-1])
  
  docs=tm_map(docs,content_transformer(replacepunctuation))
  docs=tm_map(docs, removeNumbers)
  docs=tm_map(docs, content_transformer(removeshortword))
  ##docs=tm_map(docs,content_transformer(shownot))
  docs=tm_map(docs,stripWhitespace)
  ##docs=tm_map(docs, stemDocument)
  docs=tm_map(docs,PlainTextDocument)
  
  docs=as.character(docs[[1]])
  words=strsplit(docs,"\\s+")[[1]]
  if(length(words)>2){
  uniword=words[length(words)]
  biword=words[(length(words)-1):length(words)]
  biword=paste(biword,collapse=" ")
  triword=words[(length(words)-2):length(words)]
  triword=paste(triword,collapse=" ")
  }
  else if(length(words)==2){
    uniword=words[length(words)]
    biword=words[(length(words)-1):length(words)]
    biword=paste(biword,collapse=" ")
    triword=""
  }
  else if(length(words)==1){
    uniword=words[length(words)]
    biword=""
    triword=""
  }
  else{uniword=""
  biword=""
  triword=""}
  return(list(uniword,biword,triword))
}

calculatep<-function(uniword,biwords,triwords){
  
  predict=N_gram[N_gram==triwords|N_gram==biwords|N_gram==uniword]
  return(unique(predict))
}

predictword1<-function(sentence,candidates=NULL){
  lis=clean_sentence(sentence)
  r=calculatep(lis[[1]],lis[[2]],lis[[3]])
  if(!is.null(candidates)){##there is candidates
    if(nrow(r)!=0)
    {   ## hints from N gram
      if(nrow(r[r$N_gram%in%candidates])!=0)
      { ##"n gram prediction found in candidates
        prediction=r[r$N_gram%in%candidates]
        if(nrow(prediction)>1)prediction=prediction[order(p,decreasing=T)]
        return(prediction$prediction)
      }
      
      else { ## hints from N gram not work 
        u=uni_gram$uni_gram
        predict=uni_gram[u%in%candidates]
        predict=predict[order(Frequency,decreasing=T)]
        ## n gram not work, unigram prediction
        if(nrow(predict)>0){
          return(predict$uni_gram)
        }
        else return(candidates)
      }
    }
    else{## one gram prediction
      u=uni_gram$uni_gram
      predict=uni_gram[u%in%candidates]
      predict=predict[order(Frequency,decreasing=T)]
      ## no n gram, unigram prediction
      if(nrow(predict)>0){
        return(predict$uni_gram)
      }
      else return(candidates)
    }
  }
  else{## no candidates
    if(nrow(r)!=0){
      ##n gram prediction with no candidates
      prediction=r
      if(nrow(prediction)>1)prediction=prediction[order(p,decreasing=T)]
      if(nrow(prediction)>5)return(prediction[1:5]$prediction)
      return (prediction$prediction)
    }
    else 
    {
      ##no n gram prediction with no candidates
      return(c("the","and","of","in","to"))
    }
  }
}

predictword2<-function(sentence,candidates=NULL){
  lis=clean_sentence(sentence)
  r=calculatep(lis[[1]],lis[[2]],lis[[3]])
  
  if(!is.null(candidates)){##there is candidates
    if(nrow(r)!=0)
    {   ## hints from N gram
      if(nrow(r[r$N_gram%in%candidates])!=0)return("N gram prediction with candidates")
      else { 
        u=uni_gram$uni_gram
        predict=uni_gram[u%in%candidates]
        predict=predict[order(Frequency,decreasing=T)]
        if(nrow(predict)>0){
          return("Uni-gram prediction in candidates")
        }
        else return("Cannot predict next word in candidates. Please predict without candidates!")
      }
    }
    else{## one gram prediction
      u=uni_gram$uni_gram
      predict=uni_gram[u%in%candidates]
      predict=predict[order(Frequency,decreasing=T)]
     
      if(nrow(predict)>0){
        return("Uni-gram prediction with candidates")
      }
      else return("Cannot predict next word in candidates.  Please predict without candidates!")
    }
  }
  
  else{## no candidates
    if(nrow(r)!=0){
      return("N gram prediction top words")
      }
    else 
    {
      return("Cannot predict next words, suggest stopwords")
      }
  }
}

shinyServer(
  function(input, output,session) {
    observe({
    
    if(input$button1 == '1') {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#text1').prop('disabled',true)"))
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#text2').prop('disabled',true)"))
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#text3').prop('disabled',true)"))
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#text4').prop('disabled',true)"))
    } else {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#text1').prop('disabled',false)"))
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#text2').prop('disabled',false)"))
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#text3').prop('disabled',false)"))
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#text4').prop('disabled',false)"))
    }
    
    })
    
    output$outputtext<-renderText({
      
      a=input$sentence
      candidates=NULL
      if(input$button1 == '1'){
        candidates=NULL
      }
      else{
      cand1=input$text1
      cand2=input$text2
      cand3=input$text3
      cand4=input$text4
      candidates=c(cand1,cand2,cand3,cand4)
      candidates=candidates[candidates!=""]
      
      if(length(candidates)==0)candidates=NULL
      
      }
      c=" "
      b=""
      if(nchar(a)>1){b=strsplit(a,"")[[1]]}
      if(input$submit2>0){
        p=predictword1(a,candidates)
        paste("(",1:length(p),")",p)
        updateTextInput(session, "text1",value = "")
        updateTextInput(session, "text2",value = "")
        updateTextInput(session, "text3",value = "")
        updateTextInput(session, "text4",value = "")
        
      }
      
      if(b[length(b)]==" "|input$submit1>0){
        p=predictword1(a,candidates)
       paste("(",1:length(p),")",p)
      }
      else{NULL}
    })
    
    
    
    output$hints<-renderText({
      a=input$sentence
      cand1=input$text1
      cand2=input$text2
      cand3=input$text3
      cand4=input$text4
      
      if(length(candidates[candidates!=""])==0)candidates=NULL
      c=" "
      b=""
      if(nchar(a)>1)
      {b=strsplit(a,"")[[1]]}
      
      
      if(b[length(b)]==" "|input$submit1>1){
        predictword2(a,candidates)
      }
      else{""}
      
    })
    
    
    observe({
       if(input$submit2>0){
        
      }
    })
    
  }
)
