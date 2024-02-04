#!/usr/bin/env node


args = commandArgs(trailingOnly=TRUE)

if (length(args)==0)
{
  stop("no argument", call.=FALSE)
}else if(length(args)==4)
{
  #read command line
  if(args[1]=="--input")
  {
    df<-args[2]
    
    if(args[3]=="--output")
    {
      OUTPUTfile<-args[4]
    }else
    {
      stop("wrong format, no output", call.=FALSE)
    }
    
  }else if(args[1]=="--output")
  {
    OUTPUTfile<-args[2]
    
    if(args[3]=="--input")
    {
      df<-args[4]
    }else
    {
      stop("wrong format, no input", call.=FALSE)
    }
    
  }else
  {
    stop("wrong format, no input or output", call.=FALSE)
  }
  
  #read input file
  INPUT <-read.csv(df,TRUE,",")
  
  #extract column
  WW <-subset(INPUT, select=c("weight"))
  HH <-subset(INPUT, select=c("height"))
  
  #extract maximum index of selected column
  wmax <-which.max(INPUT$"weight")
  hmax <-which.max(INPUT$"height")
  
  #extract maximum value and round it to 2 decimal
  w1<-WW[wmax,]
  wround<-format(round(w1,2),nsmall=2)
  h1<-HH[hmax,]
  hround<-format(round(h1,2),nsmall=2)
  
  #header for input
  #header<-substring(df,1,6) #wrong
  header<-gsub(".csv", "",df)
  #generate output file
  outt <- data.frame(set=header,weight=wround,height=hround)
  write.csv(outt,file=OUTPUTfile,row.names=FALSE)

}else{
  stop("wrong format", call.=FALSE)
}
