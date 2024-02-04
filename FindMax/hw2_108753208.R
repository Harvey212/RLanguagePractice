library(ROCR)

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_yourID.R --target male|female --input file1 file2 ... filen --output out.csv", call.=FALSE)
}

# parse parameters
j<-grep("-",c(args[1:length(args)]))

tarcheck<-0
inputcheck<-0
outputcheck<-0

if(length(j)!=3){
  stop("you should type 3 argument", call.=FALSE)
}else{
  
  if((args[j[1]]=="--target")|(args[j[2]]=="--target")|(args[j[3]]=="--target")){
    tarcheck<-1
  }else{
    stop("missing target", call.=FALSE)
  }
  
  if((args[j[1]]=="--input")|(args[j[2]]=="--input")|(args[j[3]]=="--input")){
    inputcheck<-1
  }else{
    stop("missing input", call.=FALSE)
  }
  
  if((args[j[1]]=="--output")|(args[j[2]]=="--output")|(args[j[3]]=="--output")){
    outputcheck<-1
  }else{
    stop("missing output", call.=FALSE)
  }
  
}


for(i in 1:3){
  judge<-j[i]
  
  if(args[judge]=="--target"){
    query_m<-args[judge+1]
  }else if(args[judge]=="--output"){
    out_f<-args[judge+1]
  }else if(args[judge]=="--input"){
    
    if(i==3){
      end<-length(args)
    }else{
      end<-j[i+1]-1
    }
    files<-args[(judge+1):(end)]
    
    
  }else{
    stop("Unknown flag", call.=FALSE)
  }
  
}

#retrive value
query_func<-function(query_m, file)
{
  
  f<-table(Predictions=file$prediction,TrueLabels=file$reference)
  
  TP<-0
  TN<-0
  FP<-0
  FN<-0
  
  sense<-0
  spec<-0
  prec<-0
  F1<-0
  AUC<-0
  
  if(query_m == "male"){
    TP<-f[2,2]
    TN<-f[1,1]
    FP<-f[2,1]
    FN<-f[1,2]
    file$reference<-ifelse(file$reference=="male",1,0)
    
  }
  else if (query_m == "female") {
    TP<-f[1,1]
    TN<-f[2,2]
    FP<-f[1,2]
    FN<-f[2,1]
    file$reference<-ifelse(file$reference=="female",1,0)
    file$pred.score<-1-file$pred.score
    
  } else {
    stop(paste("ERROR: unknown query function", query_m))
  }
  
  sense<-TP/(TP+FN)
  spec<-TN/(TN+FP)
  prec<-TP/(TP+FP)
  F1<-2*(prec*sense)/(prec+sense)
  
  
  pp<-prediction(as.numeric(file$pred.score),as.numeric(file$reference))
  auc<-performance(pp,'auc')
  auc=unlist(slot(auc,"y.values"))
  
  
  sense<-format(round(sense,2),nsmall=2)
  spec<-format(round(spec,2),nsmall=2)
  F1<-format(round(F1,2),nsmall=2)
  auc<-format(round(auc,2),nsmall=2)
  
  my_list<-list(sense,spec,F1,auc)
  return(my_list) 
  
}

#Initialize list
METHOD<-c()
SENSE<-c()
SPEC<-c()
F11<-c()
AUC<-c()

SENSEH<-0
SENSEWHO<-0

SPECH<-0
SPECWHO<-0

F1H<-0
F1WHO<-0

AUCH<-0
AUCWHO<-0

#construct data frame
for(i in 1:length(files))
{
  name<-gsub(".csv", "",files[i])
  METHOD<-c(METHOD,name)
  
  INPUT <-read.csv(files[i],TRUE,",")
  k<-query_func(query_m,INPUT)
  SENSE<-c(SENSE,k[1])
  SPEC<-c(SPEC,k[2])
  F11<-c(F11,k[3])
  AUC<-c(AUC,k[4])
  
  
  
  if(as.numeric(unlist(k[1]))>SENSEH){
    SENSEH<-as.numeric(unlist(k[1]))
    SENSEWHO<-name
  }
  
  if(as.numeric(unlist(k[2]))>SPECH){
    SPECH<-as.numeric(unlist(k[2]))
    SPECWHO<-name
  }
  
  if(as.numeric(unlist(k[3]))>F1H){
    F1H<-as.numeric(unlist(k[3]))
    F1WHO<-name
  }
  
  if(as.numeric(unlist(k[4]))>AUCH){
    AUCH<-as.numeric(unlist(k[4]))
    AUCWHO<-name
  }
}

#convert list to numeric
SENSE<-as.numeric(unlist(SENSE))
SPEC<-as.numeric(unlist(SPEC))
F11<-as.numeric(unlist(F11))
AUC<-as.numeric(unlist(AUC))

#combine data frame
out1<-data.frame(method=METHOD,sensitivity=SENSE,specificity=SPEC,F1=F11,AUC=AUC)
out2<-data.frame("highest",SENSEWHO,SPECWHO,F1WHO,AUCWHO)
names(out2)<-c("method","sensitivity","specificity","F1","AUC")
newdf<-rbind(out1,out2)

#create output file
write.csv(newdf,file=out_f,row.names=FALSE)








