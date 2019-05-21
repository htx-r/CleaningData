cleanPLACEBOtrials.fun=function(datapath){
  
  ##libraries
  library(readxl)
  library(tidyverse)
  library(car)
  library(MASS)
  ### datasets
  dmpath=paste(datapath,"/dm.csv",sep="")
  cepath=paste(datapath,"/ce.csv",sep="")
  ftpath=paste(datapath,"/ft.csv",sep="")
  qspath=paste(datapath,"/qs.csv",sep="")
  
  
  
  ## Read Biogen dataset that contains the demographic and baseline information
  datadm <- read.csv(dmpath)
  datace <- read.csv(cepath)
  ft <- read.csv(ftpath)
  qs <- read.csv(qspath) 
  
  ###############################################DM dataset-demographics#####################################################
  #keeping only variables:  USUBJID, AGE, SEX, RACE
  keeps<-c("USUBJID","AGE","SEX","RACE")
  demographics<-datadm[,keeps]
  ###empty cells convert to NA
  demographics$RACE[demographics$RACE==""]<-NA
  #ORDER DATA BY ID
  demographics<- demographics[order(demographics$USUBJID),]
  
  
  ###############################################CE DATASET (CONFIRMED==1, UNCONFIRMED=="")-outcome#####################################################
  datace<-datace[order(datace$USUBJID),]
  #keep only columns ID, MIDS, CESEQ, CESTDY (time of visit)
  keeps<-c("USUBJID", "CESTDY", "MIDS")
  unconfirmed1<-datace[,keeps]
  #recode all RELAPSES, empty values are 0 as non events, all other relapses (RELAPSE 1, RELAPSE 2, etc)
  #transformed as RELAPSE=YES
  unconfirmed1$MIDS[unconfirmed1$MIDS == ""] <- NA
  unconfirmed1$MIDS<-as.numeric(unconfirmed1$MIDS)
  unconfirmed1$MIDS<-recode(unconfirmed1$MIDS, "2=1; 3=1; 4=1; 5=1; 6=1; 7=1; 8=1; 9=1; 10=1;")
  unconfirmed1$MIDS[is.na(unconfirmed1$MIDS)]<-0 ##NA here is no event confirmed
  #delete patients with no agreement
  #unconfirmed1<- subset(unconfirmed1, unconfirmed1$USUBJID != "MSOAC/9764")
  
  ##only 0 relapses-nonconfirmed relapses
  
  confirmed<-unconfirmed1[which(unconfirmed1$MIDS==1),]
  ##1st time relapses for each patient
  confirmed1<-confirmed %>% group_by(USUBJID) %>% 
    slice(which.min(CESTDY))
  ##Nas of time relapses in different dataset
  f<-confirmed[which(is.na(confirmed$CESTDY)),]
  
  ####dataset with all confirmed relapses and their time of relapse (once per patient)
  relapsestime<-merge(f,confirmed1,by="USUBJID",all = TRUE)
  relapsestime$time<-pmax(relapsestime$CESTDY.x,relapsestime$CESTDY.y,na.rm = TRUE)
  relapsestime$relapse<-pmax(relapsestime$MIDS.x,relapsestime$MIDS.y,na.rm = TRUE)
  keeps<-c("USUBJID","time","relapse")
  relapsestime<-relapsestime[,keeps]
  relapsestime<-unique(relapsestime)
  
  #####################unconfirmed (relapse==0)################
  unconfirmed<-unconfirmed1[which(unconfirmed1$MIDS==0),]
  unconfirmed2<-unconfirmed %>% group_by(USUBJID) %>% 
    slice(which.max(CESTDY))
  q<-unconfirmed[which(is.na(unconfirmed$CESTDY)),]
  q<-unique(q)
  #unconfirmed with an obstime
  unconfirm<-merge(unconfirmed2,q,by="USUBJID",all=TRUE)
  keeps<-c("USUBJID","CESTDY.x")
  unconfirm<-unconfirm[,keeps]
  unconfirm$relapse<-0
  names(unconfirm)=c("USUBJID","VISIT", "RELAPSE")
  ###observational time 1
  keeps<-c("USUBJID","FTDY")
  ft<-ft[,keeps]
  ft<-ft[order(ft$USUBJID),]
  ft<-unique(ft)
  ft1<-ft %>% group_by(USUBJID) %>% 
    slice(which.max(FTDY))
  ###observational time 2
  keep<-c("USUBJID","QSDY")
  qs<-qs[,keep]
  qs<-unique(qs)
  qs1<-qs %>% group_by(USUBJID) %>% 
    slice(which.max(QSDY))
  ###obserational time 3
  keep<-c("USUBJID","VISIT")
  obs3<-unconfirm[,keep]
  ###########################maximun known observational time for patients
  observationaltime<-merge(qs1,ft1,by="USUBJID",all=TRUE)
  observationaltime<-merge(observationaltime,obs3,by="USUBJID",all=TRUE)
  observationaltime$maxobs<-pmax(observationaltime$QSDY,observationaltime$FTDY,observationaltime$VISIT,na.rm = TRUE)
  keep<-c("USUBJID","maxobs")
  observationaltime<-observationaltime[,keep]
  ########################no relapses and their max observational time
  unconfirmtime<-merge(unconfirm,observationaltime,by="USUBJID",all=TRUE)
  unconfirmtime<-unconfirmtime[which(unconfirmtime$RELAPSE==0),]
  keep<-c("USUBJID","RELAPSE","maxobs")
  unconfirmtime<-unconfirmtime[,keep]
  
  ######relapses and norelapses with observation time and time of relapse
  conuncontimes<-merge(unconfirmtime,relapsestime,by="USUBJID",all=TRUE)
  conuncontimes$relapse01<-pmax(conuncontimes$RELAPSE,conuncontimes$relapse,na.rm=TRUE)
  keep<-c("USUBJID","maxobs","time","relapse01")
  conuncontimes<-conuncontimes[,keep]
  ###in relapses we need only timeofrelapse no maxobs
  conuncontimes$maxobs[which((conuncontimes$relapse01==1))]<-NA
  
  #####add demographics
  allrel<-merge(demographics,conuncontimes,by="USUBJID",all=TRUE)
  names(allrel)<-c("USUBJID","AGE", "SEX", "RACE", "Obstime", "RelapseTime", "Relapse")
  
  ####relapse1year
  allrel$Relapse1year<-NA
  #confirmed
  allrel$Relapse1year[which(allrel$Relapse==1 & allrel$RelapseTime<=365)]<-1
  allrel$Relapse1year[which(allrel$Relapse==1 & allrel$RelapseTime>365)]<-0
  #unconfirmed
  allrel$Relapse1year[which(allrel$Relapse==0 & allrel$Obstime>355)]<-0
  
  ####relapse2year
  allrel$Relapse2year<-NA
  #confirmed
  allrel$Relapse2year[which(allrel$Relapse==1 & allrel$RelapseTime<=730)]<-1
  allrel$Relapse2year[which(allrel$Relapse==1 & allrel$RelapseTime>730)]<-0
  #unconfirmed
  allrel$Relapse2year[which(allrel$Relapse==0 & allrel$Obstime>710)]<-0
  
  rm(conuncontimes,datace,datadm,demographics,f,ft,ft1,obs3,observationaltime,q,qs,qs1,relapsestime,unconfirm,unconfirmed
     ,unconfirmed1,unconfirmed2,unconfirmtime,confirmed,confirmed1,keep,keeps)
}