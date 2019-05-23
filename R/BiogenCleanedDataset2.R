rm(list=ls())
##########################################################################################

# 1. I chose the 'objective relpase assessment' (PARAMCD='OBJREL') as a type of the relapse
  # However, it is the same as 'all realpse assessment'.
# 2. Create a dummy varaible from the AVAL varaible for the 1 year relapses (AVISIT='0-1 Year')
# 3. Create a dummy varaible from the AVAL varaible for the 2 years relapses (AVISIT='Overall 0-2 Years')  
# 4. Check if the calculated dummy variables are correct.
# 5. Check if the IPD compatible with AD (form MS file)

# PS: AVAL counts the number of the relapses to each type of the assessment in PARAMCD.

#########################################################################################

library(readxl)

## Read the IPD biogen data

# path
datapath="/Users/tasneem/Desktop/TasnimPhD/multiple sclerosis/IPD data from 6 Biogen trials"
adslpath=paste(datapath,"/adsl.csv",sep="")
adarrpath=paste(datapath,"/adarr.csv",sep="")
adrelpath=paste(datapath,"/adrel.csv",sep="")

# data
adsl <- read.csv(adslpath)
adarr <- read.csv(adarrpath)
adrel <- read.csv(adrelpath)

## 1. Create the dataset with only the 'objective relpase assessment' (PARAMCD='OBJREL') 
adarr_OBJREL <- adarr[adarr$PARAMCD=='OBJREL',]


## 2. Create a dummy varaible (from AVAL) for the within 1 year relapses RELAPSE01Year
adarr_OBJREL01year <- adarr_OBJREL[adarr_OBJREL$AVISIT=='0-1 Year',]
RELAPSE01Year <-  ifelse(adarr_OBJREL01year$AVAL>0,1,0)

## 3. Create a dummy varaible (from AVAL) for the within 2 years relapses RELAPSE02Year

#%%% All the patients have been checked within overall 0-2 Years except one study (only 0-1 year).
       # so it is not straight forward as 0-1 year where all studies have data.

# 3.1 I extract the row for each subject who has 'Overall 0-2 Years' assessment (adarr_OBJREL_02year 'list of dataframes') then
nsubjects <- length(unique(adarr$USUBJID))
usubjectID <- unique(adarr$USUBJID)

adarr_OBJREL_02year_ds <- sapply(1:nsubjects, function(i) subset(adarr_OBJREL,subset = adarr_OBJREL$USUBJID == as.character(usubjectID[i]) & adarr_OBJREL$AVISIT=='Overall 0-2 Years'),
                              simplify = F)

# 3.2 I check the nrow() of each data frame, if nrow==0 then there is no assessment for 'Overall 0-2 Years'
# If not (nrow==1), then I check if the patient relapsed or not ('SUBJRR' differ from zero or not).

RELAPSE02Year <- 1:nsubjects
for (i in 1:nsubjects) {
  
  if(nrow(adarr_OBJREL_02year_ds[[i]]) == 0){
    RELAPSE02Year[i] <- NA
  }else{
    if(adarr_OBJREL_02year_ds[[i]]$AVAL>0){
      RELAPSE02Year[i] <- 1
      
    }else{
      RELAPSE02Year[i] <- 0
      
    }
  }
  
}

## 3.3 Add the two dummy variables (RELAPSE01Year and RELAPSE02Year) to ADSL dataset now it is adsl01
adsl01 <- adsl
adsl01$RELAPSE1year <- RELAPSE01Year
adsl01$RELAPSE2year <- RELAPSE02Year

## 4. Check if the calculated dummy variables are correct.

# Change the names of the studies

# Table for each study how many patients have and have not relapsed
table(adarr_OBJREL$AVISIT[adarr_OBJREL$AVAL>0],adarr_OBJREL$STUDYID[adarr_OBJREL$AVAL>0])
table(adarr_OBJREL$AVISIT[adarr_OBJREL$AVAL==0],adarr_OBJREL$STUDYID[adarr_OBJREL$AVAL==0])

# within one year
table(adsl01$STUDYID,adsl01$RELAPSE1year)

# within 2 years
table(adsl01$STUDYID,adsl01$RELAPSE2year)

#%%%%% 
# the calculations within this IPD dataset to calculate the dummy varaibles  are correct.
#%%%%% 

## 5. Check if the IPD compatible with AD (form MS file)

# 5.1 read MS aggregated dataset
MS <- read_excel('/Users/tasneem/Desktop/TasnimPhD/multiple sclerosis/IPD data from 6 Biogen trials/MSaggregated.xlsx')
MS <- as.data.frame(MS)

# 5.2 change names of the studies
levels(adsl01$STUDYID) <- c('ADVANCE', 'DEFINE', 'CONFIRM','AFFIRM','SENTINEL','MSCRG')

# 5.3 change the names (levels) of the drugs in ADSL dataset
levels(adsl01$TRT01A )[c(2,3,6)] <- c('Dimethyl fumarate','Dimethyl fumarate','Glatiramer acetate')

# 5.4 compare the IPD and AD

# 5.4.1 AD: total number of patients that have relpases/no relapses 
MS_onlyIPDstudies <- MS[MS$study%in%levels(adsl01$STUDYID),][,c('study','r','n')]
AD_01 <- t(sapply(unique(MS_onlyIPDstudies$study),function(i) {
  rtotal <- sum(MS_onlyIPDstudies$r[MS_onlyIPDstudies$study==i])
  ntotal <- sum(MS_onlyIPDstudies$n[MS_onlyIPDstudies$study==i])
  notrtotal <- ntotal-rtotal
  cbind(notrtotal,rtotal)
  }
  ))
colnames(AD_01) <- c('AD0', 'AD1')

# 5.4.2 IPD: total number of patients that have relpases/no relapses
IPD1 <-table(adarr_OBJREL$AVISIT[adarr_OBJREL$AVAL>0],adarr_OBJREL$STUDYID[adarr_OBJREL$AVAL>0])[8,c(6,4,2,3)]
IPD0<-table(adarr_OBJREL$AVISIT[adarr_OBJREL$AVAL==0],adarr_OBJREL$STUDYID[adarr_OBJREL$AVAL==0])[8,c(6,4,2,3)]

#%%%%%%%%%%%% Table of Comparison AD and IPD: the number of the relapsed patients and nonrealpsed patients
compare_AD_IPD <- cbind(AD_01,IPD0,IPD1)
compare_AD_IPD


