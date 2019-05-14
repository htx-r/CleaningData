
cleanBIOGENtrials.fun=function(datapath){

rm(list=ls())
library(readxl)


  adslpath=paste(mydatapath,"/adsl.csv",sep="")
  adarrpath=paste(mydatapath,"/adarr.csv",sep="")
  adrelpath=paste(mydatapath,"/adrel.csv",sep="")


## Read Biogen dataset that contains the demographic and baseline information
# remove biogen from the name dataset.
adsl <- read.csv(adslpath)
adarr <- read.csv(adarrpath)
adrel <- read.csv(adrelpath)

## Task1: Add two columns(0/1) RELAPSE1year and RELAPSE2year to ADSL dataset
nsubjects <- length(unique(adarr$USUBJID))
usubjectID <- unique(adarr$USUBJID)

## Consider the 'all relapse assessment' method to diagnose MS
adarr_ALLREL <- adarr[adarr$PARAMCD=='OBJREL',]

## transfer the RELAPSE within 1 year to 0/1
adarr_ALLREL01year <- adarr_ALLREL[adarr_ALLREL$AVISIT=='0-1 Year',]
RELAPSE01Year <-  ifelse(adarr_ALLREL01year$SUBJRR>0,1,0)
length(RELAPSE01Year) ## all individuals have 1 year assessment

## transfer the RELAPSE within 2 years to 0/1: 2 steps


# Not all the patients have been checked within overall 0-2 Years
# 1. I extract the row for each subject who has 'Overall 0-2 Years' assessment (adarr_ALLREL_02year 'list of dataframes') then
# 2. I check the nrow() of each data frame, if nrow==0 then there is no assessment for 'Overall 0-2 Years'
   # If not (nrow==1), then I check if the patient relapsed or not ('SUBJRR' differ from zero or not).

# 1.
adarr_ALLREL_02year <- sapply(1:nsubjects, function(i) subset(adarr_ALLREL,subset = adarr_ALLREL$USUBJID == as.character(usubjectID[i]) & adarr_ALLREL$AVISIT=='Overall 0-2 Years'),
             simplify = F)


# 2.
RELAPSE02Year <- 1:nsubjects
for (i in 1:nsubjects) {

  if(nrow(adarr_ALLREL_02year[[i]]) == 0){
    RELAPSE02Year[i] <- NA
  }else{
    if(adarr_ALLREL_02year[[i]]$SUBJRR>0){
      RELAPSE02Year[i] <- 1

    }else{
      RELAPSE02Year[i] <- 0

    }
  }

}

## Add the two columns to ADSL dataset
adsl01 <- adsl
adsl01$RELAPSE1year <- RELAPSE01Year
adsl01$RELAPSE2year <- RELAPSE02Year

### Check:
# Table for each study how many patients have and have not relapsed

# within 2 years
m <-as.matrix(table(adsl01$STUDYID,adsl01$RELAPSE2year))
mm <- apply(m, 1, sum)
mm

# within one year
m1 <-as.matrix(table(adsl01$STUDYID,adsl01$RELAPSE1year))
mm1 <- apply(m1, 1, sum)
mm1

# Table: for each study, the total number of relpases over time
table(adarr_ALLREL$AVISIT,adarr_ALLREL$STUDYID)

# Notice that the values in mm equals the last line in the last table

# Overall 0-2 Years        0     1234     1417    939   1171     301

# While the values in mm1 equals the first line in the last table

#  0-1 Year              1512     1234     1417    939   1171     301

#%%%% I think now everything is correct

## Task2: Correct drug names/study names to be matched with drug names/study names in MS dataset
   #%%%%% change the levels of adsl$TRT01A to MSdataset
## read MS aggregated dataset
MSdataset <- read_excel('MSaggregated.xlsx')
MSdataset <- as.data.frame(MSdataset)
names(table(MSdataset$treat))

levels(adsl01$TRT01A )

## Change the names (levels) of the drugs in ADSL dataset
levels(adsl01$TRT01A )[c(2,3,6)] <- c('Dimethyl fumarate','Dimethyl fumarate','Glatiramer acetate')

## Change the names (levels) of the studies names in ADSL dataset

levels(adsl01$STUDYID) <- c('ADVANCE', 'DEFINE', 'CONFIRM','AFFIRM','SENTINEL','MSCRG')
#


####!!!!!!!! remove what data is no longer needed
}
