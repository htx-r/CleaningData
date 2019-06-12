##%%%%%%%%

library(devtools)
install_github('htx-r/CleaningData')
library(CleaningData)
ls(package:CleaningData)
## 4. Check if the calculated dummy variables are correct.
datapath="/Users/tasneem/Desktop/TasnimPhD/multiple sclerosis/IPD data from 6 Biogen trials"

cleanBIOGENtrials <- cleanBIOGENtrials.fun(datapath)
adarr_OBJREL <- cleanBIOGENtrials$adarr_OBJREL
adsl01 <- cleanBIOGENtrials$adsl01
# Table for each study how many patients have and have not relapsed
as.matrix(table(adarr_OBJREL$AVISIT[adarr_OBJREL$AVAL>0],adarr_OBJREL$STUDYID[adarr_OBJREL$AVAL>0]))+as.matrix(table(adarr_OBJREL$AVISIT[adarr_OBJREL$AVAL==0],adarr_OBJREL$STUDYID[adarr_OBJREL$AVAL==0]))


# Table for each study how many patients in each drugs
table(adarr_OBJREL$TRTA[adarr_OBJREL$AVISIT=='Overall 0-2 Years'],adarr_OBJREL$STUDYID[adarr_OBJREL$AVISIT=='Overall 0-2 Years'])
#apply(as.matrix(table(adarr_OBJREL$TRTA,adarr_OBJREL$STUDYID))#,2,sum)


##### check if the number of relapsed in IPD and articles are matched
##### IPD dataset
# within one year
table(adsl01$STUDYID,adsl01$RELAPSE1year)
apply(as.matrix(table(adsl01$STUDYID,adsl01$RELAPSE1year)),1,sum)

# within 2 years
table(adsl01$STUDYID,adsl01$RELAPSE2year)
apply(as.matrix(table(adsl01$STUDYID,adsl01$RELAPSE2year)),1,sum)

##### Articles
# Confirm
totalRelpasedCONFIRM <- 0.41*363 + 0.29*359 + 0.24*345+ 0.32*350
1417-totalRelpasedCONFIRM
#  451 966 ?= 448 969 almost the same

# DEFINE
totalRelpasedDEFINE <- 0.46*408 + 410*0.27+416*0.26
408 + 410+416 - totalRelpasedDEFINE


# AFFIRM
totalRelpasedAFFIRM <-942-477
942 - totalRelpasedAFFIRM
#%%%%%
# the calculations within this IPD dataset to calculate the dummy varaibles  are correct: 
# compare the first row (one year) and the last row (2 year) with the last two tables.
#%%%%%

## 5. Check if the IPD compatible with AD (from MS file)

# 5.1 read MS aggregated dataset
MS <- read_excel('/Users/tasneem/Desktop/TasnimPhD/multiple sclerosis/IPD data from 6 Biogen trials/MSaggregated.xlsx')
MS <- as.data.frame(MS)


# 5.2 compare the IPD and AD

# 5.2.1 AD: total number of patients that have relpases/no relapses
MS_onlyIPDstudies <- MS[MS$study%in%levels(adsl01$STUDYID),][,c('study','r','n')]
AD_01 <- t(sapply(unique(MS_onlyIPDstudies$study),function(i) {
  rtotal <- sum(MS_onlyIPDstudies$r[MS_onlyIPDstudies$study==i])
  ntotal <- sum(MS_onlyIPDstudies$n[MS_onlyIPDstudies$study==i])
  notrtotal <- ntotal-rtotal
  cbind(notrtotal,rtotal)
}
))
colnames(AD_01) <- c('AD0', 'AD1')

# 5.2.2 IPD: total number of patients that have relpases/no relapses
IPD1 <-table(adarr_OBJREL$AVISIT[adarr_OBJREL$AVAL>0],adarr_OBJREL$STUDYID[adarr_OBJREL$AVAL>0])[8,c(6,4,2,3)]
IPD0<-table(adarr_OBJREL$AVISIT[adarr_OBJREL$AVAL==0],adarr_OBJREL$STUDYID[adarr_OBJREL$AVAL==0])[8,c(6,4,2,3)]

#%%%%%%%%%%%% Table of Comparison AD and IPD: the number of the relapsed patients and nonrealpsed patients
compare_AD_IPD <- cbind(AD_01,IPD0,IPD1)
compare_AD_IPD

## The sample size for each study in AD and IPD are almost equivalent but the number of relapsed and none relapsed differ. 
apply(compare_AD_IPD[,1:2], 1,sum)
apply(compare_AD_IPD[,3:4], 1,sum)

