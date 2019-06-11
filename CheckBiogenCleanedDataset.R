library(devtools)
install_github('htx-r/CleaningData')
library(CleaningData)
ls(package:CleaningData)
## 4. Check if the calculated dummy variables are correct.

cleanBIOGENtrials <- cleanBIOGENtrials.fun(datapath)
adarr_OBJREL <- cleanBIOGENtrials$adarr_OBJREL
adsl01 <- cleanBIOGENtrials$adsl01
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
levels(adarr_OBJREL$STUDYID) <- c('ADVANCE','DEFINE','CONFIRM','AFFIRM','SENTINEL','MSCRG')
IPD1 <-table(adarr_OBJREL$AVISIT[adarr_OBJREL$AVAL>0],adarr_OBJREL$STUDYID[adarr_OBJREL$AVAL>0])[8,c(6,4,2,3)]
IPD0<-table(adarr_OBJREL$AVISIT[adarr_OBJREL$AVAL==0],adarr_OBJREL$STUDYID[adarr_OBJREL$AVAL==0])[8,c(6,4,2,3)]

#%%%%%%%%%%%% Table of Comparison AD and IPD: the number of the relapsed patients and nonrealpsed patients
compare_AD_IPD <- cbind(AD_01,IPD0,IPD1)
compare_AD_IPD


