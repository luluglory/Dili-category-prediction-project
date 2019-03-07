## Import the library
library(xlsx)
library(ggplot2)
library(ggthemes)
library(tidyr)

## Import the dataset
getwd()
setwd('C:/Users/morty/Desktop/study/Capstone')
getwd()
Dili <-read.csv('medNAS_dili_tt.csv') 

## count the number of drugs(Todrug), target(Totaeget), Diliclass(Toclass)
Todrug <- length(Dili$chnm)
Totarget <- length(Dili$tgt_abbr)
Toclass <- length(Dili$Classification)

## Get the unique value of target
Target <- c(as.character(unique(Dili$tgt_abbr)))

## calculate the probability of the occurence of each target PT.
## Create the dataframe PTdf have the target name and probability of the occurence of each target (Vlaue)
P_target <- c()
for (i in Target){
  P_target[i] <- sum(Dili$tgt_abbr==i)
  PT <- c(P_target/Totarget)
}
PTdf <- data.frame(value = PT)
PTdf['target']=rownames(PTdf)

## Create the dataframe PDdf have the target name and probability of the occurence of each class (Vlaue). PD is P(Dili category)
DILIC <- c(as.character(unique(Dili$Classification)))
P_DILI <- c()
for (i in DILIC){
  P_DILI[i] <- sum(Dili$Classification==i)
  PD <- c(P_DILI/Toclass)
}
PDdf <- data.frame(value = PD)
PDdf['Classification']=rownames(PDdf)

## count the number of sample for each dili catagory
MDILI <- Dili[Dili$Classification == 'MostDILI Drugs',]
ADILI <- Dili[Dili$Classification == 'AmbiDILIDrugs',]
LDILI <- Dili[Dili$Classification == 'LessDILIDrugs',]
NDILI <- Dili[Dili$Classification == 'NoDILI Drugs',]
totalM <- length(MDILI$tgt_abbr)
totalL <- length(LDILI$tgt_abbr)
totalA <- length(ADILI$tgt_abbr)
totalN <- length(NDILI$tgt_abbr)

## Create the dataframe PTdf have the target name and probability of the conditional situation that 
## when the dili catagory is mostdili the probability of the occurance of each target. PMT is P(target|mostdili)
MTarget <- c(as.character(unique(MDILI$tgt_abbr)))
P_Mtarget <- c()
for (i in MTarget){
  P_Mtarget[i] <- sum(MDILI$tgt_abbr==i)
  PMT <- c(P_Mtarget/totalM)
}
PMTdf <- data.frame(value = PMT)
PMTdf['target']=rownames(PMTdf)


## Calculate the conditional probability P(mostdili | target)
ProTM <- c()
for(x in PTdf$target){
  for(y in PMTdf$target){
    if(x == y){
      ProTM[y] <- round(PMTdf[y,'value']*PDdf['MostDILI Drugs','value']/PTdf[x,'value'],2)
    }
  }
}
ProTMdf <-data.frame(value = ProTM)
ProTMdf['target']=rownames(ProTMdf)


## Calculate the conditional probability P(Ambidili | target)
ATarget <- c(as.character(unique(ADILI$tgt_abbr)))
P_Atarget <- c()
for (i in ATarget){
  P_Atarget[i] <- sum(ADILI$tgt_abbr==i)
  PAT <- c(P_Atarget/totalA)
}
PATdf <- data.frame(value = PAT)
PATdf['target']=rownames(PATdf)

ProTA <- c()
for(x in PTdf$target){
  for(y in PATdf$target){
    if(x == y){
      ProTA[y] <- round(PATdf[y,'value']*PDdf['AmbiDILIDrugs','value']/PTdf[x,'value'],2)
    }
  }
}
ProTAdf <-data.frame(value = ProTA)
ProTAdf['target']=rownames(ProTAdf)

## Calculate the conditional probability P(Lessdili | target)
LTarget <- c(as.character(unique(LDILI$tgt_abbr)))
P_Ltarget <- c()
for (i in LTarget){
  P_Ltarget[i] <- sum(LDILI$tgt_abbr==i)
  PLT <- c(P_Ltarget/totalL)
}
PLTdf <- data.frame(value = PLT)
PLTdf['target']=rownames(PLTdf)

ProTL <- c()
for(x in PTdf$target){
  for(y in PLTdf$target){
    if(x == y){
      ProTL[y] <- round(PLTdf[y,'value']*PDdf[ 'LessDILIDrugs','value']/PTdf[x,'value'],2)
    }
  }
}
ProTLdf <-data.frame(value = ProTL)
ProTLdf['target']=rownames(ProTLdf)

## Calculate the conditional probability P(Nodili | target)
NTarget <- c(as.character(unique(NDILI$tgt_abbr)))
P_Ntarget <- c()
for (i in NTarget){
  P_Ntarget[i] <- sum(NDILI$tgt_abbr==i)
  PNT <- c(P_Ntarget/totalN)
}
PNTdf <- data.frame(value = PNT)
PNTdf['target']=rownames(PNTdf)

ProTN <- c()
for(x in PTdf$target){
  for(y in PNTdf$target){
    if(x == y){
      ProTN[y] <- round(PNTdf[y,'value']*PDdf[ 'NoDILI Drugs','value']/PTdf[x,'value'],2)
    }
  }
}
ProTNdf <-data.frame(value = ProTN) 
ProTNdf['target']=rownames(ProTNdf)

## Create the Dili category & Assay target association table
MDC <- ProTMdf[ProTMdf['value']==1,]
MDC['Dili Catagory']='MostDILI Drugs'
ADC <- ProTAdf[ProTAdf['value']==1,]
ADC['Dili Catagory']='AmbiDILIDrugs'
LDC <- ProTLdf[ProTLdf['value']==1,]
LDC['Dili Catagory']='LessDILIDrugs'
NDC <- ProTNdf[ProTNdf['value']==1,]
NDC['Dili Catagory']='NoDILI Drugs'

MDC1 <- MDC
MDC1['MostDili'] = 1
MDC2 <- MDC1[,c('target','MostDili')]

ADC1 <- ADC
ADC1['AmbiDili'] = 1
ADC2 <- ADC1[,c('target','AmbiDili')]

LDC1 <- LDC
LDC1['LessDili'] = 1
LDC2 <- LDC1[,c('target','LessDili')]

NDC1 <- NDC
NDC1['NoDili'] = 1
NDC2 <- NDC1[,c('target','NoDili')]

TvsDili1 <- merge(MDC2,ADC2, by.x = "target", by.y = "target",all=TRUE)
TvsDili2 <- merge(TvsDili1,LDC2, by.x = "target", by.y = "target",all=TRUE)
TvsDili <- merge(TvsDili2,NDC2, by.x = "target", by.y = "target",all=TRUE)
TvsDili[is.na(TvsDili)]<-0

