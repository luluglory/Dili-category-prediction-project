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

write.xlsx(TvsDili, "Dili category & Assay target association table.xlsx")

## graph of conditional probability P(mostdili | target)
ggplot(ProTMdf,aes(x=reorder(target,value),y=value,fill=ProTMdf$value))+
  geom_bar(stat='identity')+
  labs (title = 'P(MOST DILI | Target)',x= "Target", y = 'P(mostdili | target)')+
  theme(panel.background=element_rect('white'))+
  theme(title =element_text(size = 40,face = "bold",hjust = 0.5))+
  theme(axis.title.x = element_text(size = 20,face = "bold", vjust = 3, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 20,face = "bold", vjust = 5, hjust = 0.5))+
  theme(panel.grid.major=element_line(colour='lightgrey',linetype="dashed"))+
  theme(axis.text.x=element_text(size=8,colour="gray40",face = "bold", vjust = 0, hjust = 1,angle = 90))+
  theme(axis.text.y=element_text(size=15,colour="gray40",face = "bold"))+
  theme(plot.margin=unit(rep(2,4),'lines'))+
  theme(axis.line = element_line(size=1, colour = 'black'))

## graph of conditional probability P(Ambidili | target)
ggplot(ProTAdf,aes(x=reorder(target,value),y=value,fill=ProTAdf$value))+
  geom_bar(stat='identity')+
  labs (title = 'P(Ambi DILI | Target)',x= "Target", y = 'P(Ambidili | target)')+
  theme(panel.background=element_rect('white'))+
  theme(title =element_text(size = 40,face = "bold",hjust = 0.5))+
  theme(axis.title.x = element_text(size = 20,face = "bold", vjust = 3, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 20,face = "bold", vjust = 5, hjust = 0.5))+
  theme(panel.grid.major=element_line(colour='lightgrey',linetype="dashed"))+
  theme(axis.text.x=element_text(size=8,colour="gray40",face = "bold", vjust = 0, hjust = 1,angle = 90))+
  theme(axis.text.y=element_text(size=15,colour="gray40",face = "bold"))+
  theme(plot.margin=unit(rep(2,4),'lines'))+
  theme(axis.line = element_line(size=1, colour = 'black'))

## graph of conditional probability P(LessDili | target)
ggplot(ProTLdf,aes(x=reorder(target,value),y=value,fill=ProTLdf$value))+
  geom_bar(stat='identity')+
  labs (title = 'P(LESS DILI | Target)',x= "Target", y = 'P(LessDili | target)')+
  theme(panel.background=element_rect('white'))+
  theme(title =element_text(size = 40,face = "bold",hjust = 0.5))+
  theme(axis.title.x = element_text(size = 20,face = "bold", vjust = 3, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 20,face = "bold", vjust = 5, hjust = 0.5))+
  theme(panel.grid.major=element_line(colour='lightgrey',linetype="dashed"))+
  theme(axis.text.x=element_text(size=8,colour="gray40",face = "bold", vjust = 0, hjust = 1,angle = 90))+
  theme(axis.text.y=element_text(size=15,colour="gray40",face = "bold"))+
  theme(plot.margin=unit(rep(2,4),'lines'))+
  theme(axis.line = element_line(size=1, colour = 'black'))

## graph of conditional probability P(NoDili | target)
ggplot(ProTNdf,aes(x=reorder(target,value),y=value,fill=ProTNdf$value))+
  geom_bar(stat='identity')+
  labs (title = 'P(No DILI | Target)',x= "Target", y = 'P(NoDili | target)')+
  theme(panel.background=element_rect('white'))+
  theme(title =element_text(size = 40,face = "bold",hjust = 0.5))+
  theme(axis.title.x = element_text(size = 20,face = "bold", vjust = 3, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 20,face = "bold", vjust = 5, hjust = 0.5))+
  theme(panel.grid.major=element_line(colour='lightgrey',linetype="dashed"))+
  theme(axis.text.x=element_text(size=8,colour="gray40",face = "bold", vjust = 0, hjust = 1,angle = 90))+
  theme(axis.text.y=element_text(size=15,colour="gray40",face = "bold"))+
  theme(plot.margin=unit(rep(2,4),'lines'))+
  theme(axis.line = element_line(size=1, colour = 'black'))

## Get the Perdiction Result
DiliT <- Dili
for (i in DiliT$tgt_abbr){
  for (y in MDC$target){
    if (i==y){
      DiliT[DiliT$tgt_abbr==i,'Test_C']='MostDILI Drugs'
    }
  }
}

for (i in DiliT$tgt_abbr){
  for (y in ADC$target){
    if (i==y){
      DiliT[DiliT$tgt_abbr==i,'Test_C']='AmbiDILIDrugs'
    }
  }
}

for (i in DiliT$tgt_abbr){
  for (y in LDC$target){
    if (i==y){
      DiliT[DiliT$tgt_abbr==i,'Test_C']='LessDILIDrugs'
    }
  }
}

for (i in DiliT$tgt_abbr){
  for (y in NDC$target){
    if (i==y){
      DiliT[DiliT$tgt_abbr==i,'Test_C']='NoDILI Drugs'
    }
  }
}
DiliT1 <- na.omit(DiliT)
DiliT_dup <- DiliT1[!duplicated(DiliT1[,c('chnm','Test_C')]),]
DiliTM1 <- DiliT_dup[DiliT_dup$Test_C=='MostDILI Drugs',]
DiliTA1 <- DiliT_dup[DiliT_dup$Test_C=='AmbiDILIDrugs',]
DiliTL1 <- DiliT_dup[DiliT_dup$Test_C=='LessDILIDrugs',]
DiliTN1 <- DiliT_dup[DiliT_dup$Test_C=='NoDILI Drugs',]


DiliTM1['MostDILI']=1
DiliTA1['AmbiDILI']=1
DiliTL1['LessDILI']=1
DiliTN1['NoDILI']=1

DiliTM <- DiliTM1[,c('chnm','MostDILI')]
DiliTA <- DiliTA1[,c('chnm','AmbiDILI')]
DiliTL <- DiliTL1[,c('chnm','LessDILI')]
DiliTN <- DiliTN1[,c('chnm','NoDILI')]

TvsC1 <- merge(DiliTM,DiliTA, by.x = "chnm", by.y = "chnm",all=TRUE)
TvsC2 <- merge(TvsC1,DiliTL, by.x = "chnm", by.y = "chnm",all=TRUE)
TvsC3 <- merge(TvsC2,DiliTN, by.x = "chnm", by.y = "chnm",all=TRUE)
DrDlCl <- DiliT_dup[,c("chnm",'Classification')]
TvsC <- merge(TvsC3,DrDlCl, by.x = "chnm", by.y = "chnm",all=TRUE)
TvsC[is.na(TvsC)] <- 0

#Conditional Probability of each Dili Category based on target.
ProTMdf['MostDili'] <- ProTMdf$value
CPM <- ProTMdf[,c('target','MostDili')]
ProTAdf['AmbiDili'] <- ProTAdf$value
APM <- ProTAdf[,c('target','AmbiDili')]
ProTLdf['LessDili'] <- ProTLdf$value
LPM <- ProTLdf[,c('target','LessDili')]
ProTNdf['NoDili'] <- ProTNdf$value
NPM <- ProTNdf[,c('target','NoDili')]

CD_DiliT1 <- merge(CPM,APM, by.x = "target", by.y = "target",all=TRUE)
CD_DiliT2 <- merge(CD_DiliT1,LPM, by.x = "target", by.y = "target",all=TRUE)
CD_DiliT <- merge(CD_DiliT2,NPM, by.x = "target", by.y = "target",all=TRUE)
CD_DiliT[is.na(CD_DiliT)] <- 0 

CD_DiliTX <- CD_DiliT[CD_DiliT$MostDili!=0.5&CD_DiliT$AmbiDili!=0.5&CD_DiliT$LessDili!=0.5&CD_DiliT$NoDili!=0.5,]

#Predict the Dili category
for (i in rownames(CD_DiliTX)){
  CD_DiliTX[i,'MAX']= max(CD_DiliTX[i,'MostDili'],CD_DiliTX[i,'AmbiDili'],CD_DiliTX[i,'LessDili'],CD_DiliTX[i,'NoDili'])
}

for (i in rownames(CD_DiliTX)){
  if (CD_DiliTX[i,'MostDili']==CD_DiliTX[i,'MAX']){
    CD_DiliTX[i,'Dili_Category']='MostDili'
  }
  else if (CD_DiliTX[i,'AmbiDili']==CD_DiliTX[i,'MAX']){
    CD_DiliTX[i,'Dili_Category']='AmbiDili'
  }
  else if (CD_DiliTX[i,'LessDili']==CD_DiliTX[i,'MAX']){
    CD_DiliTX[i,'Dili_Category']='LessDili'
  }
  else if (CD_DiliTX[i,'NoDili']==CD_DiliTX[i,'MAX']){
    CD_DiliTX[i,'Dili_Category']='NoDili'
  }
}

PredM <- CD_DiliTX[,c('target','Dili_Category')]

DiliWT <- Dili
for (i in DiliWT$tgt_abbr){
  for (y in PredM[PredM$Dili_Category=='MostDili','target']){
    if (i==y){
      DiliWT[DiliWT$tgt_abbr==i,'Test_C']='MostDILI Drugs'
    }
  }
}

for (i in DiliWT$tgt_abbr){
  for (y in PredM[PredM$Dili_Category=='AmbiDili','target']){
    if (i==y){
      DiliWT[DiliWT$tgt_abbr==i,'Test_C']='AmbiDILI Drugs'
    }
  }
}

for (i in DiliWT$tgt_abbr){
  for (y in PredM[PredM$Dili_Category=='LessDili','target']){
    if (i==y){
      DiliWT[DiliWT$tgt_abbr==i,'Test_C']='LessDILI Drugs'
    }
  }
}

for (i in DiliWT$tgt_abbr){
  for (y in PredM[PredM$Dili_Category=='NoDili','target']){
    if (i==y){
      DiliWT[DiliWT$tgt_abbr==i,'Test_C']='NoDILI Drugs'
    }
  }
}

DiliWT1 <- na.omit(DiliWT)
DiliWT_dup <- DiliWT1[!duplicated(DiliWT1[,c('chnm','Test_C')]),]
DiliWTM1 <- DiliWT_dup[DiliWT_dup$Test_C=='MostDILI Drugs',]
DiliWTA1 <- DiliWT_dup[DiliWT_dup$Test_C=='AmbiDILI Drugs',]
DiliWTL1 <- DiliWT_dup[DiliWT_dup$Test_C=='LessDILI Drugs',]
DiliWTN1 <- DiliWT_dup[DiliWT_dup$Test_C=='NoDILI Drugs',]

DiliWTM1['MostDILI']=1
DiliWTA1['AmbiDILI']=1
DiliWTL1['LessDILI']=1
DiliWTN1['NoDILI']=1

DiliWTM <- DiliWTM1[,c('chnm','MostDILI')]
DiliWTA <- DiliWTA1[,c('chnm','AmbiDILI')]
DiliWTL <- DiliWTL1[,c('chnm','LessDILI')]
DiliWTN <- DiliWTN1[,c('chnm','NoDILI')]

TvsCW1 <- merge(DiliWTM,DiliWTA, by.x = "chnm", by.y = "chnm",all=TRUE)
TvsCW2 <- merge(TvsCW1,DiliWTL, by.x = "chnm", by.y = "chnm",all=TRUE)
TvsCW3 <- merge(TvsCW2,DiliWTN, by.x = "chnm", by.y = "chnm",all=TRUE)
DrDlCl <- DiliWT_dup[,c("chnm",'Classification')]
DrDLC1 <- DrDlCl[!duplicated(DrDlCl[,c('chnm','Classification')]),]
TvsCW <- merge(TvsCW3,DrDLC1, by.x = "chnm", by.y = "chnm",all=TRUE)
TvsCW[is.na(TvsCW)] <- 0                                                                           

