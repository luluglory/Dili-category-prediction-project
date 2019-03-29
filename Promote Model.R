## Import the library
library(xlsx)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)

## Import the dataset
getwd()
setwd('C:/Users/morty/Desktop/study/Capstone')
getwd()
Dili <-read.csv('medNAS_dili_tt.csv') 

## Sperate the target into two category by NAS value
DiliP <- Dili
DiliP[DiliP$NAS>=0,'Act_level']='High'
DiliP[DiliP$NAS<=0,'Act_level']='low'
DiliP <- unite(DiliP,'Tgt_level',tgt_abbr,Act_level,sep = ',')

## count the number of drugs(Todrug), target(Totaeget), Diliclass(Toclass)
Todrug <- length(DiliP$chnm)
Totarget <- length(DiliP$Tgt_level)
Toclass <- length(DiliP$Classification)

## Get the unique value of target
Targetlv <- c(as.character(unique(DiliP$Tgt_level)))

## calculate the probability of the occurence of each target with activation level PT.
## Create the dataframe PTdf have the target name and probability of the occurence of each target with activation level (Value)
P_targetL <- c()
for (i in Targetlv){
  P_targetL[i] <- sum(DiliP$Tgt_level==i)
  PT <- c(P_targetL/Totarget)
}
PTLdf <- data.frame(value = PT)
PTLdf['target']=rownames(PTLdf)

## Create the dataframe PDdf have the target name and probability of the occurence of each DILI class (Value). PD is P(Dili category)
DILIC <- c(as.character(unique(Dili$Classification)))
P_DILI <- c()
for (i in DILIC){
  P_DILI[i] <- sum(Dili$Classification==i)
  PD <- c(P_DILI/Toclass)
}
PDdf <- data.frame(value = PD)
PDdf['Classification']=rownames(PDdf)

## count the number of sample for each dili catagory
MDILI <- DiliP[DiliP$Classification == 'MostDILI Drugs',]
ADILI <- DiliP[DiliP$Classification == 'AmbiDILIDrugs',]
LDILI <- DiliP[DiliP$Classification == 'LessDILIDrugs',]
NDILI <- DiliP[DiliP$Classification == 'NoDILI Drugs',]
totalM <- length(MDILI$Tgt_level)
totalL <- length(LDILI$Tgt_level)
totalA <- length(ADILI$Tgt_level)
totalN <- length(NDILI$Tgt_level)


## Create the dataframe PTdf have the target name and probability of the conditional situation that 
## when the dili catagory is mostdili the probability of the occurance of each target level. PMT is P(targetwithActivationlevel|mostdili)
MTarget <- c(as.character(unique(MDILI$Tgt_level)))
P_Mtarget <- c()
for (i in MTarget){
  P_Mtarget[i] <- sum(MDILI$Tgt_level==i)
  PMTL <- c(P_Mtarget/totalM)
}
PMTLdf <- data.frame(value = PMTL)
PMTLdf['target']=rownames(PMTLdf)

## Calculate the conditional probability P(mostdili | targetwithActivationLevel)
ProTLM <- c()
for(x in PTLdf$target){
  for(y in PMTLdf$target){
    if(x == y){
      ProTLM[y] <- round(PMTLdf[y,'value']*PDdf['MostDILI Drugs','value']/PTLdf[x,'value'],2)
    }
  }
}
ProTMLdf <-data.frame(value = ProTLM)
ProTMLdf['target']=rownames(ProTMLdf)

## Calculate the conditional probability P(Ambidili | targetwithActivationLevel)
ATarget <- c(as.character(unique(ADILI$Tgt_level)))
P_Atarget <- c()
for (i in ATarget){
  P_Atarget[i] <- sum(ADILI$Tgt_level==i)
  PATL <- c(P_Atarget/totalA)
}
PATLdf <- data.frame(value = PATL)
PATLdf['target']=rownames(PATLdf)

ProTLA <- c()
for(x in PTLdf$target){
  for(y in PATLdf$target){
    if(x == y){
      ProTLA[y] <- round(PATLdf[y,'value']*PDdf['AmbiDILIDrugs','value']/PTLdf[x,'value'],2)
    }
  }
}
ProTALdf <-data.frame(value = ProTLA)
ProTALdf['target']=rownames(ProTALdf)

## Calculate the conditional probability P(Lessdili | targetwithActivationLevel)
LTarget <- c(as.character(unique(LDILI$Tgt_level)))
P_Ltarget <- c()
for (i in LTarget){
  P_Ltarget[i] <- sum(LDILI$Tgt_level==i)
  PLTL <- c(P_Ltarget/totalL)
}
PLTLdf <- data.frame(value = PLTL)
PLTLdf['target']=rownames(PLTLdf)

ProTLL <- c()
for(x in PTLdf$target){
  for(y in PLTLdf$target){
    if(x == y){
      ProTLL[y] <- round(PLTLdf[y,'value']*PDdf['LessDILIDrugs','value']/PTLdf[x,'value'],2)
    }
  }
}
ProTLLdf <-data.frame(value = ProTLL)
ProTLLdf['target']=rownames(ProTLLdf)

## Calculate the conditional probability P(Ambidili | targetwithActivationLevel)
NTarget <- c(as.character(unique(NDILI$Tgt_level)))
P_Ntarget <- c()
for (i in NTarget){
  P_Ntarget[i] <- sum(NDILI$Tgt_level==i)
  PNTL <- c(P_Ntarget/totalN)
}
PNTLdf <- data.frame(value = PNTL)
PNTLdf['target']=rownames(PNTLdf)

ProTLN <- c()
for(x in PTLdf$target){
  for(y in PNTLdf$target){
    if(x == y){
      ProTLN[y] <- round(PNTLdf[y,'value']*PDdf['NoDILI Drugs','value']/PTLdf[x,'value'],2)
    }
  }
}
ProTNLdf <-data.frame(value = ProTLN)
ProTNLdf['target']=rownames(ProTNLdf)

## Create the Dili category & Assay target association table
MDC <- ProTMLdf[ProTMLdf['value']==1,]
MDC['Dili Catagory']='MostDILI Drugs'
ADC <- ProTALdf[ProTALdf['value']==1,]
ADC['Dili Catagory']='AmbiDILIDrugs'
LDC <- ProTLLdf[ProTLLdf['value']==1,]
LDC['Dili Catagory']='LessDILIDrugs'
NDC <- ProTNLdf[ProTNLdf['value']==1,]
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

TLvsDili1 <- merge(MDC2,ADC2, by.x = "target", by.y = "target",all=TRUE)
TLvsDili2 <- merge(TLvsDili1,LDC2, by.x = "target", by.y = "target",all=TRUE)
TLvsDili <- merge(TLvsDili2,NDC2, by.x = "target", by.y = "target",all=TRUE)
TLvsDili[is.na(TLvsDili)]<-0
TLvsDiliout <- separate(TLvsDili,col=target,into = c('target','Act_level'),sep = ',')

## graph of conditional probability P(mostdili | targetlevel)
ggplot(ProTMLdf,aes(x=reorder(target,value),y=value,fill=ProTMLdf$value))+
  geom_bar(stat='identity')+
  labs (title = 'P(MOST DILI | Target&Activation Level',x= "Target", y = 'P(mostdili | target with activate level)')+
  theme(panel.background=element_rect('white'))+
  theme(title =element_text(size = 40,face = "bold",hjust = 0.5))+
  theme(axis.title.x = element_text(size = 20,face = "bold", vjust = 3, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 20,face = "bold", vjust = 5, hjust = 0.5))+
  theme(panel.grid.major=element_line(colour='lightgrey',linetype="dashed"))+
  theme(axis.text.x=element_text(size=8,colour="gray40",face = "bold", vjust = 0, hjust = 1,angle = 90))+
  theme(axis.text.y=element_text(size=15,colour="gray40",face = "bold"))+
  theme(plot.margin=unit(rep(2,4),'lines'))+
  theme(axis.line = element_line(size=1, colour = 'black'))

## graph of conditional probability P(mostdili | targetlevel)
ggplot(ProTALdf,aes(x=reorder(target,value),y=value,fill=ProTALdf$value))+
  geom_bar(stat='identity')+
  labs (title = 'P(Ambi DILI | Target&Activation Level)',x= "Target", y = 'P(Ambidili | target with activate level)')+
  theme(panel.background=element_rect('white'))+
  theme(title =element_text(size = 40,face = "bold",hjust = 0.5))+
  theme(axis.title.x = element_text(size = 20,face = "bold", vjust = 3, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 20,face = "bold", vjust = 5, hjust = 0.5))+
  theme(panel.grid.major=element_line(colour='lightgrey',linetype="dashed"))+
  theme(axis.text.x=element_text(size=8,colour="gray40",face = "bold", vjust = 0, hjust = 1,angle = 90))+
  theme(axis.text.y=element_text(size=15,colour="gray40",face = "bold"))+
  theme(plot.margin=unit(rep(2,4),'lines'))+
  theme(axis.line = element_line(size=1, colour = 'black'))

## graph of conditional probability P(mostdili | targetlevel)
ggplot(ProTLLdf,aes(x=reorder(target,value),y=value,fill=ProTLLdf$value))+
  geom_bar(stat='identity')+
  labs (title = 'P(LESS DILI | Target&Activation Level)',x= "Target", y = 'P(Lessdili | target with activate level)')+
  theme(panel.background=element_rect('white'))+
  theme(title =element_text(size = 40,face = "bold",hjust = 0.5))+
  theme(axis.title.x = element_text(size = 20,face = "bold", vjust = 3, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 20,face = "bold", vjust = 5, hjust = 0.5))+
  theme(panel.grid.major=element_line(colour='lightgrey',linetype="dashed"))+
  theme(axis.text.x=element_text(size=8,colour="gray40",face = "bold", vjust = 0, hjust = 1,angle = 90))+
  theme(axis.text.y=element_text(size=15,colour="gray40",face = "bold"))+
  theme(plot.margin=unit(rep(2,4),'lines'))+
  theme(axis.line = element_line(size=1, colour = 'black'))

## graph of conditional probability P(mostdili | targetlevel)
ggplot(ProTNLdf,aes(x=reorder(target,value),y=value,fill=ProTNLdf$value))+
  geom_bar(stat='identity')+
  labs (title = 'P(No DILI | Target&Activation Level)',x= "Target", y = 'P(Nodili | target with activate level)')+
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
DiliT <- DiliP
for (i in DiliT$Tgt_level){
  for (y in MDC$target){
    if (i==y){
      DiliT[DiliT$Tgt_level==i,'Test_C']='MostDILI Drugs'
    }
  }
}

for (i in DiliT$Tgt_level){
  for (y in ADC$target){
    if (i==y){
      DiliT[DiliT$Tgt_level==i,'Test_C']='AmbiDILIDrugs'
    }
  }
}

for (i in DiliT$Tgt_level){
  for (y in LDC$target){
    if (i==y){
      DiliT[DiliT$Tgt_level==i,'Test_C']='LessDILIDrugs'
    }
  }
}

for (i in DiliT$Tgt_level){
  for (y in NDC$target){
    if (i==y){
      DiliT[DiliT$Tgt_level==i,'Test_C']='NoDILI Drugs'
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
ProTMLdf['MostDili'] <- ProTMLdf$value
CPM <- ProTMLdf[,c('target','MostDili')]
ProTALdf['AmbiDili'] <- ProTALdf$value
APM <- ProTALdf[,c('target','AmbiDili')]
ProTLLdf['LessDili'] <- ProTLLdf$value
LPM <- ProTLLdf[,c('target','LessDili')]
ProTNLdf['NoDili'] <- ProTNLdf$value
NPM <- ProTNLdf[,c('target','NoDili')]

CD_DiliT1 <- merge(CPM,APM, by.x = "target", by.y = "target",all=TRUE)
CD_DiliT2 <- merge(CD_DiliT1,LPM, by.x = "target", by.y = "target",all=TRUE)
CD_DiliT <- merge(CD_DiliT2,NPM, by.x = "target", by.y = "target",all=TRUE)
CD_DiliT[is.na(CD_DiliT)] <- 0 

CD_DiliTout <- separate(CD_DiliT,col=target,into = c('target','Act_level'),sep = ',')

CD_DiliTX <- CD_DiliT[CD_DiliT$MostDili!=0.5&CD_DiliT$AmbiDili!=0.5&CD_DiliT$LessDili!=0.5&CD_DiliT$NoDili!=0.5,]
CD_DiliTX <- CD_DiliTX[-which(CD_DiliTX$MostDili==0.33&CD_DiliTX$LessDili==0.33),]
CD_DiliTX <- CD_DiliTX[-which(CD_DiliTX$MostDili==0.4&CD_DiliTX$LessDili==0.4),]
CD_DiliTX <- CD_DiliTX[-which(CD_DiliTX$AmbiDili==0.4&CD_DiliTX$LessDili==0.4),]
CD_DiliTX <- CD_DiliTX[-which(CD_DiliTX$LessDili==0.4&CD_DiliTX$NoDili==0.4),]

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

PredM <- CD_DiliTX[,c('target','MAX','Dili_Category')]

DiliWT <- DiliP
for (i in DiliWT$Tgt_level){
  for (y in PredM[PredM$Dili_Category=='MostDili','target']){
    if (i==y){
      DiliWT[DiliWT$Tgt_level==i,'Test_C']='MostDILI Drugs'
      DiliWT[DiliWT$Tgt_level==i,'MostDili']= PredM[PredM$target==y,'MAX']
    }
  }
}

for (i in DiliWT$Tgt_level){
  for (y in PredM[PredM$Dili_Category=='AmbiDili','target']){
    if (i==y){
      DiliWT[DiliWT$Tgt_level==i,'Test_C']='AmbiDILI Drugs'
      DiliWT[DiliWT$Tgt_level==i,'AmbiDili']= PredM[PredM$target==y,'MAX']
    }
  }
}

for (i in DiliWT$Tgt_level){
  for (y in PredM[PredM$Dili_Category=='LessDili','target']){
    if (i==y){
      DiliWT[DiliWT$Tgt_level==i,'Test_C']='LessDILI Drugs'
      DiliWT[DiliWT$Tgt_level==i,'LessDili']= PredM[PredM$target==y,'MAX']
    }
  }
}

for (i in DiliWT$Tgt_level){
  for (y in PredM[PredM$Dili_Category=='NoDili','target']){
    if (i==y){
      DiliWT[DiliWT$Tgt_level==i,'Test_C']='NoDILI Drugs'
      DiliWT[DiliWT$Tgt_level==i,'NoDili']= PredM[PredM$target==y,'MAX']
    }
  }
}

DiliWT_dup <- DiliWT[!duplicated(DiliWT[,c('chnm','Test_C')]),]
DiliWTM <- na.omit(DiliWT_dup[DiliWT_dup$Test_C=='MostDILI Drugs',c('chnm','MostDili')])
DiliWTA <- na.omit(DiliWT_dup[DiliWT_dup$Test_C=='AmbiDILI Drugs',c('chnm','AmbiDili')])
DiliWTL <- na.omit(DiliWT_dup[DiliWT_dup$Test_C=='LessDILI Drugs',c('chnm','LessDili')])
DiliWTN <- na.omit(DiliWT_dup[DiliWT_dup$Test_C=='NoDILI Drugs',c('chnm','NoDili')])

TvsCW1 <- merge(DiliWTM,DiliWTA, by.x = "chnm", by.y = "chnm",all=TRUE)
TvsCW2 <- merge(TvsCW1,DiliWTL, by.x = "chnm", by.y = "chnm",all=TRUE)
TvsCW <- merge(TvsCW2,DiliWTN, by.x = "chnm", by.y = "chnm",all=TRUE)
TvsCW[is.na(TvsCW)] <- 0 

for (i in rownames(TvsCW)){
  TvsCW[i,'MAX']= max(TvsCW[i,'MostDili'],TvsCW[i,'AmbiDili'],TvsCW[i,'LessDili'],TvsCW[i,'NoDili'])
}

for (i in rownames(TvsCW)){
  if (TvsCW[i,'MostDili']==TvsCW[i,'MAX']){
    TvsCW[i,'MostDiliC']=1
  }
}

for (i in rownames(TvsCW)){
  if (TvsCW[i,'AmbiDili']==TvsCW[i,'MAX']){
    TvsCW[i,'AmbiDiliC']=1
  }
}

for (i in rownames(TvsCW)){
  if (TvsCW[i,'LessDili']==TvsCW[i,'MAX']){
    TvsCW[i,'LessDiliC']=1
  }
}

for (i in rownames(TvsCW)){
  if (TvsCW[i,'NoDili']==TvsCW[i,'MAX']){
    TvsCW[i,'NoDiliC']=1
  }
}

TvsCW[is.na(TvsCW)] <- 0
PdRs1<-TvsCW[,c('chnm','MostDiliC','AmbiDiliC','LessDiliC','NoDiliC')]
Drug_Cl<- Dili[!duplicated(Dili[,c('chnm','Classification')]),c('chnm','Classification')]

PdRs <- merge(PdRs1,Drug_Cl, by.x = "chnm", by.y = "chnm",all.x = TRUE)
PdRs['sum'] <- PdRs$MostDiliC+PdRs$AmbiDiliC+PdRs$LessDiliC+PdRs$NoDili
PdRs <- PdRs[PdRs$sum == 1,c('chnm','MostDiliC','AmbiDiliC','LessDiliC','NoDiliC','Classification')]

# Export the dataset
write.xlsx(TLvsDiliout, "Predictive Model 1.xlsx")
write.xlsx(TvsC, "Prediction Result 1.xlsx")
write.xlsx(CD_DiliTX, "Prediction Model 2.xlsx")
write.xlsx(PdRs, "Prediction Result 2.xlsx")
