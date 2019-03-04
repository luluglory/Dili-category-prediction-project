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
View(Dili)
colnames(Dili)

## Explore the dataset
summary(Dili)
unique(Dili$tgt_abbr)
unique(Dili$chnm)
unique(Dili$Classification)

## count the number of drugs(Todrug), target(Totaeget), Diliclass(Toclass)
Todrug <- length(Dili$chnm)
Totarget <- length(Dili$tgt_abbr)
Toclass <- length(Dili$Classification)

## Get the unique value of target
Target <- c(as.character(unique(Dili$tgt_abbr)))

## calculate the probability of the occurence of each target PT.
P_target <- c()
for (i in Target){
  P_target[i] <- sum(Dili$tgt_abbr==i)
  PT <- c(P_target/Totarget)
}

## Create the dataframe PTdf have the target name and probability of the occurence of each target (Vlaue).
PTdf <- data.frame(target = Target, value = PT)
PTdf

## Create the dataframe PTdf have the target name and probability of the occurence of each class (Vlaue). PD is P(Dili category)
DILIC <- c(as.character(unique(Dili$Classification)))
P_DILI <- c()
for (i in DILIC){
  P_DILI[i] <- sum(Dili$Classification==i)
  PD <- c(P_DILI/Toclass)
}
PDdf <- data.frame(target = DILIC, value = PD)
PDdf

## count the number of sample for each dili catagory
MDILI <- Dili[Dili$Classification == 'MostDILI Drugs',]
ADILI <- Dili[Dili$Classification == 'AmbiDILIDrugs',]
LDILI <- Dili[Dili$Classification == 'LessDILIDrugs',]
NDILI <- Dili[Dili$Classification == 'NoDILI Drugs',]

## Create the dataframe PTdf have the target name and probability of the conditional situation that when the dili 
## catagory is mostdili the probability of the occurance of each target. PMT is P(target|mostdili)
totalM <- length(MDILI$tgt_abbr)
MTarget <- c(as.character(unique(MDILI$tgt_abbr)))
P_Mtarget <- c()
for (i in MTarget){
  P_Mtarget[i] <- sum(MDILI$tgt_abbr==i)
  PMT <- c(P_Mtarget/totalM)
}
PMTdf <- data.frame(target = MTarget, value = PMT)

## Calculate the conditional probability P(mostdili | target)
ProTM <- c()
for(x in PTdf$target){
  for(y in PMTdf$target){
    if(x == y){
      ProTM[y] <- PMTdf[y,'value']*PDdf['MostDILI Drugs','value']/PTdf[x,'value']
    }
  }
}
ProTMdf <-data.frame(target = PMTdf$target, value = ProTM) 

## graph of conditional probability P(mostdili | target)
ggplot(ProTMdf,aes(x=reorder(target,value),y=value))+
  geom_bar(stat='identity',fill='lightslateblue')+
  labs (x= "Target", y = 'P(mostdili | target)')+
  theme(panel.background=element_rect('white'))+
  theme(axis.title.x = element_text(size = 20,face = "bold", vjust = 3, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 20,face = "bold", vjust = 5, hjust = 0.5))+
  theme(panel.grid.major=element_line(colour='lightgrey',linetype="dashed"))+
  theme(axis.text.x=element_text(size=8,colour="gray40",face = "bold", vjust = 0, hjust = 1,angle = 90))+
  theme(axis.text.y=element_text(size=15,colour="gray40",face = "bold"))+
  theme(plot.margin=unit(rep(2,4),'lines'))+
  theme(axis.line = element_line(size=1, colour = 'black'))

## Calculate the conditional probability P(AmbiDili | target)
totalA <- length(ADILI$tgt_abbr)
ATarget <- c(as.character(unique(ADILI$tgt_abbr)))
P_Atarget <- c()
for (i in ATarget){
  P_Atarget[i] <- sum(ADILI$tgt_abbr==i)
  PAT <- c(P_Atarget/totalA)
}
PATdf <- data.frame(target = ATarget, value = PAT)
ProTA <- c()

for(x in PTdf$target){
  for(y in PATdf$target){
    if(x == y){
      ProTA[y] <- PATdf[y,'value']*PDdf['AmbiDILIDrugs','value']/PTdf[x,'value']
    }
  }
}
ProTAdf <-data.frame(target = PATdf$target, value = ProTA)   

## graph of conditional probability P(Ambidili | target)
ggplot(ProTAdf,aes(x=reorder(target,value),y=value))+
  geom_bar(stat='identity',fill='lightslateblue')+
  labs (x= "Target", y = 'P(Ambidili | target)')+
  theme(panel.background=element_rect('white'))+
  theme(axis.title.x = element_text(size = 20,face = "bold", vjust = 3, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 20,face = "bold", vjust = 5, hjust = 0.5))+
  theme(panel.grid.major=element_line(colour='lightgrey',linetype="dashed"))+
  theme(axis.text.x=element_text(size=8,colour="gray40",face = "bold", vjust = 0, hjust = 1,angle = 90))+
  theme(axis.text.y=element_text(size=15,colour="gray40",face = "bold"))+
  theme(plot.margin=unit(rep(2,4),'lines'))+
  theme(axis.line = element_line(size=1, colour = 'black'))

## Calculate the conditional probability P(LessDili | target)
totalL <- length(LDILI$tgt_abbr)
LTarget <- c(as.character(unique(LDILI$tgt_abbr)))
P_Ltarget <- c()
for (i in LTarget){
  P_Ltarget[i] <- sum(LDILI$tgt_abbr==i)
  PLT <- c(P_Ltarget/totalL)
}
PLTdf <- data.frame(target = LTarget, value = PLT)

ProTL <- c()
for(x in PTdf$target){
  for(y in PLTdf$target){
    if(x == y){
      ProTL[y] <- PLTdf[y,'value']*PDdf[ 'LessDILIDrugs','value']/PTdf[x,'value']
    }
  }
}
ProTLdf <-data.frame(target = PLTdf$target, value = ProTL)

## graph of conditional probability P(LessDili | target)
ggplot(ProTLdf,aes(x=reorder(target,value),y=value))+
  geom_bar(stat='identity',fill='lightslateblue')+
  labs (x= "Target", y = 'P(LessDili | target)')+
  theme(panel.background=element_rect('white'))+
  theme(axis.title.x = element_text(size = 20,face = "bold", vjust = 3, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 20,face = "bold", vjust = 5, hjust = 0.5))+
  theme(panel.grid.major=element_line(colour='lightgrey',linetype="dashed"))+
  theme(axis.text.x=element_text(size=8,colour="gray40",face = "bold", vjust = 0, hjust = 1,angle = 90))+
  theme(axis.text.y=element_text(size=15,colour="gray40",face = "bold"))+
  theme(plot.margin=unit(rep(2,4),'lines'))+
  theme(axis.line = element_line(size=1, colour = 'black'))

## Calculate the conditional probability P(NoDili | target)
totalN <- length(NDILI$tgt_abbr)
NTarget <- c(as.character(unique(NDILI$tgt_abbr)))
P_Ntarget <- c()
for (i in NTarget){
  P_Ntarget[i] <- sum(NDILI$tgt_abbr==i)
  PNT <- c(P_Ntarget/totalN)
}
PNTdf <- data.frame(target = NTarget, value = PNT)

ProTN <- c()
for(x in PTdf$target){
  for(y in PNTdf$target){
    if(x == y){
      ProTN[y] <- PNTdf[y,'value']*PDdf[ 'NoDILI Drugs','value']/PTdf[x,'value']
    }
  }
}
ProTNdf <-data.frame(target = PNTdf$target, value = ProTN)

## graph of conditional probability P(NoDili | target)
ggplot(ProTNdf,aes(x=reorder(target,value),y=value))+
  geom_bar(stat='identity',fill='lightslateblue')+
  labs (x= "Target", y = 'P(NoDili | target)')+
  theme(panel.background=element_rect('white'))+
  theme(axis.title.x = element_text(size = 20,face = "bold", vjust = 3, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 20,face = "bold", vjust = 5, hjust = 0.5))+
  theme(panel.grid.major=element_line(colour='lightgrey',linetype="dashed"))+
  theme(axis.text.x=element_text(size=8,colour="gray40",face = "bold", vjust = 0, hjust = 1,angle = 90))+
  theme(axis.text.y=element_text(size=15,colour="gray40",face = "bold"))+
  theme(plot.margin=unit(rep(2,4),'lines'))+
  theme(axis.line = element_line(size=1, colour = 'black'))

