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

## Sperate the target into two category by NAS value
DiliP <- Dili
DiliP[DiliP$NAS>=0,'Act_level']='High'
DiliP[DiliP$NAS<=0,'Act_level']='low'
DiliP <- unite(DiliP,'Tgt_level',tgt_abbr,Act_level,sep = '_')

Test1<- Dili[,c('chnm','Classification','NAS','tgt_abbr')]
Test2<- DiliP[,c('chnm','Classification','Tgt_level')]
