#######################################################################################
#######################################################################################
#############This file has been developed to extract sample of ORBIS firms ############
#######################################################################################
#######################################################################################

setwd("Y:/ESS/ES10_valuation IP assets/ORBIS_data/full_extract/rds")
lista<-list.files(pattern="orbis.*.rds")

####We have to treat Nl separately, so eliminate it for the time being
lista<-lista[!str_detect(lista,"_NL\\.rds")]

library(reshape2)
library(stringr)
library(data.table)

i<-14

for(i in 17:length(lista))
{
  orb<-readRDS(lista[i])
  orb<-orb[!is.na(orb$BvD.ID.number),]
  cc<-str_extract(lista[i],"[A-Z][A-Z]")
  cc<-ifelse(cc=="GB","UK",cc)
  cc<-ifelse(cc=="GR","EL",cc)
  ###Now check the availability of turnover and employment data
  orb<-as.data.frame(orb)
  turn<-orb[,c("BvD.ID.number",colnames(orb)[str_detect(colnames(orb),"Turnover")])]
  turn<-melt(turn,id.vars=c("BvD.ID.number"))
  turn$variable<-as.character(turn$variable)
  turn$year<-as.integer(str_extract(turn$variable,"....$"))
  turn$variable<-"Turnover"
  length(unique(orb$BvD.ID.number))
  library(data.table)
  turn<-as.data.table(turn)
  turn<-unique(turn)
  turn$a<-ifelse(turn$value=="n.a.",F,T)
  turn<-turn[order(turn$BvD.ID.number,turn$year),]
  turn<-turn[,counter:=seq_len(.N),by=rleid(BvD.ID.number,a)]
  f<-turn[turn$a&turn$counter>2,]
  f$f_year<-f$year-f$counter+1
  f<-f[order(f$BvD.ID.number,-f$year),]
  setkey(f,BvD.ID.number,f_year)
  f<-unique(f,by=c("BvD.ID.number","f_year"))
  f<-f[,c("BvD.ID.number","year","f_year")]
  colnames(f)[2]<-"max_year"
  
  turn<-merge(turn,f,by="BvD.ID.number")
  turn<-turn[turn$year>=turn$f_year&turn$year<=turn$max_year,]
  turn$value<-as.character(turn$value)
  turn$value<-str_replace_all(turn$value,"\\,","")
  turn$value<-as.numeric(turn$value)
  ###check size by turnover
  turn$size<-ifelse(turn$value<=2000,"micro",ifelse(turn$value>2000&turn$value<=10000,"small",ifelse(turn$value>10000&turn$value<=50000,"medium","large")))
  turn$crit<-"turnover"
  
  emp<-orb[,c("BvD.ID.number",colnames(orb)[str_detect(colnames(orb),"employees")])]
  emp<-melt(emp,id.vars=c("BvD.ID.number"))
  emp$variable<-as.character(emp$variable)
  emp$year<-as.integer(str_extract(emp$variable,"....$"))
  emp$variable<-"employees"
  library(data.table)
  emp<-as.data.table(emp)
  emp<-unique(emp)
  emp$a<-ifelse(emp$value=="n.a.",F,T)
  emp<-emp[order(emp$BvD.ID.number,emp$year),]
  emp<-emp[,counter:=seq_len(.N),by=rleid(BvD.ID.number,a)]
  f<-emp[emp$a&emp$counter>2,]
  f$f_year<-f$year-f$counter+1
  f<-f[order(f$BvD.ID.number,-f$year),]
  setkey(f,BvD.ID.number,f_year)
  f<-unique(f,by=c("BvD.ID.number","f_year"))
  f<-f[,c("BvD.ID.number","year","f_year")]
  colnames(f)[2]<-"max_year"
  
  emp<-merge(emp,f,by="BvD.ID.number")
  emp<-emp[emp$year>=emp$f_year&emp$year<=emp$max_year,]
  emp$value<-as.character(emp$value)
  emp$value<-str_replace_all(emp$value,"\\,","")
  emp$value<-as.numeric(emp$value)
  ###check size by employment
  emp$size<-ifelse(emp$value<10,"micro",ifelse(emp$value>9&emp$value<50,"small",ifelse(emp$value>49&emp$value<250,"medium","large")))
  ###eliminate large firms
  #emp<-emp[emp$size!="large",]
  emp$crit<-"employment"
  s<-rbind(unique(turn[,c("BvD.ID.number","year","size","crit")]),unique(emp[,c("BvD.ID.number","year","size","crit")]))
  s<-s[,n:=length(size),by=.(BvD.ID.number,year)]
  s<-s[,n_div:=uniqueN(size),by=.(BvD.ID.number,year)]
  sizes<-data.frame(ind=c(1,2,3,4),s=c("micro","small","medium","large"))
  si<-merge(s,sizes,by.x="size",by.y="s",all.x=T)
  si<-data.table(si)
  setkey(si,BvD.ID.number,year)
  key(si)
  si<-si[order(si$BvD.ID.number,si$year,-si$ind),]
  su<-unique(si,by=c("BvD.ID.number","year"))
  su$crit_size<-ifelse(su$n==2&su$n_div==1,"both",su$crit)
  
  su<-su[,c("BvD.ID.number","year","size","crit_size"),with=F]
  turn<-turn[,c("BvD.ID.number","year","value"),with=F]
  colnames(turn)[3]<-"turnover"
  emp<-emp[,c("BvD.ID.number","year","value"),with=F]
  colnames(emp)[3]<-"employment"
  
  dat<-merge(su,turn,by=c("BvD.ID.number","year"))
  dat<-merge(dat,emp,by=c("BvD.ID.number","year"))
  
  dep<-orb[,c("BvD.ID.number","GUO...BvD.ID.number","DUO...BvD.ID.number","BvD.Indep..Indic.")]
  dep<-unique(dep)
  dat<-merge(dat,dep,by="BvD.ID.number")
  
  dat<-unique(dat)
  
  
  saveRDS(dat,paste("Y:/ESS/IP impact/2019_firm_level/2019_firm_level/data_preparation/orbis/orb_data_",cc,".rds",sep=""))
  saveRDS(dat,"Y:/ESS/IP impact/2019_firm_level/2019_firm_level/data_preparation/orbis/orb_data_EL_2.rds")
  print(cc)
  gc()
  
}


###compare the availability for countries where we had more than one datasets
setwd("Y:/ESS/IP impact/2019_firm_level/2019_firm_level/data_preparation/orbis")

DE_org<-readRDS("orb_data_DE_org.rds")
DE<-readRDS("orb_data_DE.rds")

min(DE_org$year)
min(DE$year)


EL<-readRDS("orb_data_EL.rds")
EL2<-readRDS("orb_data_EL_2.rds")

###We have eliminated EL_2 and DE_org files as they contained exactly the same data

list<-list.files()
i<-1
stat<-data.frame()
####Prepare statistics on the various strata availability in the dataset
for (i in 1:length(list))
{
  orb<-readRDS(list[i])
  orb<-as.data.table(orb)
  ###take only last available year for the size classification
  orb<-orb[order(-orb$year),]
  setkey(orb,BvD.ID.number)
  orb<-unique(orb,by="BvD.ID.number")
  s<-orb[,.(n=uniqueN(BvD.ID.number)),by=size]
  s$per<-s$n/sum(s$n)
  s$c<-str_extract(list[i],"[A-Z][A-Z]")
  stat<-rbind(stat,s)
  print(i)
}


