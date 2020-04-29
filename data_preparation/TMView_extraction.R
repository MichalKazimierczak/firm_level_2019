#######This code has been developed to extract TMView data############################
#######This code was created on 08/04/2020############################################
######################################################################################

####Read the csv files of the TMView repository

getwd()
setwd("Y:/ESS/IP impact/2019_firm_level/2019_firm_level/data_preparation/TMView_data/")
setwd("data_preparation/TMView_data")
lista<-list.files("../EU-DSV","zip$")
library(stringr)

i<-6
c(8,)

for (i in 1:length(lista))
{
  c<-read.csv(unzip(paste("../EU-DSV/",lista[i],sep="")))
  write.csv(c,paste("../EU-DSV/DES/",str_replace(lista[i],"-DSV(-.)?\\.zip","\\1.csv"),sep=""))
}

c<-read.csv("tmdsv/AT-TMV/AT-TMV.csv")

remove(c)
bx<-read.csv("DE-TMV-1.csv")


###check the columns available in each country
setwd("TM")
getwd()
lista<-list.files()
i<-1
cols<-data.frame()
for (i in 1:length(lista))
{
  c<-read.csv(lista[i],nrows=10)
  col<-colnames(c)[-1]
  c<-cbind(country=lista[i],col)
  c<-as.data.frame(c)
  cols<-rbind(cols,c)
}


unique(cols$col)

i<-2

cz<-read.csv("DK.csv",nrows=10000)

getwd()
lista<-list.files("TM")
lista<-lista[str_detect(lista,"DE")]
DE<-data.frame()
for (i in 1:length(lista))
{
  d<-read.csv(paste("TM/",lista[i],sep=""),encoding="UTF-8")
  colnames(d)<-colnames(DE)
  DE<-rbind(DE,d)
}


#################################################################################
############prepare sample for scrapping#########################################
#################################################################################
#################################################################################

lista<-list.files("TM")
samp<-data.frame()
i<-24
col<-colnames(s)

for (i in 23:25)
{
  s<-read.csv(paste("TM/",lista[i],sep=""),encoding="UTF-8",header=T)
  s$X<-NULL
  s$c<-str_extract(lista[i],"^..")
  s<-as.data.table(s)
  sa<-s[,sample(ST13,5,replace=T),by=sc]
  s<-s[s$ST13%in%sa$V1,]
  s<-s[str_detect(s$ST13,s$c),]
  s$ExpiryDate<-NULL
  samp<-rbind(samp,s)
  print(i)
}

colnames(samp)
colnames(samp_gb)

samp$ExpiryDate<-NA
samp<-rbind(samp,samp_gb)

check<-unique(samp[,c("sc","c")])
unique(s$sc)

s<-s[s$sc=="Ended",]
s<-s[1:100,]
s$c<-"AT"
s$ExpiryDate<-NA
s$X<-NULL

samp<-rbind(samp,s)

library(data.table)
s<-data.table(samp)

s<-s[,head(.SD,10),by=c("c","sc")]

getwd()
write.csv(s,"../scrap_sample.csv",row.names=F)

###################################################################################
#####Prepare similar sample for design data scrapping###############################
####################################################################################

getwd()
lista<-list.files("../EU-DSV/DES")
i<-1
samp<-data.frame()
col<-colnames(s)

for (i in 1:length(lista))
{
  s<-read.csv(paste("../EU-DSV/DES/",lista[i],sep=""),encoding="UTF-8")
  s$X<-NULL
  s$c<-str_extract(lista[i],"^..")
  s<-as.data.table(s)
  sa<-s[,sample(ST13,3,replace=T),by=sc]
  s<-s[s$ST13%in%sa$V1,]
  colnames(s)<-col
  samp<-rbind(samp,s)
}

samp<-unique(samp)

write.csv(samp,"../scrap_sample_des.csv",row.names=F)
