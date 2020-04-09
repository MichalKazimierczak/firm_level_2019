#######This code has been developed to extract TMView data############################
#######This code was created on 08/04/2020############################################
######################################################################################

####Read the csv files of the TMView repository

getwd()
setwd("data_preparation/TMView_data")
lista<-list.files("tmdsv")
library(stringr)

i<-8

for (i in c(6,7,8))
{
  c<-read.csv(unzip(paste("tmdsv/",lista[i],sep="")))
  write.csv(c,paste("TM/",str_replace(lista[i],"-TMV-(.)\\.zip","_\\1.csv"),sep=""))
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


cz<-read.csv("DK.csv",nrows=10000)
