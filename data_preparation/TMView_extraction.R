#######This code has been developed to extract TMView data############################
#######This code was created on 08/04/2020############################################
######################################################################################

####Read the csv files of the TMView repository

getwd()
setwd("data_preparation/TMView_data")
lista<-list.files("tmdsv")
library(stringr)

i<-1

for (i in 1:length(lista))
{
  c<-read.csv(unzip(paste("tmdsv/",lista[i],sep="")))
  write.csv(c,paste("TM/",str_replace(lista[i],"-TMV\\.zip",".csv"),sep=""))
}

c<-read.csv("tmdsv/AT-TMV/AT-TMV.csv")

remove(c)
