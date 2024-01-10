setwd("C:/Users/Vannessa Duarte/Documents/CursoSocialNetwork/Data_GroupWork")
rm(list=ls())
load('miner_dispute.rda')
miner_dispute$jocking_exchange

class(miner_dispute$workers)
miner_wok=miner_dispute$workers[-(15),]

library(igraph)
# Attributes
matrix_miner<-as.data.frame(cbind(label = miner_wok$label, 
                    conversation = miner_dispute$conversation_exchange, 
                    occupation = miner_wok$occupation))
library(netmem)
netmem::matrix_report(matrix_miner)

library(network)
mineros<-as.network(miner_dispute$conversation_exchange,matrix.type="adjacency",directed=TRUE)
class(mineros)
mineros
mineros<-set.vertex.attribute(mineros,"occupation",miner_wok$occupation)
mineros<-set.vertex.attribute(mineros,"age",miner_wok$age)
mineros
plot.network(mineros,displayisolate=F)
mineros %v% "vertex.name"<-miner_wok$label 
plot.network(mineros,displayisolate=F,displaylabels=TRUE,label.cex=0.8,label.col="blue")
table(miner_wok$religion)
religion<-as.factor(miner_wok$religion)
religion<-unclass(religion)
religion
re<-unique(religion)
re<-c(1,2,3,4)
par(mar = c(0,0,1,0), mfrow = c(1, 1)) 
plot.network(mineros,displayisolate=F,displaylabels=TRUE,label.cex=0.8,label.col="blue",vertex.col=religion)
legend("bottomleft",legend = c("African Reform Church", "ex-Johovah's Witness" , "Nil","Roman Catholic"),fill = re)
ocup<-unclass(as.factor(miner_wok$occupation))
ocup
par(mar = c(0,0,1,0), mfrow = c(1, 1)) 
tri<-c(1,2,3,4,5)
plot.network(mineros,displayisolate=F,displaylabels=TRUE,label.cex=0.8,label.col="blue",vertex.col=ocup)
legend("bottomleft",legend = c("Crew Boss", "Dryer"  , "Scale Boy" ,"Scrubber" , "Stripper"),fill = tri)

