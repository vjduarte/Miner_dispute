setwd("C:/Users/Vannessa Duarte/Documents/CursoSocialNetwork/Data_GroupWork")
rm(list=ls())
library("classicnets")
#install.packages("texreg")
library(texreg)
load('miner_dispute.rda')
?miner_dispute

miner_dispute
class(miner_dispute$workers)
miner_wok=miner_dispute$workers[-(15),]


#library(igraph)
# Attributes
#matrix_miner<-as.data.frame(cbind(label = miner_wok$label, 
 #                   conversation = miner_dispute$conversation_exchange, 
  #                  occupation = miner_wok$occupation))
#library(netmem)
#netmem::matrix_report(matrix_miner)

library(network)
library(sna)
library(ergm)
mineros_conve<-as.network(miner_dispute$conversation_exchange,matrix.type="adjacency",directed=TRUE)
class(mineros_conve)
mineros_conve
mineros_conve<-set.vertex.attribute(mineros_conve,"occupation",miner_wok$occupation)
mineros_conve<-set.vertex.attribute(mineros_conve,"age",as.numeric(miner_wok$age))
mineros_conve
plot.network(mineros_conve,displayisolate=F)
mineros_conve %v% "vertex.name"<-miner_wok$label 
plot.network(mineros_conve,displayisolate=F,displaylabels=TRUE,label.cex=0.8,label.col="blue")
table(miner_wok$religion)
religion<-as.factor(miner_wok$religion)
religion<-unclass(religion)
religion
re<-unique(religion)
re<-c(1,2,3,4)
par(mar = c(0,0,1,0), mfrow = c(1, 1)) 
plot.network(mineros_conve,displayisolate=F,displaylabels=TRUE,label.cex=0.8,label.col="blue",vertex.col=religion)
legend("bottomleft",legend = c("African Reform Church", "ex-Johovah's Witness" , "Nil","Roman Catholic"),fill = re)
ocup<-unclass(as.factor(miner_wok$occupation))
ocup
par(mar = c(0,0,1,0), mfrow = c(1, 1)) 
tri<-c(1,2,3,4,5)

plot.network(mineros_conve,displayisolate=F,displaylabels=TRUE,label.cex=0.8,label.col="blue",vertex.col=ocup)
legend("bottomleft",legend = c("Crew Boss", "Dryer"  , "Scale Boy" ,"Scrubber" , "Stripper"),fill = tri)

miner_dispute$cell_room
#__________________________________---
mineros_ph<-as.network(miner_dispute$personal_service,matrix.type="adjacency",directed=TRUE)
class(mineros_ph)
mineros_ph
mineros_ph<-set.vertex.attribute(mineros_ph,"occupation",miner_wok$occupation)
mineros_ph<-set.vertex.attribute(mineros_ph,"age",miner_wok$age)
mineros_ph
par(mar = c(0,0,1,0), mfrow = c(1, 1)) 
tri<-c(1,2,3,4,5)
plot.network(mineros_ph,displayisolate=F,displaylabels=TRUE,label.cex=0.8,label.col="blue",vertex.col=ocup)
legend("bottomleft",legend = c("Crew Boss", "Dryer"  , "Scale Boy" ,"Scrubber" , "Stripper"),fill = tri)
#-------------------------------------------------------
mineros_ja<-as.network(miner_dispute$jocking_exchange,matrix.type="adjacency",directed=TRUE)
class(mineros_ja)
mineros_ja
mineros_ja<-set.vertex.attribute(mineros_ja,"occupation",miner_wok$occupation)
mineros_ja<-set.vertex.attribute(mineros_ja,"age",as.numeric(miner_wok$age))
mineros_ja
par(mar = c(0,0,1,0), mfrow = c(1, 1)) 
tri<-c(1,2,3,4,5)
plot.network(mineros_ja,displayisolate=F,displaylabels=TRUE,label.cex=0.8,label.col="blue",vertex.col=ocup)
legend("bottomleft",legend = c("Crew Boss", "Dryer"  , "Scale Boy" ,"Scrubber" , "Stripper"),fill = tri)
#CELL ROOM DISPUTE
#__________________________________---
dispute<-as.network(miner_dispute$cell_room,matrix.type="adjacency",directed=FALSE)
class(dispute)
dispute
dispute<-set.vertex.attribute(dispute,"occupation",miner_dispute$workers$occupation)
dispute<-set.vertex.attribute(dispute,"age",as.numeric(miner_dispute$workers$age))
dispute
par(mar = c(0,0,1.5,0), mfrow = c(1, 1)) 
tri<-c(1,2,3,4,5)
pdf("DispNet.pdf")
plot.network(dispute,displayisolate=F,displaylabels=TRUE,label.cex=0.8,label.col="blue",vertex.col=ocup)
legend("bottomleft",legend = c("Crew Boss", "Dryer"  , "Scale Boy" ,"Scrubber" , "Stripper"),fill = tri)

#-
#----------------------------------------ERGM------------
b<- ergm(mineros_conve ~ edges)
summary(b)
# Edges negativos hace referencia a la densidad de la red
set.seed(2021)
markov <- ergm(mineros_conve ~ edges+mutual)
summary(markov)

markov1 <- ergm(mineros_conve ~ edges+mutual+nodematch("occupation"))
summary(markov1)

markov2 <- ergm(mineros_conve ~ edges+mutual+nodematch("occupation")+absdiff("age"))
summary(markov2)

markov3 <- ergm(mineros_conve ~ edges+mutual+nodematch("occupation")+absdiff("age")+gwesp(.5,fixed=TRUE))
summary(markov3)

#----------------------------------------ERGM_Dispute------------
d<- ergm(dispute ~ edges)
summary(d)
# Edges negativos hace referencia a la densidad de la red
set.seed(2021)
# dismarkov <- ergm(dispute ~ edges+mutual, control = control.ergm(main.method = "Stochastic-Approximation"))
dismarkov <- ergm(dispute ~ edges+nodematch("occupation"))
summary(dismarkov)

dismarkov <- ergm(dispute ~ edges+nodematch("occupation")+absdiff("age"))
summary(dismarkov)

dismarkov <- ergm(dispute ~ edges+mutual+nodematch("occupation")+absdiff("age")+gwesp(.5,fixed=TRUE))
summary(dismarkov)
texreg(markov3)
knitreg(list("Before Dispute"=markov3,"Dispute"=dismarkov),center = FALSE, caption = "Ergm parameters results", table = FALSE)

#REsults Convergence
######################################
#install.packages("rstan")
library("bayesplot")
library("ggplot2")
library("rstan") 
pdf("convergenceConv.pdf")
mcmc.diagnostics(markov3, which   = c("plots"))
mcmc.diagnostics(dismarkov, which   = c("plots"))

#Goodness of fit
pdf("GoogConv.pdf")
gof_final <- gof(markov3)
print(gof_final)
# Plotting the result (5 figures)
op <- par(mfrow = c(3,2))
plot(gof_final)
par(op)

#DISPUTE
pdf("Googdis.pdf")
gof_final_dis <- gof(dismarkov)
print(gof_final_dis)
# Plotting the result (5 figures)
op <- par(mfrow = c(3,2))
plot(gof_final_dis)
par(op)

knitreg(list("Before Dispute"=gof_final,"Dispute"=gof_final_dis),center = FALSE, caption = "Ergm parameters results", table = FALSE)
