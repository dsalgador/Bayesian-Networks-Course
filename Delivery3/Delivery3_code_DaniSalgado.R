#Delivery 3, Exercise 9 BLOCK 2: Daniel Salgado Rojo

#install.packages("gRain")
#source("http://bioconductor.org/biocLite.R")
#biocLite("RBGL")
#biocLite("Rgraphviz")
#install.packages("Rcpp")
library(gRain)
library(gRbase)
library(Rgraphviz)
library(RBGL)
par(mfrow = c(1,1))


# Specify the CPTs:
# First: which are the possible values of the nodes (all nodes are boolean):
tf <- c("false","true")
fp = 0.01*100
fn = 0.03*100

######################
#     Kit A          #
######################


  # Specify the CPTs:
  node.T0<- cptable(~ T0, values=c(8,2),levels=tf)
  node.B0<- cptable(~ B0, values=c(1,9), levels=tf)
  node.S1<- cptable(~ S1 + T0 + B0, values=c(1,0,1,0,100-fp,fp,fn,100-fn), levels=tf)
  node.S2<- cptable(~ S2 + T0 + B0, values=c(1,0,1,0,100-fp,fp,fn,100-fn), levels=tf)
  
  
  # Create an intermediate representation of the CPTs:
  nodesA <- list(node.T0,node.B0,node.S1,node.S2)
  plist <- compileCPT(nodesA)
  
  # Create a network of kit A:
  BNA<-grain(plist)
  summary(BNA)
  # The graph:
  BNA.plot=plot(BNA)
  BNA.plot
  
  marginals <- querygrain(BNA,nodes=c("T0","B0","S1","S2"),
             type="marginal")
  
  joints1s2 <- querygrain(BNA,nodes=c("S1","S2"),
             type="joint")

  #################
  # Subexercise (a)#
  #################
    
    #Compute P(T0 = T/S1 = F, S2 = F) 
    BNAa<-setEvidence(BNA,nodes=c("S1","S2"),
                     states=c("false", "false"))
    marga=querygrain(BNAa,nodes
                    =c("T0"), type="marginal")
    marga
    marga$T0["true"]
    #P(T0 = T/S1 = F, S2 = F) = 0.02502004
    
  #################
  # Subexercise (b)#
  ################
    #Compute P(T0 = T/S1 = T, S2 = T) 
    BNAb<-setEvidence(BNA,nodes=c("S1","S2"),
                      states=c("true", "true"))
    margb=querygrain(BNAb,nodes
                     =c("T0"), type="marginal")
    margb
    margb$T0["true"]
    #P(T0 = T/S1 = T, S2 = T) = 0.9995751

  #################
  # Subexercise (c)#
  ################
    
    #Compute P(T0 = T/S1 = T, S2 = F) 
    #Compute P(T0 = T/S1 = F, S2 = T) 
    
    BNAc<-setEvidence(BNA,nodes=c("S1","S2"),
                      states=c("true", "false"))
    margc=querygrain(BNAc,nodes
                     =c("T0"), type="marginal")
    margc
    margc$T0["true"]    
    #P(T0 = T/S1 = T, S2 = F) = 0.4235808
    
######################
#     Kit B          #
######################
    
    
    # Specify the CPTs:
    node.T0<- cptable(~ T0, values=c(8,2),levels=tf)
    node.B1<- cptable(~ B1, values=c(1,9), levels=tf)
    node.B2<- cptable(~ B2, values=c(1,9), levels=tf)
    node.S1<- cptable(~ S1 + T0 + B1, values=c(1,0,1,0,100-fp,fp,fn,100-fn), levels=tf)
    node.S2<- cptable(~ S2 + T0 + B2, values=c(1,0,1,0,100-fp,fp,fn,100-fn), levels=tf)
    
    
    # Create an intermediate representation of the CPTs:
    nodesB <- list(node.T0,node.B1, node.B2,node.S1,node.S2)
    plistB <- compileCPT(nodesB)
    
    
    # Create a network of kit A:
    BNB<-grain(plistB)
    summary(BNB)
    # The graph:
    BNB.plot=plot(BNB)
    BNB.plot
    
    marginals <- querygrain(BNB,nodes=c("T0","B1","B2","S1","S2"),
                            type="marginal")
    
    joints1s2 <- querygrain(BNB,nodes=c("S1","S2"),
                            type="joint")
    
    #################
    # Subexercise (a)#
    #################
    
    #Compute P(T0 = T/S1 = F, S2 = F) 
    BNBa<-setEvidence(BNB,nodes=c("S1","S2"),
                      states=c("false", "false"))
    marga=querygrain(BNBa,nodes
                     =c("T0"), type="marginal")
    marga
    marga$T0["true"]
    #P(T0 = T/S1 = F, S2 = F)=0.004089033
    
    #################
    # Subexercise (b)#
    ################
    #Compute P(T0 = T/S1 = T, S2 = T) 
    BNBb<-setEvidence(BNB,nodes=c("S1","S2"),
                      states=c("true", "true"))
    margb=querygrain(BNBb,nodes
                     =c("T0"), type="marginal")
    margb
    margb$T0["true"]
    #P(T0 = T/S1 = T, S2 = T) = 0.9995751 
    
    #################
    # Subexercise (c)#
    ################
    
    #Compute P(T0 = T/S1 = T, S2 = F) 
    #Compute P(T0 = T/S1 = F, S2 = T) 
    
    BNBc<-setEvidence(BNB,nodes=c("S1","S2"),
                      states=c("true", "false"))
    margc=querygrain(BNBc,nodes
                     =c("T0"), type="marginal")
    margc
    margc$T0["true"]  
    #P(T0 = T/S1 = T, S2 = F)  = 0.7565559
    
####################################################
# Some calculations for the "by hand" computations #
###################################################   
    fn = 0.03
    fp = 0.01
    
# KIT A    
#Compute P(T0 = T/S1 = F, S2 = F) 
num = fn^2*0.9 + 0.1
denom = 0.9*(0.2*fn^2 + 0.8*(1-fp)^2) + 0.1 
num/denom * 0.2

#Compute P(T0 = T/S1 = T, S2 = T) 
num = (1-fn)^2*0.9*0.2
denom = 0.9*(0.2*(1-fn)^2 + 0.8* (fp)^2)
num/denom

# KIT B 

#c) Compute P(T0 = T/S1 = F, S2 = T) 
num = fn*(1-fn)*0.9^2 + (1-fn)*0.9*0.1
denom = fn*(1-fn)*0.2*0.9^2+(1-fn)*0.2*0.1*0.9+(1-fp)*fp*0.8*0.9^2+fp*0.8*0.1*0.9
num/denom * 0.2