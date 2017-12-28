#Exercise 3 BLOCK 2: Daniel Salgado Rojo

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

#Subindex 1 means false and 2 means true

tf <- c("false","true")

# Specify the CPTs:
node.X <- cptable(~ X, values = c(1, 9), levels = tf)
node.Y <- cptable(~ Y|X, values = c(6, 4,2,8), levels = tf)
node.W <- cptable(~ W|Y, values = c(9, 1, 3, 7 ), levels = tf)
node.Z <- cptable(~ Z|X, values = c(7, 3, 1, 9 ), levels = tf)
node.T <- cptable(~ T|Z, values = c(8,2, 1, 9 ), levels = tf)

# Create an intermediate representation of the CPTs:
nodes <- list(node.X,node.Y,node.W,node.Z, node.T)
plist <- compileCPT(nodes)
plist
plist$X
plist$Y
plist$W

# Create a network of name "Norman.net", for instance:
BN<-grain(plist)
summary(BN)
# The graph:
plot1=plot(BN)
plot1

# We can compute the marginal probability
# of each variable
# These probabilities are EXACT!!
querygrain(BN,nodes=c("X","Y","W", "Z","T"),
           type="marginal")

############################
## QUERIES TO SOLVE EXERCISE 1 (a) and (b)
#########################


#(a) UPWARD PROP
BN2<-setEvidence(BN,nodes=c("W"),
                       states=c("false"))
# The marginal distributions given
# the evidence are:
marg=querygrain(BN2,nodes
                =c("X", "Y", "Z","T"), type="marginal")

#P(x1/w1)
marg$X["false"]
#P(y1/w1)
marg$Y["false"] 

#(b) DOWNWARD PROP

#P(z1/w1)
marg$Z["false"]
#P(t1/w1)
marg$'T'["false"] 

############################
## QUERIES TO SOLVE EXERCISE 2  
#########################

#P(x1/t2,w1) ?

BN3<-setEvidence(BN,nodes=c("W", "T"),
                 states=c("false", "true"))
marg=querygrain(BN3,nodes
                =c("X"), type="marginal")

#P(x1/t2,w1) =
marg$X["false"]