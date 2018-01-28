#DELIVERY 1: Daniel Salgado Rojo

#install.packages("gRain")
#source("http://bioconductor.org/biocLite.R")
#biocLite("RBGL")
#biocLite("Rgraphviz")
#install.packages("Rcpp")
library(gRain)
library(gRbase)
library(Rgraphviz)
library(RBGL)

tf <- c("false","true")

# Specify the CPTs:
node.X1 <- cptable(~ X1, values = c(4, 6), levels = tf)
node.X2 <- cptable(~ X2|X1, values = c(9.5, 0.5,5,5), levels = tf)
node.X3 <- cptable(~ X3|X1, values = c(9, 1, 2, 8 ), levels = tf)

# Create an intermediate representation of the CPTs:
nodes <- list(node.X1,node.X2,node.X3)
plist <- compileCPT(nodes)
plist
plist$X1
plist$X2
plist$X3

# Create a network of name "Norman.net", for instance:
Ann.net<-grain(plist)
summary(Ann.net)
# The graph:
plot1=plot(Ann.net)
plot1

# We can compute the marginal probability
# of each variable
# These probabilities are EXACT!!
querygrain(Ann.net,nodes=c("X1","X2","X3"),
           type="marginal")

############################
## QUERIES TO SOLVE (b) and (c)
#########################


#(b)
Ann.net.2<-setEvidence(Ann.net,nodes=c("X3"),
                          states=c("true"))
# The marginal distributions given
# the evidence are:
marg=querygrain(Ann.net.2,nodes
                =c("X2"), type="marginal")

marg$X2["true"] # = 0.4653846

#(c)

margx2 <- querygrain(Ann.net,nodes=c("X2"),
                     type="marginal")

margx2$X2["true"] # P(X2 = T) = 0.32
