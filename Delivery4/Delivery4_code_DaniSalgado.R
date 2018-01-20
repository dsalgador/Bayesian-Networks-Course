#Delivery 4, Exercise 7 BLOCK 3: Daniel Salgado Rojo

library(Rgraphviz)
#install.packages("bnlearn")
library(bnlearn)

par(mfrow = c(1,1))

#########################################
# Building the BN structure (DAG)       #
#########################################
#We create the empty network with the nodes (no edges)
  names <- c("X", "Y", "Z")
  net <- empty.graph(names)
  #print(class(net))

#Define de adjacency matrix of the DAG:
  # V = {"X", "Y", "Z"}, E = {(X,Z), (Y,Z)}
  adjMatrix <- matrix(c(0,0,1,0,0,1,0,0,0),
                      nrow = 3,
                      byrow = TRUE,
                      dimnames=list(nodes(net), nodes(net)))
  #print(adjMatrix)
  amat(net) = adjMatrix

#Load the data
  #getwd()
    load("datos_generados_ejercicio_7.rdata")  
    data <- datos_generados_ejercicio_7
    data <- data[complete.cases(data),]
    data[] <- lapply(data, as.factor)
  
    net.estimated = bn.fit(net, data, method = "mle")
    #print(net.estimated)

#Save coefficients
  coef <- coefficients(net.estimated)
  #dim(coef)

#Marginal of X
  print("Marginal of X"); print(coef$X)
#Marginal of Y  
  print("Marginal of Y");print(coef$Y)
# P(Z| Y = 0,X )
  print("CPT of Y = 0");print(coef$Z[1,,])
# P(Z| Y = 1,X )
  print("CPT of Y = 1");print(coef$Z[2,,]) 
  
  