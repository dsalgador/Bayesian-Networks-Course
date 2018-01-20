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
  graphviz.plot(net)
  
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
  print("CPT of Y = 0");print(coef$Z[,,1])
# P(Z| Y = 1,X )
  print("CPT of Y = 1");print(coef$Z[,,2]) 

#Estimations of the parameters:
  theta_11 = coef$X[2]
  theta_21 = coef$Y[2]
  
  theta_31 = coef$Z[2,2,2]    #P(Z = 1 / X = 1, Y = 1)
  theta_32 = coef$Z[2,1,2]   #P(Z = 1 / X = 0, Y = 1)
  theta_33 = coef$Z[2,2,1]    #P(Z = 1 / X = 1, Y = 0)
  theta_34 = coef$Z[2,1,1]   #P(Z = 1 / X = 0, Y = 0)
  
 
###################################################################
###################################################################

#########################################
# LEARNING the BN structure (DAG)        #
######################################### 

  net.pred <- hc(data, score = "bic")
  graphviz.plot(net.pred)
  #the arrows are inverted with respect the original structure
  class(net.pred)
  net.pred.estimated = bn.fit(net.pred, data, method = "mle")
  #print(net.estimated)
  
  #Save coefficients
  coef.pred <- coefficients(net.pred.estimated)
  #dim(coef)
  
  print(net.pred.estimated)
  
################################
# Example to compare the two cases
#################################
  library(gRain)  
  #Assume we know that X = 1 (instantiate X=1),
  #which is the probability of Z = 1 ?
  
  predictors <-c("X")
  values <- c(1)
  eevvii <- matrix(values, nrow = 1, byrow = T)
  colnames(eevvii) <- predictors
  evi <- as.data.frame(eevvii)
  responses <- c("Z")
  
  #(A) net.estimated, when we imposed the structure
  
  net.estimated.gRain <- as.grain(net.estimated)
  pp.estimated <- predict(net.estimated.gRain,
          response = responses, 
          newdata = evi,
          predictor = predictors,
          type = "class")
  
  
  
  #(B) net.pred.estimated, when we predicted the structure
  net.pred.estimated.gRain <- as.grain(net.pred.estimated)
  pp.pred.estimated <- predict(net.pred.estimated.gRain,
                          response = responses, 
                          newdata = evi,
                          predictor = predictors,
                          type = "class")

#Comparison between the two approaches
  
print("How do differ both bayesian networks?")    
print(all.equal(net.estimated, net.pred.estimated))
print("Are both networks predicting the same and with the same CL?")
print(all.equal(pp.estimated, pp.pred.estimated))
