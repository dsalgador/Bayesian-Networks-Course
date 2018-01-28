#Delivery 4, Exercise 7 BLOCK 3: Daniel Salgado Rojo

library(Rgraphviz)
#install.packages("bnlearn")
library(bnlearn)

#path where 'datos_generados_ejercicio_7.rdata' is located
path = "C:/Users/Daniel/Dropbox/Master/Data Visualization/PART3/Delivery4"
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
    setwd(path)
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
  theta_11 = coef$X[[2]]
  theta_21 = coef$Y[[2]]
  
  theta_31 = coef$Z[2,2,2]    #P(Z = 1 / X = 1, Y = 1)
  theta_32 = coef$Z[2,1,2]   #P(Z = 1 / X = 0, Y = 1)
  theta_33 = coef$Z[2,2,1]    #P(Z = 1 / X = 1, Y = 0)
  theta_34 = coef$Z[2,1,1]   #P(Z = 1 / X = 0, Y = 0)

print("Estimated parameters")
  param <- matrix(c(theta_11, theta_21, theta_31, theta_32, theta_33, theta_34)
                  , nrow=1)  
colnames(param) <- c("theta11", "theta21", "theta31","theta32","theta33","theta34")
print(param)  

#       theta11  theta21   theta31   theta32   theta33   theta34
#[1,] 0.3928571 0.702381 0.8151261 0.3806818 0.6956522 0.2531646



###################################################################
# Extra work just to practise
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
  
#Estimations of the parameters:
  #P(Z=1)
  theta_11.pred = coef.pred$Z[[2]]
  #P(Y =1 / Z=1)
  theta_21.pred = coef.pred$Y[2,2]
  #P(Y=1/ Z=0)
  theta_22.pred = coef.pred$Y[2,1]
  
  #P(X =1 / Z=1)
  theta_31.pred = coef.pred$X[2,2]
  #P(X=1/ Z=0)
  theta_32.pred = coef.pred$X[2,1] 
    
################################
# Example to compare the two cases
#################################
  library(gRain)  
  #Assume we know that X = 1 (instantiate X=1),
  #which is the probability of Z = 1 ?
  
  predictors <-c("X")
  values <- c(1)
  eevvii <- matrix(values, nrow = length(values), byrow = T)
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
print("Network predictions:")
print("Structure given preds:"); print(pp.estimated$pred[[1]] )
print("Structure learned preds:"); print(pp.pred.estimated$pred[[1]] )

print("Structure given CL:"); print(pp.estimated$pEvidence[[1]] )
print("Structure learned CL:"); print(pp.pred.estimated$pEvidence[[1]] )


#Observation: although the structures are different, if we try different
#queries we see that the predictions with both BBNN are the same.

#Estimated parameters:

#(A) WE KNOW STRUCTURE --> WE ESTIMATE PARAMETERS
print("Parameters BNA")
param <- matrix(c(theta_11, theta_21, theta_31, theta_32, theta_33, theta_34)
                     , nrow=1)
colnames(param) <- c("theta11", "theta21", "theta31","theta32","theta33","theta34")
print(param)

#(B) WE ESTIMATE BOTH PARAMETERS AND STRUCTURE
print("Parameters BNB")
param.pred <- matrix(c(theta_11.pred, theta_21.pred, theta_22.pred, theta_31.pred, theta_32.pred), nrow=1)
colnames(param.pred) <- c("theta11", "theta21","theta22","theta31","theta32")
print(param.pred)
