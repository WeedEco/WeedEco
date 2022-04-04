#FIBSLDA package
#remember input csv for model.lda() needs to be in the order
#"SLA","ARNODE","LOGCAHN","LOGCADN","FLOWPER","Study"

#model.LDA() will take the functional attribute scores of archeaobotanical samples and compared them to a linear discriminant analysis
#of known crop husbandry regimes and classify the archaeobotanical samples in to either low intensity/extensive or high intensity/intensive
#cultivation. Currently two model types are available - model1 is ast/prov data, while the model2 is evvia, ast/prov and morocco
#suitable for semi-arid locations
model.LDA<-function(model,x){
  library(dplyr)
  library(haven)
  library(MASS)
  if (model=='model1'|model== 1) data(model1)
  if(model=='model1'|model== 1) discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCAHN+LOGCADN+FLOWPER,data, CV = TRUE)
  if(model=='model1'|model== 1) model_lda <- lda(Study ~SLA+ARNODE+LOGCAHN+LOGCADN+FLOWPER,data)

  if(model=='model2'|model== 2)data(model2)
  if(model=='model2'|model== 2) discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCAHN+LOGCADN,data, CV = TRUE)
  if(model=='model2'|model== 2) model_lda <- lda(Study ~SLA+ARNODE+LOGCAHN+LOGCADN,data)

  if(model=='model3'|model== 3)load (file="model3.rda")
  if(model=='model3'|model== 3) discrim_cv <- lda(Study ~ FLOWPER+VEGPROP,data, CV = TRUE)
  if(model=='model3'|model== 3) model_lda <- lda(Study ~FLOWPER+VEGPROP,data)


  predictionmodel <- predict(model_lda,data)
  functionalAt <- data.frame(Study = as.factor(data$Study),
                             Classification= predictionmodel$class,
                             LD1 = predictionmodel$x)
  centroids <- functionalAt %>%
    group_by(Study) %>%
    summarise(centroid1 = mean(LD1))
  model <- cbind(as.data.frame(predict(model_lda,x)),x)
  model$LD1<-model$LD1*-1
  print(model)
}
