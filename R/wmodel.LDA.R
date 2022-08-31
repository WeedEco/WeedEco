#Weedmodels package
#remember input csv for model.lda() needs to be in the order
#"SLA","ARNODE","LOGCANH","LOGCAND","FLOWPER","Study"

#model.LDA() will take the functional attribute scores of archeaobotanical samples and compared them to a linear discriminant analysis
#of known crop husbandry regimes and classify the archaeobotanical samples in to either low intensity/extensive or high intensity/intensive
#cultivation. Currently two model types are available - model1 is ast/prov data, while the model2 is evvia, ast/prov and morocco
#suitable for semi-arid locations
wmodel.LDA<-function(model,x){

  if(model=='model1'|model== 1) data.model<-data.frame(model1)
  if(model=='model1'|model== 1) discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCANH+LOGCAND+FLOWPER,data.model, CV = TRUE)
  if(model=='model1'|model== 1) model_lda <- lda(Study ~SLA+ARNODE+LOGCANH+LOGCAND+FLOWPER, data.model)
  if(model=='model1'|model== 1) model_lda50<- lda(Study ~SLA+ARNODE+LOGCANH+LOGCAND+FLOWPER, data.model, prior=c(1,1)/2)
  if(model=='model2'|model== 2) data.model<-data.frame(model2)
  if(model=='model2'|model== 2) discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCANH+LOGCAND,data.model, CV = TRUE)
  if(model=='model2'|model== 2) model_lda50 <- lda(Study ~SLA+ARNODE+LOGCANH+LOGCAND,data.model, prior=c(1,1)/2)
  if(model=='model2'|model== 2) model_lda <- lda(Study ~SLA+ARNODE+LOGCANH+LOGCAND,data.model)
  if(model=='model3'|model== 3) data.model<-data.frame(model3)
  if(model=='model3'|model== 3) discrim_cv <- lda(Study ~ FLOWPER+VEGPROP,data.model, CV = TRUE)
  if(model=='model3'|model== 3) model_lda <- lda(Study ~ FLOWPER+VEGPROP,data.model)
  if(model=='model3'|model== 3) model_lda50 <- lda(Study ~ FLOWPER+VEGPROP,data.model, prior=c(1,1)/2 )
  predictionmodel <- predict(model_lda,data.model)
  predictionmodel50<- predict(model_lda50,data.model)
  functionalAt <- data.frame(Study = as.factor(data.model$Study),
                             Classification= predictionmodel$class,
                             LD1 = predictionmodel$x)
  functionalAt50 <- data.frame(Study = as.factor(data.model$Study),
                             Classification= predictionmodel50$class,
                             LD1 = predictionmodel50$x)

  centroids <- functionalAt %>%
    group_by(Study) %>%
    summarise(centroid1 = mean(LD1))
  centroids50 <- functionalAt50 %>%
    group_by(Study) %>%
    summarise(centroid1 = mean(LD1))
  # groupmean<-(model_lda$prior%*%model_lda$means)
  # constant<-(groupmean%*%model_lda$scaling)
  # LDAhand<-scale(x,center=groupmean, scale=false)%*%model_lda$scaling
  model <- cbind(as.data.frame((predict(model_lda,x))), as.data.frame(predict(model_lda50,x)),x)
  model$LD1<-model$LD1*-1
  print(model)
}


