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
  if (model=='model1'|model== 1) usethis::use_data("model1.rda")
  if(model=='model1'|model== 1) discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCAHN+LOGCADN+FLOWPER,data.model, CV = TRUE)
  if(model=='model1'|model== 1) model_lda <- lda(Study ~SLA+ARNODE+LOGCAHN+LOGCADN+FLOWPER,data.model)

  if(model=='model2'|model== 2)use(file="model2.rda")
  if(model=='model2'|model== 2) discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCAHN+LOGCADN,data.model, CV = TRUE)
  if(model=='model2'|model== 2) model_lda <- lda(Study ~SLA+ARNODE+LOGCAHN+LOGCADN,data.model)

  if(model=='model3'|model== 3)load (file="model3.rda")
  if(model=='model3'|model== 3) discrim_cv <- lda(Study ~ FLOWPER+VEGPROP,data.model, CV = TRUE)
  if(model=='model3'|model== 3) model_lda <- lda(Study ~FLOWPER+VEGPROP,data.model)


  predictionmodel <- predict(model_lda,data.model)
  functionalAt <- data.frame(Study = as.factor(data.model$Study),
                             Classification= predictionmodel$class,
                             LD1 = predictionmodel$x)
  centroids <- functionalAt %>%
    group_by(Study) %>%
    summarise(centroid1 = mean(LD1))
  model <- cbind(as.data.frame(predict(model_lda,x)),x)
  model$LD1<-model$LD1*-1
  print(model)
}

### this function uses the package beeswarm to plot the resulting discriminate functions from model.LDA.
#model is the model used to classify the data - either temperate or arid
#x is the column which contains the LD1 data of the archaeological samples from model.LDA
#xlims are the limits of the x axis
# method follows beeswarm method
#pch1,and col1 are the colour and symbol used to denote the centroids of the model
#pch2,col2 are the colour and symbol used to represent the modern model used
#pch3,col3 are the colour and symbol used to represent the archaeological data used
plot1<-function(model,x, xlims,method,pch1, pch2, pch3, col1, col2, col3){
  library(beeswarm)
  library(dplyr)
  library(haven)
  library(MASS)
  if (model=='temperate') load(file="data_model.rda")
  if(model=='temperate') discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCAHN+LOGCADN+FLOWPER,data.model, CV = TRUE)
  if(model=='temperate') model_lda <- lda(Study ~SLA+ARNODE+LOGCAHN+LOGCADN+FLOWPER,data.model)

  if(model=='arid')load (file="data_model_arid.rda")
  if(model=='arid') discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCAHN+LOGCADN,data.model, CV = TRUE)
  if(model=='arid') model_lda <- lda(Study ~SLA+ARNODE+LOGCAHN+LOGCADN,data.model)

   predictionmodel <- predict(model_lda,data.model)
  functionalAt <- data.frame(Study = as.factor(data.model$Study),
                             Classification= predictionmodel$class,
                             LD1 = predictionmodel$x)
  centroids <- functionalAt %>%
    group_by(Study) %>%
    summarise(centroid1 = mean(LD1))

  par(mar =c(12,2,1,2))
  beeswarm::beeswarm(predictionmodel$x*-1, vertical =FALSE,side=1, ylim = c(0.85,2), method=method, xlim=xlims, col=col2,pch=pch2, axes=F)
  par(new=T)
  beeswarm::beeswarm(centroids$centroid1*-1, vertical =FALSE, ylim=c(-0.2,3), side=-1,method=method, xlim=xlims,col=col1, pch=pch1, axes=F)
  par(new=T)
  beeswarm::beeswarm(x*-1, vertical =FALSE,  ylim= c(1,1.5),side=1,method=method, xlim=xlims, col=col3, pch=pch3, axes=F)
  axis(side=1)
}

#this function will plot just the centroids of the modern model and the archaeological datapoints
#all variables follow plot1
plot2<-function(model, x, xlims,method, pch1= NULL,  pch3= NULL, col1= NULL, col3= NULL){
  library(beeswarm)
  library(dplyr)
  library(haven)
  library(MASS)
  if (model=='temperate') load(file="data_model.rda")
  if(model=='temperate') discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCAHN+LOGCADN+FLOWPER,data.model, CV = TRUE)
  if(model=='temperate') model_lda <- lda(Study ~SLA+ARNODE+LOGCAHN+LOGCADN+FLOWPER,data.model)

  if(model=='arid')load (file="data_model_arid.rda")
  if(model=='arid') discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCAHN+LOGCADN,data.model, CV = TRUE)
  if(model=='arid') model_lda <- lda(Study ~SLA+ARNODE+LOGCAHN+LOGCADN,data.model)
  predictionmodel <- predict(model_lda,data.model)
  functionalAt <- data.frame(Study = as.factor(data.model$Study),
                             Classification= predictionmodel$class,
                             LD1 = predictionmodel$x)
  centroids <- functionalAt %>%
    group_by(Study) %>%
    summarise(centroid1 = mean(LD1))
  beeswarm::beeswarm(centroids$centroid1*-1, vertical =FALSE, side=-1,method=method, xlim=xlims,col=col1, pch=pch1, axes=F)
  par(new=T)
  beeswarm::beeswarm(x*-1, vertical =FALSE,  ylim= c(1,1.05),side=1,method=method, xlim=xlims, col=col3, pch=pch3,axes=F)
  axis(side=1)
}
