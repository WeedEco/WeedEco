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
  functionalAt <- data.frame(Group = as.factor(data.model$Study),
                             Classification= predictionmodel$class,
                             LD1 = predictionmodel$x)
  functionalAt50 <- data.frame(Group = as.factor(data.model$Study),
                             Classification= predictionmodel50$class,
                             LD1 = predictionmodel50$x)

  centroids <- functionalAt %>%
    group_by(Group) %>%
    summarise(Centroid1 = mean(LD1))
  centroids<-as.data.frame(centroids)
  centroids$Centroid1<-centroids$Centroid1*-1
  centroids50 <- functionalAt50 %>%
    group_by(Group) %>%
    summarise(Centroid1 = mean(LD1))
  centroids50<-as.data.frame(centroids50)
  centroids50$Centroid1<-centroids50$Centroid1*-1
#   gc_proc_1 <- apply(data.model[which(data.model$Study == "1"), 2:5], 2, function(x) {x - mean (x)})
#   gc_proc_2 <- apply(data.model[which(data.model$Study == "2"), 2:5],2, function(x) {x - mean (x)})
#
#   #Calculating an SSCP matrix (see: https://stats.stackexchange.com/a/22520) for each group
#   SSCP_1_gc <- crossprod(gc_proc_1)
#   SSCP_2_gc <- crossprod(gc_proc_2)
#
#   #Taking the sum of these to give Sw
#   Sw <- SSCP_1_gc + SSCP_2_gc
#   #Centering the iris data to calculate the total scatter matrix
#   c_proc <- apply(data.model[,2:5], 2, FUN = function(x) {(x - mean(x))})
#
#   #Calculating the total scatter matrix
#   St <- crossprod(c_proc)
#
#   #And the between group scatter matrix
#   Sb <- St - Sw
#
#   #The cholesky root of Sw
#   U <- chol(Sw)
#
#   #Calculation of the eigenvectors of the LDA
#   LDA_V <- solve(U) %*% eigen(t(solve(U)) %*% Sb %*% solve(U))$vectors
#
#   #The eigenvectors
#   LDA_V
#
#   # Standarized coefficients
#   sqrt(diag(Sw)) * LDA_V[,1:3]
#
#   #this works and produces the structure matrix - slight different to spss
#  st<- solve(sqrt(diag(diag(Sw)))) %*% Sw %*% LDA_V[,1:3]
#  colnames(st)<-c("LD1", "LD2", "LD3")
# row.names(st)<-c("SLA","ARNODE","LOGCANH","LOGCAND","FLOWPER")
#   # groupmean<-(model_lda$prior%*%model_lda$means)
#   # constant<-(groupmean%*%model_lda$scaling)
#   # LDAhand<-scale(x,center=groupmean, scale=false)%*%model_lda$scaling
  x<-na.omit(x)
  models <- cbind( as.data.frame(predict(model_lda50,x)),as.data.frame((predict(model_lda,x))))
  names(models)<-c("CLASS_std*", "Prob.1_std*", "Prob.2_std*", "Ld1_std","Class", "Prob.1", "Prob.2", "LD1*")
  models$`LD1*`<-models$LD1*-1
  models<-models%>% mutate(across(where(is.numeric), round, digits =3))
  model<-models
  model_print<-models[c(1:3,8)]
  names(model_print)<-c("CLASS_std*", "Prob.1_std*", "Prob.2_std*", "LD1*")
  model_print<-cbind(model_print)
  cat("\nResults and linear discriminant scores:\n")
  print(model_print)
  cat("\nCentroids:\n")
  centroids<-centroids%>% mutate(across(where(is.numeric), round, digits =3))
  print(centroids)
  # cat("\nStructure matrix:\n")
  # print(st)
 results<-model
}

