#this function uses beeswarm's swarmy function to plot the same variables as plot 2
#x is the column of dataframe which as the LD1 results
#xlim is the limits of the x axis, while ticks is the location of the x axis labels
#col 1 and pch 1 = centroids, col3 and pch3 is the color and symbol of the archaeological data


plot4<-function(model, x, xlims= NULL, ylims = NULL, ticks =NULL, col1="black",col3="black", pch1=1, pch3=0, compact= F, priority= "density", lines=F, site){
  library(beeswarm)
  library(dplyr)
  library(haven)
  library(MASS)

  if (model=='model1'|model== 1) load(file="data_model.rda")
  if(model=='model1'|model== 1) discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCAHN+LOGCADN+FLOWPER,data.model, CV = TRUE)
  if(model=='model1'|model== 1) model_lda <- lda(Study ~SLA+ARNODE+LOGCAHN+LOGCADN+FLOWPER,data.model)

  if(model=='model2'|model== 2)load (file="data_model_arid.rda")
  if(model=='model2'|model== 2) discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCAHN+LOGCADN,data.model, CV = TRUE)
  if(model=='model2'|model== 2) model_lda <- lda(Study ~SLA+ARNODE+LOGCAHN+LOGCADN,data.model)

  if(model=='model3'|model== 3) load(file="model3.rda")
  if(model=='model3'|model== 3) discrim_cv <- lda(Study ~ FLOWPER+VEGPROP,data.model, CV = TRUE)
  if(model=='model3'|model== 3) model_lda <- lda(Study ~FLOWPER+VEGPROP,data.model)


  predictionmodel <- predict(model_lda,data.model)
  functionalAt <- data.frame(Study = as.factor(data.model$Study),
                             Classification= predictionmodel$class,
                             LD1 = predictionmodel$x,
                             pch= data.model$Study)
  centroids <- functionalAt %>%
    group_by(Study) %>%
    summarise(centroid1 = mean(LD1))
  x.value<-unlist(x)
  m.value<-unlist(predictionmodel$x*-1)
  xmin<-min(x.value)
  xmax<-max(x.value)
  mmin<-min(m.value)
  mmax<-max(m.value)


  if(pch1>2){
    stop('the parameter "pch1" must be 0,1 or 2')
  }
  if(xmin > mmin){
    min<-mmin
  }else {
    min<-xmin
  }
  if(xmax > mmax){
    max<-xmax
  }else {
    max<-mmax
  }

  if (length(xlims)){
    xlim<-xlims
  }else {
    xlim<-c(min-1,max+1)}

  if(is.null(ticks)){
    ticks<-round(min-0.5):round(max+0.5)
  }
  if (is.null(ylims)){
    ylims<-c(0,1)
  }

  par(mar = c(6,2,6,3))
  plot(2:3, type='n', xlim = xlim, ylim=ylims,axes=F, xlab = "", ylab="")
  points(swarmy(centroids$centroid1*-1, rep(0.4,2)), col= col1, pch=c(pch1+15, pch1), cex=1.75)
  points(swarmy(x, rep(0.1, 2), side=1,compact=compact, priority = priority), col= col3, pch=pch3)
  axis(1, ticks)
  if (lines== T) segments(min(centroids$centroid1*-1), 0.389,min(centroids$centroid1*-1),-0.04 )
  if (lines== T) segments(max(centroids$centroid1*-1), 0.389,max(centroids$centroid1*-1),-0.04 )
  legend( max-1,0.5, c("Group \ncentroids"), pch=c(pch1), col= c(col1), cex=0.95, bty="n")
  legend( max-1,0.3, site, pch=c(pch3), col= c(col1), cex=0.95, bty="n")

}


