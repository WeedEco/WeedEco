#FIBSLDA package
#remember input csv for model.lda() needs to be in the order
#"SLA","ARNODE","LOGCAHN","LOGCADN","FLOWPER","Study"

#model.LDA() will take the functional attribute scores of archeaobotanical samples and compared them to a linear discriminant analysis
#of known crop husbandry regimes and classify the archaeobotanical samples in to either low intensity/extensive or high intensity/intensive
#cultivation. Currently two model types are available - the "temperate" model is suitable for temperate locations, while the "arid" model is
#suitable for semi-arid locations
model.LDA<-function(model,x){
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
  model <- cbind(as.data.frame(predict(model_lda,x)),x)
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

#this function uses beeswarm's swarmy function to plot the same variables as plot 2
# x is the column of data frame which as the LD1 results
#xlim is the limits of the x axis, while ticks is the location of the x axis labels
#col 1 and pch 1 = centroids, col3 and pch3 is the color and symbol of the archaeological data, col2 and pch2 are the model data
# priority can be ascending,descending,density or random, compact can be True or false - this are from beeswarm swarmy and change the look of the graphs

plot3<-function(model, x, xlims= NULL,ticks =NULL, col1="black",col2= "black",col3="black", pch1=1, pch2=2, pch3=0, compact= F, priority= "density", lines=F){
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
                             LD1 = predictionmodel$x,
                             pch= data.model$Study)
  centroids <- functionalAt %>%
    group_by(Study) %>%
    summarise(centroid1 = mean(LD1))
  x.value<-unlist(x*-1)
  m.value<-unlist(predictionmodel$x*-1)
  xmin<-min(x.value)
  xmax<-max(x.value)
  mmin<-min(m.value)
  mmax<-max(m.value)

  dlim<-extendrange(x.value)
  mlim<-extendrange(m.value)

  if(pch2>2){
     stop('the parameter "pch2" must be 0,1 or 2')
  }
  if(pch1>2){
    stop('the parameter "pch1" must be 0,1 or 2')
  }
  if (is.null(xlims)){
    if(xmin > mmin){
      min<-mmim
    }
    else {min<-xmin
    }
    if(xmax> mmax){
      max<-xmax
    }else {
      max<-mmax
    }
    xlims<-c(min-0.5,max+0.5)
}
if(is.null(ticks)){
  ticks<-round(min-0.5):round(max+0.5)
}

  functionalAt$pch[functionalAt$pch=="1"]<-pch2+15
  functionalAt$pch[functionalAt$pch=="2"]<-pch2

  par(mar=c(4,2,0,2), xpd=TRUE)
  plot(2:5, type='n', xlim = xlims, ylim=c(0,0.17),axes=F, xlab = "", ylab="")
  points(swarmy(centroids$centroid1*-1, rep(0.15,2)), col= col1, pch=c(pch1+15, pch1), cex=1.75)
  points(swarmy(functionalAt$LD1*-1, rep(0.1,2), side=1, compact=compact, priority = priority),col=col2, pch=as.numeric(functionalAt$pch),cex=1.2)
  points(swarmy(x*-1, rep(0.03, 2), side=1,compact=compact, priority=priority), col= col3, pch=pch3,cex=1.2)
  axis(1, ticks, cex=1.5)
  if (lines== T) segments(min(centroids$centroid1*-1), 0.148,min(centroids$centroid1*-1),-0.007 )
  if (lines== T) segments(max(centroids$centroid1*-1), 0.148,max(centroids$centroid1*-1),-0.007 )
  legend("topright", inset=c(-0.05,0.05), c("Group \ncentroids", "Model","Archaeological \nsamples"), pch=c(pch1,pch2, pch3), col= c(col1,col2,col3), cex=0.95, bty="n")
}
#this function uses beeswarm's swarmy function to plot the same variables as plot 2
#x is the column of dataframe which as the LD1 results
#xlim is the limits of the x axis, while ticks is the location of the x axis labels
#col 1 and pch 1 = centroids, col3 and pch3 is the color and symbol of the archaeological data


plot4<-function(model, x, xlims= NULL, ylims = NULL, ticks =NULL, col1="black",col3="black", pch1=1, pch3=0, compact= F, priority= "density", lines=F){
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
                             LD1 = predictionmodel$x,
                             pch= data.model$Study)
  centroids <- functionalAt %>%
    group_by(Study) %>%
    summarise(centroid1 = mean(LD1))
  x.value<-unlist(x*-1)
  m.value<-unlist(predictionmodel$x*-1)
  xmin<-min(x.value)
  xmax<-max(x.value)
  mmin<-min(m.value)
  mmax<-max(m.value)

  dlim<-extendrange(x.value)
  mlim<-extendrange(m.value)

  if(pch1>2){
    stop('the parameter "pch1" must be 0,1 or 2')
  }
  if (is.null(xlims)){
    if(xmin > mmin){
      min<-mmim
    }
    else {min<-xmin
    }
    if(xmax> mmax){
      max<-xmax
    }else {
      max<-mmax
    }
    xlims<-c(min-0.5,max+0.5)
  }
  if(is.null(ticks)){
    ticks<-round(min-0.5):round(max+0.5)
  }
  if (is.null(ylims)){
    ylims<-c(0,1)
  }

  par(mar = c(6,2,6,2))
  plot(2:3, type='n', xlim = xlims, ylim=ylims,axes=T, xlab = "", ylab="")
  points(swarmy(centroids$centroid1*-1, rep(0.2,2)), col= col1, pch=c(pch1+15, pch1), cex=1.75)
  points(swarmy(x*-1, rep(0.08, 2), side=1,compact=compact, priority = priority), col= col3, pch=pch3)
  axis(1, ticks)
  if (lines== T) segments(min(centroids$centroid1*-1), 0.189,min(centroids$centroid1*-1),-0.05 )
  if (lines== T) segments(max(centroids$centroid1*-1), 0.189,max(centroids$centroid1*-1),-0.05 )
  legend("topright", inset=c(-0.05,0.05), c("Group \ncentroids", "Model","Archaeological \nsamples"), pch=c(pch1,pch3), col= c(col1,col3), cex=0.95, bty="n")
}

plot5<-function(model, x, xlims= NULL,ticks =NULL, col1="black",col2= "black",col3="black", pch1=1, pch2=15, pch3=0, compact= F, priority= "descending", site= "Archaeological samples", lines=F){
    library(beeswarm)
    library(dplyr)
    library(haven)
    library(MASS)

    if (model=='temperate') {load(file="data_model.rda")
  discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCAHN+LOGCADN+FLOWPER,data.model, CV = TRUE)
     model_lda <- lda(Study ~SLA+ARNODE+LOGCAHN+LOGCADN+FLOWPER,data.model)

    predictionmodel <- predict(model_lda,data.model)

    functionalAt <- data.frame(Study = as.factor(data.model$Study),
                                                      Classification= predictionmodel$class,
                                                      LD1 = predictionmodel$x,
                                                      husbandry= data.model$Study)

    centroids <- functionalAt %>%
      group_by(Study) %>%
      summarise(centroid1 = mean(LD1))
    x.value<-unlist(x*-1)
    m.value<-unlist(predictionmodel$x*-1)
    xmin<-min(x.value)
    xmax<-max(x.value)
    mmin<-min(m.value)
    mmax<-max(m.value)

    dlim<-extendrange(x.value)
    mlim<-extendrange(m.value)
    if (is.null(xlims)){
      if(xmin > mmin){
        min<-mmim
      }
      else {min<-xmin
      }
      if(xmax> mmax){
        max<-xmax
      }else {
        max<-mmax
      }
      xlims<-c(min-1,max+1)
    }
    if(is.null(ticks)){
      ticks<-round(min-1):round(max+1)
    }
    AsturiasPro<-functionalAt[functionalAt$husbandry =="1"|functionalAt$husbandry =="2",]
    AsturiasPro$husbandry[AsturiasPro$husbandry=="1"]<-15
    AsturiasPro$husbandry[AsturiasPro$husbandry=="2"]<-0


    par(mar=c(4,2,0,2), xpd=TRUE)
    plot(2:5, type='n', xlim = xlims, ylim=c(0,0.17),axes=F, xlab = "", ylab="")
    points(swarmy(centroids$centroid1*-1, rep(0.15,2)), col= col1, pch=c(pch1+15, pch1), cex=1.75)
    points(swarmy(AsturiasPro$LD1*-1, rep(0.09,2), side=1, compact=compact, priority = priority),col=col2, pch=as.numeric(AsturiasPro$husbandry),cex=1.2)

    points(swarmy(x*-1, rep(0.03, 2), side=1,compact=compact, priority=priority), col= col3, pch=pch3,cex=1.2)
    axis(1, ticks, cex=1.5)
    if (lines== T) segments(min(centroids$centroid1*-1), 0.148,min(centroids$centroid1*-1),-0.007 )
    if (lines== T)  segments(max(centroids$centroid1*-1), 0.148,max(centroids$centroid1*-1),-0.007 )
    legend(max-2,0.12,  c( "Asturias", "Haute \nProvence"), pch=c(0,15), col=col2, cex=0.95, bty="n")
    legend(max-2,0.16,  c("Group \ncentroids"), pch=c(pch1), col= c(col1), cex=0.95, bty="n")
    legend(max-2,0.05,  legend =site, pch=c( pch3), col= c(col3), cex=0.95, bty="n")
    }


    if(model=='arid'){load (file="data_model_arid.rda")
     discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCAHN+LOGCADN,data.model, CV = TRUE)
     model_lda <- lda(Study ~SLA+ARNODE+LOGCAHN+LOGCADN,data.model)
      predictionmodel <- predict(model_lda,data.model)
      functionalAt <- data.frame(Study = as.factor(data.model$Study),
                               Classification= predictionmodel$class,
                               LD1 = predictionmodel$x,
                               husbandry= data.model$husbandry)

      centroids <- functionalAt %>%
      group_by(Study) %>%
      summarise(centroid1 = mean(LD1))
      x.value<-unlist(x*-1)
     m.value<-unlist(predictionmodel$x*-1)
      xmin<-min(x.value)
      xmax<-max(x.value)
      mmin<-min(m.value)
      mmax<-max(m.value)

      dlim<-extendrange(x.value)
      mlim<-extendrange(m.value)
      if (is.null(xlims)){
      if(xmin > mmin){
        min<-mmim
      }
      else {min<-xmin
      }
      if(xmax> mmax){
        max<-xmax
      }else {
        max<-mmax
      }
      xlims<-c(min-1,max+1)
      }
      if(is.null(ticks)){
      ticks<-round(min-1):round(max+1)
      }
    evvia<-functionalAt[functionalAt$husbandry=="Evvia group 2"| functionalAt$husbandry=="Evvia group 1",]
    evvia$husbandry[evvia$husbandry=="Evvia group 1"]<-2
    evvia$husbandry[evvia$husbandry=="Evvia group 2"]<-17
    AsturiasPro<-functionalAt[functionalAt$husbandry=="Asturias"| functionalAt$husbandry=="Provence",]
    AsturiasPro$husbandry[AsturiasPro$husbandry=="Asturias"]<-15
    AsturiasPro$husbandry[AsturiasPro$husbandry=="Provence"]<-0
    Morocco<-functionalAt[functionalAt$husbandry== "Morocco oases" | functionalAt$husbandry=="Morocco rain-fed terraces",]
    Morocco$husbandry[Morocco$husbandry== "Morocco oases"]<-16
    Morocco$husbandry[Morocco$husbandry== "Morocco rain-fed terraces"]<-1

    par(mar=c(4,2,0,2), xpd=TRUE)
    plot(2:5, type='n', xlim = xlims, ylim=c(0,0.17),axes=F, xlab = "", ylab="")
    points(swarmy(centroids$centroid1*-1, rep(0.15,2)), col= col1, pch=c(pch1+15, pch1), cex=1.75)
    points(swarmy(evvia$LD1*-1, rep(0.12,2), side=1, compact=compact, priority = priority),col=col2, pch=as.numeric(evvia$husbandry),cex=1.2)
    points(swarmy(AsturiasPro$LD1*-1, rep(0.09,2), side=1, compact=compact, priority = priority),col=col2, pch=as.numeric(AsturiasPro$husbandry),cex=1.2)
    points(swarmy(Morocco$LD1*-1, rep(0.06,2), side=1, compact=compact, priority = priority),col=col2, pch=as.numeric(Morocco$husbandry),cex=1.2)

    points(swarmy(x*-1, rep(0.03, 2), side=1,compact=compact, priority=priority), col= col3, pch=pch3,cex=1.2)
    axis(1, ticks, cex=1.5)
    if (lines== T) segments(min(centroids$centroid1*-1), 0.148,min(centroids$centroid1*-1),-0.007 )
    if (lines== T) segments(max(centroids$centroid1*-1), 0.148,max(centroids$centroid1*-1),-0.007 )
    legend(max-2,0.14,  c( "Evvia feilds", "Evvia gardens"), pch=c(2,17), col=col2, cex=0.95, bty="n")
    legend(max-2,0.12,  c( "Asturias", "Haute \nProvence"), pch=c(0,15), col=col2, cex=0.95, bty="n")
    legend(max-2,0.09,  c( "Morocco oases","Morocco \nrain-fed terraces"), pch=c(1,16), col=col2, cex=0.95, bty="n")
    legend(max-2,0.16,  c("Group \ncentroids"), pch=c(pch1), col= c(col1), cex=0.95, bty="n")
    legend(max-2,0.05,  legend =site, pch=c( pch3), col= c(col3), cex=0.95, bty="n")
}
}
