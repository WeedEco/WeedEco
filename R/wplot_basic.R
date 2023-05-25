#this function uses beeswarm's swarmy function to plot the same variables as plot 2
# x is the column of data frame which as the LD1 results
#xlim is the limits of the x axis, while ticks is the location of the x axis labels
#col 1 and pch 1 = centroids, col3 and pch3 is the color and symbol of the archaeological data, col2 and pch2 are the model data
# priority can be ascending,descending,density or random, compact can be True or false - this are from beeswarm swarmy and change the look of the graphs

wplot_basic<-function(model, x, xlims= NULL,ticks =NULL, col1="black",col2= "black",col3="black", pch1=1, pch2=2, pch3=0, xlab="Discriminant function",compact= FALSE, priority= "density", lines=TRUE, site="Samples", lcol=col3, lpch=pch3, legend="split"){
  REGIME<-Group<-LD1<-NULL
  if(model=='model1'|model== 1) data.model<-data.frame(model1)
  if(model=='model1'|model== 1) discrim_cv <- lda(REGIME ~ SLA+ARNODE+LOGCANH+LOGCAND+FLOWPER,data.model, CV = TRUE)
  if(model=='model1'|model== 1) model_lda <- lda(REGIME ~SLA+ARNODE+LOGCANH+LOGCAND+FLOWPER,data.model)

  if(model=='model2'|model== 2)data.model<-data.frame(model2)
  if(model=='model2'|model== 2) discrim_cv <- lda(REGIME ~ SLA+ARNODE+LOGCANH+LOGCAND,data.model, CV = TRUE)
  if(model=='model2'|model== 2) model_lda <- lda(REGIME ~SLA+ARNODE+LOGCANH+LOGCAND,data.model)

  if(model=='model3'|model== 3) data.model<-data.frame(model3)
  if(model=='model3'|model== 3) discrim_cv <- lda(REGIME ~ FLOWPER+VEGPROP,data.model, CV = TRUE)
  if(model=='model3'|model== 3) model_lda <- lda(REGIME ~FLOWPER+VEGPROP,data.model)

  predictionmodel <- predict(model_lda,data.model)
  functionalAt <- data.frame(REGIME = as.factor(data.model$REGIME),
                             Classification= predictionmodel$class,
                             LD1 = predictionmodel$x,
                             pch= data.model$REGIME)
  centroids <- functionalAt %>%
    group_by(REGIME) %>%
    summarise(centroid1 = mean(LD1))

  x.value<-unlist((x))
  m.value<-unlist(predictionmodel$x*-1)
  xmin<-min(x.value)
  xmax<-max(x.value)
  mmin<-min(m.value)
  mmax<-max(m.value)

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

  if(length(ticks)){
    ticks<-ticks
  }else{
    ticks<- round(min-0.5):round(max+0.5)
  }

  if(pch1>2){
    stop('the parameter "pch1" must be 0,1 or 2')
  }

  if(pch2>2){
    stop('the parameter "pch1" must be 0,1 or 2')
  }

  functionalAt$pch[functionalAt$pch=="1"]<-pch2+15
  functionalAt$pch[functionalAt$pch=="2"]<-pch2

  par(mar=c(4,2,0,4), xpd=TRUE)
  plot(2:5, type='n', xlim = xlim, ylim=c(0,0.17),axes=F, xlab = "", ylab="")
  points(swarmy(centroids$centroid1*-1, rep(0.15,2)), col= col1, pch=c(pch1+15, pch1), cex=1.75)
  points(swarmy(functionalAt$LD1*-1, rep(0.09,2), side=1, compact = compact, priority = priority),col=col2, pch=as.numeric(functionalAt$pch),cex=1.2)
  points(swarmy(x, rep(0.03, 2), side=1,compact = compact, priority = priority), col= col3, pch=as.numeric(pch3), cex=1.2)
  axis(1, ticks, cex=1.5)
  mtext(side =1, text = xlab, line=2, cex=0.7)
  if (lines== T) segments(min(centroids$centroid1*-1), 0.148,min(centroids$centroid1*-1),-0.007 )
  if (lines== T) segments(max(centroids$centroid1*-1), 0.148,max(centroids$centroid1*-1),-0.007 )
  if (lines== T)points(swarmy(centroids$centroid1*-1, rep(0.15,2)), col= col1, pch=c(pch1+15, pch1+20), cex=1.75, bg="white")

  lname<-unique(site)
  if (legend =="right") legend("topright", inset=c(-0.05, 0.05), c("Group 1 centroid","Group 2 centroid", "Group 1", "Group 2",lname), pch=c(pch1 ,pch1+15, pch2+15,pch2, as.numeric(lpch)), col= c(col1,col2,col2,lcol), cex=0.85, bty="n")
  if (legend =="left") legend("topleft", inset=c(-0.05, 0.05), c("Group 1 centroid", "Group 2 centroid","Group 1", "Group 2",lname), pch=c(pch1 , pch1+15, pch2+15, pch2, as.numeric(lpch)), col= c(col1,col2,col2,lcol), cex=0.85, bty="n")
  if (legend =="split") {legend(max+0.5,0.162,  c("Group 1 \ncentroid"), pch=c(pch1+15), col= c(col1), cex=0.85, bty="n")
    legend(max+0.5,0.143,  c("Group 2 \ncentroid"), pch=c(pch1), col= c(col1), cex=0.85, bty="n")
    legend(max+0.5,0.06,  legend =lname, pch=c(lpch), col= c(lcol), cex=0.85, bty="n")
    legend(max+0.5,0.12, c("Group 1", "Group 2"), pch=c(pch2+15, pch2), col= c(col2), cex=0.85, bty="n")}
}
