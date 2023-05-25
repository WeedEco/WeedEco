
wplot_phase<-function(model, x, site= c("phase 1", "phase 2", "phase 3", "phase 4", "phase 5"), g1=NULL, g2= NULL, g3=NULL, g4=NULL, g5=NULL, gcol=c("black","black","black","black", "black"),
                           gpch=c(22,23,24,8,4), gbg=c("grey","grey","grey","grey", "grey"),pcolumn, compact= FALSE, priority= "density", xlims= NULL, ylims = NULL, ticks =NULL, xlab="Discriminant function", lines=TRUE){
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
  colnames(x) = gsub("*", "", colnames(x))
  y<-x$LD1
  x.value<-unlist(y)
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
  if (is.null(ylims)){
    ylims<-c(0,1)
  }


  if(is.null(g3)){

    group1<-x[pcolumn==g1,]
    group2<-x[pcolumn==g2,]

    par(mfrow=c(2,1))
    par(oma=c(5,2,1,2))
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim = xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
     points(beeswarm::swarmy(group1$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[1], pch=gpch[1], bg=gbg[1])
    axis(1,ticks)
    mtext(side =1, text = xlab, line=2, cex=0.8)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white" ))
    legend("right", inset=c(0.05,0.05), site[1], col=gcol[1], pch=gpch[1], pt.bg=gbg[1],cex=0.85, bty="n")
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim = xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
    points(beeswarm::swarmy(group2$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[2], pch=gpch[2], bg=gbg[2])
    axis(1, ticks)
    mtext(side =1, text = xlab, line=2, cex=0.8)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
    legend("right", inset=c(0.05,0.05), site[2], col=gcol[2], pch=gpch[2],pt.bg=gbg[2], cex=0.85, bty="n")
    par(mfrow=c(1,1))
    }
  else if(is.null(g4)){
      group1<-x[pcolumn==g1,]
      group2<-x[pcolumn==g2,]
      group3<-x[pcolumn==g3,]
    par(mfrow=c(3,1))
    par(oma=c(5,2,1,2))
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim = xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")

    points(beeswarm::swarmy(group1$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[1], pch=gpch[1],bg=gbg[1])
    axis(1, ticks )
    mtext(side =1, text = xlab, line=2, cex=0.7)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
    legend("right", inset=c(0.05,0.05), site[1], col=gcol[1], pch=gpch[1], pt.bg=gbg[1],cex=0.85, bty="n")
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim = xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
      points(beeswarm::swarmy(group2$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[2], pch=gpch[2], bg=gbg[2])
    axis(1, ticks)
    mtext(side =1, text = xlab, line=2, cex=0.7)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
    legend("right", inset=c(0.05,0.05), site[2], col=gcol[2], pch=gpch[2],pt.bg=gbg[2], cex=0.85, bty="n")
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim = xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
     points(beeswarm::swarmy(group3$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[3], pch=gpch[3], bg=gbg[3])
    axis(1, ticks)
    mtext(side =1, text = xlab, line=2, cex=0.7)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
    legend("right", inset=c(0.05,0.05), site[3], col=gcol[3], pch=gpch[3],pt.bg=gbg[3], cex=0.85, bty="n")
    par(mfrow=c(1,1))}
  else if(is.null(g5)){
    group1<-x[pcolumn==g1,]
    group2<-x[pcolumn==g2,]
    group3<-x[pcolumn==g3,]
    group4<-x[pcolumn==g4,]
    par(mfrow=c(4,1))
    par(oma=c(5,2,1,2))
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim =xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
    points(beeswarm::swarmy(group1$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[1], pch=gpch[1], bg=gbg[1])
    axis(1, ticks )
    mtext(side =1, text = xlab, line=2, cex=0.7)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
     legend("right", inset=c(0.05,0.05), site[1], col=gcol[1], pch=gpch[1], pt.bg=gbg[1],cex=0.85, bty="n")
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim = xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
     points(beeswarm::swarmy(group2$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[2], pch=gpch[2], bg=gbg[2])
    axis(1,ticks)
    mtext(side =1, text = xlab, line=2, cex=0.7)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
    legend("right", inset=c(0.05,0.05), site[2], col=gcol[2], pch=gpch[2],pt.bg=gbg[2], cex=0.85, bty="n")
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim = xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
    points(beeswarm::swarmy(group3$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[3], pch=gpch[3], bg=gbg[3])
    axis(1,ticks)
    mtext(side =1, text = xlab, line=2, cex=0.7)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
    legend("right", inset=c(0.05,0.05), site[3], col=gcol[3], pch=gpch[3],pt.bg=gbg[3], cex=0.85, bty="n")
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim = xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
    points(beeswarm::swarmy(group4$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[4], pch=gpch[4], bg=gbg[4])
    axis(1, ticks)
    mtext(side =1, text = xlab, line=2, cex=0.7)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
    legend("right", inset=c(0.05,0.05), site[4], col=gcol[4], pch=gpch[4], cex=0.85,pt.bg=gbg[4], bty="n")
    par(mfrow=c(1,1))}
    else{
    group1<-x[pcolumn==g1,]
    group2<-x[pcolumn==g2,]
    group3<-x[pcolumn==g3,]
    group4<-x[pcolumn==g4,]
    group5<-x[pcolumn==g5,]
    par(mfrow=c(5,1))
    par(oma=c(5,2,1,2))
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim = xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
    points(beeswarm::swarmy(group1$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[1], pch=gpch[1], bg=gbg[1])
    axis(1, ticks)
    mtext(side =1, text = xlab, line=2, cex=0.7)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
    legend("right", inset=c(0.05,0.05), site[1], col=gcol[1], pch=gpch[1], pt.bg=gbg[1], cex=0.85, bty="n")
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim = xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
    points(beeswarm::swarmy(group2$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[2], pch=gpch[2], bg=gbg[2])
    axis(1, ticks)
    mtext(side =1, text = xlab, line=2, cex=0.7)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
    legend("right", inset=c(0.05,0.05), site[2], col=gcol[2], pch=gpch[2],pt.bg=gbg[2], cex=0.85, bty="n")
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim =xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
     points(beeswarm::swarmy(group3$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[3], pch=gpch[3],bg=gbg[3] )
    axis(1, ticks)
    mtext(side =1, text = xlab, line=2, cex=0.7)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
    legend("right", inset=c(0.05,0.05), site[3], col=gcol[3], pch=gpch[3],pt.bg=gbg[3], cex=0.85, bty="n")
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim = xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
     points(beeswarm::swarmy(group4$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[4], pch=gpch[4], bg=gbg[4])
    axis(1, ticks)
    mtext(side =1, text = xlab, line=2, cex=0.7)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
    legend("right", inset=c(0.05,0.05), site[4], col=gcol[4], pch=gpch[4],pt.bg=gbg[4], cex=0.85, bty="n")
    par(mar = c(0,0,0,0))
    plot(2:3, type='n', xlim = xlim, ylim=c(0,1),axes=F, xlab = "", ylab="")
    points(beeswarm::swarmy(group5$LD1, rep(0.2, 2), side=1), cex=1.25, col=gcol[5], pch=gpch[5], bg=gbg[5])
    axis(1,ticks)
    mtext(side =1, text = xlab, line=2, cex=0.7)
    segments(min(centroids$centroid1*-1), 0.58,min(centroids$centroid1*-1),-0.6)
    segments(max(centroids$centroid1*-1), 0.589,max(centroids$centroid1*-1),-0.04 )
    points(beeswarm::swarmy(centroids$centroid1*-1, rep(0.6,2)),col= c("black", "black"), pch=c(21, 21), cex=1.75, bg=c( "black","white"))
    legend("right", inset=c(0.05,0.05), site[5], col=gcol[5], pch=gpch[5],pt.bg=gbg[5], cex=0.85, bty="n")}
  par(mfrow=c(1,1))
  }



