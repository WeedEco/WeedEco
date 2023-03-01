wplot_geog<-function(model, x, xlims= NULL,ticks =NULL, col1="black",col2= "black",col3="black", pch1=1,pch3=5, xlab="Discriminant function",compact= F, priority= "descending", site= "Samples", lcol=col3, lpch=pch3,lines=T, legend=T){

  if(model=='model1'|model== 1) {
    data.model<-data.frame(model1)
    discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCANH+LOGCAND+FLOWPER,data.model, CV = TRUE)
    model_lda <- lda(Study ~SLA+ARNODE+LOGCANH+LOGCAND+FLOWPER,data.model)

    predictionmodel <- predict(model_lda,data.model)

    functionalAt <- data.frame(Study = as.factor(data.model$Study),
                               Classification= predictionmodel$class,
                               LD1 = predictionmodel$x,
                               husbandry= data.model$Study)

    centroids <- functionalAt %>%
      group_by(Study) %>%
      summarise(centroid1 = mean(LD1))
    x.value<-unlist(x)
    m.value<-unlist(predictionmodel$x*-1)
    xmin<-min(x.value)
    xmax<-max(x.value)
    mmin<-min(m.value)
    mmax<-max(m.value)
    xlimss<-xlims

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

    if (length(xlimss)){
      xlim<-xlimss
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


    AsturiasPro<-functionalAt[functionalAt$husbandry =="1"|functionalAt$husbandry =="2",]
    AsturiasPro$husbandry[AsturiasPro$husbandry=="1"]<-15
    AsturiasPro$husbandry[AsturiasPro$husbandry=="2"]<-0


    par(mar=c(4,2,4,3), xpd=TRUE)
    plot(2:5, type='n', xlim = xlim, ylim=c(0,0.17),axes=F, xlab = "", ylab="")
    points(swarmy(centroids$centroid1*-1, rep(0.15,2)), col= col1, pch=c(pch1+15, pch1), cex=1.75)
    points(swarmy(AsturiasPro$LD1*-1, rep(0.07,2), side=1, compact=compact, priority = priority),col=col2, pch=as.numeric(AsturiasPro$husbandry),cex=1.2)
    points(swarmy(x, rep(0.02, 2), side=1,compact=compact, priority=priority), col= col3, pch=pch3,cex=1.2)
    axis(1,ticks, cex=1.5)
    mtext(side =1, text = xlab, line=2, cex=0.8)
    if (lines== T) segments(min(centroids$centroid1*-1), 0.15,min(centroids$centroid1*-1),-0.007 )
    if (lines== T)  segments(max(centroids$centroid1*-1), 0.15,max(centroids$centroid1*-1),-0.007 )
    if (lines== T)points(swarmy(centroids$centroid1*-1, rep(0.15,2)), col= col1, pch=c(pch1+15, pch1+20), cex=1.75, bg="white")

    if(legend==T){legend(max(xlim)-1,0.11,  c( "Haute \nProvence", "Asturias" ), pch=c(0,15), col=col2, cex=0.85, bty="n")
    lname<-unique(site)
    legend(max(xlim)-1,0.16, c("Group 1 \ncentroid"), pch=c(pch1+15), col= c(col1), cex=0.85, bty="n")
    legend(max(xlim)-1,0.14, c("Group 2 \ncentroid"), pch=c(pch1), col= c(col1), cex=0.85, bty="n")
    legend(max(xlim)-1,0.05,  legend =site, pch=c(lpch), col= c(lcol), cex=0.85, bty="n")}

  }
 if(model=='model2'|model== 2) {
  data.model<-data.frame(model2)
    discrim_cv <- lda(Study ~ SLA+ARNODE+LOGCANH+LOGCAND,data.model, CV = TRUE)
    model_lda <- lda(Study ~SLA+ARNODE+LOGCANH+LOGCAND,data.model)
    predictionmodel <- predict(model_lda,data.model)
    functionalAt <- data.frame(Study = as.factor(data.model$Study),
                               Classification= predictionmodel$class,
                               LD1 = predictionmodel$x,
                               husbandry= data.model$regime)

    centroids <- functionalAt %>%
      group_by(Study) %>%
      summarise(centroid1 = mean(LD1))
    x.value<-unlist(x)
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

    if(is.null(ticks)){
      ticks<-round(min-0.5):round(max+0.5)
    }

    if(pch1>2){
      stop('the parameter "pch1" must be 0,1 or 2')
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

    par(mar=c(4,2,2,2), xpd=TRUE)
    plot(2:5, type='n', xlim = c(min-1, max+3), ylim=c(0,0.17),axes=F, xlab = "", ylab="")
    points(swarmy(centroids$centroid1*-1, rep(0.16,2)), col= col1, pch=c(pch1+15, pch1), cex=1.75)
    points(swarmy(evvia$LD1*-1, rep(0.08,2), side=1, compact=compact, priority = priority),col=col2, pch=as.numeric(evvia$husbandry),cex=1.2)
    points(swarmy(AsturiasPro$LD1*-1, rep(0.11,2), side=1, compact=compact, priority = priority),col=col2, pch=as.numeric(AsturiasPro$husbandry),cex=1.2)
    points(swarmy(Morocco$LD1*-1, rep(0.05,2), side=1, compact=compact, priority = priority),col=col2, pch=as.numeric(Morocco$husbandry),cex=1.2)

    points(swarmy(x, rep(0.02, 2), side=1,compact=compact, priority=priority), col= col3, pch=pch3,cex=1.2)
    axis(1, ticks, cex=1.5)
    mtext(side =1, text = xlab, line=2, cex=0.8)
    if (lines== T) segments(min(centroids$centroid1*-1), 0.16,min(centroids$centroid1*-1),-0.007 )
    if (lines== T) segments(max(centroids$centroid1*-1), 0.16,max(centroids$centroid1*-1),-0.007 )
    if (lines== T)points(swarmy(centroids$centroid1*-1, rep(0.16,2)), col= col1, pch=c(pch1+15, pch1+20), cex=1.75, bg="white")


    if(legend==T){legend(max+0.5,0.11,  c( "Evvia fields", "Evvia gardens"), pch=c(2,17), col=col2, cex=0.85, bty="n")
      legend(max+0.5,0.14,  c( "Haute \nProvence", "Asturias" ), pch=c(0,15), col=col2, cex=0.85, bty="n")
      legend(max+0.5,0.085,  c( "Morocco \noases","Morocco \nrain-fed \nterraces"), pch=c(1,16), col=col2, cex=0.85, bty="n")

      lname<-unique(site)
      legend(max+0.5,0.17, c("Group 1 \ncentroid"), pch=c(pch1+15), col= c(col1), cex=0.85, bty="n")
      legend(max+0.5,0.16, c("Group 2 \ncentroid"), pch=c(pch1), col= c(col1), cex=0.85, bty="n")

      legend(max+0.5,0.04, legend =site, pch=c(lpch), col= c(lcol), cex=0.85, bty="n")}


  }
  if (model=='model3'|model== 3) {
    data.model<-data.frame(model3)
    discrim_cv <- lda(Study ~ FLOWPER+VEGPROP,data.model, CV = TRUE)
    model_lda <- lda(Study ~FLOWPER+VEGPROP,data.model)
    predictionmodel <- predict(model_lda,data.model)
    functionalAt <- data.frame(Study = as.factor(data.model$Study),
                               Classification= predictionmodel$class,
                               LD1 = predictionmodel$x,
                               husbandry= data.model$Field)

    centroids <- functionalAt %>%
      group_by(Study) %>%
      summarise(centroid1 = mean(LD1))
    x.value<-unlist(x)
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
      xlim<-c(min-0.5,max+0.5)}

    if(length(ticks)){
      ticks<-ticks
    }else{
      ticks<- round(min-1):round(max+1)
    }

    if(pch1>2){
      stop('the parameter "pch1" must be 0,1 or 2')
    }
    laxton<- functionalAt[functionalAt$husbandry=="LAX",]
    laxton$husbandry<-as.character(laxton$Study)
    laxton$husbandry[laxton$husbandry==1]<-17
    laxton$husbandry[laxton$husbandry==2]<-2
    high<-functionalAt[functionalAt$husbandry== "HIGH" ,]
    high$husbandry<-15

    par(mar=c(4,2,4,2), xpd=TRUE)
    plot(2:5, type='n', xlim = c(min-1, max+3), ylim=c(0,0.09),axes=F, xlab = "", ylab="")
    points(swarmy(centroids$centroid1*-1, rep(0.07,2)), col= col1, pch=c(pch1+15, pch1), cex=1.75)
    points(swarmy(laxton$LD1*-1, rep(0.048,2), side=1, compact=compact, priority = priority),col=col2, pch=as.numeric(laxton$husbandry),cex=1.2)
    points(swarmy(high$LD1*-1, rep(0.03,2), side=1, compact=compact, priority = priority),col=col2, pch=as.numeric(high$husbandry),cex=1.2)

    points(swarmy(x, rep(0.01, 2), side=1,compact=compact, priority=priority), col= col3, pch=pch3,cex=1.2)
    axis(1, ticks, cex=1.5)
    mtext(side =1, text = xlab, line=2, cex=0.8)
    if (lines== T) segments(min(centroids$centroid1*-1), 0.069,min(centroids$centroid1*-1),-0.0041 )
    if (lines== T) segments(max(centroids$centroid1*-1), 0.069,max(centroids$centroid1*-1),-0.0041 )
    if (lines== T)points(swarmy(centroids$centroid1*-1, rep(0.07,2)), col= col1, pch=c(pch1+15, pch1+20), cex=1.75, bg="white")
    if(legend==T){legend(max+0.5,0.06,c( "Laxton fields", "Laxton sykes"), pch=c(17,2), col=col2, cex=0.85, bty="n")
      legend(max+0.5,0.04,c( "Highgrove fields"), pch=c(15), col=col2, cex=0.85, bty="n")
      lname<-unique(site)
      legend(max+0.5,0.02,legend =site, pch=c(lpch), col= c(lcol), cex=0.85, bty="n")
      legend(max+0.5,0.075,c("Group 1 centroid"), pch=c(pch1+15), col= c(col1), cex=0.85, bty="n")
      legend(max+0.5,0.07, c("Group 2 centroid"), pch=c(pch1), col= c(col1), cex=0.85, bty="n")}
  }
  }


