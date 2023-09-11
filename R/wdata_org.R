
wdata_org<-function(dataframe,samples, codes, codename, model, fl_pr=NULL, sp_av=NULL, vg_pr=NULL){
  data<-dataframe[,samples:ncol(dataframe)]
  data[is.na(data)]<-0
  labels<-dataframe[, 1:samples]
  data[data >0]<-1
  samplenames<-row.names(t(data[1,]))
  trait_dataN<- trait_data[,c(5:10)]
  Codes<-as.data.frame(labels[,codes])
  names(Codes)<-codename
  data2<-cbind(Codes,data)
  if(is.null(sp_av)==TRUE) {species_lookup <- trait_dataN
  }else {species_lookup <- rbind(trait_dataN,sp_av)}
if (model==1){
  #SLA
  names <- colnames(data2)
  species_names <-  data2[,grepl(codename, names) | grepl("Codes", names) | grepl("Code", names)]#####
  matched_speciesSLA <- species_lookup[match(species_names, species_lookup$species.code), c("SLA")]
  matched_speciesSLA<-as.numeric(unlist(matched_speciesSLA))
  matched_speciesSLA<-as.data.frame(matched_speciesSLA)
  SLAdata<-cbind(data2,matched_speciesSLA)
  if(sum(is.na(SLAdata))>0){
    y<-which(is.na(SLAdata), arr.ind=TRUE)
    length<-length(y)
    x<-SLAdata[y[1:nrow(y)],1]
  warning(list(x) , "has returned an NA: it does not match the trait database's four_three code list. This/these species is/are excluded from the results")}
  data3<-data2[,2:ncol(data2)]
  matched_SLA<-as.data.frame(sapply(data3,function(data3) data3*matched_speciesSLA))
  SLApositive<-matched_SLA
  SLApositive[is.na(SLApositive)]<-0
  SLApositive[SLApositive >0]<-1
  SLAsumsample<-colSums(matched_SLA, na.rm=TRUE)
  SLANOOCC<-colSums(SLApositive, na.rm=FALSE)
  # ARNODE
  names <- colnames(data2)
  matched_speciesARNODE <- species_lookup[match(species_names, species_lookup$species.code), c("ARNODE")]
  matched_speciesARNODE<-as.numeric(unlist(matched_speciesARNODE))
  matched_speciesARNODE<-as.data.frame(matched_speciesARNODE)
  ARNODEdata<-cbind(data2,matched_speciesARNODE)
  data3<-data2[,2:ncol(data2)]
  matched_ARNODE <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesARNODE))
  ARNODEpositive<-matched_ARNODE
  ARNODEpositive[is.na(ARNODEpositive)]<-0
  ARNODEpositive[ARNODEpositive >0]<-1
  ARNODENOOCC<-colSums(ARNODEpositive, na.rm=FALSE)
  ARNODEsumsample<-colSums(matched_ARNODE, na.rm=TRUE)
  #LOGCANH
  names <- colnames(data2)
  matched_speciesLOGCANH <- species_lookup[match(species_names, species_lookup$species.code), c("LOGCANH")]
  matched_speciesLOGCANH <-as.numeric(unlist(matched_speciesLOGCANH))
  matched_speciesLOGCANH <-as.data.frame(matched_speciesLOGCANH)
  LOGCANHdata<-cbind(data2,matched_speciesLOGCANH)
  data3<-data2[,2:ncol(data2)]
  matched_LOGCANH <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesLOGCANH))
  LOGCANHsumsample<-colSums(matched_LOGCANH, na.rm=TRUE)
  LOGCANHpositive<-matched_LOGCANH
  LOGCANHpositive[is.na(LOGCANHpositive)]<-0
  LOGCANHpositive[LOGCANHpositive >0]<-1
  LOGCANHNOOCC<-colSums(LOGCANHpositive, na.rm=FALSE)
  # LOGCAND
  names <- colnames(data2)
  matched_speciesLOGCAND <- species_lookup[match(species_names, species_lookup$species.code), c("LOGCAND")]
  matched_speciesLOGCAND <-as.numeric(unlist(matched_speciesLOGCAND))
  matched_speciesLOGCAND <-as.data.frame(matched_speciesLOGCAND)
  LOGCANDdata<-cbind(data2,matched_speciesLOGCAND)
  data3<-data2[,2:ncol(data2)]
  matched_LOGCAND <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesLOGCAND))
  LOGCANDsumsample<-colSums(matched_LOGCAND, na.rm=TRUE)
  LOGCANDpositive<-matched_LOGCAND
  LOGCANDpositive[is.na(LOGCANDpositive)]<-0
  LOGCANDpositive[LOGCANDpositive >0]<-1
  LOGCANDNOOCC<-colSums(LOGCANDpositive, na.rm=FALSE)
  # FLOWPER
  names <- colnames(data2)
  species_lookup<-fl_pr
  colnames(species_lookup)<-c("species.code", "FLOWPER")
  matched_speciesFLOWPER <- species_lookup[match(species_names, species_lookup$species.code), c("FLOWPER")]
  matched_speciesFLOWPER <-as.numeric(unlist(matched_speciesFLOWPER))
  matched_speciesFLOWPER <-as.data.frame(matched_speciesFLOWPER)
  FLOWPERdata<-cbind(data2,matched_speciesFLOWPER)
  if(sum(is.na(FLOWPERdata))>0){
    y<-which(is.na(FLOWPERdata), arr.ind=TRUE)
    x<-FLOWPERdata[y[1:nrow(y)],1]
    warning(list(x)," has returned an NA : no flowering period data has been provided or it does not match the entered dataset. This/these species is/are excluded from the results")
      }
  data3<-data2[,2:ncol(data2)]
  matched_FLOWPER <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesFLOWPER))
  FLOWPERsumsample<-colSums(matched_FLOWPER, na.rm=TRUE)
  FLOWPERpositive<-matched_FLOWPER
  FLOWPERpositive[is.na(FLOWPERpositive)]<-0
  FLOWPERpositive[FLOWPERpositive >0]<-1
  FLOWPERNOOCC<-colSums(FLOWPERpositive, na.rm=FALSE)
  alldata<-(rbind(SLAsumsample,ARNODEsumsample,LOGCANHsumsample,LOGCANDsumsample,FLOWPERsumsample,SLANOOCC,ARNODENOOCC,LOGCANHNOOCC,LOGCANDNOOCC,FLOWPERNOOCC))
  df<-as.data.frame(t(alldata))
  df$SLA<-df$SLAsumsample/df$SLANOOCC
  df$ARNODE<-df$ARNODEsumsample/df$ARNODENOOCC
  df$LOGCANH<-df$LOGCANHsumsample/df$LOGCANHNOOCC
  df$LOGCAND<-df$LOGCANDsumsample/df$LOGCANDNOOCC
  df$FLOWPER<-df$FLOWPERsumsample/df$FLOWPERNOOCC
  row.names(df)<-samplenames
 finaldf<-df[,c("SLA", "ARNODE", "LOGCANH", "LOGCAND", "FLOWPER")]
  # assign("wdata",finaldf,envir = .GlobalEnv)
  print(finaldf)
  results<-finaldf
}
if (model==2){
  #SLA
  names <- colnames(data2)
  species_names <-  data2[,grepl(codename, names) | grepl("Codes", names) | grepl("Code", names)]#####
  matched_speciesSLA <- species_lookup[match(species_names, species_lookup$species.code), c("SLA")]
  matched_speciesSLA<-as.numeric(unlist(matched_speciesSLA))
  matched_speciesSLA<-as.data.frame(matched_speciesSLA)
  SLAdata<-cbind(data2,matched_speciesSLA)
  if(sum(is.na(SLAdata))>0){
    y<-which(is.na(SLAdata), arr.ind=TRUE)
    length<-length(y)
    x<-SLAdata[y[1:nrow(y)],1]
    warning(list(x) , "has returned an NA: it does not match the trait database's four_three code list. This/these species is/are excluded from the results")}
  data3<-data2[,2:ncol(data2)]
  matched_SLA<-as.data.frame(sapply(data3,function(data3) data3*matched_speciesSLA))
  SLApositive<-matched_SLA
  SLApositive[is.na(SLApositive)]<-0
  SLApositive[SLApositive >0]<-1
  SLAsumsample<-colSums(matched_SLA, na.rm=TRUE)
  SLANOOCC<-colSums(SLApositive, na.rm=FALSE)
  # ARNODE
  names <- colnames(data2)
  matched_speciesARNODE <- species_lookup[match(species_names, species_lookup$species.code), c("ARNODE")]
  matched_speciesARNODE<-as.numeric(unlist(matched_speciesARNODE))
  matched_speciesARNODE<-as.data.frame(matched_speciesARNODE)
  ARNODEdata<-cbind(data2,matched_speciesARNODE)
  data3<-data2[,2:ncol(data2)]
  matched_ARNODE <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesARNODE))
  ARNODEpositive<-matched_ARNODE
  ARNODEpositive[is.na(ARNODEpositive)]<-0
  ARNODEpositive[ARNODEpositive >0]<-1
  ARNODENOOCC<-colSums(ARNODEpositive, na.rm=FALSE)
  ARNODEsumsample<-colSums(matched_ARNODE, na.rm=TRUE)
  #LOGCANH
  names <- colnames(data2)
  matched_speciesLOGCANH <- species_lookup[match(species_names, species_lookup$species.code), c("LOGCANH")]
  matched_speciesLOGCANH <-as.numeric(unlist(matched_speciesLOGCANH))
  matched_speciesLOGCANH <-as.data.frame(matched_speciesLOGCANH)
  LOGCANHdata<-cbind(data2,matched_speciesLOGCANH)
  data3<-data2[,2:ncol(data2)]
  matched_LOGCANH <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesLOGCANH))
  LOGCANHsumsample<-colSums(matched_LOGCANH, na.rm=TRUE)
  LOGCANHpositive<-matched_LOGCANH
  LOGCANHpositive[is.na(LOGCANHpositive)]<-0
  LOGCANHpositive[LOGCANHpositive >0]<-1
  LOGCANHNOOCC<-colSums(LOGCANHpositive, na.rm=FALSE)
  # LOGCAND
  names <- colnames(data2)
  matched_speciesLOGCAND <- species_lookup[match(species_names, species_lookup$species.code), c("LOGCAND")]
  matched_speciesLOGCAND <-as.numeric(unlist(matched_speciesLOGCAND))
  matched_speciesLOGCAND <-as.data.frame(matched_speciesLOGCAND)
  LOGCANDdata<-cbind(data2,matched_speciesLOGCAND)
  data3<-data2[,2:ncol(data2)]
  matched_LOGCAND <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesLOGCAND))
  LOGCANDsumsample<-colSums(matched_LOGCAND, na.rm=TRUE)
  LOGCANDpositive<-matched_LOGCAND
  LOGCANDpositive[is.na(LOGCANDpositive)]<-0
  LOGCANDpositive[LOGCANDpositive >0]<-1
  LOGCANDNOOCC<-colSums(LOGCANDpositive, na.rm=FALSE)

    alldata<-(rbind(SLAsumsample,ARNODEsumsample,LOGCANHsumsample,LOGCANDsumsample,SLANOOCC,ARNODENOOCC,LOGCANHNOOCC,LOGCANDNOOCC))
    df<-as.data.frame(t(alldata))
    df$SLA<-df$SLAsumsample/df$SLANOOCC
    df$ARNODE<-df$ARNODEsumsample/df$ARNODENOOCC
    df$LOGCANH<-df$LOGCANHsumsample/df$LOGCANHNOOCC
    df$LOGCAND<-df$LOGCANDsumsample/df$LOGCANDNOOCC
    row.names(df)<-samplenames
    finaldf<-df[,c("SLA", "ARNODE", "LOGCANH", "LOGCAND")]
    # assign("data",finaldf,envir = .GlobalEnv)
    print(finaldf)
    results<-finaldf
}

  if (model==3){
    # VEGPROP
    if(is.null(vg_pr)==TRUE) {species_lookup <- species_lookup
    }else {
      vg_prn<-vg_pr[!is.na(vg_pr$LIFEHIST),]
      species_lookup<-rbind(vg_prn[,c(2:3)], sp_av[,c(1,6)])}
    trait_dataN<- trait_data[,c(5:10)]
    names <- colnames(data2)
    species_names <-  data2[,grepl(codename, names) | grepl("Codes", names) | grepl("Code", names)]#####
    matched_speciesVEGPROP <- species_lookup[match(species_names, species_lookup$species.code), c("VEGPROP")]
    matched_speciesVEGPROP <-as.numeric(unlist(matched_speciesVEGPROP))
    matched_speciesVEGPROP <-as.data.frame(matched_speciesVEGPROP)
    matched_speciesVEGPROP[matched_speciesVEGPROP==0]<-"2"
    matched_speciesVEGPROP[matched_speciesVEGPROP==1]<-"4"
    VEGPROPdata<-cbind(data2,matched_speciesVEGPROP)
    if(sum(is.na(VEGPROPdata))>0){
      y<-which(is.na(VEGPROPdata), arr.ind=TRUE)
      x<-VEGPROPdata[y[1:nrow(y)],1]
      warning(list(x)," has returned an NA: it does not match the trait database's four_three code list or are annual/biennal species. This/these species is/are excluded from the results")
    }
    data3<-data2[,2:ncol(data2)]
    matched_speciesVEGPROP <-as.numeric(unlist(matched_speciesVEGPROP))
    matched_speciesVEGPROP <-as.data.frame(matched_speciesVEGPROP)
    matched_VEGPROP <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesVEGPROP))
    matched_VEGPROPSUM<-matched_VEGPROP
    matched_VEGPROPSUM[matched_VEGPROPSUM==0]<-NA
    matched_VEGPROPSUM[matched_VEGPROPSUM==4]<-1
    matched_VEGPROPSUM[matched_VEGPROPSUM==2]<-0
    VEGPROPsumsample<-colSums(matched_VEGPROPSUM, na.rm=TRUE) #check
    VEGPROPpositiveNOOCC<-matched_VEGPROP
    #####calculations for NOOCC - which is total perennials
    VEGPROPpositiveNOOCC[VEGPROPpositiveNOOCC==0]<-NA
    VEGPROPpositiveNOOCC[VEGPROPpositiveNOOCC==2]<-1
    VEGPROPpositiveNOOCC[VEGPROPpositiveNOOCC==4]<-1
    VEGPROPNOOCC<-colSums(VEGPROPpositiveNOOCC, na.rm=TRUE)
    # FLOWPER
    names <- colnames(data2)
    species_lookup<-fl_pr
    colnames(species_lookup)<-c("species.code", "FLOWPER")
    matched_speciesFLOWPER <- species_lookup[match(species_names, species_lookup$species.code), c("FLOWPER")]
    matched_speciesFLOWPER <-as.numeric(unlist(matched_speciesFLOWPER))
    matched_speciesFLOWPER <-as.data.frame(matched_speciesFLOWPER)
    FLOWPERdata<-cbind(data2,matched_speciesFLOWPER)
    if(sum(is.na(FLOWPERdata))>0){
      y<-which(is.na(FLOWPERdata), arr.ind=TRUE)
      x<-FLOWPERdata[y[1:nrow(y)],1]
      warning(list(x)," has returned an NA : no flowering period data has been provided or it does not match the entered dataset. This/these species is/are excluded from the results")}
    data3<-data2[,2:ncol(data2)]
    matched_FLOWPER <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesFLOWPER))
    FLOWPERsumsample<-colSums(matched_FLOWPER, na.rm=TRUE)
    FLOWPERpositive<-matched_FLOWPER
    FLOWPERpositive[is.na(FLOWPERpositive)]<-0
    FLOWPERpositive[FLOWPERpositive >0]<-1
    FLOWPERNOOCC<-colSums(FLOWPERpositive, na.rm=FALSE)

   alldata<-(rbind(VEGPROPsumsample,FLOWPERsumsample,VEGPROPNOOCC, FLOWPERNOOCC))
  df<-as.data.frame(t(alldata))
  df$VEGPROP<-df$VEGPROPsumsample/df$VEGPROPNOOCC
  df$FLOWPER<-df$FLOWPERsumsample/df$FLOWPERNOOCC
  row.names(df)<-samplenames
  finaldf<-df[,c("FLOWPER", "VEGPROP")]
  # assign("data",finaldf,envir = .GlobalEnv)
  print(finaldf)
  results<-finaldf
}
results<-finaldf
  }
