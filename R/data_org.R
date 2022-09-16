
data_org<-function(dataframe,samples, codes, codename, model, x=NULL, sp_av=NULL){
  data<-dataframe[,samples:ncol(dataframe)]
  data[is.na(data)]<-0
  labels<-dataframe[, 1:samples]
  data[data >0]<-1
  samplenames<-row.names(t(data[1,]))
  NOOCC<-colSums(data) # I don't thinks this is correct.
  fibscodes<-labels[,codes]
  data2<-cbind(fibscodes,data)
  #trait_data<-trait_data1[,c(2,4:8)]
  #trait_data<-traitdata_2[,c(1:5,7)]
  trait_data<-trait_data[,c(2:3,7:9,11)]
  if(is.null(sp_av)==TRUE) {species_lookup <- trait_data
  }else {species_lookup <- rbind(trait_data,sp_av)}
if (model==1){
  #SLA
  names <- colnames(data2)
  species_names <-  data2[,grepl(codename, names) | grepl("Codes", names) | grepl("Code", names)]#####
  matched_speciesSLA <- species_lookup[match(species_names, species_lookup$species.code), c("SLA")]
  matched_speciesSLA<-as.numeric(unlist(matched_speciesSLA))
  matched_speciesSLA<-as.data.frame(matched_speciesSLA)
  SLAdata<-cbind(data2,matched_speciesSLA)
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
  species_lookup<-x
  colnames(species_lookup)<-c("species.code", "FLOWPER")
  matched_speciesFLOWPER <- species_lookup[match(species_names, species_lookup$species.code), c("FLOWPER")]
  matched_speciesFLOWPER <-as.numeric(unlist(matched_speciesFLOWPER))
  matched_speciesFLOWPER <-as.data.frame(matched_speciesFLOWPER)
  FLOWPERdata<-cbind(data2,matched_speciesFLOWPER)
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
    # VEGPRO
    names <- colnames(data2)
    matched_speciesVEGPRO <- species_lookup[match(species_names, species_lookup$`species codes`), c("VEGPRO")]
    VEGPROdata<-cbind(data2,matched_speciesVEGPRO)
    matched_speciesFLOWPER <-as.numeric(unlist(matched_speciesVEGPRO))
    matched_speciesFLOWPER <-as.data.frame(matched_speciesVEGPRO)
    data3<-data2[,2:ncol(data2)]
    matched_VEGPRO <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesVEGPRO))
    VEGPROsumsample<-colSums(matched_VEGPRO, na.rm=TRUE)
    VEGPROpositive<-matched_VEGPRO
    VEGPROpositive[VEGPROpositive == "ann"]<-0
    VEGPROpositive[is.na(VEGPROpositive)]<-0
    VEGPROpositive[VEGPROpositive >0]<-1
    VEGPRONOOCC<-colSums(VEGPROpositive, na.rm=FALSE)

  # FLOWPER
    names <- colnames(data2)
    species_lookup<-x
    colnames(species_lookup)<-c("species.code", "FLOWPER")
    matched_speciesFLOWPER <- species_lookup[match(species_names, species_lookup$species.code), c("FLOWPER")]
    matched_speciesFLOWPER <-as.numeric(unlist(matched_speciesFLOWPER))
    matched_speciesFLOWPER <-as.data.frame(matched_speciesFLOWPER)
    FLOWPERdata<-cbind(data2,matched_speciesFLOWPER)
    data3<-data2[,2:ncol(data2)]
    matched_FLOWPER <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesFLOWPER))
    FLOWPERsumsample<-colSums(matched_FLOWPER, na.rm=TRUE)
    FLOWPERpositive<-matched_FLOWPER
    FLOWPERpositive[is.na(FLOWPERpositive)]<-0
    FLOWPERpositive[FLOWPERpositive >0]<-1
    FLOWPERNOOCC<-colSums(FLOWPERpositive, na.rm=FALSE)

   alldata<-(rbind(VEGPROsumsample,FLOWPERsumsample,VEGPRONOOCC, FLOWPERNOOCC))
  df<-as.data.frame(t(alldata))
  df$VEGPROP<-df$VEGPROsumsample/df$VEGPRONOOCC
  df$FLOWPER<-df$FLOWPERsumsample/df$FLOWPERNOOCC
  row.names(df)<-samplenames
  finaldf<-df[,c("FLOWPER", "VEGPROP")]
  # assign("data",finaldf,envir = .GlobalEnv)
  print(finaldf)
  results<-finaldf
}
results<-finaldf
  }
