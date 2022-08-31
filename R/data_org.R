
data_org<-function(dataframe,samples, codes, model, x=NULL, sp_av){
  data<-dataframe[,(samples+1):ncol(dataframe)]
  data[is.na(data)]<-0
  labels<-dataframe[, 1:samples]
  data[data >0]<-1
  samplenames<-row.names(t(data[1,]))
  NOOCC<-colSums(data)
  fibscodes<-labels[,codes]

  data2<-cbind(fibscodes,data)
  trait_data<-trait_data[,c(2,3,11,7,8,9)]
  species_lookup <- rbind(trait_data,sp_av)

if (model==1){
  #SLA
  names <- colnames(data2)
  species_names <-  data2[,grepl("FIBS", names) | grepl("Codes", names) | grepl("Code", names)]#####
  matched_speciesSLA <- species_lookup[match(species_names, species_lookup$species.code), c("SLA")]
  matched_speciesSLA<-as.numeric(unlist(matched_speciesSLA))
  matched_speciesSLA<-as.data.frame(matched_speciesSLA)
  SLAdata<-cbind(data2,matched_speciesSLA)
  data3<-data2[,2:ncol(data2)]
  matched_SLA<-as.data.frame(sapply(data3,function(data3) data3*matched_speciesSLA))
  SLAsumsample<-colSums(matched_SLA, na.rm=TRUE)
  # ARNODE
  names <- colnames(data2)
  matched_speciesARNODE <- species_lookup[match(species_names, species_lookup$species.code), c("ARNODE")]
  matched_speciesARNODE<-as.numeric(unlist(matched_speciesARNODE))
  matched_speciesARNODE<-as.data.frame(matched_speciesARNODE)
  ARNODEdata<-cbind(data2,matched_speciesARNODE)
  data3<-data2[,2:ncol(data2)]
  matched_ARNODE <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesARNODE))
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
  # LOGCAND
  names <- colnames(data2)
  matched_speciesLOGCAND <- species_lookup[match(species_names, species_lookup$species.code), c("LOGCAND")]
  matched_speciesLOGCAND <-as.numeric(unlist(matched_speciesLOGCAND))
  matched_speciesLOGCAND <-as.data.frame(matched_speciesLOGCAND)
  LOGCANDdata<-cbind(data2,matched_speciesLOGCAND)
  data3<-data2[,2:ncol(data2)]
  matched_LOGCAND <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesLOGCAND))
  LOGCANDsumsample<-colSums(matched_LOGCAND, na.rm=TRUE)
  # FLOWPER
  names <- colnames(data2)
  species_lookup<-x
  matched_speciesFLOWPER <- species_lookup[match(species_names, species_lookup$species.code), c("FLOWPER")]
  matched_speciesFLOWPER <-as.numeric(unlist(matched_speciesFLOWPER))
  matched_speciesFLOWPER <-as.data.frame(matched_speciesFLOWPER)
  FLOWPERdata<-cbind(data2,matched_speciesFLOWPER)
  data3<-data2[,2:ncol(data2)]
  matched_FLOWPER <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesFLOWPER))
  FLOWPERsumsample<-colSums(matched_FLOWPER, na.rm=TRUE)
  alldata<-(rbind(SLAsumsample,ARNODEsumsample,LOGCANHsumsample,LOGCANDsumsample,FLOWPERsumsample,NOOCC))
  df<-as.data.frame(t(alldata))
  df$SLA<-df$SLAsumsample/df$NOOCC
  df$ARNODE<-df$ARNODEsumsample/df$NOOCC
  df$LOGCANH<-df$LOGCANHsumsample/df$NOOCC
  df$LOGCAND<-df$LOGCANDsumsample/df$NOOCC
  df$FLOWPER<-df$FLOWPERsumsample/df$NOOCC
  row.names(df)<-samplenames
 finaldf<-df[,c("SLA", "ARNODE", "LOGCANH", "LOGCAND", "FLOWPER")]
  assign("wdata",finaldf,envir = .GlobalEnv)
  print(finaldf)
}
if (model==2){
#SLA
    names <- colnames(data2)
    species_names <-  data2[,grepl("FIBS", names) | grepl("Codes", names) | grepl("checked codes", names)]
    matched_speciesSLA <- species_lookup[match(species_names, species_lookup$species.code), c("SLA")]
    matched_speciesSLA<-as.numeric(matched_speciesSLA)
    matched_speciesSLA<-as.data.frame(matched_speciesSLA)
    SLAdata<-cbind(data2,matched_speciesSLA)
    data3<-data2[,2:ncol(data2)]
    matched_SLA<-as.data.frame(sapply(data3,function(data3) data3*matched_speciesSLA))
    SLAsumsample<-colSums(matched_SLA, na.rm=TRUE)
# ARNODE
    names <- colnames(data2)
    matched_speciesARNODE <- species_lookup[match(species_names, species_lookup$species.code), c("ARNODE")]
    matched_speciesARNODE<-as.numeric(matched_speciesARNODE)
    matched_speciesARNODE<-as.data.frame(matched_speciesARNODE)
    ARNODEdata<-cbind(data2,matched_speciesARNODE)
    data3<-data2[,2:ncol(data2)]
    matched_ARNODE <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesARNODE))
    ARNODEsumsample<-colSums(matched_ARNODE, na.rm=TRUE)
#LOGCANH
    names <- colnames(data2)
    matched_speciesLOGCANH <- species_lookup[match(species_names, species_lookup$species.code), c("LOGCANH")]
    matched_speciesLOGCANH <-as.numeric(matched_speciesLOGCANH)
    matched_speciesLOGCANH <-as.data.frame(matched_speciesLOGCANH)
    LOGCANHdata<-cbind(data2,matched_speciesLOGCANH)
    data3<-data2[,2:ncol(data2)]
    matched_LOGCANH <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesLOGCANH))
    LOGCANHsumsample<-colSums(matched_LOGCANH, na.rm=TRUE)
# LOGCAND
    names <- colnames(data2)
    matched_speciesLOGCAND <- species_lookup[match(species_names, species_lookup$species.code), c("LOGCAND")]
    matched_speciesLOGCAND <-as.numeric(matched_speciesLOGCAND)
    matched_speciesLOGCAND <-as.data.frame(matched_speciesLOGCAND)
    LOGCANDdata<-cbind(data2,matched_speciesLOGCAND)
    data3<-data2[,2:ncol(data2)]
    matched_LOGCAND <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesLOGCAND))
    LOGCANDsumsample<-colSums(matched_LOGCAND, na.rm=TRUE)
    alldata<-(rbind(SLAsumsample,ARNODEsumsample,LOGCANHsumsample,LOGCANDsumsample,NOOCC))
    df<-as.data.frame(t(alldata)) #####need to change this in all below and above
    df$SLA<-df$SLAsumsample/df$NOOCC
    df$ARNODE<-df$ARNODEsumsample/df$NOOCC
    df$LOGCANH<-df$LOGCANHsumsample/df$NOOCC
    df$LOGCAND<-df$LOGCANDsumsample/df$NOOCC
    row.names(df)<-samplenames
    finaldf<-df[,c("SLA", "ARNODE", "LOGCANH", "LOGCAND")]
    assign("data",finaldf,envir = .GlobalEnv)
    print(finaldf)
}

  if (model==3){
    # VEGPRO
    names <- colnames(data2)
    matched_speciesVEGPRO <- species_lookup[match(species_names, species_lookup$`species codes`), c("VEGPROP")]
    VEGPROdata<-cbind(data2,matched_speciesVEGPRO)
    matched_speciesFLOWPER <-as.numeric(matched_speciesVEGPRO)
    matched_speciesFLOWPER <-as.data.frame(matched_speciesVEGPRO)
    data3<-data2[,2:ncol(data2)]
    matched_VEGPRO <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesVEGPRO))
    VEGPROsumsample<-colSums(matched_VEGPRO, na.rm=TRUE)
  # FLOWPER
    names <- colnames(data2)
    species_lookup<-x
    matched_speciesFLOWPER <- species_lookup[match(species_names, species_lookup$`species codes`), c("FLOWPER")]
    FLOWPERdata<-cbind(data2,matched_speciesFLOWPER)
    matched_speciesFLOWPER <-as.numeric(matched_speciesFLOWPER)
    matched_speciesFLOWPER <-as.data.frame(matched_speciesFLOWPER)
    data3<-data2[,2:ncol(data2)]
    matched_FLOWPER <- as.data.frame(sapply(data3,function(data3) data3*matched_speciesFLOWPER))
    FLOWPERsumsample<-colSums(matched_FLOWPER, na.rm=TRUE)
   alldata<-(rbind(VEGPROsumsample,FLOWPERsumsample,NOOCC))
  df<-as.data.frame(t(alldata))
  df$VEGPROP<-df$VEGPROsumsample/df$NOOCC
  df$FLOWPER<-df$FLOWPERsumsample/df$NOOCC
  row.names(df)<-samplenames
  finaldf<-df[,c("FLOWPER", "VEGPROP")]
  assign("data",finaldf,envir = .GlobalEnv)
  print(finaldf)
  }

  }
