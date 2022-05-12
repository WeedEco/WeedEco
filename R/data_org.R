

data_org<-function(dataframe,samples, codes, model, x=NULL){
  data<-dataframe[,(samples+1):ncol(dataframe)]
  data[is.na(data)]<-0
  labels<-dataframe[, 1:samples]
  data[data >0]<-1
  NOOCC<-colSums(data)
  fibscodes<-labels[,codes]
  data2<-cbind(fibscodes,data)
  species_lookup <- FIBS_short

if (model==1){
  #SLA
  names <- colnames(data2)
  species_names <-  data2[,grepl("FIBS", names) | grepl("Codes", names) | grepl("fibs codes", names) | grepl("Taxon", names)]
  matched_speciesSLA <- species_lookup[match(species_names, species_lookup$`Fibs codes`), c("SLA")]
  SLAdata<-cbind(data2,matched_speciesSLA)
  data3<-data2[,2:ncol(data2)]
  matched_SLA <- sapply(data3,"*", matched_speciesSLA$SLA)
  SLAsumsample<-colSums(matched_SLA, na.rm=TRUE)
  # ARNODE
  names <- colnames(data2)
  species_names <-  data2[,grepl("FIBS", names) | grepl("Codes", names) | grepl("fibs codes", names) | grepl("Taxon", names)]
  matched_speciesARNODE <- species_lookup[match(species_names, species_lookup$`Fibs codes`), c("ARNODE")]
  ARNODEdata<-cbind(data2,matched_speciesARNODE)
  data3<-data2[,2:ncol(data2)]
  matched_ARNODE <- sapply(data3,"*", matched_speciesARNODE$ARNODE)
  ARNODEsumsample<-colSums(matched_ARNODE, na.rm=TRUE)
  #LOGCAHN
  names <- colnames(data2)
  matched_speciesLOGCAHN <- species_lookup[match(species_names, species_lookup$`Fibs codes`), c("CANHT")]
  LOGCAHNdata<-cbind(data2,matched_speciesLOGCAHN)
  data3<-data2[,2:ncol(data2)]
  matched_LOGCAHN <- sapply(data3,"*", matched_speciesLOGCAHN$CANHT)
  LOGCAHNsumsample<-colSums(matched_LOGCAHN, na.rm=TRUE)
  # LOGCADN
  names <- colnames(data2)
  matched_speciesLOGCADN <- species_lookup[match(species_names, species_lookup$`Fibs codes`), c("CAND")]
  LOGCADNdata<-cbind(data2,matched_speciesLOGCADN)
  data3<-data2[,2:ncol(data2)]
  matched_LOGCADN <- sapply(data3,"*", matched_speciesLOGCADN$CAND)
  LOGCADNsumsample<-colSums(matched_LOGCADN, na.rm=TRUE)
  # FLOWPER
  names <- colnames(data2)
  species_lookup<-x
  matched_speciesFLOWPER <- species_lookup[match(species_names, species_lookup$`Fibs codes`), c("FLOWPER")]
  FLOWPERdata<-cbind(data2,matched_speciesFLOWPER)
  data3<-data2[,2:ncol(data2)]
  matched_FLOWPER <- sapply(data3,"*", matched_speciesFLOWPER$FLOWPER)
  FLOWPERsumsample<-colSums(matched_FLOWPER, na.rm=TRUE)
  alldata<-(rbind(SLAsumsample,ARNODEsumsample,LOGCAHNsumsample,LOGCADNsumsample,FLOWPERsumsample,NOOCC))
  df<-as.data.frame(t(alldata[,-1]))
  df$SLA<-df$SLAsumsample/df$NOOCC
  df$ARNODE<-df$ARNODEsumsample/df$NOOCC
  df$LOGCAHN<-df$LOGCAHNsumsample/df$NOOCC
  df$LOGCADN<-df$LOGCADNsumsample/df$NOOCC
  df$FLOWPER<-df$FLOWPERsumsample/df$NOOCC
  finaldf<-df[,c("SLA", "ARNODE", "LOGCAHN", "LOGCADN", "FLOWPER")]
  print(finaldf)
}
if (model==2){
#SLA
    names <- colnames(data2)
    species_names <-  data2[,grepl("FIBS", names) | grepl("Codes", names) | grepl("checked codes", names)]
    matched_speciesSLA <- species_lookup[match(species_names, species_lookup$`Fibs codes`), c("SLA")]
    SLAdata<-cbind(data2,matched_speciesSLA)
    data3<-data2[,2:ncol(data2)]
    matched_SLA <- sapply(data3,"*", matched_speciesSLA$SLA)
    SLAsumsample<-colSums(matched_SLA, na.rm=TRUE)
# ARNODE
    names <- colnames(data2)
    matched_speciesARNODE <- species_lookup[match(species_names, species_lookup$`Fibs codes`), c("ARNODE")]
    ARNODEdata<-cbind(data2,matched_speciesARNODE)
    data3<-data2[,2:ncol(data2)]
    matched_ARNODE <- sapply(data3,"*", matched_speciesARNODE$ARNODE)
    ARNODEsumsample<-colSums(matched_ARNODE, na.rm=TRUE)
#LOGCAHN
    names <- colnames(data2)
    matched_speciesLOGCAHN <- species_lookup[match(species_names, species_lookup$`Fibs codes`), c("CANHT")]
    LOGCAHNdata<-cbind(data2,matched_speciesLOGCAHN)
    data3<-data2[,2:ncol(data2)]
    matched_LOGCAHN <- sapply(data3,"*", matched_speciesLOGCAHN$CANHT)
    LOGCAHNsumsample<-colSums(matched_LOGCAHN, na.rm=TRUE)
# LOGCADN
    names <- colnames(data2)
    matched_speciesLOGCADN <- species_lookup[match(species_names, species_lookup$`Fibs codes`), c("CAND")]
    LOGCADNdata<-cbind(data2,matched_speciesLOGCADN)
    data3<-data2[,2:ncol(data2)]
    matched_LOGCADN <- sapply(data3,"*", matched_speciesLOGCADN$CAND)
    LOGCADNsumsample<-colSums(matched_LOGCADN, na.rm=TRUE)
    alldata<-(rbind(SLAsumsample,ARNODEsumsample,LOGCAHNsumsample,LOGCADNsumsample,NOOCC))
    df<-as.data.frame(t(alldata)) #####need to change this in all below and above
    df$SLA<-df$SLAsumsample/df$NOOCC
    df$ARNODE<-df$ARNODEsumsample/df$NOOCC
    df$LOGCAHN<-df$LOGCAHNsumsample/df$NOOCC
    df$LOGCADN<-df$LOGCADNsumsample/df$NOOCC
    finaldf<-df[,c("SLA", "ARNODE", "LOGCAHN", "LOGCADN")]
    assign("data",finaldf,envir = .GlobalEnv)
    print(finaldf)
}

  if (model==3){
    # VEGPRO
    names <- colnames(data2)
    matched_speciesVEGPRO <- species_lookup[match(species_names, species_lookup$`Fibs codes`), c("VEGPROP")]
    VEGPROdata<-cbind(data2,matched_speciesVEGPRO)
    data3<-data2[,2:ncol(data2)]
    matched_VEGPRO <- sapply(data3,"*", matched_speciesVEGPRO$VEGPROP)
    VEGPROsumsample<-colSums(matched_VEGPRO, na.rm=TRUE)
  # FLOWPER
    names <- colnames(data2)
    species_lookup<-x
    matched_speciesFLOWPER <- species_lookup[match(species_names, species_lookup$`Fibs codes`), c("FLOWPER")]
    FLOWPERdata<-cbind(data2,matched_speciesFLOWPER)
    data3<-data2[,2:ncol(data2)]
    matched_FLOWPER <- sapply(data3,"*", matched_speciesFLOWPER$FLOWPER)
    FLOWPERsumsample<-colSums(matched_FLOWPER, na.rm=TRUE)
   alldata<-(rbind(VEGPROsumsample,FLOWPERsumsample,NOOCC))
  df<-as.data.frame(t(alldata[,-1]))

  df$VEGPROP<-df$VEGPROsumsample/df$NOOCC
  df$FLOWPER<-df$FLOWPERsumsample/df$NOOCC
  finaldf<-df[,c("FLOWPER", "VEGPROP")]
  print(finaldf)
  }

  }
