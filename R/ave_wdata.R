ave_wdata<-function(newname, species1, species2, species3=NULL, species4=NULL){
  species<-c(species1,species2,species3, species4)
  species_lookup <- data.frame(trait_data)
  SLA<-species_lookup[match(species, species_lookup$species.code), c("SLA")]
  SLA<-as.numeric(SLA)
  ARNODE<- species_lookup[match(species, species_lookup$species.code), c("ARNODE")]
  ARNODE<-as.numeric(ARNODE)
  LOGCANH<- species_lookup[match(species, species_lookup$species.code), c("LOGCANH")]
  LOGCANH<-as.numeric(LOGCANH)
  LOGCAND<- species_lookup[match(species, species_lookup$species.code), c("LOGCAND")]
  LOGCAND<-as.numeric(LOGCAND)
  VEGPROP<- species_lookup[match(species, species_lookup$species.code), c("VEGPROP")]
  VEGPROP <-as.numeric(VEGPROP)

  df_new<-data.frame(species,SLA,ARNODE,LOGCANH,LOGCAND,VEGPROP)
  SLA<-mean(df_new$SLA)
  ARNODE<-mean(df_new$ARNODE)
  LOGCANH<-mean(df_new$LOGCANH)
  LOGCAND<-mean(df_new$LOGCAND)
  VEGPROP<-mean(df_new$VEGPROP)## can't suppress the mean warning to do with NA's
  species.code<-newname
  sp_av<-data.frame(species.code, SLA,ARNODE, LOGCANH, LOGCAND, VEGPROP)
  sp_av$VEGPROP[sp_av$VEGPROP < 1]<-0
  results<-sp_av
}

