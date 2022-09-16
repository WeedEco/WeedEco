ave_wdata<-function(newname, species1, species2, species3=NULL, species4=NULL, species5=NULL,species6=NULL,species7=NULL, species8=NULL){
  species<-c(species1,species2,species3, species4, species5, species6, species7, species8)
  # species_lookup <- data.frame(trait_data1)
  #species_lookup <- data.frame(traitdata_2)
  species_lookup <- data.frame(trait_data)
  SLA<-species_lookup[match(species, species_lookup$species.code), c("SLA")]
  SLA<-as.numeric(SLA)
  ARNODE<- species_lookup[match(species, species_lookup$species.code), c("ARNODE")]
  ARNODE<-as.numeric(ARNODE)
  LOGCANH<- species_lookup[match(species, species_lookup$species.code), c("LOGCANH")]
  LOGCANH<-as.numeric(LOGCANH)
  LOGCAND<- species_lookup[match(species, species_lookup$species.code), c("LOGCAND")]
  LOGCAND<-as.numeric(LOGCAND)
  VEGPRO<- species_lookup[match(species, species_lookup$species.code), c("VEGPRO")]




  df_new<-data.frame(species,SLA,ARNODE,LOGCANH,LOGCAND) #,VEGPRO)
  SLA<-mean(df_new$SLA)
  ARNODE<-mean(df_new$ARNODE)
  LOGCANH<-mean(df_new$LOGCANH)
  LOGCAND<-mean(df_new$LOGCAND)
  VEGPRO<-0
  species.code<-newname
  sp_av<-data.frame(species.code, SLA,ARNODE, LOGCANH, LOGCAND, VEGPRO)
}
