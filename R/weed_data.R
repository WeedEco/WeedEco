weed_data<-function(x){#### remember x$x
  species_lookup <- data.frame(trait_data)
  SLA<- species_lookup[match(x, species_lookup$species.code), c("SLA")]
  SLA<-as.numeric(SLA)
  ARNODE<- species_lookup[match(x, species_lookup$species.code), c("ARNODE")]
  ARNODE<-as.numeric(ARNODE)
  LOGCANH<- species_lookup[match(x, species_lookup$species.code), c("SLA")]
  LOGCANH<-as.numeric(LOGCANH)
  LOGCAND<- species_lookup[match(x, species_lookup$species.code), c("SLA")]
  LOGCAND<-as.numeric(LOGCAND)
  VEGPRO<- species_lookup[match(x, species_lookup$species.code), c("SLA")]
  VEGPRO<-as.numeric(VEGPRO)
  wdata<-data.frame(x,SLA,ARNODE,LOGCANH,LOGCAND,VEGPRO)
}
