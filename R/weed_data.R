weed_data<-function(x,trait="all"){#### remember x$x
  species_lookup <- data.frame(trait_data)

  if(trait=='all'|trait== "ALL") {
  SLA<- species_lookup[match(x, species_lookup$species.code), c("SLA")]
  SLA<-as.numeric(SLA)
  ARNODE<- species_lookup[match(x, species_lookup$species.code), c("ARNODE")]
  ARNODE<-as.numeric(ARNODE)
  LOGCANH<- species_lookup[match(x, species_lookup$species.code), c("LOGCANH")]
  LOGCANH<-as.numeric(LOGCANH)
  LOGCAND<- species_lookup[match(x, species_lookup$species.code), c("LOGCAND")]
  LOGCAND<-as.numeric(LOGCAND)
  VEGPROP<- species_lookup[match(x, species_lookup$species.code), c("VEGPROP")]
  LIFEHIST<-species_lookup[match(x, species_lookup$species.code), c("LIFEHIST")]
  wdata<-data.frame(x,SLA,ARNODE,LOGCANH,LOGCAND,VEGPROP, LIFEHIST)
  names(wdata)<-c("species.code", "SLA", "ARNODE", "LOGCANH", "LOGCAND", "VEGPROP", "LIFEHIST")
  wdata<-wdata[!is.na(wdata$LIFEHIST), ]
  }
  if(trait=='vegprop'|trait== "VEGPROP") {
    VEGPROP<- species_lookup[match(x, species_lookup$species.code), c("VEGPROP")]
    LIFEHIST<-species_lookup[match(x, species_lookup$species.code), c("LIFEHIST")]
    wdata<-data.frame(x,VEGPROP, LIFEHIST)
    names(wdata)<-c("species.code","VEGPROP","LIFEHIST")
    wdata<-wdata[!is.na(wdata$LIFEHIST), ]
    }
  if(trait=='sla'|trait== "SLA") {
    SLA<- species_lookup[match(x, species_lookup$species.code), c("SLA")]
    SLA<-as.numeric(SLA)
    wdata<-data.frame(x,SLA)
    names(wdata)<-c("species.code","SLA")
    wdata<-wdata[!is.na(wdata$SLA), ]
    }
  if(trait=='ARNODE'|trait== "arnode") {
    ARNODE<- species_lookup[match(x, species_lookup$species.code), c("ARNODE")]
    ARNODE<-as.numeric(ARNODE)
    wdata<-data.frame(x,ARNODE)
    names(wdata)<-c("species.code","ARNODE")
    wdata<-wdata[!is.na(wdata$ARNODE), ]
    }
  if(trait=='logcanh'|trait== "LOGCANH") {
    LOGCANH<- species_lookup[match(x, species_lookup$species.code), c("LOGCANH")]
    LOGCANH<-as.numeric(LOGCANH)
    wdata<-data.frame(x,LOGCANH)
    names(wdata)<-c("species.code","LOGCANH")
    wdata<-wdata[!is.na(wdata$LOGCANH), ]
    }
  if(trait=='logcand'|trait== "LOGCAND") {
    LOGCAND<- species_lookup[match(x, species_lookup$species.code), c("LOGCAND")]
    LOGCAND<-as.numeric(LOGCAND)
    wdata<-data.frame(x,LOGCAND)
    names(wdata)<-c("species.code","LOGCAND")
    wdata<-wdata[!is.na(wdata$LOGCAND), ]
    }
  print(wdata)
  results<- wdata

}
