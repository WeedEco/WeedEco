weed_data<-function(x,trait="all"){#### remember x$x
  species_lookup <- data.frame(trait_data)
if (is.character(x)){
  if(trait=='all'|trait== "ALL") {
  FlEurNo<-species_lookup[match(x, species_lookup$species.code), c("FlEurNo")]
  SLA<- species_lookup[match(x, species_lookup$species.code), c("SLA")]
  SLA<-as.numeric(SLA)
  ARNODE<- species_lookup[match(x,species_lookup$species.code), c("ARNODE")]
  ARNODE<-as.numeric(ARNODE)
  LOGCANH<- species_lookup[match(x, species_lookup$species.code), c("LOGCANH")]
  LOGCANH<-as.numeric(LOGCANH)
  LOGCAND<- species_lookup[match(x, species_lookup$species.code), c("LOGCAND")]
  LOGCAND<-as.numeric(LOGCAND)
  VEGPROP<- species_lookup[match(x, species_lookup$species.code), c("VEGPROP")]
  LIFEHIST<-species_lookup[match(x, species_lookup$species.code), c("LIFEHIST")]
  wdata<-data.frame(FlEurNo,x,SLA,ARNODE,LOGCANH,LOGCAND,VEGPROP, LIFEHIST)
  names(wdata)<-c("FlEurNo","species.code", "SLA", "ARNODE", "LOGCANH", "LOGCAND", "VEGPROP", "LIFEHIST")
  wdata<-wdata[!is.na(wdata$LIFEHIST), ]
  }
  if(trait=='vegprop'|trait== "VEGPROP") {
    FlEurNo<-species_lookup[match(x, species_lookup$species.code), c("FlEurNo")]
    VEGPROP<- species_lookup[match(x, species_lookup$species.code), c("VEGPROP")]
    LIFEHIST<-species_lookup[match(x, species_lookup$species.code), c("LIFEHIST")]
    wdata<-data.frame(FlEurNo,x,VEGPROP, LIFEHIST)
    names(wdata)<-c("FlEurNo","species.code","VEGPROP","LIFEHIST")
    wdata<-wdata[!is.na(wdata$LIFEHIST), ]
    }
  if(trait=='sla'|trait== "SLA") {
    FlEurNo<-species_lookup[match(x, species_lookup$species.code), c("FlEurNo")]
    SLA<- species_lookup[match(x, species_lookup$species.code), c("SLA")]
    SLA<-as.numeric(SLA)
    wdata<-data.frame(FlEurNo,x,SLA)
    names(wdata)<-c("FlEurNo","species.code","SLA")
    wdata<-wdata[!is.na(wdata$SLA), ]
    }
  if(trait=='ARNODE'|trait== "arnode") {
    FlEurNo<-species_lookup[match(x, species_lookup$species.code), c("FlEurNo")]
    ARNODE<- species_lookup[match(x, species_lookup$species.code), c("ARNODE")]
    ARNODE<-as.numeric(ARNODE)
    wdata<-data.frame(FlEurNo,x,ARNODE)
    names(wdata)<-c("FlEurNo","species.code","ARNODE")
    wdata<-wdata[!is.na(wdata$ARNODE), ]
    }
  if(trait=='logcanh'|trait== "LOGCANH") {
    FlEurNo<-species_lookup[match(x, species_lookup$species.code), c("FlEurNo")]
    LOGCANH<- species_lookup[match(x, species_lookup$species.code), c("LOGCANH")]
    LOGCANH<-as.numeric(LOGCANH)
    wdata<-data.frame(FlEurNo,x,LOGCANH)
    names(wdata)<-c("FlEurNo","species.code","LOGCANH")
    wdata<-wdata[!is.na(wdata$LOGCANH), ]
    }
  if(trait=='logcand'|trait== "LOGCAND") {
    FlEurNo<-species_lookup[match(x, species_lookup$species.code), c("FlEurNo")]
    LOGCAND<- species_lookup[match(x, species_lookup$species.code), c("LOGCAND")]
    LOGCAND<-as.numeric(LOGCAND)
    wdata<-data.frame(FlEurNo,x,LOGCAND)
    names(wdata)<-c("FlEurNo","species.code","LOGCAND")
    wdata<-wdata[!is.na(wdata$LOGCAND), ]
    }}
if (is.numeric(x)){
  if(trait=='all'|trait== "ALL") {
      species.code<- species_lookup[match(x, species_lookup$FlEurNo), c("species.code")]
      SLA<- species_lookup[match(x, species_lookup$FlEurNo), c("SLA")]
      SLA<-as.numeric(SLA)
      ARNODE<- species_lookup[match(x,species_lookup$FlEurNo), c("ARNODE")]
      ARNODE<-as.numeric(ARNODE)
      LOGCANH<- species_lookup[match(x, species_lookup$FlEurNo), c("LOGCANH")]
      LOGCANH<-as.numeric(LOGCANH)
      LOGCAND<- species_lookup[match(x, species_lookup$FlEurNo), c("LOGCAND")]
      LOGCAND<-as.numeric(LOGCAND)
      VEGPROP<- species_lookup[match(x, species_lookup$FlEurNo), c("VEGPROP")]
      LIFEHIST<-species_lookup[match(x, species_lookup$FlEurNo), c("LIFEHIST")]
      wdata<-data.frame(x,species.code,SLA,ARNODE,LOGCANH,LOGCAND,VEGPROP, LIFEHIST)
      names(wdata)<-c("FlEurNo","species.code", "SLA", "ARNODE", "LOGCANH", "LOGCAND", "VEGPROP", "LIFEHIST")
      wdata<-wdata[!is.na(wdata$LIFEHIST), ]
    }
  if(trait=='vegprop'|trait== "VEGPROP") {
    species.code<- species_lookup[match(x, species_lookup$FlEurNo), c("species.code")]
    VEGPROP<- species_lookup[match(x, species_lookup$FlEurNo), c("VEGPROP")]
    LIFEHIST<-species_lookup[match(x, species_lookup$FlEurNo), c("LIFEHIST")]
    wdata<-data.frame(x,species.code,VEGPROP, LIFEHIST)
    names(wdata)<-c("FlEurNo","species.code","VEGPROP","LIFEHIST")
    wdata<-wdata[!is.na(wdata$LIFEHIST), ]
  }
  if(trait=='sla'|trait== "SLA") {
    species.code<- species_lookup[match(x, species_lookup$FlEurNo), c("species.code")]
    SLA<- species_lookup[match(x, species_lookup$FlEurNo), c("SLA")]
    SLA<-as.numeric(SLA)
    wdata<-data.frame(x,species.code,SLA)
    names(wdata)<-c("FlEurNo","species.code","SLA")
    wdata<-wdata[!is.na(wdata$SLA), ]
  }
  if(trait=='ARNODE'|trait== "arnode") {
    species.code<- species_lookup[match(x, species_lookup$FlEurNo), c("species.code")]
    ARNODE<- species_lookup[match(x, species_lookup$FlEurNo), c("ARNODE")]
    ARNODE<-as.numeric(ARNODE)
    wdata<-data.frame(x,species.code,ARNODE)
    names(wdata)<-c("FlEurNo","species.code","ARNODE")
    wdata<-wdata[!is.na(wdata$ARNODE), ]
  }
  if(trait=='logcanh'|trait== "LOGCANH") {
    species.code<- species_lookup[match(x, species_lookup$FlEurNo), c("species.code")]
    LOGCANH<- species_lookup[match(x, species_lookup$FlEurNo), c("LOGCANH")]
    LOGCANH<-as.numeric(LOGCANH)
    wdata<-data.frame(x,species.code,LOGCANH)
    names(wdata)<-c("FlEurNo","species.code","LOGCANH")
    wdata<-wdata[!is.na(wdata$LOGCANH), ]
  }
  if(trait=='logcand'|trait== "LOGCAND") {
    species.code<- species_lookup[match(x, species_lookup$FlEurNo), c("species.code")]
    LOGCAND<- species_lookup[match(x, species_lookup$FlEurNo), c("LOGCAND")]
    LOGCAND<-as.numeric(LOGCAND)
    wdata<-data.frame(x,species.code,LOGCAND)
    names(wdata)<-c("FlEurNo","species.code","LOGCAND")
    wdata<-wdata[!is.na(wdata$LOGCAND), ]
  }}
  print(wdata)
  results<- wdata

}
