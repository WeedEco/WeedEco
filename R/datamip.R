data_org<-function(dataframe,samples=6 ){
  data<-dataframe[,(samples+1):ncol(dataframe)]
  labels<-dataframe[, 1:samples]
  data[data >0]<-1
  data$NOOCC<-colSums(data)
  data2<-cbind(fibscodes,data)

 #  df<-as.data.frame(t(data[,-1]))
fibscodes<-labels[,3]
 # colnames(df)<-fibscodes
# df$NOOCC<-rowSums(df)
comp<-FIBS_short
SLA<-as.data.frame(cbind(comp$...1,comp$SLA))




for (i in 1:nrow(data2)){
  if(data2$fibscodes==SLA$V1) {
    data2<-i*SLA$V1}
}
new.list<-matrix(nrow = length(data2$NOOCO),ncol =length(fibscodes))
 for (i in 1:nrow(FIBS_short)){
   new.list[data2$SLA==comp[i,SLA]] <- (data2*i)
    }
}

ARNODE<-
CANHT<-
CAND<-
FLOWPER<-
VEGPROP<-

}

standard_species <-
  function(df){
    names <- colnames(data2)
    species_names <-  data2[,grepl("FIBS", names) | grepl("Codes", names) | grepl("fibscodes", names) | grepl("Taxon", names)]
    species_lookup <- FIBS_short
    matched_speciesSLA <- species_lookup[match(species_names, species_lookup$...1), 2]
    SLA<-cbind(data2,matched_speciesSLA)
    data3<-data2[,2:ncol(data2)]
    matched_SLA <- sapply(data3,"*", matched_speciesSLA$SLA)
    standard <- cbind(df, matched_plot_params)
    data.frame(standard[, 2:ncol(standard)], row.names=1:nrow(standard))
  }






standard.species <- function(species.vector, barleymerge=T){
  comp <- read.csv("species.compkynos.csv")
  new.list <- matrix(nrow=length(species.vector), ncol=11)
  for (k in 1:10){
    for (i in 1:nrow(comp)){
      for (j in 10:ncol(comp)){
        new.list[species.vector==paste(comp[i, j]) | species.vector==paste(comp[i, j]) | species.vector==paste(comp[i, j]) | species.vector==paste(comp[i, j]),k] <- paste(comp[i, k])
      }
    }
  }

  result <- data.frame(new.list)
  names(result) <- names(comp)[1:10]
  return(result)
}
