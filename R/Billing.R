getBilling<-function(startDate,endDate,bqProject,bqDataset,bqTable,bqBilling){
  require(bigrquery)
  require(dbplyr)
  require(magrittr)
  require(dplyr)
  con <- DBI::dbConnect(
       bigquery(),
       project = bqProject,
       dataset = bqDataset,
       billing = bqBilling
     )
  out = con%>%tbl(bqTable)%>%
    filter(usage_start_time>=startDate&usage_end_time<endDate)%>%
    collect()
  return(out)
}

checkTerraSub<-function(item,terraSubID){
  if("key"%in%names(item)){
		if(any(item$key=="terra-submission-id" & item$value==terraSubID)){
      ind=which(item$key=="terra-submission-id" & item$value==terraSubID)
			return(TRUE)
		}else{return(FALSE)}
  }else{return(FALSE)}
}

getTerraSubsetByTID<-function(mybilling,myterraSubID){
  res=lapply(mybilling$labels,function(x){checkTerraSub(x,tid)})
  unlist(res)
}

getTerraRam<-function(mybilling,myterraSubID){
  temp=getTerraSubsetByTID(mybilling,myterraSubID)
  temp2=do.call("rbind.data.frame",mybilling$sku)
  temp3=temp2[,2]=="Custom Instance Ram running in Americas"
  return(temp&temp3)
}
