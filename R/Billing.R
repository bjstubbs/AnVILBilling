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


getKeys<-function(mybilling){
  temp=mybilling$labels
  getKeyTemp<-function(item){
    if(length(item)>0){
	     return(item$key)
     }else{return(NA)}
  }
  res=unique(unlist(lapply(temp,getKeyTemp)))
  res[!is.na(res)]

}

getValues<-function(mybilling,mykey){
  temp=mybilling$labels
  checkKey<-function(item){
    if(length(item)>0){
		if(any(item$key==mykey)){
      ind=which(item$key==mykey)
			return(item[ind,2])
    }else{return(NA)}
  }else{return(NA)}
  }
  res=unique(unlist(lapply(temp, checkKey)))
  res[!is.na(res)]
}

subsetByKeyValue<-function(mybilling, mykey, myvalue){
  temp=mybilling$labels
  checkKeyVal<-function(item){
    if(length(item)>0){
		if(any(item$key==mykey&item$value==myvalue)){
			return(TRUE)
    }else{return(FALSE)}
  }else{return(FALSE)}
  }
  keep=sapply(temp, checkKeyVal)
  mybilling[keep,]
}

getSkus<-function(mybilling){
  unique(unlist(lapply(mybilling$sku,function(x){x$description})))
}

subsetBySku<-function(mybilling,mysku){
  temp=mybilling$sku
  keep=sapply(temp,function(x){ifelse(x$description==mysku,TRUE,FALSE)})
  mybilling[keep,]
}
