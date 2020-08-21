#' request billing data
#' @param start character(1) date of start of reckoning
#' @param end character(1) date of end of reckoning
#' @param project character(1) GCP project id
#' @param dataset character(1) GCP dataset id for billing data in BQ
#' @param table character(1) GCP table for billing data in BQ
#' @param billing_code character(1) GCP billing code
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

#' return keys
#' @param mybillilng tbl_df
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

#' deal with nested tables in a reckoning
#' @param mybilling tbl_df from reckon()
#' @param mykey character(1) key
#' @export
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

#' filter a reckoning by 'label' retaining records associated with a particular key-value pair
#' @param mybilling instance of avReckoning
#' @param mykey character(1)
#' @param myvalue character(1)
#' @examples
#' example(reckon) # makes rec
#' v = getValues(demo_rec@reckoning, "terra-submission-id")[1] # for instance
#' nt = subsetByKeyValue(demo_rec@reckoning, "terra-submission-id", v)
#' head(nt)
#' dim(nt)
#' @export
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

#' List the available GCP product skus
#' @param mybillilng tbl_df
getSkus<-function(mybilling){
  unique(unlist(lapply(mybilling$sku,function(x){x$description})))
}

#' subset a billing object by sku
#' @param mysku character(1) GCP product sku
#' @param mybillilng tbl_df
subsetBySku<-function(mybilling,mysku){
  temp=mybilling$sku
  keep=sapply(temp,function(x){ifelse(x$description==mysku,TRUE,FALSE)})
  mybilling[keep,]
}
