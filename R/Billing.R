#' request billing data
#' @importFrom DBI dbConnect dbListTables
#' @importFrom bigrquery bigquery dbConnect
#' @import dplyr
#' @import magrittr
#' @param startDate character(1) date of start of reckoning
#' @param endDate character(1) date of end of reckoning
#' @param bqProject character(1) GCP project id
#' @param bqDataset character(1) GCP dataset id for billing data in BQ
#' @param bqTable character(1) GCP table for billing data in BQ
#' @param bqBilling_code character(1) GCP billing code
#' @param page_size numeric(1) passed to dbConnect
#' @return tbl_df
#' @note On 21 August 2020 VJC changed condition on endDate to <=
getBilling<-function(startDate,endDate,bqProject,bqDataset,bqTable,bqBilling_code,
              page_size=50000){
  con <- bigrquery::dbConnect(
       bigquery(),
       project = bqProject,
       dataset = bqDataset,
       billing = bqBilling_code,
       page_size=page_size
     )
  out = con%>%tbl(bqTable)%>%
    filter(usage_start_time >= startDate & usage_end_time <= endDate)%>%
    collect()
  return(out)
}

#' return keys
#' @return character()
#' @param mybilling tbl_df
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
#' @return character()
#' @examples
#' if (interactive()) #' getValues(reckoning(demo_rec), "security")
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
#' @return data.frame
#' @examples
#' data(demo_rec) # makes rec
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
#' @param mybilling tbl_df
#' @return character()
getSkus<-function(mybilling){
  unique(unlist(lapply(mybilling$sku,function(x){x$description})))
}

#' subset a billing object by sku
#' @param mysku character(1) GCP product sku
#' @param mybilling tbl_df
#' @return data.frame
subsetBySku<-function(mybilling,mysku){
  temp=mybilling$sku
  keep=sapply(temp,function(x){ifelse(x$description==mysku,TRUE,FALSE)})
  mybilling[keep,]
}

#' Calcuate costs for a workflow submission by ID
#' @param mybilling tbl_df
#' @param submissionID character(1) Terra submission ID
#' @return numeric()
#' @examples
#' data(demo_rec) # makes rec
#' v = getValues(demo_rec@reckoning, "terra-submission-id")[1] # for instance
#' getSubmissionCost(demo_rec@reckoning,v)
#' @export
getSubmissionCost<-function(mybilling, submissionID){
    if(!grepl(submissionID,pattern="^terra",perl=TRUE)){submissionID=paste0("terra-",submissionID)}
    temp=subsetByKeyValue(mybilling, "terra-submission-id", submissionID)
    sum(temp$cost)
}

#' Calcuate ram usage for a workflow submission by ID
#' @param mybilling tbl_df
#' @param submissionID character(1) Terra submission ID
#' @return data.frame
#' @examples
#' data(demo_rec) # makes rec
#' v = getValues(demo_rec@reckoning, "terra-submission-id")[1] # for instance
#' getSubmissionRam(demo_rec@reckoning,v)
#' @export
getSubmissionRam<-function(mybilling, submissionID){
    if(!grepl(submissionID,pattern="^terra", perl=TRUE)){submissionID=paste0("terra-",submissionID)}
    temp=subsetByKeyValue(mybilling, "terra-submission-id", submissionID)
    tempskus=sapply(temp$sku, function(x){x$description})
    temp=temp[tempskus=="Custom Instance Ram running in Americas",]

    cromids=as.character(sapply(temp$labels, function(x){
        ind=which(x$key=="cromwell-workflow-id")
        x[ind,"value"]}))

    wfnames=as.character(sapply(temp$labels,function(x){
        ind=which(x$key=="wdl-task-name")
        x[ind,"value"]}))

    ram1=sapply(temp$usage, function(x){x$amount})
    ram2=sapply(temp$usage, function(x){x$unit})
    ram3=sapply(temp$usage, function(x){x$pricing_unit})
    ram4=sapply(temp$usage, function(x){x$amount_in_pricing_units})
    fskus=sapply(temp$sku, function(x){x$description})
    retdf=data.frame(submissionID=rep(submissionID,nrow(temp)),workflow=wfnames, cromwellID=cromids,sku=fskus, amount=ram1, unit=ram2, pricingUnit=ram3, amountInPricingUnit=ram4)
    retdf
}
