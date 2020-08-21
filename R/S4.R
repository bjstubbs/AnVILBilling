setOldClass("tbl_df")

setClass("avReckoningRequest", representation(start="ANY", # FIXME -- use POSIXct
  end="ANY", project="character", dataset="character", table="character",
   billing_code="character"))

setClass("avReckoning", contains="avReckoningRequest", 
  representation(reckoning="tbl_df", keys="character"))

setMethod("show", "avReckoningRequest", function(object) {
cat("AnVIL reckoning info for project ", object@project, "\n")
cat(sprintf("  starting %s, ending %s.\n", object@start, object@end))
})

setMethod("show", "avReckoning", function(object) {
 callNextMethod()
 cat("There are ", nrow(object@reckoning), " records.\n")
 cat("Available keys:\n")
 print(object@keys)
 cat("---","\n")
 cat("Use obj@reckoning for full table [this will change...]\n")
})

#' set up request object
#' @param start character(1) date of start of reckoning
#' @param end character(1) date of end of reckoning
#' @param project character(1) GCP project id
#' @param dataset character(1) GCP dataset id for billing data in BQ
#' @param table character(1) GCP table for billing data in BQ
#' @param billing_code character(1) GCP billing code
#' @return instance of avReckoningRequest
#' @examples
#' lk1 = setup_billing_request("2020-08-01", "2020-08-15",
#'    "bq_scoped_project", "bq_dataset", "bq_table", "billcode")
#' lk1
#' @export
setup_billing_request = function(
 start, end, project, dataset, table, billing_code ) {
  new("avReckoningRequest", 
      start=start,
      end=end,
      project=project,
      dataset=dataset,
      table=table,
      billing_code=billing_code)
}

#' perform reckoning
#' @param obj instance of avReckoningRequest
#' @return instance of avReckoning
#' @examples
#' data(demo_rec)
#' reckon(demo_rec)
#' @export
reckon = function(obj) {
  dat = getBilling(obj@start, obj@end, obj@project, obj@dataset, obj@table, obj@billing_code)
  keys = getKeys(dat)
  new("avReckoning", obj, reckoning = dat, keys=keys)
}

setGeneric("reckoning", function(x) standardGeneric("reckoning"))
#' accessor for reckoning component
#' @param x instance of avReckoning
#' @return tbl_df
#' @export
setMethod("reckoning", "avReckoning", function(x) x@reckoning)
