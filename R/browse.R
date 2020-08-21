# simple utility for taking key-value pair list to data.frame
kvpivot = function(lis) {
 lens = vapply(lis, length, integer(1))
 stopifnot(all(lens==2))
 nms = names(lis[[1]])
 ul = unlist(lis)
 ans = data.frame(t(matrix(ul, nr=2)))
 names(ans) = nms
 ans
}

# combine setup and reckon with try-error
report_interval = function(
   start, end, bqproject, dataset, table,
   billing_code) {
    req = try(setup_billing_request(start=start,
	end=end, project=bqproject, dataset=dataset,
        table=table, billing_code=billing_code))
    if (inherits(req, "try-error")) {
      warning("billing_request failed.")
      return(req)
      }
    reckon(req)
}

#' prototypical cost exploring app
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom shinytoastr useToastr toastr_info
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom DBI dbConnect
#' @importFrom bigrquery bigquery bq_auth
#' @importFrom lubridate as_date
#' @param bq_email character(1) email used to identify google BigQuery api user
#' @return NULL
#' @export
browse_reck = function(bq_email=NA) {
  bigrquery::bq_auth(email=bq_email)
  ui = fluidPage(
   shinytoastr::useToastr(),
   sidebarLayout(
    sidebarPanel(
     helpText("AnVIL Billing browser"),
     textInput("bqproj", "BQproject", value="bjbilling"),
     textInput("dataset", "BQdataset", value="anvilbilling"),
     textInput("billing", "billing code", value="landmarkanvil2"),
     dateInput("startd", "start date", value="2020-08-01"),
     dateInput("endd", "end date (inclusive)", value="2020-08-18"),
     actionButton("stopBtn", "stop app"),
     width=2
     ),
    mainPanel(
     tabsetPanel(
      tabPanel("basic",
       DT::dataTableOutput("bag")
       ),
      tabPanel("plot",
       plotOutput("plot")
       )
      )
    )
   )
  )
  server = function(input, output) {
   getdb = reactive({
    shinytoastr::toastr_info("establishing BQ connection", newestOnTop=TRUE)
    con = DBI::dbConnect(bigrquery::bigquery(), 
         project =input$bqproj, billing=input$billing, dataset=input$dataset)
    list(con=con, table=dbListTables(con))
    })
   getrequest = reactive({
    dbstuff = getdb()
    AnVILBilling::setup_billing_request(input$startd,
        input$endd, input$bqproj, input$dataset, dbstuff$table, input$billing)
    })
    getreck = reactive({
    shinytoastr::toastr_info("reckoning...", newestOnTop=TRUE)
      AnVILBilling::reckon(getrequest())@reckoning
      })

   output$bag = DT::renderDataTable({
      arec = NULL
      arec = getreck()
      sk = as_tibble(kvpivot(arec$sku))
      ss = split(arec$cost, sk$description)
      ans = sort(sapply(ss, sum), decreasing=TRUE) 
      ans = ans[ans>0]
      nm = names(ans)
      lk = data.frame(service=nm, cost=ans[ans>0])
      rownames(lk) = NULL
      lk
      })
   output$plot = renderPlot({
      arec = getreck()
      xx = split(arec$cost, arec$usage_start_time)
      sxx = sapply(xx,sum)
      plot(lubridate::as_date(names(sxx)), as.numeric(sxx))
      })
   observeEvent(input$stopBtn, stopApp(returnVal=NULL))
  }
  runApp(list(ui=ui, server=server))
}
