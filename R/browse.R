# simple utility for taking key-value pair list to data.frame
kvpivot = function(lis) {
 lens = vapply(lis, length, integer(1))
 stopifnot(all(lens==2))
 nms = names(lis[[1]])
 ul = unlist(lis)
 ans = data.frame(t(matrix(ul, nrow=2)))
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
#' @importFrom lubridate as_date as_datetime
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom ggplot2 ggplot aes geom_point geom_bar theme
#' @return returns "NULL"
#' @examples
#' if (interactive()) browse_reck()
#' @export
browse_reck = function() runApp(list(ui=brec_ui, server=brec_server))


  brec_ui = fluidPage(
   shinytoastr::useToastr(),
   sidebarLayout(
    sidebarPanel(
     helpText("AnVIL Billing browser"),
     textInput("email", "Google email", value=""),
     textInput("bqproj", "BQproject", value="bjbilling"),
     textInput("dataset", "BQdataset", value="anvilbilling"),
     textInput("billing", "billing code", value="landmarkanvil2"),
     dateInput("startd", "start date", value="2020-08-04"),
     dateInput("endd", "end date (inclusive)", value="2020-08-10"),
     actionButton("go", "proceed", class="btn-success"),
     actionButton("stopBtn", "stop app"),
     width=3
     ),
    mainPanel(
     tabsetPanel(
      tabPanel("basic",
       DT::dataTableOutput("bag")
       ),
      tabPanel("plot",
       plotlyOutput("plot"),
       plotOutput("cumplot")
       ),
      tabPanel("about",
       verbatimTextOutput("sess")
       )
      )
    )
   )
  )


  brec_server = function(input, output) {
   getdb = eventReactive(input$go, {
    shinytoastr::toastr_info("establishing BQ connection", newestOnTop=TRUE)
    validate(need(nchar(input$email)>0, "enter your google identity (email)"))
    bigrquery::bq_auth(email=input$email, use_oob=TRUE)
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
      sk = as_tibble(AnVILBilling:::kvpivot(arec$sku))
      ss = split(arec$cost, sk$description)
      ans = sort(sapply(ss, sum), decreasing=TRUE) 
      ans = ans[ans>0]
      nm = names(ans)
      lk = data.frame(service=nm, cost=ans[ans>0])
      sm = sum(ans[ans>0])
      nd = data.frame(service="TOTAL", cost=sm)
      lk = rbind(lk, nd)
      rownames(lk) = NULL
      lk
      }, options=list(lengthMenu=c(25,50,100)))
   output$plot = renderPlotly({
       arec = getreck()
       arecsk = AnVILBilling:::kvpivot(arec$sku)
       arec$res = arecsk[,2]
       arec = arec[arec$cost>0,]
       parec = ggplot(arec, aes(x=usage_start_time, y=cost, fill=res)) + 
             geom_bar(stat="identity")  + theme(legend.position="none")
       ggplotly(parec)
      })
   output$cumplot = renderPlot({
      arec = getreck()
      arec = arec[order(arec$usage_start_time),]
      ggplot(arec, aes(x=usage_start_time, y=cumsum(cost))) + geom_point()
      })
   output$sess = renderPrint({
      list(note="This is a prototype of a system for reviewing costs associated with AnVIL usage.", sess=sessionInfo())
      })
   observeEvent(input$stopBtn, stopApp(returnValue=getreck()))
  }

oldbrowse_reck = function(bq_email=NA, do_auth=FALSE) {
  ui = fluidPage(
   shinytoastr::useToastr(),
   sidebarLayout(
    sidebarPanel(
     helpText("AnVIL Billing browser"),
     textInput("bqproj", "BQproject", value="bjbilling"),
     textInput("dataset", "BQdataset", value="anvilbilling"),
     textInput("billing", "billing code", value="landmarkanvil2"),
     dateInput("startd", "start date", value="2020-08-04"),
     dateInput("endd", "end date (inclusive)", value="2020-08-10"),
     actionButton("stopBtn", "stop app"),
     width=3
     ),
    mainPanel(
     tabsetPanel(
      tabPanel("basic",
       DT::dataTableOutput("bag")
       ),
      tabPanel("plot",
       plotlyOutput("plot"),
       plotOutput("cumplot")
       ),
      tabPanel("about",
       verbatimTextOutput("sess")
       )
      )
    )
   )
  )
  server = function(input, output) {
   getdb = reactive({
    shinytoastr::toastr_info("establishing BQ connection", newestOnTop=TRUE)
    #if (do_auth) bigrquery::bq_auth(email=bq_email, use_oob=TRUE)
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
      sm = sum(ans[ans>0])
      nd = data.frame(service="TOTAL", cost=sm)
      lk = rbind(lk, nd)
      rownames(lk) = NULL
      lk
      }, options=list(lengthMenu=c(25,50,100)))
   output$plot = renderPlotly({
#      arec = getreck()
#      xx = split(arec$cost, arec$usage_start_time)
#      sxx = sapply(xx,sum)
#      plot(lubridate::as_datetime(names(sxx)), as.numeric(sxx))
       arec = getreck()
       arecsk = AnVILBilling:::kvpivot(arec$sku)
       arec$res = arecsk[,2]
       arec = arec[arec$cost>0,]
       parec = ggplot(arec, aes(x=usage_start_time, y=cost, fill=res)) + 
             geom_bar(stat="identity")  + theme(legend.position="none")
       ggplotly(parec)
      })
   output$cumplot = renderPlot({
      arec = getreck()
      arec = arec[order(arec$usage_start_time),]
      ggplot(arec, aes(x=usage_start_time, y=cumsum(cost))) + geom_point()
      })
   output$sess = renderPrint({
      list(note="This is a prototype of a system for reviewing costs associated with AnVIL usage.", sess=sessionInfo())
      })
   observeEvent(input$stopBtn, stopApp(returnValue=getreck()))
  }
  runApp(list(ui=ui, server=server))
}


