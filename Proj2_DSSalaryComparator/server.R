

server <- function(input, output) { 
  
  options("scipen"=10) 

  ##########################################################
  # Data manipulation (for Salary DataTable)
  ###########################################################
  updateInputDataForDTable <- reactive({  
    
    dataFilteredForDTable <- salary_refined
    dataTmp <- dataFilteredForDTable[1:500, ]
    dataTmp <- dataTmp %>% group_by(JOB_TITLE_SUBGROUP)
    
    if(input$checkboxForDS != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "data scientist"),]        
    } 
    if(input$checkboxForSW != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "software engineer"),]        
    } 
    if(input$checkboxForDA != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "data analyst"),]        
    } 
    if(input$checkboxForBA != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "business analyst"),]        
    } 
    if(input$checkboxForAP != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "assistant professor"),]        
    } 
    if(input$checkboxForMC != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "management consultant"),]        
    } 
    if(input$checkboxForAT != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "attorney"),]        
    } 
    if(input$checkboxForTC != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "teacher"),]        
    } 
    
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by States (with Multiple Selectize)
    # (AND operations. --> Ex) find 'business' in CA and GA having avg.ratings of 3.5)
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForStates) ){
      targetStates <- unlist(strsplit(input$multiSelectForStates," "))
      dataFilteredForDTable <- dataFilteredForDTable %>% filter(WORK_STATE_ABBREVIATION %in% targetStates)
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by City
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForCity != ""){
      dataFilteredForDTable <- dataFilteredForDTable %>% 
        filter(grepl(input$searchInputForCity,dataFilteredForDTable$WORK_CITY, ignore.case = TRUE)) 
    }
    
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Employer (with Search Term)
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForEmployer != ""){
      dataFilteredForDTable <- dataFilteredForDTable %>% 
        filter(grepl(input$searchInputForEmployer, dataFilteredForDTable$EMPLOYER_NAME, ignore.case = TRUE)) 
    }
  
    dataFilteredForDTable
  })
  
  
  ##########################################################
  # Data manipulation (for Salary Scatter Plot)
  ###########################################################
  updateInputDataForScatterPlot <- reactive({  
    dataFilteredForScatterPlot <- salary_refined
    dataFilteredForScatterPlot <- dataFilteredForScatterPlot %>% group_by(JOB_TITLE_SUBGROUP) 
    
    dataFilteredForScatterPlot <- dataFilteredForScatterPlot[(input$sliderForSalaryRangeForScatterPlot[1] <= dataFilteredForScatterPlot$PAID_WAGE_PER_YEAR &
                                                                dataFilteredForScatterPlot$PAID_WAGE_PER_YEAR <= input$sliderForSalaryRangeForScatterPlot[2]),]
    
    if(input$singleSelectForStatesForScatterPlot != ""){
      dataFilteredForScatterPlot <<- dataFilteredForScatterPlot[(input$singleSelectForStatesForScatterPlot == dataFilteredForScatterPlot$WORK_STATE_ABBREVIATION),]
    }
    
    dataFilteredForScatterPlot <<- dataFilteredForScatterPlot %>% mutate(JOB_GROUP_CODE = ifelse(JOB_TITLE_SUBGROUP == "assistant professor", 1,
                                                                                                 ifelse(JOB_TITLE_SUBGROUP == "attorney", 2,
                                                                                                        ifelse(JOB_TITLE_SUBGROUP == "business analyst", 3,
                                                                                                               ifelse(JOB_TITLE_SUBGROUP == "data analyst", 4,
                                                                                                                      ifelse(JOB_TITLE_SUBGROUP == "data scientist", 5,
                                                                                                                             ifelse(JOB_TITLE_SUBGROUP == "management consultant", 6,
                                                                                                                                    ifelse(JOB_TITLE_SUBGROUP == "software engineer", 7,
                                                                                                                                           ifelse(JOB_TITLE_SUBGROUP == "teacher", 8)))))))))
    

    manualQuartile <- function(x) {
      x <- sort(x)
      n <- length(x)
      m <- (n+1)/2
      if (floor(m) != m) { l <- m-1/2; u <- m+1/2
      } else { l <- m-1; u <- m+1 }
      c(Min=min(x), Q1=median(x[1:l]), Median = median(x[1:n]), Mean=mean(x), Q3=median(x[u:n]), Max=max(x))
    }
    res_mq <<- manualQuartile(dataFilteredForScatterPlot$PAID_WAGE_PER_YEAR)
    
    output$minBoxInScatterSummary <- renderInfoBox({
      infoBox( "MIN:", res_mq['Min'], icon = icon("fa fa-exclamation-circle"), color = "blue" ) })
    
    output$meanBoxInScatterSummary <- renderInfoBox({
      infoBox( "MEAN:", res_mq['Median'], icon = icon("fa fa-info-circle"), color = "teal" ) })
    
    output$maxBoxInScatterSummary <- renderInfoBox({ 
      infoBox( "MAX:", res_mq['Max'], icon = icon("fa fa-exclamation-circle"), color = "aqua" ) })
    
    output$q1BoxInScatterSummary <- renderInfoBox({
      infoBox( "Q1:", res_mq['Q1'], icon = icon("fa fa-exclamation-circle"), color = "blue" ) })
    
    output$medBoxInScatterSummary <- renderInfoBox({
      infoBox( "MEDIAN:", res_mq['Median'], icon = icon("fa fa-info-circle"), color = "teal" ) })
    
    output$q3BoxInScatterSummary <- renderInfoBox({
      infoBox( "Q3:", res_mq['Q3'], icon = icon("fa fa-exclamation-circle"), color = "aqua" ) })
    
    
    dataFilteredForScatterPlot
  })
  
  
  ##########################################################
  # Data manipulation (for Salary Comparison Maps)
  ###########################################################
  updateInputDataForMapOverall <- reactive({  
    dataFilteredForMap <- salary_refined
    dataFilteredForMap <- dataFilteredForMap %>% group_by(WORK_STATE) %>% summarise(AVG_SALARY= round(mean(PAID_WAGE_PER_YEAR), 2))
    
    dataFilteredForMap
    
  })
  #
  
  updateInputDataForMapByJobTitle1 <- reactive({  
    dataFilteredForMapByJobTitle1 <- salary_refined
    dataFilteredForMapByJobTitle1 <- dataFilteredForMapByJobTitle1 %>% group_by(WORK_STATE, JOB_TITLE_SUBGROUP) %>% summarise(AVG_SALARY= round(mean(PAID_WAGE_PER_YEAR), 2), NUM_POS = n())
    
    if(input$singleSelectForJobTitleForComparison1 != ""){
      dataFilteredForMapByJobTitle1 <- dataFilteredForMapByJobTitle1[(input$singleSelectForJobTitleForComparison1 == dataFilteredForMapByJobTitle1$JOB_TITLE_SUBGROUP),]
    } 
    
    dataFilteredForMapByJobTitle1
    
  })
  
  updateInputDataForMapByJobTitle2 <- reactive({  
    dataFilteredForMapByJobTitle2 <- salary_refined
    dataFilteredForMapByJobTitle2 <- dataFilteredForMapByJobTitle2 %>% group_by(WORK_STATE, JOB_TITLE_SUBGROUP) %>% summarise(AVG_SALARY= round(mean(PAID_WAGE_PER_YEAR), 2), NUM_POS = n())
    
    if(input$singleSelectForJobTitleForComparison2 != ""){
      dataFilteredForMapByJobTitle2 <- dataFilteredForMapByJobTitle2[(input$singleSelectForJobTitleForComparison2 == dataFilteredForMapByJobTitle2$JOB_TITLE_SUBGROUP),]
    } 
    
    dataFilteredForMapByJobTitle2
  })
  
  
  
  ####################################################################################################################
  # Rendering Section
  #####################################################################################################################
  
  #////////////////////////////////////////////////////////////////////////////////
  # DataTable
  #////////////////////////////////////////////////////////////////////////////////
  output$myTable <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTable <- updateInputDataForDTable()
    dataForDTable <- dataForDTable[, c(1,2,4,5,7,9,12,14:15)]
    colnames(dataForDTable) <- c("JOB_TITLE_GROUP","JOB_TITLE","EMPLOYER", "WAGE_YEARLY", "CITY", "STATE", "REQ_EXPERIENCE?", "REQ_EDU_LEVEL", "REQ_UNIV_MAJOR")
    
    dataForDTable
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = F,
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
    #, pageLength = 20
  ) #,filter = 'top' #<-- Warning: no non-missing arguments to min & max; returning Inf (or -Inf)
  
  #%>% formatCurrency(c('WAGE_YEARLY'), "$") #%>% formatPercentage('D', 2)
  ) %>% formatCurrency(c('WAGE_YEARLY'), "$") ) 
  #)) 
  
  
  
  
  #////////////////////////////////////////////////////////////////////////////////
  # googleVis Map 
  #////////////////////////////////////////////////////////////////////////////////
  output$myGvisMap1 <- renderGvis({
    
    mapData <- updateInputDataForMapByJobTitle1() # View(mapData)
    gvisGeoChart(mapData, locationvar= "WORK_STATE", colorvar="AVG_SALARY",
                 options=list(region="US", displayMode="regions", resolution="provinces", 
                              width="100%", #height=200, 
                              colorAxis="{colors:['#36648b', '#9a162c']}",
                              backgroundColor="gray"
                              #colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                 )
    )
  })  
  # myGvisMap1 myGvisMap2 myComparisonTableByState
  output$myGvisMap2 <- renderGvis({
    
    mapData <- updateInputDataForMapByJobTitle2() # View(mapData)
    gvisGeoChart(mapData, locationvar= "WORK_STATE", colorvar="AVG_SALARY",
                 options=list(region="US", displayMode="regions", resolution="provinces", 
                              width="100%", #height=400, 
                              #colorAxis="{colors:['#91BFDB', '#FC8D59']}",
                              colorAxis="{colors:['#5adbf2', '#fe9128']}",
                              backgroundColor="gray"
                              #colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                 )
    )
  })  
  
  #////////////////////////////////////////////////////////////////////////////////
  # DataTables for googleVis Map 
  #////////////////////////////////////////////////////////////////////////////////
  output$myComparisonTableByJobTitle1 <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTable1 <- updateInputDataForMapByJobTitle1()
    colnames(dataForDTable1) <- c("STATE","JOB_TITLE","AVG_SALARY", "JOBS") 
    dataForDTable1
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  )) %>% formatCurrency(c('AVG_SALARY'), "$") ) 
  
  
  
  output$myComparisonTableByJobTitle2 <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTable2 <- updateInputDataForMapByJobTitle2()
    colnames(dataForDTable2) <- c("STATE","JOB_TITLE","AVG_SALARY", "JOBS") 
    dataForDTable2
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  )) %>% formatCurrency(c('AVG_SALARY'), "$") ) 
  
  
  #////////////////////////////////////////////////////////////////////////////////
  # ScatterPlot (qplot)
  #////////////////////////////////////////////////////////////////////////////////
  output$myQScatterChart <- renderPlot({
    
    dataForScatterPlot <- updateInputDataForScatterPlot()
    
    if(input$checkboxForShowDataPoint == T){
      qplot(JOB_TITLE_SUBGROUP, PAID_WAGE_PER_YEAR, data=dataForScatterPlot, geom="boxplot", group = JOB_TITLE_SUBGROUP, color=JOB_TITLE_SUBGROUP) + 
        labs(title = "Total review counts by the number of fans") + 
        geom_jitter(position=position_jitter(width=.9), size=1, alpha=.3) +  
        theme(legend.position="none")  
    } else{
      qplot(JOB_TITLE_SUBGROUP, PAID_WAGE_PER_YEAR, data=dataForScatterPlot, geom="boxplot", group = JOB_TITLE_SUBGROUP, color=JOB_TITLE_SUBGROUP) + 
        labs(title = "Total review counts by the number of fans") + 
        theme(legend.position="none")
    }
  })
  
  
  
  
  ###########################################################
  # DataTables for Overall Recruitment Ranking
  ###########################################################
  output$myTableForOverallRank <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTableOverall <- rankingSummaryByEmployer
    dataForDTableOverall <- dataForDTableOverall %>% arrange(desc(JOBS))
    dataForDTableOverall
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#ffebe6', 'color': '#2a4e6c'});",
      "$(this.api().table().header()).css({'background-color': '#cccccc', 'color': '#2a4e6c'});",
      "}")
    #, pageLength = 20
  )) %>% formatCurrency(c('AVG', 'MIN', 'Q1', 'Median', 'Q3', 'MAX'), "$") 
  ) 
  
  
  updateInputDataForDTableForDS <- reactive({
    
    if(input$singleSelectForStatesForTopRecruiterDS != "All"){
      dataForDTableByEmployerJobTitleDS <- rankingSummaryByEmployerJobTitleAggr  
      
      dataForDTableByEmployerJobTitleDS <- dataForDTableByEmployerJobTitleDS[(input$singleSelectForStatesForTopRecruiterDS == dataForDTableByEmployerJobTitleDS$WORK_STATE),]
    } else {
      dataForDTableByEmployerJobTitleDS <- rankingSummaryByEmployerJobTitle        
    }
    
    dataForDTableByEmployerJobTitleDS <- dataForDTableByEmployerJobTitleDS %>% arrange(desc(JOBS))
    dataForDTableByEmployerJobTitleDS
  })
  
  
  output$myTableForDataScientistRank <- DT::renderDataTable(DT::datatable({ 
    
    dataLocalForDTableByEmployerJobTitleDS <- updateInputDataForDTableForDS()
    dataLocalForDTableByEmployerJobTitleDS <- dataLocalForDTableByEmployerJobTitleDS[dataLocalForDTableByEmployerJobTitleDS$JOB_TITLE_SUBGROUP == "data scientist",]
    dataLocalForDTableByEmployerJobTitleDS <- dataLocalForDTableByEmployerJobTitleDS %>% arrange(desc(JOBS))
    
    dataLocalForDTableByEmployerJobTitleDS # colnames(dataForDTableByEmployerJobTitleDS)
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#b3f0ff', 'color': '#2a4e6c'});",
      "$(this.api().table().header()).css({'background-color': '#cccccc', 'color': '#2a4e6c'});",
      "}")
    #, pageLength = 20
  )) %>% formatCurrency(c('AVG', 'MIN', 'Q1', 'Median', 'Q3', 'MAX'), "$") 
  ) 
  
  updateInputDataForDTableForSW <- reactive({
    
    if(input$singleSelectForStatesForTopRecruiterSW != "All"){
      dataForDTableByEmployerJobTitleSW <- rankingSummaryByEmployerJobTitleAggr  
      dataForDTableByEmployerJobTitleSW <- dataForDTableByEmployerJobTitleSW[(input$singleSelectForStatesForTopRecruiterSW == dataForDTableByEmployerJobTitleSW$WORK_STATE),]
    } else {
      dataForDTableByEmployerJobTitleSW <- rankingSummaryByEmployerJobTitle        
    }
    
    dataForDTableByEmployerJobTitleSW <- dataForDTableByEmployerJobTitleSW %>% arrange(desc(JOBS))
    dataForDTableByEmployerJobTitleSW
  })
  
  
  output$myTableForSoftwareRank <- DT::renderDataTable(DT::datatable({ 
    
    dataLocalForDTableByEmployerJobTitleSW <- updateInputDataForDTableForSW()
    dataLocalForDTableByEmployerJobTitleSW <- dataLocalForDTableByEmployerJobTitleSW[dataLocalForDTableByEmployerJobTitleSW$JOB_TITLE_SUBGROUP == "software engineer",]
    dataLocalForDTableByEmployerJobTitleSW <- dataLocalForDTableByEmployerJobTitleSW %>% arrange(desc(JOBS))
    
    dataLocalForDTableByEmployerJobTitleSW # colnames(dataForDTableByEmployerJobTitleDS)
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#e6e6ff', 'color': '#2a4e6c'});",
      "$(this.api().table().header()).css({'background-color': '#cccccc', 'color': '#2a4e6c'});",
      "}")
    #, pageLength = 20
  )) %>% formatCurrency(c('AVG', 'MIN', 'Q1', 'Median', 'Q3', 'MAX'), "$") 
  ) 
  
  
  updateInputDataForDTableForDA <- reactive({
    
    if(input$singleSelectForStatesForTopRecruiterDA != "All"){
      dataForDTableByEmployerJobTitleDA <- rankingSummaryByEmployerJobTitleAggr  
      dataForDTableByEmployerJobTitleDA <- dataForDTableByEmployerJobTitleDA[(input$singleSelectForStatesForTopRecruiterDA == dataForDTableByEmployerJobTitleDA$WORK_STATE),]
    } else {
      dataForDTableByEmployerJobTitleDA <- rankingSummaryByEmployerJobTitle        
    }
    
    dataForDTableByEmployerJobTitleDA
  })
  
  output$myTableForDataAnalystRank <- DT::renderDataTable(DT::datatable({ 
    
    dataLocalForDTableByEmployerJobTitleDA <- updateInputDataForDTableForDA() 
    dataLocalForDTableByEmployerJobTitleDA <- dataLocalForDTableByEmployerJobTitleDA[dataLocalForDTableByEmployerJobTitleDA$JOB_TITLE_SUBGROUP == "data analyst",]
    dataLocalForDTableByEmployerJobTitleDA
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#e6ffe6', 'color': '#2a4e6c'});",
      "$(this.api().table().header()).css({'background-color': '#cccccc', 'color': '#2a4e6c'});",
      "}")
    #, pageLength = 20
  )) %>% formatCurrency(c('AVG', 'MIN', 'Q1', 'Median', 'Q3', 'MAX'), "$") 
  ) 
  
  
  updateInputDataForDTableForDA <- reactive({
    
    if(input$singleSelectForStatesForTopRecruiterOT != "All"){
      dataForDTableByEmployerJobTitleOT <- rankingSummaryByEmployerJobTitleAggr  
      dataForDTableByEmployerJobTitleOT <- dataForDTableByEmployerJobTitleOT[(input$singleSelectForStatesForTopRecruiterOT == dataForDTableByEmployerJobTitleOT$WORK_STATE),]
    } else {
      dataForDTableByEmployerJobTitleOT <- rankingSummaryByEmployerJobTitle        
    }
    
    dataForDTableByEmployerJobTitleOT <- dataForDTableByEmployerJobTitleOT %>% arrange(desc(JOBS))
    dataForDTableByEmployerJobTitleOT
  })
  
  
  output$myTableForOthersRank <- DT::renderDataTable(DT::datatable({ 
    
    dataLocalForDTableByEmployerJobTitleOT <- updateInputDataForDTableForDA() 
    dataLocalForDTableByEmployerJobTitleOT <- dataLocalForDTableByEmployerJobTitleOT[(
      dataLocalForDTableByEmployerJobTitleOT$JOB_TITLE_SUBGROUP != "data scientist" &
        dataLocalForDTableByEmployerJobTitleOT$JOB_TITLE_SUBGROUP != "software engineer" &
        dataLocalForDTableByEmployerJobTitleOT$JOB_TITLE_SUBGROUP != "data analyst"
    ),]
    dataLocalForDTableByEmployerJobTitleOT <- dataLocalForDTableByEmployerJobTitleOT %>% arrange(desc(JOBS))
    dataLocalForDTableByEmployerJobTitleOT
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#fff5e6', 'color': '#2a4e6c'});",
      "$(this.api().table().header()).css({'background-color': '#cccccc', 'color': '#2a4e6c'});",
      "}")
    #, pageLength = 20
  )) %>% formatCurrency(c('AVG', 'MIN', 'Q1', 'Median', 'Q3', 'MAX'), "$") 
  ) 
  

  
  ###########################################################
  # DataTables for External Links
  ###########################################################
  output$myTableForExternalLinkDSC <- DT::renderDataTable(DT::datatable({ 
    dataForExternalLinkDSC <- externalData
    dataForExternalLinkDSC
    
  }, rownames = F, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(50, 5, 10, 15, 25, 25, 100), c('50', '5', '10', '15', '20', '25','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
    #, pageLength = 20
  ))) 
  
}

##########################################################
# ShinyApp main function
###########################################################

app <- shinyApp(
  ui <- ui, 
  server <- server
)

runApp(app, host = '0.0.0.0', port = 3168)
