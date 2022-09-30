library(shiny)
library(shinyWidgets)
library(readxl)
library(ggplot2)
library(shinydashboard)
library(shinythemes)
library(DT)
library(dplyr)
library(ggridges)
library(shinyjs)
#library(slickR)

#The below java scrip code is for the print button function in the report card tab in the analytics tab
jsCode <- 'shinyjs.winprint = function(){
window.print();
}'

ui <- fluidPage(
  theme = shinytheme("cosmo"), #selection of one of the standard themes from shinytheme package.
  navbarPage(title = "                              ",
             tabPanel("HOME", icon = icon("home"),# code for home tab starts
                      setBackgroundImage(src = "all13.png"), # this is the link to the background image of the webapp. Currently the Volvo 'headlight'. the image is in the 'www' folder
                      fluidRow(column(11,tags$video(src ="Volvo Trucks - Masters of Efficiency.mp4" ,type = "video/mp4",autoplay = TRUE,controls = TRUE, muted = TRUE, playsinline = TRUE, height = 309, width = 550)), column(1,)) # code for the video that is played n the homepage. Video in the 'www' folder
                      #fluidRow(column(4,slickROutput("imgslide", width = "448px", height = "299px")), column(8,)) # This is code for running a slideshow in the homepage instead of the video
             ), #Code for home tab ends
             tabPanel("DATA", icon = icon("database"), #Code for data tab UI starts 
                      wellPanel(
                        fluidRow(column(3,fileInput(inputId = "rawFile", label = "Select files", multiple = TRUE, accept = ".xlsx"))) #code for file input (xlsx only).
                      ), hr(),
                      uiOutput("dataUIelement") #The consolidated data table UI is generated after the input file(s) is uploaded. The code will be found in the server function
             ),#Code for data tab UI ends
             tabPanel("ANALYTICS", icon = icon("brain"), #Code for Analytics tab starts
                      tabsetPanel( # for tabs within a tab
                        tabPanel("Fleet analytics",hr(), #code for fleet analytics tab UI starts
                                 uiOutput("fleetAnalyticsTab") # The entire UI of the fleetanalytics tab is encapsulated in one single UI element. Code for this single element is within the server function after all the constituent UI elements are defined
                        ), #code for analytics tab UI ends
                        tabPanel("Plots", hr(), # Code for plots tab UI starts
                                 uiOutput("plotsTab") # Entire UI encapsulated, just like for analytics tab
                        ), #code for plots tab UI ends
                        tabPanel("Report card", hr(),  #code for report card tab UI starts
                          uiOutput("reportcardTab") #Entire UI encapsulated
                        ) #code for report card UI ends
                      ),
             ),
             tabPanel("COMPARISON", icon = icon("grip-vertical"), #code for comparison tab
                      wellPanel(
                        fluidRow(column(3,fileInput(inputId = "rawFile1", label = "Data before training", multiple = TRUE, accept = ".xlsx")), column(3,), column(3,),column(3,fileInput(inputId = "rawFile2", label = "Data after training", multiple = TRUE, accept = ".xlsx")))  #UI for both file inputs
                      ),
                      uiOutput("compFullUIelement") # Comparison tab UI encapsulated
             )
  )
)

server <- function(input, output, session) {
  
  #HOME
  # #slideshow
  # output$imgslide <- renderSlickR({
  #   imgs <- list.files("./www/slide/", pattern=".jpg", full.names = TRUE)
  #   slickR(imgs) + settings(autoplay = TRUE, autoplaySpeed = 850)
  # })
  
  
  #DATA TAB...................................
  
  baseData = reactive({ #Reading data into a dataframe called baseData. This will be available as a reactive data frame
    req(input$rawFile) #checking if the file input is done
    tryCatch(
      {
        df = lapply(input$rawFile$datapath, function(i){
          x = read_excel(i, sheet = 1, col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess","guess","guess","guess","date")) # reading and coercing data types. Guess would mean automatic datatype assigning. FOr the last column, we specify date data type
          return(x)
        }) #looping through to take multiple file inputs
        df = do.call("rbind.data.frame", df)  #Row-binding the contents of the file
        df['Fuel Efficiency(Kmpl)'] = (df[,3] / df[,4]) #defining new column for fuel efficiency
        df = df[, colnames(df)[c(1,2,3,4,27,5:26)]] #Re-arranging the columns
        df = df %>% filter(`Total distance (km)` > 10) #filtering out cases where total distance less than 10KM per day
        df = df %>% filter(`PTO time` > 0.02 ) #filtering out cases where PTO time is less than 2% per day
        df = df %>% filter(`Idling time` > 0.1) #filtering out cases where idling time less than 10%
      },
      error = function(e){
        stop(safeError(e))
      }
    )
  })
  
  #Table output to the Data tab.This UI element is added to the single UI encasulation
  output$rawDataTable = renderDataTable({
    datatable(
      baseData(),
      extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = 650,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     # buttons = list('excel',
                     # list(extend = 'colvis', targets = 0, visible = FALSE)),
                     #dom = 'lBfrtip',
                     dom = 'lfti',
                     fixedColumns = TRUE)
    )
  })
  
  #UI encapsulation. THis is done with a check if the file(s) is uploaded or not.If the file is not uploaded, then nothing is displayed
  output$dataUIelement = renderUI({
    req(baseData())
    wellPanel(
      fluidRow(dataTableOutput("rawDataTable"))
    )
  })
  
  #Creating 2 separate lists: variable list and chassis list , which will be used extensively
 
  #create the list of vehicles/chassis
  chList = reactive({
    as.list(unique((baseData()[,1])))
  })
  
  #Create the list of variables from the column headers. This is common for Histogram, XY & Time-series
  varlist = reactive({
    as.list(colnames(baseData()))
  })
  
  
  #ANALYTICS TAB..............................................

  
  ##########code for Plots tab############
  
  #create 6 X-Y plots in the plots tab within the analytics tab
  
  output$engLoadXYplot = renderPlot({
    ggplot(data = baseData(), aes(x = `Engine load time`, y = `Fuel Efficiency(Kmpl)`)) + labs(title="FE Vs Engine Load Time", x="% Engine Load Time", y="Fuel Efficiency(Kmpl)") + geom_point(color = "steelblue", size = 3) +  geom_smooth(method = lm, size = 0.8, col = 3, fill = "lightblue") + ylim(c(0,2))
  })
  
  output$avgSpeedXYplot = renderPlot({
    ggplot(data = baseData(), aes(x = `Average speed driving (km/h)`, y = `Fuel Efficiency(Kmpl)`)) + labs(title="FE Vs Average Speed", x="Average Speed", y="Fuel Efficiency(Kmpl)") + geom_point(color = "steelblue", size = 3) +  geom_smooth(method = lm, size = 0.8, col = 3, fill = "lightblue") + ylim(c(0,2))
  })
  
  output$coastingXYplot = renderPlot({
    ggplot(data = baseData(), aes(x = `Coasting time`, y = `Fuel Efficiency(Kmpl)`)) + labs(title="FE Vs Coasting", x="% Coasting", y="Fuel Efficiency(Kmpl)") + geom_point(color = "steelblue", size = 3) +  geom_smooth(method = lm, size = 0.8, col = 3, fill = "lightblue") + ylim(c(0,2))
  })
  
  output$idlingXYplot = renderPlot({
    ggplot(data = baseData() , aes(x = `Idling time`, y = `Fuel Efficiency(Kmpl)`)) + labs(title="FE Vs Idling Chart", x="% Idling", y="Fuel Efficiency(Kmpl)") + geom_point(color = "steelblue", size = 3) + geom_smooth(method = lm, size = 0.8, col = 3, fill = "lightblue")+ ylim(c(0,2))
  }) 
  
  output$ptoTimeXYplot = renderPlot({
    ggplot(data = baseData(), aes(x = `PTO time`, y = `Fuel Efficiency(Kmpl)`)) + labs(title="FE Vs PTO Operation", x="% PTO Time", y="Fuel Efficiency(Kmpl)") + geom_point(color = "steelblue", size = 3) +  geom_smooth(method = lm, size = 0.8, col = 3, fill = "lightblue") + ylim(c(0,2))
  })
  
  output$abvEconXYplot = renderPlot({
    ggplot(data = baseData() , aes(x = `Above economy time`, y = `Fuel Efficiency(Kmpl)`)) + labs(title="FE Vs Above Economy Driving", x="% Above Economy Time", y="Fuel Efficiency(Kmpl)") + geom_point(color = "steelblue", size = 3) + geom_smooth(method = lm, size = 0.8, col = 3, fill = "lightblue") + ylim(c(0,2))
  })
  

  #create the 3 time series plots
  
  #selection UI to choose the chassis
  output$chassisChoice1 = renderUI({
    selectInput(inputId = "vehId1", label = "Chooose Chassis", choices = chList()) 
  })
  
  #based on the selection of chassis, we pull out a new data base with data only for the selected chassis
  tempDF = reactive({
    baseData() %>% filter(baseData()[,1] == input$vehId1)
  })
  
  #creating a single column dataframe for FE values from the selected vehicle Dataframe 
  varTSFE = reactive({
    tempDF()[,5]
  })
  
  #creating a single column dataframe for Engg load values from the selected vehicle Dataframe
  varTSEngLoad = reactive({
    tempDF()[,19]
  })
  
  #creating a single column dataframe for above econ time values from the selected vehicle Dataframe
  varTSAbovEcon = reactive({
    tempDF()[,14]
  })
  
  #Time series chart for FE
  output$TimeSeriesFE = renderPlot({
    ggplot(data = tempDF(), aes(x = as.Date(tempDF()$Date), y = as.numeric(varTSFE()[[1]]))) + geom_line() + geom_hline(yintercept = mean(as.numeric(varTSFE()[[1]])), colour = "steelblue") + geom_hline(yintercept = (mean(as.numeric(varTSFE()[[1]])) + (1 * sd(as.numeric(varTSFE()[[1]])))), linetype = 2, colour = "red") + geom_hline(yintercept = (mean(as.numeric(varTSFE()[[1]])) - (1 * sd(as.numeric(varTSFE()[[1]])))), linetype = 2, colour = "red") + labs(title = "Fuel Efficiency:day-wise change", x = "Days", y = "Fuel Efficiency")
  })
  
  #Time series chart for Engg load
  output$TimeSeriesEngLoad = renderPlot({
    #plot(x = as.Date(tempDF()$Date), y = as.numeric(varTS()[[1]]), type = "l" )
    ggplot(data = tempDF(), aes(x = as.Date(tempDF()$Date), y = as.numeric(varTSEngLoad()[[1]]))) + geom_line() + geom_hline(yintercept = mean(as.numeric(varTSEngLoad()[[1]])), colour = "steelblue") + geom_hline(yintercept = (mean(as.numeric(varTSEngLoad()[[1]])) + (1 * sd(as.numeric(varTSEngLoad()[[1]])))), linetype = 2, colour = "red") + geom_hline(yintercept = (mean(as.numeric(varTSEngLoad()[[1]])) - (1 * sd(as.numeric(varTSEngLoad()[[1]])))), linetype = 2, colour = "red") + labs(title = "Engine Load time:day-wise change", x = "Days", y = "Engine load time")
  })
  
  #Time series chart for above economy
  output$TimeSeriesAboveEcon = renderPlot({
    #plot(x = as.Date(tempDF()$Date), y = as.numeric(varTS()[[1]]), type = "l" )
    ggplot(data = tempDF(), aes(x = as.Date(tempDF()$Date), y = as.numeric(varTSAbovEcon()[[1]]))) + geom_line() + geom_hline(yintercept = mean(as.numeric(varTSAbovEcon()[[1]])), colour = "steelblue") + geom_hline(yintercept = (mean(as.numeric(varTSAbovEcon()[[1]])) + (1 * sd(as.numeric(varTSAbovEcon()[[1]])))), linetype = 2, colour = "red") + geom_hline(yintercept = (mean(as.numeric(varTSAbovEcon()[[1]])) - (1 * sd(as.numeric(varTSAbovEcon()[[1]])))), linetype = 2, colour = "red") + labs(title = "Above Economy:day-wise change", x = "Days", y = "Above Economy time")
  })
  
  #Fleet Analytics
  
  #Creating a matrix with mean values for the specific site and the ideal site
  FLAnalyticsDF = reactive({
    #creating a matrix for storing the min and max value of the 95% confidence interval(mean)
    tstmatx = matrix(NA,8,2, dimnames = NULL)
    tstmatx[,1] = c(0.9500153, 0.06528498, 19.79709, 0.06223213, 0.04426097, 0.317919, 0.2997286,0.04677539) #min value of the 95% confidence interval for mean
    tstmatx[,2] = c(0.9529933, 0.06590266, 19.85313, 0.06278808, 0.04455361, 0.3189285, 0.301,0.04718934 ) #max value of the 95% confidence interval for mean
    #Creating a matrix for the mean of parameter values for the specific site(column1)and the ideal site(column 2). The 3rd column is the RGB /123 value based on whether its less,between or more than the mean
    tstmat = matrix(NA, 8, 4, dimnames = list(c("Fuel Efficiency(Kmpl)","Engine load time", "Average speed driving (km/h)", "Coasting time", "PTO time", "Idling time", "Within economy time", "Above economy time"), c("Benchmark Site","Selected Site", "RGBval", "Trend")))
    tstmat[,2] = c(round(mean(as.numeric(baseData()$`Fuel Efficiency(Kmpl)`)),3), round(mean(as.numeric(baseData()$`Engine load time`)),4), round(mean(as.numeric(baseData()$`Average speed driving (km/h)`)),2), round(mean(as.numeric(baseData()$`Coasting time`)),2), round(mean(as.numeric(baseData()$`PTO time`)),4), round(mean(as.numeric(baseData()$`Idling time`)),4), round(mean(as.numeric(baseData()$`Within economy time`)),4), round(mean(as.numeric(baseData()$`Above economy time`)),4))
    tstmat[,1] = c(0.953,0.0608, 21.35, 0.05281, 0.0396, 0.3100,0.3330, 0.0470)
    #Creating 2 for loops: one for paramters that are better when increasing in value and the other for paramters that are better when decreasing in value
    for(i in c(1,3,4)){
      if(tstmat[i,2] < tstmatx[i,1]){
        tstmat[i,3] = 1
      } else if((tstmat[i,2] > tstmatx[i,1]) && (tstmat[i,2] < tstmatx[i,2])){
        tstmat[i,3] = 2
      } else {
        tstmat[i,3] = 3
      }
    }
    for(i in c(2,5,6,7,8)){
      if(tstmat[i,2] < tstmatx[i,1]){
        tstmat[i,3] = 3
      } else if((tstmat[i,2] > tstmatx[i,1]) && (tstmat[i,2] < tstmatx[i,2])){
        tstmat[i,3] = 2
      } else {
        tstmat[i,3] = 1
      }
    }
    for(i in c(1,3,4)){
      if(tstmat[i,2] < tstmatx[i,1]){
        tstmat[i,4] = as.character(icon("arrow-down", lib = "glyphicon"))
      } else if((tstmat[i,2] > tstmatx[i,1]) && (tstmat[i,2] < tstmatx[i,2])){
        tstmat[i,4] = as.character(icon("equals", lib = "glyphicon"))
      } else {
        tstmat[i,4] = as.character(icon("arrow-up", lib = "glyphicon"))
      }
    }
    for(i in c(2,5,6,7,8)){
      if(tstmat[i,2] < tstmatx[i,1]){
        tstmat[i,4] = as.character(icon("arrow-up", lib = "glyphicon"))
      } else if((tstmat[i,2] > tstmatx[i,1]) && (tstmat[i,2] < tstmatx[i,2])){
        tstmat[i,4] = as.character(icon("equals", lib = "glyphicon"))
      } else {
        tstmat[i,4] = as.character(icon("arrow-down", lib = "glyphicon"))
      }
    }
    return(tstmat)
  })
  
  #Table which shows the comaparison of ideal site values with total fleet values
  output$siteSummaryTable = renderDataTable({
    datatable(FLAnalyticsDF(), options = list(columnDefs = list(list(targets = 3, visible =FALSE), list(className = 'dt-center', targets = 1:4)), dom = 't'), escape = 4) %>% formatStyle('Selected Site','RGBval', backgroundColor = styleEqual(c(1,2,3), c('orange', 'yellow', 'green')))
  })
  
  #VARIABILITY
  #compares the fleet variability(sd) of fuel efficiency with the variability of the 200k master data. Greater variability is NOK
  VariabMsg = reactive({
    if(sd(as.numeric(baseData()$`Fuel Efficiency(Kmpl)`)) > 0.19529){
      h4(HTML("&#9"),strong("Driver Behaviour"),HTML("&#10004"), span(), h5("Site Conditions"))
    } else {
      h4(HTML("&#9"),strong("Site Conditions"),HTML("&#10004"), span(), h5("Driver behaviour"))
    }
  })
  
  #return the above message as text
  output$siteFEVariance = renderUI({
    return(VariabMsg())
  })
  
  #FE PREDICTION TABLE
  #Dynamic Numeric inputs for FE prediction. Create UI outputs for all significant parameters ( in this case 9)
  output$engLoadChoice = renderUI({
    numericInput(inputId = "engLoad", label = "Engine Load", value = round(mean(as.numeric(baseData()$`Engine load time`)),4) , min = 0.00, max = 0.04, step = 0.001)
  })
  
  output$avgSpdChoice = renderUI({
    numericInput(inputId = "avgSpd", label = "Avg Speed", value = round(mean(as.numeric(baseData()$`Average speed driving (km/h)`)),2), min = 5, max = 50)
  })
  
  output$coastTimeChoice = renderUI({
    numericInput(inputId = "coastTime", label = "Coast Time", value = round(mean(as.numeric(baseData()$`Coasting time`)),4), min = 0.00, max = .20, step = 0.0001)
  })
  
  output$PTOtimeChoice = renderUI({
    numericInput(inputId = "PTOtime", label = "PTO Time", value = round(mean(as.numeric(baseData()$`PTO time`)),4), min = 0.2, max = 0.15, step = 0.0001)
  })
  
  output$idleTimeChoice = renderUI({
    numericInput(inputId = "idleTime", label = "Idle Time", value = round(mean(as.numeric(baseData()$`Idling time`)),4), min = 0.05, max = .50, step = 0.0001)
  })
  
  output$econTimeChoice = renderUI({
    numericInput(inputId = "econTime", label = "Econ Time", value = round(mean(as.numeric(baseData()$`Within economy time`)),4), min = 0.01, max = 0.80, step = 0.0001)
  })
  
  output$aboveEconTimeChoice = renderUI({
    numericInput(inputId = "aboveEconTime", label = "Abv Econ Time", value = round(mean(as.numeric(baseData()$`Above economy time`)),4), min = 0.01, max = 0.80, step = 0.0001)
  })
  
  
  
  #Equation for predicting the FE based on best 7 parameter values.
  FE = reactive({
    0.61722711 + (-3.86085728 * input$engLoad) + (0.04634512 * input$avgSpd)+ (0.43949104 * input$coastTime) + (-0.70760909 * input$PTOtime) + (-0.63906410 * input$idleTime) + (-0.39918805 * input$econTime) + (-0.63605523 * input$aboveEconTime) 
  })
  
  #returns the calculated FE value from above equation as a text
  output$FEtext = renderText({
    return(FE())
  })
  
  #Vehicle Analytics
  
  output$fleetChassisChoice = renderUI({
    selectInput(inputId = "vehId", label = "Select chassis for comparison", choices = chList()) #chList is already created for Time-series charts
  })
  
  output$comparisonChoice = renderUI({
    radioButtons(inputId = "comChoice", label = "Select to compare", choices = c("Benchmark Truck"," Site best truck"), selected = "Benchmark Truck", inline = TRUE)
  })
  
  #Filtering out and creating a new dataframe for the selected truck from fleet
  selectVehicleDF = reactive({
    baseData() %>% filter(baseData()[,1] == input$vehId)
  })
  
  #creating a table which will have average FE values against each vehicle in the fleet
  vehicleFEtableDF = reactive({
    tm = baseData() %>% group_by(`Vehicle`) %>% summarise(mean(`Fuel Efficiency(Kmpl)`))
    colnames(tm) = c("Vehicle", "Average_FE")
    data.frame(tm)
  })
  
  #Finding the truck with min FE
  vehicleMinAvgFE = reactive({
    vehicleFEtableDF()[which.min(vehicleFEtableDF()$`Average_FE`), 1]
  })
  
  #filtering and creating a new dataframe for the minimun FE truck
  vehicleMinDF = reactive({
    baseData() %>% filter(baseData()[,1] == vehicleMinAvgFE())
  })
  
  #Finding the truck with max Fe
  vehicleMaxAvgFE = reactive({
    vehicleFEtableDF()[which.max(vehicleFEtableDF()$`Average_FE`), 1]
  })
  
  #creating a separate DF for all the line items corresponding to the max FE truck
  vehicleMaxDF = reactive({
    baseData() %>% filter(baseData()[,1] == vehicleMaxAvgFE())
  })
  
  output$vehicleFEChart = renderPlot({
    ggplot(data = vehicleFEtableDF(), aes(x = `Vehicle`, y = `Average_FE`)) + geom_point() + geom_hline(yintercept = mean(as.numeric(baseData()$`Fuel Efficiency(Kmpl)`)), linetype = 2) + geom_hline(yintercept = mean(as.numeric(baseData()$`Fuel Efficiency(Kmpl)`)) + (0.05*mean(as.numeric(baseData()$`Fuel Efficiency(Kmpl)`))), linetype = 4, color = "blue") + geom_hline(yintercept = mean(as.numeric(baseData()$`Fuel Efficiency(Kmpl)`)) + (0.1*mean(as.numeric(baseData()$`Fuel Efficiency(Kmpl)`))), linetype = 3, color = "green") + theme(axis.text.x = element_blank())
  })
  
  
  #Build a comparison matrix which will have 95% CI values for ideal truck and site best FE truck
  comparisonMatrix = reactive({
    tstmatxc = matrix(NA,8,6, dimnames = NULL)
    tstmatxc[,1] = c(1.38435, 0.02912442, 21, 0.09856444, 0.0405476, 0.2841441, 0.219772,0.0718944 )
    tstmatxc[,2] = c(1.388128, 0.03101959, 21, 0.1018015, 0.04189289, 0.2883709, 0.2305609,0.0721056)
    tstmatxc[,3] = c(round(mean((as.numeric(vehicleMaxDF()$`Fuel Efficiency(Kmpl)`)) - (sd(as.numeric(vehicleMaxDF()$`Fuel Efficiency(Kmpl)`))/sqrt(nrow(vehicleMaxDF())))),3), round((mean(as.numeric(vehicleMaxDF()$`Engine load time`)) -(sd(as.numeric(vehicleMaxDF()$`Engine load time`))/sqrt(nrow(vehicleMaxDF())))),4), round((mean(as.numeric(vehicleMaxDF()$`Average speed driving (km/h)`))-(sd(as.numeric(vehicleMaxDF()$`Average speed driving (km/h)`))/sqrt(nrow(vehicleMaxDF())))),2), round((mean(as.numeric(vehicleMaxDF()$`Coasting time`)) -(sd(as.numeric(vehicleMaxDF()$`Coasting time`))/sqrt(nrow(vehicleMaxDF())))),4), round((mean(as.numeric(vehicleMaxDF()$`PTO time`))-(sd(as.numeric(vehicleMaxDF()$`PTO time`))/sqrt(nrow(vehicleMaxDF())))) ,4), round((mean(as.numeric(vehicleMaxDF()$`Idling time`))-(sd(as.numeric(vehicleMaxDF()$`Idling time`))/sqrt(nrow(vehicleMaxDF())))),4), round((mean(as.numeric(vehicleMaxDF()$`Within economy time`))-(sd(as.numeric(vehicleMaxDF()$`Within economy time`))/sqrt(nrow(vehicleMaxDF())))),4), round((mean(as.numeric(vehicleMaxDF()$`Above economy time`))-(sd(as.numeric(vehicleMaxDF()$`Above economy time`))/sqrt(nrow(vehicleMaxDF())))),4))
    tstmatxc[,4] = c(round(mean((as.numeric(vehicleMaxDF()$`Fuel Efficiency(Kmpl)`)) + (sd(as.numeric(vehicleMaxDF()$`Fuel Efficiency(Kmpl)`))/sqrt(nrow(vehicleMaxDF())))),3), round((mean(as.numeric(vehicleMaxDF()$`Engine load time`)) +(sd(as.numeric(vehicleMaxDF()$`Engine load time`))/sqrt(nrow(vehicleMaxDF())))),4), round((mean(as.numeric(vehicleMaxDF()$`Average speed driving (km/h)`))+(sd(as.numeric(vehicleMaxDF()$`Average speed driving (km/h)`))/sqrt(nrow(vehicleMaxDF())))),2), round((mean(as.numeric(vehicleMaxDF()$`Coasting time`)) +(sd(as.numeric(vehicleMaxDF()$`Coasting time`))/sqrt(nrow(vehicleMaxDF())))),4), round((mean(as.numeric(vehicleMaxDF()$`PTO time`))+(sd(as.numeric(vehicleMaxDF()$`PTO time`))/sqrt(nrow(vehicleMaxDF())))) ,4), round((mean(as.numeric(vehicleMaxDF()$`Idling time`))+(sd(as.numeric(vehicleMaxDF()$`Idling time`))/sqrt(nrow(vehicleMaxDF())))),4), round((mean(as.numeric(vehicleMaxDF()$`Within economy time`))+(sd(as.numeric(vehicleMaxDF()$`Within economy time`))/sqrt(nrow(vehicleMaxDF())))),4), round((mean(as.numeric(vehicleMaxDF()$`Above economy time`))+(sd(as.numeric(vehicleMaxDF()$`Above economy time`))/sqrt(nrow(vehicleMaxDF())))),4))
    tstmatxc[,5] = c(round(mean((as.numeric(baseData()$`Fuel Efficiency(Kmpl)`)) - (sd(as.numeric(baseData()$`Fuel Efficiency(Kmpl)`))/sqrt(nrow(baseData())))),3), round((mean(as.numeric(baseData()$`Engine load time`)) -(sd(as.numeric(baseData()$`Engine load time`))/sqrt(nrow(baseData())))),4), round((mean(as.numeric(baseData()$`Average speed driving (km/h)`))-(sd(as.numeric(baseData()$`Average speed driving (km/h)`))/sqrt(nrow(baseData())))),2), round((mean(as.numeric(baseData()$`Coasting time`)) -(sd(as.numeric(baseData()$`Coasting time`))/sqrt(nrow(baseData())))),4), round((mean(as.numeric(baseData()$`PTO time`))-(sd(as.numeric(baseData()$`PTO time`))/sqrt(nrow(baseData())))) ,4), round((mean(as.numeric(baseData()$`Idling time`))-(sd(as.numeric(baseData()$`Idling time`))/sqrt(nrow(baseData())))),4), round((mean(as.numeric(baseData()$`Within economy time`))-(sd(as.numeric(baseData()$`Within economy time`))/sqrt(nrow(baseData())))),4), round((mean(as.numeric(baseData()$`Above economy time`))-(sd(as.numeric(baseData()$`Above economy time`))/sqrt(nrow(baseData())))),4))
    tstmatxc[,6] = c(round(mean((as.numeric(baseData()$`Fuel Efficiency(Kmpl)`)) + (sd(as.numeric(baseData()$`Fuel Efficiency(Kmpl)`))/sqrt(nrow(baseData())))),3), round((mean(as.numeric(baseData()$`Engine load time`)) +(sd(as.numeric(baseData()$`Engine load time`))/sqrt(nrow(baseData())))),4), round((mean(as.numeric(baseData()$`Average speed driving (km/h)`))+(sd(as.numeric(baseData()$`Average speed driving (km/h)`))/sqrt(nrow(baseData())))),2), round((mean(as.numeric(baseData()$`Coasting time`)) +(sd(as.numeric(baseData()$`Coasting time`))/sqrt(nrow(baseData())))),4), round((mean(as.numeric(baseData()$`PTO time`))+(sd(as.numeric(baseData()$`PTO time`))/sqrt(nrow(baseData())))) ,4), round((mean(as.numeric(baseData()$`Idling time`))+(sd(as.numeric(baseData()$`Idling time`))/sqrt(nrow(baseData())))),4), round((mean(as.numeric(baseData()$`Within economy time`))+(sd(as.numeric(baseData()$`Within economy time`))/sqrt(nrow(baseData())))),4), round((mean(as.numeric(baseData()$`Above economy time`))+(sd(as.numeric(baseData()$`Above economy time`))/sqrt(nrow(baseData())))),4))
    return(tstmatxc)
  })
  
  # this builds the table which shows the ideal fe truck, site best fe truck and the selected truck. The 5th column is the RGB value selected on the basis of "less than", "equal to", and "greater than" ideal OR site best as per selected comparison. The 5 column is hidden.
  VehAnalytics = reactive({
    tstmatd = matrix(NA, 8, 5, dimnames = list(c("Fuel Efficiency(Kmpl)","Engine load time", "Average speed driving (km/h)", "Coasting time", "PTO time", "Idling time", "Within economy time", "Above economy time"), c("Benchmark truck", "Site best FE truck", "Site least FE truck", "Selected truck", "RGBval1")))
    tstmatd[,1] = c(1.39, 0.0300, 21, 0.086, 0.0415, 0.279, 0.192, 0.001 )
    tstmatd[,2] = c(round(mean(as.numeric(vehicleMaxDF()$`Fuel Efficiency(Kmpl)`)),3), round(mean(as.numeric(vehicleMaxDF()$`Engine load time`)),4), round(mean(as.numeric(vehicleMaxDF()$`Average speed driving (km/h)`)),2), round(mean(as.numeric(vehicleMaxDF()$`Coasting time`)),3), round(mean(as.numeric(vehicleMaxDF()$`PTO time`)),4), round(mean(as.numeric(vehicleMaxDF()$`Idling time`)),4), round(mean(as.numeric(vehicleMaxDF()$`Within economy time`)),4), round(mean(as.numeric(vehicleMaxDF()$`Above economy time`)),4))
    tstmatd[,3] = c(round(mean(as.numeric(vehicleMinDF()$`Fuel Efficiency(Kmpl)`)),3), round(mean(as.numeric(vehicleMinDF()$`Engine load time`)),4), round(mean(as.numeric(vehicleMinDF()$`Average speed driving (km/h)`)),2), round(mean(as.numeric(vehicleMinDF()$`Coasting time`)),3), round(mean(as.numeric(vehicleMinDF()$`PTO time`)),4), round(mean(as.numeric(vehicleMinDF()$`Idling time`)),4), round(mean(as.numeric(vehicleMinDF()$`Within economy time`)),4), round(mean(as.numeric(vehicleMinDF()$`Above economy time`)),4))
    tstmatd[,4] = c(round(mean(as.numeric(selectVehicleDF()$`Fuel Efficiency(Kmpl)`)),3), round(mean(as.numeric(selectVehicleDF()$`Engine load time`)),4), round(mean(as.numeric(selectVehicleDF()$`Average speed driving (km/h)`)),2), round(mean(as.numeric(selectVehicleDF()$`Coasting time`)),3), round(mean(as.numeric(selectVehicleDF()$`PTO time`)),4), round(mean(as.numeric(selectVehicleDF()$`Idling time`)),4), round(mean(as.numeric(selectVehicleDF()$`Within economy time`)),4), round(mean(as.numeric(selectVehicleDF()$`Above economy time`)),4))
    req(input$comChoice)
    if(input$comChoice == "Benchmark Truck" ){
      for(i in c(1,3,4)){
        if(tstmatd[i,4] < comparisonMatrix()[i,1]){
          tstmatd[i,5] = 1
        } else if((tstmatd[i,4] > comparisonMatrix()[i,1]) && (tstmatd[i,4] < comparisonMatrix()[i,2])){
          tstmatd[i,5] = 2
        } else {
          tstmatd[i,5] = 3
        }
      }
      for(i in c(2,5,6,7,8)){
        if(tstmatd[i,4] < comparisonMatrix()[i,1]){
          tstmatd[i,5] = 3
        } else if((tstmatd[i,4] > comparisonMatrix()[i,1]) && (tstmatd[i,4] < comparisonMatrix()[i,2])){
          tstmatd[i,5] = 2
        } else {
          tstmatd[i,5] = 1
        }
      }
    } else {
      for(i in c(1,3,4)){
        if(tstmatd[i,4] < comparisonMatrix()[i,3]){
          tstmatd[i,5] = 1
        } else if((tstmatd[i,4] > comparisonMatrix()[i,3]) && (tstmatd[i,4] < comparisonMatrix()[i,4])){
          tstmatd[i,5] = 2
        } else {
          tstmatd[i,5] = 3
        }
      }
      for(i in c(2,5,6,7,8)){
        if(tstmatd[i,4] < comparisonMatrix()[i,3]){
          tstmatd[i,5] = 3
        } else if((tstmatd[i,4] > comparisonMatrix()[i,3]) && (tstmatd[i,4] < comparisonMatrix()[i,4])){
          tstmatd[i,5] = 2
        } else {
          tstmatd[i,5] = 1
        }
      }
    }
    return(tstmatd)
  })
  
  #Disply the above data table
  output$selectedVehicleComparison = renderDataTable({
    datatable(VehAnalytics(), options = list(columnDefs = list(list(targets = 5, visible =FALSE)), dom = 't')) %>% formatStyle('Selected truck','RGBval1', backgroundColor = styleEqual(c(1,2,3), c('orange', 'yellow', 'green')))
  })
  
  #creating a sorted list of trucks and their FEs.From this table we will display the 5 best and the 5 worst FE truck
  sortedListDF = reactive({
    tempvtDF = vehicleFEtableDF()
    vehNum = length(tempvtDF$`Vehicle`)
    mt = matrix(NA,vehNum,2, dimnames = list(NULL, c("Truck", "Fuel Efficiency")))
    for(j in 1:vehNum){
      lstfe = round(tempvtDF[[1,2]],3)
      rowMin = 1
      for(i in 1:nrow(tempvtDF)){
        if(tempvtDF[[i,2]] < lstfe){
          mt[j,2] = round(tempvtDF[[i,2]],3)
          lstfe = round(tempvtDF[[i,2]],3)
          rowMin = i
        } else {
          mt[j,2] = lstfe
        }
      }
      mt[j,1] = tempvtDF[[rowMin,1]]
      tempvtDF = tempvtDF[-rowMin, ]
    }
    return(mt)
  })
  
  #Top 7
  bestFEVehlistDF = reactive({
    vehNum = nrow(sortedListDF())
    if(vehNum < 6) {
      sortedListDF()[vehNum:1,]
    } else {
      sortedListDF()[vehNum:(vehNum-6),]
    }
  })
  
  #Bottom 7
  leastFEVehlistDF = reactive({
    vehNum = nrow(sortedListDF())
    if(vehNum < 6) {
      sortedListDF()[1:vehNum,]
    } else {
      sortedListDF()[1:7,]
    }
  })
  
  #Diplaying top 7
  output$bestvehTab = renderDataTable({
    datatable(bestFEVehlistDF(), options = list(dom = 't'))
  })
  
  #displaying bottom 7
  output$leastvehTab = renderDataTable({
    datatable(leastFEVehlistDF(), options = list(dom = 't'))
  })
  
  vehicleFElessThanAvg = reactive({
    sortedListDFConv = data.frame(sortedListDF())
    sortedListDFConv %>% filter(`Fuel.Efficiency` < mean(baseData()$`Fuel Efficiency(Kmpl)`))
    colnames(sortedListDFConv) = c("Truck", "Fuel Efficiency")
    return(sortedListDFConv)
  })
  
  output$vehicleFElessThanAvgTab = renderDataTable({
    datatable(vehicleFElessThanAvg(), options = list(dom = 't'))
  })
  
  
  #Full vehicle summary table building
  vehFullSumDF = reactive({
    tm = baseData() %>% group_by(`Vehicle`) %>% summarise(mean(`Fuel Efficiency(Kmpl)`), mean(`Engine load time`), mean(`Average speed driving (km/h)`), mean(`Coasting time`), mean(`PTO time`), mean(`Idling time`), mean(`Within economy time`), mean(`Above economy time`))
    colnames(tm) = c("Vehicle", "mean Fuel Efficiency", "mean Engine load", "mean Average Speed", "mean Coasting time", "mean PTO time", "mean Idling time", "mean Within Econ", "mean Above economy time")
    tm = cbind(tm[,1], round(tm[,2:9], 4))
  })
  
  #Taking the right column from the comparison matrix based on the selection
  k = reactive({
    if(input$comparisonSelect == "Benchmark truck" ){
      1
    } else if(input$comparisonSelect == "Site best truck"){
      3
    } else{
      5
    }
  })
  
  #All vehicles comparison table
  output$fullVehTab = renderDataTable({
    datatable(vehFullSumDF(), options = list(dom = 'lBrtip', paging = FALSE)) %>%
      formatStyle('mean Fuel Efficiency', backgroundColor = styleInterval(c(comparisonMatrix()[[1,k()]],comparisonMatrix()[[1,k()+1]]), c("orange", "yellow","green"))) %>%
      formatStyle('mean Engine load', backgroundColor = styleInterval(c(comparisonMatrix()[[2,k()]],comparisonMatrix()[[2,k()+1]]), c("green", "yellow","orange"))) %>%
      formatStyle('mean Average Speed', backgroundColor = styleInterval(c(comparisonMatrix()[[3,k()]],comparisonMatrix()[[3,k()+1]]), c("orange", "yellow","green"))) %>%
      formatStyle('mean Coasting time', backgroundColor = styleInterval(c(comparisonMatrix()[[4,k()]],comparisonMatrix()[[4,k()+1]]), c("orange", "yellow","green"))) %>%
      formatStyle('mean PTO time', backgroundColor = styleInterval(c(comparisonMatrix()[[5,k()]],comparisonMatrix()[[5,k()+1]]), c("green", "yellow","orange"))) %>%
      formatStyle('mean Idling time', backgroundColor = styleInterval(c(comparisonMatrix()[[6,k()]],comparisonMatrix()[[6,k()+1]]), c("green", "yellow","orange"))) %>%
      formatStyle('mean Within Econ', backgroundColor = styleInterval(c(comparisonMatrix()[[7,k()]],comparisonMatrix()[[7,k()+1]]), c("green", "yellow","orange"))) %>%
      formatStyle('mean Above economy time', backgroundColor = styleInterval(c(comparisonMatrix()[[8,k()]],comparisonMatrix()[[8,k()+1]]), c("green", "yellow","orange")))
  })
  
  #Report card Tab
  
  output$ChassisChoiceRC = renderUI({
    req(baseData())
    selectInput(inputId = "vehIdRC", label = "Chooose Chassis", choices = chList())
  })
  
  selectVehicleDFRC = reactive({
    baseData() %>% filter(baseData()[,1] == input$vehIdRC)
  })
  
  vehicleHeaderDfRC = reactive({
    mt = matrix(NA, 3, 1, dimnames = list(c("Vehicle", "Driver", "Site"), NULL))
    mt[1,1] = input$vehIdRC
    mt[2,1] = ""
    mt[3,1] = ""
    return(mt)
  })
  
  output$vehicleHeaderRC = renderDataTable({
    datatable(vehicleHeaderDfRC(), colnames = '', options = list(dom = 't', bSort = FALSE))
  })
  
  chosenTruckTextRC = reactive({
    input$vehIdRC
  })
  
  #creating a table which will have average FE values against each vehicle in the fleet
  vehicleFEtableDFRC = reactive({
    tm = baseData() %>% group_by(`Vehicle`) %>% summarise(mean(`Fuel Efficiency(Kmpl)`))
    colnames(tm) = c("Vehicle", "Average_FE")
    data.frame(tm)
  })
  
  #creating a table which will have average FE values VS Eng load against each vehicle in the fleet
  vehicleFEvsEngLoadTableDFRC = reactive({
    tm = baseData() %>% group_by(`Vehicle`) %>% summarise(mean(`Fuel Efficiency(Kmpl)`), mean(`Engine load time`))
    colnames(tm) = c("Vehicle", "Average_FE", "Average_engLoad")
    data.frame(tm)
  })
  
  #creating a table which will have average FE values VS coasting time against each vehicle in the fleet
  vehicleFEvsCoasttimeTableDFRC = reactive({
    tm = baseData() %>% group_by(`Vehicle`) %>% summarise(mean(`Fuel Efficiency(Kmpl)`), mean(`Coasting time`))
    colnames(tm) = c("Vehicle", "Average_FE", "Average_CostingTime")
    data.frame(tm)
  })
  
  #creating a table which will have average FE values VS Above Economy against each vehicle in the fleet
  vehicleFEvsAbvecontimeTableDFRC = reactive({
    tm = baseData() %>% group_by(`Vehicle`) %>% summarise(mean(`Fuel Efficiency(Kmpl)`), mean(`Above economy time`))
    colnames(tm) = c("Vehicle", "Average_FE", "Average_abvecontime")
    data.frame(tm)
  })
  
  #creating a sorted list of trucks and their FEs.
  sortedListDFRC = reactive({
    tempvtDF = vehicleFEtableDFRC()
    vehNum = length(tempvtDF$`Vehicle`)
    mt = matrix(NA,vehNum,2, dimnames = list(NULL, c("Truck", "Fuel Efficiency")))
    for(j in 1:vehNum){
      lstfe = round(tempvtDF[[1,2]],3)
      rowMin = 1
      for(i in 1:nrow(tempvtDF)){
        if(tempvtDF[[i,2]] < lstfe){
          mt[j,2] = round(tempvtDF[[i,2]],3)
          lstfe = round(tempvtDF[[i,2]],3)
          rowMin = i
        } else {
          mt[j,2] = lstfe
        }
      }
      mt[j,1] = tempvtDF[[rowMin,1]]
      tempvtDF = tempvtDF[-rowMin, ]
    }
    return(mt)
  })
  
  #Finding the truck with max Fe
  vehicleMaxAvgFERC = reactive({
    vehicleFEtableDFRC()[which.max(vehicleFEtableDFRC()$`Average_FE`), 1]
  })
  
  #creating a separate DF for all the line items corresponding to the max FE truck
  vehicleMaxDFRC = reactive({
    baseData() %>% filter(baseData()[,1] == vehicleMaxAvgFERC())
  })
  
  #identyfing the median value of FE. Median depends on if we have odd or even number of records
  vehicleMedianAvgFERC = reactive({
    if(((nrow(sortedListDFRC())) %% 2)==1){
      sortedListDFRC()[((nrow(sortedListDFRC()) / 2) + 1), 1]
    } else {
      sortedListDFRC()[(nrow(sortedListDFRC()) / 2), 1]
    }
  })
  
  vehicleMedianDFRC = reactive({
    baseData() %>% filter(baseData()[,1] == vehicleMedianAvgFERC())
  })
  
  driverPerformanceDFRC = reactive({
    mt = matrix(NA, 8, 6, dimnames = list(NULL, c("DRIVER PERFORMANCE", chosenTruckTextRC(), "Best Vehicle", "Site Average", "Trend", "RGB")))
    mt[,1] = c("Fuel Efficiency(Kmpl)","Engine load time", "Average speed driving (km/h)", "Coasting time", "PTO time", "Idling time", "Within economy time", "Above economy time")
    mt[,2] = c(round(mean(as.numeric(selectVehicleDFRC()$`Fuel Efficiency(Kmpl)`)),3), round(mean(as.numeric(selectVehicleDFRC()$`Engine load time`)),4), round(mean(as.numeric(selectVehicleDFRC()$`Average speed driving (km/h)`)),2), round(mean(as.numeric(selectVehicleDFRC()$`Coasting time`)),2), round(mean(as.numeric(selectVehicleDFRC()$`PTO time`)),4), round(mean(as.numeric(selectVehicleDFRC()$`Idling time`)),4), round(mean(as.numeric(selectVehicleDFRC()$`Within economy time`)),4), round(mean(as.numeric(selectVehicleDFRC()$`Above economy time`)),4))
    mt[,3] = c(round(mean(as.numeric(vehicleMaxDFRC()$`Fuel Efficiency(Kmpl)`)),3), round(mean(as.numeric(vehicleMaxDFRC()$`Engine load time`)),4), round(mean(as.numeric(vehicleMaxDFRC()$`Average speed driving (km/h)`)),2), round(mean(as.numeric(vehicleMaxDFRC()$`Coasting time`)),3), round(mean(as.numeric(vehicleMaxDFRC()$`PTO time`)),4), round(mean(as.numeric(vehicleMaxDFRC()$`Idling time`)),4), round(mean(as.numeric(vehicleMaxDFRC()$`Within economy time`)),4), round(mean(as.numeric(vehicleMaxDFRC()$`Above economy time`)),4))
    mt[,4] = c(round(mean(as.numeric(vehicleMedianDFRC()$`Fuel Efficiency(Kmpl)`)),3), round(mean(as.numeric(vehicleMedianDFRC()$`Engine load time`)),4), round(mean(as.numeric(vehicleMedianDFRC()$`Average speed driving (km/h)`)),2), round(mean(as.numeric(vehicleMedianDFRC()$`Coasting time`)),3), round(mean(as.numeric(vehicleMedianDFRC()$`PTO time`)),4), round(mean(as.numeric(vehicleMedianDFRC()$`Idling time`)),4), round(mean(as.numeric(vehicleMedianDFRC()$`Within economy time`)),4), round(mean(as.numeric(vehicleMedianDFRC()$`Above economy time`)),4))
    for(i in c(1,3,4)){
      if(mt[i,2] < (as.numeric(mt[i,4]) * 0.99)){
        mt[i,5] = as.character(icon("arrow-down", lib = "glyphicon"))
      }else if((mt[i,2] > (as.numeric(mt[i,4]) * 0.99)) && (mt[i,2] < (as.numeric(mt[i,4]) * 1.01))){
        mt[i,5] = "Similar to Avg"
        
      } else {
        mt[i,5] = as.character(icon("arrow-up", lib = "glyphicon"))
      }
    }
    for(i in c(2,5,6,7,8)){
      if(mt[i,2] < (as.numeric(mt[i,4]) * 0.99)){
        mt[i,5] = as.character(icon("arrow-up", lib = "glyphicon"))
      }else if((mt[i,2] > (as.numeric(mt[i,4]) * 0.99)) && (mt[i,2] < (as.numeric(mt[i,4]) * 1.01))){
        mt[i,5] = "Similar to Avg"
      } else {
        mt[i,5] = as.character(icon("arrow-down", lib = "glyphicon"))
      }
    }
    for(i in c(1,3,4)){
      if(mt[i,2] < (as.numeric(mt[i,4]) * 0.99)){
        mt[i,6] = 1
      }else if((mt[i,2] < (as.numeric(mt[i,4]) * 1.01)) && (as.numeric(mt[i,2]) > (as.numeric(mt[i,4]) * 0.99))){
        mt[i,6] = 2
      } else {
        mt[i,6] = 3
      }
    }
    for(i in c(2,5,6,7,8)){
      if(mt[i,2] < (as.numeric(mt[i,4]) * 0.99)){
        mt[i,6] = 3
      }else if((mt[i,2] < (as.numeric(mt[i,4]) * 1.01)) && (as.numeric(mt[i,2]) > (as.numeric(mt[i,4]) * 0.99))){
        mt[i,6] = 2
      } else {
        mt[i,6] = 1
      }
    }
    return(mt)
  })
  
  output$driverPerformanceChartRC = renderDataTable({
    datatable(driverPerformanceDFRC(), options = list(columnDefs = list(list(targets = 5, visible =FALSE)), dom = 't', bSort = FALSE), escape = FALSE)
  })
  
  # adding the 3 charts
  output$engLoadXYplotRC = renderPlot({
    ggplot(data = vehicleFEvsEngLoadTableDFRC(), aes(x = `Average_engLoad`, y = `Average_FE`)) + labs(title="FE Vs Engine Load Time", x="Avg % Engine Load Time", y="Avg Fuel Efficiency(Kmpl)") + geom_point(color = "steelblue", size = 3) +  ylim(c(0,2)) + geom_smooth(method = lm, size = 0.8, col = 3, fill = "lightblue") + geom_point(data = vehicleFEvsEngLoadTableDFRC()[vehicleFEvsEngLoadTableDFRC()$`Vehicle`==input$vehIdRC,],color = "red", size = 4) 
  })
  
  output$coastingXYplotRC = renderPlot({
    ggplot(data = vehicleFEvsCoasttimeTableDFRC(), aes(x = `Average_CostingTime`, y = `Average_FE`)) + labs(title="FE Vs Coasting", x="Avg % Coasting", y="Avg Fuel Efficiency(Kmpl)") + geom_point(color = "steelblue", size = 3) +  ylim(c(0,2)) + geom_smooth(method = lm, size = 0.8, col = 3, fill = "lightblue") + geom_point(data = vehicleFEvsCoasttimeTableDFRC()[vehicleFEvsCoasttimeTableDFRC()$`Vehicle`==input$vehIdRC,],color = "red", size = 4)
  })
  
  output$abvEconXYplotRC = renderPlot({
    ggplot(data = vehicleFEvsAbvecontimeTableDFRC() , aes(x = `Average_abvecontime`, y = `Average_FE`)) + labs(title="FE Vs Above Economy Driving", x="Avg % Above Economy Time", y="Avg Fuel Efficiency(Kmpl)") + geom_point(color = "steelblue", size = 3) + ylim(c(0,2)) + geom_smooth(method = lm, size = 0.8, col = 3, fill = "lightblue") + geom_point(data = vehicleFEvsAbvecontimeTableDFRC()[vehicleFEvsAbvecontimeTableDFRC()$`Vehicle`==input$vehIdRC,],color = "red", size = 4)
  })
  
  #Build the comments table
  fuelAdviceTableDFRC = reactive({
    # here m is the comments table with comments positive, negative and neutral for all the 7 parameters
    m = matrix(NA, 7, 3, dimnames = list(c("Engine load time", "Average speed driving (km/h)", "Coasting time", "PTO time", "Idling time", "Within economy time", "Above economy time"), c(1,2,3)))
    m[,1] = c("You could improve your usage of gears and engine rpm. Optimise the selected gear based on driving condition and don't forget to be gentle on the accelerator pedal.<br/>Always start in the lowest gear with the lowest possible engine speed. Do not raise Engine rpm above 1100 while starting to move the vehicle from loading point."," Preselect the right gear while climbing gradients to prevent breakage of momentum.", "Your anticipation could be better, coast downhill to save fuel<br/>On downhill gradients you should try to avoid accelerating, and instead allow the truck to increase speed by coasting. Regulate the downhill speed with the truck's auxiliary brakes. Make it a habit to completely remove your foot from the accelerator pedal while you are coasting.","PTO usage is on the higher side.", "Too much idling, engine can be allowed to shut off after 2.5 minutes of idling if the que is long.", "When driving within the Green Economy band, try to utilise max engine torque within 1300 rpm band.", "Your Above economy % is on the higher side. Be gentler on the accelerator pedal.<br/>Maintain the engine speed within the green zone on the gauge and use the truck's pulling power at low rev's.")
    m[,2] = c("You could improve your usage of gears and engine rpm. Optimise the selected gear based on driving condition and don't forget to be gentle on the accelerator pedal.<br/>Always start in the lowest gear with the lowest possible engine speed.", "Preselect the right gear while climbing gradients to prevent breakage of momentum", "Coasting imporves fuel consumption hence anticipation of stopping distance and releasing accelerator more often can be practised.", "PTO function is ok, however check for possibilities to improve this.", "Look for opportunities to allow the engine to shut off after 2.5 minutes when it is safe and possible to do so.","When driving within the Green Economy band, try to utilise max engine torque within 1300 rpm band.", "Driving style can be more gentler, do not operate above the green band.<br/>Maintain the engine speed within the green zone on the gauge and use the truck's pulling power at low rev's.")
    m[,3] = c("Good! You use the Engine power & torque effectively. Even though you should consider optimising even more to save a few more litres of fuel.", "Good! You adapt to speed very well.", "Usage of coasting is good, however look for possibilities to improve this further. Anticipate the braking distance and take your foot off the accelerator pedal while slowing down or stopping.", "PTO function is ok.", "Your Idling performance is Good! Look for opportunities to allow the engine to shut off after 2.5 minutes when it is safe and possible to do so.", "When driving within the Green Economy band, try to utilise max engine torque within 1300 rpm band.", "You practise a gentle driving style, which is good for the drivetrain and helps save a few litres in the process.")
    n = matrix(NA, 4, 1, dimnames = list(c("Engine & Gear Utilization", "Speed Adaptation", "Anticipation & Braking", "Standstill"), c("Recommendations")))
    n[1,1] = paste(m[[1,as.numeric(driverPerformanceDFRC()[[1,6]])]],m[[7,as.numeric(driverPerformanceDFRC()[[7,6]])]], sep = "<br/>")
    n[2,1] = paste(m[[2,as.numeric(driverPerformanceDFRC()[[2,6]])]],m[[6,as.numeric(driverPerformanceDFRC()[[6,6]])]], sep = "<br/>")
    n[3,1] = m[[3,as.numeric(driverPerformanceDFRC()[[3,6]])]]
    n[4,1] = paste(m[[4,as.numeric(driverPerformanceDFRC()[[4,6]])]],m[[5,as.numeric(driverPerformanceDFRC()[[5,6]])]], sep = "<br/>")
    return(n)
  })
  
  output$fuelAdviceTableRC = renderDataTable({
    datatable(fuelAdviceTableDFRC(),colnames = '', options = list(dom = 't', bSort = FALSE), escape = FALSE)
  })
  
  #FINAL Report card UI
  output$reportCardUI = renderUI({
    req(input$vehIdRC)
    wellPanel(
      fluidRow(
        column(4,h3(strong("Driver Fuel Report")), dataTableOutput("vehicleHeaderRC")), column(8,div(img(src = "Volvo.jpg", width = 75, height = 75), style = "text-align:right;"))
      ),hr(),
      fluidRow(
        dataTableOutput("driverPerformanceChartRC")
      ),hr(),
      fluidRow(
        column(4,plotOutput("engLoadXYplotRC")), column(4,plotOutput("coastingXYplotRC")), column(4,plotOutput("abvEconXYplotRC"))
      ), hr(),
      fluidRow(
        h4(img(src = "advice.png", width = 50, height = 50), "Fuel Advice")
      ),
      fluidRow(
        dataTableOutput("fuelAdviceTableRC")
      )
    )
  })
  
  observeEvent(input$print, {
    js$winprint()
  })
  
  
  output$fleetAnalyticsTab = renderUI({
    req(input$rawFile)
    fluidRow(
      wellPanel(
        h3("Fleet performance"),
        fluidRow(column(6,dataTableOutput("siteSummaryTable")), column(3,h4(strong("Top FE trucks")), dataTableOutput("bestvehTab")), column(3,h4(strong("Least FE trucks")), dataTableOutput("leastvehTab"))),
        hr(), h3("Factors influencing FE"), uiOutput("siteFEVariance")
      ),hr(),
      wellPanel(
        h3("Vehicle Performance Summary"),
        fluidRow(column(3,uiOutput("fleetChassisChoice")), column(3,), column(3,),column(3,uiOutput("comparisonChoice"))),
        fluidRow(dataTableOutput("selectedVehicleComparison"))
      ),
      wellPanel(h3("Fleet Performance Summary"),radioButtons(inputId = "comparisonSelect", label = "Choose comparison", choices = c("Benchmark truck","Site best truck", "Average values"), inline = TRUE), dataTableOutput("fullVehTab")),
      wellPanel(
        h3("Fuel Efficiency Forecast"),
        fluidRow(
          column(3, uiOutput("engLoadChoice")), column(3,uiOutput("avgSpdChoice")), column(3, uiOutput("coastTimeChoice")), column(3, uiOutput("PTOtimeChoice"))
        ),
        fluidRow(
          column(3, uiOutput("idleTimeChoice")), column(3,uiOutput("econTimeChoice")), column(3, uiOutput("aboveEconTimeChoice"))
        ),
        fluidRow(wellPanel(h4("Fuel Efficiency range (80% accurate only)"),textOutput("FEtext")))
      )
    )
  })
  
  output$plotsTab = renderUI({
    req(input$rawFile)
    fluidRow(
      wellPanel(
        h3("Standard X-Y plots"),
        fluidRow(
          fluidRow(box(plotOutput("engLoadXYplot"), width = 4), box(plotOutput("avgSpeedXYplot"), width = 4),box(plotOutput("coastingXYplot"), width = 4)), fluidRow(box(plotOutput("idlingXYplot"), width = 4), box(plotOutput("ptoTimeXYplot"), width = 4), box(plotOutput("abvEconXYplot"), width = 4))
        )
      ),
      wellPanel(
        fluidRow(column(3,uiOutput("chassisChoice1")), column(9,)), HTML("<br>"),
        fluidRow(
          box(fluidRow(box(plotOutput("TimeSeriesFE"), width = 12)), fluidRow(box(plotOutput("TimeSeriesEngLoad"), width = 12)), fluidRow(box(plotOutput("TimeSeriesAboveEcon"), width = 12)), width = 12)
        )
      )
    )
  })
  
  output$reportcardTab = renderUI({
    req(input$rawFile)
    fluidRow(
      wellPanel(
        uiOutput("ChassisChoiceRC")
      ),
      uiOutput("reportCardUI"),
      useShinyjs(),
      extendShinyjs(text = jsCode, functions = c("winprint")),
      actionButton("print", "PRINT")
    )
  })
  
  
  #BUILD FOR COMPARISON TAB
  
  #Base data before 
  baseDataBefore = reactive({
    req(input$rawFile1)
    tryCatch(
      {
        df = lapply(input$rawFile1$datapath, function(i){
          x = read_excel(i, sheet = 1, col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "date"))
          return(x)
        })
        df = do.call("rbind.data.frame", df)
        df['Fuel Efficiency(Kmpl)'] = (df[,3] / df[,4])
        df = df[, colnames(df)[c(1,2,3,4,27,5:26)]]
        df = df %>% filter(`Total distance (km)` > 10)
        df = df %>% filter(`PTO time` > 0 )
      },
      error = function(e){
        stop(safeError(e))
      }
    )
  })
  
  #base data after
  baseDataAfter = reactive({
    req(input$rawFile2)
    tryCatch(
      {
        df = lapply(input$rawFile2$datapath, function(i){
          x = read_excel(i, sheet = 1, col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "date"))
          return(x)
        })
        df = do.call("rbind.data.frame", df)
        df['Fuel Efficiency(Kmpl)'] = (df[,3] / df[,4])
        df = df[, colnames(df)[c(1,2,3,4,27,5:26)]]
        df = df %>% filter(`Total distance (km)` > 10)
        df = df %>% filter(`PTO time` > 0 )
      },
      error = function(e){
        stop(safeError(e))
      }
    )
  })
  
  #Building table of average summary values against each vehicle in the 'before'data
  file_before_sumDF = reactive({
    baseDataBefore() %>% group_by(`Vehicle`) %>% summarise(mean(`Fuel Efficiency(Kmpl)`))
  })
  
  #Building table of average summary values against each vehicle in the 'after'data
  file_after_sumDF = reactive({
    baseDataAfter() %>% group_by(`Vehicle`) %>% summarise(mean(`Fuel Efficiency(Kmpl)`))
  }) 
  
  #Building table comparing the 'before'and 'after'FE
  file_comparisonDF = reactive({
    tm = file_before_sumDF()
    tm$nv = 0
    tm$nvb = 0
    colnames(tm) = c("Vehicle", "FE before", "FE after", "% change")
    for(i in 1:nrow(file_before_sumDF())){
      for(j in 1:nrow(file_after_sumDF())){
        if(file_after_sumDF()[[j,1]] == file_before_sumDF()[[i,1]]){
          tm[[i,3]] = file_after_sumDF()[[j,2]]
          break
        } else {
          tm[[i,3]] = NA
        }
      }
    }
    tm[,4] = (((tm[,3] - tm[,2])/tm[,2]) * 100)
    tm = cbind(tm[,1], round(tm[,2],2), round(tm[,3], 2), round(tm[,4],2))
    return(tm)
  })
  
  #Displaying the above table as a datatable with the correct formatting
  output$comparisonTable= renderDataTable({
    datatable(file_comparisonDF(), options = list(dom = 'lBrtip', paging = FALSE)) %>%
      formatStyle('% change', backgroundColor = styleInterval(0, c("orange", "green")))
  })
  
  #buidling a table comparing 'before'/ 'after'for all parameters
  parameterCompDF = reactive({
    t = matrix(NA, 8, 3, dimnames = list(c("Fuel Efficiency(Kmpl)","Engine load time", "Average speed driving (km/h)", "Coasting time", "PTO time", "Idling time", "Within economy time", "Above economy time"), c("Before","After","change")))
    t[,1] = c(round(mean(as.numeric(baseDataBefore()$`Fuel Efficiency(Kmpl)`)),3), round(mean(as.numeric(baseDataBefore()$`Engine load time`)),4), round(mean(as.numeric(baseDataBefore()$`Average speed driving (km/h)`)),2),round(mean(as.numeric(baseDataBefore()$`Coasting time`)),4), round(mean(as.numeric(baseDataBefore()$`PTO time`)),4), round(mean(as.numeric(baseDataBefore()$`Idling time`)),4), round(mean(as.numeric(baseDataBefore()$`Within economy time`)),4),round(mean(as.numeric(baseDataBefore()$`Above economy time`)),4))
    t[,2] = c(round(mean(as.numeric(baseDataAfter()$`Fuel Efficiency(Kmpl)`)),3), round(mean(as.numeric(baseDataAfter()$`Engine load time`)),4), round(mean(as.numeric(baseDataAfter()$`Average speed driving (km/h)`)),2),round(mean(as.numeric(baseDataAfter()$`Coasting time`)),4), round(mean(as.numeric(baseDataAfter()$`PTO time`)),4), round(mean(as.numeric(baseDataAfter()$`Idling time`)),4), round(mean(as.numeric(baseDataAfter()$`Within economy time`)),4),round(mean(as.numeric(baseDataAfter()$`Above economy time`)),4))
    t[,3] = round((((t[,2] - t[,1])/t[,1]) * 100),2)
    t = cbind(t[,1], t[,2], t[,3])
    colnames(t) = c("Before","After","% change")
    return(t)
  })
  
  output$parameterCompTable = renderDataTable({
    datatable(parameterCompDF(), options = list(dom = 't', paging = FALSE)) 
  })
  
  feChange = reactive({
    round(as.numeric((sum(baseDataAfter()$`Total distance (km)`)/sum(baseDataAfter()$`Total fuel (l)`)) - (sum(baseDataBefore()$`Total distance (km)`)/sum(baseDataBefore()$`Total fuel (l)`))),5)
  })
  
  #create the text for FE change 
  feChangeText = reactive({
    h4("Fuel Efficiency Change:", feChange())
  })
  
  #create UI for text 'FE change:' 
  output$FEchangeUI = renderUI({
    return(feChangeText())
  })
  
  output$compFullUIelement = renderUI({
    req(input$rawFile1)
    req(input$rawFile2)
    wellPanel(
      h3("Key vehicle Paramters"),
      fluidRow(
        column(9,dataTableOutput("parameterCompTable")),column(3,)
      ),
      hr(),
      h3("Vehicle wise FE change"),
      fluidRow(
        column(9,dataTableOutput("comparisonTable")), column(3,)
      ),
      hr(),
      h3("FE advantage"),uiOutput("FEchangeUI"),
      fluidRow(
        column(2, numericInput(inputId = "distPerDayPerTruck", label = "Distance travelled(/day /truck)", value = 1, min = 1, max = 5000)), column(2, numericInput(inputId = "numberTruck", label = "Number of trucks", value = 1, min = 1, max = 1000)), column(2, numericInput(inputId = "numberDays", label = "Number of days", value = 1, min = 1, max = 365)), column(6,)
      ),
      h3("Total Fuel savings(Litres)"),uiOutput("fuelSavingsUI")
      
    )
    
  })
  
  fuelSavings = reactive({
    round(((sum(baseDataBefore()$`Total fuel (l)`)/sum(baseDataBefore()$`Total distance (km)`)) * (input$distPerDayPerTruck) * (input$numberTruck) * (input$numberDays))- ((sum(baseDataAfter()$`Total fuel (l)`)/sum(baseDataAfter()$`Total distance (km)`)) * (input$distPerDayPerTruck) * (input$numberTruck) * (input$numberDays)),4)
    #round((as.numeric((sum(baseDataAfter()$`Total fuel (l)`)/sum(baseDataAfter()$`Total distance (km)`)) - (sum(baseDataBefore()$`Total fuel (l)`)/sum(baseDataBefore()$`Total distance (km)`)))) * (input$distPerDayPerTruck) * (input$numberTruck) * (input$numberDays),2)
  })
  
  fuelSavingsText = reactive({
    h4("Total fuel savings:", fuelSavings())
  })
  
  output$fuelSavingsUI = renderUI({
    return(fuelSavingsText())
  })
}

shinyApp(ui, server)