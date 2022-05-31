################################################################
# COVID19 - COLOMBIA - 2020                                    #
################################################################
##### Install and / or load the required packages #############


library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(treemap)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(GGally)



##### DATA SOURCE - COLOMBIAN HEALTH MINISTER ######################
####  https://www.ins.gov.co/Noticias/Paginas/Coronavirus.aspx  ###
###################################################################

### read from local files ##############################

Covid <- read.csv("DateCasesApril_13_COVID19.csv",header = TRUE)


############################################################
#  FUNCTIONS                                               #
############################################################

cleandata <- function(data){
  
  #### Adjust variables to use - Columns 1 to 9 ###
  
  CData <- data[,2:9]
  
 
  ### convert all blank spaces in NA and eliminate all NAs with "complete.cases"###

  CData[CData == ""] <- NA
  Cdata <- na.omit(CData)
  return(Cdata)
  
  
}

prediction <- function(data_desc,Covid_Test,Datatest) {
  
  
  
  Covid_acum <- data_desc %>%
    group_by(data_desc$Date) 	%>%
    tally()
  
  
  ## Prediction data frame for real cases  ##
  
  Covid_acum$`data_desc$Date` <- as.Date(Covid_acum$`data_desc$Date`)
  Numeric_date <- as.numeric(Covid_acum$`data_desc$Date`)
  lm_Covid_acum <- lm(formula = Covid_acum$n ~ Numeric_date)
  Predict_lm_Covid_acum <- predict(lm_Covid_acum, interval = "prediction")
  Data_predict_lm_Covid_acum <- cbind(Covid_acum,Predict_lm_Covid_acum)
  colnames(Data_predict_lm_Covid_acum)[1] <- "Date"
  
  ### Prediction data frame for new cases and dates forecast ###
  ### Numeric_date is the number equivalent range for prediction data dates
  
  E_Predict_lm_Covid_acum <- predict(lm_Covid_acum, data.frame(Numeric_date = c(18365:18400)), interval = "prediction")
  E_Predict_lm_Covid_acum <- as.data.frame(E_Predict_lm_Covid_acum)
  Date <- c(1:36)
  Date <- as.Date(Date, origin = "2020-04-13")
  fit   <- E_Predict_lm_Covid_acum$fit
  lwr <- E_Predict_lm_Covid_acum$lwr
  upr <- E_Predict_lm_Covid_acum$upr
  E_Data_predict_lm_Covid_acum <- data.frame(Date,fit,lwr,upr) ## New predict data frame
  
  #### arrange and merge the two data sets 1.real data - 2. forecast data #####
  
  Data_pred_plot_1 <-Data_predict_lm_Covid_acum[,c(1,2,3)]
  n_pred <- E_Data_predict_lm_Covid_acum$fit
  Data_pred_plot_2 <- E_Data_predict_lm_Covid_acum[,c(1,2)]
  Data_pred_plot_2$n <- n_pred
  colnames(Data_pred_plot_1)[1] <- "Date"
  Data_pred_plot_1$Date <- as.Date(Data_pred_plot_1$Date)
  colnames(Data_pred_plot_2)[1] <- "Date"
  Data_pred_plot_1$n <- as.numeric(Data_pred_plot_1$n)
  colnames(Data_pred_plot_2)[2] <- "n"
  colnames(Data_pred_plot_2)[3] <- "fit"
  
  ### Build and plot the new data frame with real and forecast data ####
  
  Data_forecast_plot <- rbind(Data_pred_plot_1,Data_pred_plot_2)
  Numeric_date <- as.numeric(Data_forecast_plot$Date) 
  F_lm_Covid_acum <- lm(formula = Data_forecast_plot$n ~ Numeric_date)
  F_Predict_lm_Covid_acum <- predict(F_lm_Covid_acum, interval = "prediction")
  F_Data_predict_lm_Covid_acum <- cbind(Data_forecast_plot,F_Predict_lm_Covid_acum)
  colnames(F_Data_predict_lm_Covid_acum)[3] <- "Firstfit"
  rownames(F_Data_predict_lm_Covid_acum) <- NULL ## redefine data frame indices
  
  
  if (Datatest == "F") {return(F_Data_predict_lm_Covid_acum)}
   else{
     
     Pred_T_Test <- Covid_Test %>%
       group_by(Covid_Test$Date) 	%>%
       tally()
     
     ### create data set with real old data and real new "test data"
     
     Pred_T_Test_n <- numeric(72)
     Pred_T_Test_n[1:40] <- F_Data_predict_lm_Covid_acum$n[1:40]
     Pred_T_Test_n[41:72]<- Pred_T_Test$n[1:31]
     Pred_T_Date <- F_Data_predict_lm_Covid_acum$Date
     Pred_T_fit <- F_Data_predict_lm_Covid_acum$fit
     Pred_T_lwr <- F_Data_predict_lm_Covid_acum$lwr
     Pred_T_upr <- F_Data_predict_lm_Covid_acum$upr
     
     Pred_Test <- data.frame(Pred_T_Date,Pred_T_fit,Pred_T_lwr,Pred_T_upr,Pred_T_Test_n)
     
     return(Pred_Test)
     
   }
  
}
  
  desc_plot <- function(var){

    data_desc <- cleandata(Covid)
    
    if (var == "TreeMap") {
      
      ## Acum By State and City
      
      Covid_acum <- data_desc %>%
        group_by(data_desc$State,data_desc$City) %>%
        tally()
      
      
      #### Add number label to work with the treeMap
      
      colnames(Covid_acum) <- c("State","City","n")
      Covid_acum$label_city <- paste(Covid_acum$City, Covid_acum$n, sep = "-")
    
        
        D_Plot <-treemap(Covid_acum,index=c("State","label_city"), vSize = "n",vColor = "n",type = "value",
                         fontsize.labels = c(20,15),align.labels = list(c("center","center"),c("left","top")),
                         lowebound.cex.labels = 2, title = "Acum. No.of CASES by State by City (Colombia) - APRIL 13 - 2020", fontsize.title = 20)
        
                             }
    
        if  (var == "Place") {
      
      
      Covid_acum <- data_desc %>%
        group_by(data_desc$Date,data_desc$Place) 	%>%
        tally()
      
        Covid_acum$`data_desc$Date` <- as.Date(Covid_acum$`data_desc$Date`)
        D_Plot <- ggplot(Covid_acum, aes(`data_desc$Date`,Covid_acum$n,color = `data_desc$Place`)) + geom_line(size=2) + labs(title = "Cases by Date and Place (Colombia) - 2020", colour ="Place") + xlab("Date") +ylab("Number of cases") 
                            }
    
    if (var == "Gender") {
      
        ### COVID_Analysis_by_Date By Sex (Bar) ####
        data_desc$Date <- as.Date(data_desc$Date)
        D_Plot <- ggplot(data_desc,aes(Date,fill=Sex)) + geom_bar() + labs(title = "Histogram by Date and Sex (Colombia)", colour = "Gender") + xlab("Date") + ylab("Number of cases")
                          }
    
        if ( var == "Age") {
      
        data_desc$Date <- as.Date(data_desc$Date)
        D_Plot <- ggplot(data_desc, aes(Age)) + geom_bar(fill = "purple") + labs(title = "Covid Cases by Age (Colombia) - Acum - April 13 2020") + xlab("Age") + ylab("Number of cases")
                           }
    
    if (var == "Cor") {
      
      Covid_acum <- data_desc %>%
        group_by(data_desc$Date,data_desc$Age) 	%>%
        tally()
      
        Covid_acum$`data_desc$Date` <- as.Date(Covid_acum$`data_desc$Date`)
        D_Plot <- ggpairs(data = Covid_acum, columns=1:2,ggplot2::aes(colour='data_desc$Age'),title="Correlation:  Date vs Age (Colombia - Acum - April 13 2020)", columnLabels = c("Date","Age"))
       
                     }
    
        if (var == "Date") {
      
      Covid_acum <- data_desc %>%
        group_by(data_desc$Date) 	%>%
        tally()
      
        Covid_acum$`data_desc$Date` <- as.Date(Covid_acum$`data_desc$Date`)
        D_Plot <- ggplot(Covid_acum, aes(`data_desc$Date`,n, color = n)) + geom_line() + labs(title = "Total Cases COVID-19 by Date (2020)")+ 	xlab("Date") +ylab("Number of cases") + geom_text(aes(label = n), vjust = "inward", hjust = 	"inward",show.legend = FALSE) + scale_color_continuous(name = "Colombia")
                          }
    
    if (var == "GDate") {
      
      Covid_acum <- data_desc %>%
        group_by(data_desc$Date) 	%>%
        tally()
      
      Covid_acum$`data_desc$Date` <- as.Date(Covid_acum$`data_desc$Date`)
      
      #### Add two columns: Daily increments in number and % ####
      
      inc_cases <- diff(Covid_acum$n)
      Covid_acum$inc <- 0
      for (i in 2:nrow(Covid_acum))  {
        Covid_acum$inc[i] <- inc_cases[i-1]
      }
      
        Covid_acum <- mutate(Covid_acum,inc_p = round((inc/n)*100),1)
        D_Plot <- ggplot(Covid_acum,aes(`data_desc$Date`,inc_p, color = inc_p)) + geom_line() + labs(title = "Growth % cases COVID-19 (2020)") + xlab("Date") + ylab("% of Growth") + geom_text(aes(label = inc_p),vjust = "inward",hjust = "inward", show.legend = FALSE) + scale_color_continuous(name = "% Colombia")
                      }
    
    if (var == "DCor") {
      
      Covid_acum <- data_desc %>%
        group_by(data_desc$Date) 	%>%
        tally()
      
        Covid_acum$`data_desc$Date` <- as.Date(Covid_acum$`data_desc$Date`)
        D_Plot <- ggpairs(data = Covid_acum, columns=1:2,ggplot2::aes(colour='data_desc$Date'),title="Correlation:  Date vs No. of cases (Colombia - Acum - April 13 2020)", columnLabels = c("Date","No.of cases"))
                      
                       }
  return(D_Plot)
   
  }
  
predict_plot <- function (var2){
  
   data_desc <- cleandata(Covid)
   
   Covid_acum <- data_desc %>%
     group_by(data_desc$Date) 	%>%
     tally()

   # ###  Predictive Linear Models and plots  ####

   if (var2 == "Loess") {

     Covid_acum$`data_desc$Date` <- as.Date(Covid_acum$`data_desc$Date`)

     #### Number of cases by Date LOESS LRegression - predict Analysis ##
     #### Date as a numeric value to build the prediction model ####

     numeric_date <- as.numeric(Covid_acum$`data_desc$Date`)
     loess_Covid_acum <- loess(formula = Covid_acum$n ~ numeric_date)

     Predict_loess_Covid_acum <- predict(loess_Covid_acum, interval = "prediction")
     Data_predict_loess_Covid_acum <- cbind(Covid_acum,Predict_loess_Covid_acum)
     Data_predict_loess_Covid_acum$Predict_loess_Covid_acum <- abs(Data_predict_loess_Covid_acum$Predict_loess_Covid_acum)

       ####  PLot difference between real and prediction LOESS model (Number of cases / Date) ####

       P_Plot <- ggplot(Data_predict_loess_Covid_acum, 	aes(`data_desc$Date`,n)) + geom_line(color = "red", size = 2 ) + geom_line(aes(y = Predict_loess_Covid_acum), size = 2) + labs(title = "Total Cases Real(Red) Vs Loesspredicted model (Black)") + xlab("Date") + ylab("Number of cases")
                         }

  ### Smooth Loess Linear Regression

   if  (var2 == "Smooth") {

     Covid_acum$`data_desc$Date` <- as.Date(Covid_acum$`data_desc$Date`)

     #### Number of cases by Date LOESS LRegression - predict Analysis ##
     #### Date as a numeric value to build the prediction model ####

     numeric_date <- as.numeric(Covid_acum$`data_desc$Date`)
     loess_Covid_acum <- loess(formula = Covid_acum$n ~ numeric_date)

     Predict_loess_Covid_acum <- predict(loess_Covid_acum, interval = "prediction")
     Data_predict_loess_Covid_acum <- cbind(Covid_acum,Predict_loess_Covid_acum)
     Data_predict_loess_Covid_acum$Predict_loess_Covid_acum <- abs(Data_predict_loess_Covid_acum$Predict_loess_Covid_acum)

       P_Plot <- ggplot(Data_predict_loess_Covid_acum, aes(`data_desc$Date`,n)) + geom_smooth() + geom_point() +labs(title = "Smooth prediction model - COVID-19 (Colombia) by Date ")+ xlab("Date") +ylab("Number of cases")

                           }

       if ( var2 == "Forecast") {

         Datatest <- "F"
         Covid_Test <- NULL
         F_Data_predict_lm_Covid_acum <- prediction(data_desc,Covid_Test,Datatest)

           P_Plot <- ggplot(F_Data_predict_lm_Covid_acum, aes(Date,n)) + geom_point() + geom_smooth(method = "lm") + labs(title = "COVID-19 Forecast trend (Colombia) by Date", subtitle ="Real data: March 06 to April 13")+ xlab("Date") +ylab("Number of cases")+ geom_line(aes(y = lwr), color = "red", linetype = "dashed") + geom_line(aes(y = upr), color = "red", linetype = "dashed")

       }
   
  return(P_Plot) 
}
  
  

#######################################################
# USER INTERFACE                                      #
#######################################################
ui <- dashboardPage(
   dashboardHeader(
     title = h3("COVID19 -ANALYSIS"),
     titleWidth = 300,
    disable = F),
    dashboardSidebar(###  insert your URL logo here ###
    img(src = "logo-vertical-2022_388x300.jpg",height = 388, width = 300, align = "center"),
    hr(),
    h5(HTML("DATA UPLOAD & CLEANING"), style="text-align:center"),
    hr(),
    disable = F,
    width = 300,
    collapsed = F,
       actionButton("case_story"," 1.Read: case story"), #  id input$case_story
       actionButton("Data"," 2.Read: data source"),
       actionButton(inputId = "upload",
                   label = " 3.Data upload ",
                   icon =  icon("refresh")),
      actionButton("cleandata"," 4.Read: data cleaning"),  
      actionButton(inputId = "clean",
                 label = " 5.Data cleaning ",
                 icon =  icon("refresh")),
    hr(),
    h5(HTML("DESCRIPTIVE ANALYSIS"), style="text-align:center"),
    hr(),
    actionButton("descriptive"," 6. Read: decriptive analysis"),
    selectInput("desc_var","Descriptive variables",
                choices = list(
                  "State and City" = "TreeMap",
                  "Place of attention" = "Place",
                  "Gender Histogram" = "Gender",
                  "Analysis by age" = "Age",
                  "Date - Age correlation" = "Cor",
                  "Number of cases by date"= "Date",
                  "% of growth by date" = "GDate",
                  "Date - No cases Corr" = "DCor"),
                selected = "State and City"),
    actionButton("Plot","Plot", icon("option-vertical",lib ="glyphicon")),
    hr(),
    h5(HTML("PREDICTIVE ANALYSIS"), style="text-align:center"),
    hr(),
    actionButton("predictive"," 7. Read: predictive analysis"),
    selectInput("pred_var","Predictive LR models",
                choices = list(
                  "Loess linear regression" = "Loess",
                  "Smooth loess linear regression" = "Smooth",
                  "LR Standard Error" = "Standard",
                  "Forecast linear regression" = "Forecast"),
                 selected = "Loess"),
    actionButton("pred_plot","Predictive Plot", icon("option-vertical",lib ="glyphicon")),
    hr(),
    h5(HTML("TEST"), style="text-align:center"),
    hr(),
    actionButton("test"," 8. Read: TEST DATA"),
    actionButton(inputId = "testupload",
                 label = "9.Test data upload",
                 icon =  icon("refresh")),
    actionButton(inputId = "plottestupload",
                 label = " 10.Plot test data",
                 icon =  icon("refresh")),
    actionButton(inputId = "plotforecast_test",
                 label = " 11.Plot forecast and test data",
                 icon =  icon("refresh")),
    
    hr(),
    tags$div(class="header", checked=NA,
             tags$p("¿DO YOU WANT TO TELL US ABOUT YOUR BUSINESS CASE?", align = "center"),
             tags$a(href="### INSERT YOUR URL WEP PAGE HERE " , h5("CONTACT US HERE", align = "center", target = "_blank"))
    ),
    hr(),
    tags$div(class="header", checked=NA,
             tags$a(href="### INSERT YOUR MAIN MENU URL HERE ###", h5("GO TO MAIN PAGE", align = "center", target = "_blank"))
    ),
    hr(),
    actionButton("refresh","Refresh", icon("refresh")),
    hr()
  ),
  
  dashboardBody(
     
    
    box(
      title = "Data upload",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      DT::dataTableOutput("Data")
    ),
    
    box(
      title = "Cleaned data",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      DT::dataTableOutput("CData")
    ),
    
    box(
      title = "Descriptive analysis",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      plotOutput(outputId = "D_Plot")
    ),
    
    box(
      title = "Predictive analysis",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      plotOutput(outputId = "P_Plot")
    ),
    
    box(
      title = "Standard error",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      verbatimTextOutput(outputId = "summary")
    ),
    
    box(
      title = "Test data upload",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      DT::dataTableOutput("TData")
    ),
    
    box(
      title = "Test data plot",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      plotOutput(outputId = "T_Plot")
    ),
    
    box(
      title = "Forecast and test analysis",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      plotOutput(outputId = "Graph_Pred_Test")
    )
    
    
  )
 
)
   
  
  
  


#######################################################
# server                                              #
#######################################################

server <- function( input, output, session){
  
  observeEvent(input$case_story,{

    showModal(modalDialog(

      title = "COVID19 - COLOMBIAN CASE (2020)  ",
      HTML("The starting data for a pandemic such as COVID19 is not enough to create a well trained <br>
           predictive model. In such a case, a simple linear regression model will help to follow <br>
           the pandemic trend on a daily basis. In the following project the first 41 days , March 06th <br>
           to April the 13th, will bring data to develop an initial LR model, that will be tested with <br>
           real data after april the 13th. While the predicted intervals contain the new test data, <br>
           the prediction LR model will be helpful; otherwise, the new data will be loaded as input for <br>
           the next iteration, which will ends up with a new predictive LR slope, and new predictive intervals <br>
           for the expected model. The process may continue, until there is enough data to develop a more <br>
           accurate predictive model." ),
      #footer = "Esto es un pie de página",
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", #tamaño de la ventana
      easyClose = T,
      fade = T #Efecto

    ))

  })

  observeEvent(input$Data,{

    showModal(modalDialog(

      title = "DATA SOURCE ",
      HTML("Original data source is from the Colombian goverment health institute; data format  <br>
      sometimes have changes. Those changes have to be identified to maintain a unified data structure.<br>
      <br>source at : https://www.ins.gov.co/Noticias/Paginas/coronavirus-casos.aspx <br>
      <br> follow the steps and upload the data"),
      #footer = "Esto es un pie de página",
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", #tamaño de la ventana
      easyClose = T,
      fade = T #Efecto

    ))

  })

  observeEvent(input$upload,{
    updateActionButton(
      session = session,
      inputId = "upload",
      label = "3. Loaded data",
      icon = icon("ok",lib ="glyphicon")
    )
    output$Data <-
      output$results <- DT::renderDataTable(
        Covid,
        options = list(scrollX = TRUE)
      )
  })

  observeEvent(input$cleandata,{
    
    showModal(modalDialog(
      
      title = "DATA CLEANING ",
      HTML("Blank spaces and some columns will be deleted. The cleaned data set will <br>
            show less columns."),
      #footer = "Esto es un pie de página",
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", #tamaño de la ventana
      easyClose = T,
      fade = T #Efecto
      
    ))
  })
  
  
  observeEvent(input$descriptive,{
    
    showModal(modalDialog(
      
      title = "DESCRIPTIVE ANALYSIS",
      HTML("Analysis of historic data will be next; understand the initial data patterns is crucial. <br>
           Different scenarios will be plotted according to the selected variable by using heat maps<br>
           line trends, histograms, and correlation analysis. " ),
      #footer = "Esto es un pie de página",
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", #tamaño de la ventana
      easyClose = T,
      fade = T #Efecto
      
    ))
    
  })
  
  
    observeEvent(input$clean,{
      updateActionButton(                                   
        session = session,
        inputId = "clean",
        label = " 5.Cleaned data",
        icon = icon("ok",lib ="glyphicon")
      )
  
      CData <- cleandata(Covid)
      
        output$CData <- 
        output$results <- DT::renderDataTable(
          CData,
          options = list(scrollX = TRUE)
        )
    })
    
    observeEvent(input$Plot,{
      
      
      updateSelectInput(
        session = session,
        inputId = "desc_var",
        label = "Descriptive plot",
        choices = list(
          "State and City" = "TreeMap" ,
          "Place of attention" = "Place",
          "Gender Histogram" = "Gender",
          "Analysis by age" = "Age",
          "Date - Age correlation" = "Cor",
          "Number of cases by date"= "Date",
          "% of growth by date" = "GDate",
          "Date - No cases Corr" = "DCor"),  
        selected ="TreeMap")
      
      updateActionButton(                                   
        session = session,
        inputId = "Plot",
        label = " Select plot",
        icon = icon("option-vertical",lib ="glyphicon"))
      
      descv <- input$desc_var
  
      ### Descriptive bar plots  ####
      
      if (descv == "TreeMap") {output$D_Plot <- renderPlot({desc_plot(var = "TreeMap")})}
      if (descv == "Place") {output$D_Plot <- renderPlot({desc_plot(var = "Place")})}
      if (descv == "Gender") {output$D_Plot <- renderPlot({desc_plot(var = "Gender")})}
      if (descv == "Age") {output$D_Plot <- renderPlot({desc_plot(var = "Age")})}
      if (descv == "Cor") {output$D_Plot <- renderPlot({desc_plot(var = "Cor")})}
      if (descv == "Date") {output$D_Plot <- renderPlot({desc_plot(var = "Date")})}
      if (descv == "GDate") {output$D_Plot <- renderPlot({desc_plot(var = "GDate")})}
      if (descv == "DCor") {output$D_Plot <- renderPlot({desc_plot(var = "DCor")})}
  
    })
    
    ##### PREDICTIVE MODELS ANALYSIS - LINEAR REGRESSION 
    
    observeEvent(input$predictive,{
      
      showModal(modalDialog(
        
        title = "PREDICTIVE ANALYSIS",
        HTML("The first step to build the predictive model is in this section. Last section <br>
             did show a high correlation of the date and the number of cases; which means that a <br>
             linear regression model would work. The model will show its predicted confidence intevals <br>
             with a smooth technique, the standard statistic error, and the expected predicted intervals. <br>
             After April the 13th,the forecast model will show the LR model expected slope for future <br>
             COVID19 cases." ),
        #footer = "Esto es un pie de página",
        footer = tagList(modalButton("CLOSE WINDOW")),
        size = "m", #tamaño de la ventana
        easyClose = T,
        fade = T #Efecto
        
      ))
    
    })
    
    
    
    
    
    observeEvent(input$pred_plot,{
      
      
      updateSelectInput(
        session = session,
        inputId = "pred_var",
        label = "Predictive LR models",
        choices = list(
          "Loess linear regression" = "Loess",   
          "Smooth loess linear regression" = "Smooth",
          "LR Standard Error" = "Standard" ,
          "Forecast linear regression" = "Forecast"),
         selected ="Loess")
      
      updateActionButton(                                   
        session = session,
        inputId = "pred_plot",
        label = " Select and Plot",
        icon = icon("option-vertical",lib ="glyphicon"))
      
      predv <- input$pred_var
      
      if (predv == "Loess") {output$P_Plot <- renderPlot({predict_plot(var2 = "Loess")})}
      if (predv == "Smooth") {output$P_Plot <- renderPlot({predict_plot(var2 = "Smooth")})}
      if (predv == "Forecast") {output$P_Plot <- renderPlot({predict_plot(var2 = "Forecast")})}
      # Standard Error plot ###
      if (predv == "Standard") {
        
             data_desc <- cleandata(Covid)
             
             Covid_acum <- data_desc %>%
             group_by(data_desc$Date) 	%>%
             tally()

           data_desc$Date <- as.Date(data_desc$Date)
           numeric_date <- as.numeric(Covid_acum$`data_desc$Date`)
           lm_Covid_acum <- lm(formula = Covid_acum$n ~ numeric_date)

             output$summary <- renderPrint({

             summary(lm_Covid_acum)

           })
        }
  
    })
    
    
    observeEvent(input$testupload,{
      updateActionButton(
        session = session,
        inputId = "testupload",
        label = "9.Test data upload",
        icon = icon("ok",lib ="glyphicon")
      )

      Covid_Test <- read.csv("DateCasesMay_14_COVID19.csv",header = TRUE)
      output$TData <-
        output$results <- DT::renderDataTable(
          Covid_Test,
          options = list(scrollX = TRUE)
        )

    })
    
    
    observeEvent(input$test,{
      
      showModal(modalDialog(
        
        title = "Test data",
        HTML("The test data is the real data obtained for a period of one month after April 13th 2020. <br>
             The test data will be compared within the predicted red max and min range pointed lines<br>
             to check the date when the test data gets out of the limited range; from that date a new linear<br>
             regression predicted model has to be calculated. <b>The final conclusion for this case<br>
             is that the LR predictive model worked well for two weeks, until the vertical plotted <br>
             line shows the deviation of the test data.</b><br>"),
        #footer = "Esto es un pie de página",
        footer = tagList(modalButton("CLOSE WINDOW")),
        size = "m", #tamaño de la ventana
        easyClose = T,
        fade = T #Efecto
        
      ))
      
    })
  
   
    observeEvent(input$plottestupload,{
      updateActionButton(
        session = session,
        inputId = "plottestupload",
        label = "10. PLot test data",
        icon = icon("ok",lib ="glyphicon")
      )
      
      Covid_Test <- read.csv("DateCasesMay_14_COVID19.csv",header = TRUE)
      Covid_acum_T <- Covid_Test %>%
        group_by(Covid_Test$Date) %>%
        tally()
      
      output$T_Plot <- renderPlot({

        Covid_acum_T$`Covid_Test$Date` <- as.Date(Covid_acum_T$`Covid_Test$Date`)
        T_Plot <- ggplot(Covid_acum_T, aes(`Covid_Test$Date`,n, color = n))
        T_Plot + geom_point() + labs(title = "Total Cases COVID-19 by Date (2020) - real test data", subtitle = "April 14 to May 14") + xlab("Date") + scale_x_date(date_labels = "%m-%d") + ylab("Number of cases") + geom_text(aes(label = n),vjust = "inward",hjust = "inward",show.legend = FALSE) + scale_color_continuous(name = "Colombia")
        
      })
      
    })
    
    
    observeEvent(input$plotforecast_test,{
      updateActionButton(
        session = session,
        inputId = "plotforecast_test",
        label = "12. PLot forecast and test data",
        icon = icon("ok",lib ="glyphicon")
      )
      
      #real data and test data data frames
      
      data_desc <- cleandata(Covid)
      data_desc <- data_desc[,c(-8)] # delete last column to compare data sets
      Covid_Test <- read.csv("DateCasesMay_14_COVID19.csv",header = TRUE)
      Datatest <- "T"
      Pred_Test <- prediction(data_desc,Covid_Test,Datatest)
     
     
       output$Graph_Pred_Test <- renderPlot({
     
         Graph_Pred_Test <- ggplot(Pred_Test, aes(Pred_T_Date,Pred_T_Test_n))
         Graph_Pred_Test + geom_point() + geom_smooth( method = "loess") +labs(title = "COVID-19 (Colombia - 2020) - Test", subtitle = "Real : March 06 to April 13 / Test(real data): April 14 to May 14") + xlab("Date") +ylab("Number of cases")+ geom_line(aes(y = Pred_T_lwr), color = "red", linetype = "dashed") + geom_line(aes(y = Pred_T_upr), color = "red", linetype = "dashed") + geom_vline(xintercept = as.numeric(as.Date("2020-05-01")), linetype=4)
         
      })
    
  })
    
    
    observeEvent(input$refresh,{
      updateActionButton(                                  
        session = session,
        inputId = "refresh",
        label = "refresh",
        icon = icon("ok",lib ="glyphicon")
      )
      
      session$reload()
      
    })
   
}
  

#######################################################
# LAUNCH                                             #
#######################################################

shinyApp( ui = ui , server = server)  
    
   

