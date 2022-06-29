# Load Packages ----
library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(DT)
library(V8)
#library(discrimARTs)
library(leaflet)
library(raster)
library(shinyWidgets)
library(stats)
library(TSA)
library(ggplot2)
library(magrittr)
library(tidyr)
library(rlang)
library(RColorBrewer)
library(raster)
library(rgdal)
library(boastUtils)


# Define UI for the App ----

ui <- list(
  ## create the app page ----
  dashboardPage(
    skin = "purple",
    ### App header ----
    dashboardHeader(
      title = "Time Series Models",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "personal_introduction_app-sv101")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer_alt")),
        menuItem("Prerequisite", tabName = "pre", icon = icon("book")),
        menuItem("Simulation", tabName = "sim", icon = icon("wpexplorer")),
        menuItem("Analyzing Real Data", tabName = "data", icon = icon("cogs")),
        menuItem("Concept Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Content ----
    dashboardBody(
      tabItems(
        #### Prerequisite page ----
        tabItem(
          tabName = "pre",
          withMathJax(),
          h2("Prerequisite"),
          #h3(strong("Background: Time Series Models")),
          h4("Recall the following:"),
          br(),
          box(
            width =12,
            collapsible = TRUE,
            collapsed = FALSE,
            title = strong("Stationarity"),
            p("Diagnostics for stationarity include looking for constant mean 
               (or, trend) and variance over time"),
            p("Constant mean is associated with data that does not have any 
                 sort of vertical (typically linear) trend over time."),
            p("Seasonality could also be apparent in the mean structure. 
                 Recall that seasonal ARIMA cannot explain a seasonal trend, 
                 only seasonal correlations (ARIMA models work to explain 
                 correlation structure of a time series AFTER the mean and 
                 variance are constant)."),
            p("Constant variance is associated with data whose vertical s
                 pread (in the valleys and peaks) is constant over the duration
                 of the time series.")
          ),
          # column(
          #   11,
          #   h3(tags$li("Stationarity:")),
          #   h4("Diagnostics for stationarity include looking for constant mean 
          #      (or, trend) and variance over time"),
          #   column(11, offset=1,
          #          
          #     h4("Constant mean is associated with data that does not have any 
          #        sort of vertical (typically linear) trend over time."),
          #     h4("Seasonality could also be apparent in the mean structure. 
          #        Recall that seasonal ARIMA cannot explain a seasonal trend, 
          #        only seasonal correlations (ARIMA models work to explain 
          #        correlation structure of a time series AFTER the mean and 
          #        variance are constant)."),
          #     h4("Constant variance is associated with data whose vertical s
          #        pread (in the valleys and peaks) is constant over the duration
          #        of the time series.")
          #   ),
          br(),
          box(
            width =12,
            collapsible = TRUE,
            collapsed = TRUE,
            title = strong("Autocorrelation Functions of Stationary Time Series"),
            p("We typically trust the dashed lines in the autocorrelation 
               function (ACF) plots to be the significance cut-off bounds for 
               any lag's correlation"),
            p("In a model with non-zero autoregressive (AR) and moving average 
               (MA) parts, there is no logical interpretation for both ACFS 
               cutting off, thus,"),
            p("For AR(p) models, the ACF will tail off and the PACF 
                         will cut off after lag p."),
            p("For MA(q) models, the ACF will cut off after lag q, 
                         and the PACF will tail off."),
            p("For ARMA(p, q) models, both the ACF and the PACF 
                          will both tail off."),
            p("The ARMA subsets plot is not the best tool for determining 
               ARMA(p,q) orders, and thus will only be used as a tie breaker or 
               guide after the ACF and PACF plots have been thoroughly 
               inspected."),
          ),
            # h3(tags$li("Autocorrelation Functions of Stationary Time Series:")),
            # h4("We typically trust the dashed lines in the autocorrelation 
            #    function (ACF) plots to be the significance cut-off bounds for 
            #    any lag's correlation"),
            # h4("In a model with non-zero autoregressive (AR) and moving average 
            #    (MA) parts, there is no logical interpretation for both ACFS 
            #    cutting off, thus,"),
            # column(
            #   11, offset=1,
            #   h4(tags$li("For AR(p) models, the ACF will tail off and the PACF 
            #              will cut off after lag p.")),
            #   h4(tags$li("For MA(q) models, the ACF will cut off after lag q, 
            #              and the PACF will tail off.")),
            #    h4(tags$li("For ARMA(p, q) models, both the ACF and the PACF 
            #               will both tail off."))
            # ),
            # h4("The ARMA subsets plot is not the best tool for determining 
            #    ARMA(p,q) orders, and thus will only be used as a tie breaker or 
            #    guide after the ACF and PACF plots have been thoroughly 
            #    inspected."),
          br(),
          box(
            width =12,
            collapsible = TRUE,
            collapsed = TRUE,
            title = strong("Model Diagnostics"),
            p("The ARIMA model aims to forecast future values of a stationary 
               time series by estimating a mathematical function to explain the 
               underlying correlation structure. For this reason, the ACF and 
               PACF of the residuals of the ARIMA model that has been fitted 
               should not contain any significant remaing correlation."),
            p("Though forecasting is the purpose for fitting an ARIMA model, 
               looking at the forecast itself (against future values that have 
               been reserved) isnt the best way to assess the goodness of the 
               model's fit, this is why we look at the AIC and the ACF plots 
               of the residuals of the model.")
          ),
            # h3(tags$li("Model Diagnostics:")),
            # h4("The ARIMA model aims to forecast future values of a stationary 
            #    time series by estimating a mathematical function to explain the 
            #    underlying correlation structure. For this reason, the ACF and 
            #    PACF of the residuals of the ARIMA model that has been fitted 
            #    should not contain any significant remaing correlation."),
            # h4("Though forecasting is the purpose for fitting an ARIMA model, 
            #    looking at the forecast itself (against future values that have 
            #    been reserved) isnt the best way to assess the goodness of the 
            #    model's fit, this is why we look at the AIC and the ACF plots 
            #    of the residuals of the model.")
        ),
        #### Overview page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Time Series Model"),
          br(),
          p("In this app the goal is to become more familiar with time series analysis."),
          p("In the three parts of the app, you will explore time series with simulations, challange yourself with time series analysis, and review your knowledge of associated material with a game."),
          p("In particular, the first part requires students to engage with simulations so they could find out how model equations may relate to the graphical representation of the time series. The second feature walks the user through time series analysis of a selected real world data set. The user will make the data stationary, fit a model, and observe the quality of their model in this analysis. The third feature will quiz the user about time series concepts as a part of a fun tic-tac-toe game."),
          h2("Instructions:"),
          h3("Simulation Exploration:"),
          tags$ol(
            tags$li("Use the sliders for the coefficients and explore how 
                    changing parameter values affects the time series plot."),
            tags$li("Use the drop down menus and observe how different orders 
                    of models effect the autocorrelation function (ACF) and 
                    partial autocorrelation function (PACF) plots.")
          ),
          div(style = "text-align: center",
            bsButton("go1", "GO!", icon("bolt"), size = "large", class="circle grow")
          ),
          h3("Time Series Analysis with real data:"),
          tags$ol(
            tags$li("In the first tab, select the data set that 
              you would like to analyze, and fit 
              transformations until the data seems 
              stationary."),
            tags$li("Here, you must consider that the last 12 observations of 
                    each data set are hidden from the user so that they could 
                    be presented in the last tab alongside the user's model's 
                    forecasts."),
            tags$li("For each transformation consider the following:"),
            tags$li("The transformations are of the form 
                    `seas_diff(diff(log(data)))`, that is, the log 
                    transformation will always be taken first, followed by the 
                    difference of lag one, and then the seasonal differencing."),
            tags$li("The trend can only be removed before any other 
                    transformation, or after all transformations. The trend is 
                    removed using regression, and the transformed data is the 
                    residuals from that regression.")
          ),
          div(style = "text-align: center",
            bsButton("go2", "GO!", icon("bolt"), size = "large", 
                     class="circle grow")
          ),
          br(),br(),
          h2("Acknowledgements:"),
          # p("This app was conceived in its entirety by Ryan Voyack and 
          #          Yubaihe Grace Zhou in June and July of 2018 with Ryan 
          #          leading on the data analysis and game components and Grace 
          #          leading on the simulation components . Special thanks to 
          #          Saurabh Jugalkishor Jaju from AnalyticsVidhya.com and 
          #          University of California, Berkeley, for giving us permission 
          #          to use his questions, and special thanks to Professor Scott 
          #          Roths, Penn State University, for help on using time series 
          #          ARIMA functions, and special thanks to Angela Ting for help 
          #          with applying the front-end design."),
          p("This app wass developed by Ryan Voyack and Yubaihe Grace Zhou 
            in 2018 and updated by Stuart Vas in 2022",
            br(),br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),br(),
            div(class = "updated", "Last Update: 6/13/2022 by NJH")
          )
        ),
        
        tabItem(
          tabName = "sim",
          fluidPage(
          fluidRow(
          column(
            width = 4,
            selectInput(
              "models","Models",
              list( "Autoregressive" = "AR",
                "Moving Average" = "MA",
                "Autoregressive Moving Average" = "ARMA"
              )
            ),
            sliderInput("n",
              label = "Sample Size",
              min = 10,
              max = 1000,
              step = 5,
              value = 20,
              ticks = T
            ),
            conditionalPanel(
            #AR
              condition = ("input.models=='AR' || input.models=='ARMA'"),
              h4(p("AR(p)")),
              selectInput("p","p order",
                list( "1","2" )
              ),
              conditionalPanel(
                condition = ("input.p == '1' || input.p == '2'"),
                #h5(p(withMathJax(textOutput("Phi1")))),
                sliderInput("phi1",
                                           label = "Phi1",
                                           min = -0.9,
                                           max = 0.9,
                                           step = 0.1,
                                           value = 0.5,
                                           ticks = T
                               ),
                               conditionalPanel(
                                 condition = "input.p == '2'",
                                 
                                 sliderInput("phi2",
                                             label = "Phi2",
                                             min = -0.9,
                                             max = 0.9,
                                             step = 0.1,
                                             value = 0,
                                             ticks = T
                                 )
                               )
                             )
                             
                           ),
                           conditionalPanel(
                             condition = ("input.models=='ARMA'"),
                             hr()
                           ),
                           conditionalPanel(
                             #MA
                             condition = ("input.models=='MA' || input.models=='ARMA'"),
                             h4(p("MA(q)")),
                             
                             selectInput("q","q order",
                                         list( "1",
                                               "2"
                                         )
                             ),
                             conditionalPanel(
                               condition = ("input.q == '1' || input.q == '2'"),
                               #h5(p(withMathJax(textOutput("Phi1")))),
                               
                               sliderInput("theta1",
                                           label = "Theta1",
                                           min = -0.9,
                                           max = 0.9,
                                           step = 0.1,
                                           value = 0.5,
                                           ticks = T
                               ),
                               conditionalPanel(
                                 condition = ("input.q == '2'"),
                                 
                                 sliderInput("theta2",
                                             label = "Theta2",
                                             min = -0.9,
                                             max = 0.5,
                                             step = 0.1,
                                             value = 0.1,
                                             ticks = T
                                 )
                               )
                             )
                           )
                    ),
                    column(width = 8,
                           fluidRow(
                             column(12,
                                    plotOutput("plotSIM")
                             )
                           ),
                           fluidRow(
                             conditionalPanel(
                               condition="input.models=='ARMA'",
                               column(width = 6, plotOutput("plot.ACF")),
                               column(width = 6, plotOutput("plot.PACF"))
                             )
                           )
                    )
                  ),
                  
                  fluidRow(column(width = 12,
                                  fluidRow(
                                    conditionalPanel(
                                      condition="input.models!='ARMA'",
                                      fluidRow(
                                        column(6, plotOutput("plotACF")),
                                        column(6, plotOutput("plotPACF"))
                                      )
                                    )
                                  )
                  )
                  )
                )#fluidpage
        ),
        
        #### Datasets ----
        tabItem(
          tabName = "data",
          (
          tabsetPanel(
            id = "tabs2",
            tabPanel(
              title = h4("Achieving Stationarity"), value = "step1",
              h4("To begin, please choose from of the data sets below any one 
                 that you would like."),
              h4("Use the checkbox options to make your data look satisfactorily 
                 stationary."),
              h4("The first part of our analysis is achieving stationarity, so 
                 that you can correctly judge the correlation structure to 
                 assign to the arima model."),
              sidebarLayout(
                sidebarPanel(
                selectInput("sets",tags$b("Choose a Dataset Below"),
                  list("Choose"="Choose",
                    "Internet Traffic"= "internet",
                    "Monthly Traffic Fatalities" = "monthly",
                    "varve" = "varve",
                    "sheep" = "sheep",
                    "Southern Oscillation Index" = "soi",
                    "Daily Max Temp State College" = "temperature"
                  )
                ),
                br(),
                conditionalPanel(
                  condition = "input.sets!='Choose'",
                  checkboxInput(inputId="trend", label="Remove trend/non-constant mean in data", value=FALSE),
                  # bsPopover(id="trend", title = "tit", content = "The trend can only be removed before or after all transformations.", trigger = "hover", placement = "top"),
                  conditionalPanel(
                    condition = "input.trend",
                    tags$div(style="margin-left: 3vw;",
                      checkboxInput(
                        inputId="trend1", 
                        label="Estimate and Remove linear Trend in Data With 
                          Regression", value=FALSE),
                      checkboxInput(
                        inputId="trend2", 
                        label="Estimate and Remove Seasonal Trend in Data 
                          With Regression", value=FALSE),
                      checkboxInput(
                        inputId="trend3", label="Remove Seasonal Trend in 
                          Data With Cosine Regression", value=FALSE)
                    ),
                    conditionalPanel(
                      condition="input.sets != 'temperature' 
                      && input.sets != 'monthly' && input.sets != 'soi' 
                      && (input.trend2 || input.trend3)",
                      numericInput(
                        inputId="frequency", label="Choose the frequency of 
                        the time series", value=1, min=1, max=365)
                    )
                  ),
                  checkboxInput(
                    inputId="log", 
                    label="Take log transformation of data", 
                    value=FALSE),
                  checkboxInput(
                    inputId="diff", 
                    label="Take first difference of data", 
                    value=FALSE),
                  conditionalPanel(
                    condition="input.diff",
                    tags$div(style="margin-left: 3vw;",
                      checkboxInput(
                        inputId="diff2", 
                        label="Take second difference of data", 
                        value=FALSE)
                    )
                  ),
                  checkboxInput(
                    inputId="seas_diff", 
                    label="Take first seasonal difference of data", 
                    value=FALSE),
                  conditionalPanel(
                    condition="input.seas_diff",
                    tags$div(style="margin-left: 3vw;",
                      numericInput(
                        inputId="seas", 
                        label="Choose the seasonal period", 
                        value=1, min=1, max=30),
                      checkboxInput(
                        inputId="seas_diff2", 
                        label="Take second seasonal difference of data", 
                        value=FALSE)
                    )
                  ),
                  div(style = "text-align: center",
                    div(id = "div",
                      actionButton("go4", "Next step!", 
                        style = "primary", disabled = TRUE)
                    )
                  )
                )
              ),
              mainPanel(
                conditionalPanel(
                  condition = "input.sets!='Choose'",
                  plotOutput("original"),
                  uiOutput("cite"),
                  plotOutput("transform")
                )
              )
            )
          ),
          tabPanel(
            title = h4("Determine ARMA order"), value = "step2",
            h4("Now, that you have made your data stationary, you can inspect 
               the resulting acf plots as well as the ARMAsubsets plot below 
               to determine the arima order and fit a model."),
            fluidRow(
              plotOutput("ACF"),
              bsPopover(id="ACF", title = "ACF of transformed data", 
                      content = "The blue dashed line represents significance 
                      bounds for correlation at different lags in the data.", 
                      trigger = "hover", placement = "bottom"),
              plotOutput("PACF"),
              bsPopover(id="PACF", title = "PACF of tranformed data", 
                      content = "The blue dashed line represents significance 
                      bounds for correlation at different lags in the data.", 
                      trigger = "hover", placement = "top")
            ),
            div(style = "text-align: center",
              h4('After you are finished making your choices, press the 
                "Next step!" button below to see how good of a fit your model 
                was.')
            ),
            fluidRow(
              column(4,
                numericInput("p.order", "AR part order", value=0, max=10, min=0),
                numericInput("q.order", "MA part order", value=0, max=10, min=0),
                numericInput("P.order", "Seasonal AR part order", 
                             value=0, max=10, min=0),
                numericInput("Q.order", "Seasonal MA part order", 
                             value=0, max=10, min=0),
                numericInput("period", "Seasonal Period", value=0, max=12, min=0)
              ),
              column(8,
                plotOutput("subsets"),
                bsPopover(
                  id="subsets", title = "ARMA subsets", 
                  content = "This plot shows the best combinations of ARMA 
                  orders using AIC. The greyed squares indicate that the 
                  parameter is used in the model.", 
                  trigger = "hover", placement = "top")
              ),
              div(style = "text-align: center",
                bsButton("go5", "Next step!", style = "primary")
              )
            )
          ),
          tabPanel(title = h4("Forecast"), value = "step3",
            fluidRow(
              column(11,
                h4("Below you can see how well you fit the data to a time 
                   series by seeing the resulting forecasts (plotted against 
                   the last 12 observations of the data set, which were hidden 
                   from the initial time series plot), this plot will show 
                   you-in blue-only the final 100 observations in the data set. 
                   You can also observe the correlation structure of the 
                   residuals of the arima fit to see if you were able to 
                   fully explain the correlation with the model. The progress 
                   bar will show you how close you are to the best possible 
                   fit for the data set.")
              )
            ),
            fluidRow(
              div(style = "position:relative; z-index: 950;",
                plotlyOutput("forecast")
              ),br(),
              # fluidRow(
              #   div(style = "position:relative; z-index: 900;",
              #     div(style = "margin-top: -20vh;",
              #       div(style = "text-align: center",
              #         imageOutput("bar") #, height = "1000px")
              #       )
              #     )
              #   )
              # ),
              wellPanel(
                fluidRow(
                  #div(style = "position:relative; z-index: 950;",
                  #div(style = "margin-top: -20vh;",
                  column(6, plotOutput("fitQuality1")),
                    bsPopover(id="fitQuality1", title = "ACF residuals", content = "The ACF and PACF of the residuals of the fitted model can indicate if theres any remaining correlation structure in the data.", trigger = "hover", placement = "top"),
                    column(6, plotOutput("fitQuality2")),
                    bsPopover(id="fitQuality2", title = "PACF residuals", content = "The ACF and PACF of the residuals of the fitted model can indicate if theres any remaining correlation structure in the data.", trigger = "hover", placement = "top")
                  #)
                  #)
              )),
              fluidRow(
                div(style = "position:relative; z-index: auto;",
                  div(style = "margin-top: -20vh;",
                    column(5, plotOutput("bar")),
                    column(6, 
                      br(),
                      br(),
                      br(),
                      br(),
                      br(), #these are necessary
                      br(),
                      br(),
                      br(),
                      br(),
                      verbatimTextOutput("feedback", placeholder = TRUE)
                    )
                  )
                ),
                div(style = "position:relative; z-index: auto;",
                  bsPopover(
                    id="bar", title = "Model fit evaluation", 
                    content = "This indicates how well your model was fit. 
                    100 would indicate that your model is as good as can be. 
                    0 would indicate that your model was no better than using 
                    just the mean as the predictor.", trigger = "hover", 
                    placement = "right")
                )
              )
            )
          )
        ))
        ),
        
        ##Concept Game tic-tac-toe
        tabItem(tabName = "game",
                fluidPage(
                  fluidRow(
                    div(style = "text-align: center;",
                        h4("If you answer correctly, You will receive an X in the square you chose, if not, it will be an O."),
                        h4("Try your best to win the game and get 3 X's in a row !"),
                        br()
                    )
                  ),
                  fluidRow(
                    column(4,
                           leafletOutput('image'),
                           br(),
                           textOutput("warning"),
                           textOutput("gameMessage")
                    ),
                    column(8,
                           
                           conditionalPanel("output.temp != 2",
                                            conditionalPanel("input.image_click",
                                                             uiOutput("Question"),
                                                             uiOutput("CurrentQuestion"),
                                                             uiOutput("CurrentQuestion.extra"),
                                                             br(),
                                                             br(),
                                                             br()
                                            ),
                                            textOutput("directions"),
                                            br()
                           ),
                           column(8,
                                  fluidRow(
                                    column(6, div(style="text-align: center", bsButton(inputId = 'submit', label = 'Submit Answer', style="primary", disabled=TRUE))),
                                    column(6, div(style="text-align: center", bsButton(inputId = "nextButton",label = "Next Question", style="primary", disabled=TRUE)))
                                  ),
                                  fluidRow(
                                    column(12, div(style="text-align: center", bsButton(inputId="reset", label="Start new game", style="primary")))
                                  )
                                  
                           )
                    )
                  ),
                  fluidRow(
                    # column(width=12, offset = 6,
                    #        bsButton(inputId="reset", label="Start new game", style="primary")
                    # ),
                    column(12,uiOutput("Feedback"))
                  )
                )
                
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  ######exploration and simulation
  observeEvent(input$go0,{
    updateTabItems(session, "tabs", "overview")
  })
  observeEvent(input$go1,{
    updateTabItems(session, "tabs", "sim")
  })
  observeEvent(input$go2,{
    updateTabItems(session, "tabs", "data")
  })
  observeEvent(input$go4,{
    # if(!(input$trend & !input$trend1 & !input$trend2 & !input$trend3)){
    updateTabsetPanel(session, "tabs2",
                      selected = "step2"
    )
    # }
  })
  observeEvent(input$go5,{
    updateTabsetPanel(session, "tabs2",
                      selected = "step3"
    )
  })
  
  
  ####### SIMULATED #######
  
  observeEvent(input$info1,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "•	Use the sliders for the coefficients and explore how changing parameter values affects the time series plot.\n
      •	Use the drop down menus and observe how different orders of models effect the autocorrelation function (ACF) and partial autocorrelation function (PACF) plots.",
      type = "info"
    )
  })
  observeEvent(input$info2,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "•	In the first tab, select the data set that you would like to analyze, and fit transformations until the data seems stationary.\n
      •	Here, you must consider that the last 12 observations of each data set are hidden from the user so that they could be presented in the last tab alongside the user's model's forecasts.\n
      •	For each transformation consider the following:\n
      i. The transformations are of the form `seas_diff(diff(log(data)))`, that is, the log transformation will always be taken first, followed by the difference of lag one, and then the seasonal differencing.\n
      ii. The trend can only be removed before any other transformation, or after all transformations. The trend is removed using regression, and the transformed data is the residuals from that regression.",
      type = "info"
    )
  })
  #make sure higher order models are stationary
  observeEvent(input$phi1, {
    if(input$p == '1'){return(NULL)}
    val <- input$phi1
    updateSliderInput(session, inputId="phi2", min=ifelse(val > 0, -val, val), max=ifelse(val < 0, -val, val), value=input$phi2, step=.1)
  })
  #make sure higher order models are stationary
  observeEvent(input$theta1, {
    if(input$q == '1'){return(NULL)}
    val <- input$theta1
    updateSliderInput(session, inputId="theta2", min=ifelse(val > 0, -val, val), max=ifelse(val < 0, -val, val), value=input$theta2, step=.1)
  })
  
  
  
  sim <- reactiveVal(NULL)
  observeEvent(
    {input$models
      input$resample
      input$phi1
      input$phi2
      input$theta1
      input$theta2
      input$p
      input$q}, {
        output$plotSIM <- renderPlot({
          if(input$models == "AR"){
            sim(arima.sim(n=input$n,list(ar=c(input$phi1,tryCatch(input$phi2, error=function(e)NULL)))))
          }else if(input$models == "MA"){
            sim(arima.sim(n=input$n,list(ma=c(input$theta1,tryCatch(input$theta2, error=function(e)NULL)))))
          }else if(input$models == "ARMA"){
            sim(arima.sim(n=input$n,list(ma=c(input$theta1,tryCatch(input$theta2, error=function(e)NULL)), ar=c(input$phi1, tryCatch(input$phi2, error=function(e)NULL)))))
          }
          plot(sim(), main="Simulation")
        })
      })
  output$plotACF <- renderPlot({
    acf(sim(), main = "Autocorrelation Function")
  })
  output$plotPACF <- renderPlot({
    pacf(sim(), main = "Partial Autocorrelation Function")
  })
  #     these render the same plot, but we need two different calls to comply with constraints introduced by the UI
  output$plot.ACF <- renderPlot({
    acf(sim(), main = "ACF")
  })
  output$plot.PACF <- renderPlot({
    pacf(sim(), main = "PACF")
  })
  
  
  
  
  
  ####### REAL DATA #######
  
  #####
  # assign permissions to buttons and checkboxes (etc) based on tabs  
  observe({
    validate(need(input$sets=="Choose", label=''))
    hideTab(inputId = "tabs2", target = "step2")
    hideTab(inputId = "tabs2", target = "step3")
  })
  disabled_ <- reactiveVal("NO")
  observeEvent(input$sets, priority = 2, {
    if(input$sets != "Choose"){
      updateButton(session, "go4", disabled = FALSE)
      disabled_("NO")
    }else{
      updateButton(session, "go4", disabled = TRUE)
      disabled_("YES")
    }
  })
  observeEvent(input$go4, {
    showTab(inputId = "tabs2", target = "step2")
  })
  observeEvent(input$go5, {
    showTab(inputId = "tabs2", target = "step3")
  })
  
  
  #server side calculations for the data and the function of the challange itself
  
  #### step1 code ####
  data <- eventReactive(input$sets, {
    if(input$sets == "Choose"){
      ts(c(1:20)) # create a dummy time series
    }else if(input$sets == "internet"){
      X <- read.table("internet.traffic.csv", sep=",", header = T)
      names(X) <- c("Time", "Series")
      hours <- as.POSIXct(X$Time, format = "%m/%d/%Y %H:%M")
      as.ts(X$Series) #[1:1644]
    }else if(input$sets == "monthly"){
      load("MonthlyTrafficFatalities.Rdata")
      ts(MonthlyTrafficFatalities, frequency = frequency(MonthlyTrafficFatalities), start=1960)
    }else if(input$sets == "varve"){
      ts(read.table("varve.dat"), frequency=1, start=-9823)
    }else if(input$sets == "sheep"){
      ts(read.table("sheep.dat"), frequency = 1, start=1867)
    }else if(input$sets == "soi"){
      ts(read.table("soi.dat"), frequency = 12, start=1950)
    }else if(input$sets == "temperature"){
      load("SC_Daily_Tmax.Rdata")
      ts(SC_Daily_Tmax, frequency=365, start=2001)
    }
  })
  
  observeEvent(input$sets, once = TRUE, {
    shinyjs::enable("go4")
    disabled_("NO")
  })
  
  observeEvent(input$trend, priority = 4, {
    if(input$trend){
      shinyjs::disable("go4")
      disabled_("YES")
      hideTab(inputId = "tabs2", target = "step2")
      hideTab(inputId = "tabs2", target = "step3")
    }else{
      shinyjs::enable("go4")
      disabled_("NO")
      showTab(inputId = "tabs2", target = "step2")
      showTab(inputId = "tabs2", target = "step3")
    }
  })
  
  observeEvent(
    {input$trend1
      input$trend2
      input$trend3}, priority = 4, {
        if(input$trend1 | input$trend2 | input$trend3){
          shinyjs::enable("go4")
          disabled_("NO")
          shinyjs::disable("log")
          showTab(inputId = "tabs2", target = "step2")
        }else{
          shinyjs::disable("go4")
          disabled_("YES")
          hideTab(inputId = "tabs2", target = "step2")
          shinyjs::enable("log")
        }
      })
  
  observeEvent(
    {input$trend1
      input$trend2
      input$trend3}, priority=3, label = 'disable and update trend boxes', {
        if(!(input$trend1 == FALSE & input$trend2 == FALSE & input$trend3 == FALSE)){
          # this executes when the trend has been chosen
          if(input$trend1){
            shinyjs::disable("trend2")
            shinyjs::disable("trend3")
          }else if(input$trend2){
            shinyjs::disable("trend1")
            shinyjs::disable("trend3")
          }else if(input$trend3){
            shinyjs::disable("trend1")
            shinyjs::disable("trend2")
          }
        }else{
          # this is when the trend goes from being chosen to not chosen (all check boxes unchecked)
          shinyjs::enable("trend1")
          shinyjs::enable("trend2")
          shinyjs::enable("trend3")
          if(!(input$sets %in% c('temperature', 'monthly', 'soi'))){
            updateNumericInput(session, "frequency", value=1)
          }
        }
        warning("trend choice", 2)
      })
  
  observeEvent(input$trend, priority = 2, {
    if(input$trend == FALSE){
      updateCheckboxInput(session, "trend1", value=FALSE)
      updateCheckboxInput(session, "trend2", value=FALSE)
      updateCheckboxInput(session, "trend3", value=FALSE)
    }
  })
  
  observeEvent(
    {input$trend
      input$trend1
      input$trend2
      input$trend3}, label = 'turn off everything when the trend is removed if things were removed before it', {
        if((input$trend1 | input$trend2 | input$trend3) & (input$log | input$diff | input$seas_diff)){
          shinyjs::disable("log")
          shinyjs::disable("diff")
          shinyjs::disable("diff2")
          shinyjs::disable("seas_diff")
          shinyjs::disable("seas_diff2")
        }else{
          shinyjs::enable("diff")
          shinyjs::enable("diff2")
          shinyjs::enable("seas_diff")
          shinyjs::enable("seas_diff2")
        }
      })
  
  observeEvent(input$diff, label = 'turn off diff2 when diff1 off', {
    if(input$diff == FALSE){
      updateCheckboxInput(session, "diff2", value=FALSE)
    }
  })
  observeEvent(input$seas_diff, label = 'turn off seasdiff2 when seasdiff1 off', {
    if(input$seas_diff == FALSE){
      updateNumericInput(session, "seas", value=1) 
      updateCheckboxInput(session, "seas_diff2", value=FALSE)    
      #this may interfere with the observer directly below but that shouldnt be a problem
      warning("four", 4)
    }
  })
  observeEvent(
    {input$seas
      input$seas_diff2}, label = 'reset "period" in step2 when set "season" in step1', {
        validate(need(!(input$seas_diff == FALSE & input$seas_diff2 == FALSE), label = ''))
        updateNumericInput(session, "period", value=0)
      })
  
  transformed <- reactiveVal(NULL)
  observe({
    validate(need(input$trend1 == FALSE & input$trend2 == FALSE & input$trend3 == FALSE & input$log == FALSE & input$diff == FALSE & input$seas_diff == FALSE, message=''))
    transformed( data()[1:(length(data())-12)] )
    warning("REDO")
  })
  #this observer clears the checkboxes in step1 tab when a new data set is chosen
  observeEvent(input$sets, label = 'reset all checkboxes', priority = 2, {
    validate(need(input$sets!="Choose", label=''))
    updateCheckboxInput(session, "trend", value=FALSE)
    updateCheckboxInput(session, "trend1", value=FALSE)
    updateCheckboxInput(session, "trend2", value=FALSE)
    updateCheckboxInput(session, "trend3", value=FALSE)
    updateCheckboxInput(session, "log", value=FALSE)
    updateCheckboxInput(session, "diff", value=FALSE)
    updateCheckboxInput(session, "diff2", value=FALSE)
    updateCheckboxInput(session, "seas_diff", value=FALSE)
    updateCheckboxInput(session, "seas_diff2", value=FALSE)
    updateNumericInput(session, "seas", value=1)
    if(!(input$sets %in% c('temperature', 'monthly', 'soi'))){
      updateNumericInput(session, "frequency", value=1)
    }
    transformed(data()[1:(length(data())-12)])
    warning('new set', 6)
  })
  
  checks <- reactiveValues(a=FALSE, b=FALSE, c=FALSE)
  observeEvent(input$trend1, priority = 2, {
    checks$a <- if(input$trend1){
      TRUE
    }else{
      FALSE
    }
  })
  observeEvent(input$trend2, priority = 2, {
    checks$b <- if(input$trend2){
      TRUE
    }else{
      FALSE
    }
  })
  observeEvent(input$trend3, priority = 2, {
    checks$c <- if(input$trend3){
      TRUE
    }else{
      FALSE
    }
  })
  observeEvent(input$frequency, priority = 2, {
    checks$b <- if(input$frequency!=1){
      TRUE
    }else{
      FALSE
    }
    checks$c <- if(input$frequency!=1){
      TRUE
    }else{
      FALSE
    }
  })
  
  # transformed.with.trend <- reactiveVal(NA)
  reg.mean <- reactiveVal(NA)
  reg <- reactiveVal(NA)
  when <- reactiveVal("no trend removal")
  #assign the transformation
  observeEvent(label = 'assign transformation',
               {input$trend
                 input$trend1
                 input$trend2
                 input$trend3
                 input$frequency
                 input$log
                 input$diff
                 input$diff2
                 input$seas
                 input$seas_diff2}, priority = 1, {
                   # are these the correct values to include ^ ?
                   # theres no updates here so adding more couldnt hurt
                   # i say this when adding the trend2 and trend3 variables, because of this, i needed to add a validate call below
                   
                   warning(checks$a)
                   warning(checks$b)
                   warning(checks$c)
                   #the training portion of the data,
                   data <- ts(data()[1:(length(data())-12)], frequency = ifelse(frequency(data())==1, input$frequency, frequency(data())))
                   
                   # data <- ifelse(frequency(data())==1, data()[1:(length(data())-12)], ts(data()[1:(length(data())-12)], frequency = frequency(data())))
                   
                   if(!(input$trend2 == FALSE & input$trend3 == FALSE)){
                     if(!(input$sets %in% c('monthly','temperature', 'soi'))){
                       validate(need(input$frequency!=1, label=''))
                     }
                   }
                   
                   #assign transformation that removes trend/mean if it was done first
                   if(sum(c(checks$a, checks$b, checks$c)) == 0 | sum(c(input$log, input$diff, input$seas_diff)) == 0){
                     if(input$trend1 | input$trend2 | input$trend3){
                       warning("TIT1")
                       when("first")
                       data <- ts(data, frequency = ifelse(frequency(data())==1, input$frequency, frequency(data())))
                       if(!(input$trend1 == FALSE & input$trend2 == FALSE & input$trend3 == FALSE)){
                         if(input$trend1){
                           mean <- time(data) - mean(time(data))
                         }else if(input$trend2){
                           mean <- season(x=data)
                         }else if(input$trend3){
                           mean <- harmonic(x=data, m = ifelse(frequency(data())==1, input$frequency, frequency(data())) /2)
                         }
                         reg.mean(mean)
                         reg(lm(data~mean-1))
                         transformed(ts(reg()$residuals, frequency = ifelse(frequency(data())==1, input$frequency, frequency(data()))))
                         data <- transformed()
                       }
                       checks$a <- FALSE
                       checks$b <- FALSE
                       checks$c <- FALSE
                     }
                   }
                   
                   #assign other types of transformations
                   if(input$log==TRUE){
                     if(input$diff==TRUE){
                       if(input$diff2==TRUE){
                         if(input$seas_diff==TRUE){
                           if(input$seas_diff2==TRUE){
                             # 2nd diff of log and then 2nd seasonal diff
                             transformed(diff(diff(log(data), differences = 2), lag = input$seas, differences = 2))
                           }else{
                             # 2nd diff of log and then 1st seasonal diff
                             transformed(diff(diff(log(data), differences = 2), lag = input$seas))
                           }
                         }else{
                           # 2nd diff of log
                           transformed(diff(log(data), differences = 2))
                         }  
                       }else{
                         if(input$seas_diff==TRUE){
                           if(input$seas_diff2==TRUE){
                             # 1st diff of log and then 2nd seasonal diff
                             transformed(diff(diff(log(data)), lag = input$seas, differences = 2))
                           }else{
                             # 1st diff of log and then 1st seasonal diff
                             transformed(diff(diff(log(data)), lag = input$seas))
                           }
                         }else{
                           # 1st diff of log
                           transformed(diff(log(data), differences = 1))
                         }
                       }
                     }else{
                       if(input$seas_diff==TRUE){
                         if(input$seas_diff2==TRUE){
                           # 2nd seasonal diff of log
                           transformed(diff(log(data), lag = input$seas, differences = 2))
                         }else{
                           # 1st seasonal diff of log
                           transformed(diff(log(data), lag = input$seas))
                         }
                       }else{
                         # log of data
                         transformed(log(data))
                       }
                     }
                     
                   }else if(input$diff==TRUE){
                     if(input$diff2==TRUE){
                       if(input$seas_diff==TRUE){
                         if(input$seas_diff2==TRUE){
                           # 2nd diff and then 2nd seasonal diff
                           transformed(diff(diff(data, differences = 2), lag = input$seas, differences = 2))
                         }else{  
                           # 2nd diff and then 1st seasonal diff
                           transformed(diff(diff(data, differences = 2), lag = input$seas))
                         }
                       }else{
                         # 2nd diff
                         transformed(diff(data, differences = 2))
                       }  
                     }else{
                       if(input$seas_diff==TRUE){
                         if(input$seas_diff2==TRUE){
                           # 1st diff and then 2nd seasonal diff
                           transformed(diff(diff(data), lag = input$seas, differences = 2))
                         }else{
                           # 1st diff and then 1st seasonal diff
                           transformed(diff(diff(data), lag = input$seas))
                         }
                       }else{
                         # 1st diff
                         transformed(diff(data))
                       }
                     }
                     
                   }else if(input$seas_diff==TRUE){
                     if(input$seas_diff2==TRUE){
                       # 2nd seasonal diff
                       transformed(diff(data, lag = input$seas, differences = 2))
                     }else{
                       # 1st seasonal diff
                       transformed(diff(data, lag = input$seas))
                     }
                   }
                   
                   
                   #assign transformation that removes trend/mean if it WASNT done first (if it was just recently entered)
                   if(sum(c(checks$a, checks$b, checks$c)) > 0){
                     when("last")
                     warning("TIT2")
                     data <- transformed()
                     if(!(input$trend1 == FALSE & input$trend2 == FALSE & input$trend3 == FALSE)){
                       if(input$trend1){
                         mean <- time(data) - mean(time(data))
                       }else if(input$trend2){
                         mean <- season(x=data)
                       }else if(input$trend3){
                         mean <- harmonic(x=data, m = ifelse(frequency(data())==1, input$frequency, frequency(data())) /2)
                       }
                       reg.mean(mean)
                       reg(lm(data~mean-1))
                       transformed(ts(reg()$residuals, frequency = ifelse(frequency(data())==1, input$frequency, frequency(data()))))
                     }
                     checks$a <- FALSE
                     checks$b <- FALSE
                     checks$c <- FALSE
                   }
                   
                   if(sum(c(input$trend1, input$trend2, input$trend3))==0){
                     when("no trend removal")
                     reg(NA)
                     reg.mean(NA)
                   }
                   
                   if(input$log == FALSE & input$diff == FALSE & input$seas_diff == FALSE & when()!= "no trend removal"){
                     when("first")
                   }
                 })
  
  ylabel <- eventReactive(input$sets, {
    if(input$sets=="internet"){
      "Traffic Data in Bits"
    }else if(input$sets=="monthly"){
      "Number of Fatalities"
    }else if(input$sets=="varve"){
      "Deposit Thickness"
    }else if(input$sets=="sheep"){
      "Population in millions"
    }else if(input$sets=="soi"){
      "Monthly index"
    }else if(input$sets=="temperature"){
      "Daily Maximum Temperature (F)"
    }
  })
  
  #### plots step1 ####
  output$original <- renderPlot({
    validate(need(input$sets!="Choose",message=''))
    if(input$sets == "internet"){
      X <- read.table("internet.traffic.csv", sep=",", header = T)
      hours <- as.POSIXct(X$Time, format = "%m/%d/%Y %H:%M")
      plot(hours[1:(length(data())-12)], data()[1:(length(data())-12)], type="l", main = "Hourly Internet Data (in Bits) from an ISP in the UK",
           xlab = "Time", ylab = ylabel())
    }else if(input$sets == "monthly"){
      plot.ts(ts(data()[1:(length(data())-12)], frequency = 12, start = 1960),
              main = "Monthly Traffic Fatalities in Ontario between 1960 and 1974",
              ylab = ylabel(), xlab = "year")
    }else if(input$sets == "varve"){
      plot.ts(ts(data()[1:(length(data())-12)], frequency = 1, start = -9823) , main = "Sedimentary deposit (sand and silt from melting glaciers) thicknesses every year in Massachusetts",
              ylab = ylabel(), xlab = "year")
    }else if(input$sets == "sheep"){
      plot.ts(ts(data()[1:(length(data())-12)], frequency = 1, start = 1867) , main = "Sheep Population (in millions) for England and Wales",
              ylab = ylabel(), xlab = "year")
    }else if(input$sets == "soi"){
      plot.ts(ts(data()[1:(length(data())-12)], frequency = 12, start = 1950), main = "Southern Oscillation Index (monthly data)",
              ylab = ylabel(), xlab = "year")
    }else if(input$sets == "temperature"){
      plot.ts(ts(data()[1:(length(data())-12)], frequency = 365, start = 2001) , main = "Daily Maximum Temperature in State College, Pa",
              ylab = ylabel(), xlab = "year")
    }else{
      plot.ts(data()[1:(length(data())-12)], ylab=NULL)
    }
  })
  
  observeEvent(input$sets,{
    validate(need(input$sets!="Choose", label=''))
    
    citation <- if(input$sets == "internet"){
      "https://datamarket.com/data/set/232h/internet-traffic-data-in-bits-from-an-isp-aggregated-traffic-in-the-united-kingdom-academic-network-backbone-it-was-collected-between-19-november-2004-at-0930-hours-and-27-january-2005-at-1111-hours-hourly-data#!ds=232h&display=line"
    }else if(input$sets == "monthly"){
      "https://datamarket.com/data/set/22ty/monthly-traffic-fatalities-in-ontario-1960-1974#!ds=22ty&display=line"
    }else if(input$sets == "varve"){
      "http://anson.ucdavis.edu/~shumway/tsa.html"
    }else if(input$sets == "sheep"){
      "https://datamarket.com/data/set/22px/annual-sheep-population-1000s-in-england-wales-1867-1939#!ds=22px&display=line"
    }else if(input$sets == "soi"){
      "http://db.ucsd.edu/static/TimeSeries.pdf"
    }else{
      NA
    }
    
    output$cite <- renderUI({
      addPopover(session = session, id="original", title="Citation", content=sprintf("Many of these data sets were taken from the text book used in PSU's time series classes (Time series analysis and its applications, Stoffer and Shumway). %s", ifelse(is.na(citation), "", sprintf("Further documentation can be found here: %s", citation))), placement = "left")
    })
  })
  
  output$transform <- renderPlot({
    # warning(input$log)
    # warning(input$diff)
    # warning(input$diff2)
    
    validate(need(!(input$trend1 == FALSE & input$trend2 == FALSE & input$trend3 == FALSE & input$log == FALSE & input$diff == FALSE & input$diff2 == FALSE & input$seas_diff == FALSE), message='Choose a transformation to be displayed below'))
    warning("plot transformation")
    transformed_data <- ts(transformed(), frequency = ifelse(frequency(data())==1, input$frequency, frequency(data())), start = if(input$sets=="temperature"){2001}else if(input$sets=="monthly"){1960}else if(input$sets=="sheep"){1867}else if(input$sets=="varve"){-9823}else{1})
    plot.ts(transformed_data) #, ylab=NULL)
  })
  
  observeEvent(
    {input$trend
      input$trend1
      input$trend2
      input$trend3}, {
        # output$trendwarning <- renderUI({
        warning(disabled_())
        if(disabled_()=="YES"){
          addPopover(session, id="div", title="warning", content="If you select 'remove trend' then you must select a method of trend removal", placement = "top")
        }else if(disabled_()=="NO"){
          removePopover(session, id="div")
        }
      })
  
  # output$trendwarning <- renderText({
  #   warning(disabled_())
  #   if(disabled_()=="YES"){
  #     "If you select 'remove trend' then you must select a method of trend removal"
  #   }else if(disabled_()=="NO"){
  #     "FCUCK"
  #   }else{
  #     "FUCK"
  #   }
  # })
  
  
  #### step2 code ####
  counter <- reactiveValues(a=0,b=0)
  # when the model doesnt change after we leave the 3rd tab (step3) and also still hasnt changed when the user comes back to the 3rd tab, we dont want to recalculate the same fit, so we create "counter" reactive values to account for this in the fit() reactive handler and the model() observation handler
  # {idk if this still applies i changed the counter code since}:  must keep in mind that (when both fit and model are created in a reactive handler) though we want model list (counter$a) to govern what happens, its hard when you consider the fact that it is called inside the reactive call to fit (since reactive handlers are "lazy")
  
  model <- reactiveVal(NULL)
  observeEvent(label = 'model()',
               {input$log
                 input$trend
                 input$trend1
                 input$trend2
                 input$trend3
                 input$diff
                 input$diff2
                 input$p.order
                 input$q.order
                 input$P.order
                 input$Q.order
                 input$seas_diff
                 input$seas_diff2
                 input$seas
                 input$period}, {
                   
                   if(counter$a == counter$b){
                     counter$a <- counter$a + 1
                   }
                   trend <- if(input$trend){
                     if(input$trend1){1}else if(input$trend2){2}else if(input$trend3){3}
                   }else{0}
                   
                   model(list(ifelse(input$log==TRUE,"yes","no"),
                              c(input$p.order, ifelse(input$diff==TRUE, ifelse(input$diff2==TRUE, 2, 1), 0), input$q.order),
                              c(input$P.order, ifelse(input$seas_diff==TRUE, ifelse(input$seas_diff2==TRUE, 2, 1), 0), input$Q.order),
                              ifelse( (input$seas_diff | input$trend2 | input$trend3), if(input$period==0){if(input$trend2 | input$trend3){input$frequency}else{input$seas}}else{input$period}, input$period),
                              trend)
                   )
                 })
  
  #### plots step2 ####
  output$ACF <- renderPlot({
    transformed_data <- transformed()
    acf(transformed_data, lag.max = 60, main = "Autocorrelation Function of the Transformed data")
  })
  output$PACF <- renderPlot({
    transformed_data <- transformed()
    pacf(transformed_data, lag.max = 60, main = "Partial Autocorrelation Function of the Transformed data")
  })
  output$subsets <- renderPlot({
    transformed_data <- transformed()
    plot(armasubsets(transformed_data, nar=9, nma=9), scale = "AIC")
  })
  
  #### step3 code ####
  fit.holder <- reactiveVal(NULL)
  fit <- reactive({
    #this arima function calculation is burdensome and time consuming, so we use validate
    warning(counter$a, counter$b, "  FIT REACTIVE")
    validate(need(input$tabs2 == "step3", label=''))
    
    if(counter$a == counter$b){
      fit.holder()
    }else{
      counter$b <- counter$b + 1
      
      # if(when()=="no trend removal"){
      if(input$trend1 == FALSE & input$trend2 == FALSE & input$trend3 == FALSE){
        data <- as.numeric(data()[1:(length(data())-12)])
        if(model()[[1]]=="yes"){
          data <- log(data)
        }
        fit.holder(tryCatch(arima(data, order=model()[[2]], seasonal = list(order=model()[[3]], period=model()[[4]]), method = "ML"), error=function(e)
          tryCatch(arima(data, order=model()[[2]], seasonal = list(order=model()[[3]], period=model()[[4]]), method = "CSS-ML"), error=function(e)
            tryCatch(arima(data, order=model()[[2]], seasonal = list(order=model()[[3]], period=model()[[4]]), method="CSS"))
          )
        ))
      }else{
        data <- ts(transformed(), frequency = ifelse(frequency(data())==1, input$frequency, frequency(data())))
        if(when()=="first"){
          data <- reg()$residuals
          if(input$trend1 == TRUE){
            if(model()[[1]]=="yes"){
              data <- log(data)
            }
            fit.holder(arima(data, order=model()[[2]], seasonal = list(order=model()[[3]], period=model()[[4]]), xreg=as.numeric(reg.mean()), include.mean = T, method = "ML"))
          }else{
            #AND YOU CANT DO log() after getting residuals from regression because residuals are standardized (zero mean)
            fit.holder(arima(data, order=model()[[2]], seasonal = list(order=model()[[3]], period=model()[[4]]), include.mean = TRUE, method="ML"))
          }
        }else if(when()=="last"){
          if(input$trend1 == TRUE){
            fit.holder(arima(transformed(), order=c(model()[[2]][1], 0, model()[[2]][3]), 
                             seasonal = list(order=c(model()[[3]][1], 0, model()[[3]][3]), period=model()[[4]]), 
                             xreg = as.numeric(reg.mean()), include.mean = TRUE, method="ML"))
          }else{
            fit.holder(arima(transformed(), order=c(model()[[2]][1], 0, model()[[2]][3]), 
                             seasonal = list(order=c(model()[[3]][1], 0, model()[[3]][3]), period=model()[[4]]), 
                             include.mean = TRUE, method="ML"))
          }
        }else{
          stop("VERY BAD")
        }
      }
      fit.holder()
    }
  })
  
  output$forecast <- renderPlotly({
    validate(need(input$tabs2 == "step3", message="Calculating"))
    validate(need(T, message="Calculating"))
    
    warning("ITS FUCKING HAPPENING")
    warning(when(), "when()")
    
    # CREATE PREDICTIONS OBJECT
    if(input$trend1 == FALSE & input$trend2 == FALSE & input$trend3 == FALSE){
      predictions <- predict(fit(), 12)$pred
    }else{
      data <- ts(data(), frequency = ifelse(frequency(data())==1, input$frequency, frequency(data())))
      if(input$trend1 == TRUE){
        reg.mean()
        predictions <- predict(fit(), 12, newxreg = data[(length(data)-11):length(data)])$pred 
      }else{
        start <- (length(data())-12) %% frequency(data())
        warning(start)
        predictions <- predict(fit(), 12)$pred + reg()$coef[(c(1:12)+start)]
      }
    }
    #now we need to manually calculate predictions (if the trend was removed)
    #...AND WE CREATE THE FORE OBJECT IN THIS CONDITIONAL TREE
    if(when()=="last" & input$trend1==FALSE){
      #when seasonal trend removed last
      #we need a manual calculation to convert the predictions if the trend was removed last because of the (transformed) data that fed to the arima fit
      data <- as.numeric( data()[1:(length(data())-12)] )
      if(input$log == TRUE){
        data <- log(data)
      }
      
      if(input$diff2 == TRUE & input$seas_diff2 == TRUE){
        fore <- c('', predictions)
        fore <- diffinv(diffinv(fore, lag=input$seas, differences=2, xi=diff(data, differences=2)[1:(input$seas*2)]), differences = 2, xi=data[1:2])
      }else if(input$diff == TRUE & input$seas_diff2 == TRUE){
        fore <- c('', predictions)
        fore <- diffinv(diffinv(fore, lag=input$seas, differences=2, xi=diff(data, differences=1)[1:(input$seas*2)]), differences = 1, xi=data[1])
      }else if(input$diff2 == TRUE & input$seas_diff == TRUE){
        fore <- c('', predictions)
        fore <- diffinv(diffinv(fore, lag=input$seas, differences=1, xi=diff(data, differences=2)[1:(input$seas)]), differences = 2, xi=data[1:2])
      }else if(input$diff == TRUE & input$seas_diff == TRUE){
        fore <- c('', predictions)
        fore <- diffinv(diffinv(fore, lag=input$seas, differences=1, xi=diff(data, differences=1)[1:(input$seas)]), differences = 1, xi=data[1])
      }else if(input$seas_diff == TRUE){
        if(input$seas_diff2 == TRUE){
          fore <- c('', predictions)
          fore <- diffinv(fore, lag=input$seas, differences=2, xi=data[1:(input$seas*2)])
        }else{
          fore <- c('', predictions)
          fore <- diffinv(fore, lag=input$seas, differences=1, xi=data[1:(input$seas)])
        }
      }else if(input$diff == TRUE){
        if(input$diff2 == TRUE){
          fore <- c('', predictions)
          fore <- diffinv(fore, differences=2, xi=data[1:2])
        }else{
          warning("diffinv")
          fore <- c(diff(data), predictions)
          fore <- diffinv(fore, differences=1, xi=data[1])
        }
      }
      
      if(input$log == TRUE){
        fore <- exp(fore)
      }
    }else{
      if(input$log == TRUE){
        warning(9)
        fore <- c(data()[1:(length(data())-12)], exp(predictions))
      }else{
        warning(10)
        fore <- c(data()[1:(length(data())-12)], predictions)
      }
    }
    
    warning(length(fore))
    warning(length(data()))
    fore <- as.numeric(fore)
    data <- as.numeric(data())
    time <- as.numeric(time(data()))
    df <- data.frame(fore, data, time) #c(1:length(data)))
    colnames(df) <- c("prediction", "observation", "time")
    if(length(data())>100){df<-df[((nrow(df)-100):nrow(df)), ]}
    df %>% ggplot(aes(x=time)) + geom_line(aes(y=prediction, color="prediction")) + geom_line(aes(y=observation, color="observation")) +
      geom_vline(xintercept = time[(length(data())-12)], linetype="dotted") + ggtitle("Your Forecasts") + labs(y = ylabel()) + scale_color_manual(values = c("blue", "red")) +
      labs(x = ifelse(input$sets=="internet", "Time, in Hours", sprintf("Time, in years (recall, there are %d observations per year in this data set)", frequency(data())))) +
      theme(legend.title=element_blank(), axis.title.y = element_text(angle = 45, color = "blue"), axis.title.x = element_text(color = "blue"))
  })
  #plot.ts(fit, n.ahead=12) # plot data with forecasts and 95% limits
  #+ xlim((length(data)-100), length(data))
  
  # plot.ly(
  #   df %>% ggplot(aes()) + geom_line(aes(x=time, y=prediction)) + geom_vline(xintercept = (length(data)-12), linetype="dotted") + geom_line(aes(x=time, y=observation)) + scale_color_manual(values = c("blue", "red"))
  # )
  
  
  output$fitQuality1 <- renderPlot({
    validate(need(T, message="Calculating"))
    residuals <- fit()$residuals
    validate(need(T, message="Calculating"))
    acf(residuals, lag.max = 40, main = "ACF of your ARIMA model's residuals")
  })
  output$fitQuality2 <- renderPlot({
    validate(need(T, message="Calculating"))
    residuals <- fit()$residuals
    validate(need(T, message="Calculating"))
    pacf(residuals, lag.max = 40, main = "PACF of your ARIMA model's residuals")
  })
  
  
  almost1 <- ''#youre just missing something very menial, the important stuff is there
  almost2 <- ''#arima order very close
  
  success <- "Great job!"
  notyet1 <- "Try improving the stationarity of your model"
  notyet2 <- "Try a new value for the period, or frequency for the seasonality in your model"
  notyet3 <- "Consult the acf plots and the armasubsets and try fitting better coefficients"
  
  my_if <- function(condition, ...){
    if(length(unique(condition))>1){
      return(FALSE)
    }else{
      return(unique(condition))
    }
  }
  
  feed <- reactive({
    validate(need(input$tabs2 == "step3", label = ''))
    model <- model()
    #should we just not have a success return ?
    
    if(input$sets == "internet"){
      if(my_if(model[[2]]==c(1,2,1)) & my_if(model[[3]]==c(2,1,0)) & model[[4]]==24 & model[[5]]==0 & model[[1]]=="yes"){
        success
      }else if(my_if(model[[2]]==c(2,2,1)) & my_if(model[[3]]==c(2,1,0)) & model[[4]]==24 & model[[5]]==0){
        almost # or, theyre good; success
      }else if(sum(model[[2]][2]!=2, model[[3]][2]!=1, model[[4]]!=24, model[[5]]!=0)>0){
        if(sum(model[[2]][2]!=2, model[[3]][2]!=1, model[[5]]!=0)==0 & model[[4]]!=24){
          notyet2
        }else{
          notyet1
        }
      }else{
        notyet3
      }
      
    }else if(input$sets == "monthly"){
      if(model[[5]] == 0){
        if(model[[4]] == 12){
          if(model[[1]]=="yes" & (model[[2]][2]+model[[3]][2]==3)){
            if(model[[2]][2]==1){
              "You're close, but the data could be differenced a little better (regarding stationarity)"
            }else{
              if(model[[2]][2]==5 & (model[[3]][1]+model[[3]][3]==2)){
                if(AIC(fit()) < -132){
                  success
                }else{
                  notyet3
                }
              }else{
                notyet3
              }
            }
          }else{
            notyet1
          }
        }else{
          notyet2
        }
      }else if(model[[5]] %in% c(2,3)){
        if(when()=="last" | sum(input$log == FALSE, input$diff == FALSE, input$seas_diff == FALSE, when()=="first")==4){
          if(model[[1]]=="yes" & model[[3]][2]==0 & model[[2]][2]==1){
            if(my_if(model[[2]]==c(0,1,1)) & my_if(model[[3]]==c(0,0,0))){
              success
            }else{
              notyet3
            }
          }else{
            notyet1
          }
        }else{
          "Recall, the order that you select the transformations matters"
        }
      }else{
        notyet1
      }
      
    }else if(input$sets == "varve"){
      if(model[[1]]=="yes" & model[[2]][2]==1 & model[[5]]==0 & model[[3]][2]==0){
        if(model[[4]] == 7){
          if(model[[2]][1]==2 & my_if(model[[3]]==c(2,0,0))){
            if(my_if(model[[2]]==c(2,2,2)) | my_if(model[[2]]==c(2,1,1))){
              success
            }else{
              #close close close
              notyet3
            }
          }
        }else if(model[[4]]==0){
          if(my_if(model[[2]] == c(1,1,1)) & my_if(model[[3]] == c(0,0,0))){
            success
          }else{
            notyet3
          }
        }else{
          notyet2
        }
      }else{
        notyet1
      }
      
    }else if(input$sets == "sheep"){
      if(model[[1]]=="no" & model[[2]][2]==1 & model[[5]]==0 & model[[3]][2]==0 & model[[4]]==0){
        if(model[[4]] == 0){
          if(my_if(model[[2]]==c(3,1,0)) & my_if(model[[3]]==c(0,0,0))){
            success
          }else{
            #close close close
            notyet3
          }
        }else{
          notyet2
        }
      }else{
        notyet1
      }
    }else if(input$sets == "soi"){
      if(model[[1]]=="no" & model[[2]][2]==0 & model[[3]][2]==1 & model[[5]]==0){
        if(model[[4]] == 12){
          if(my_if(model[[3]]==c(0,1,1)) & sum(my_if(model[[2]]==c(9,0,0)), my_if(model[[2]]==c(3,0,1)))==2){
            success
          }else{
            notyet3
          }
        }else{
          notyet2
        } 
      }else{
        notyet1
      }
      
    }else if(input$sets == "temperature"){
      if(when()=="first"){
        if(model[[1]]=="no" & model[[2]][2]==0 & model[[3]][2]==0 & model[[5]] %in% c(2,3)){
          if(my_if(model[[2]]==c(3,0,1)) & my_if(model[[3]]==c(0,0,0))){
            success
          }else{
            #close close close
            notyet3 
          }
        }else{
          notyet1
        }
      }else if(when()=="last"){
        notyet2 #?
      }
    }
  })
  
  output$feedback <- renderText({
    validate(need(T, message="Calculating"))
    feed()
  })
  
  output$bar <- renderImage({
    validate(need(input$tabs2 == 'step3', message="Calculating"))
    validate(need(T, message="Calculating"))    
    
    # data <- data()[1:(length(data())-12)]
    # if(input$log){
    #   data <- log(data)
    # }
    zero <- AIC(arima(data()[1:(length(data())-12)], order=c(0,0,0), method="ML"))
    warning(zero, "zero")
    hundred <- if(input$sets=="internet"){
      -4300
    }else if(input$sets=="monthly"){
      #should I have two different calls here?
      if(model()[[5]] %in% c(2,3)){
        -221
      }else{
        -134
      }
    }else if(input$sets=="varve"){
      858
    }else if(input$sets=="sheep"){
      684
    }else if(input$sets=="soi"){
      93
    }else if(input$Sets=="temperature"){
      23963
    }
    
    entry <- AIC(fit())
    warning(entry, "AIC(fit)")
    
    if(entry > zero){
      place <- 2
    }else if(entry < hundred){
      place <- 100
    }else{
      span <- zero-hundred #because the lower the AIC the better, negative infinity being perfect
      place <- (zero-entry)/span
      place <- place * 100
      place <- ifelse((98 < place) & (place < 100), 
                      98,
                      ifelse(place <= 1.5, 1, (round(place, digits=0)))
      )
      # if(feed()==notyet1){
      #   #penalize poor fits because of nature of some of these span lengths
      #   place <- place - 33
      # }else if(feed()==notyet2){
      #   place <- place - 25
      # }
      if(place <= 1.5){
        place <- 2
      }
    }
    warning(place, "place")
    
    #A temp file to save the output.
    #This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.png')
    
    #Generate the PNG
    png(outfile, width = 400, height = 300,bg = "transparent")
    image(1:place, 1:10, matrix(9, place, 10), asp=1, xlim=c(0,100), ylim=c(0,57),
          xaxt="n", yaxt="n", xlab="", ylab="", frame=F, axes=F)
    # segments(c(0.3,0.3,0.3,10.3), c(10.3,0.3,0.3,0.3),
    #          c(100,100,0.3,0.3), c(10.3,0.3,10.3,0.3), lwd=3, col=gray(0.1))
    rect(0,0,100,10.3, lwd=3)
    axis(1, at = seq(0, 100, by = 10)) #, labels=("Percentage to the optimal model fit's AIC"))
    text("Percentage to the optimal model fit's AIC", x=50, y=20, cex=1.2)
    dev.off()
    
    #Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
    
  }, deleteFile = TRUE)
  
  ######tic-tac-toe
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Click on desired square, answer the question, then hit submit and go to next question.",
      type = "info"
    )
  })
  
  
  #### question bank ####
  bank <- read.csv('questions.csv', stringsAsFactors = FALSE)
  bank <- bank[,c(1:3,5:10,12)]
  Qs <- nrow(bank)
  
  
  ######## MY SERVER CODE ##########
  
  #setwd("~/PSU (courses,etc) & Act-Sci (exams, etc)/shiny research/BOAST-EstimationTesting shiny")
  #the coordinates (sizing attributes) below are hard coded to work with the X and O images that I created/am currently using
  X.icon <- makeIcon(iconUrl = 'X.PNG', iconWidth = 95) #, iconHeight = 95)
  O.icon <- makeIcon(iconUrl = 'O.PNG', iconWidth = 105) #, iconHeight = 105)
  value <- matrix(rep(-3,9),3,3)
  values <- list(value) # 
  container <- c() # contains right or wrong answers after submit button is pressed 
  XsAndOs <- list()
  resolved <- c(rep(FALSE, Qs))
  #lists <- reactiveValues(container <- c(),values <- list(),value <- NULL)
  sr1.1=Polygon(cbind(c(0.5,0.5,3.5,3.5), c(2.5,1.5,1.5,2.5))) # inner, middle horizontal lines
  sr1.2=Polygon(cbind(c(0.5,0.5,3.5,3.5), c(2.5,3.5,3.5,2.5))) # inner, horizontal lines
  sr1.3=Polygon(cbind(c(0.5,0.5,3.5,3.5), c(0.5,1.5,1.5,0.5))) # inner, horizontal lines
  sr2=Polygon(cbind(c(1.5,1.5,2.5,2.5), c(3.5,0.5,0.5,3.5)))
  sr3=Polygon(cbind(c(0.5,0.5,0.5,3.5), c(0.52,3.47,0.5,0.5))) #outer 
  sr4=Polygon(cbind(c(3.5,3.5,0.5,3.5), c(0.52,3.47,3.5,3.5))) #square
  srs1.1=Polygons(list(sr1.1), 's1.1')
  srs1.2=Polygons(list(sr1.2), 's1.2')
  srs1.3=Polygons(list(sr1.3), 's1.3')
  srs2=Polygons(list(sr2), 's2')
  srs3=Polygons(list(sr3), 's3')
  srs4=Polygons(list(sr4), 's4')
  #srs1.2,srs1.3
  spp = SpatialPolygons(list(srs1.1,srs2,srs3,srs4), 1:4)
  #spp = SpatialPolygons(list(srs3,srs4), 1:2)
  
  r <- raster(xmn = 0.5, xmx = 3.5, ymn = 0.5, ymx = 3.5, nrows = 3, ncols = 3)
  values(r) <- matrix(1:9, nrow(r), ncol(r), byrow = TRUE)
  crs(r) <- CRS("+init=epsg:4326")
  new.board <- leaflet(options = leafletOptions(zoomControl = FALSE, doubleClickZoom = FALSE, minZoom = 7, maxZoom = 7, dragging = FALSE)) %>%  addPolygons(data=spp) %>% addRasterImage(r, colors="Set3")
  # %>% addTiles()
  line <- function(){
    #recall, in the value matrix, zero's represent O's, or wrong answers
    for(i in 1:3){
      total.1 <- 0 ; total.2 <- 0
      for(j in 1:3){
        total.1 <- total.1 + value[i, j]
        total.2 <- total.2 + value[j, i]
      }
      if(total.1==0 | total.2==0 | total.1==3 | total.2==3){
        break
      }
    }
    total.3 <- value[1, 1] + value[2, 2] + value[3, 3]
    total.4 <- value[1, 3] + value[2, 2] + value[3, 1]
    
    #if the game has been won:
    if(total.1==0 | total.2==0 | total.3==0 | total.4==0 | total.1==3 | total.2==3 | total.3==3 | total.4==3){
      #place.na[!is.na(place.na)] <<- NA
      if(total.1==0 | total.2==0 | total.3==0 | total.4==0){
        warning('LOSE')
        return("You Lose !")
        #return(paste("<span style=\"color:red\">You Lose !</span>"))
        #title(sub=list("You Lose !", col="darkblue", font=2, cex=2.5), line=2)
        
      }else{
        warning('WIN')
        return("You Win ! Game Over !")  #title(sub=list("You Win ! Game Over !", col="red", font=2, cex=2.5), line=2)
        
      }
    }
    
    #if the previous is true (if the game is over) then this will fire through as well
    #this will also fire through if the board is full, regardless of whether the previous if() has executed
    #i dont know why these two statements should both be here
    if(length(which(value!=-3))==9){
      if(total.1==0 | total.2==0 | total.3==0 | total.4==0 | total.1==3 | total.2==3 | total.3==3 | total.4==3){
        #if(total.1==0 | total.2==0 | total.3==0 | total.4==0){
        #  title(sub=list("You Are a Loser !", col="darkblue", font=2, cex=2.5), line=2)
        #}else{
        #  title(sub=list("You Win ! Game Over !", col="orange", font=2, cex=2.5), line=2)
        #}
      }else{
        warning('RESTART')
        return("Draw ! Please try again !")  #title(sub=list("Draw ! Please try again !", col="blue", font=2, cex=2.5), line=2)
      }
    }
    return(NULL)
  }
  
  
  
  v <- reactiveValues(doPlot = FALSE)
  observeEvent(input$image_click, priority = 7, {
    validate(need(is.null(game()), label='Game is over'))
    
    #input$image_click will return coordinates when clicked, not a logical value
    if(!is.null(input$image_click)){
      v$doPlot <- TRUE
    }
  })
  
  #object that contains the coordinates returned from clicking the image
  coords <- eventReactive(input$image_click, {
    validate(need(is.null(game()), label='Game is over'))
    
    validate(need(!is.null(input$image_click), 'need to click image'))
    input$image_click
  })
  
  
  #### keep track of clicks on A: the plot, B: the submit button, C: the next button ####
  clicks <- reactiveValues(A=0,B=0,C=0)
  observeEvent(input$image_click, priority = 6, {
    validate(need(is.null(game()), label='Game is over'))
    mouse.at <- coords()
    output$warning <- renderText('')
    #we dont return an error for out of bounds clicks
    if(!(mouse.at[[1]] > 3.5 | mouse.at[[1]] < 0.5 | mouse.at[[2]] > 3.5 | mouse.at[[2]] < 0.5)){
      if(!is.null(input$image_click) & clicks$A==clicks$B){
        #we allow the user to switch his tic tac toe selection (before a corresponding answer) here
        clicks$A <- (clicks$A+1)
        warning(clicks$A, "A")
      }
    }else{
      if(clicks$A == clicks$B){
        output$warning <- renderText('Please click a valid square')
      }
    }
  })
  observeEvent(input$submit, priority = 6, {
    # (validate) make sure the player selected an answer when he pressed submit
    num <- as.character(numbers$question[length(numbers$question)])
    # DIRECTLY BELOW: YOU WILL SEE THIS EXACT SAME WARNING 5 TIMES TOTAL IN THIS SERVER. It refers to the following line directly below, which will be the EXACT SAME `ans <- ` line of code each time
    # this (directly below) has been hard coded to correspond with the number of questions in the question bank
    ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}
    else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}else if(num==29){input$'29'}else if(num==30){input$'30'}else if(num==31){input$'31'}else if(num==32){input$'32'}else if(num==33){input$'33'}else if(num==34){input$'34'}else if(num==35){input$'35'}else if(num==36){input$'36'}else if(num==37){input$'37'}else if(num==38){input$'38'}else if(num==39){input$'39'}else if(num==40){input$'40'}
    
    # also, the 100 max set below in seq() is hard coded for certain probability questions I ask in the question bank, if you have questions with numeric inputs, this may need to be changed (unless input associating input attributes are changed instead)
    validate(need((ans %in% c("A", "B", "C", "D","E", seq(0:100))), label='please select one of the multiple choice responses'))
    
    if(!is.null(input$submit)){clicks$B <- (clicks$B+1)}
    warning(clicks$B, 'b')
  })
  observeEvent(input$nextButton, priority = 6, {
    if(!is.null(input$nextButton)){clicks$C <- (clicks$C+1)}
    warning(clicks$C, 'c')
    output$Feedback <- renderText({''})
  })
  
  #called in the next observer below 
  ID <- reactive({
    validate(need(is.null(game()), label='Game is over'))
    
    validate(
      need(!is.null(input$image_click), 'click image') # because this will always come first
    )
    if(clicks$A>=clicks$B){
      if(clicks$A>clicks$B){
        "choice"
      }else if(clicks$A==clicks$B){
        #if the first few clicks are not valid squares, we must make sure theres no errors in the next observe handler below
        if(clicks$A==0){
          "choice"
        }else{
          "answer"
        }
      }
    }
  })
  # observe({
  #   updateButton(session, "feedback",disabled = T)
  # })
  game <- reactiveVal(NULL)
  #contains the board as it changes throughout the game
  #will store markers in XsAndOs list
  #as this event fires, only one marker will be added to the XsAndOs list each time
  #the first if statement will store an empty marker, when a box is chosen. Then,
  #then the second will overwrite it with an X or an O, when a question is answered
  observeEvent({input$submit
    input$image_click}, priority = 0,{
      validate(need(is.null(game()), label='Game is over'))
      
      mouse.at <- coords()
      ID <- ID()
      
      #if the user enters an invalid click after he already entered his choice on the board, it will not affect the outcome of an answer submission
      if(clicks$A > clicks$B | clicks$A == 0){
        validate(
          need(!(mouse.at[[1]] > 3.5 | mouse.at[[1]] < 0.5 | mouse.at[[2]] > 3.5 | mouse.at[[2]] < 0.5), label='please enter a valid click')
        )
      }else if(clicks$A == clicks$B & ID == "answer" & XsAndOs[[ifelse(length(XsAndOs)==0,'',length(XsAndOs))]][3] %in% c(0,1) ){
        #or, if the user enters an invalid click before entering a valid click, it will not crash the app
        if(length(XsAndOs) == clicks$A){
          validate(
            need(!(mouse.at[[1]] > 3.5 | mouse.at[[1]] < 0.5 | mouse.at[[2]] > 3.5 | mouse.at[[2]] < 0.5), label='please enter a valid click')
          )
        }
      }
      
      #statement that checks the ID return from either of the input triggering events. choice or answer
      if(ID=="choice"){
        mouse.at[[1]] <- round(mouse.at[[1]])
        mouse.at[[2]] <- round(mouse.at[[2]])
        x<<-mouse.at[[1]]
        y<<-mouse.at[[2]]
        
        #     should this be length(XsAndOs) ?  6/14/18
        XsAndOs[[(length(container)+1)]] <<- c(mouse.at[[2]],(mouse.at[[1]]-.3),NULL)
        leafletProxy("image", session) %>% addMarkers(lng=XsAndOs[[length(XsAndOs)]][1], lat=XsAndOs[[length(XsAndOs)]][2], layerId = "dummy")
        
      }else if(ID=="answer"){
        value <<- values[[length(values)]]
        if(container[length(container)]==0){
          value[x, y] <<- 0
        }else if(container[length(container)]==1){
          value[x, y] <<- 1
        }
        values[[1]] <<- matrix(-3,3,3)
        values[[(length(values)+1)]] <<- value
        temp <<- which((values[[length(values)]] - values[[ (length(values)-1) ]])!=0)
        values[[length(values)]][temp] <<- container[length(container)]
        #plotting part, this will overwrite what previously passed through the first if statement in this observe handler
        if(temp<4){
          XsAndOs[[(length(XsAndOs))]][3] <<- ifelse(matrix(values[[length(values)]],3,3)[[temp]]==0, 0, 1)
        }else if(temp<7){ 
          XsAndOs[[(length(XsAndOs))]][3] <<- ifelse(matrix(values[[length(values)]],3,3)[[temp]]==0, 0, 1)
        }else if(temp<10){
          XsAndOs[[(length(XsAndOs))]][3] <<- ifelse(matrix(values[[length(values)]],3,3)[[temp]]==0, 0, 1)
        }else{
          stop("OH NO")
        }
        
        #the coordinates below are hard coded to work with the X and O images that I created/am currently using (the 2nd and 3rd leafletproxy calls below)
        #if you use the latest updated pictures that I used, then you should not need change any code
        leafletProxy("image", session)  %>% removeMarker(layerId = "dummy")
        warning(XsAndOs[[length(XsAndOs)]][3])
        if(!(XsAndOs[[length(XsAndOs)]][3] %in% c(0,1))){stop('VERY BAD')}
        if(XsAndOs[[length(XsAndOs)]][3]==1){
          leafletProxy("image", session)  %>% addMarkers(y-.0175,x+.61,icon = X.icon)
          #updateButton(session, "feedback",disabled = T)
        }else{
          leafletProxy("image", session)  %>% addMarkers(y+.008,x+.54,icon = O.icon)
          num <- as.character(numbers$question[length(numbers$question)])
          output$Feedback <- renderUI({
            withMathJax(
              h4(sprintf(bank[num, 10]
              ))
            )
            
          })
        }
        
        temp <- line()
        game(temp) #reactiveVal syntax
        message(temp)
      }
    }
  )  
  
  #### alerts user of status of game (if it is over) ####
  #return from line() function
  output$gameMessage <- renderText({
    validate(need(!is.null(game), label = 'Game is over'))
    
    game <- game()
    game
  })
  
  
  ####render image of tic tac toe board####
  output$image <- renderLeaflet({
    new.board
    #  out = out %>% addMarkers(lng=temp[[1]],lat=temp[[2]], icon=tryCatch(ifelse(temp[[3]]==1, X.icon, O.icon), error=function(e)NULL))
  })
  
  #### go button ####
  observeEvent(input$go, priority=1, {
    updateTabItems(session, "tabs", "qqq")
  })
  observe({
    validate(
      need(is.null(input$image_click), message='')
    )
    output$directions <- renderText({"Begin by selecting the square on the plot you would like"})
  })
  
  ####start over; new game####
  observeEvent(input$reset, priority = 5,{
    if(!is.null(game())){
      game(NULL)
    }
    leafletProxy("image", session) %>% clearMarkers()
    
    value <<- matrix(rep(-3,9),3,3)
    values <<- list(value)
    container <<- c()
    XsAndOs <<- list()
    #need to use scoping operator because i didnt create the (above) as reactive objects
    clicks$A <- 0
    clicks$B <- 0
    clicks$C <- 0
    
    
    #if there are less than 9 questions left, then we reset the question bank so that all questions can be drawn from again
    if(length(which(answers() %in% c("correct","incorrect")))>(Qs-9)){
      numbers$question <- c()
    }else{
      #put the incorrectly answered questions back into play
      if("incorrect" %in% unique(answers())){
        if("correct" %in% unique(answers())){
          #numbers$question <- numbers$question[which((answers()=="correct"))] #we assign the taken questions to be only the correctly answered ones
          numbers$question <- which(answers()=="correct") #we assign the taken questions to be only the correctly answered ones
        }else{
          numbers$question <- c() #since no answers were correct, all questions are in play
        }
      }
      #get rid of most recent question (but if it was answered correctly, it still will not be included)
      numbers$question <- numbers$question[-length(numbers$question)]
    }
    
    #resample to get new random question for when the game is restarted (when image is clicked)
    space<-c(1:Qs)
    numbers$question[(length(numbers$question)+1)] <- sample(space[-tryCatch(if(numbers$question){numbers$question}, error=function(e) (Qs+1))], 1)
    
    updateButton(session, 'nextButton', style = "color: white;", disabled = TRUE)
    output$directions <- renderText({"Begin by selecting the square on the plot you would like"})
  })
  #temporary place holders, will be used as a logical pass to the conditional panel containing the renderUI output
  observeEvent(input$image_click, {
    validate(need(is.null(game()), label='Game is over'))
    
    output$temp <- renderText({'1'})
  })
  observeEvent(input$reset, priority = 8, {
    output$Feedback <- renderText({''})
    output$temp <- renderText({'2'})
  })
  
  
  #### enact game over mode ####
  observeEvent(input$submit, priority = -1, {
    validate(need(!is.null(game()), label='Game is over'))
    
    # (validate) makes sure the player selected an answer when he pressed submit
    num <- as.character(numbers$question[length(numbers$question)])
    # this (directly below) has been hard coded to correspond with the number of questions in the question bank
    ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}
    else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}else if(num==29){input$'29'}else if(num==30){input$'30'}else if(num==31){input$'31'}else if(num==32){input$'32'}else if(num==33){input$'33'}else if(num==34){input$'34'}else if(num==35){input$'35'}else if(num==36){input$'36'}else if(num==37){input$'37'}else if(num==38){input$'38'}else if(num==39){input$'39'}else if(num==40){input$'40'}
    
    # also, the 100 max set below in seq() is hard coded for certain probability questions I ask in the question bank, if you have questions with numeric inputs, this may need to change (unless input associating input attributes are changed instead)
    validate(need((ans %in% c("A", "B", "C", "D","E", seq(0:100))), label='please select one of the multiple choice responses'))
    
    updateButton(session, 'nextButton', style = "color: white;", disabled = TRUE)
    updateButton(session, 'submit', style = "color: white;", disabled = TRUE)
    output$directions <- renderText({"If you would like to play again, press 'Start new game'!"})
  })
  
  
  ######## QUESTIONS #########
  
  
  
  
  #### random question ####
  numbers <- reactiveValues(question = c())
  observeEvent(input$image_click, once=TRUE, priority = 9, {
    validate(need(is.null(game()), label='Game is over'))
    
    numbers$question[1] <- sample(1:Qs, 1)
    updateButton(session, 'nextButton', style = "color: white;", disabled = TRUE)
    output$directions <- renderText({"Now answer the question and press submit"})
  })
  observeEvent(input$nextButton, priority = 4, {
    space <- c(1:Qs)
    numbers$question[(length(numbers$question)+1)] <- sample(space[-tryCatch(numbers$question, error=function(e) (Qs+1))], 1)
    updateButton(session, 'nextButton', style = "color: white;", disabled = TRUE)
    output$directions <- renderText({"Now select another square on the board"})
    if(clicks$A == (clicks$C + 1)){
      updateButton(session, 'submit', style = "color: white;", disabled = FALSE)
      output$directions <- renderText({"Now answer the question and press submit"})
    }
    #updateCheckboxInput(session, "feedback",label = "Show Solution", value = FALSE)
  })
  
  output$Question <- renderUI({
    num <- as.character(numbers$question[length(numbers$question)])
    
    if(num == 26){
      withMathJax(
        h4(sprintf(
          bank[num, 2]
        ))
      )
    }
    else if(num == 29){
      withMathJax(
        h4(sprintf(
          bank[num, 2]
        ))
      )
    }
    
    else{print(bank[num,2])}
  })
  ####output random question####
  output$CurrentQuestion <- renderUI({
    num <- as.character(numbers$question[length(numbers$question)])
    if(num == 23){
      withMathJax(
        h4(sprintf(
          bank[num, 4]
        ))
      )
    }
    else if(num == 37){
      withMathJax(
        h4(sprintf(
          bank[num, 4:6]
        ))
      )
    }
    temp <- NULL
    
    
    #coded for multiple choices
    if(!(FALSE %in% unique(as.vector(bank[num,6:8]=='')))){
      # ^ if the question has 2 multiple choice responses
      radioButtons(inputId = (num), label='', choiceNames=c(bank[num, 4], bank[num, 5]), choiceValues = c("A", "B"),  selected = character(0))
    }else if(!(FALSE %in% unique(as.vector(bank[num,7:8]=='')))){
      # ^ if the question has 3 multiple choice responses
      radioButtons(inputId = (num), label='', choiceNames=c(bank[num, 4], bank[num, 5], bank[num, 6]), choiceValues = c("A", "B", "C"), selected = character(0))
    }else if(bank[num,8]==''){
      # ^ if the question has 4 multiple choice responses
      radioButtons(inputId = (num), label='', choiceNames=c(bank[num, 4], bank[num, 5], bank[num, 6],bank[num, 7]), choiceValues = c("A", "B", "C","D"), selected = character(0))
    }else{
      # ^ if the question has 5 multiple choice responses
      radioButtons(inputId = (num), label='', choiceNames=c(bank[num, 4], bank[num, 5], bank[num, 6], bank[num, 7], bank[num, 8]), choiceValues = c("A", "B", "C", "D","E"), selected = character(0))
    }
  })
  
  #hard coded observer for specific questions in the csv,
  #these questions were special in that they had pictures to go along with them, or they utilized latex to write mathematical functions
  output$CurrentQuestion.extra <- renderUI({
    num <- as.character(numbers$question[length(numbers$question)])
    setwd("./")
    if(num == 5){
      img(src="5.PNG",height = 150,width = 500,align = "middle")
    }else if(num == 14){
      img(src="14.PNG",height = 150,width = 400,align = "middle")
    }else if(num == 20){
      img(src="20.PNG",height = 150,width = 400,align = "middle")
    }else if(num == 21){
      img(src="21.PNG",height = 150,width = 400,align = "middle")
    }else if(num == 26){
      img(src="27.PNG",height = 150,width = 400,align = "middle")
    }else if(num == 27){
      img(src="28.PNG",height = 150,width = 400,align = "middle")
    }else if(num == 30){
      img(src="31.PNG",height = 150,width = 400,align = "middle")
    }else if(num == 35){
      img(src="36.PNG",height = 150,width = 400,align = "middle")
    }else if(num == 36){
      img(src="37.PNG",height = 150,width = 400,align = "middle")
    }else if(num == 37){
      img(src="38.PNG",height = 150,width = 400,align = "middle")
    }
  })
  
  
  ####logical flow of answering questions; some extra code to deal with button disabling sequence####
  #these observers are for before the game is begun
  observeEvent(input$go3, {
    updateButton(session, "submit", disabled = TRUE)
  })
  observeEvent(input$go3, {
    updateButton(session, "nextButton", disabled = TRUE)
  })
  
  #these apply to if the game has already been started 
  observeEvent(input$submit, {
    # (validate) makes sure the player selected an answer when he pressed submit
    num <- as.character(numbers$question[length(numbers$question)])
    # this (directly below) has been hard coded to correspond with the number of questions in the question bank
    ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}
    else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}else if(num==29){input$'29'}else if(num==30){input$'30'}else if(num==31){input$'31'}else if(num==32){input$'32'}else if(num==33){input$'33'}else if(num==34){input$'34'}else if(num==35){input$'35'}else if(num==36){input$'36'}else if(num==37){input$'37'}else if(num==38){input$'38'}else if(num==39){input$'39'}else if(num==40){input$'40'}
    
    # also, the 100 max set below in seq() is hard coded for certain probability questions I ask in the question bank, if you have questions with numeric inputs, this may need to change (unless input associating input attributes are changed instead)
    validate(need((ans %in% c("A", "B", "C", "D","E", seq(0:100))), label='please select one of the multiple choice responses'))
    
    updateButton(session, 'submit', style = "color: white;", disabled = TRUE)
    updateButton(session, 'nextButton', style = "color: white;", disabled = FALSE)
    output$directions <- renderText({"Now press the next button"})
  })
  observeEvent(input$image_click, {
    validate(need(is.null(game()), label='Game is over'))
    
    if(!(clicks$A > (clicks$C + 1))){
      updateButton(session, 'submit', style = "color: white;", disabled = FALSE)
      output$directions <- renderText({"Now answer the question and press submit"})
    }
  })
  
  ####checks answer####
  answers <- reactiveVal(c(rep('', Qs)))
  observeEvent(input$submit, {
    #(validate) makes sure the player selected an answer when he pressed submit
    num <- as.character(numbers$question[length(numbers$question)])
    # this (directly below) has been hard coded to correspond with the number of questions in the question bank
    ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}
    else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}else if(num==29){input$'29'}else if(num==30){input$'30'}else if(num==31){input$'31'}else if(num==32){input$'32'}else if(num==33){input$'33'}else if(num==34){input$'34'}else if(num==35){input$'35'}else if(num==36){input$'36'}else if(num==37){input$'37'}else if(num==38){input$'38'}else if(num==39){input$'39'}else if(num==40){input$'40'}
    
    # also, the 100 max set below in seq() is hard coded for certain probability questions I ask in the question bank, if you have questions with numeric inputs, this may need to change (unless input associating input attributes are changed instead)
    validate(need((ans %in% c("A", "B", "C", "D","E", seq(0:100))), label='please select one of the multiple choice responses'))
    
    temp <- answers()
    num <- as.character(numbers$question[length(numbers$question)])
    # this (directly below) has been hard coded to correspond with the number of questions in the question bank
    ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}
    else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}else if(num==29){input$'29'}else if(num==30){input$'30'}else if(num==31){input$'31'}else if(num==32){input$'32'}else if(num==33){input$'33'}else if(num==34){input$'34'}else if(num==35){input$'35'}else if(num==36){input$'36'}else if(num==37){input$'37'}else if(num==38){input$'38'}else if(num==39){input$'39'}else if(num==40){input$'40'}
    
    if(ans == bank[num, 3]){
      temp2 <- "correct"
    }else{
      temp2 <- "incorrect"
    }
    temp[num] <- temp2
    answers(temp)
    container[(length(container)+1)] <<- ifelse(temp[num] == "correct", 1, 0)
    
  })
}

boastUtils::boastApp(ui = ui, server = server)