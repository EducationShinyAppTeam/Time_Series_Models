library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
#library(shinyalert)
library(plotly)
library(stats)
library(TSA)
library(ggplot2)
library(magrittr)
library(tidyr)
library(raster)
#library(rgdal)
#library(DT)
#library(leaflet)
library(raster)
library(shinyWidgets)
library(boastUtils)

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "purple",
    dashboardHeader(
      title = "Time Series Models",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Time_Series_Model")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Simulation", tabName = "sim", icon = icon("wpexplorer")),
        menuItem("Analyzing Real Data", tabName = "data", icon = icon("cogs")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    #What is the stuff below for?
    dashboardBody(
      tags$head(
        #tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
      ),
      tabItems(
        #### Set up the Overview Page ----
        tabItem(tabName = "overview",
            h1("Times Series Models"),
            p("In this app the goal is to become more familiar with time series 
            analysis."),
            
            p("In particular, the first part requires students to engage with 
            simulations so they could find out how model equations may relate to
            the graphical representation of the time series. The second feature
            walks the user through time series analysis of a selected real world
            data set. The user will make the data stationary, fit a model, and
            observe the quality of their model in this analysis."),
            br(),
            
            h2("Instructions:"),
            
            h4("Simulation Exploration:"),
            tags$ul(
              tags$li("Use the sliders for the coefficients and explore how changing
                    parameter values affects the time series plot."),
              tags$li("Use the drop down menus and observe how different orders of
                    models effect the autocorrelation function (ACF) and partial
                    autocorrelation function (PACF) plots.")
            ),
            br(), 
            h4("Time Series Analysis with real data:"),
            
            tags$li("Select the data set that you would like
                     to analyze, and fit transformations until the data seems
                     stationary."),
            tags$li("Here, you must consider that the last 12 observations of
                     each data set are hidden from the user so that they could
                     be presented in the last tab alongside the user's model's
                     forecasts."),
            tags$li("For each transformation consider the following:", 
                    tags$ul(
                      tags$li("The transformations are of the form `seas_diff(diff(log(data)))`,
                          that is, the log transformation will always be taken first,
                          followed by the difference of lag one, and then the seasonal
                          differencing."),
                      tags$li("The trend can only be removed before any other transformation,
                          or after all transformations. The trend is removed using
                          regression, and the transformed data is the residuals
                          from that regression."))
            ),
            
            br(), 
            
            br(), 
            br(), 
            div(style = "text-align: center",
                bsButton(inputId = "go0", 
                         label = "Prerequisites!",
                         icon("book"),
                         style = "default",
                         size = "large")
            ),
            
            
            br(), 
            br(),
            h2("Acknowledgements:"),
            p("This app was conceived in its entirety by Ryan Voyack and Yubaihe
            Grace Zhou in June and July of 2018 with Ryan leading on the data
            analysis and game components and Grace leading on the simulation
            components. Special thanks to Saurabh Jugalkishor Jaju from
            AnalyticsVidhya.com and University of California, Berkeley, fo
            r giving us permission to use his questions, and special thanks to
            Professor Scott Roths, Penn State University, for help on using time
            series ARIMA functions, and special thanks to Angela Ting for help
            with applying the front-end design. This app was updated by Shravani 
            Samala in 2021.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/1/2021 by SJS.")
            )
            
        ),
        
        #### Set up the Prerequisites Page ----
        tabItem(tabName = "prerequisites",
          h2("Background"),
          h3("Stationarity:"),
          p("Diagnostics for stationarity include looking for constant mean
         (or, trend) and variance over time"),
         tags$ul(
           tags$li("Constant mean is associated with data that does not have any sort of vertical (typically linear) trend over time."),
           tags$li("Seasonality could also be apparent in the mean structure. Recall that seasonal ARIMA cannot explain a seasonal trend, only seasonal correlations (ARIMA models work to explain correlation structure of a time series AFTER the mean and variance are constant)."),
           tags$li("Constant variance is associated with data whose vertical spread (in the valleys and peaks) is constant over the duration of the time series.")
         ), 
         
         h3("Autocorrelation Functions of Stationary Time Series:"),
         p("We typically trust the dashed lines in the autocorrelation function
         (ACF) plots to be the significance cut-off bounds for any lag's
         correlation"),
         p("In a model with non-zero autoregressive (AR) and moving average
         (MA) parts, there is no logical interpretation for both ACFS cutting
         off, thus,"),
         tags$ul(
           tags$li("For AR(p) models, the ACF will tail off and the PACF will
                cut off after lag p."),
           tags$li("For MA(q) models, the ACF will cut off after lag q, and
                the PACF will tail off."),
           tags$li("For ARMA(p, q) models, both the ACF and the PACF will both
                tail off.")
         ),
         p("The ARMA subsets plot is not the best tool for determining ARMA(p,q)
         orders, and thus will only be used as a tie breaker or guide after
         the ACF and PACF plots have been thoroughly inspected."),
         br(), 
         h3("Model Diagnostics:"),
         p("The ARIMA model aims to forecast future values of a stationary time
        series by estimating a mathematical function to explain the underlying
        correlation structure. For this reason, the ACF and PACF of the residuals
        of the ARIMA model that has been fitted should not contain any significant
        remaing correlation."),
        p("Though forecasting is the purpose for fitting an ARIMA model, looking at the forecast itself (against future values that have been reserved) isnt the best way to assess the goodness of the model's fit, this is why we look at the AIC and the ACF plots of the residuals of the model."),
        
        br(),
        br(), 
        
        div(style = "text-align: center",
          bsButton(
            inputId = "go1", 
            label = "GO!", 
            icon("wpexplorer"), 
            style = "default", 
            size = "large")
        ),
        br(), 
        ),
        
        #### Set up the Simulation Page ----
        tabItem(tabName = "sim",
          withMathJax(), 
          #tags$style(type= "text/css", ".content-wrapper,.right-side {background-color: white;}"),
          h2("Simulation"), 
          p("Use the sliders for the coefficients and explore how changing parameter
          values affects the time series plot."),
          p("Use the drop down menus and observe how different orders of models
          effect the autocorrelation function (ACF) and partial autocorrelation
          function (PACF) plots."),
          br(),
          fluidPage(
            fluidRow(
              column(width = 4,
                     selectInput("models","Models",
                                 list("Autoregressive" = "AR",
                                      "Moving Average" = "MA",
                                      "Autoregressive Moving Average" = "ARMA")
                     ),
                     sliderInput(
                       inputId = "n",
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
                       
                       selectInput(
                         inputId = "p",
                         label = "p order",
                         list( "1", "2")
                       ),
                       conditionalPanel(
                         condition = ("input.p == '1' || input.p == '2'"),
                         #h5(p(withMathJax(textOutput("Phi1")))),
                         sliderInput(
                           inputId = "phi1",
                           label = "\\(\\Phi_1\\)",
                           min = -0.9,
                           max = 0.9,
                           step = 0.1,
                           value = 0.5,
                           ticks = T
                         ),
                         conditionalPanel(
                           condition = "input.p == '2'",
                           
                           sliderInput("phi2",
                                       label = "\\(\\Phi_2\\)",
                                       min = -0.9,
                                       max = 0.9,
                                       step = 0.1,
                                       value = 0,
                                       ticks = T)
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
                       
                       selectInput(
                         inputId = "q",
                         label = "q order",
                         list("1","2")
                       ),
                       conditionalPanel(
                         condition = ("input.q == '1' || input.q == '2'"),
                         #h5(p(withMathJax(textOutput("Phi1")))),
                         
                         sliderInput(
                           inputId = "theta1",
                           label = "\\(\\Theta_1\\)",
                           min = -0.9,
                           max = 0.9,
                           step = 0.1,
                           value = 0.5,
                           ticks = T),
                         
                         conditionalPanel(
                           condition = ("input.q == '2'"),
                           
                           sliderInput(
                             inputId = "theta2",
                             label = "\\(\\Theta_2\\)",
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
                         column(width = 12,
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
              
              fluidRow(
                column(width = 12,
                       fluidRow(
                         conditionalPanel(
                           condition="input.models!='ARMA'",
                           fluidRow(
                             column(width = 6, plotOutput("plotACF")),
                             column(width = 6, plotOutput("plotPACF"))
                           )
                         )
                       )
                )
              )
            ),#fluidpage
            
            div(style = "text-align: center",
                bsButton(
                  inputId = "go2", 
                  label = "GO!", 
                  icon("cogs"), 
                  style = "default", 
                  size = "large")
            ),
            br(), 
        ),
        
        #### Set up the Data Page ----
        tabItem(tabName = "data",
                h2("Analyzing Real Data"), 
                p("In the first tab, select the data set that you would like to analyze,
            and fit transformations until the data seems stationary."),
            p("Here, you must consider that the last 12 observations of each data
            set are hidden from the user so that they could be presented in the
            last tab alongside the user's model's forecasts."), 
            p("For each transformation consider the following:", 
              tags$ul(
                tags$li("The transformations are of the form `seas_diff(diff(log(data)))`,
                      that is, the log transformation will always be taken first,
                      followed by the difference of lag one, and then the seasonal
                      differencing."), 
                tags$li("The trend can only be removed before any other transformation,
                      or after all transformations. The trend is removed using
                      regression, and the transformed data is the residuals from
                      that regression.")
              )
            ),
            br(), 
            tabsetPanel(id = "tabs2",
                        tabPanel(title = h4("Achieving Stationarity"), value = "step1",
                                 p(
                                   tags$ul(
                                     tags$li("To begin, please choose from of the data sets below any
                        one that you would like."),
                        tags$li("Use the checkbox options to make your data look satisfactorily
                      stationary."),
                      tags$li("The first part of our analysis is achieving stationarity,
                        so that you can correctly judge the correlation structure
                        to assign to the arima model.")
                                   )),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "sets",
                            tags$b("Choose a Dataset Below"),
                            list("Choose" = "Choose",
                                 "Internet Traffic" = "internet",
                                 "Monthly Traffic Fatalities" = "monthly",
                                 "varve" = "varve",
                                 "sheep" = "sheep",
                                 "Southern Oscillation Index" = "soi",
                                 "Daily Max Temp State College" = "temperature")
                          ),
                          br(),
                          conditionalPanel(
                            condition = "input.sets! = 'Choose'",
                            checkboxInput(
                              inputId = "trend", 
                              label = "Remove trend/non-constant mean in data", 
                              value = FALSE),
                            # bsPopover(id = "trend", title = "tit", content = "The trend can only be removed before or after all transformations.", trigger = "hover", placement = "top"),
                            conditionalPanel(
                              condition = "input.trend",
                              tags$div(style = "margin-left: 3vw;",
                                       checkboxInput(
                                         inputId = "trend1", 
                                         label = "Estimate and Remove linear Trend in Data With Regression", 
                                         value = FALSE),
                                       checkboxInput(
                                         inputId = "trend2", 
                                         label = "Estimate and Remove Seasonal Trend in Data With Regression",
                                         value = FALSE),
                                       checkboxInput(
                                         inputId = "trend3", 
                                         label = "Remove Seasonal Trend in Data With Cosine Regression", 
                                         value = FALSE)
                              ),
                              conditionalPanel(
                                condition = "input.sets != 'temperature' && input.sets != 'monthly' && input.sets != 'soi' && (input.trend2 || input.trend3)",
                                numericInput(
                                  inputId = "frequency", 
                                  label = "Choose the frequency of the time series",
                                  value = 1,
                                  min = 1,
                                  max = 365)
                              )
                            ),
                            checkboxInput(
                              inputId = "log",
                              label = "Take log transformation of data",
                              value = FALSE),
                            checkboxInput(
                              inputId = "diff",
                              label = "Take first difference of data",
                              value = FALSE),
                            conditionalPanel(
                              condition = "input.diff",
                              tags$div(
                                style = "margin-left: 3vw;",
                                checkboxInput(
                                  inputId = "diff2",
                                  label = "Take second difference of data",
                                  value = FALSE)
                              )
                            ),
                            checkboxInput(
                              inputId = "seas_diff",
                              label = "Take first seasonal difference of data",
                              value = FALSE),
                            conditionalPanel(
                              condition = "input.seas_diff",
                              tags$div(style = "margin-left: 3vw;",
                                       numericInput(
                                         inputId = "seas",
                                         label = "Choose the seasonal period",
                                         value = 1,
                                         min = 1,
                                         max = 30),
                                       checkboxInput(
                                         inputId = "seas_diff2",
                                         label = "Take second seasonal difference of data",
                                         value = FALSE)
                              )
                            ),
                            div(
                              style = "text-align: center",
                              div(id = "div",
                                  actionButton(
                                    inputId = "go4", 
                                    label = "Next step!", 
                                    style = "primary", 
                                    disabled = TRUE)
                              )
                            ), 
                            
                            br(), 
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
                      tabPanel(title = h4("Determine ARMA order"), value = "step2",
                               p("Now, that you have made your data stationary, you can inspect the
              resulting acf plots as well as the ARMAsubsets plot below to determine
              the arima order and fit a model."),
              fluidRow(
                plotOutput("ACF"),
                bsPopover(
                  id="ACF",
                  title = "ACF of transformed data", 
                  content = "The blue dashed line represents significance bounds
                for correlation at different lags in the data.", 
                trigger = "hover", 
                placement = "bottom"),
                plotOutput("PACF"),
                bsPopover(
                  id="PACF", 
                  title = "PACF of tranformed data", 
                  content = "The blue dashed line represents significance bounds
                for correlation at different lags in the data.",
                trigger = "hover",
                placement = "top")
              ),
              div(style = "text-align: center",
                  h4('After you are finished making your choices, press the "Next step!"
                   button below to see how good of a fit your model was.')
              ),
              fluidRow(
                column(4,
                       numericInput(
                         inputId = "p.order", 
                         label = "AR part order", 
                         value = 0, 
                         max = 10,
                         min = 0),
                       numericInput(
                         inputId = "q.order",
                         label = "MA part order",
                         value = 0,
                         max = 10,
                         min = 0),
                       numericInput(
                         inputId = "P.order",
                         label = "Seasonal AR part order",
                         value = 0,
                         max = 10,
                         min = 0),
                       numericInput(
                         inputId = "Q.order",
                         label = "Seasonal MA part order",
                         value = 0, 
                         max = 10,
                         min = 0),
                       numericInput(
                         inputId = "period",
                         label = "Seasonal Period",
                         value = 0,
                         max = 12,
                         min = 0)
                ),
                column(width = 8,
                       plotOutput("subsets"),
                       bsPopover(
                         id = "subsets", 
                         title = "ARMA subsets", 
                         content = "This plot shows the best combinations of ARMA orders
                  using AIC. The greyed squares indicate that the parameter is
                  used in the model.",
                  trigger = "hover",
                  placement = "top")
                ),
                div(style = "text-align: center",
                    bsButton(
                      inputId = "go5",
                      label = "Next step!",
                      style = "primary")
                ), 
                br()
              )
                      ),
              tabPanel(title = h4("Forecast"), value = "step3",
                       fluidRow(
                         column(width = 11,
                                p("Below you can see how well you fit the data to a time
                  series by seeing the resulting forecasts (plotted against
                  the last 12 observations of the data set, which were hidden
                  from the initial time series plot), this plot will show
                  you-in blue-only the final 100 observations in the data set.
                  You can also observe the correlation structure of the
                  residuals of the arima fit to see if you were able to fully
                  explain the correlation with the model. The progress bar
                  will show you how close you are to the best possible fit
                  for the data set.")
                         )
                       ),
                  fluidRow(
                    div(style = "position:relative; z-index: 950;",
                        plotlyOutput("forecast")
                    ),
                    br(),
                    # fluidRow(
                    #   div(style = "position:relative; z-index: 900;",
                    #     div(style = "margin-top: -20vh;",
                    #       div(style = "text-align: center",
                    #         imageOutput("bar") #, height = "1000px")
                    #       )
                    #     )
                    #   )
                    # ),
                    
                    wellPanel(fluidRow(
                      #div(style = "position:relative; z-index: 950;",
                      #div(style = "margin-top: -20vh;",
                      column(width = 6, plotOutput("fitQuality1")),
                      bsPopover(
                        id="fitQuality1",
                        title = "ACF residuals",
                        content = "The ACF and PACF of the residuals of the fitted model
                  can indicate if theres any remaining correlation structure in
                  the data.",
                  trigger = "hover",
                  placement = "top"),
                  column(width = 6, plotOutput("fitQuality2")),
                  bsPopover(
                    id = "fitQuality2",
                    title = "PACF residuals",
                    content = "The ACF and PACF of the residuals of the fitted model
                  can indicate if theres any remaining correlation structure in the
                  data.",
                  trigger = "hover",
                  placement = "top")
                  #)
                  #)
                    )),
                  fluidRow(
                    div(style = "position:relative; z-index: auto;",
                        div(style = "margin-top: -20vh;",
                            column(width = 5, plotOutput("bar")),
                            column(width = 6, 
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
                    div(
                      style = "position:relative; z-index: auto;",
                      bsPopover(
                        id="bar",
                        title = "Model fit evaluation",
                        content = "This indicates how well your model was fit. 100
                      would indicate that your model is as good as can be. 0 would
                      indicate that your model was no better than using just the
                      mean as the predictor.",
                      trigger = "hover",
                      placement = "right")
                    )
                  ),
                  
                  br(),
                  
                  )
              )
            )
        ), 
        
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          
          p(
            class = "hangingindent",
            "Attali, D. (2016). shinyjs: Easily Improve the User Experience of
            Your Shiny Apps in Seconds. R package version 2.0.0.
            Available from https://cran.r-project.org/package=shinyjs"
          ),
          
          p(
            class = "hangingindent",
            "Bache, S. and Wickham, H. (2014). magrittr: A Forward-Pipe Operator
            for R. R package version 1.5. Available from 
            https://cran.r-project.org/package=magrittr"
          ),
          
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities. R 
            package version 0.1.6.3. Available from 
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          
          p(
            class = "hangingindent",
            "Chan, K. and Ripley, B. (2018). TSA: Time Series Analysis. R package
            version 1.3. Available from https://cran.r-project.org/package=TSA"
          ), 
          
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y. and McPherson, J. (2017).
            shiny: Web Application Framework for R. R package version 1.6.0.
            Available from https://cran.r-project.org/package=shiny"
          ),
          
          p(
            class = "hangingindent",
            "Chang, W. and Ribeiro, B. (2018). shinydashboard: Create Dashboards
            with ‘Shiny’. R package version 0.7.1 Available from
            https://cran.r-project.org/package=shinydashboard"
          ),
          
          p(
            class = "hangingindent",
            "Hijmans, R. (2017). raster: Geographic Data Analysis and Modeling.
            R package version 2.6-7. Available from
            https://cran.r-project.org/package=raster"
          ),
          
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F. and Granjon, D. (2018). shinyWidgets: Custom
            Inputs Widgets for Shiny. R package version 0.6.0. Available from
            https://cran.r-project.org/package=shinyWidgets"
          ),
          
          p(
            class = "hangingindent",
            "R Core Team (2018). stats: A language and environment for statistical
            computing. R Foundation for Statistical Computing, Vienna, Austria.
            R package version 0.2. Available from
            https://github.com/arunsrinivasan/cran.stats"
          ),
          
          p(
            class = "hangingindent",
            "Sievert, C., Parmer, C., Hocking, T., Chamberlain, S., Ram, K., 
            Corvellec, M. and Despouy, P. (2017). plotly: Create Interactive Web
            Graphics via ‘plotly.js’. R package version 4.9.3. Available from
            https://cran.r-project.org/package=plotly"
          ),
          
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis.
            Springer-Verlag New York. R package version tidyerse.
            Available from https://ggplot2.tidyverse.org/"
          ),
          
          p(
            class = "hangingindent",
            "Wickham, H. and Henry, L. (2018). tidyr: Tidy Messy Data. R package
            version 1.1.3. Available from https://cran.r-project.org/package=tidyr"
          ), 
          br(), 
          boastUtils::copyrightInfo()
          
        ) 
      )
    )
  )
)

### Set up Server ----

server <- function(session, input, output) {
  
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "In this app, the user will explore common time series models with
        simulations and real data, and be challenged to review the material in a
        tic-tac-toe game."
      )
    }
  )
  
  ######exploration and simulation
  observeEvent(input$go0,{
    updateTabItems(
      session = session, 
      inputId = "pages", 
      selected = "prerequisites")
  })
  observeEvent(input$go1,{
    updateTabItems(
      session = session, 
      inputId = "pages", 
      selected = "sim")
  })
  observeEvent(input$go2,{
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "data")
  })
  
  observeEvent(input$go4,{
    # if(!(input$trend & !input$trend1 & !input$trend2 & !input$trend3)){
    updateTabsetPanel(
      session = session, 
      inputId = "tabs2",
      selected = "step2"
    )
    # }
  })
  observeEvent(input$go5,{
    updateTabsetPanel(
      session = session,
      inputId = "tabs2",
      selected = "step3"
    )
  })
  
  
  ####### SIMULATED #######
  
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
  # when the model doesnt change after we leave the 3rd tab (step3) and also still 
  # hasnt changed when the user comes back to the 3rd tab, we dont want to recalculate
  #the same fit, so we create "counter" reactive values to account for this in the fit() 
  #reactive handler and the model() observation handler
  
  # {idk if this still applies i changed the counter code since}:  must keep in 
  #mind that (when both fit and model are created in a reactive handler) though 
  #we want model list (counter$a) to govern what happens, its hard when you consider 
  #the fact that it is called inside the reactive call to fit (since reactive handlers
  #are "lazy")
  
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
  
  # output$feedback <- renderText({
  #   c("\r\n \r\n \r\n \r\n \r\n \r\n \r\n \r\n \r\n \r\n \r\n \r\n", feed())
  # })
  
  # output$feedback <- renderUI({
  #   HTML("<br> <br> <br> <br> <br> <br>")
  # })
  
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
  
}
boastUtils::boastApp(ui = ui, server = server)

