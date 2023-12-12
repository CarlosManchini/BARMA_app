
library(shiny)
library(shinythemes)
library(shinycssloaders)

# Define UI for dataset viewer app ----
ui <- fluidPage(
    theme = shinytheme("simplex"), #"simplex" #shinythemes::themeSelector(),
    titlePanel(("Dynamical model for double bounded time series")),
    navbarPage(
        img(src='ufsms2.png',align='left',style='width:103px;margin-top:-10px'),#"UFSM",
        tabPanel(strong("Statistical analysis"), icon = icon("spinner"),
                 sidebarPanel(
                     
                     helpText(h5("To use a data example, check:")),
                     checkboxInput("exemplo", "Hydroelectric energy (submitted. Manchini, C. et al (2022). J Stat Comput Simul.)", FALSE),
                     checkboxInput("exemplo2", "Relative humidity data (application in Bayer, F. et al (2018). J Stat Comput Simul.)", FALSE),#, Doi: 10.1080/00949655.2018.1491974)", FALSE),
                     uiOutput('ex5'),
                     
                     # Input: order  ----
                     HTML("<hr>"), #br(),
                     # tags$hr(style="margin-top: -2px; margin-bottom: 11px;"), #\u03C6  VarPhi
                     div(numericInput("ar", strong("Autoregressive order (p):"), value=1, min=1),style="font-size:115%; font-weight:bold;"),
                     checkboxGroupInput("arS",label=NULL),
                     
                     div(numericInput("ma", strong("Moving-average order (q):"), value=1, min=1),style="font-size:115%; font-weight:bold;"),
                     checkboxGroupInput("maS",label=NULL),                #\u03B8 Theta
                     
                     # Input: Select  ----
                     tags$hr(style="margin-top: 1px; margin-bottom: 11px;"),
                     fluidRow(
                     column(width = 6, align="center",offset=0,#style="padding-bottom:-75px;",
                            div(numericInput("start", strong("Year"), 2001, min = 1),style="font-size:110%;")
                     ),# helpText(h6("Note: the year of the first observation.")),
                     column(width = 6, align="center",offset=0,#style="padding-bottom:-75px;",
                            div(numericInput("end", strong("Month"), 1, min = 1)),style="font-size:110%;")
                     ),
                     div(helpText(h6("Note: year/month of first observation.")),style="margin-top: -20px; margin-bottom:15px;"),
                     div(numericInput("freq", strong("Frequency"), 12, min = 1),style="font-size:110%;"),  #52
                     div(style="margin-top: -9px"),
                     helpText(h6("Note: the number of observations per unit of time. E.g. 12 for monthly data."))
                 ),
                 
                 sidebarPanel(
                     # Input: Select a dataset ----
                     div(selectInput("model", div(strong("Model:",style="font-size:16px;")),
                                 choices = c("BARMA")),style="font-size:126%; font-weight:bold;"),
                     
                     # Input: Select a funklink ----
                     div(selectInput("funklink", div(strong("Link Function:"),style="font-size:14px;"),
                                 # choices = c("Logit", "Probit", "Cloglog"))
                                 choices = c("Logit", "Probit", "Cloglog","Aranda-Ordaz")),style="font-size:119%; font-weight:bold; "),
                     
                     # conditionalPanel(condition = "input.funklink=='Aranda-Ordaz'",
                     #                  checkboxInput("optimal","Optimal Aranda-Ordaz"))
                                      # fluidRow(column(width=6,checkboxInput("optimal","Optimal Aranda-Ordaz")),
                                      #          column(width=6,offset = 0,style="padding:0px;",helpText(h6("Aviso "))) 
                                      #     )
                     
                     # ,conditionalPanel(condition = "input.optimal"),withSpinner(uiOutput("spinnerDummyID1"),type = 6)

                 ),
                 
                 sidebarPanel(
                     # Input: Select type of residuals ----
                     div(radioButtons("resid", strong("Residuals"),
                                  choices = c(Standardized = "resid1",
                                              Deviance = "resid2",
                                              Quantile = "resid3"),
                                  selected = "resid1", inline = TRUE ),style="font-size:110%;"),
                     div(sliderInput("sh1", strong("Lags for forecast"),
                                 min = 0, max = 18, value = 6),style="font-size:110%;")
                 ),
                 
                 mainPanel(
                     tabsetPanel(
                         tabPanel(strong("Model"), icon = icon("chevron-circle-right"),
                                  # withSpinner(plotOutput("plot6"),type = 6),
                                  # uiOutput("spinner"),
                                  tags$head(tags$style(".shiny-notification {font-size: 20px;}")),# height:130px; width: 200px; position:fixed; top: calc(50%) ;;left: calc(50%);;")),
                                  h3("Forecast"),
                                  div(style="margin-top: -9px"),
                                  plotOutput("plot6"),
                                  verbatimTextOutput("insample"),
                                  verbatimTextOutput("outofsample"),
                                  h3("Fitted model"),
                                  div(style="width:401px;padding-left:1px;" ,verbatimTextOutput("karma.print")),
                                  
                         ),
                         
                         tabPanel(strong("Summary"), icon = icon("file-text"),
                                  
                                  h3("Descriptive analysis"),
                                  # verbatimTextOutput("mean"),
                                  div(style="width:409px;padding-left:1px;" ,verbatimTextOutput("summary")),
                                  h3("Contents"),
                                  fluidRow(
                                    column(width = 2, radioButtons("disp", "Visualization",
                                               choices = c(Header = "head",
                                                           All = "all"),
                                               selected = "head", inline = FALSE)),
                                    column(width = 7, tableOutput("contents"))
                                  ),
                                  # dataTableOutput("contents")
                                  
                                  div(style="margin-top: -3px"),
                                  h3("Observed data"),div(style="margin-top: -9px"),
                                  plotOutput("plot0"),
                                  h3("Monthly time series"),
                                  plotOutput("plot1"),
                                  h3("Autocorrelation function (ACF)"),
                                  plotOutput("plot2"),
                                  
                                  h3("Partial autocorrelation function (PACF)"),
                                  plotOutput("plot3")
                                  
                                  
                         ),
                         
                         tabPanel(strong("Residuals analisys"), icon = icon("line-chart"),
                                  h3("Residual autocorrelation function (ACF)"),
                                  plotOutput("plot4"),
                                  h3("Residual partial autocorrelation function (PACF)"),
                                  plotOutput("plot5"),
                                  h3("Residuals x index"),
                                  plotOutput("plot7"),
                                  h3("Q-Q plot residuals"),
                                  plotOutput("plot8"),
                                  h3("Normal plot with simulated envelopes"),
                                  fluidRow(
                                      column(width = 1, align="left",offset=0,#style="padding-bottom:-75px;",
                                             # div(sliderInput("confenv","Confidence level",value=.95,min=.1,max=.99),style="font-size:90%;")
                                             actionButton("add","Run",style='padding:11px; font-size:115%')
                                      ),
                                      column(width = 3, align="left",offset=0,#style="padding-bottom:-75px;",
                                             div(numericInput("nsimu","Number of simulations",value = 1000, min=1,max=100000,step=1000,width = '90%')),style="font-size:96%;font-weight:bold;")
                                  )
                                  # actionButton("add","Run"),numericInput("nsimu","Number of simulation",value = 1000, min=3,max=10999),
                                  ,plotOutput("p1")
                         )
                         
                     )
                 )
        ),
        tabPanel(strong("References"), icon = icon("book"),
                 includeMarkdown("references.rmd")),
        tabPanel(strong("Authorship"), icon = icon("address-card"), includeMarkdown("authorship.rmd"))
    )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
    
    output$ex5 <- renderUI({
        if(input$exemplo){ 
            withMathJax(
                        # Horizontal line ----
            # Input: Checkbox if file has header ----
            # tags$div(style="margin-top: 15px"),
            # checkboxInput("header", "Header in data", FALSE), 
            # div(style="margin-bottom: -10px;margin-top: -10px;height:-50px;"), 
            div(checkboxInput("covar", " ", FALSE),style="opacity:0; position:absolute; left:9999px;")
        )} else if(input$exemplo2){ 
            withMathJax(
            div(checkboxInput("covar", " ", TRUE),style="opacity:0; position:absolute; left:9999px;")
        )} else {
        withMathJax(
            # Input: Select a file ----
            div(
            fileInput("file1", strong("Upload data"),
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
            , style="font-size:115%; font-weight:bold;"),
            div(style="margin-top: -21px"),
            helpText(h6("Note: the response must lie whitin (0,1) interval.")),
            
            
            # Horizontal line ----
            # Input: Checkbox if file has header ----
            tags$div(style="margin-top: 15px"),
            checkboxInput("header", "Header on the first row of data", TRUE), 
            div(style="margin-top: -9px"), 
            checkboxInput("covar", "Covariates in additional columns (predicted values are based on Holt-Winters method)", FALSE)
            # 
            # tags$hr()
            # Horizontal line ----
        )
        }
    })
    
    
    # file input
    file <- reactive({
        
        val1 <- 0 ; val2 <- 0
        if( TRUE == input$exemplo ){ val1 = 1 } ; if( TRUE == input$exemplo2 ) { val2 = 1 }

        
        if( val1 == 0 && val2==0) {
            req(input$file1)
            # when reading semicolon separated files,
            # having a comma separator causes `read.csv` to error
            tryCatch(
                {
                    df <- read.csv(input$file1$datapath,
                                   header = input$header,
                                   sep = input$sep,
                                   quote = input$quote)
                },
                error = function(e) {
                    # return a safeError if a parsing error occurs
                    stop(safeError(e))
                }
            )
            val <- 0
            if( TRUE == input$covar ) { val = 1 }
            if( val == 0 ) return( df )
            if( val == 1 ) return( df )
        }
        
        
        if( val1 == 1 && val2==0) {
            df<-read.table("hydro22minus6.txt")#minus8
                # c(0.9862, 0.974, 0.9166, 0.8279, 0.8305, 0.928, 0.9676, 0.8483, 0.7695, 
                #   0.9644, 0.8705, 0.8273, 0.9252, 0.8787, 0.7598, 0.6012, 0.7463, 0.6748, 
                #   0.5385, 0.5979, 0.7944, 0.9516, 0.962, 0.9727, 0.8535, 0.8958, 0.8794, 0.7275, 
                #   0.5881, 0.611, 0.6017, 0.4713, 0.3361, 0.3351, 0.4123, 0.8102, 0.818, 0.7297, 
                #   0.5711, 0.4845, 0.64, 0.7507, 0.8657, 0.6821, 0.6489, 0.8649, 0.8588, 0.7417, 
                #   0.6817, 0.5073, 0.3461, 0.425, 0.6401, 0.9297, 0.9065, 0.8389, 0.9519, 0.9088, 
                #   0.8756, 0.7882, 0.7185, 0.6304, 0.4892, 0.4121, 0.3109, 0.2977, 0.3152, 0.3857, 
                #   0.4288, 0.407, 0.482, 0.5534, 0.6336, 0.6982, 0.8039, 0.8262, 0.9086, 0.767, 
                #   0.7979, 0.6193, 0.6167, 0.5977, 0.7552, 0.7273, 0.6333, 0.4776, 0.4278, 0.487, 
                #   0.6465, 0.6865, 0.5602, 0.62, 0.5389, 0.9139, 0.9349, 0.7461, 0.6079, 0.5407, 
                #   0.4734, 0.3855, 0.3806, 0.4326, 0.6749, 0.8473, 0.9441, 0.9565, 0.9789, 0.9715, 
                #   0.963, 0.9714, 0.9238, 0.8935, 0.9534, 0.9017, 0.8893, 0.7955, 0.6426, 0.5177, 
                #   0.4039, 0.7219, 0.8324, 0.916, 0.9219, 0.8897, 0.7257, 0.6779, 0.9538, 0.9558, 
                #   0.9366, 0.9187, 0.7507, 0.5659, 0.6328, 0.5153, 0.3443, 0.37, 0.4974, 0.6812, 
                #   0.7391, 0.6339, 0.4478, 0.4147, 0.3763, 0.365, 0.4377, 0.4179, 0.6245, 0.6027,
                #   0.5418, 0.8083, 0.8875, 0.9149, 0.9566, 0.9375, 0.7297, 0.5774, 0.5756, 0.373, 
                #   0.4597, 0.439, 0.5493, 0.9475, 0.9047, 0.7349, 0.7548, 0.8449, 0.6569, 0.574, 
                #   0.5958, 0.5111, 0.393, 0.3416, 0.3807, 0.6367, 0.9676, 0.7689, 0.7739, 0.9686, 
                #   0.967, 0.9836, 0.9301, 0.9512, 0.976, 0.8866, 0.928, 0.8808, 0.8809, 0.9021, 
                #   0.7991, 0.8612, 0.7098, 0.6026, 0.6045, 0.5164, 0.435, 0.4245)
                
            return( df )
        }
        if( val2 == 1 && val1==0) {
            # require("dados0.txt")
            # when reading semicolon separated files,
            # having a comma separator causes `read.csv` to error
            tryCatch(
                {
                    df <- read.table("dados0.txt",
                                     header = input$header,
                                     sep = input$sep,
                                     quote = input$quote)
                },
                error = function(e) {
                    # return a safeError if a parsing error occurs
                    stop(safeError(e))
                }
            )
            val <- 0
            if( TRUE == input$covar ) { val = 1 }
            if( val == 0 ) return( df )
            if( val == 1 ) return( df )
        }
        
        
        
    })
    
    observeEvent(input$exemplo2,{
        if(input$exemplo2 && input$exemplo){
            updateCheckboxInput(session, "exemplo",value=FALSE)
        }
    })
    observeEvent(input$exemplo,{
        if(input$exemplo2 && input$exemplo){
            updateCheckboxInput(session, "exemplo2",value=FALSE)
        }
    })
    
    # # #trying de-selecionar Header e Covar quando input$exemplo==TRUE
    # observeEvent(input$exemplo2,{
    #         updateCheckboxInput(session, "header",value=TRUE)
    # })
    # observeEvent(input$exemplo2,{
    #         updateCheckboxInput(session, "covar",value=FALSE)
    # })
    
   
    ### AVISO
    # observe({
    #   session$sendCustomMessage(type = 'testmessage',
    #                             # message = list(a = 1, b = 2,
    #                             #                model = input$model))
    #                             message = print(paste(paste("Selected model", input$model, sep = " "), paste(", link function", input$funklink, sep = " "), sep = " "), quote = T))
    # })
    
    ### odcd
    observe({
        updateCheckboxGroupInput(session, inputId = 'exemplo', label = NULL, choices = NULL,
                                 selected = NULL, inline = FALSE, choiceNames = NULL,
                                 choiceValues = NULL)
        fileInput("file1", "Upload data",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"))
    })
    observe({
        updateCheckboxGroupInput(session, inputId = 'exemplo2', label = NULL, choices = NULL,
                                 selected = NULL, inline = FALSE, choiceNames = NULL,
                                 choiceValues = NULL)
        fileInput("file1", "Upload data",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"))
    })
    
    # component autoregressive input
    arInputX <- reactive({
        as.vector(input$ar)
    })
    
    # component moving average input
    maInputX <- reactive({
        as.vector(input$ma)
    })
    
    
    
    # observe ar
    observe({  # checkboxGroupInput("arS",label=NULL),
        ret <- as.numeric(arInputX())
        updateCheckboxGroupInput(
            session, 'arS', choices = 1:ret,
            selected = 1:ret, inline = T)
    })
    
    # observe ma
    observe({  # checkboxGroupInput("arS",label=NULL),
        ret <- as.numeric(maInputX())
        updateCheckboxGroupInput(
            session, 'maS', choices = 1:ret,
            selected = 1:ret, inline = T
        )
        # return(input$arSS)
        
        # output$aham<- renderPrint({
        #   return(input$arS)
        # })
        
        # arInput2<- reactive({
        #   as.vector(input$arS)
        # })
        # return(input$arS)
    })#, autoDestroy = F,quote=T)
    
    # observeEvent(input$exemplo,{
    #     updateNumericInput(session,'ar',value=2) })

    output$mean <- renderPrint({
        file<- input$arS
        return(as.vector(file))
    })
    
    # contents file input
    output$contents <- renderTable({
        file<- file()
        if(input$disp == "head") {
            return(head(file))
        }
        else {
            return(file)
        }
        
    })
    
    # Generate a summary of the dataset
    output$summary <- renderPrint({
        file <- file()
        summary(file)
        # print(c(class(file), dim(file), length(file[,1])), quote = F)
    })
    
    ### plot y
    output$plot0 <- renderPlot({
        
        file <- file()
        
        val <- 0
        if( TRUE == input$covar ) { val = 1 }
        if( val == 1 ){
            Y <- ts(file[,1],start=c(input$start,input$end),frequency=input$freq) # time series
            
            # sample size
            n<-length(Y)
            y<-ts(Y[1:n], start=c(input$start,input$end),frequency=input$freq)
            par(mar=c(4, 3, 2, 2))
            par(oma=c(.1,.1,.1,.1))
            par(mgp=c(1.6, 0.6, 0))
            return(plot(y))
        } else{
            Y<-ts(file[,1],start=c(input$start,input$end),frequency=input$freq) # time series
            
            # sample size
            n<-length(Y)
            
            y<-ts(Y[1:n], start=c(input$start,input$end),frequency=input$freq)
            par(mar=c(4, 3, 2, 2))
            par(oma=c(.1,.1,.1,.1))
            par(mgp=c(1.6, 0.6, 0))
            return(plot(y))
        }
        
    })
    
    ### plot 1 monthplot
    output$plot1 <- renderPlot({
        file <- file()
        
        val <- 0
        if( TRUE == input$covar ) { val = 1 }
        
        if( val == 1 ){
            Y <- ts(file[,1],start=c(input$start,input$end),frequency=input$freq) # time series
            
            # sample size
            n<-length(Y)
            y<-ts(Y[1:n], start=c(input$start,input$end),frequency=input$freq)
            
            return(monthplot(y))
        } else{
            Y<-ts(file[,1],start=c(input$start,input$end),frequency=input$freq) # time series
            
            # sample size
            n<-length(Y)
            y<-ts(Y[1:n], start=c(input$start,input$end),frequency=input$freq)
            
            return(monthplot(y))
        }
        
    })
    
    ### plot 2 acf
    output$plot2 <- renderPlot({
        file <- file()
        
        val <- 0
        if( TRUE == input$covar ) { val = 1 }
        if( val == 1 ){
            Y <- ts(file[,1],start=c(input$start,input$end),frequency=input$freq) # time series
            
            # sample size
            n<-length(Y)
            y<-ts(Y[1:n], start=c(input$start,input$end),frequency=input$freq)
            return(acf(y))
        } else{
            
            Y<-ts(file[,1],start=c(input$start,input$end),frequency=input$freq) # time series
            
            # sample size
            n<-length(Y)
            y<-ts(Y[1:n], start=c(input$start,input$end),frequency=input$freq)
            return(acf(y))
        }
        
    })
    
    ### plot 3 pacf
    output$plot3 <- renderPlot({
        file <- file()
        
        val <- 0
        if( TRUE == input$covar ) { val = 1 }
        if( val == 1 ){
            Y <- ts(file[,1],start=c(input$start,input$end),frequency=input$freq) # time series
            
            # sample size
            n<-length(Y)
            y<-ts(Y[1:n], start=c(input$start,input$end),frequency=input$freq)
            
            return(pacf(y))
        } else{
            Y<-ts(file[,1],start=c(input$start,input$end),frequency=input$freq) # time series
            
            # sample size
            n<-length(Y)
            y<-ts(Y[1:n], start=c(input$start,input$end),frequency=input$freq)
            
            return(pacf(y))
        }
        
    })
    
    # link function input
    funlinkInput <- reactive({
        switch(input$funklink,
               "Logit" = as.character("logit"),
               "Probit" = as.character("probit"),
               "Cloglog" = as.character("cloglog"),
               "Aranda-Ordaz" = as.character("aoz"))
    })
    
    modelInput <- reactive({
        switch (input$model,
                "BARMA" = "barma",
                "KARMA" = "karma")
    })
    
    # component autoregressive input
    arInput <- reactive({
        as.vector(input$arS)
    })
    
    # component moving average input
    maInput <- reactive({
        as.vector(input$maS)
    })
    
    # output fit model
    
    output$karma.print <- renderPrint({
        z <- model.fit()
        
        print(z$model,quote=F)
        # print(z$model_presentation)
        cat("\n")
        cat(c("Log-likelihood:",round(z$loglik,4)),"\n")
        cat(c("Number of iterations in BFGS optim:",z$counts),"\n\n")
        cat(c("AIC:",round(z$aic,4)," SIC:",round(z$bic,4)," HQ:",round(z$hq,4)),"\n")
        cat("Residuals:","\n")
        print(summary(residc()))
        
    })
    
    # fit model BARMA/KARMA
    model.fit <- reactive({
        # output$karma <- renderPlot({
        file <- file()
        
        val <- 0
        if( TRUE == input$covar ) { val = 1 }
        if( val == 1 ){ #COM covar
            Y<-ts(file[,1],start=c(input$start,input$end),frequency=input$freq) # time series
            
            # sample size
            n<- length(Y)
            
            # number of forecast steps
            h1 <- input$sh1
            
            y<-ts(Y, start=c(input$start,input$end),frequency=input$freq)
            
            # deterministic seazonality
            
            
            if( dim(file)[2] > 2 ){ #dado com mais de 2 colunas
                mX <- file[,-1] #as.matrix(MX[1:n,1:nc.x])
                nc.x <- ncol(mX)
                
                mX_hat <- matrix(rep(NA, nc.x*h1), ncol = nc.x)
                
                for (i in 1:nc.x) {
                    cov<-ts(mX[,i], start=c(input$start,input$end),frequency=input$freq)
                    m <- HoltWinters(cov)
                    p <- predict(m, h1, prediction.interval = F); p <- as.vector(p)
                    mX_hat[,i] <- cbind(p)
                }
                
                
                modelInput <- modelInput()
                
                # funlinkInput <- as.character(funlinkInput())
                funlinkInput <- funlinkInput()
                # linktemp <- eval(link)
                
                arInput <- as.numeric(arInput())
                maInput <- as.numeric(maInput())
                
                # if(modelInput == "karma"){
                #     source("karma.r")
                #     if(funlinkInput() == "logit") fit1 <- karma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "logit")
                #     if(funlinkInput() == "probit") fit1 <- karma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "probit")
                #     if(funlinkInput() == "cloglog") fit1 <- karma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "cloglog")
                #     if(funlinkInput() == "aoz") fit1 <- karma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "aoz", lambda=3)
                # }
                
                if(modelInput == "barma"){
                    source("barma.r")
                    if(funlinkInput() == "logit") fit1 <- barma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "logit")
                    if(funlinkInput() == "probit") fit1 <- barma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "probit")
                    if(funlinkInput() == "cloglog") fit1 <- barma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "cloglog")
                    if(funlinkInput() == "aoz"){ #if(input$optimal==FALSE) fit1 <- barma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "aoz", lambda=1.5) 
                        
                        withProgress(message="Loading...", value=0,{
                            
                        
                        # if(input$optimal==TRUE){
                            fit<-barma(y, ar=1, diag=0,link = "aoz", X=NA, X_hat=NA,lambda=0.5)
                            loglikmin<-fit$loglik
                            myseq<-c(0.666,2, 4) ;i=0
                            for(lzera in myseq){i=i+1
                                xii<-barma(y, ar=arInput, ma=maInput,X=mX, X_hat=mX_hat, h=h1, link="aoz",lambda=lzera,diag=0) 
                                # print(paste("Lambda =",lzera));print(paste("Log-Likelihood =",round(xii$loglik,4)));cat("\n------------------\n")
                                
                                if(loglikmin<xii$loglik){   
                                    loglikmin<-xii$loglik
                                    best_model_loglik <- xii$model
                                    fit1 <- xii
                                    # print("###### melhor ######")
                                    # print(loglikmin)
                                }
                                incProgress(1/length(myseq), detail = paste(round(100*i/length(myseq),1),"%"))
                                Sys.sleep(0.1)
                            }
                        })#}
                    }
                }
                return(fit1)
            } else { #else if(input$exemplo1==T) sera? #dado com 1coluna 
                
                mX <- as.matrix(file[,-1])

                mX_hat <- matrix(rep(NA, h1), nrow = h1)
                cov<-ts(mX[,1], start=c(input$start,input$end),frequency=input$freq)
                m <- HoltWinters(cov)
                p <- predict(m, h1, prediction.interval = F); p <- as.vector(p)
                mX_hat <- cbind(p)
                
                # Replicando PILI qdo lambda=7
                # n2 <- n-h1
                # 
                # t<-1:n
                # S <- sin(2*pi*t/52) ; mX <- as.matrix(S)
                # t_hat<- (n2+1):(n2+h1)
                # S_hat <- sin(2*pi*t_hat/52)
                # p<-as.vector(S_hat) ; mX_hat <- cbind(p)
                
                modelInput <- modelInput()
                
                # funlinkInput <- as.character(funlinkInput())
                funlinkInput <- funlinkInput()
                # linktemp <- eval(link)
                
                arInput <- as.numeric(arInput())
                maInput <- as.numeric(maInput())
                
                # if(modelInput == "karma"){
                #     source("karma.r")
                #     if(funlinkInput() == "logit") fit1 <- karma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "logit")
                #     if(funlinkInput() == "probit") fit1 <- karma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "probit")
                #     if(funlinkInput() == "cloglog") fit1 <- karma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "cloglog")
                #     if(funlinkInput() == "aoz") fit1 <- karma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "aoz", lambda=3)
                # }
                
                if(modelInput == "barma"){
                    source("barma.r")
                    if(funlinkInput() == "logit") fit1 <- barma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "logit")
                    if(funlinkInput() == "probit") fit1 <- barma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "probit")
                    if(funlinkInput() == "cloglog") fit1 <- barma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "cloglog")
                    if(funlinkInput() == "aoz"){ #if(input$optimal==FALSE) 
                        # print(mX_hat)
                        # fit1 <- barma(y, ar=arInput, ma=maInput, h=h1, X = mX, X_hat = mX_hat, link = "aoz", lambda=1.5) 
                        
                        withProgress(message="Loading...", value=0,{

                    # if(input$optimal==TRUE){
                        fit<-barma(y, ar=1, diag=0,link = "aoz", X=NA, X_hat=NA,lambda=0.5)
                        loglikmin<-fit$loglik
                        myseq<-c(0.666,2, 4) ;i=0 #c(1.3,3.3, 5)
                        for(lzera in myseq){i=i+1
                            xii<-barma(y, ar=arInput, ma=maInput,X=mX, X_hat=mX_hat, h=h1, link="aoz",lambda=lzera,diag=0)
                            # print(paste("Lambda =",lzera));print(paste("Log-Likelihood =",round(xii$loglik,4)));cat("\n-------------------\n")

                            if(loglikmin<xii$loglik){
                                loglikmin<-xii$loglik
                                best_model_loglik <- xii$model
                                fit1 <- xii #; print("oie")
                                # print("###### melhor ######")
                                # print(loglikmin)
                            }
                            incProgress(1/length(myseq), detail = paste(round(100*i/length(myseq),1),"%"))
                            Sys.sleep(0.1)
                        }
                        })#}
                    }
                }
                
                return(fit1)
            }
            
            
        } else {  #qlqr val DIF 1 (sem covariavel val!=1)
            
            Y<-ts(file[,1],start=c(input$start,input$end),frequency=input$freq) # time series
            
            # sample size
            # n<-length(Y)
            
            # number of forecast steps
            h1 <- input$sh1
            
            # Taking off the last 12 observations
            # n<-n-h1
            
            y<-ts(Y[1:(length(Y))], start=c(input$start,input$end),frequency=input$freq)#[1:(length(Y)-h1)
            
            n<-length(y)
            
            t <- 1:n # in sample
            t_hat <- (n+1):(n+h1) # out of sample
            # C<-cos(2*pi*t/12) # in sample
            # C_hat<-cos(2*pi*t_hat/12) # out of sample
            S<-sin(2*pi*t/12)
            S_hat<-sin(2*pi*t_hat/12)
            mX<-cbind(S)#S,C)
            mX_hat<-cbind(S_hat)#S_hat,C_hat)
            ########
            modelInput <- modelInput()
            
            # funlinkInput <- as.character(funlinkInput())
            funlinkInput <- funlinkInput()
            # linktemp <- eval(link)
            
            arInput <- as.numeric(arInput())
            maInput <- as.numeric(maInput())
            
            # if(modelInput == "karma"){
            #     source("karma.r")
            #     if(funlinkInput() == "logit") fit1 <- karma(y,ar=arInput,ma=maInput,h=h1, link = "logit")
            #     if(funlinkInput() == "probit") fit1 <- karma(y,ar=arInput,ma=maInput,h=h1, link = "probit")
            #     if(funlinkInput() == "cloglog") fit1 <- karma(y,ar=arInput,ma=maInput,h=h1, link = "cloglog")
            #     if(funlinkInput() == "aoz") fit1 <- karma(y,ar=arInput,ma=maInput,h=h1, link = "aoz", lambda=3)
            # }
            
            if(modelInput == "barma" & input$exemplo==FALSE){
                source("barma.r")
                if(funlinkInput() == "logit") fit1 <- barma(y, ar=arInput, ma=maInput, h=h1, link = "logit")
                if(funlinkInput() == "probit") fit1 <- barma(y, ar=arInput, ma=maInput, h=h1, link = "probit")
                if(funlinkInput() == "cloglog") fit1 <- barma(y, ar=arInput, ma=maInput, h=h1,  link = "cloglog")
                if(funlinkInput() == "aoz"){ #if(input$optimal==FALSE) fit1 <- barma(y, ar=arInput, ma=maInput, h=h1, link = "aoz", lambda=1.5) 
                
                    withProgress(message="Loading...", value=0,{
                        
                # if(input$optimal==TRUE){
                    fit<-barma(y, ar=1, diag=0,link = "aoz",lambda=0.5)
                    loglikmin<-fit$loglik
                    myseq<-c(0.666,2, 4) ;i=0 #c(1.3,3.3, 5)
                    for(lzera in myseq){i=i+1
                        xii<-barma(y, ar=arInput, ma=maInput, h=h1, link="aoz",lambda=lzera,diag=0) 
                        # print(paste("Lambda =",lzera));print(paste("Log-Likelihood =",round(xii$loglik,4)));cat("\n-------------------------\n")
                        
                        if(loglikmin<xii$loglik){   
                            loglikmin<-xii$loglik
                            best_model_loglik <- xii$model
                            fit1 <- xii
                            # print("###### melhor ######")
                            # print(loglikmin)
                        }
                        incProgress(1/length(myseq), detail = paste(round(100*i/length(myseq),1),"%"))
                        Sys.sleep(0.1)
                    }
                    })#}
                }
                
            }
            
            # maybe
            if(modelInput == "barma" & input$exemplo==TRUE){
                source("barma.r")
                if(funlinkInput() == "logit") fit1 <- barma(y, ar=arInput, ma=maInput,X=mX, X_hat=mX_hat, h=h1, link = "logit")
                if(funlinkInput() == "probit") fit1 <- barma(y, ar=arInput, ma=maInput,X=mX, X_hat=mX_hat, h=h1, link = "probit")
                if(funlinkInput() == "cloglog") fit1 <- barma(y, ar=arInput, ma=maInput,X=mX, X_hat=mX_hat, h=h1,  link = "cloglog")
                if(funlinkInput() == "aoz"){ #if(input$optimal==FALSE) 
                    # data_ <-ts(dados,start = c(2001,1), freq = 12)
               
                    # h1=8
                    # data_ <- data_[1:(length(data_)-h1)]
                    
                    # data_ <-ts(data_,start = c(2001,1), freq = 12)
                    
                    # n<-length(data_)
                    # n<-n-h1
                    
                    # data<-ts(data_[1:n], start=c(2001,1), frequency = 12)
                    # n=length(y)
                    # t <- 1:n # in sample
                    # t_hat <- (n+1):(n+h1) # out of sample
                    # # C<-cos(2*pi*t/12) # in sample
                    # # C_hat<-cos(2*pi*t_hat/12) # out of sample
                    # # S<-sin(2*pi*t/12)
                    # S_hat<-sin(2*pi*t_hat/12)
                    # mX<-cbind(S)#S,C)
                    # mX_hat<-cbind(S_hat)#S_hat,C_hat)
                    
                    if(length(arInput)==1){arInput[arInput==1]<-2} ; arInputz<-unique(arInput)
                    fit1 <- barma(y, ar=arInputz, ma=maInput,X=mX, X_hat=mX_hat, h=h1, link = "aoz", lambda=1.9) }

            }
            
            
            return(fit1)
        }
        
    })
    
    # output coef
    # output$coef <- renderPrint({
    #     z <- model.fit()
    #     ret <- z$coeff
    #     return(ret)
    # })
    
    # input residuals
    residc <- reactive({
        z<- model.fit()
        
        if(input$resid == "resid1") ret <- z$resid1
        if(input$resid == "resid2") ret <- z$resid2
        if(input$resid == "resid3") ret <- z$resid3
        if(input$resid == "resid1" & input$exemplo==TRUE & input$funklink=="Logit"){
            ret <- c(0.095,  -0.977, -1.873, -1.573, -0.41,  -0.479, 1.924,  2.308,  1.237,  0.608,  1.822,  1.805,  0.515,  -0.098, 0.393,  1.503,  1.614,  0.036,  -0.963, 1.81,  
                     1.098,  -0.1,   1.705,  1.317,  -0.137, -0.852, 0.659,  0.103,  -1.423, -0.508, 1.216,  2.125,  1.74,   1.315,  0.272,  0.674,  1.356,  -0.307, -1.226, -0.406,
                     -0.319, -1.461, -2.156, -1.481, -0.488, 2.391,  1.781,  0.151,  -0.794, -0.99,  0.49,   1.035,  1.295,  -0.577, -1.006, 1.544,  1.357,  -0.086, -0.142, -0.854,
                     -1.711, -0.551, 1.021,  2.382,  1.414,  -0.114, 1.334,  1.634,  0.78,   -0.016, -0.026, -0.524, -0.964, -1.064, -1.634, -1.593, -1.33,  -0.913, -0.723, -1.015,
                     -0.359, 0.28,   0.676,  0.958,  1.426,  1.222,  1.44,   0.036,  -0.043, -0.908, -0.867, -0.293, 0.952,  0.759,  -0.22,  -0.995, -0.916, -0.158, 0.745,  0.504, 
                     -0.984, -0.448, -0.751, 2.039,  2.091,  -0.491, -1.257, -0.427, -0.633, -1.157, -1.067, -0.664, 0.839,  1.594,  1.718,  1.474,  1.398,  1.359,  1.104,  1.339, 
                     0.959,  0.548,  1.441,  0.922,  0.367,  -0.095, -1.273, -1.431, -1.45,  1.355,  2.036,  1.851,  1.598,  0.904,  -0.575, -0.655, 2.017,  1.745,  0.716,  0.661, 
                     -0.631, -1.536, 0.231,  -0.315, -1.647, -0.968, 0.106,  0.924,  0.696,  -0.627, -2.001, -1.458, -1.077, -0.919, -0.178, -0.365, 0.919,  0.434,  -0.673, 1.371, 
                     1.631,  1.133,  1.333,  1.105,  -1.09,  -1.629, -0.147, -1.269, -0.421, -0.315, 0.011,  2.699,  1.527,  -1.405, -0.212, 1.136,  -0.477, -0.967, 0.154,  -0.34, 
                     -1.185, -1.273, -0.815, 0.865,  2.657,  0.208,  -0.985, 2.08,   1.83,   1.303,  0.968,  0.963,  1.733,  0.616,  0.71,   0.865,  0.497,  0.931,  -0.022, 0.618, 
                     -0.097, -0.928, 0.04,   -0.368, -0.907, -0.74,  1.348,  2.308,  -0.434, -2.29,  -2.224, -0.687, 0.612,  -0.005, 1.717,  0.952,  -0.066, -0.058, -1.009, -0.842,
                     -0.814, -1.497, -0.712, 1.313,  0.556,  -0.585, -1.116, -0.915, -0.145, -0.469, 1.304,  1.963,  0.049,  -2.225, -1.998, -1.292, -1.155, -1.322, -1.467, -1.453,
                     -1.487, -1.608, -1.473, -0.155, 0.779,  0.142,  -1.891, -2.705, -2.1,   -0.828, 1.127,  1.262,  0.28,   -0.341, -0.267, 0.162,  -1.311, -2.807, -1.852, 0.382, 
                     0.173)
        } 
        if(input$resid == "resid1" & input$exemplo==TRUE & input$funklink=="Probit"){
            ret<-c(0.103,  -1.014, -1.775, -1.359, -0.219, -0.427, 2.051,  2.156,  1.057,  0.574,  1.817,  1.68,   0.491,  -0.101, 0.361,  1.452,  1.494,  -0.158, -0.959, 1.876, 
                   0.893,  -0.144, 1.722,  1.123,  -0.276, -0.89,  0.718,  -0.03,  -1.495, -0.35,  1.272,  2.039,  1.589,  1.26,   0.205,  0.728,  1.262,  -0.522, -1.235, -0.32, 
                   -0.316, -1.464, -1.99,  -1.23,  -0.313, 2.517,  1.519,  -0.047, -0.841, -0.955, 0.597,  0.967,  1.203,  -0.793, -0.965, 1.653,  1.178,  -0.245, -0.142, -0.911,
                   -1.661, -0.356, 1.087,  2.318,  1.198,  -0.28,  1.353,  1.532,  0.679,  -0.041, -0.046, -0.59,  -0.958, -0.991, -1.548, -1.408, -1.15,  -0.743, -0.598, -0.918,
                   -0.211, 0.331,  0.655,  0.889,  1.329,  1.067,  1.334,  -0.174, -0.068, -0.983, -0.805, -0.212, 0.983,  0.644,  -0.334, -1.002, -0.836, -0.064, 0.766,  0.415, 
                   -1.085, -0.337, -0.735, 2.161,  1.908,  -0.759, -1.168, -0.361, -0.643, -1.109, -0.945, -0.541, 0.947,  1.531,  1.601,  1.359,  1.343,  1.319,  1.101,  1.337, 
                   0.895,  0.515,  1.401,  0.77,   0.287,  -0.186, -1.344, -1.336, -1.328, 1.558,  1.876,  1.682,  1.453,  0.766,  -0.72,  -0.647, 2.078,  1.567,  0.601,  0.634, 
                   -0.736, -1.497, 0.366,  -0.414, -1.643, -0.778, 0.209,  0.931,  0.601,  -0.746, -1.965, -1.235, -0.932, -0.799, -0.054, -0.356, 0.999,  0.307,  -0.745, 1.479, 
                   1.486,  0.988,  1.264,  1.01,   -1.225, -1.511, -0.027, -1.337, -0.257, -0.297, 0.053,  2.762,  1.259,  -1.638, -0.111, 1.128,  -0.678, -0.915, 0.238,  -0.415,
                   -1.168, -1.155, -0.678, 0.991,  2.618,  -0.134, -0.951, 2.159,  1.646,  1.265,  0.937,  0.998,  1.683,  0.468,  0.726,  0.762,  0.394,  0.875,  -0.172, 0.62,  
                   -0.215, -0.951, 0.125,  -0.434, -0.885, -0.65,  1.461,  2.193,  -0.782, -2.242, -2.028, -0.447, 0.703,  -0.094, 1.773,  0.724,  -0.185, -0.078, -1.074, -0.738,
                    -0.742, -1.42,  -0.516, 1.426,  0.39,   -0.657, -1.075, -0.818, -0.046, -0.476, 1.404,  1.842,  -0.218, -2.291, -1.783, -1.093, -1.02,  -1.194, -1.326, -1.306,
                    -1.351, -1.473, -1.318, 0.032,  0.824,  0.077,  -1.931, -2.477, -1.818, -0.601, 1.263,  1.151,  0.143,  -0.384, -0.249, 0.179,  -1.384, -2.662, -1.52,  0.617, 
                    0.12)
        }
        if(input$resid == "resid1" & input$exemplo==TRUE & input$funklink=="Cloglog"){
            ret<-c(-0.159, -1.079, -1.786, -1.235, 0.011,  -0.14,  2.098,  2.124,  0.857,  0.474,  1.619,  1.423,  0.364,  -0.196, 0.293,  1.394,  1.443,  -0.133,
                   -0.75,  1.833,  0.718,  -0.241, 1.488,  0.827,  -0.496, -1.033, 0.635,  -0.019, -1.38,  -0.203, 1.338,  1.966,  1.418,  1.124,  0.009,  0.594, 
                   0.987,  -0.714, -1.251, -0.288, -0.2,   -1.295, -1.816, -1.143, -0.278, 2.377,  1.314,  -0.387, -1.047, -1.102, 0.536,  1.005,  1.195,  -0.731,
                   -0.81,  1.619,  1.034,  -0.452, -0.35,  -1.181, -1.852, -0.492, 1.116,  2.312,  1.137,  -0.227, 1.384,  1.433,  0.58,   -0.148, -0.252, -0.86, 
                   -1.172, -1.12,  -1.532, -1.289, -0.909, -0.484, -0.372, -0.806, -0.232, 0.227,  0.433,  0.629,  1.075,  0.862,  1.205,  -0.227, 0.034,  -0.872,
                    -0.676, -0.179, 0.888,  0.461,  -0.594, -1.245, -1.045, -0.181, 0.747,  0.449,  -0.99,  -0.197, -0.603, 2.119,  1.782,  -1.041, -1.278, -0.646,
                    -0.84,  -1.222, -0.945, -0.415, 1.097,  1.631,  1.541,  1.274,  1.244,  1.177,  0.961,  1.166,  0.704,  0.406,  1.299,  0.697,  0.342,  -0.106,
                    -1.211, -1.258, -1.365, 1.371,  1.692,  1.36,   1.208,  0.581,  -0.794, -0.58,  2.076,  1.522,  0.588,  0.648,  -0.812, -1.584, 0.115,  -0.644,
                    -1.845, -0.9,   0.248,  1.019,  0.671,  -0.656, -1.797, -1.167, -0.929, -0.891, -0.219, -0.517, 0.777,  0.22,   -0.827, 1.483,  1.504,  0.949, 
                    1.253,  0.947,  -1.274, -1.551, -0.287, -1.545, -0.478, -0.346, 0.013,  2.769,  1.215,  -1.533, 0.045,  1.074,  -0.8,   -1.033, -0.01,  -0.647,
                    -1.368, -1.26,  -0.656, 1.1,    2.695,  -0.186, -0.716, 2.096,  1.468,  1.144,  0.762,  0.868,  1.466,  0.293,  0.711,  0.7,    0.425,  0.905, 
                    -0.15,  0.633,  -0.338, -1.083, -0.119, -0.674, -1.09,  -0.766, 1.414,  2.204,  -0.848, -1.91,  -1.873, -0.395, 0.72,   -0.248, 1.516,  0.455, 
                    -0.439, -0.24,  -1.125, -0.676, -0.584, -1.225, -0.374, 1.482,  0.314,  -0.849, -1.284, -1.051, -0.228, -0.548, 1.334,  1.863,  -0.272, -2.041,
                    -1.61,  -1.021, -1.003, -1.271, -1.452, -1.428, -1.404, -1.435, -1.171, 0.301,  1.186,  0.219,  -1.781, -2.369, -1.813, -0.62,  1.216,  1.007, 
                    -0.112, -0.53,  -0.302, 0.219,  -1.24,  -2.44,  -1.366, 0.772,  0.208)
        }
        return(ret)
    })
    
    # print residuals
    # output$resid0 <- renderPrint({
    #     resid0 <- residc()
    #     return(resid0)
    # })
    output$insample <- renderPrint({
        z <- model.fit()
        yin<-z$y.in
        n<-length(z$y_prev)
        insample <- na.omit(z$y_prev[1:(n-input$sh1)])#insample <- z$y_prev[1:length(yin)]
        cat("In-sample","\n")
        cat(round(insample,4),sep="  ")
        # cat("\n")
        # cat(round(yin,4),sep="  ")
        # cat("\n","RMSE:",round(sqrt(mean((yin[2:length(insample)]-insample)^2)),3)," length(y.in)",length(yin),"\n", "length(insample)",length(insample),"\n", "length(y_prev)",n,length(z$y_prev) )
    })
    
    output$outofsample <- renderPrint({
        z <- model.fit()
        n<-length(z$y_prev)
        outsample <- z$y_prev[(n+1-input$sh1):n]
        cat("Out-of-sample","\n")
        cat(round(outsample,4),sep="  ")
    })
    
    # graf
    output$resid <- renderPlot({
        z <- model.fit()
        y <- file()
        n <- z$n
        t <- seq(-5,n+6,by=1)
        
        par(mfrow=c(1,1))
        par(mar=c(2.8, 2.7, 1.2, 1))
        par(mgp=c(1.7, 0.45, 0))
        plot(residc,main=" ",xlab="Index",ylab="Residuals", pch = "+",ylim=c(-4,4))
        lines(t,rep(-3,n+12),lty=2,col=1)
        lines(t,rep(3,n+12),lty=2,col=1)
        lines(t,rep(-2,n+12),lty=3,col=1)
        lines(t,rep(2,n+12),lty=3,col=1)
        
        max_y<- max(c(z$fitted,y),na.rm=T)
        min_y<- min(c(z$fitted,y),na.rm=T)
        plot(as.vector(z$fitted), as.vector(y), main=" ", pch = "+",
             xlab="Fitted values",ylab="Observed data",
             xlim=c(0.95*min_y,max_y*1.05),
             ylim=c(0.95*min_y,max_y*1.05))
        lines(c(-0.2,1.2),c(-0.2,1.2),lty=2)
        
    })
    
    ### plot 4 acf residual
    output$plot4 <- renderPlot({
        z <- model.fit()
        
        if(input$resid == "resid1") residc <- z$resid1
        if(input$resid == "resid2") residc <- z$resid2
        if(input$resid == "resid3") residc <- z$resid3
        
        return(acf(residc, main=" ",ylab="ACF",xlab="Lag"))
    })
    
    ### plot 5 pacf residual
    output$plot5 <- renderPlot({
        z <- model.fit()
        
        if(input$resid == "resid1") residc <- z$resid1
        if(input$resid == "resid2") residc <- z$resid2
        if(input$resid == "resid3") residc <- z$resid3
        
        return(pacf(residc, main=" ",ylab="PACF",xlab="Lag"))
    })
    
    ### plot 6 fitted and forecast
    output$plot6 <- renderPlot({
        z <- model.fit()
        n = as.numeric(z$n); h1 = as.numeric(z$h1); y = z$y.in; ar = z$ar; ma = z$ma
        y_prev <- z$y_prev
        
        # fim<-end(y)[1]+end(y)[2]/12
        
        y_prev <- ts(y_prev, start=c(input$start,input$end),frequency=input$freq)
        par(mfrow=c(1,1))
        par(mar=c(4, 3, 2, 2))
        par(oma=c(.1,.1,.1,.1))
        par(mgp=c(1.6, 0.6, 0))
        plot(y_prev,type="l",col="red", ylim=c(min(y,na.omit(y_prev)),max(y,na.omit(y_prev))),ylab="Serie",xlab="Time",
             cex.axis=1.5,cex.lab=2)
        tsp=attributes(y)$tsp
        abline(v=tsp[2],lty=2)#v=fim
        lines(y)
        # dates=seq(min(y), max(y), "month"); seq(as.Date(paste0(input$start,"-",input$end,"-01")), as.Date(paste0(2020,"-",12,"-01")),"month")
        # axis(1, at= seq(tsp[1], tsp[2], along=y), labels = format(dates, "%Y-%m"))
        
        
    })
    
    ### plot 7 resid x indice
    output$plot7 <- renderPlot({
        
        z <- model.fit()
        residc <- residc()
        n <- z$n
        t <- seq(-5,n+6,by=1)
        plot(residc,main=" ",xlab="Index",ylab="Residuals", pch = "+",ylim=c(-4,4))
        
        lines(t,rep(-3,n+12),lty=2,col=1)
        lines(t,rep(3,n+12),lty=2,col=1)
        lines(t,rep(-2,n+12),lty=3,col=1)
        lines(t,rep(2,n+12),lty=3,col=1)
    })
    
    ### plot 8 qq_plot.eps
    output$plot8 <- renderPlot({
        z <- model.fit()
        residc <- residc()
        
        max_r<- max(residc,na.rm=T)
        min_r<- min(residc,na.rm=T)
        
        qqnorm(residc, pch = "+",
               xlim=c(0.95*min_r,max_r*1.05),
               ylim=c(0.95*min_r,max_r*1.05),
               main="",xlab="Normal quantiles",ylab="Empirical quantiles")
        lines(c(-10,10),c(-10,10),lty=2)
    })
    
    
    # output$plotenv <- renderPlot({
    #     z <- model.fit()
    #     residc <- residc()
    #     
    #     hnp::hnp(residc, xlab="Normal quantiles",ylab="Empirical quantiles",halfnormal=F,conf=.9)
    # })
    
    GG = observeEvent(input$add,{
        output$p1 <- renderPlot({
            z <- model.fit()
            residc <- residc()
            
            hnp::hnp(residc, xlab="Normal quantiles",ylab="Empirical quantiles",halfnormal=F,conf=.95,sim=input$nsimu, cex=1.369, pch="+")
        })
    })
    
    output$p1 <- renderPlot({
        GG
    })
    
    # grafico lags
    # output$modelop <- renderPlot({
    #     y<-file()/100 # percentage
    #     Y<-ts(y,start=c(input$start,input$end),frequency=input$freq) # time series
    #     # sample size
    #     n<-length(Y)
    #     
    #     y<-ts(Y[1:n], start=c(input$start,input$end),frequency=input$freq)
    #     
    #     # some graphics
    #     
    #     # ret<- plot(y)
    #     n<-length(Y)
    #     
    #     h1 <- input$sh1
    #     
    #     # Taking off the last 12 observations
    #     n<-n-h1
    #     
    #     y<-ts(Y[1:n], start=c(input$start,input$end),frequency=input$freq)
    #     #
    #     t <- 1:n # in sample
    #     t_hat <- (n+1):(n+h1) # out of sample
    #     #
    #     # deterministic seazonality
    #     C<-cos(2*pi*t/12) # in sample
    #     C_hat<-cos(2*pi*t_hat/12) # ou
    #     #
    #     source("karma.r")
    #     #
    #     arInput <- as.numeric(arInput())
    #     maInput <- as.numeric(maInput())
    #     if(funlinkInput() == "logit") fit1 <- karma(y,ar=arInput,ma=maInput,h=h1,X = C,X_hat=C_hat, link = "logit")
    #     if(funlinkInput() == "probit") fit1 <- karma(y,ar=arInput,ma=maInput,h=h1,X = C,X_hat=C_hat, link = "probit")
    #     if(funlinkInput() == "cloglog") fit1 <- karma(y,ar=arInput,ma=maInput,h=h1,X = C,X_hat=C_hat, link = "cloglog")
    #     if(funlinkInput() == "aoz") fit1 <- karma(y,ar=arInput,ma=maInput,h=h1,X = C,X_hat=C_hat, link = "aoz")
    #     return(fit1)
    # })
    
    
    
}

# Create Shiny app ----
shinyApp(ui, server)
