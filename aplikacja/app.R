library(magrittr)
library(plotly)
library(DT)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(shiny)

source("./Conf/janek.R")

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              box(
                title = "Fitted model", width = NULL, solidHeader = TRUE, status = "primary",
                plotlyOutput("plot_model", height = 600)
              )
            ),
            fluidRow(
              infoBox(verbatimTextOutput("R_2")),
              infoBox(verbatimTextOutput("coefficients")),
              infoBox(verbatimTextOutput("correlation"))
            ),
            fluidRow(
              column(width = 6,
                     box(
                       title = "Fitted residuals", width = NULL, solidHeader = TRUE, status = "primary",
                       plotlyOutput("plot_res_fitted", width = "100%")
                     ),
                     box(
                       title = "QQ Plot of Residuals", width = NULL, solidHeader = TRUE, status = "primary",
                       plotlyOutput("plot_res_qq")
                     )
              ),
              
              column(width = 6,
                     box(
                       title = "Title 3", width = NULL, solidHeader = TRUE, status = "primary",
                       plotlyOutput("plot_res_scale")
                     ),
                     box(
                       title = "Title 4", width = NULL, solidHeader = TRUE, status = "primary",
                       plotlyOutput("plot_res_leverage")
                     )
              )
            )
    ),
    tabItem(tabName = "widgets",
            fluidRow(
              box(width = NULL, solidHeader = TRUE, status = "primary",
                  uiOutput("stats_selector"))
            ),
            fluidRow(
              box(width =NULL, solidHeader = TRUE, status = "primary",
                  DT::dataTableOutput("table1"))
            ),
            fluidRow(
              box(plotlyOutput("boxplot_basic")),
              box(plotOutput("histogram_basic"))
            )
            
    )
  )
)
  

sidebar <- dashboardSidebar(
  sidebarMenu(
  menuItem("Models", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Basic statistics", icon = icon("th"), tabName = "widgets"),
  selectInput("model_type", label="Type of Model", choices = c("Linear","Quadratic","Exponential"), selected = "Linear"),
  uiOutput("dep_selector"),
  uiOutput("dep_range_slider"),
  uiOutput("indep_selector"),
  uiOutput("indep_range_slider"),
  selectInput("data", label="Data Set", choices = c("Ethmoid Bone","Measurement methods"), selected = c("Ethmoid Bone"))
)
)


# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "Medical research"),
  sidebar,
  body
)

server <- function(input, output) {
  #print(dir())
  #source("./Conf/janek.R")
  Model_Select <- reactive({input$model_type})
  Dependent_Select <- reactive({input$dependent})
  Independent_Select <- reactive({input$independent})
  Stats_Select <- reactive({input$tabele})
  Dep_Range <- reactive({input$dep_range})
  Indep_Range <- reactive({input$indep_range})
  
  Data_Select <- reactive({
    dane1<-Data_Select_NA()
    dep_rng <- Dep_Range()
    dep <- Dependent_Select()
    indep_rng <- Indep_Range()
    indep <- Independent_Select()
    dane1 <- dane1[c(dep_rng[2]> dane1[,dep])&c(dane1[,dep] > dep_rng[1])&c(indep_rng[2]> dane1[,indep])& c(dane1[,indep] > indep_rng[1]), ]
    dane1
    })
  Data_Select_Full <- reactive({
    if(input$data == "Ethmoid Bone"){
      dane1 <- read.csv("Conf/dane analiza medyczna.csv", encoding = "UTF-8")
      nazwy <- read.csv("Conf/ethmoid_bone.csv", encoding = "UTF-8")
    } else if (input$data == "Measurement methods"){
      dane1 <- read.csv("Conf/dane2.csv", encoding = "UTF-8")
      nazwy <- read.csv("Conf/measurement.csv", encoding = "UTF-8")
    }
    names(dane1)<-nazwy$english
    dane1[,nazwy$in_analysis == 1]
  })
  Data_Select_NA <- reactive({
    dane1 <- Data_Select_Full()
    indep <- Independent_Select()
    dep   <- Dependent_Select()
    dane1 <- dane1[!is.na(dane1[,dep]),]
    dane1 <- dane1[!is.na(dane1[,indep]),]
    dane1
  })
  Names_List<-reactive({
    dane1 <- Data_Select_Full()
    names_list <- names(dane1)[as.vector(sapply(names(dane1), function(x) is.numeric(dane1[, x])))]
    names_list
  })
  # Names_Generate <- reactive({
  #   new_names <- Names_List()
  #   new_names <- new_names[-which(new_names == Independent_Select())]
  #   new_names <- new_names[-which(new_names == Dependent_Select())]
  #   new_names
  # })
  output$plot_model <- renderPlotly({
    model <- Model_Select()
    dep <- Dependent_Select()
    indep <- Independent_Select()
    plt <- plot_lm(Data_Select(), dep, indep)
    if (model == "Linear") {
      plt <- plt$p2
    }
    if (model == "Quadratic") {
      plt <- plt$p1
    }
    if (model =="Exponential") {
      plt <- plt$p3
    }
    plt
  })
  
  Fit_Set <- reactive({
    fit_lm_model(dane = Data_Select(), zalezna = Dependent_Select(), niezalezna = Independent_Select())
  })
  
  
  Chosen_Model <- reactive({
    model <- Model_Select()
    fit <- Fit_Set()
    if (model == "Linear") {
      fit <- fit$model_liniowy
    }
    if (model == "Quadratic") {
      fit <- fit$model_kwadratowy
    }
    if (model =="Exponential") {
      fit <- fit[[2]]
    }
    fit
  })
  
  Fit_Model <- reactive({
    fit <- Chosen_Model()
    plots<-RegressionPlots(fit)
    plots
  })
  output$plot_res_fitted <- renderPlotly({
    plots <- Fit_Model()
    plots$p1
  })
  output$plot_res_qq <- renderPlotly({
    plots <- Fit_Model()
    plots$p2
  })
  output$plot_res_scale <- renderPlotly({
    plots <- Fit_Model()
    plots$p3
  })
  output$plot_res_leverage <- renderPlotly({
    plots <- Fit_Model()
    plots$p4
  })
  
  output$table1 <- DT::renderDataTable({
    datatable(BasicStats(Data_Select(),Stats_Select()))
  })
  
  output$boxplot_basic <- renderPlotly({
    box_plot(Data_Select(),Stats_Select())
  })
  
  output$histogram_basic <- renderPlot({
    dane1<-Data_Select()
    hist(dane1[,Stats_Select()])
  })
  
  output$dep_selector <- renderUI({
    names_list <- Names_List()
    selectInput("dependent", label="Dependent variable", choices = names_list, selected = names_list[1])
  })
  
  output$indep_selector <- renderUI({
    names_list <- Names_List()
    selectInput("independent", label="Independent variable", choices = names_list, selected = names_list[4])
  })
  output$stats_selector <- renderUI({
    names_list <- Names_List()
    selectInput("tabele","Basic statistics",choices = c("All",names_list), selected = names_list[1])
  })
  output$dep_range_slider <- renderUI({
    dane1 <- Data_Select_NA()
    depend <- Dependent_Select()
    mi <- floor(min(dane1[,depend]))
    ma <- ceiling(max(dane1[,depend]))
    sliderInput("dep_range",label="Dependent variable range", min=mi, max=ma, value = c(mi,ma) )
  })  
  output$indep_range_slider <- renderUI({
    dane1 <- Data_Select_NA()
    independ <- Independent_Select()
    mi <- floor(min(dane1[,independ]))
    ma <- ceiling(max(dane1[,independ]))
    sliderInput("indep_range",label="Independent variable range", min=mi, max=ma, value = c(mi,ma) )
  })
  output$R_2 <- renderText({
    model<-Chosen_Model()
    summary(model)$r.squared
  })
  output$coefficients <- renderText({
    model<-Chosen_Model()
    summary(model)$coefficients[,1]
  })
  output$correlation <- renderText({
    fit<-Fit_Set()
    fit[[4]]$p.value
  })
  
}

shinyApp(ui, server)
