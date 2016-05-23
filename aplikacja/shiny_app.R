library(magrittr)
library(plotly)
library(DT)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(shiny)



body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              box(
                title = "Histogram", background = "maroon", solidHeader = TRUE, width =NULL,
                plotlyOutput("plot_model", height = 400)
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
  menuItem("Modele", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Podstawowe statystyki", icon = icon("th"), tabName = "widgets"),
  selectInput("model_type", label="Rodzaj modelu", choices = c("Liniowy","Kwadratowy","Eksponencjalny"), selected = "Liniowy"),
  uiOutput("dep_selector"),
  uiOutput("indep_selector"),
  selectInput("data", label="Dane", choices = c("Blaszka","Metody"), selected = c("Blaszka"))
)
)


# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "Badanie Blaszki Sitowej"),
  sidebar,
  body
)

server <- function(input, output) {
  print(dir())
  source("./Conf/janek.R")
  Model_Select <- reactive({input$model_type})
  Dependent_Select <- reactive({input$dependent})
  Independent_Select <- reactive({input$independent})
  Stats_Select <- reactive({input$tabele})
  Data_Select <- reactive({
    if(input$data == "Blaszka"){
      dane1 <- read.csv("Conf/dane analiza medyczna.csv")
    } else if (input$data == "Metody"){
      dane1 <- read.csv("Conf/dane2.csv")
    }
    dane1
    })
  Names_List<-reactive({
    dane1 <- Data_Select()
    names_list <- names(dane1)[as.vector(sapply(names(dane1), function(x) is.numeric(dane1[, x])))]
    names_list
  })
  
  output$plot_model <- renderPlotly({
    model <- Model_Select()
    dep <- Dependent_Select()
    indep <- Independent_Select()
    plt <- plot_lm(Data_Select(), dep, indep)
    if (model == "Liniowy") {
      plt <- plt$p2
    }
    if (model == "Kwadratowy") {
      plt <- plt$p1
    }
    if (model =="Eksponencjalny") {
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
    if (model == "Liniowy") {
      fit <- fit[[1]]
    }
    if (model == "Kwadratowy") {
      fit <- fit[[3]]
    }
    if (model =="Eksponencjalny") {
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
    dane1 <- Data_Select()
    names_list <- Names_List()
    selectInput("dependent", label="Zmienna zalezna", choices = names_list, selected = names_list[2])
  })
  
  output$indep_selector <- renderUI({
    dane1 <- Data_Select()
    names_list <- Names_List()
    selectInput("independent", label="Zmienna niezalezna", choices = names_list, selected = names_list[1])
  })
  output$stats_selector <- renderUI({
    dane1 <- Data_Select()
    names_list <- Names_List()
    selectInput("tabele","Podstawowe statystyki",choices = c("All",names_list), selected = names_list[1])
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
