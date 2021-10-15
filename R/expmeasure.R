#' @title Open Expmeasure software
#'
#' @description 
#' \code{expmeasure} opens a shiny-based software that helps iGEMers to
#' analyse their data in part characterization experiments.
#'
#' @details
#' This function opens opens a shiny-based software that provides a range of
#' statistical tools which can be useful for part characterization in iGEM.
#' Data used in this software should be database file formats with each column
#' specifies a variable and each row for an observation.
#' 
#' @importFrom shiny fluidPage
#' @importFrom shiny fluidRow
#' @importFrom shiny column
#' @importFrom shiny h1
#' @importFrom shiny h2
#' @importFrom shiny sidebarLayout
#' @importFrom shiny fileInput
#' @importFrom shiny uiOutput
#' @importFrom shiny mainPanel
#' @importFrom shiny textOutput
#' @importFrom shiny tableOutput
#' @importFrom shiny htmlOutput
#' @importFrom shiny navbarPage
#' @importFrom shiny tabPanel
#' @importFrom shiny navbarMenu
#' @importFrom shiny renderImage
#' @importFrom shiny reactive
#' @importFrom shiny renderText
#' @importFrom shiny renderTable
#' @importFrom shiny renderUI
#' @importFrom shiny imageOutput
#' @importFrom shiny HTML
#' @importFrom shiny sidebarPanel
#' @importFrom shiny shinyApp
#' @importFrom data.table fread
#' @importFrom shinyWidgets actionBttn
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinyWidgets sliderTextInput
#' @importFrom shinyWidgets pickerInput
#' @importFrom plotly plotlyOutput
#' @importFrom plotly renderPlotly
#' @importFrom plotly plot_ly
#' @importFrom plotly add_histogram
#' @importFrom plotly add_lines
#' @importFrom plotly add_markers
#' @importFrom plotly layout
#' @importFrom plotly add_ribbons
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggforce geom_sina
#' @importFrom ggpubr stat_compare_means
#' @importFrom ggpubr "%>%"
#' @importFrom bootstrap crossval
#' @importFrom rgl plot3d
#' @importFrom rgl spheres3d
#' @importFrom rgl segments3d
#' @importFrom rgl surface3d
#' @importFrom rgl rgl.bbox
#' @importFrom rgl rgl.material
#' @importFrom rgl axes3d
#' @importFrom rgl mtext3d
#' @importFrom car outlierTest
#' @importFrom stats predict
#' @importFrom stats step
#' @importFrom stats lm
#' @importFrom stats t.test
#' @importFrom stats coefficients
#' @importFrom stats fitted
#' @importFrom stats hatvalues
#' @importFrom stats cooks.distance
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @importFrom stats pf
#' @importFrom stats rstudent
#' @importFrom stats dnorm
#' @importFrom stats density
#' @importFrom stats rstudent
#' @importFrom stats qnorm
#' @importFrom stats lsfit
#' @importFrom stats cor
#' @importFrom stats anova
#' @importFrom stats aggregate
#' @importFrom methods as
#' @importFrom graphics grid
#' @importFrom utils combn
#' @importFrom utils head
#' @importFrom agricolae LSD.test
#' @importFrom agricolae HSD.test
#' @importFrom agricolae scheffe.test
#' @importFrom sp Polygon
#' @importFrom sp Polygons
#' @importFrom sp SpatialPolygons
#' @importFrom sp SpatialPolygonsDataFrame
#' @importFrom sp SpatialPoints
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp coordinates
#' @importFrom raster raster
#' @importFrom raster extent
#' @importFrom raster rasterize
#' @importFrom raster interpolate
#' @importFrom raster xmax
#' @importFrom raster xmin
#' @importFrom raster ymax
#' @importFrom raster ymin
#' @importFrom raster values
#' @importFrom raster "values<-"
#' @importFrom fields Tps
#' @importFrom gstat gstat 
#' @importFrom gstat variogram
#' @importFrom gstat fit.variogram
#' @importFrom gstat vgm
#' @importFrom gstat krige
#' @importFrom shinythemes shinytheme
#' @importFrom shiny icon
#' @importFrom shiny actionLink
#' 
#' 
#' @export
#' @author Wu Hao-Ran <haoranwu@zju.edu.cn>
#' @examples
#' ##Once you execute the following function
#' ##  It will not stop until you close the window of "Expmeasure" GUI
#' \dontrun{
#' expmeasure()
#' }
expmeasure <- function(){
  # library(shiny)
  # library(data.table)
  # library(shinydashboard)  NOT USE
  # library(shinyWidgets)
  # library(plotly)
  # library(agricolae)
  # library(tidyverse)  NOT USE
  # library(ggplot2)
  # library(rgl)
  # library(ggforce)
  # library(ggpubr)
  # library(car)
  # library(sp)
  # library(gstat)
  # library(raster)
  # library(fields)
  # library(bootstrap)
  # library(shinythemes)
  requireNamespace("shiny", quietly = T)
  requireNamespace("data.table", quietly = T)
  requireNamespace("plotly", quietly = T)
  requireNamespace("agricolae", quietly = T)
  requireNamespace("ggplot2", quietly = T)
  requireNamespace("rgl", quietly = T)
  requireNamespace("ggforce", quietly = T)
  requireNamespace("ggpubr", quietly = T)
  requireNamespace("car", quietly = T)
  requireNamespace("sp", quietly = T)
  requireNamespace("gstat", quietly = T)
  requireNamespace("raster", quietly = T)
  requireNamespace("fields", quietly = T)
  requireNamespace("bootstrap", quietly = T)
  requireNamespace("shinythemes", quietly = T)
  
  expmeasure.ui <- function(){
    ui.home <- fluidPage(
      sidebarPanel(fluidRow(column(12, align = "center", imageOutput('igem_icon', height = "250px"))),
                   fluidRow(column(12, align = "center", h1("Expmeasure: A tool for part characterization"))),
                   fluidRow(column(12, align = "center", h2("Developed by ZJU-China, 2021"))),
                   width = 12)
    )
    
    ui.data <- fluidPage(
      sidebarLayout(
        sidebarPanel(fileInput("data_input1", "Experiment:", accept = ".csv"),
                     
                     uiOutput("data_pick1d_response"),
                     uiOutput("data_pick2d_explantory"),
                     
                     width = 3),
        mainPanel(textOutput('data_txtout1'),
                  tableOutput('data_tbout1'),
                  width = 9)
      )
    )
    
    ui.trend.2D_Plot <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "trend_2DPlot_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("trend_2DPlot_pick1d_explantory"),
                     width = 3),
        mainPanel(plotlyOutput('trend_2DPlot_plot1', height = "700px"), width = 9)
      )
    )
    
    ui.trend.3D_Plot <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "trend_3DPlot_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("trend_3DPlot_pick1d_explantory"),
                     uiOutput("trend_3DPlot_pick2d_explantory"),
                     prettyRadioButtons(
                       inputId = "trend_3DPlot_checkbox1_order",
                       label = "Order:", 
                       choices = c("1", "2", "3"),
                       inline = TRUE, 
                       status = "danger",
                       fill = TRUE
                     ),
                     
                     width = 3),
        mainPanel(textOutput('trend_3DPlot_plot1'), width = 9)
      )
    )
    
    ui.trend.VioPlot <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "trend_VioPlot_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("trend_VioPlot_pick1d_explantory"),
                     width = 3),
        mainPanel(plotlyOutput('trend_VioPlot_plot1', height = "700px"), width = 9)
      )
    )
    
    ui.error.Outlier <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "error_Outlier_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("error_Outlier_pick1d_explantory"),
                     uiOutput("error_Outlier_pick2d_explantory"),
                     prettyRadioButtons(
                       inputId = "error_Outlier_checkbox1_order",
                       label = "Order:", 
                       choices = c("1", "2", "3"),
                       inline = TRUE, 
                       status = "danger",
                       fill = TRUE
                     ),
                     
                     width = 3),
        mainPanel(tableOutput('error_Outlier_table1'), width = 9)
      )
    )
    
    ui.error.Leverage <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "error_Leverage_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("error_Leverage_pick1d_explantory"),
                     uiOutput("error_Leverage_pick2d_explantory"),
                     prettyRadioButtons(
                       inputId = "error_Leverage_checkbox1_order",
                       label = "Order:", 
                       choices = c("1", "2", "3"),
                       inline = TRUE, 
                       status = "danger",
                       fill = TRUE
                     ),
                     
                     width = 3),
        mainPanel(tableOutput('error_Leverage_table1'), width = 9)
      )
    )
    
    ui.error.Influence <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "error_Influence_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("error_Influence_pick1d_explantory"),
                     uiOutput("error_Influence_pick2d_explantory"),
                     prettyRadioButtons(
                       inputId = "error_Influence_checkbox1_order",
                       label = "Order:", 
                       choices = c("1", "2", "3"),
                       inline = TRUE, 
                       status = "danger",
                       fill = TRUE
                     ),
                     
                     width = 3),
        mainPanel(tableOutput('error_Influence_table1'), width = 9)
      )
    )
    
    ui.significance.k  <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "significance_k_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     sliderTextInput(
                       inputId = "significance_k_slider1_nsim",
                       label = "Parameter k:", 
                       choices = seq(5,200,5)
                     ),
                     uiOutput("significance_k_pick1d_explantory"),
                     uiOutput("significance_k_pick2d_level"),
                     width = 3),
        mainPanel(plotlyOutput('significance_k_plot1', height = "700px"),
                  width = 9)
      )
    )
    
    ui.significance.GLMs <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "significance_GLMs_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("significance_GLMs_pick1d_explantory"),
                     uiOutput("significance_GLMs_pick2d_explantory"),
                     prettyRadioButtons(
                       inputId = "significance_GLMs_checkbox1_order",
                       label = "Order:", 
                       choices = c("1", "2", "3"),
                       inline = TRUE, 
                       status = "danger",
                       fill = TRUE
                     ),
                     
                     width = 3),
        mainPanel(tableOutput('significance_GLMs_table1'),
                  htmlOutput('significance_GLMs_txt1'),
                  width = 9)
      )
    )
    
    ui.significance.residue <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "significance_residue_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("significance_residue_pick1d_explantory"),
                     uiOutput("significance_residue_pick2d_explantory"),
                     prettyRadioButtons(
                       inputId = "significance_residue_checkbox1_order",
                       label = "Order:", 
                       choices = c("1", "2", "3"),
                       inline = TRUE, 
                       status = "danger",
                       fill = TRUE
                     ),
                     pickerInput(
                       inputId = "significance_residue_pick3_method",
                       label = "Default", 
                       choices = c("Distribution", "Q-Q Plot", "Table")
                     ),
                     width = 3),
        mainPanel(tableOutput('significance_residue_tb1'),
                  plotlyOutput('significance_residue_plot1', height = "700px"),
                  width = 9)
      )
    )
    
    ui.significance.jackknife <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "significance_jackknife_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("significance_jackknife_pick1d_explantory"),
                     uiOutput("significance_jackknife_pick2d_explantory"),
                     prettyRadioButtons(
                       inputId = "significance_jackknife_checkbox1_order",
                       label = "Order:", 
                       choices = c("1", "2", "3"),
                       inline = TRUE, 
                       status = "danger",
                       fill = TRUE
                     ),
                     sliderTextInput(
                       inputId = "significance_jackknife_alide1_k",
                       label = "Parameter k:", 
                       choices = 2:20
                     ),
                     width = 3),
        mainPanel(htmlOutput('significance_jackknife_txt1'),
                  width = 9)
      )
    )
    
    ui.explantory.ANOVA <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "explantory_ANOVA_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("explantory_ANOVA_pick1d_explantory"),
                     uiOutput("explantory_ANOVA_pick2d_explantory"),
                     prettyRadioButtons(
                       inputId = "explantory_ANOVA_checkbox1_order",
                       label = "Order:", 
                       choices = c("1", "2", "3"),
                       inline = TRUE, 
                       status = "danger",
                       fill = TRUE
                     ),
                     
                     width = 3),
        mainPanel(tableOutput('explantory_ANOVA_table1'),
                  width = 9)
      )
    )
    
    ui.explantory.muticomp <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "explantory_muticomp_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("explantory_muticomp_pick1d_explantory"),
                     uiOutput("explantory_muticomp_pick2d_explantory"),
                     prettyRadioButtons(
                       inputId = "explantory_muticomp_checkbox1_order",
                       label = "Order:", 
                       choices = c("1", "2", "3"),
                       inline = TRUE, 
                       status = "danger",
                       fill = TRUE
                     ),
                     pickerInput(
                       inputId = "explantory_muticomp_pick3_method",
                       label = "Default", 
                       choices = c("LSD", "Tukey", "Scheffe", "Bonferroni")
                     ),
                     width = 3),
        mainPanel(tableOutput('explantory_muticomp_table1'),
                  width = 9)
      )
    )
    
    ui.prediction.trend <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "prediction_trend_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("prediction_trend_pick1d_explantory"),
                     uiOutput("prediction_trend_pick2d_explantory"),
                     prettyRadioButtons(
                       inputId = "prediction_trend_checkbox1_order",
                       label = "Order:", 
                       choices = c("1", "2", "3"),
                       inline = TRUE, 
                       status = "danger",
                       fill = TRUE
                     ),
                     width = 3),
        mainPanel(plotlyOutput('prediction_trend_plot1', height = "700px"),
                  width = 9)
      )
    )
    
    ui.prediction.spline <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "prediction_spline_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("prediction_spline_pick1d_explantory"),
                     uiOutput("prediction_spline_pick2d_explantory"),
                     width = 3),
        mainPanel(plotlyOutput('prediction_spline_plot1', height = "700px"),
                  width = 9)
      )
    )
    
    ui.prediction.inverse <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "prediction_inverse_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("prediction_inverse_pick1d_explantory"),
                     uiOutput("prediction_inverse_pick2d_explantory"),
                     sliderTextInput(
                       inputId = "prediction_inverse_slider1_idp",
                       label = "Inverse distance power p:", 
                       choices = seq(0,3,0.2),
                       grid = TRUE
                     ),
                     width = 3),
        mainPanel(plotlyOutput('prediction_inverse_plot1', height = "700px"),
                  width = 9)
      )
    )
    
    ui.prediction.simmov <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "prediction_simmov_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("prediction_simmov_pick1d_explantory"),
                     uiOutput("prediction_simmov_pick2d_explantory"),
                     uiOutput("prediction_simmov_slider1d_R"),
                     width = 3),
        mainPanel(plotlyOutput('prediction_simmov_plot1', height = "700px"),
                  width = 9)
      )
    )
    
    ui.prediction.krigging <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "prediction_krigging_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("prediction_krigging_pick1d_explantory"),
                     uiOutput("prediction_krigging_pick2d_explantory"),
                     width = 3),
        mainPanel(plotlyOutput('prediction_krigging_plot1', height = "700px"),
                  width = 9)
      )
    )
    
    ui.uncertainty.TNE <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "uncertainty_TNE_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("uncertainty_TNE_pick1d_explantory"),
                     uiOutput("uncertainty_TNE_pick2d_explantory"),
                     width = 3),
        mainPanel(plotlyOutput('uncertainty_TNE_plot1', height = "700px"),
                  width = 9)
      )
    )
    
    ui.uncertainty.TNEsimu <- fluidPage(
      sidebarLayout(
        sidebarPanel(fluidRow(column(12, align = "center",
                                     actionBttn(
                                       inputId = "uncertainty_TNEsimu_APPLY",
                                       label = "APPLY!",
                                       style = "pill", 
                                       color = "danger"
                                     ) )),
                     
                     uiOutput("uncertainty_TNEsimu_pick1d_explantory"),
                     uiOutput("uncertainty_TNEsimu_pick2d_explantory"),
                     sliderTextInput(
                       inputId = "uncertainty_TNEsimu_slider1_nsim",
                       label = "Number of Simulation:", 
                       choices = seq(10,200,5)
                     ),
                     width = 3),
        mainPanel(plotlyOutput('uncertainty_TNEsimu_plot1', height = "700px"),
                  width = 9)
      )
    )
    
    ui.overall <- navbarPage("expmeasures",
                             theme = shinytheme("flatly"),
                             tabPanel("Home", ui.home),
                             tabPanel("Data", ui.data),
                             
                             navbarMenu("Trend",
                                        tabPanel("2D Plot", ui.trend.2D_Plot),
                                        tabPanel("3D Plot", ui.trend.3D_Plot),
                                        tabPanel("VioPlot", ui.trend.VioPlot)
                             ),
                             
                             navbarMenu("Error",
                                        tabPanel("Outlier", ui.error.Outlier),
                                        tabPanel("Leverage", ui.error.Leverage),
                                        tabPanel("Influence", ui.error.Influence)
                             ),
                             
                             navbarMenu("Significance",
                                        tabPanel("K-function", ui.significance.k),
                                        tabPanel("GLMs", ui.significance.GLMs),
                                        tabPanel("Residue", ui.significance.residue),
                                        tabPanel("Jackknife", ui.significance.jackknife)
                             ),
                             
                             navbarMenu("Explantory",
                                        tabPanel("ANOVA", ui.explantory.ANOVA),
                                        tabPanel("Muti Comp", ui.explantory.muticomp)
                             ),
                             
                             navbarMenu("Prediction",
                                        tabPanel("Trend", ui.prediction.trend),
                                        tabPanel("Spline", ui.prediction.spline),
                                        tabPanel("Inverse", ui.prediction.inverse),
                                        tabPanel("Simp Mov", ui.prediction.simmov),
                                        tabPanel("Krigging", ui.prediction.krigging)
                             ),
                             
                             navbarMenu("Uncertainty",
                                        tabPanel("TNE", ui.uncertainty.TNE),
                                        tabPanel("T/E Simu", ui.uncertainty.TNEsimu)
                             ),
                             
                             navbarMenu("Quit",
                                        tabPanel(actionLink("stop_radiant", "Stop", icon = icon("power-off"), 
                                                            onclick = "setTimeout(function(){window.close();}, 100); ")
                                        )
                                        
                             )
    )
    
    ui.overall
  }
  server.home <- function(input, output, session) {
    ##home
    output$igem_icon <- renderImage({
      list(src = './expmeasure/R/logo.png')
    }	, deleteFile = FALSE)
    
    ##data
    read.d1 <- reactive({
      inFile1 <- input$data_input1
      if (is.null(inFile1)) return(NULL)
      fread(inFile1$datapath)
    })
    
    output$data_txtout1 <- renderText({
      if(!is.null(input$data_input1)){
        "Experiment:"
      }
    })
    
    output$data_tbout1 <- renderTable({
      dat1 <- read.d1()
      head(dat1)
    })
    
    output$data_pick1d_response <- renderUI({
      pickerInput(
        inputId = "data_pick1_response",
        label = "Select response variable", 
        choices = colnames(read.d1())
      )
    })
    
    output$data_pick2d_explantory <- renderUI({
      choices <- colnames(read.d1())
      choices <- choices[choices != input$data_pick1_response]
      
      pickerInput(
        inputId = "data_pick2_explantory",
        label = "Select explantory variables", 
        choices = choices,
        options = list(
          `selected-text-format` = "count > 3"), 
        multiple = TRUE
      )
    })
    
    #Trend.2D Plot
    trend.2Dplot <- function(dat, x, y){
      txt1 <- paste("dat$",x,sep='')
      txt2 <- paste("dat$",y,sep='')
      
      g <- ggplot(dat, aes(eval(parse(text = txt1)), eval(parse(text = txt2)))) + 
        geom_smooth() + geom_point() + 
        xlab(x) + ylab(y)
      
      return(plotly::ggplotly(g))
    }
    
    output$trend_2DPlot_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "trend_2DPlot_pick1_explantory",
        label = "Select explantory variable", 
        choices = input$data_pick2_explantory
      )
    })
    
    trend_2DPlot_APPLY_click <- 0
    trend_2DPlot_plot1area <- NULL
    output$trend_2DPlot_plot1 <- renderPlotly({
      if(input$trend_2DPlot_APPLY > trend_2DPlot_APPLY_click){
        trend_2DPlot_APPLY_click  <<- trend_2DPlot_APPLY_click + 1
        trend_2DPlot_plot1area <<- trend.2Dplot(read.d1(), 
                                                input$trend_2DPlot_pick1_explantory, 
                                                input$data_pick1_response)
      }
      trend_2DPlot_plot1area
    })
    
    #Trend.3D Plot
    trend.3Dplot <- function(dat, y, var1, var2, order = 2){
      predictgrid<-function(model, xvar, yvar, zvar, res=16, type=NULL){
        if(!(xvar %in% colnames(model$model))){
          stop("Var1 is deleted in stepwise regression. Can not make 3D Plot.")
        }
        
        if(!(yvar %in% colnames(model$model))){
          stop("Var2 is deleted in stepwise regression. Can not make 3D Plot.")
        }
        
        xrange <- range(model$model[[xvar]])
        yrange <- range(model$model[[yvar]])
        
        newdata <- expand.grid(x=seq(xrange[1],xrange[2],length.out=res),
                               y=seq(yrange[1],yrange[2],length.out=res))
        names(newdata) <- c(xvar,yvar)
        newdata[[zvar]] <- predict(model,newdata=newdata,type=type)
        newdata
      }
      
      df2mat<-function(p,xvar=NULL,yvar=NULL,zvar=NULL){
        if(is.null(xvar)) xvar<-names(p)[1]
        if(is.null(yvar)) yvar<-names(p)[2]
        if(is.null(zvar)) zvar<-names(p)[3]
        x<-unique(p[[xvar]])
        y<-unique(p[[yvar]])
        z<-matrix(p[[zvar]],nrow=length(y),ncol=length(x))
        m<-list(x,y,z)
        names(m)<-c(xvar,yvar,zvar)
        m
      }
      
      interleave<-function(v1,v2) as.vector(rbind(v1,v2))
      
      m <- dat
      
      var1.d <- eval(parse(text = paste("dat$",var1,sep='')))
      var2.d <- eval(parse(text = paste("dat$",var2,sep='')))
      y.d <- eval(parse(text = paste("dat$",y,sep='')))
      
      if(order == 1){
        mod <- step(lm(y.d ~ var1.d * var2.d))
      } else if(order == 2){
        mod <- step(lm(y.d ~ var1.d * var2.d * I(var1.d^2) * I(var2.d^2)))
      } else if(order == 3){
        mod <- step(lm(y.d ~ var1.d * var2.d * I(var1.d^2) * I(var2.d^2)
                       * I(var1.d^3) * I(var2.d^3)))
      } else{
        stop("`order` should be 1, 2, or 3.")
      }
      
      pred_y.d <- predict(mod)
      mpgrid_df <- predictgrid(mod, 'var1.d', 'var2.d', 'y.d')
      mpgrid_list <- df2mat(mpgrid_df)
      
      plot3d(var1.d, var2.d, y.d, xlab='', ylab='', zlab='',
             axes=FALSE,size=.5,type='s',lit=FALSE)
      
      spheres3d(var1.d, var2.d, y.d, alpha=0.4, type='s', size=0.5, lit=FALSE)
      
      segments3d(interleave(var1.d, var1.d),
                 interleave(var2.d, var2.d),
                 interleave(y.d, pred_y.d),
                 alpha=0.4,col='red')
      
      surface3d(mpgrid_list$var1.d, mpgrid_list$var2.d,mpgrid_list$y.d,alpha=.4,front='lines',back='lines')
      
      rgl.bbox(color='grey50',emission='grey50',xlen=0,ylen=0,zlen=0)
      rgl.material(color='black')
      axes3d(edges=c('x--','y+-','z--'), cex=.75)
      mtext3d(var1,edge='x--',line=2)
      mtext3d(var2,edge='y+-',line=3)
      mtext3d(y,edge='z--',line=3)
      
      return("Please see the graph in RGL device opened in an independent window.")
    }
    
    output$trend_3DPlot_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "trend_3DPlot_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$trend_3DPlot_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$trend_3DPlot_pick1_explantory]
      
      pickerInput(
        inputId = "trend_3DPlot_pick2_explantory",
        label = "Select variable 2", 
        choices = choices
      )
    })
    
    trend_3DPlot_APPLY_click <- 0
    trend_3DPlot_plot1area <- NULL
    output$trend_3DPlot_plot1 <- renderText({
      if(input$trend_3DPlot_APPLY > trend_3DPlot_APPLY_click){
        trend_3DPlot_APPLY_click  <<- trend_3DPlot_APPLY_click + 1
        trend_3DPlot_plot1area <<- trend.3Dplot(dat = read.d1(), 
                                                y = input$data_pick1_response, 
                                                var1 = input$trend_3DPlot_pick1_explantory,
                                                var2 = input$trend_3DPlot_pick2_explantory,
                                                order = as.numeric(input$trend_3DPlot_checkbox1_order))
      }
      trend_3DPlot_plot1area
    })
    
    #Trend.VioPlot
    trend.vioplot <- function(dat, type, value){
      txt.type <- paste("dat$",type,sep='')
      txt.value <- paste("dat$",value,sep='')
      
      type.d <- eval(parse(text = txt.type))
      value.d <- eval(parse(text = txt.value))
      
      allcomp <- combn(names(table(type.d)),2)
      needcomp <- list()
      for(i in 1:ncol(allcomp)){
        t1 <- dat[type.d == allcomp[1,i],]
        t2 <- dat[type.d == allcomp[2,i],]
        
        res <- t.test(eval(parse(text = paste("t1$",value,sep=''))),
                      eval(parse(text = paste("t2$",value,sep=''))))
        if(res$p.value <= 0.05){
          needcomp <- c(needcomp, list(allcomp[,i]))
        }
      }
      
      g <- ggplot(dat,aes(type.d, value.d, fill=type.d)) +
        geom_violin(na.rm = TRUE,alpha=0.5)+
        geom_sina(aes(),color='black', size = 0.1, alpha=0.5)+
        geom_boxplot(width=0.1, color="black", alpha=0.7)+
        stat_compare_means(label = "p.signif",comparisons = needcomp,method = 't.test',color='blue')+
        xlab(type) + ylab(value) + theme(legend.title=element_blank())
      
      return(plotly::ggplotly(g))
    }
    
    output$trend_VioPlot_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "trend_VioPlot_pick1_explantory",
        label = "Select explantory variable", 
        choices = input$data_pick2_explantory
      )
    })
    
    trend_VioPlot_APPLY_click <- 0
    trend_VioPlot_plot1area <- NULL
    output$trend_VioPlot_plot1 <- renderPlotly({
      if(input$trend_VioPlot_APPLY > trend_VioPlot_APPLY_click){
        trend_VioPlot_APPLY_click  <<- trend_VioPlot_APPLY_click + 1
        trend_VioPlot_plot1area <<- trend.vioplot(read.d1(), 
                                                  input$trend_VioPlot_pick1_explantory, 
                                                  input$data_pick1_response)
      }
      trend_VioPlot_plot1area
    })
    
    #Error.Outlier
    fit.expr <- function(dat, y, var1, var2=NA, order = 2){
      m <- dat
      y.d <- dat[[y]]
      var1.d <- dat[[var1]]
      
      if(order != 1 && order != 2 && order !=3){
        stop("`order` should be 1, 2, or 3.")
      }
      
      if(!is.na(var2)){
        var2.d <- dat[[var2]]
        
        if(order == 1){
          mod <- step(lm(y.d ~ var1.d * var2.d))
        } else if(order == 2){
          mod <- step(lm(y.d ~ var1.d * var2.d * I(var1.d^2) * I(var2.d^2)))
        } else if(order == 3){
          mod <- step(lm(y.d ~ var1.d * var2.d * I(var1.d^2) * I(var2.d^2)
                         * I(var1.d^3) * I(var2.d^3)))
        }
        
      } else {
        if(order == 1){
          mod <- step(lm(y.d ~ var1.d))
        } else if(order == 2){
          mod <- step(lm(y.d ~ var1.d * I(var1.d^2)))
        } else if(order == 3){
          mod <- step(lm(y.d ~ var1.d * I(var1.d^2) * I(var1.d^3)))
        }
      }
      
      mod
    }
    error.outlier <- function(dat, y, var1, var2=NA, order = 2){
      res <- outlierTest(fit.expr(dat, y, var1, var2, order))
      data.frame(id = names(res$rstudent), 
                 rstudent = res$rstudent, 
                 p = res$p, 
                 bonf.p = res$bonf.p)
    }
    
    output$error_Outlier_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "error_Outlier_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$error_Outlier_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$error_Outlier_pick1_explantory]
      
      pickerInput(
        inputId = "error_Outlier_pick2_explantory",
        label = "Select variable 2 (Or Not)", 
        choices = c("Not Use", choices)
      )
    })
    
    error_Outlier_APPLY_click <- 0
    error_Outlier_tb1area <- NULL
    output$error_Outlier_table1 <- renderTable({
      if(input$error_Outlier_APPLY > error_Outlier_APPLY_click){
        error_Outlier_APPLY_click  <<- error_Outlier_APPLY_click + 1
        if(input$error_Outlier_pick2_explantory == "Not Use"){
          error_Outlier_tb1area <<- error.outlier(dat = read.d1(),
                                                  y = input$data_pick1_response,
                                                  var1 = input$error_Outlier_pick1_explantory,
                                                  order = input$error_Outlier_checkbox1_order)
        } else{
          error_Outlier_tb1area <<- error.outlier(dat = read.d1(),
                                                  y = input$data_pick1_response,
                                                  var1 = input$error_Outlier_pick1_explantory,
                                                  var2 = input$error_Outlier_pick2_explantory,
                                                  order = input$error_Outlier_checkbox1_order)
        }
        
      }
      error_Outlier_tb1area
    })
    
    #Error.Leverage
    error.leverage <- function(dat, y, var1, var2=NA, order = 2, loose = F){
      mod <- fit.expr(dat, y, var1, var2, order)
      
      p <- length(coefficients(mod))
      n <- length(fitted(mod))
      hat_val <- hatvalues(mod)
      
      if(loose){
        hat_val <- hat_val[hat_val > 2 * p/n]
      } else{
        hat_val <- hat_val[hat_val > 3 * p/n]
      }
      
      data.frame(id=names(hat_val), hat=hat_val)
    }
    
    output$error_Leverage_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "error_Leverage_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$error_Leverage_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$error_Leverage_pick1_explantory]
      
      pickerInput(
        inputId = "error_Leverage_pick2_explantory",
        label = "Select variable 2 (Or Not)", 
        choices = c("Not Use", choices)
      )
    })
    
    error_Leverage_APPLY_click <- 0
    error_Leverage_tb1area <- NULL
    output$error_Leverage_table1 <- renderTable({
      if(input$error_Leverage_APPLY > error_Leverage_APPLY_click){
        
        error_Leverage_APPLY_click  <<- error_Leverage_APPLY_click + 1
        
        if(input$error_Leverage_pick2_explantory == "Not Use"){
          error_Leverage_tb1area <<- error.leverage(dat = read.d1(),
                                                    y = input$data_pick1_response,
                                                    var1 = input$error_Leverage_pick1_explantory,
                                                    order = input$error_Leverage_checkbox1_order)
        } else{
          error_Leverage_tb1area <<- error.leverage(dat = read.d1(),
                                                    y = input$data_pick1_response,
                                                    var1 = input$error_Leverage_pick1_explantory,
                                                    var2 = input$error_Leverage_pick2_explantory,
                                                    order = input$error_Leverage_checkbox1_order)
        }
        
      }
      
      error_Leverage_tb1area
    })
    
    #Error.Influence
    error.influence <- function(dat, y, var1, var2=NA, order = 2){
      mod <- fit.expr(dat, y, var1, var2, order)
      
      cutoff <- 4/(nrow(dat) - length(mod$coefficients - 2))
      cooks <- cooks.distance(mod)
      cooks <- cooks[cooks > cutoff]
      
      data.frame(id = names(cooks), cooks = cooks)
    }
    
    output$error_Influence_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "error_Influence_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$error_Influence_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$error_Influence_pick1_explantory]
      
      pickerInput(
        inputId = "error_Influence_pick2_explantory",
        label = "Select variable 2 (Or Not)", 
        choices = c("Not Use", choices)
      )
    })
    
    error_Influence_APPLY_click <- 0
    error_Influence_tb1area <- NULL
    output$error_Influence_table1 <- renderTable({
      if(input$error_Influence_APPLY > error_Influence_APPLY_click){
        
        error_Influence_APPLY_click  <<- error_Influence_APPLY_click + 1
        
        if(input$error_Influence_pick2_explantory == "Not Use"){
          error_Influence_tb1area <<- error.influence(dat = read.d1(),
                                                      y = input$data_pick1_response,
                                                      var1 = input$error_Influence_pick1_explantory,
                                                      order = input$error_Influence_checkbox1_order)
        } else{
          error_Influence_tb1area <<- error.influence(dat = read.d1(),
                                                      y = input$data_pick1_response,
                                                      var1 = input$error_Influence_pick1_explantory,
                                                      var2 = input$error_Influence_pick2_explantory,
                                                      order = input$error_Influence_checkbox1_order)
        }
        
      }
      
      error_Influence_tb1area
    })
    
    #Significance.k
    Kval <- function(simu, plot.fine = 1000, cut = 4, Xvect = NA){
      comb <- combn(simu, 2)
      dif <- abs(comb[1,] - comb[2,])
      
      if(any(is.na(Xvect))){
        low <- min(dif)
        up <- (max(dif) - min(dif))/cut + min(dif)
        X <- seq(low, up, (up - low)/plot.fine)
      } else{
        X <- Xvect
      }
      
      n <- length(simu)
      Y <- c()
      for(r in X){
        res <- 0
        filt <- (simu + r < max(simu)) & (simu - r > min(simu))
        
        for(i in simu[filt]){
          res <- res + sum((simu < i + r) & (simu > i - r))
        }
        res <- res / (n * (n-1))
        
        Y <- c(Y, res)
      }
      
      data.frame(r=X[1:length(Y)], K=Y)
    }
    Kval.envelope <- function(simu, nsim = 99, ...){
      
      res <- Kval(simu, ...)
      colnames(res)[2] <- "Kobs"
      
      for(i in 1:nsim){
        message(i)
        
        simu.CK <- rnorm(length(simu), mean = mean(simu), sd = sd(simu))
        tres <- Kval(simu.CK, Xvect = res[,1], ...)
        
        new.nrow <- min(nrow(tres), nrow(res))
        res <- res[1:new.nrow,]
        tres <- tres[1:new.nrow,]
        
        res <- cbind(res, tres[,2])
      }
      
      
      Kmax <- apply(res[,3:(3+nsim-1)], FUN = max, MARGIN = 1)
      Kmin <- apply(res[,3:(3+nsim-1)], FUN = min, MARGIN = 1)
      
      data.frame(r = res$r, Kobs = res$Kobs, Kmax = Kmax, Kmin = Kmin)
    }
    signif.k <- function(y, nsim=99){
      res<-Kval.envelope(y,nsim)
      ###Otherwise check program report NOTE
      r<-res$r
      Kobs<-res$Kobs
      Kmin<-res$Kmin
      Kmax<-res$Kmax
      ###
      g<-ggplot(res, aes(r, Kobs)) + 
        geom_line(size = 0.5) +
        geom_ribbon(aes(ymin = Kmin, ymax = Kmax), alpha =0.5) +
        xlab("r") + ylab("K")
      plotly::ggplotly(g)
    }
    output$significance_k_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "significance_k_pick1_explantory",
        label = "Select variable 1", 
        choices = c("Not Use", input$data_pick2_explantory)
      )
    })
    output$significance_k_pick2d_level <- renderUI({
      if(input$significance_k_pick1_explantory=="Not Use"){
        choices <- selected <- NA
      } else{
        levels <- names(table(read.d1()[[input$significance_k_pick1_explantory]]))
        if(any(is.na(as.numeric(levels)))){
          choices <- selected <- NA
        } else{
          choices <- as.numeric(levels)
          selected <- c(min(choices), max(choices))
        }
      }
      
      sliderTextInput(
        inputId = "significance_k_pick2_level",
        label = "Select a range:", 
        choices = choices,
        selected = selected
      )
    })
    
    significance_k_APPLY_click <- 0
    significance_k_plot1area <- NULL
    output$significance_k_plot1 <- renderPlotly({
      if(input$significance_k_APPLY > significance_k_APPLY_click){
        significance_k_APPLY_click  <<- significance_k_APPLY_click + 1
        dat <- read.d1()
        var1 <- input$significance_k_pick1_explantory
        y <- dat[[input$data_pick1_response]]
        if(length(input$significance_k_pick2_level)==2){
          left<-input$significance_k_pick2_level[1]
          right<-input$significance_k_pick2_level[2]
          filt <- as.numeric(dat[[var1]])>=as.numeric(left) & as.numeric(dat[[var1]])<=as.numeric(right)
          y <- y[filt]
        }
        significance_k_plot1area <<- signif.k(y = y,
                                              nsim = input$significance_k_slider1_nsim)
      }
      significance_k_plot1area
    })
    
    #Significance.GLMs
    signif.glms <- function(dat, y, var1, order, var2 = NA){
      summary(fit.expr(dat, y, var1, var2, order))
    }
    
    output$significance_GLMs_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "significance_GLMs_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$significance_GLMs_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$significance_GLMs_pick1_explantory]
      
      pickerInput(
        inputId = "significance_GLMs_pick2_explantory",
        label = "Select variable 2 (Or Not)", 
        choices = c("Not Use", choices)
      )
    })
    
    significance_GLMs_APPLY_click <- 0
    significance_GLMs_tb1area <- NULL
    output$significance_GLMs_table1 <- renderTable({
      
      if(input$significance_GLMs_APPLY > significance_GLMs_APPLY_click){
        
        significance_GLMs_APPLY_click  <<- significance_GLMs_APPLY_click + 1
        
        if(input$significance_GLMs_pick2_explantory == "Not Use"){
          res <- signif.glms(dat = read.d1(),
                             y = input$data_pick1_response,
                             var1 = input$significance_GLMs_pick1_explantory,
                             order = input$significance_GLMs_checkbox1_order)
        } else{
          res <- signif.glms(dat = read.d1(),
                             y = input$data_pick1_response,
                             var1 = input$significance_GLMs_pick1_explantory,
                             var2 = input$significance_GLMs_pick2_explantory,
                             order = input$significance_GLMs_checkbox1_order)
        }
        
        cout <- cbind(Item=row.names(res$coefficients),
                      as.data.frame(res$coefficients))
        row.names(cout) <- NULL
        significance_GLMs_tb1area <<- cout
        
      }
      
      significance_GLMs_tb1area
    })
    
    significance_GLMs_APPLY_click_cop <- 0
    significance_GLMs_txt1area <- NULL
    output$significance_GLMs_txt1 <- renderUI({
      
      if(input$significance_GLMs_APPLY > significance_GLMs_APPLY_click_cop){
        
        significance_GLMs_APPLY_click_cop  <<- significance_GLMs_APPLY_click_cop + 1
        
        if(input$significance_GLMs_pick2_explantory == "Not Use"){
          res <- signif.glms(dat = read.d1(),
                             y = input$data_pick1_response,
                             var1 = input$significance_GLMs_pick1_explantory,
                             order = input$significance_GLMs_checkbox1_order)
        } else{
          res <- signif.glms(dat = read.d1(),
                             y = input$data_pick1_response,
                             var1 = input$significance_GLMs_pick1_explantory,
                             var2 = input$significance_GLMs_pick2_explantory,
                             order = input$significance_GLMs_checkbox1_order)
        }
        
        cout <- paste("Multiple R-squared: ",round(res$r.squared,3),
                      "   Adjusted R-squared: ",round(res$adj.r.squared,3),sep='')
        
        if(!is.null(res$fstatistic)){
          F.p <- round(1 - pf(res$fstatistic[1], res$fstatistic[2], res$fstatistic[3]-res$fstatistic[2]),3)
          cout <- paste(cout,"<br />","F-statistic:",round(res$fstatistic[1],3),"on",res$fstatistic[2],
                        "and",res$fstatistic[3],"DF,  p value:",F.p)
        }
        
        significance_GLMs_txt1area <<- HTML(cout)
      }
      
      significance_GLMs_txt1area
    })
    
    #Significance.Residue
    residplot <- function(fit){
      z <- rstudent(fit)
      no.x <- seq(min(z), max(z), (max(z)-min(z))/1000)
      no.y <- dnorm(no.x,mean=mean(z),sd=sd(z))
      k.x <- density(z)$x
      ker.x <- k.x[k.x >= min(z) & k.x <= max(z)]
      k.y <- density(z)$y
      ker.y <- k.y[k.x >= min(z) & k.x <= max(z)]
      
      plot_ly(alpha = 0.6) %>%
        add_histogram(x = ~z, histnorm="probability", name="Histogram") %>%
        add_lines(x = ~no.x, y = ~no.y, name="Normal Curve") %>%
        add_lines(x = ~ker.x, y = ~ker.y, name="Kernel Denity Curve") %>%
        layout(xaxis = list(title = "Studentized Residual"),
               yaxis = list(title = "Density"))
    }
    error.residplot <- function(dat, y, var1, var2=NA, order = 2){
      residplot(fit.expr(dat, y, var1, var2, order))
    }
    error.residqq <- function(dat, y, var1, var2=NA, order = 2){
      fit <- fit.expr(dat, y, var1, var2, order)
      y <- rstudent(fit)
      N <- length(y)
      n <- seq(1, N)
      xais <- qnorm((n - (.5*N) /N)/ N)
      
      simu <- sort(rnorm(N))
      for(i in 1:98){
        simu <- cbind(simu,sort(rnorm(N)))
      }
      y.max <- apply(simu,FUN=max,MARGIN=1)
      y.min <- apply(simu,FUN=min,MARGIN=1)
      
      y <- sort(y)
      eq <- seq(min(xais),max(xais),(max(xais)-min(xais))/1000)
      
      plot_ly() %>%
        add_lines(x=~xais, y=y, name="rResidue") %>%
        add_lines(x=~eq, y=eq, name="Normal") %>%
        add_ribbons(x=~xais, ymax=y.max, ymin=y.min,name="CI") %>%
        layout(xaxis = list(title = "t Quantiles"),
               yaxis = list(title = "Studentized Residuals(fit)"))
    }
    error.residtb <- function(dat, y, var1, var2=NA, order = 2){
      fit <- fit.expr(dat, y, var1, var2, order)
      residue <- fit$residuals
      rstudent <- rstudent(fit)
      data.frame(id = names(residue), residue = residue, rstudent = rstudent)
    }
    
    output$significance_residue_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "significance_residue_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$significance_residue_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$significance_residue_pick1_explantory]
      
      pickerInput(
        inputId = "significance_residue_pick2_explantory",
        label = "Select variable 2 (Or Not)", 
        choices = c("Not Use", choices)
      )
    })
    
    significance_residue_APPLY_click <- 0
    significance_residue_plot1area <- NULL
    output$significance_residue_plot1 <- renderPlotly({
      
      if(input$significance_residue_APPLY > significance_residue_APPLY_click){
        
        significance_residue_APPLY_click  <<- significance_residue_APPLY_click + 1
        
        if(input$significance_residue_pick2_explantory == "Not Use"){
          if(input$significance_residue_pick3_method == "Distribution"){
            res <- error.residplot(dat = read.d1(),
                                   y = input$data_pick1_response,
                                   var1 = input$significance_residue_pick1_explantory,
                                   order = input$significance_residue_checkbox1_order)
          } else if(input$significance_residue_pick3_method == "Q-Q Plot"){
            res <- error.residqq(dat = read.d1(),
                                 y = input$data_pick1_response,
                                 var1 = input$significance_residue_pick1_explantory,
                                 order = input$significance_residue_checkbox1_order)
          } else{
            res <- NULL
          }
        } else{
          if(input$significance_residue_pick3_method == "Distribution"){
            res <- error.residplot(dat = read.d1(),
                                   y = input$data_pick1_response,
                                   var1 = input$significance_residue_pick1_explantory,
                                   var2 = input$significance_residue_pick2_explantory,
                                   order = input$significance_residue_checkbox1_order)
          } else if(input$significance_residue_pick3_method == "Q-Q Plot"){
            res <- error.residqq(dat = read.d1(),
                                 y = input$data_pick1_response,
                                 var1 = input$significance_residue_pick1_explantory,
                                 var2 = input$significance_residue_pick2_explantory,
                                 order = input$significance_residue_checkbox1_order)
          } else{
            res <- NULL
          }
        }
        
        significance_residue_plot1area <<- res
      }
      
      significance_residue_plot1area
    })
    
    significance_residue_APPLY_click2 <- 0
    significance_residue_tb1area <- NULL
    output$significance_residue_tb1 <- renderTable({
      res <- NULL
      if(input$significance_residue_APPLY > significance_residue_APPLY_click2){
        
        significance_residue_APPLY_click2  <<- significance_residue_APPLY_click2 + 1
        
        if(input$significance_residue_pick2_explantory == "Not Use"){
          if(input$significance_residue_pick3_method == "Table"){
            res <- error.residtb(dat = read.d1(),
                                 y = input$data_pick1_response,
                                 var1 = input$significance_residue_pick1_explantory,
                                 order = input$significance_residue_checkbox1_order)
          } else{
            res <- NULL
          }
        } else{
          if(input$significance_residue_pick3_method == "Table"){
            res <- error.residtb(dat = read.d1(),
                                 y = input$data_pick1_response,
                                 var1 = input$significance_residue_pick1_explantory,
                                 var2 = input$significance_residue_pick2_explantory,
                                 order = input$significance_residue_checkbox1_order)
          } else{
            res <- NULL
          }
        }
        significance_residue_tb1area <<- res
      }
      
      significance_residue_tb1area
    })
    
    #Significance.Jackknife
    shrinkage <- function(fit, k=10){
      #Ref: R in Action  Robert I. Kabacoff
      
      theta.fit <- function(x,y){lsfit(x,y)}
      theta.predict <- function(fit, x){cbind(1,x)%*%fit$coef}
      
      if(ncol(fit$model)<2){
        stop("No variable is included in the model after stepwise regression. 
           So Jackknife test can not be performed.")
      }
      
      x <- fit$model[,2:ncol(fit$model)]
      y <- fit$model[,1]
      
      results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
      r2 <- cor(y, fit$fitted.value)^2
      r2cv <- cor(y, results$cv.fit)^2
      cout <- paste("Original R-square = ",r2,"<br />",sep='')
      cout <- paste(cout, k, "Fold Cross-Validated R-square =",r2cv,"<br />",sep='')
      paste(cout, "Change =",r2-r2cv,sep='')
    }
    signif.jackknif <- function(dat, y, var1, var2=NA, order = 2, k = 10){
      shrinkage(fit.expr(dat, y, var1, var2, order),k=k)
    }
    output$significance_jackknife_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "significance_jackknife_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$significance_jackknife_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$significance_jackknife_pick1_explantory]
      
      pickerInput(
        inputId = "significance_jackknife_pick2_explantory",
        label = "Select variable 2 (Or Not)", 
        choices = c("Not Use", choices)
      )
    })
    
    significance_jackknife_APPLY_click <- 0
    significance_jackknife_txt1area <- NULL
    output$significance_jackknife_txt1 <- renderUI({
      
      if(input$significance_jackknife_APPLY > significance_jackknife_APPLY_click){
        
        significance_jackknife_APPLY_click  <<- significance_jackknife_APPLY_click + 1
        
        if(input$significance_jackknife_pick2_explantory == "Not Use"){
          res <- signif.jackknif(dat = read.d1(),
                                 y = input$data_pick1_response,
                                 var1 = input$significance_jackknife_pick1_explantory,
                                 order = input$significance_jackknife_checkbox1_order,
                                 k = as.numeric(input$significance_jackknife_alide1_k))
        } else{
          res <- signif.jackknif(dat = read.d1(),
                                 y = input$data_pick1_response,
                                 var1 = input$significance_jackknife_pick1_explantory,
                                 var2 = input$significance_jackknife_pick2_explantory,
                                 order = input$significance_jackknife_checkbox1_order,
                                 k = as.numeric(input$significance_jackknife_alide1_k))
        }
        
        significance_jackknife_txt1area <<- HTML(res)
      }
      
      significance_jackknife_txt1area
    })
    
    #Explantory.ANOVA
    explantory.ANOVA <- function(dat, y, var1, var2=NA, order=2){
      res <- anova(fit.expr(dat,y,var1,var2,order))
      fval <- c(res$`F value`[1:(length(res$`F value`)-1)],"/")
      pval <- c(res$`Pr(>F)`[1:(length(res$`Pr(>F)`)-1)],"/")
      SS <- sum(res$`Sum Sq`)
      data.frame(Item=row.names(res),
                 Df=res$Df,
                 `Sum Sq`=res$`Sum Sq`,
                 `Mean Sq`=res$`Mean Sq`,
                 `F value`=fval,
                 `P value`=pval,
                 `Explained`=paste(round((res$`Sum Sq`/SS)*100,3),"%",sep=''))
    }
    
    output$explantory_ANOVA_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "explantory_ANOVA_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$explantory_ANOVA_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$explantory_ANOVA_pick1_explantory]
      
      pickerInput(
        inputId = "explantory_ANOVA_pick2_explantory",
        label = "Select variable 2 (Or Not)", 
        choices = c("Not Use", choices)
      )
    })
    
    explantory_ANOVA_APPLY_click <- 0
    explantory_ANOVA_tb1area <- NULL
    output$explantory_ANOVA_table1 <- renderTable({
      
      if(input$explantory_ANOVA_APPLY > explantory_ANOVA_APPLY_click){
        
        explantory_ANOVA_APPLY_click  <<- explantory_ANOVA_APPLY_click + 1
        
        if(input$explantory_ANOVA_pick2_explantory == "Not Use"){
          res <- explantory.ANOVA(dat = read.d1(),
                                  y = input$data_pick1_response,
                                  var1 = input$explantory_ANOVA_pick1_explantory,
                                  order = input$explantory_ANOVA_checkbox1_order)
        } else{
          res <- explantory.ANOVA(dat = read.d1(),
                                  y = input$data_pick1_response,
                                  var1 = input$explantory_ANOVA_pick1_explantory,
                                  var2 = input$explantory_ANOVA_pick2_explantory,
                                  order = input$explantory_ANOVA_checkbox1_order)
        }
        
        explantory_ANOVA_tb1area <<- res
      }
      
      explantory_ANOVA_tb1area
    })
    
    #Explantory.Muti Comp
    explantory.muticomp <- function(dat,y,var1,var2=NA,method,mutivar,order,mutialpha=0.05){
      
      fit <- fit.expr(dat,y,var1,var2,order)
      res <- NULL
      if(!any(grepl("*var1.d*",names(fit$coefficients)))){
        stop("After regression, 'var1' is excluded. Can not make mutiple comparision for 'var1'.")
      }
      
      if(method=="LSD"){
        res <- LSD.test(fit, "var1.d", console=F, group=T, alpha = mutialpha)
      } else if(method=="Tukey"){
        res <- HSD.test(fit, "var1.d", console=F, group=T, alpha = mutialpha)
      } else if(method=="Scheffe"){
        res <- scheffe.test(fit, "var1.d",console=F, group=T, alpha = mutialpha)
      } else {
        res <- LSD.test(fit, "var1.d", console=F, group=T, p.adj="bonferroni")
      }
      
      cbind(level=row.names(res$groups),res$groups)
    }
    
    output$explantory_muticomp_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "explantory_muticomp_pick1_explantory",
        label = "Select variable 1 (Also for mutiple comparision)", 
        choices = input$data_pick2_explantory
      )
    })
    output$explantory_muticomp_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$explantory_muticomp_pick1_explantory]
      
      pickerInput(
        inputId = "explantory_muticomp_pick2_explantory",
        label = "Select variable 2 (Or Not)", 
        choices = c("Not Use", choices)
      )
    })
    
    explantory_muticomp_APPLY_click <- 0
    explantory_muticomp_tb1area <- NULL
    output$explantory_muticomp_table1 <- renderTable({
      
      if(input$explantory_muticomp_APPLY > explantory_muticomp_APPLY_click){
        
        explantory_muticomp_APPLY_click  <<- explantory_muticomp_APPLY_click + 1
        
        if(input$explantory_muticomp_pick2_explantory == "Not Use"){
          res <- explantory.muticomp(dat = read.d1(),
                                     y = input$data_pick1_response,
                                     var1 = input$explantory_muticomp_pick1_explantory,
                                     method = input$explantory_muticomp_pick3_method,
                                     order = input$explantory_muticomp_checkbox1_order)
        } else{
          res <- explantory.muticomp(dat = read.d1(),
                                     y = input$data_pick1_response,
                                     var1 = input$explantory_muticomp_pick1_explantory,
                                     var2 = input$explantory_muticomp_pick2_explantory,
                                     method = input$explantory_muticomp_pick3_method,
                                     order = input$explantory_muticomp_checkbox1_order)
        }
        
        explantory_muticomp_tb1area <<- res
      }
      
      explantory_muticomp_tb1area
    })
    
    #Prediction.Trend
    prediction.trend <- function(dat,y,var1,var2,order){
      model <- fit.expr(dat,y,var1,var2,order)
      as.grid <- function(x_coord, y_coord, nrow, ncol){
        xstep <- (max(x_coord)-min(x_coord))/(nrow-1)
        ystep <- (max(y_coord)-min(y_coord))/(ncol-1)
        x <- seq(min(x_coord), max(x_coord), xstep)
        y <- seq(min(y_coord), max(y_coord), ystep)
        
        y <- rev(y) # consist with RasterLayer
        y.grid <- c()
        for(i in 1:ncol){ y.grid <- c(y.grid,rep(y[i],nrow)) }
        x.grid <- rep(x,ncol)
        return(data.frame(x=x.grid,y=y.grid))
      }
      
      var1.grid <- as.numeric(names(table(dat[[var1]])))
      var2.grid <- as.numeric(names(table(dat[[var2]])))
      if(any(is.na(var1.grid))){
        stop("'var1' is not a continuous variable. Can not make trend prediction.")
      }
      if(any(is.na(var2.grid))){
        stop("'var2' is not a continuous variable. Can not make trend prediction.")
      }
      
      to.plot <- as.grid(var1.grid,var1.grid,100,100)
      colnames(to.plot) <- c("var1.d","var2.d")
      to.plot <- cbind(to.plot, predict(model, newdata = to.plot))
      names(to.plot) <- c("var1","var2","Predicted")
      
      fig <- plot_ly(to.plot, x = ~var1, y = ~var2, z = ~Predicted,color = ~Predicted, colors = c('#0100AC', '#DCE121', '#830003' ))
      fig <- fig %>% add_markers()
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'var1'),
                                         yaxis = list(title = 'var2'),
                                         zaxis = list(title = 'Predicted')))
      
      fig
    }
    output$prediction_trend_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "prediction_trend_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$prediction_trend_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$prediction_trend_pick1_explantory]
      
      pickerInput(
        inputId = "prediction_trend_pick2_explantory",
        label = "Select variable 2", 
        choices = choices
      )
    })
    
    prediction_trend_APPLY_click <- 0
    prediction_trend_plot1area <- NULL
    output$prediction_trend_plot1 <- renderPlotly({
      if(input$prediction_trend_APPLY > prediction_trend_APPLY_click){
        prediction_trend_APPLY_click  <<- prediction_trend_APPLY_click + 1
        res <- prediction.trend(dat = read.d1(),
                                y = input$data_pick1_response,
                                var1 = input$prediction_trend_pick1_explantory,
                                var2 = input$prediction_trend_pick2_explantory,
                                order = input$prediction_trend_checkbox1_order)
        prediction_trend_plot1area <<- res
      }
      
      prediction_trend_plot1area
    })
    
    #Prediction.Spline
    prediction.spline <- function(dat,y,var1,var2){
      geo.interpolate <- function(dat, var, method = c("Spline", "Inverse"), 
                                  Xextend = "defaulted", Yextend = "defaulted"){
        
        if(class(dat) != "data.frame"){
          stop("`dat` should be a 'data.frame'")
        }
        
        Xmax <- max(dat$X)
        Ymax <- max(dat$Y)
        Xmin <- min(dat$X)
        Ymin <- min(dat$Y)
        if(Xextend == "defaulted"){
          Xextend = (Xmax - Xmin) * 0.1
        }
        if(Yextend == "defaulted"){
          Yextend = (Ymax - Ymin) * 0.1
        }
        
        pol <- Polygon(data.frame(c(Xmin - Xextend,Xmin - Xextend,Xmax + Xextend,Xmax + Xextend,Xmin - Xextend),
                                  c(Ymin - Yextend,Ymax + Yextend,Ymax + Yextend,Ymin - Yextend,Ymin - Yextend)))
        Sr <- SpatialPolygons(list(Polygons(list(pol),1)))
        bound <- SpatialPolygonsDataFrame(Sr, data = data.frame(Id = 0))
        
        Co_inf <- SpatialPoints(dat[,1:2]) 
        Co_inf <- SpatialPointsDataFrame(Co_inf,dat)
        blank_raster <- raster(nrow=100,ncol=100,extent(bound)) 
        values(blank_raster) <- 1 
        bound_raster<-rasterize(bound,blank_raster)
        
        if(method == "Spline"){
          m <- Tps(coordinates(Co_inf), var)
        } else if(method == "Inverse"){
          m <- gstat(formula=var~1, locations=Co_inf, set=list(idp = 0.5)) 
        } 
        res <- interpolate(bound_raster, m)
        return(res)
      }
      as.data.frame.RasterLayer <- function(ras){
        xstep <- (xmax(ras) - xmin(ras))/(nrow(ras)-1)
        ystep <- (ymax(ras) - ymin(ras))/(ncol(ras)-1)
        x <- seq(xmin(ras), xmax(ras), xstep)
        y <- seq(ymin(ras), ymax(ras), ystep)
        
        y <- rev(y) # Transformation
        y.grid <- c()
        for(i in 1:nrow(ras)){ y.grid <- c(y.grid,rep(y[i],nrow(ras))) }
        x.grid <- rep(x,ncol(ras))
        return(data.frame(x=x.grid,y=y.grid,value=values(ras)))
      }
      input_inte <- data.frame(X=dat[[var1]], Y=dat[[var2]])
      ras <- geo.interpolate(input_inte,dat[[y]])
      to.plot <- as.data.frame.RasterLayer(ras)
      names(to.plot) <- c("var1","var2","Predicted")
      
      fig <- plot_ly(to.plot, x = ~var1, y = ~var2, z = ~Predicted,color = ~Predicted, colors = c('#0100AC', '#DCE121', '#830003' ))
      fig <- fig %>% add_markers()
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'var1'),
                                         yaxis = list(title = 'var2'),
                                         zaxis = list(title = 'Predicted')))
      
      fig
    }
    output$prediction_spline_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "prediction_spline_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$prediction_spline_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$prediction_spline_pick1_explantory]
      
      pickerInput(
        inputId = "prediction_spline_pick2_explantory",
        label = "Select variable 2", 
        choices = choices
      )
    })
    
    prediction_spline_APPLY_click <- 0
    prediction_spline_plot1area <- NULL
    output$prediction_spline_plot1 <- renderPlotly({
      if(input$prediction_spline_APPLY > prediction_spline_APPLY_click){
        prediction_spline_APPLY_click  <<- prediction_spline_APPLY_click + 1
        res <- prediction.spline(dat = read.d1(),
                                 y = input$data_pick1_response,
                                 var1 = input$prediction_spline_pick1_explantory,
                                 var2 = input$prediction_spline_pick2_explantory)
        prediction_spline_plot1area <<- res
      }
      
      prediction_spline_plot1area
    })
    
    #Prediction.Inverse
    prediction.inverse <- function(dat,y,var1,var2,idp){
      geo.interpolate <- function(dat, var, method = c("Spline", "Inverse"), 
                                  Xextend = "defaulted", Yextend = "defaulted", idp){
        Xmax <- max(dat$X)
        Ymax <- max(dat$Y)
        Xmin <- min(dat$X)
        Ymin <- min(dat$Y)
        if(Xextend == "defaulted"){
          Xextend = (Xmax - Xmin) * 0.1
        }
        if(Yextend == "defaulted"){
          Yextend = (Ymax - Ymin) * 0.1
        }
        
        pol <- Polygon(data.frame(c(Xmin - Xextend,Xmin - Xextend,Xmax + Xextend,Xmax + Xextend,Xmin - Xextend),
                                  c(Ymin - Yextend,Ymax + Yextend,Ymax + Yextend,Ymin - Yextend,Ymin - Yextend)))
        Sr <- SpatialPolygons(list(Polygons(list(pol),1)))
        bound <- SpatialPolygonsDataFrame(Sr, data = data.frame(Id = 0))
        
        Co_inf <- SpatialPoints(dat[,1:2]) 
        Co_inf <- SpatialPointsDataFrame(Co_inf,dat)
        blank_raster <- raster(nrow=100,ncol=100,extent(bound)) 
        values(blank_raster) <- 1 
        bound_raster<-rasterize(bound,blank_raster)
        
        if(method == "Spline"){
          m <- Tps(coordinates(Co_inf), var)
        } else if(method == "Inverse"){
          print(idp)
          m <- gstat(formula=var~1, locations=Co_inf, set=list(idp = idp)) 
        } 
        res <- interpolate(bound_raster, m)
        return(res)
      }
      as.data.frame.RasterLayer <- function(ras){
        xstep <- (xmax(ras) - xmin(ras))/(nrow(ras)-1)
        ystep <- (ymax(ras) - ymin(ras))/(ncol(ras)-1)
        x <- seq(xmin(ras), xmax(ras), xstep)
        y <- seq(ymin(ras), ymax(ras), ystep)
        
        y <- rev(y) # Transformation
        y.grid <- c()
        for(i in 1:nrow(ras)){ y.grid <- c(y.grid,rep(y[i],nrow(ras))) }
        x.grid <- rep(x,ncol(ras))
        return(data.frame(x=x.grid,y=y.grid,value=values(ras)))
      }
      input_inte <- data.frame(X=as.numeric(scale(dat[[var1]])), Y=as.numeric(scale(dat[[var2]])))
      ras <- geo.interpolate(input_inte,dat[[y]],method = "Inverse",idp = idp)
      to.plot <- as.data.frame.RasterLayer(ras)
      names(to.plot) <- c("var1","var2","Predicted")
      
      fig <- plot_ly(to.plot, x = ~var1, y = ~var2, z = ~Predicted,color = ~Predicted, colors = c('#0100AC', '#DCE121', '#830003' ))
      fig <- fig %>% add_markers()
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'Scale(var1)'),
                                         yaxis = list(title = 'Scale(var2)'),
                                         zaxis = list(title = 'Predicted')))
      
      fig
    }
    output$prediction_inverse_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "prediction_inverse_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$prediction_inverse_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$prediction_inverse_pick1_explantory]
      
      pickerInput(
        inputId = "prediction_inverse_pick2_explantory",
        label = "Select variable 2", 
        choices = choices
      )
    })
    
    prediction_inverse_APPLY_click <- 0
    prediction_inverse_plot1area <- NULL
    output$prediction_inverse_plot1 <- renderPlotly({
      if(input$prediction_inverse_APPLY > prediction_inverse_APPLY_click){
        prediction_inverse_APPLY_click  <<- prediction_inverse_APPLY_click + 1
        res <- prediction.inverse(dat = read.d1(),
                                  y = input$data_pick1_response,
                                  var1 = input$prediction_inverse_pick1_explantory,
                                  var2 = input$prediction_inverse_pick2_explantory,
                                  idp = as.numeric(input$prediction_inverse_slider1_idp))
        prediction_inverse_plot1area <<- res
      }
      
      prediction_inverse_plot1area
    })
    
    #Prediction.Simp Mov
    prediction.simmov <- function(dat,y,var1,var2,R){
      input_inte <- data.frame(X=dat[[var1]], Y=dat[[var2]])
      Xs <- as.vector(scale(dat[[var1]]))
      Ys <- as.vector(scale(dat[[var2]]))
      as.grid <- function(x_coord, y_coord, nrow, ncol){
        xstep <- (max(x_coord)-min(x_coord))/(nrow-1)
        ystep <- (max(y_coord)-min(y_coord))/(ncol-1)
        x <- seq(min(x_coord), max(x_coord), xstep)
        y <- seq(min(y_coord), max(y_coord), ystep)
        
        y <- rev(y) # consist with RasterLayer
        y.grid <- c()
        for(i in 1:ncol){ y.grid <- c(y.grid,rep(y[i],nrow)) }
        x.grid <- rep(x,ncol)
        return(data.frame(x=x.grid,y=y.grid))
      }
      to.plot <- as.grid(Xs,Ys,100,100)
      sam <- data.frame(x=Xs,y=Ys,value=dat[[y]]) # modi
      
      # simple moving average
      sim.inte <- function(sam, x, y, R){
        filter <- (sam$x - x)^2 + (sam$y - y)^2 < R^2
        return(mean(sam[filter,]$value))
      }
      
      value <- c()
      for(i in 1:nrow(to.plot)){
        value <- c(value, sim.inte(sam, to.plot[i,]$x, to.plot[i,]$y, R))
      }
      
      to.plot <- cbind(to.plot,value)
      colnames(to.plot) <- c('var1','var2','Predicted')
      
      fig <- plot_ly(to.plot, x = ~var1, y = ~var2, z = ~Predicted,color = ~Predicted, colors = c('#0100AC', '#DCE121', '#830003' ))
      fig <- fig %>% add_markers()
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'Scale(var1)'),
                                         yaxis = list(title = 'Scale(var2)'),
                                         zaxis = list(title = 'Predicted')))
      
      fig
    }
    output$prediction_simmov_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "prediction_simmov_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$prediction_simmov_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$prediction_simmov_pick1_explantory]
      
      pickerInput(
        inputId = "prediction_simmov_pick2_explantory",
        label = "Select variable 2", 
        choices = choices
      )
    })
    output$prediction_simmov_slider1d_R <- renderUI({
      dat <- read.d1()
      var1 <- input$prediction_simmov_pick1_explantory
      var2 <- input$prediction_simmov_pick2_explantory
      x1 <- scale(dat[[var1]])
      x2 <- scale(dat[[var2]])
      Rmax <- sqrt((max(x1) - min(x1))^2 + (max(x2) - min(x2))^2)
      Rmin <- Rmax/20
      
      sliderTextInput(
        inputId = "prediction_simmov_slider1_R",
        label = "Radius of window R:", 
        choices = round(seq(Rmin,Rmax,Rmax/20),2),
        grid = TRUE
      )
    })
    
    prediction_simmov_APPLY_click <- 0
    prediction_simmov_plot1area <- NULL
    output$prediction_simmov_plot1 <- renderPlotly({
      if(input$prediction_simmov_APPLY > prediction_simmov_APPLY_click){
        prediction_simmov_APPLY_click  <<- prediction_simmov_APPLY_click + 1
        res <- prediction.simmov(dat = read.d1(),
                                 y = input$data_pick1_response,
                                 var1 = input$prediction_simmov_pick1_explantory,
                                 var2 = input$prediction_simmov_pick2_explantory,
                                 R = as.numeric(input$prediction_simmov_slider1_R))
        prediction_simmov_plot1area <<- res
      }
      
      prediction_simmov_plot1area
    })
    
    #Prediction.Krigging
    prediction.krigging <- function(dat,y,var1,var2){
      krige.interpolate <- function(dat, var, psill=NA, model="Sph", nugget=NA, nmax = Inf, nmin = 0,
                                    Xextend = "defaulted", Yextend = "defaulted"){
        
        if(class(dat) != "data.frame"){
          stop("`dat` should be a 'data.frame'")
        }
        
        Xmax <- max(dat$X)
        Ymax <- max(dat$Y)
        Xmin <- min(dat$X)
        Ymin <- min(dat$Y)
        if(Xextend == "defaulted"){
          Xextend = (Xmax - Xmin) * 0.1
        }
        if(Yextend == "defaulted"){
          Yextend = (Ymax - Ymin) * 0.1
        }
        
        pol <- Polygon(data.frame(c(Xmin - Xextend,Xmin - Xextend,Xmax + Xextend,Xmax + Xextend,Xmin - Xextend),
                                  c(Ymin - Yextend,Ymax + Yextend,Ymax + Yextend,Ymin - Yextend,Ymin - Yextend)))
        Sr <- SpatialPolygons(list(Polygons(list(pol),1)))
        bound <- SpatialPolygonsDataFrame(Sr, data = data.frame(Id = 0))
        
        Co_inf <- SpatialPoints(dat[,1:2]) 
        Co_inf <- SpatialPointsDataFrame(Co_inf,dat)
        blank_raster <- raster(nrow=100,ncol=100,extent(bound)) 
        values(blank_raster) <- 1 
        bound_raster<-rasterize(bound,blank_raster)
        
        if(any(var<0)){
          stop("negative values found in `var`")
        }
        v <- variogram(log(var) ~ 1, data = Co_inf) 
        v.fit<-fit.variogram(v,model=vgm(psill, model, nugget))
        
        Grid <- as(bound_raster,"SpatialGridDataFrame") 
        res <- krige(formula= var~1 ,model=v.fit,locations=Co_inf,newdata=Grid, nmax=nmax, nmin=nmin)   
        
        return(list(variogram = v.fit, krigging = res))
      }
      dat.ave <- aggregate(dat[[y]],by=list(var1=dat[[var1]],var2=dat[[var2]]),mean)
      Xs <- as.numeric(scale(dat.ave[,1]))
      Ys <- as.numeric(scale(dat.ave[,2]))
      res <- krige.interpolate(data.frame(X = Xs, Y = Ys),dat.ave[,3]) #modi
      grid <- SpatialPoints(res$krigging)@coords
      value <- res[["krigging"]]@data$var1.pred
      to.plot <- as.data.frame(cbind(grid,value))
      names(to.plot) <- c("var1","var2","Predicted")
      
      fig <- plot_ly(to.plot, x = ~var1, y = ~var2, z = ~Predicted,color = ~Predicted, colors = c('#0100AC', '#DCE121', '#830003' ))
      fig <- fig %>% add_markers()
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'Scale(var1)'),
                                         yaxis = list(title = 'Scale(var2)'),
                                         zaxis = list(title = 'Predicted')))
      
      fig
    }
    output$prediction_krigging_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "prediction_krigging_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$prediction_krigging_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$prediction_krigging_pick1_explantory]
      
      pickerInput(
        inputId = "prediction_krigging_pick2_explantory",
        label = "Select variable 2", 
        choices = choices
      )
    })
    prediction_krigging_APPLY_click <- 0
    prediction_krigging_plot1area <- NULL
    output$prediction_krigging_plot1 <- renderPlotly({
      if(input$prediction_krigging_APPLY > prediction_krigging_APPLY_click){
        prediction_krigging_APPLY_click  <<- prediction_krigging_APPLY_click + 1
        res <- prediction.krigging(dat = read.d1(),
                                   y = input$data_pick1_response,
                                   var1 = input$prediction_krigging_pick1_explantory,
                                   var2 = input$prediction_krigging_pick2_explantory)
        prediction_krigging_plot1area <<- res
      }
      
      prediction_krigging_plot1area
    })
    
    #Uncertainty.TNE
    uncertainty.TNE <- function(dat,y,var1,var2=NA){
      sem <- function(v){sd(v)/length(v)}
      if(!is.na(var2)){
        base <- dat[(dat[[var1]]=="0" | dat[[var1]] == "CK") & 
                      (dat[[var2]]=="0" | dat[[var2]] == "CK"),][[y]]
        if(length(base)==1){stop("Not enough observations")}
        base_sem <- sem(base)
        sign_sem <- aggregate(dat[[y]],by=list(var1=dat[[var1]],var2=dat[[var2]]),sem)
        sign_sem[,3] <- sign_sem[,3]/base_sem
        colnames(sign_sem)[3] <- "TNE"
        plot_ly(data = sign_sem, x=~var1, y=~var2, z=~TNE) %>% add_markers()
      } else{
        base <- dat[(dat[[var1]]=="0" | dat[[var1]] == "CK"),][[y]]
        if(length(base)==1){stop("Not enough observations")}
        base_sem <- sem(base)
        sign_sem <- aggregate(dat[[y]],by=list(var1=dat[[var1]]),sem)
        sign_sem[,2] <- sign_sem[,2]/base_sem
        colnames(sign_sem)[2] <- "TNE"
        plot_ly(data = sign_sem, x=~var1, y=~TNE) %>% add_markers()
      }
    }
    output$uncertainty_TNE_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "uncertainty_TNE_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$uncertainty_TNE_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$uncertainty_TNE_pick1_explantory]
      
      pickerInput(
        inputId = "uncertainty_TNE_pick2_explantory",
        label = "Select variable 2 (Or Not)", 
        choices = c("Not Use", choices)
      )
    })
    uncertainty_TNE_APPLY_click <- 0
    uncertainty_TNE_plot1area <- NULL
    output$uncertainty_TNE_plot1 <- renderPlotly({
      if(input$uncertainty_TNE_APPLY > uncertainty_TNE_APPLY_click){
        uncertainty_TNE_APPLY_click  <<- uncertainty_TNE_APPLY_click + 1
        if(input$uncertainty_TNE_pick2_explantory!="Not Use"){
          res <- uncertainty.TNE(dat = read.d1(),
                                 y = input$data_pick1_response,
                                 var1 = input$uncertainty_TNE_pick1_explantory,
                                 var2 = input$uncertainty_TNE_pick2_explantory)
        } else{
          res <- uncertainty.TNE(dat = read.d1(),
                                 y = input$data_pick1_response,
                                 var1 = input$uncertainty_TNE_pick1_explantory)
        }
        uncertainty_TNE_plot1area <<- res
      }
      uncertainty_TNE_plot1area
    })
    
    #Uncertainty.T/E Simu
    uncertainty.TNEsimu <- function(dat,y,var1,var2=NA,nsim=99,order=2){
      if(!is.na(var2)){
        sdt <- aggregate(dat[[y]],by=list(var1=dat[[var1]],var2=dat[[var2]]),sd)
        meant <- aggregate(dat[[y]],by=list(var1=dat[[var1]],var2=dat[[var2]]),mean)
        nt <- aggregate(dat[[y]],by=list(var1=dat[[var1]],var2=dat[[var2]]),length)
        simures <- data.frame(x=1:nsim,
                              `var1_Explained`=rep(0,nsim),
                              `var2_Explained`=rep(0,nsim))
        for(i in 1:nsim){
          res <- data.frame(var1=c(),var2=c(),value=c())
          for(j in 1:nrow(nt)){
            res <- rbind(res,data.frame(var1=rep(nt[j,1],nt[j,3]),
                                        var2=rep(nt[j,2],nt[j,3]),
                                        value=rnorm(nt[j,3],mean=meant[j,3],sd=sdt[j,3])))
          }
          fit <- fit.expr(res,'value','var1','var2',order)
          a <- anova(fit)
          SS <- sum(a$`Sum Sq`)
          exp1 <- sum(a$`Sum Sq`[grepl("*var1.d*",row.names(a))])/SS*100
          exp2 <- sum(a$`Sum Sq`[grepl("*var2.d*",row.names(a))])/SS*100
          simures[i,2] <- exp1
          simures[i,3] <- exp2
        }
        plot_ly(data = simures) %>%
          add_lines(x=~x,y=~`var1_Explained`,type='scatter',mode='lines',name='var1 Explained%') %>%
          add_lines(x=~x,y=~`var2_Explained`,type='scatter',mode='lines',name='var2 Explained%') %>%
          layout(xaxis = list(title = "No. of Simulation"),
                 yaxis = list(title = "Variance Explained%"))
      } else{
        sdt <- aggregate(dat[[y]],by=list(var1=dat[[var1]]),sd)
        meant <- aggregate(dat[[y]],by=list(var1=dat[[var1]]),mean)
        nt <- aggregate(dat[[y]],by=list(var1=dat[[var1]]),length)
        simures <- data.frame(x=1:nsim,
                              `var1_Explained`=rep(0,nsim))
        for(i in 1:nsim){
          res <- data.frame(var1=c(),var2=c(),value=c())
          for(j in 1:nrow(nt)){
            res <- rbind(res,data.frame(var1=rep(nt[j,1],nt[j,2]),
                                        value=rnorm(nt[j,2],mean=meant[j,2],sd=sdt[j,2])))
          }
          fit <- fit.expr(res,'value','var1',order=order)
          a <- anova(fit)
          SS <- sum(a$`Sum Sq`)
          exp1 <- sum(a$`Sum Sq`[grepl("*var1.d*",row.names(a))])/SS*100
          simures[i,2] <- exp1
        }
        plot_ly(data = simures) %>%
          add_lines(x=~x,y=~`var1_Explained`,type='scatter',mode='lines') %>%
          layout(xaxis = list(title = "No. of Simulation"),
                 yaxis = list(title = "Variance Explained%"))
      }
    }
    output$uncertainty_TNEsimu_pick1d_explantory <- renderUI({
      pickerInput(
        inputId = "uncertainty_TNEsimu_pick1_explantory",
        label = "Select variable 1", 
        choices = input$data_pick2_explantory
      )
    })
    output$uncertainty_TNEsimu_pick2d_explantory <- renderUI({
      choices <- input$data_pick2_explantory
      choices <- choices[choices != input$uncertainty_TNEsimu_pick1_explantory]
      
      pickerInput(
        inputId = "uncertainty_TNEsimu_pick2_explantory",
        label = "Select variable 2 (Or Not)", 
        choices = c("Not Use", choices)
      )
    })
    uncertainty_TNEsimu_APPLY_click <- 0
    uncertainty_TNEsimu_plot1area <- NULL
    output$uncertainty_TNEsimu_plot1 <- renderPlotly({
      if(input$uncertainty_TNEsimu_APPLY > uncertainty_TNEsimu_APPLY_click){
        uncertainty_TNEsimu_APPLY_click  <<- uncertainty_TNEsimu_APPLY_click + 1
        if(input$uncertainty_TNEsimu_pick2_explantory!="Not Use"){
          res <- uncertainty.TNEsimu(dat = read.d1(),
                                     y = input$data_pick1_response,
                                     var1 = input$uncertainty_TNEsimu_pick1_explantory,
                                     var2 = input$uncertainty_TNEsimu_pick2_explantory,
                                     nsim = input$uncertainty_TNEsimu_slider1_nsim)
        } else{
          res <- uncertainty.TNEsimu(dat = read.d1(),
                                     y = input$data_pick1_response,
                                     var1 = input$uncertainty_TNEsimu_pick1_explantory,
                                     nsim = input$uncertainty_TNEsimu_slider1_nsim)
        }
        uncertainty_TNEsimu_plot1area <<- res
      }
      uncertainty_TNEsimu_plot1area
    })
    
    
  }
  ui <- expmeasure.ui()
  server <- server.home
  shinyApp(ui, server, options = list(launch.browser = T))
}