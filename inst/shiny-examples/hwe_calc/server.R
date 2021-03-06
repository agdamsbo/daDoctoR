#'
#' This is a Shiny web application. You can run the application by clicking
#' the 'Run App' button above.
#'
#' Find out more about building applications with Shiny here:
#'
#'    http://shiny.rstudio.com/
#'
#'
#' User requests:
#'
#' - Proper labelling of chart

library(shiny)
library(ggplot2)
source("https://raw.githubusercontent.com/agdamsbo/daDoctoR/master/R/hwe_geno.R")

# source("https://raw.githubusercontent.com/agdamsbo/daDoctoR/master/inst/shiny-examples/hwe_calc/ui.R")
# source(list.files(system.file("shiny-examples", "hwe_calc", package = "daDoctoR"), pattern="ui.R", full.names=TRUE))

# Define server logic required to draw a histogram

server <- function(input, output, session) {

  cale <- reactive({
    as.numeric(input$ale)
  })

  dat<-reactive({
    df<-data.frame(lbls=c("MM","MN","NN","MO","NO","OO"),
                   value=rbind(input$mm,input$mn,input$nn,input$mo,input$no,input$oo),
                   stringsAsFactors = FALSE)
    print(df)
    df
  })

  cmm <- reactive({
    as.numeric(input$mm)
  })

  cmn <- reactive({
    as.numeric(input$mn)
  })

  cnn <- reactive({
    as.numeric(input$nn)
  })

  cmo <- reactive({
    as.numeric(input$mo)
  })

  cno <- reactive({
    as.numeric(input$no)
  })

  coo <- reactive({
    as.numeric(input$oo)
  })

  hwe_p <- function() ({ hwe_geno(cmm(),cmn(),cnn(),cmo(),cno(),coo(),alleles=cale()) })

  output$allele.tbl <- renderTable({ hwe_p()$allele.dist })

  output$obs.tbl <- renderTable({ hwe_p()$observed.dist })

  output$exp.tbl <- renderTable({ hwe_p()$expected.dist })

  output$chi.val <- renderTable({ hwe_p()$chi.value })

  output$p.val <- renderTable({ hwe_p()$p.value })

  output$allele.dist <- renderText({"Allele distribution"})

  output$obs.dist <- renderText({"Observed distribution"})

  output$exp.dist <- renderText({"Expected distribution"})

  output$chi <- renderText({"Chi square value"})

  output$p <- renderText({"P value"})

  output$geno.pie.plt<- renderPlot({
    ggplot(dat(), aes(x="", y=value, fill=lbls))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="Dark2")
  })

  output$geno.pie.ttl <- renderText({"Genotype distribution"})
}

# Run the application

shinyApp(ui = ui, server = server)

