
library(shiny)
library(htmltools)
library(DT)
library(dplyr)
library(rsconnect)
source("table_chart.r")
#runApp(host="192.168.3.87", port=5033)
shinyServer <- function(input, output,session) {
  duration.choices <- reactive({
    if (input$chart.ticksize=="15 Min"){
      return(chart.tick.duration)
    }else if (input$chart.ticksize=="Hourly"){
      return(chart.hourly.duration)
    }
    else if (input$chart.ticksize=="Daily"){
      return(chart.daily.duration)
    }
    else {return(chart.weekly.duration)}
  })
  observe({
    updateSelectInput(session, "chart.duration",choices = duration.choices())
    })
  
  duration.choices.table <- reactive({
    if (input$table.ticksize=="Hourly"){
      return(chart.hourly.duration)
    }
    else if (input$table.ticksize=="Daily"){
      return(chart.daily.duration)
    }
    else {return(chart.weekly.duration)}
  })
  observe({
    updateSelectInput(session, "table.duration",choices = duration.choices.table())
  })
  
  output$chart <- renderPlotly({
   plot.trendline(input$chart.currency, input$chart.ticksize, input$chart.duration)
  })
  
  
  output$table <- DT::renderDataTable({
  createTable(input$table.ticksize, input$table.duration)
    }, options=list(pageLength=100 ))
    
  
  
}

