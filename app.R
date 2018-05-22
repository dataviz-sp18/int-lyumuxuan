library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)


ratings_llvf_sem <- read_csv("ratings_llvf_sem.csv")

ui <- fluidPage(
  
  #title
  titlePanel("Features and ratings plots with regression analyses"),

  plotlyOutput("ratingsPlot"),
  plotlyOutput("semPlot"),
  
  hr(),
  verbatimTextOutput("hover"),
  verbatimTextOutput("brush"),
  verbatimTextOutput("zoom"),
  
  fluidRow(
    column(10,
     selectInput("type",
          label = "Please select video type",
          choices = unique(ratings_llvf_sem$video_type),
          selected = "Flat 1"),
     br(),
    
     sliderInput('plotHeight1', 'Height of semantics plot (in pixels)', 
                min = 100, max = 427, value = 250),
     br(),
    
     sliderInput("range", "Time range for regression analysis:",
                min = 0.5, max = 240,
                value = c(0.5, 240)),
     br(),
     
     h3("Below is the regression analysis result according to time input:"),
     helpText("Note: estimate might result in NA because that feature was not present in that time interval."),
     verbatimTextOutput("modelSummary")
    )
     
     
))


server <- function(input, output, session) {

  
  output$ratingsPlot <- renderPlotly({
    
    
    df <- reactive({
      ratings_llvf_sem[(ratings_llvf_sem$video_type == input$type),]
    })
    
      
    p <- ggplot(df(), aes(Time.Interval, Avg)) +
      geom_point(size = .02) +
      geom_line() +
      geom_ribbon(aes(ymin = Avg - SD, ymax = Avg + SD, 
                      x = Time.Interval, 
                      fill = "Error band"), alpha = 0.3) +
      scale_fill_manual("", values="pink") +
      geom_line(aes(Time.Interval, tree), color = "green", linetype = "dashed") +
      labs(title = "Average ratings and amount of trees", 
           x = "Time(s)",
           y = "Average rating and tree amount") +
      theme_minimal() 
    
    ggplotly(p)%>% 
      layout(dragmode = "select")
  }
  )
  
  output$semPlot <- renderPlotly({
    
      
      df <- reactive({
      ratings_llvf_sem[(ratings_llvf_sem$video_type == input$type),]
    })
    
    cols <- c("Billboard"="grey60",
              "Lake"="lightblue2", 
              "House" = "bisque2", "Traffic sign" = "indianred1")
      
      p3 <- ggplot(df(), aes(Time.Interval, lake, fill = "Lake")) +
      geom_area() +
      geom_area(aes(Time.Interval, billboard, fill = "Billboard")) +
      geom_area(aes(Time.Interval, house, fill = "House")) +
      geom_area(aes(Time.Interval, sign, fill = "Traffic sign")) +
      scale_fill_manual(name="Features", values = cols) +
      theme_minimal() +
      labs(title = "", 
           x = "Time(s)",
           y = "Features present = 1")
        
      ggplotly(p3) %>% layout(height = input$plotHeight1, autosize=TRUE)
    
  })
      
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })

  
  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d
  })
  
  
  output$zoom <- renderPrint({
    d <- event_data("plotly_relayout")
    if (is.null(d)) "Relayout (i.e., zoom) events appear here" else d
  })
  
  
  output$modelSummary <- renderPrint({
    
    df2 <- reactive({
      ratings_llvf_sem %>% filter(video_type == input$type,
                                  ratings_llvf_sem$Time.Interval>=input$range[1],
                                  ratings_llvf_sem$Time.Interval<=input$range[2])
    })
    
    if(input$type == "Flat 1"){
      fit = lm(AvePref ~ Edge + Hue + Sat + Lum + sdHUe + sdSat + sdBright + Entropy + tree + house + billboard,
             data = df2())
      summary(fit)
    } else {
      if(input$type == "Flat 2") {
        fit = lm(AvePref ~ Edge + Hue + Sat + Lum + sdHUe + sdSat + sdBright + Entropy + tree + house + sign + billboard,
                 data = df2())
        summary(fit)
      } else {
        if(input$type == "Hill 1") {
          fit = lm(AvePref ~ Edge + Hue + Sat + Lum + sdHUe + sdSat + sdBright + Entropy + tree + lake + sign,
                   data = df2())
          summary(fit)
        } else {
          if(input$type == "Hill 2") {
            fit = lm(AvePref ~ Edge + Hue + Sat + Lum + sdHUe + sdSat + sdBright + Entropy + tree + lake,
                     data = df2())
            summary(fit)
          } else {
            if(input$type == "Mount 1") {
              fit = lm(AvePref ~ Edge + Hue + Sat + Lum + sdHUe + sdSat + sdBright + Entropy + tree,
                       data = df2())
              summary(fit)
            } else {
              fit = lm(AvePref ~ Edge + Hue + Sat + Lum + sdHUe + sdSat + sdBright + Entropy + tree + lake,
                       data = df2())
              summary(fit)
            }
          }
        }
      }
    }
    
   
  }
  )

 
}


shinyApp(ui = ui, server = server)