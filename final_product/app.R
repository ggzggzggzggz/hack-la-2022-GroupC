# Learning Analytics Hackthon Group C
library(shiny)
library(tidyverse)
library(shinythemes)
library(ggrepel)
library(scales)

# import the data
source("helpers.R")

# Define UI ----
ui <- fluidPage( 
  theme = shinytheme("flatly"),
  tags$head(tags$style('
     body {
        font-family: Arial; 
        font-size: 16px; 
     }'
  )),
  titlePanel("Student Performance Analysis Based on Discussion Activity"),
  navbarPage(" ",
    tabPanel("Acticity Overview over Time",
          sidebarLayout(
            sidebarPanel(
                radioButtons("plotType", "Plot type",
                              c("by Date"="d", "by Active User"="u")
                 )
              ),
            mainPanel(
              conditionalPanel(
                condition = "input.plotType == 'd'",
                plotOutput("plot")
              ),
              conditionalPanel(
                condition = "input.plotType == 'u'",
                plotOutput("plot2")
              )
            )
            
             )
    ), # end of tab1
    tabPanel("User Summaries",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput('x', 'Student ID', choices = discussions_merge$user_id),
               ),
               mainPanel(
                 verbatimTextOutput('values')
               )
             )
             
    ), # end of tab2
    tabPanel("Top Discussion Contributors",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("rank",label = h3("Rank students by:"),
                              c("# of Posts" = "posts",
                                "Total word count" = "words",
                                "Total received likes" = "likes")),
               ),
               mainPanel(
                 fluidRow(column(4,dataTableOutput('dto'))),
                 plotOutput("plot3")
               )
             )
    ), # end of tab3
    
  ) # end of navbarPage
)


server <- function(input, output) {
  require(ggplot2)
    # plot for tab1
  # by post
    output$plot <- renderPlot({
      ggplot(data = discusstions_date_merge, aes(x = date, y = post)) +
        geom_line() +
        labs(x = "Date", y = "# of post")+
        theme(text = element_text(size = 20)) +
        ggtitle("# of Posts per Day")
    })
    # by users
    output$plot2 <- renderPlot({ 
      ggplot(data = discusstions_date_merge, aes(x = date, y = users)) +
        geom_line() +
        labs(x = "Date", y = "# of post")+
        theme(text = element_text(size = 20)) +
        ggtitle("# of Posts per Day")
    })
    
    # tab2
    output$values <- renderPrint({
      discussions_merge = discussions_merge |> rename(`Student Name` = user_id)
      x = filter(discussions_merge, `Student Name` == input$x) 
      x
    })
    
    # tab 3
    output$dto <- renderDataTable({discussions_merge[order(-discussions_merge[input$rank]),]})
    
    output$plot3 <- renderPlot({ 
      ggplot(discusstion_topic_hot, aes(x="", y=x, fill=topic))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start=0)+
        theme_minimal()+
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          axis.ticks = element_blank())+
        theme(axis.text.x=element_blank())+
        theme(axis.text.y=element_blank())+
        geom_text_repel(aes(y = x/16 + c(0, cumsum(x)[-length(x)]), 
                            label = percent(x/1611)), size=5)
    })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
