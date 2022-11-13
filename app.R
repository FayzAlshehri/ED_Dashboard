#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(flexdashboard)
library(tidyverse)
library(plotly)
library(shiny)
library(readxl)



Ed_data <- read_excel("Ed_data.xlsx")
ed1<- Ed_data%>%
  group_by(PrimaryDiag)%>%
  head(10)%>%
  arrange(desc(PrimaryDiag))


# edplot1 <- ggplot(ed1, aes(DiagnosisDesc, PrimaryDiag, color = DiagnosisDesc, size = PrimaryDiag)) +
#   geom_point()+
# 
#   xlab("Diagnosis") + ylab("Frequency") +
#   ggtitle("Most Common Cases in ER") +
#   theme_bw()+
#   #remove value in x axis for more clarity
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# options(scipen = 100)
# 
# 
# # overcome duplicate in info in mouse hover
# fig1 <- ggplotly(edplot1, tooltip = c("x","y"))
# 
# fig1




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ER"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
       
          
            sliderInput("count", "text", 
                        min(0),
                        max(555555),
                        value = c(1, 300), 
                        step = 10),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
          
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
      ggplot(filter(ed1,  PrimaryDiag>=input$count[1] & PrimaryDiag<=input$count[2]))+
        geom_point(aes(DiagnosisDesc, PrimaryDiag, color = DiagnosisDesc, size = PrimaryDiag))
        
        
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
