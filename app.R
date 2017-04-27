library(shiny)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Diabetes Patients"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput("i_age", "Age",choices = c("[0-10)","[10-20)","[20-30)","[30-40)","[40-50)","[50-60)","[60-70)","[70-80)","[80-90)","[90-100)"), selected = "[40-50)")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Bubble", plotOutput("distPlot")), 
          tabPanel("Multiples", plotOutput("smallm")), 
          tabPanel("Parallel Coord", plotOutput("pc"))
        )
         
      )
      
        
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     all <- read.csv("diabetic_data.csv")
     
     ndata <- all[,c(1,3,5,6,10,13:18)]
     
     ndata <- ndata[ndata$weight != "?",]
     ndata <- ndata[ndata$age == input$i_age,]
     qplot(x = num_lab_procedures, y = num_procedures, xlab = "# lab procedures", ylab = "# procedures",
           size = num_medications, data = ndata, main = "Visit details for selected age range")
     
   })
   
   output$smallm <- renderPlot({
     all <- read.csv("diabetic_data.csv")
     
     ndata <- all[,c(1,3,5,6,10,13:18)]
     
     ndata <- ndata[ndata$weight != "?",]
     ndata <- ndata[ndata$age == input$i_age,]
     ndata <- ndata[ndata$race != "?",]
     ggplot(data=ndata, aes(x=race,y=time_in_hospital)) +
       geom_bar(stat = "summary", fun.y = "mean") +
       facet_wrap(~weight) + coord_flip() + 
       ggtitle("Length of stay faceted by body weight in kg") + xlab("Race Category") +
       ylab("Days in Hospital")
   })
   
   output$pc <- renderPlot({
     all <- read.csv("diabetic_data.csv")
     
     ndata <- all[,c(1,3,5,6,10,13:18)]
     
     #ndata <- ndata[ndata$weight != "?",]
     ndata <- ndata[ndata$age == input$i_age,]
     pcdata <- ndata[,c(5:8)]
     parcoord(pcdata, col=pcdata$time_in_hospital, var.label=TRUE)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

