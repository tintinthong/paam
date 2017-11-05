#/diversity/

library(shiny)
library(igraph)
library(ineq)
source("../../dist/pareto.R")
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

ui <- fluidPage(
   
   # Application title
   titlePanel("Diversity Rewards"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         sliderInput("n",
                     "N",
                     min = 1,
                     max = 1000,
                     value = 500),
         sliderInput("alpha", "alpha",
                     min = 1, max = 5,
                     value = 2, step = 0.05),
         sliderInput("xmin",
                     "xmin",
                     min = 1,
                     max = 50,
                     value = 20),
         sliderInput("p1",
                     "prob1:",
                     min = 0.1,
                     max = 1,
                     value = 0.5),
         sliderInput("p2",
                     "prob2:",
                     min = 0,
                     max = 1,
                     value = 0.6),
         sliderInput("pool",
                     "Reward Pool(Percentage)",
                     min =0,
                     max = 1,
                     value = 0.1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabPanel("Plot",
                 # fluidRow(...)
                 plotOutput("distPlot"),
                 plotOutput("distPlot2"),
                 plotOutput("distPlot3")
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  dataInputmain <- reactive({
    rpareto(input$n,input$alpha,input$xmin)
  })
  
  dataInputlor <- reactive({
    Lc(dataInputmain())
  })
  
   dataInput1 <- reactive({
     diversity(dataInputmain(),prob=c(input$p1,input$p2),percent=input$pool)
   })
   
   
   output$distPlot <- renderPlot({

      bins <- seq(min(dataInput1()$x), max(dataInput1()$x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      place<-hist(dataInput1()$x, breaks = bins,plot=FALSE )
      hist(dataInput1()$x, breaks = bins, col = 'indianred1',xlim=c(1,max(dataInput1()$x)), border = 'white',xlab="Wealth",main="Histogram of wealth(before diversity reward)",ylim=c(0,max(place$counts)))
      
      abline(v=dataInput1()$quant[1])
      abline(v=dataInput1()$quant[2])
      text(dataInput1()$quant[1],1,paste(input$p1),pos=2)
      text(dataInput1()$quant[2],1,paste(input$p2),pos=2)
   })
   output$distPlot2<- renderPlot({
     
     WC<-dataInput1()$WC
     bins <- seq(min(WC), max(WC), length.out = input$bins + 1)
     place<-hist(WC, breaks = bins,plot=FALSE )
     
     # draw the histogram with the specified number of bins
     hist(WC, breaks = bins, col = 'lightblue',xlim=c(1,max(dataInput1()$x)), border = 'white',xlab="Wealth",main="Histogram of wealth(after diversity reward)",ylim=c(0,max(place$counts)))
    
   })
   output$distPlot3<- renderPlot({
     lor_WC<-Lc(dataInput1()$WC)
     lor<-dataInputlor()
     
     # draw the histogram with the specified number of bins
     plot(lor,xlab="Percentage of nodes",ylab="Percentage of wealth of network",col="red")
     lines(lor_WC$p,lor_WC$L,col="blue")
     abline(v=input$p1,h=lor$L[which.min(abs(lor$p-input$p1))])
     abline(v=input$p2,h=lor$L[which.min(abs(lor$p-input$p2))])
     text(0.2,0.6,paste("Percent Rank Preserved ",specify_decimal(dataInput1()$rank_change,4) ))
     text(0.2,0.8,paste("Gini Index Before= ",specify_decimal(dataInput1()$gini_before,4)) )
     text(0.2,0.7,paste("Gini Index After= ",specify_decimal(dataInput1()$gini_after,4)))
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

#histogram is not right (uses maximum, it would be nice if they shared the ylim)
#reward given to second class is larger occasionally. Per person, it is getting more
#ranking is not right
#what is wrong with the diversity reward
#it is possible, especially when reward is high, for tau2 per unit to be higher than tau1 per unit
#can adjust this by adjusting gap between prob1 and prob2
#its okay per unit to have less reward pool give for WC_2 than WC_1, but not tau1 give

