#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Reward 2-Partition"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         sliderInput("q1",
                     "q1:",
                     min = 0,
                     max = 1,
                     value = 0.5),
         sliderInput("p1",
                     "p1:",
                     min = 0,
                     max = 1,
                     value = 0.6),
         sliderInput("q2",
                     "q2:",
                     min = 0,
                     max = 1,
                     value = 0.6),
         sliderInput("p2",
                     "p2:",
                     min = 0,
                     max = 1,
                     value = 0.6),
         sliderInput("p3",
                     "p3:",
                     min = 0,
                     max = 1,
                     value = 0.5),
         selectInput("dist", "Distribution", c("Pareto", "Log-Normal"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot1"),
         plotOutput("distPlot2"),
         plotOutput("distPlot3")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
   
  set.seed(1)
  N<-length(x)
  x<-rgamma(100,1,1)
  o<-order(x)
  x_sort<-x[o]
  R<-100
  
  datObs <- reactive({
    
    paste("q1=",input$q1,"q2=", input$q2,"p1=",input$p1,"Dist=",input$dist)
    
  })
  
  dat1<-reactive({
    n1=ceiling(N*input$q1)
    print(paste("n1=",n1))
    WC_1<-x_sort[1:n1]
    #print(paste("length of WC_1=",length(WC_1)))
    #select p1 with condition p1>q1(this should be selected based on skew)
    w1=input$p1*R
    w1_unit=w1/n1
    n_left=N-n1
    #r1=R
    R=(1-input$p1)*R
    return(list(WC_1=WC_1,n1=n1,n_left=n_left,w1=w1,w1_unit=w1_unit,R=R))
  })
  
  dat2<-reactive({
    n2=ceiling(dat1()$n_left*input$q2)
    print(paste("n2=",n2))
    WC_2<-x_sort[(dat1()$n1+1):(dat1()$n1+n2)]
   # print(paste("dat1()$n1=",length((dat1()$n1+1):n2)))
    #print(paste("length of WC_2=",length(WC_2)))
    #select p2 with condition p2<(w1/n1)*(n2/R) range 
    w2=input$p2*dat1()$R
    w2_unit=w2/n2
    #n_left=N-dat1()$n_left-n2 
    n_left=dat1()$n_left-n2
    #r2=R
    R=(1-input$p2)*dat1()$R
    return(list(WC_2=WC_2,n2=n2,n_left=n_left,w2=w2,w2_unit=w2_unit,R=R))
  })
  
  dat3<-reactive({
    n3=dat2()$n_left
    #print(paste("n3=",n3))
    WC_3<-x_sort[(dat1()$n1+dat2()$n2+1):length(x_sort)]
    #print(paste("length of WC_3=",length(WC_3)))
    #select p3 with condition p3<(w2/n2)*(n3/R) range 
    w3=input$p3*dat2()$R
    w3_unit=w3/n3
    #r3=R
    R=(1-input$p3)*dat2()$R
    return(list(WC_3=WC_3,n3=n3,w3=w3,w3_unit=w3_unit,R=R))
    
    
  })
  
  datsum<-reactive({
    WC=c(dat1()$WC_1,dat2()$WC_2,dat3()$WC_3)
    print(paste("length of WC=",length(WC)))
    w_vec=c(dat1()$w1,dat2()$w2,dat3()$w3)
    wg_unit=dat3()$R/length(WC) #left-over residual
    print(paste("lengths",c(length(dat1()$WC_1),length(dat2()$WC_2),length(dat3()$WC_3))))
    print(paste("wg_unit",wg_unit))
    w_vec=c(dat1()$w1,dat2()$w2,dat3()$w3)+wg_unit*c(length(dat1()$WC_1),length(dat2()$WC_2),length(dat3()$WC_3))
    print(w_vec/c(length(dat1()$WC_1),length(dat2()$WC_2),length(dat3()$WC_3)))
    w_unit_vec=w_vec/c(length(dat1()$WC_1),length(dat2()$WC_2),length(dat3()$WC_3))
    return(list(WC=WC,w_vec=w_vec,w_unit_vec=w_unit_vec))
  })
  
  
  #observing event  
  #observeEvent(datObs(), {
  #  print(datObs())
  #})
  
  #observeEvent(dat1(), {
  #  print(paste("n1=",dat1()$n1,"w1=",dat1()$w1,"w1_unit=",dat1()$w1_unit))
  #})
  
  #observeEvent(dat2(), {
  #  print(paste("n2=",dat2()$n2,"w2=",dat2()$w2,"w2_unit=",dat2()$w2_unit))
  #})
  
  #observeEvent(dat3(), {
  #  print(paste("n3=",dat3()$n3,"w3=",dat3()$w3,"w3_unit=",dat3()$w3_unit))
  #})
  
  observeEvent(datsum(), {
    print(datsum()$w_vec)#problem here results give the sum of wealth given to last class as the highest
    print(paste("Unit Vector",datsum()$w_unit_vec))
    
    #print(paste("w_vec=",datsum()$w_vec,"w_unit_vec",datsum()$w_unit_vec))
  })
  
  observe({
    if(input$q1>input$p1){
      updateSliderInput(session, "p1", value=input$q1)
      #print(paste("q1=",input$q1,"> p1=",input$p1))
    } 
    
    if(input$p2>(dat1()$w1/dat1()$n1)*(dat2()$n2/dat1()$R)){
      updateSliderInput(session, "p2", value=(dat1()$w1/dat1()$n1)*(dat2()$n2/dat1()$R))
    }
    #if(input$p3>(dat2()$w2/dat2()$n2)*(dat3()$n3/dat2()$R)){
    #  updateSliderInput(session, "p3", value=(dat2()$w2/dat2()$n2)*(dat3()$n3/dat2()$R))
    #}
    
    if(input$p3>(dat2()$w2/dat2()$n2)*(dat3()$n3/dat2()$R)){
      updateSliderInput(session, "p3", value=(dat2()$w2/dat2()$n2)*(dat3()$n3/dat2()$R))
    }
  })
  
  
   
   #output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
  #    x    <- faithful[, 2] 
  #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
  #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # })
   output$distPlot1 <- renderPlot({
     # generate bins based on input$bins from ui.R
     #x    <- faithful[, 2] 
     #bins <- seq(min(x), max(x), length.out = input$bins + 1)
     
     # draw the histogram with the specified number of bins
     barplot(datsum()$w_vec)
   })
   output$distPlot2 <- renderPlot({
     # generate bins based on input$bins from ui.R
     #x    <- faithful[, 2] 
     #bins <- seq(min(x), max(x), length.out = input$bins + 1)
     
     # draw the histogram with the specified number of bins
     barplot(datsum()$w_unit_vec)
   })
   output$distPlot3 <- renderPlot({
     # generate bins based on input$bins from ui.R
     #x    <- faithful[, 2] 
     bins <- seq(min(x), max(x), length.out = input$bins + 1)
     
     # draw the histogram with the specified number of bins
     bins <- seq(min(x), max(x), length.out = input$bins + 1)
     hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

