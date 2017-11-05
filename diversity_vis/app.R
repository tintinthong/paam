#diversityrewards2 
#this seems like the new one
#this is the main document 
#diversity is deprecated, diversity2 contains the main code but not properly written function
#this shiny resolves disadvnatage of diversity and uses functionality of diversity2

#NOTE to fix:
#for some odd ball reason print works within reactive function. I dont know
#why it works and it creeps me out
# Sol: observeEvent or observe is observing the reactive function (SOLVED)
#w2_unit can be more than w1_unit
#DO NOT PUT TWO CONDITIONALS IN OBSERVE 

library(shiny)
library(igraph)
source("../../dist/pareto.R")
source("mod/diversity_mod.R")
set.seed(1)
x<-rgamma(100,1,1)
N<-length(x)
o<-order(x)
x_sort<-x[o]
R<-100

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Reward 2-Partition"),
   
   # Sidebar with a slider input for number of bi
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         sliderInput("q1",
                     "Proportion of data points in WC_1, q1:",
                     min = 0,
                     max = 1,
                     value = 0.5),
         sliderInput("p1",
                     "Proportion of rewards in WC_1, p1:",
                     min = 0,
                     max = 1,
                     value = 0.6),
         sliderInput("q2",
                     "Proportion of data points in WC_2(beginning at end of WC_2), q2:",
                     min = 0,
                     max = 1,
                     value = 0.6),
         sliderInput("p2",
                     "Proportion of rewards in WC_2, p2:",
                     min = 0,
                     max = 1,
                     value = 0.6),
         sliderInput("p3",
                     "Proportion of rewards in WC_3, p3:",
                     min = 0,
                     max = 1,
                     value = 0.5),
         selectInput("dist", "Distribution", c("Pareto", "Log-Normal"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         h2(textOutput("tot_reward")), 
         plotOutput("distPlot1"),
         h2(textOutput("unit_reward")),
         plotOutput("distPlot2"),
         plotOutput("distPlot3")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
   
  observe({
    print(input$q1)
    print(paste("q1=",input$q1,"q2=", input$q2,"p1=",input$p1,"Dist=",input$dist))
  })
  

  
  dat1<-reactive({
    n1=ceiling(N*input$q1)
    WC_1<-x_sort[1:n1]
    w1=input$p1*R
    w1_unit=w1/n1
    n_left=N-n1
    R=(1-input$p1)*R
    return(list(WC_1=WC_1,n1=n1,n_left=n_left,w1=w1,w1_unit=w1_unit,R=R))
  })
  
  
  dat2<-reactive({
    n2=ceiling(dat1()$n_left*input$q2)
    WC_2<-x_sort[(dat1()$n1+1):(dat1()$n1+n2)]
    #select p2 with condition p2<(w1/n1)*(n2/R) range 
    w2=input$p2*dat1()$R
    w2_unit=w2/n2
    n_left=dat1()$n_left-n2
    R=(1-input$p2)*dat1()$R
    return(list(WC_2=WC_2,n2=n2,n_left=n_left,w2=w2,w2_unit=w2_unit,R=R))
  })
  
  dat3<-reactive({
    n3=dat2()$n_left
    WC_3<-x_sort[(dat1()$n1+dat2()$n2+1):length(x_sort)]
    #select p3 with condition p3<(w2/n2)*(n3/R) range 
    w3=input$p3*dat2()$R
    w3_unit=w3/n3
    R=(1-input$p3)*dat2()$R
    return(list(WC_3=WC_3,n3=n3,w3=w3,w3_unit=w3_unit,R=R))
    
    
  })
  
  datsum<-reactive({
    WC=c(dat1()$WC_1,dat2()$WC_2,dat3()$WC_3)
    w_vec=c(dat1()$w1,dat2()$w2,dat3()$w3)
    wg_unit=dat3()$R/length(WC) #left-over residual
    w_vec=c(dat1()$w1,dat2()$w2,dat3()$w3)#+wg_unit*c(dat1()$n1,dat2()$n2,dat3()$n3)
    w_unit_vec=w_vec/c(dat1()$n1,dat2()$n2,dat3()$n3)
    return(list(WC=WC,w_vec=w_vec,w_unit_vec=w_unit_vec))
  })
  
  
 
  observeEvent(datsum(), {
    #print(datsum()$w_vec)#problem here results give the sum of wealth given to last class as the highest
    #print(paste("Unit Vector",datsum()$w_unit_vec))
    
  })
  
  observe({
    #p1*R/n1> R/n1
    #if(input$q1>input$p1){
    #  updateSliderInput(session, "p1", value=input$q1)
    #} 
    
    #when input$q1 is great than there is a problem
    
    if(input$p2>(dat1()$w1/dat1()$n1)*(dat2()$n2/dat1()$R)){
      updateSliderInput(session, "p2", value=(dat1()$w1/dat1()$n1)*(dat2()$n2/dat1()$R))
    }

  })
  
  observe({
    if(input$p3>(dat2()$w2/dat2()$n2)*(dat3()$n3/dat2()$R)){
      updateSliderInput(session, "p3", value=(dat2()$w2/dat2()$n2)*(dat3()$n3/dat2()$R))
    }
  })
  
  
   output$distPlot1 <- renderPlot({
  
     barplot(datsum()$w_vec, main="Wealth to each class",names=c("WC_1","WC_2", "WC_3"))
   })
   output$distPlot2 <- renderPlot({
   
     barplot(datsum()$w_unit_vec, main="Unit Wealth to each class", names=c("w1_unit","w2_unit", "w3_unit"))
   })
   output$distPlot3 <- renderPlot({
     #usual histogram here
     #bins <- seq(min(x), max(x), length.out = input$bins + 1)
     #hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$tot_reward<- renderText({ 
     paste("Total Reward each Class:",paste( datsum()$w_vec,collapse=", ") )
   })
   output$unit_reward<- renderText({ 

     paste("Unit Reward each Class:",paste(fix_dec(datsum()$w_unit_vec,2),collapse=", "))
   })
   

}

# Run the application 
shinyApp(ui = ui, server = server)

