#this is the most recent 
#attempting to add modules

library(shiny)
library(igraph)
library(ineq)
source("../../dist/pareto.R")
source("mod/diversity_mod.R")


#add checkbox inputs for constraints

#w2_unit>w1_unit something very wrong
#0.5,0.24,0.18,0.06,0
#may be off by a step size
#or may reach a point where p2 is just at 0

#cant help



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
      checkboxInput("constraint", "Turn on Constraints", FALSE),
      checkboxInput("hist", "Add old histogram", FALSE),
      selectInput("dist", "Distribution", c("Pareto", "Log-Normal","Gamma"))
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(
        id = "conditionedPanels",
        tabPanel("Wealth Summary",
                 h2(textOutput("tot_reward")),
                 plotOutput("distPlot1"),
                 h2(textOutput("unit_reward")),
                 plotOutput("distPlot2"),
                 value=1 ),
        tabPanel("Inequality", 
                 plotOutput("distPlot3"),
                 plotOutput("distPlot4"),
                 #h2("Current batch CSV"),
                 value=2
                 )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  set.seed(1)
  x<-rgamma(100,1,1)
  N<-length(x)
  o<-order(x)
  x_sort<-x[o]
  R<-100

  
  observe({
    print(paste("q1=",input$q1,"q2=", input$q2,"p1=",input$p1,"Dist=",input$dist))
  })
  
  
  
 dat1<-reactive({
    party_fun(x_sort=x_sort,N=N,p=input$p1,q=input$q1,R=R)
  })
 
 #observeEvent(datObs(), {
#   print(datObs())
   
# })
 
  dat2<-reactive({
    party_fun(x_sort=dat1()$x_upd,N=dat1()$n_left,p=input$p2,q=input$q2,R=dat1()$R)
   
  })
  
  
  
  dat3<-reactive({
    party_fun(x_sort=dat2()$x_upd,N=dat2()$n_left,p=input$p3,q=input$q2,R=dat2()$R,type="last")
    
  })
  
  
  observe({
    #print(paste("p2",input$p2))
    #print((dat1()$w/dat1()$n)*(dat2()$n/dat1()$R))
    #print(dat2())
    #print(dat2()$n_left)
    #print(datsum()$w_unit_vec)
    #print(dat3())
    #saveRDS(dat3(), "dat3_save")
    #dat3_save <- readRDS("dat3_save")
  })
  
  datsum<-reactive({
    WC<-c(dat1()$WC,dat2()$WC,dat3()$WC)
    w_vec=c(dat1()$w,dat2()$w,dat3()$w) #find total wealth allocated to each class
    wg_unit=dat3()$R/length(WC) #find left over unit wealth
    #w_vec=c(dat1()$w,dat2()$w,dat3()$w)#+wg_unit*c(dat1()$n1,dat2()$n2,dat3()$n3) 
    w_unit_vec=w_vec/c(dat1()$n,dat2()$n,dat3()$n) #find unit wealth of each class
    
    #update welath of everyone in each class
    WC_n1<-dat1()$WC+w_unit_vec[1]
    WC_n2<-dat2()$WC+w_unit_vec[2]
    WC_n3<-dat3()$WC+w_unit_vec[3]
    WC_new<-c(WC_n1,WC_n2,WC_n3)
    return(list(WC_new=WC_new,w_vec=w_vec,w_unit_vec=w_unit_vec,WC_n1=WC_n1,WC_n2=WC_n2,WC_n3=WC_n3))
  })
  
  #compute lornze curve of x_sort
  #lor_obj <- reactive({
  #  Lc(x_sort)
  #})
  
  
  #compute inequality metrics
  div_obj <- reactive({
   
    diversity(x_sort=x_sort,WC_n1=datsum()$WC_n1 ,WC_n2=datsum()$WC_n2,WC_n3=datsum()$WC_n3)
    #diversity(dataInputmain(),prob=c(input$p1,input$p2),percent=input$pool)
    #diversity(x,prob1,)
  })
  
#  observeEvent(datsum(), {
    #print(datsum()$w_vec)#problem here results give the sum of wealth given to last class as the highest
    #print(paste("Unit Vector",datsum()$w_unit_vec))
    
#  })
  
  observe({
    
    #when input$q1 is great than there is a problem
    if(input$constraint){
      if(input$p2>(dat1()$w/dat1()$n)*(dat2()$n/dat1()$R)){
        #print(paste(input$p2,(dat1()$w/dat1()$n)*(dat2()$n/dat1()$R)))
        updateSliderInput(session, "p2", value=(dat1()$w/dat1()$n)*(dat2()$n/dat1()$R) )
      }
    
    }
    
  })
  
   observe({
     if(input$constraint){
       if(input$p3>(dat2()$w/dat2()$n)*(dat3()$n/dat2()$R)){
         updateSliderInput(session, "p3", value=(dat2()$w/dat2()$n)*(dat3()$n/dat2()$R))
       }
     }
       
   })
   
   observe({
     #print(dat2()$WC)
     #print(lor_obj())
     print(div_obj())
     
   })
  
  
  output$distPlot1 <- renderPlot({
    
    barplot(datsum()$w_vec, main="Wealth to each class",names=c("WC_1","WC_2", "WC_3"))
  })
  output$distPlot2 <- renderPlot({
    
    barplot(datsum()$w_unit_vec, main="Unit Wealth to each class", names=c("w1_unit","w2_unit", "w3_unit"))
  })
  output$distPlot3 <- renderPlot({
    x <-datsum()$WC_new
    y<- x_sort
    bottom = min(c(x,y)) # Set the minimum for the breakpoints
    top= max(c(x,y)) # Set the maximum for the breakpoints
    bins=seq(bottom,top, length.out = input$bins + 1)
    h<-hist(x, breaks = bins,col=rgb(1,0,0,0.5),xlab=input$xcol,main="New wealth distribution")
    if(input$hist){
      g<-hist(y,breaks=bins,col=rgb(0,0,1,0.5), add=T)
    }
    text(0.8*max(x),0.85*max(h$counts),paste("mean=",fix_dec(mean(x),2) ))
    text(0.8*max(x),0.80*max(h$counts),paste("median=",fix_dec(median(x),2)))
    abline(v=x[dat1()$n])
    abline(v=x[dat2()$n])
    
  })
  
  output$distPlot4<- renderPlot({
 
    lor_new<-Lc(datsum()$WC_new)
    lor<-Lc(x_sort)
    plot(lor,xlab="Percentage of nodes",ylab="Percentage of wealth of network",col="red")
    lines(lor_new$p,lor_new$L,col="blue")
    
    ###print(which.min(abs(Lc(x)$p-input$p1)))
    
    #fix this. it is not actuall q1 and q2
    #abline(v=input$q1,h=lor$L[which.min(abs(lor$p-input$q1))])
    #abline(v=input$q2,h=lor$L[which.min(abs(lor$p-input$q2))])
   
    text(0.2,0.6,paste("Percent Rank Preserved ",fix_dec(div_obj()$rank_change,4) ))
    text(0.2,0.8,paste("Gini Index Before= ",fix_dec(div_obj()$gini_before,4)) )
    text(0.2,0.7,paste("Gini Index After= ",fix_dec(div_obj()$gini_after,4)))
    
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

