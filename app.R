library(shinydashboard)
library(ggplot2)
sum_by_median<-read.csv("./Data/sum_by_median.csv")
ulist=levels(sum_by_median$Category)
names(ulist) = ulist

ui <- dashboardPage(
  dashboardHeader(title = "Median Water Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overall", tabName = "Overall", icon = icon("dashboard")),
      menuItem("Upload Dataset", tabName = "Upload", icon = icon("th"))
    )
  )
    ,
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      # A static valueBox
      valueBox("58K Gal", "Average Total Water Used by a Median this Year", icon = icon("cloud")),
      valueBox("64M Gal", "Total Water Used in 2015", icon = icon("credit-card")),
      valueBox("66%", "Total Reduction in Water Consumption Since 2012", icon = icon("bullseye")),
      
      # Dynamic valueBoxes
      valueBoxOutput("progressBox"),
      
      valueBoxOutput("approvalBox")
    ),
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        "Per Billing", br(), "Almost all of the medians averaged under 200 HCF (149K Gallons) per billing period. One outlier was a median near 10876 San Fernando Road Pacoima CA that used 730 HCF over only one 60 day billing period",
        sliderInput("bins", "Bin Width:", 0.1, 200, 50), Title = "Per Billing Period", status = "primary"
      )
    ),
    fluidRow(
      box(plotOutput("plot2", height = 250)),
      
      box(
        "Annual Sum", br(), "Here is a distribution of the total of water usage per median in 2015. One outlier was a median near 6564 Corbin Ave Reseda CA which used ~2.9K HCF in 2015",
        sliderInput("bins2", "Bin Width:", 0.1, 200, 50), status = "primary"
      )
    ),
    fluidRow(
      box(
        "Meter Breakdown", br(), "Here is a breakdown of water usage by meter during 2015. Adjust the slider to include more meters.",
        sliderInput("bins3", "Min Usage (HCF):", 0.1, 2000, 1000)
      ),
      box(plotOutput("plot3", height = 250))
    ),
    fluidRow(
      box(
        "Results can be individually reviewed with the correct input of meter number. (Note: dates on x-axis still need to be ordered)",
        selectInput("proj", "Meter Number:",ulist)
      ),
      box(plotOutput("plot4", height = 250))
    )
)
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  begin<-read.csv("./Data/begin.csv")
  daily<-read.csv("./Data/dailyusage.csv")
  total<-read.csv("./Data/totalcost.csv")
  watercost<-read.csv("./Data/watercost.csv")
  sewercost<-read.csv("./Data/sewercost.csv")
  
  bymeter<-read.csv("./Data/bymeter.csv")
  
  sum_by_median<-read.csv("./Data/sum_by_median.csv")
  begin$X.1<-as.numeric(as.character(begin$X.1))
  dates <- unique(sort(begin$District...Harbor))
  begin$District...Harbor<-factor(begin$District...Harbor, labels = dates, ordered = T)
  begin$District...Harbor<-as.Date(begin$District...Harbor, format = "%m/%d/%Y")
  
  pdata <- reactive({
    subset(begin, X.5==input$proj)
  }
  )
  
  newdata <- reactive({
    subset(sum_by_median, x>input$bins3)
  })
  
  data<-reactive({
    file1<-input$file
    if(is.null(file1)){return()}
    read.csv(file=file1$datapath,sep=input$sep,header=FALSE)
  })
  
  output$plot1 <- renderPlot({
    bins <- input$bins
    
    ggplot(begin, aes(X.1))+geom_histogram(binwidth=bins)+ggtitle("Distribution of Water Consumption in 2015")+labs(x="Average Water Consumed Per Median",y="Count")
  })
  
  output$plot2 <- renderPlot({
    bins2 <- input$bins2
    
    ggplot(sum_by_median, aes(sum_by_median$x))+geom_histogram(binwidth=bins2)+ggtitle("Distribution Water Consumption in 2015")+labs(x="Total Water Consumed Per Median (2016)",y="Count") 
  })
  
  output$plot3 <- renderPlot({
    bins3 <- input$bins3
    
    ggplot(newdata(), aes(newdata()[,2],newdata()[,3]))+geom_bar(stat="identity")+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+labs(x="Meter # at Median",y="Water Used in 2015 (HCF)")+ggtitle("Total Consumption")
  })
  
  output$plot4 <- renderPlot({
    ggplot(pdata(), aes(pdata()[,2], pdata()[,4])) + geom_line() + xlab("Date (2015)") + ylab("Water Consumption (HCF)")+ggtitle("A Year in Usage")
  })
}

shinyApp(ui, server)