##########
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(tidyr)

# read data
rawData<-readRDS("AB_NYC_2019.RDS")
rawData%>%glimpse()

# Get neighbourhood list
neighbourhood_names <- 
    rawData %>% 
    distinct(neighbourhood) %>% 
    unlist(.)

names(neighbourhood_names ) <- NULL
neighbourhood_names<-as.character(neighbourhood_names)

# Define UI for application that draws bar charts 
ui <- fluidPage(
    
    # Application title
    titlePanel("New York City Airbnb Data Analysis"),
    
    # Sidebar with a slider input for the years, genre and number of votes
    sidebarLayout(
        sidebarPanel(
            #select neighbourhood
            selectInput(inputId = "neighbourhood_names",
                        label="Select Neighbourhood:",
                        choices = c("All",neighbourhood_names),
                        multiple = TRUE,
                        selected = "Midtown"),
            
            selectInput(inputId = "room_type",
                        label="Select Room Type:",
                        choices = unique(rawData$room_type),
                        multiple = TRUE,
                        selected = "Private room"),
            
            sliderInput(inputId = "Price",
                        label= "Select Price Range",
                        value =c(100,500),
                        min=min(rawData$price), 
                        max=max(rawData$price)
                       ),
            
            sliderInput(inputId = "TotalReview",
                        label= "Select Total Number of Reviews",
                        value =c(10,20),
                        min=min(rawData$number_of_reviews), 
                        max=max(rawData$number_of_reviews)
            ),
            
            
            sliderInput(inputId = "Availability",
                        label= "Select Availability",
                        value =c(100,200),
                        min=min(rawData$availability_365), 
                        max=max(rawData$availability_365)
            )
        ),
        
        # Show the plot
        mainPanel(
            tabsetPanel(
               tabPanel("Price Table",DT::DTOutput('PriceTable')),
               tabPanel("Price Plot",plotly::plotlyOutput('PricePlot')),
               tabPanel("Availability Plot",plotly::plotlyOutput('AvailabilityPlot')),
               tabPanel("Review Plot",plotly::plotlyOutput('ReviewPlot'))
            )
            
        )
    )
)

# Define server logic required to draw a scatter plot
server <- function(input, output){
    
          # filter according to input and get table
    
            output$PriceTable<-DT::renderDT({
                
                if(input$neighbourhood_names!="All"){
                    filtered_rawData<-rawData %>% 
                        filter(neighbourhood %in% input$neighbourhood_names,room_type==input$room_type)%>%
                        filter(price>=input$Price[1],price<=input$Price[2])%>%
                        filter(number_of_reviews>=input$TotalReview[1],number_of_reviews<=input$TotalReview[2])%>%
                        filter(availability_365>=input$Availability[1],availability_365<=input$Availability[2])
                }else{
                    filtered_rawData<-rawData %>% 
                        filter(room_type==input$room_type)%>%
                        filter(price>=input$Price[1],price<=input$Price[2])%>%
                        filter(number_of_reviews>=input$TotalReview[1],number_of_reviews<=input$TotalReview[2])%>%
                        filter(availability_365>=input$Availability[1],availability_365<=input$Availability[2])
                }
                
                filtered_rawData%>%
                            select(neighbourhood_group,neighbourhood,room_type,name,host_name,price,
                                   number_of_reviews,reviews_per_month,availability_365,last_review )
            })
            
            # Plot Price 
            
            output$PricePlot<-plotly::renderPlotly({
                
                if(input$neighbourhood_names!="All"){
                    filtered_rawData<-rawData %>% 
                        filter(neighbourhood %in% input$neighbourhood_names,room_type==input$room_type)%>%
                        filter(price>=input$Price[1],price<=input$Price[2])%>%
                        filter(number_of_reviews>=input$TotalReview[1],number_of_reviews<=input$TotalReview[2])%>%
                        filter(availability_365>=input$Availability[1],availability_365<=input$Availability[2])
                }else{ 
                    filtered_rawData<-rawData %>% 
                        filter(room_type==input$room_type)%>%
                        filter(price>=input$Price[1],price<=input$Price[2])%>%
                        filter(number_of_reviews>=input$TotalReview[1],number_of_reviews<=input$TotalReview[2])%>%
                        filter(availability_365>=input$Availability[1],availability_365<=input$Availability[2])
                }
                    
                filtered_rawData%>%
                    group_by(neighbourhood)%>%
                    summarize(MeanPrice=mean(price))%>%
                    ggplot(aes(x=neighbourhood,y=MeanPrice))+geom_col(fill="darkslateblue")+
                    labs(title="Average Prices vs Neighbourhood",
                         x="Neighbourhood",
                         y="Mean Price") +
                    theme(axis.text.x = element_text(angle = 45))
            } )
            
            # Plot Availability Statistics
            
            output$AvailabilityPlot<-plotly::renderPlotly({
                
                if(input$neighbourhood_names!="All"){
                    filtered_rawData<-rawData %>% 
                        filter(neighbourhood %in% input$neighbourhood_names,room_type==input$room_type)%>%
                        filter(price>=input$Price[1],price<=input$Price[2])%>%
                        filter(number_of_reviews>=input$TotalReview[1],number_of_reviews<=input$TotalReview[2])%>%
                        filter(availability_365>=input$Availability[1],availability_365<=input$Availability[2])
                }else{ 
                    filtered_rawData<-rawData %>% 
                        filter(room_type==input$room_type)%>%
                        filter(price>=input$Price[1],price<=input$Price[2])%>%
                        filter(number_of_reviews>=input$TotalReview[1],number_of_reviews<=input$TotalReview[2])%>%
                        filter(availability_365>=input$Availability[1],availability_365<=input$Availability[2])
                }
                
                filtered_rawData%>%
                    group_by(neighbourhood)%>%
                    summarize(MeanAval=mean(availability_365))%>%
                    ggplot(aes(x=neighbourhood,y=MeanAval))+geom_col(fill="tomato3")+
                    labs(title="Average Availabilities vs Neighbourhood",
                         x="Neighbourhood",
                         y="Mean Availabilities") +
                    theme(axis.text.x = element_text(angle = 45))
            } )
            
            # Plot Review Statistics
            
            output$ReviewPlot<-plotly::renderPlotly({
                
                if(input$neighbourhood_names!="All"){
                    filtered_rawData<-rawData %>% 
                        filter(neighbourhood %in% input$neighbourhood_names,room_type==input$room_type)%>%
                        filter(price>=input$Price[1],price<=input$Price[2])%>%
                        filter(number_of_reviews>=input$TotalReview[1],number_of_reviews<=input$TotalReview[2])%>%
                        filter(availability_365>=input$Availability[1],availability_365<=input$Availability[2])
                }else{ 
                    filtered_rawData<-rawData %>% 
                        filter(room_type==input$room_type)%>%
                        filter(price>=input$Price[1],price<=input$Price[2])%>%
                        filter(number_of_reviews>=input$TotalReview[1],number_of_reviews<=input$TotalReview[2])%>%
                        filter(availability_365>=input$Availability[1],availability_365<=input$Availability[2])
                }
                
                filtered_rawData%>%
                    group_by(neighbourhood)%>%
                    summarize(MeanRev=mean(number_of_reviews))%>%
                    ggplot(aes(x=neighbourhood,y=MeanRev))+geom_col(fill="olivedrab")+
                    labs(title="Average Review Numbers vs Neighbourhood",
                         x="Neighbourhood",
                         y="Mean Review Numbers") +
                    theme(axis.text.x = element_text(angle = 45))
            } )
    }

# Run the application 
shinyApp(ui = ui, server = server)

