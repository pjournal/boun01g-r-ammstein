
# Load Required Libraries

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(tidyr)

# Read Data
ShinyRawData <-readRDS("FinalProjectRawData.RDS") 


# Get Age Group list
Age_Group <- 
    ShinyRawData %>% 
    distinct(AgeGroup) %>% 
    unlist(.)

names(Age_Group ) <- NULL
Age_Group<-as.character(Age_Group)

# Get Education Level list
Edu_Level <- 
    ShinyRawData %>% 
    distinct(EducationLevel) %>% 
    unlist(.)

names(Edu_Level ) <- NULL
Edu_Level<-as.character(Edu_Level)

# Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel("First Marriage Statistics in Turkey"),
    
    # Sidebar with a slider input for the years, genre and number of votes
    sidebarLayout(
        sidebarPanel(
            
            # Show information
            actionButton("info", "Show İnformation"),
            
            #select Age Group
            selectInput(inputId = "Age_Group",
                        label="Select Age Group:",
                        choices = c("All",Age_Group),
                        multiple = TRUE,
                        selected = c("16-19","20-24")),
            
            #Select Education Level
            selectInput(inputId = "Edu_Level",
                        label="Select Education Level:",
                        choices = Edu_Level,
                        multiple = TRUE,
                        selected = c("İlkokul","İlköğretim")),
            
            #Select Gender
            selectInput(inputId = "Gender",
                        label="Select Gender:",
                        choices = c("Female", "Male"),
                        multiple = TRUE,
                        selected = c("Male","Female")),
            
            #Select Year
            sliderInput(inputId = "Year",
                        label= "Select Year",
                        value =c(2018,2019),
                        min=min(as.numeric(ShinyRawData$Year)), 
                        max=max(as.numeric(ShinyRawData$Year)))
        ),
        
        # Show the outputs
        mainPanel(
            tabsetPanel(
                tabPanel("Table",DT::DTOutput('Table')),
                tabPanel("Age Plot",plotly::plotlyOutput('AgePlot')),
                tabPanel("Education Level Plot",plotly::plotlyOutput('EduPlot'))
            )
            
        )
    )
)

# Define Server

server <- function(input, output){
    
    info_text<-"This app contains information about the first marriage statistics in Turkey 
                    between 2009-2019 years for different genders, education levels and age groups.
                    Data is obtained from [TUIK site](https://biruni.tuik.gov.tr/medas/?kn=112&locale=tr)."
    
    observeEvent(input$info,{showModal(modalDialog(info_text))})
    
    # Filter according to inputs and get table
    
    output$Table<-DT::renderDT({
        
        if(input$Age_Group!="All"){
            filtered_rawData<-ShinyRawData %>% 
                filter(AgeGroup %in% input$Age_Group)%>%
                filter(EducationLevel %in% input$Edu_Level)%>%
                filter(Gender %in% input$Gender)%>%
                filter(Year>=input$Year[1],Year<=input$Year[2])
        }else{
            filtered_rawData<-ShinyRawData %>% 
                filter(EducationLevel %in% input$Edu_Level)%>%
                filter(Gender %in% input$Gender)%>%
                filter(Year>=input$Year[1],Year<=input$Year[2])
        }
        
        filtered_rawData
    })
    
    # Filter according to inputs and get Age Plot
    
    output$AgePlot<-plotly::renderPlotly({
        
        if(input$Age_Group!="All"){
            filtered_rawData<-ShinyRawData %>% 
                filter(AgeGroup %in% input$Age_Group)%>%
                filter(EducationLevel %in% input$Edu_Level)%>%
                filter(Gender %in% input$Gender)%>%
                filter(Year>=input$Year[1],Year<=input$Year[2])
        }else{
            filtered_rawData<-ShinyRawData %>% 
                filter(EducationLevel %in% input$Edu_Level)%>%
                filter(Gender %in% input$Gender)%>%
                filter(Year>=input$Year[1],Year<=input$Year[2])
        }
        
        age_rawData<-filtered_rawData %>%
            group_by(AgeGroup,Year,Gender) %>%
            summarise(Average=round(mean(NbOfFirstMarriages),digits = 0))
        

        ggplot(age_rawData, aes(x=Year, y=Average, fill=AgeGroup))+
            geom_col() +
            labs(title="Yearly Average First Marriage Trend For Each Age Group",
                 y="Average First Marriage Numbers",
                 x="Year") +
            theme(axis.text.x = element_text(angle = 90))+
            facet_wrap(~Gender)
        
        
 })
    # Filter according to inputs and get Education Level plot
    
    output$EduPlot<-plotly::renderPlotly({
        
        if(input$Age_Group!="All"){
            filtered_rawData<-ShinyRawData %>% 
                filter(AgeGroup %in% input$Age_Group)%>%
                filter(EducationLevel %in% input$Edu_Level)%>%
                filter(Gender %in% input$Gender)%>%
                filter(Year>=input$Year[1],Year<=input$Year[2])
        }else{
            filtered_rawData<-ShinyRawData %>% 
                filter(EducationLevel %in% input$Edu_Level)%>%
                filter(Gender %in% input$Gender)%>%
                filter(Year>=input$Year[1],Year<=input$Year[2])
        }
        
        Edu_rawData<-filtered_rawData %>%
            group_by(EducationLevel,Year,Gender) %>%
            summarise(Average=round(mean(NbOfFirstMarriages),digits = 0))
        
        ggplot(Edu_rawData, aes(x=Year, y=Average, fill=EducationLevel))+
            geom_col() +
            labs(title="Yearly Average First Marriage Trend For Each Education Level",
                 y="Average First Marriage Numbers",
                 x="Year") +
            theme(axis.text.x = element_text(angle = 90))+
            facet_wrap(~Gender)
        
        
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)


