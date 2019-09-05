library(tidyverse)
library(shiny)
library(dplyr)
library(data.table)
library(leaflet)
library(ggplot2)
library(corrplot)
library(gplots)
library(maps)
library(ggmap)
library(plotly)


df <- fread('Crimes_-_2018.csv', na.strings = c("","NA"))

df <- df %>% separate(Date, c("Date","Time"), sep = " ", extra = "merge", fill = "right")

map.df <- df[,c(3,7,21,22)]

map.df$`Primary Type`<-as.factor(map.df$`Primary Type`)

map.df <- na.omit(map.df)

df <- df %>% separate(Date, c("Month","Date"), sep = "/", extra = "drop")

df <- df %>% separate(Time, c("Time","AM/PM"), sep = " ")

df$Time <- as.numeric(gsub(":","",df$Time))

df$Time <- gsub(".{2}$","",df$Time)

df$Time<-as.numeric(df$Time)


df<-df[,-c(1,2,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22,25)]

df$Time[df$`AM/PM`=="PM"]<-df$Time[df$`AM/PM`=="PM"]+1200

df<-df[,-c(4)]

#Shape of the dataframe
shape = as.array(dim(df))

#Find columns with missing Values
colSums(is.na(df))

df <- na.omit(df)




#df$Month <- as.numeric(df$Month)
df$`Primary Type`<- as.factor(df$`Primary Type`)



df$Month <- as.numeric(df$Month)

df$Time[df$Time<50] <- 0

h=50
for(t in 1:23)
{
  df$Time[df$Time>=h & df$Time<(h+100)] <- t
  h=h+100
}

df$Time[df$Time>=2350] <- 0


occurences<-data.frame(table(df[,c("Month","Primary Type")]))

occurencestime <- data.frame(table(df[,c("Primary Type","Time")]))

remove(df)
remove(shape)
remove(h)
remove(t)




ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'crimetype',
                  label = 'Select Crime Type',
                  choices = sort(unique(occurences$Primary.Type),decreasing = FALSE),
                  multiple = TRUE,
                  selected = sort(unique(occurences$Primary.Type),decreasing = FALSE)[1:3]),
      selectInput(inputId = 'month',
                  label = 'Select the Month',
                  choices = sort(unique(occurences$Month),decreasing = FALSE),
                  multiple = TRUE,
                  selected = occurences[1:12,1]),
      selectInput(inputId = 'date',
                  label = "Select a date to see Crime on Map",
                  choices = sort(unique(map.df$Date),decreasing = FALSE),
                  selected = map.df[1,1])
      
    ),
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Frequency of Crime by Month and Type",plotOutput(outputId = "freq")),
                  tabPanel("Location of Crime by Date",leafletOutput(outputId = "map", height=600)),
                  tabPanel("Heatmap",plotOutput("heat"))
      )
    )
    
  )
)
server <- function(input, output,session){
  
  dataset<-reactive({
    subset(occurences, (occurences$Month %in% input$month) & (occurences$Primary.Type %in% input$crimetype))
  })
  
  output$freq <- renderPlot({
    ggplot(dataset(),aes(x=dataset()$Month, y=dataset()$Freq, fill=dataset()$Primary.Type))+
      scale_x_discrete("Months",labels=c("1"="Jan","2"="Feb","3"="Mar","4"="April","5"="May" ,"6"="Jun","7"="July","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec"))+
      ylab("Number of Incidents")+
      ggtitle("Frequency of Crime by Month and Type")+
      labs(fill="Crime Types")+
      theme(plot.title = element_text(lineheight=2, face="bold", size = 25, hjust = 0.5))+
      theme(axis.title.x = element_text(color="steelblue", size=14),axis.title.y = element_text(color="steelblue", size=14))+
      geom_bar(stat = "identity")+
      geom_text(aes(label=dataset()$Freq), vjust=-0.3, size=3.5)
  },height = 600, width = 850)
  
  
  
  
  output$map <- renderLeaflet({
    dummy<- map.df[map.df$Date==input$date,]
    #leaflet(dummy)%>%addTiles()%>% addCircleMarkers(lng = dummy$Longitude, lat = dummy$Latitude, radius = 3, weight = 1)
    factpal<-colorFactor(rgb(t(col2rgb(palette())) / 255),dummy$`Primary Type`)
    leaflet(dummy)%>%addTiles()%>% addCircleMarkers(lng = dummy$Longitude, lat = dummy$Latitude, color = factpal(dummy$`Primary Type`), radius = 3, weight = 5)%>%
      addLegend("topright",pal = factpal,values = dummy$`Primary Type`,title = "Type", opacity = 1)
    
  })
  
  output$heat<-renderPlot({
    ggplot(occurencestime, aes(x = occurencestime$Time, y = occurencestime$Primary.Type)) + geom_tile(aes(fill = occurencestime$Freq))+scale_fill_gradient(name = "Total Crimes", low = "skyblue",high = "slateblue")+
      ggtitle("Heatmap for Type of Crime and Hour of Day")+
      theme(plot.title = element_text(lineheight=2, face="bold", size = 25, hjust = 0.5))+
      xlab("Hour of the day")+
      ylab("Crime Type")+
      theme(axis.title.x = element_text(color="blue", size=14),axis.title.y = element_text(color="blue", size=14))
  }, height = 600, width = 850)
  
}
shinyApp(ui = ui, server = server)













