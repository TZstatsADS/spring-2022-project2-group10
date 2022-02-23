#app
rm(list=ls())
library(shiny)
library(readxl)
library(ggplot2)
library(shinythemes)
library(leaflet)
library(lubridate)
library(tidyr)
library(dplyr)
library("rstudioapi")



################################################################################
###################################  UI ########################################
UI <- shinyUI(
  navbarPage(theme = shinytheme("darkly"),title="Park Time",
             id="group 10", windowTitle = "NYC Parks",
             tabPanel("Introduction",
                      fluidRow(
                        tags$img(
                          src="https://pentagram-production.imgix.net/0cf790ca-2e3f-4557-9677-53f176f8f54c/nyc_parks_2019_blog_update.jpg?rect=0%2C59%2C3000%2C1872&w=1500&fit=crop&q=70&fm=jpg&auto=format&h=935&dpr=2",
                          style = "opacity: 0.90", class = "background", height ="80%", width="100%"
                        ),
                        absolutePanel(id = "text", class = "foreground-content",
                                      top = "10%", left = "15%", right = "15%", width = "70%", fixed=FALSE,
                                      draggable = FALSE, height = 200,
                                      fluidRow(
                                        style = "padding: 8%; background-color: dark",
                                        tags$h1("Park Time Introduction", style="color:black;font-weight:bold"),
                                        br(),
                                        tags$p("Because of the widespread COVID-19 pandemic, residents in New York City have few choices to spend time with families and friends outdoors but going to parks. Many people would worry several aspects in term of park: whether the park they want to go to is temporarily closed due to COVID, the park's amenities and park crime stats. 
                                        Some people also want to know the distribution and history of different parks before they go. What can help these people at this very difficult time?", style="font-weight:bold;color:black"),
                                        br(),
                                        tags$p("ParkTime is a tool for people in NYC to know about the information of parks including the opening status, locations, crimes stats and amenities. 
                                               ParkTime will be a great help when people make decisions about which park to go to as it gives the users an expectation of physical distance, potential risks and type of the parks in NYC.", style="font-weight:bold;color:black"),
                                        br(),
                                        tags$p("The following pages show the user interface of Parktime: the history of total crimes in different regions of NYC, the park amenities and the map of parks with different opening status.", style="font-weight:bold;color:black"),
                                      style = "opacity: 0.9")
                        ))),
             
             tabPanel("Park Crimes",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("by", label = "Time Period:",
                                        choices = c("By quarter", "By year")),
                            selectInput("bor", label = "Borough:",
                                        choices = c("BRONX","QUEENS","STATEN ISLAND","BROOKLYN","BROOKLYN/QUEENS","MANHATTAN")),
                          ),
                          mainPanel(
                            p("Total park crime number"),
                            plotOutput(outputId = "ggplot1", width = "95%", height = "300px"),
                            br(),
                            p("Boroughs park crime number"),
                            plotOutput(outputId = "ggplot2", width = "95%", height = "300px")
                          )
                        )
                      )),
             tabPanel("Park Amenities",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("facility", label = "Facility Type:",
                                        choices = c("Adult Exercise Equipment","Athletic Facilities","Comfort Stations","Dog Runs","Playgrounds","Skate Parks")),
                            selectInput("borough", label = "Borough:",
                                        choices = c("BRONX","QUEENS","STATEN ISLAND","BROOKLYN","MANHATTAN")),
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Facilities Descriptions",
                                       br(),
                                       textOutput("text1"),
                                       br(),
                                       plotOutput(outputId = "img1", width = 450, height = 350),
                                       textOutput(outputId = "text2")
                                      
                                      
                              ),
                              tabPanel("Facilities Status",
                                       p("Are the facilities still open?"),
                                       plotOutput(outputId = "ggplot3", width = "95%", height = "280px"),
                                       br(),
                                       p("How long have they been closed?"),
                                       plotOutput(outputId = "ggplot4", width = "95%", height = "280px")
                              ) )
                                          ),
                          
                        )
                      )),
             tabPanel("Map of Parks",
                      div(class="outer map",
                          leafletOutput("map", width="100%", height=620),
                          absolutePanel(id = "choices", class = "panel panel-default",
                                        top = 100, left = 25, width = 250, fixed=FALSE,
                                        draggable = TRUE, height = "auto",
                                        tags$h1("Please Select",
                                                align = "left", style = "font-size:30px"),
                                        selectInput("Month",
                                                    label = "Month",
                                                    choices = c('05/2019','06/2019','10/2019','12/2019','02/2020','03/2020','04/2020','05/2020','06/2020')
                                        ),
                                        tags$h2("Status of Parks",
                                                align = "left",style = "font-size:15px"),
                                        checkboxInput("active",
                                                      label = "Active", value = FALSE),
                                        checkboxInput("reopened",
                                                      label = "Reopened", value = FALSE),
                                        checkboxInput("COVID",
                                                      label = "COVID-19 Closure", value = FALSE),
                                        checkboxInput("construction",
                                                      label = "Under Construction", value = FALSE),
                                        
                                        style = "opacity: 0.80")
                      )),
             navbarMenu("More",
                        tabPanel("Summary",
                                 fluidRow(
                                   tags$img(
                                     src="https://pentagram-production.imgix.net/0cf790ca-2e3f-4557-9677-53f176f8f54c/nyc_parks_2019_blog_update.jpg?rect=0%2C59%2C3000%2C1872&w=1500&fit=crop&q=70&fm=jpg&auto=format&h=935&dpr=2",
                                     style = "opacity: 0.90", class = "background", height ="80%", width="100%"
                                   ),
                                   absolutePanel(id = "text", class = "foreground-content",
                                                 top = "20%", left = "15%", right = "15%", width = "70%", fixed=FALSE,
                                                 draggable = FALSE, height = 200,
                                                 fluidRow(
                                                   style = "padding: 8%; background-color: dark",
                                                   tags$h1("Park Time Summary", style="color:black;font-weight:bold"),
                                                   br(),
                                                   tags$p("In this project, we developed the Park Time app using R shiny, plus some common data analysis and EDA packages such as dplyr, tidyr and ggplot2.", style="font-weight:bold;color:black"),
                                                   tags$p("There are three modules, each can help general users to quickly and efficiently know the NYC park data related to COVID-19 in different aspects.", style="font-weight:bold;color:black")
                                                 ),
                                                 style = "opacity: 0.85")
                                 )),
                        "----",
                        
                        tabPanel("Appendix and References",
                                 fluidRow(
                                   tags$img(
                                     src="https://pentagram-production.imgix.net/0cf790ca-2e3f-4557-9677-53f176f8f54c/nyc_parks_2019_blog_update.jpg?rect=0%2C59%2C3000%2C1872&w=1500&fit=crop&q=70&fm=jpg&auto=format&h=935&dpr=2",
                                     style = "opacity: 0.90", class = "background", height ="80%", width="100%"
                                   ),
                                   absolutePanel(id = "text", class = "foreground-content",
                                                 top = "10%", left = "15%", right = "15%", width = "70%", fixed=FALSE,
                                                 draggable = FALSE, height = 200,
                                                 fluidRow(
                                                   style = "padding: 8%; background-color: dark",
                                                   tags$h2("Data Sources", style="color:black;font-weight:bold"),
                                                   tags$p("For Park Crime Data, visit ", a("NYC Park Crime Data.",
                                                            href = "https://data.cityofnewyork.us/Public-Safety/NYC-Park-Crime-Data/ezds-sqp6",style="font-weight:bold;color:black"), style="font-weight:bold;color:black"),
                                                   tags$p("For Park Amenities Data, visit ", a("NYC Open Data.",
                                                                                           href = "https://nycopendata.socrata.com/browse/select_dataset?Dataset-Information_Agency=Department+of+Parks+and+Recreation+%28DPR%29&provenance=&utf8=",style="font-weight:bold;color:black"), style="font-weight:bold;color:black"),
                                                   tags$p("For Maps of Park Data, visit ", a("NYC Park Open Data.",
                                                                                               href = "https://data.cityofnewyork.us/City-Government/ARCHIVED-Parks-Properties/k2ya-ucmv",style="font-weight:bold;color:black"), style="font-weight:bold;color:black"),
                                                   br(),
                                                   tags$h2("Contributors", style="color:black;font-weight:bold"),
                                                   tags$p("Patricia Song, Statistics Department, Columbia University.",style="font-weight:bold;color:black"),
                                                   tags$p("Krista Zhang, Statistics Department, Columbia University.",style="font-weight:bold;color:black"),
                                                   tags$p("Chang Lu, Statistics Department, Columbia University.",style="font-weight:bold;color:black"),
                                                   
                                                   br(),
                                                   tags$h2("GitHub Repository", style="color:black;font-weight:bold"),
                                                   tags$p("For more details, visit ", a("our GitHub repository.",
                                                                                           href = "https://github.com/TZstatsADS/spring-2022-project2-group10",style="font-weight:bold;color:black"), style="font-weight:bold;color:black")
                                                 ),
                                                 style = "opacity: 0.85")
                                 )))
                        
                        
                        
             
  )
)


################################################################################
############################# Data Processing ##################################


############################## Tag Park Crime

## read data
data.list <- list()
file <- "./data/nyc-park-crime-stats-q"
for(i in 2016:2021){
  for(j in 1:4){
    data.list[[j+(i-2016)*4]] <- read_excel(paste(file,j,"-",i,".xlsx",sep=""),sheet=1,na="NA",skip=3)
  }
}

## data process
#total crime number by time period

total <- rep(NA,24)
time <- rep(NA,24)
for(i in 1:24){
  total[i] <- data.list[[i]]$TOTAL[1155]
}
for(i in 2016:2021){
  for(j in 1:4){
    time[j+(i-2016)*4] <- paste(i,"Q",j,sep = "")
  }
}
totaldf <- data.frame(time = time, total = total)

totaly <- rep(NA,6)
for(i in 1:6){
  totaly[i] <- sum(total[(i-1)*4+1:4])
}
timey <- 2016:2021
totalydf <- data.frame(time = timey, total = totaly)

# crime number by borough by quarter

Br <- rep(NA,24)
Qu <- rep(NA,24)
SI <- rep(NA,24)
Bk <- rep(NA,24)
BrnQu <- rep(NA,24)
Ma <- rep(NA,24)
for(i in 1:24){
  Br[i] <- sum(data.list[[i]][data.list[[i]]$BOROUGH=="BRONX","TOTAL"][[1]][1:length(data.list[[i]][data.list[[i]]$BOROUGH=="BRONX","TOTAL"][[1]])-1])
  Qu[i] <- sum(data.list[[i]][data.list[[i]]$BOROUGH=="QUEENS","TOTAL"][[1]][1:length(data.list[[i]][data.list[[i]]$BOROUGH=="QUEENS","TOTAL"][[1]])-1])
  SI[i] <- sum(data.list[[i]][data.list[[i]]$BOROUGH=="STATEN ISLAND","TOTAL"][[1]][1:length(data.list[[i]][data.list[[i]]$BOROUGH=="STATEN ISLAND","TOTAL"][[1]])-1])
  Bk[i] <- sum(data.list[[i]][data.list[[i]]$BOROUGH=="BROOKLYN","TOTAL"][[1]][1:length(data.list[[i]][data.list[[i]]$BOROUGH=="BROOKLYN","TOTAL"][[1]])-1])
  BrnQu[i] <- sum(data.list[[i]][data.list[[i]]$BOROUGH=="BROOKLYN/QUEENS","TOTAL"][[1]][1:length(data.list[[i]][data.list[[i]]$BOROUGH=="BROOKLYN/QUEENS","TOTAL"][[1]])-1])
  Ma[i] <- sum(data.list[[i]][data.list[[i]]$BOROUGH=="MANHATTAN","TOTAL"][[1]][1:length(data.list[[i]][data.list[[i]]$BOROUGH=="MANHATTAN","TOTAL"][[1]])-1])
}

boroughdf <- data.frame(time=time, BRONX=Br, QUEENS=Qu, STATEN_ISLAND=SI, BROOKLYN=Bk, BROOKLYNQUEENS=BrnQu, MANHATTAN=Ma)

# crime number by borough by year

Bry <- rep(NA,6)
Quy <- rep(NA,6)
SIy <- rep(NA,6)
Bky <- rep(NA,6)
BrnQuy <- rep(NA,6)
May <- rep(NA,6)
for(i in 1:6){
  Bry[i] <- sum(Br[(i-1)*4+1:4])
  Quy[i] <- sum(Qu[(i-1)*4+1:4])
  SIy[i] <- sum(SI[(i-1)*4+1:4])
  Bky[i] <- sum(Bk[(i-1)*4+1:4])
  BrnQuy[i] <- sum(BrnQu[(i-1)*4+1:4])
  May[i] <- sum(Ma[(i-1)*4+1:4])
}

boroughydf <- data.frame(time=timey, BRONX=Bry, QUEENS=Quy, STATEN_ISLAND=SIy, BROOKLYN=Bky, BROOKLYNQUEENS=BrnQuy, MANHATTAN=May)



############################## Tag Park Amenities


# DATA PROCESSING


df_athletic<-read.csv("./data/Parks_Closure_Status_Due_to_COVID-19__Athletic_Facilities.csv",header = T)
df_adult<-read.csv("./data/Parks_Closure_Status_Due_to_COVID-19__Adult_Exercise_Equipment.csv",header = T)
df_comfortstations<-read.csv("./data/Parks_Closure_Status_Due_to_COVID-19__Comfort_Stations.csv",header = T)
df_dogrun<-read.csv("./data/Parks_Closure_Status_Due_to_COVID-19__Dog_Runs.csv",header = T)
df_playground<-read.csv("./data/Parks_Closure_Status_Due_to_COVID-19__Playgrounds.csv",header = T)
df_skatepark<-read.csv("./data/Parks_Closure_Status_Due_to_COVID-19__Skate_Parks.csv",header = T)


data.list2<- list()
data.list2[[1]]<-df_adult
data.list2[[2]]<-df_athletic
data.list2[[3]]<-df_comfortstations
data.list2[[4]]<-df_dogrun
data.list2[[5]]<-df_playground
data.list2[[6]]<-df_skatepark

data.list2[[1]]$fac<-"Adult Exercise Equipment"
data.list2[[2]]$fac<-"Athletic Facilities"
data.list2[[3]]$fac<-"Comfort Stations"
data.list2[[4]]$fac<-"Dog Runs"
data.list2[[5]]$fac<-"Playgrounds"
data.list2[[6]]$fac<-"Skate Parks"


# change names of boroughs and add columns of closure length
for (i in 1:6){
  data.list2[[i]]$Borough<-ifelse(data.list2[[i]]$Borough=="X","BRONX",
                                  ifelse(data.list2[[i]]$Borough=="B","BROOKLYN",
                                         ifelse(data.list2[[i]]$Borough=="R","STATEN ISLAND",
                                                ifelse(data.list2[[i]]$Borough=="M","MANHATTAN",
                                                       ifelse(data.list2[[i]]$Borough=="Q","QUEENS",NA)))))
  
  data.list2[[i]]$timeclosed<-difftime((as.POSIXct(data.list2[[i]]$Approx.Date.Reopened,'%m/%d/%Y %I:%M:%S',tz="")),(as.POSIXct(data.list2[[i]]$Approx.Date.Closed,'%m/%d/%Y %I:%M:%S',tz="")),units = "days")
}





data3<-NA
for (i in 1:6) {
  dtmp<-as.data.frame(cbind(data.list2[[i]]$Borough,data.list2[[i]]$Status,data.list2[[i]]$fac))
  colnames(dtmp)<-c("Borough","Status","Facilities")
  data3<-rbind(data3,dtmp)
}
df<-data3%>%
  group_by(Facilities,Status,Borough)%>%
  count

selectfac <- function(facility,borough){
  na.omit(df[df$Facilities==facility & df$Borough==borough,])
}

df_all<-rbind(data.list2[[1]][c('Borough','timeclosed','fac')],
              data.list2[[2]][c('Borough','timeclosed','fac')],
              data.list2[[3]][c('Borough','timeclosed','fac')],
              data.list2[[4]][c('Borough','timeclosed','fac')],
              data.list2[[5]][c('Borough','timeclosed','fac')],
              data.list2[[6]][c('Borough','timeclosed','fac')])

df_all$timeclosed<-ifelse(df_all$timeclosed<0,0,df_all$timeclosed)
selectdata2<-function(facility,borough){
  na.omit(df_all[((df_all$fac==facility) & (df_all$Borough==borough)),])
}
  
# Process map Data
location_raw <- read.csv("./data/Parks_Properties.csv")
location <- location_raw %>% select(park_name = NAME311, Longitude=longitude,
                                    Latitude = latitude )
location$Longitude <- as.numeric(location$Longitude)
location$Latitude <- as.numeric(location$Latitude)
location %>%na.omit()

# ##########
Adult_Exercise_Equipment_raw <- read.csv("./data/Parks_Closure_Status_Due_to_COVID-19__Adult_Exercise_Equipment.csv")
Athletic_Facilities_raw <- read.csv("./data/Parks_Closure_Status_Due_to_COVID-19__Athletic_Facilities.csv")
Comfort_Stations_raw <- read.csv("./data/Parks_Closure_Status_Due_to_COVID-19__Comfort_Stations.csv")
Dog_Runs_raw <- read.csv("./data/Parks_Closure_Status_Due_to_COVID-19__Dog_Runs.csv")
Playgrounds_raw <- read.csv("./data/Parks_Closure_Status_Due_to_COVID-19__Playgrounds.csv")
Skate_Parks_raw <- read.csv("./data/Parks_Closure_Status_Due_to_COVID-19__Skate_Parks.csv")

park_status = dplyr::bind_rows(Adult_Exercise_Equipment_raw,
                               Athletic_Facilities_raw,
                               Comfort_Stations_raw,
                               Dog_Runs_raw,
                               Playgrounds_raw,
                               Skate_Parks_raw)

park_status = park_status %>%
  select(park_name= PropertyName, Status= Status, Approx_Date_Closed = Approx.Date.Closed, Approx_Date_Reopened=Approx.Date.Reopened)

park_status$Approx_Date_Closed <-as.Date(park_status$Approx_Date_Closed, '%m/%d/%Y')
park_status$Approx_Date_Closed <- as.POSIXct(park_status$Approx_Date_Closed, format = "%m/%d/%Y")
park_status$Approx_Date_Closed <-format(park_status$Approx_Date_Closed, format="%m/%Y")

park_status$Approx_Date_Reopened <-as.Date(park_status$Approx_Date_Reopened, '%m/%d/%Y')
park_status$Approx_Date_Reopened <- as.POSIXct(park_status$Approx_Date_Reopened, format = "%m/%d/%Y")
park_status$Approx_Date_Reopened <- format(park_status$Approx_Date_Reopened, format="%m/%Y")

park_status = park_status %>%
  arrange(my(park_status$Approx_Date_Closed))%>%
  replace_na(list(Approx_Date_Closed = "11/2019"))

park<-merge(x=park_status,y=location,by="park_name",all.x=TRUE)

park %>%drop_na(Longitude)

selectPark <- function(month, Status){
  park[(park$Approx_Date_Closed == month & park$Status == Status), ]
}



################################################################################
############################### Shiny Server ###################################

Server <- shinyServer(function(input, output) {
  
  output$ggplot1 <- renderPlot({
    
    if(input$by=="By quarter"){
      p <- ggplot(data = totaldf, aes(x=time,y=total,fill=total))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number by quarter", x = "", y = "Crime Number")+
        geom_vline(xintercept = 12.7, colour = "orange", size = 1.5)+
        annotate("text", x = 14, y = 1.1*max(total), label = "Covid start", color = "orange")
      print(p)
    }
    
    if(input$by=="By year"){
      p <- ggplot(data = totalydf, aes(x=time,y=total,fill=total))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number by year", x = "", y = "Crime Number")+
        geom_vline(xintercept = 2018.5, colour = "orange", size = 1.5)+
        annotate("text", x = 2018.85, y = 1.1*max(totaly), label = "Covid start", color = "orange")
      print(p)
    }
  })
  
  output$ggplot2 <- renderPlot({
    
    if(input$bor=="BRONX" & input$by=="By quarter"){
      p <- ggplot(data = boroughdf, aes(x=time,y=BRONX,fill=BRONX))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number of BRONX by quarter", x = "", y = "Crime Number")+
        geom_vline(xintercept = 12.7, colour = "orange", size = 1.5)+
        annotate("text", x = 14, y = 1.1*max(boroughdf$BRONX), label = "Covid start", color = "orange")
      print(p)
    }
    
    if(input$bor=="QUEENS" & input$by=="By quarter"){
      p <- ggplot(data = boroughdf, aes(x=time,y=QUEENS,fill=QUEENS))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number of QUEENS by quarter", x = "", y = "Crime Number")+
        geom_vline(xintercept = 12.7, colour = "orange", size = 1.5)+
        annotate("text", x = 14, y = 1.1*max(boroughdf$QUEENS), label = "Covid start", color = "orange")
      print(p)
    }
    
    if(input$bor=="STATEN ISLAND" & input$by=="By quarter"){
      p <- ggplot(data = boroughdf, aes(x=time,y=STATEN_ISLAND,fill=STATEN_ISLAND))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number of STATEN ISLAND by quarter", x = "", y = "Crime Number")+
        geom_vline(xintercept = 12.7, colour = "orange", size = 1.5)+
        annotate("text", x = 14, y = 1.1*max(boroughdf$STATEN_ISLAND), label = "Covid start", color = "orange")
      print(p)
    }
    
    if(input$bor=="BROOKLYN" & input$by=="By quarter"){
      p <- ggplot(data = boroughdf, aes(x=time,y=BROOKLYN,fill=BROOKLYN))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number of BROOKLYN by quarter", x = "", y = "Crime Number")+
        geom_vline(xintercept = 12.7, colour = "orange", size = 1.5)+
        annotate("text", x = 14, y = 1.1*max(boroughdf$BROOKLYN), label = "Covid start", color = "orange")
      print(p)
    }
    
    if(input$bor=="BROOKLYN/QUEENS" & input$by=="By quarter"){
      p <- ggplot(data = boroughdf, aes(x=time,y=BROOKLYNQUEENS,fill=BROOKLYNQUEENS))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number of BROOKLYN/QUEENS by quarter", x = "", y = "Crime Number")+
        geom_vline(xintercept = 12.7, colour = "orange", size = 1.5)+
        annotate("text", x = 14, y = 1.1*max(boroughdf$BROOKLYNQUEENS), label = "Covid start", color = "orange")
      print(p)
    }
    
    if(input$bor=="MANHATTAN" & input$by=="By quarter"){
      p <- ggplot(data = boroughdf, aes(x=time,y=MANHATTAN,fill=MANHATTAN))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number of MANHATTAN by quarter", x = "", y = "Crime Number")+
        geom_vline(xintercept = 12.7, colour = "orange", size = 1.5)+
        annotate("text", x = 14, y = 1.1*max(boroughdf$MANHATTAN), label = "Covid start", color = "orange")
      print(p)
    }
    
    if(input$bor=="BRONX" & input$by=="By year"){
      p <- ggplot(data = boroughydf, aes(x=time,y=BRONX,fill=BRONX))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number of BRONX by year", x = "", y = "Crime Number")+
        geom_vline(xintercept = 2018.5, colour = "orange", size = 1.5)+
        annotate("text", x = 2018.85, y = 1.1*max(boroughydf$BRONX), label = "Covid start", color = "orange")
      print(p)
    }
    
    if(input$bor=="QUEENS" & input$by=="By year"){
      p <- ggplot(data = boroughydf, aes(x=time,y=QUEENS,fill=QUEENS))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number of QUEENS by year", x = "", y = "Crime Number")+
        geom_vline(xintercept = 2018.5, colour = "orange", size = 1.5)+
        annotate("text", x = 2018.85, y = 1.1*max(boroughydf$QUEENS), label = "Covid start", color = "orange")
      print(p)
    }
    
    if(input$bor=="STATEN ISLAND" & input$by=="By year"){
      p <- ggplot(data = boroughydf, aes(x=time,y=STATEN_ISLAND,fill=STATEN_ISLAND))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number of STATEN ISLAND by year", x = "", y = "Crime Number")+
        geom_vline(xintercept = 2018.5, colour = "orange", size = 1.5)+
        annotate("text", x = 2018.85, y = 1.1*max(boroughydf$STATEN_ISLAND), label = "Covid start", color = "orange")
      print(p)
    }
    
    if(input$bor=="BROOKLYN" & input$by=="By year"){
      p <- ggplot(data = boroughydf, aes(x=time,y=BROOKLYN,fill=BROOKLYN))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number of BROOKLYN by year", x = "", y = "Crime Number")+
        geom_vline(xintercept = 2018.5, colour = "orange", size = 1.5)+
        annotate("text", x = 2018.85, y = 1.1*max(boroughydf$BROOKLYN), label = "Covid start", color = "orange")
      print(p)
    }
    
    if(input$bor=="BROOKLYN/QUEENS" & input$by=="By year"){
      p <- ggplot(data = boroughydf, aes(x=time,y=BROOKLYNQUEENS,fill=BROOKLYNQUEENS))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number of BROOKLYN/QUEENS by year", x = "", y = "Crime Number")+
        geom_vline(xintercept = 2018.5, colour = "orange", size = 1.5)+
        annotate("text", x = 2018.85, y = 1.1*max(boroughydf$BROOKLYNQUEENS), label = "Covid start", color = "orange")
      print(p)
    }
    
    if(input$bor=="MANHATTAN" & input$by=="By year"){
      p <- ggplot(data = boroughydf, aes(x=time,y=MANHATTAN,fill=MANHATTAN))+
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "light blue", high = "dark blue") +
        theme(axis.text.x = element_text(angle = -80, hjust = 1, vjust = .5), 
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Total park crime number of MANHATTAN by year", x = "", y = "Crime Number")+
        geom_vline(xintercept = 2018.5, colour = "orange", size = 1.5)+
        annotate("text", x = 2018.85, y = 1.1*max(boroughydf$MANHATTAN), label = "Covid start", color = "orange")
      print(p)
    }
    
  })
  
  # output$ggplot3 <- renderPlot({
  #   if(input$factype=="Athletic" & input$bor2=="BRONX"){
  #     p <- ggplot(data.list2[[2]][data.list2[[2]]$Borough=="BRONX",])+
  #       geom_bar(aes(Status,fill=Borough))+
  #       theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  #       labs(title = "the Status of Athletic Facilities in Bronx",x="Status")+
  #       scale_fill_manual("Borough", values = c( "BRONX" = "light blue"))
  #     print(p)
  #   }
  #   if(input$factype=="Adult" & input$bor2=="BRONX"){
  #     p <- ggplot(data.list2[[1]][data.list2[[1]]$Borough=="BRONX",])+
  #       geom_bar(aes(Status,fill=Borough))+
  #       theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  #       labs(title = "the Status of Adult Exercise Equipment in Bronx",x="Status")+
  #       scale_fill_manual("Borough", values = c( "BRONX" = "blue"))
  #     print(p)
  #   }
  #   
  # })
  
  output$text1<- renderText({
    paste(input$facility)
  })
  
  output$img1<-renderImage({
  
      filename <- ifelse(input$facility=="Adult Exercise Equipment",'./www/adult.png',
                         ifelse(input$facility=="Athletic Facilities",'./www/athletic.png',
                                ifelse(input$facility=="Comfort Stations",'./www/comfortstation.png',
                                       ifelse(input$facility=="Dog Runs",'./www/dogruns.png',
                                              ifelse(input$facility=="Playgrounds",'./www/playground.png','./www/skate.png')))))
      list(src = filename,
           contentType = 'image/png',
           width = 400,
           height = 310,
           alt = "This is alternate text")
    },deleteFile = FALSE
  )
  
  output$text2<- renderText({
    
    if (input$facility=="Playgrounds") {"Combination of Children's Play Areas"}
    if (input$facility=="Athletic Facilities") {"Including basketball, bocce, handball, netball, tennis, volleyball, softball, soccer, baseball, track, cricket, MPPAs, football, hockey, and rugby sites."}
  
  })
  output$ggplot3 <- renderPlot({
  ggplot(selectfac(input$facility,input$borough),aes(x=Status,y=n,fill=n,legend=Borough))+
    geom_bar(stat="identity")+
    scale_fill_gradient(low = "light blue", high = "dark blue") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = .5), 
          plot.title = element_text(hjust = 0.5)) +
    labs(title = paste("Status of",input$facility,"in",input$borough), x = "Status", y = "Numbers")
    
    })
  
 
  output$ggplot4 <- renderPlot({
    ggplot(selectdata2(input$facility,input$borough))+
      geom_density(aes(timeclosed,fill=Borough))+
      scale_fill_discrete(guide="none")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1), 
            plot.title = element_text(hjust = 0.5))+
      labs(title = paste("the Number of Days",input$facility," closed in", input$borough,"Due to Covid-19"),x="Closed Length (Days)")
   
    
  }) 
  
  ################ Tab 2 Interactive Maps ###################
  # NYC map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender(
        "function(el, x) {
                    L.control.zoom({ position: 'bottomright' }).addTo(this)
                }"
      ) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      setView(lng = -73.935250, lat = 40.730640, zoom = 10)
  })
  #
  #
  df_active_park <- reactive({
    selectPark(input$Month,"Active")
  })

  df_reopened_park <- reactive({
    selectPark(input$Month,"Reopened")
  })

  df_COVID_Closure_park <- reactive({
    selectPark(input$Month,"COVID-19 Closure")
  })

  df_Construction_park <- reactive({
    selectPark(input$Month,"UnderConstruction")
  })

  observe({
    leafletProxy("map", data = df) %>%
      clearShapes() %>%
      clearMarkers() %>%
      addProviderTiles("CartoDB.Voyager") %>%
      fitBounds(-74.354580, 40.919521, -73.761530, 40.520016)


    if (input$Month != '') {
      if (input$active){
        leafletProxy("map", data = df_active_park()) %>%
          addCircleMarkers(
            lng=~Longitude,
            lat=~Latitude,
            color = 'yellow',
            stroke = FALSE,
            fillOpacity = 0.3
          )
      }

      if (input$reopened){
        leafletProxy("map", data = df_reopened_park()) %>%
          addCircleMarkers(
            lng=~Longitude,
            lat=~Latitude,
            color = 'green',
            stroke = FALSE,
            fillOpacity = 0.3
          )
      }

      if (input$COVID){
        leafletProxy("map", data = df_COVID_Closure_park()) %>%
          addCircleMarkers(
            lng=~Longitude,
            lat=~Latitude,
            color = 'red',
            stroke = FALSE,
            fillOpacity = 0.3
          )
      }

      if (input$construction){
        leafletProxy("map", data = df_Construction_park()) %>%
          addCircleMarkers(
            lng=~Longitude,
            lat=~Latitude,
            color = 'blue',
            stroke = FALSE,
            fillOpacity = 0.3
          )
      }



    }
  })
})



shinyApp(ui = UI, server = Server)

