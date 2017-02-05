library(shiny)
library(DT)
library(formattable)
library(dplyr)
library(shinydashboard)
library(plotly)
library(devtools)
library(xts)
library(data.table)
library(leaflet)
dashboardPage(
              dashboardHeader(
                title = div(img(src ="ab4.jpg",height = 60, width = 220,align = "left"
                                ))),
           
              dashboardSidebar( 
  
########## changing sidebar length #############          
tags$head(
  tags$style(HTML("
                  .sidebar { height: 250vh; overflow-y: scroll; }
                  " )
  )
  ),
              tags$head(tags$style(".rightAlign{float:right;}")),
                        
                                sliderInput("year","Select last 'X' Years of Data",0,5,1,1),

                                 # submitButton("Submit"),
                                 uiOutput("Prod"),
                                 uiOutput("Segment"),
                                 uiOutput("Subsegment")
                            #     )
               
               
              ),
              dashboardBody(
              

####### changing table appearance  ################ 
                tags$head(tags$style(
                  HTML('
                       .box.box-solid.box-primary>.box-header {
                       color:#fff;
                       background:#A60303
                       }
                       
                       .box.box-solid.box-primary{
                       border-bottom-color:#4fbccb;
                       border-left-color:#4fbccb;
                       border-right-color:#4fbccb;
                       border-top-color:#4fbccb;
                       }
                       
                       .skin-blue .main-header .logo {
                       background-color: #F2B705;
                       }
                       
                       .skin-blue .main-header .navbar {
                       background-color: #F2B705;
                       } 
                       .skin-blue .main-sidebar {
                       background-color: #F2B705;
                       }
                       
                       body, label, input, button, select { 
                       font-family: "Arial";
                       }')
                )),
                
             
              
  
                           fluidRow(
                             box(status="primary",solidHeader = TRUE,uiOutput("para1"),width = 6),
                             box(status="primary",solidHeader = TRUE,uiOutput("plot1"),width = 6)),
                            
                           fluidRow(
                             box(title = "Performance",status="primary",solidHeader = TRUE,
                             plotlyOutput("sales_plot"),width=12,color= '#A60303'),
                             # box(dataTableOutput("sale"),width = 12)
                             fluidRow(
                               box(status="primary",solidHeader = TRUE,uiOutput("plot2"),width = 6),
                               box(status="primary",solidHeader = TRUE,uiOutput("plot3"),width = 6))
                             
                             
                             )
                             ))
                  
                  
                

