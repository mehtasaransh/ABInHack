server <- shinyServer(function(input, output,session){ 



                                  output$Prod <- renderUI({
                                     selectizeInput(  
                                      inputId = "product",
                                       label = "Select Product",
                                       choices = as.character(unique(raw_data$Product)),
                                      selected = raw_data$Product[1], 
                                       multiple = T )
                                   })
  

                                  
############### reactive functions for select and deselect ########                                    
                                selection <- reactive({
                                  available <- (raw_data)[raw_data$Product == input$product,]
                                 
                                   })
 
                              selection1 <- reactive({
                               av <- selection()[selection()$AB.Segment.Value== input$AB_segment,]
                               })
                              
                              selection2 <- reactive({
                        
                                av <- selection1()[selection1()$AB.Subsegment.Value == input$AB_subsegment,]
                              })
  
                              
                              segment_selection <- reactive ({
                                 available1 <- c("Select All","de",(as.character(selection()$AB.Segment.Value))) 
                               })
  
                              subsegment_selection <- reactive({
                                 available2 <- c("Select All","de",(as.character(selection1()$AB.Subsegment.Value)))
                               })
  
                             

                             output$Segment <- renderUI({  
                                
                               selectizeInput(
                               inputId = "AB_segment", 
                               label = "Select AB segment value",
                               choices = (segment_selection()),
                              selected = segment_selection()[1],
                              multiple = T)
                              })
 
                            output$Subsegment <- renderUI({    
                              
                             selectInput(
                              inputId = "AB_subsegment", 
                              label = "Select AB Subsegment value",
                              choices = (subsegment_selection()),
                              selected = subsegment_selection()[1],
                              multiple = T )
                            })
 
  
  
########  dimensions to be grouped by  ##########  
                          output$para1 <- renderUI({
    
                            selectInput("par","Parameters", 
                            choices  = c("AB.Segment.Value","AB.Subsegment.Value","Package.Value","Brewer.Value"),############## incomplete
                            selected = "AB.Segment.Value"
                              ,multiple= TRUE)
                          })
                              
                              output$plot1 <- renderUI({
                                
                                selectInput("xaxis","X-Axis", 
                                            choices  = c("AB.Segment.Value","AB.Subsegment.Value","Package.Value","Brewer.Value")############## incomplete
                                            ,multiple= FALSE)
                              })    
                              
                              output$plot2 <- renderUI({
                                
                                selectInput("yaxis1","Primary Y-Axis", 
                                            choices  = c("Sales","Volume.Sales")
                                            ,multiple= FALSE)
                              })
                              
                              output$plot3 <- renderUI({
                                
                                selectInput("yaxis2","Secondary Y-Axis", 
                                            choices  = c("Volume.Sales","Sales")
                                            ,multiple= FALSE)
                              })

   
                         
                              
                              
                              
                            observe({
                                      if ("Select All" %in% input$AB_segment) {
            ###### choose all the choices _except_ "Select All"#######
                              selected_choices <- setdiff(segment_selection(), c("Select All","de"))
                              updateSelectInput(session, "AB_segment", selected = selected_choices)
                                }
                                     else if ("de" %in% input$AB_segment) {
           ####### choose all the choices _except_ "Select All"########
                              selected_choices <- setdiff(segment_selection(), segment_selection()) 
                              updateSelectInput(session, "AB_segment", selected = selected_choices)
                              }
                              }) 
  

  
                             observe({
                                      if ("Select All" %in% input$AB_subsegment) {
           ####### choose all the choices _except_ "Select All"
                              selected_choices <- setdiff(subsegment_selection(), c("Select All","de"))
                              updateSelectInput(session, "AB_subsegment", selected = selected_choices)
                              }
                                     else if ("de" %in% input$AB_subsegment) {
           ######### choose all the choices _except_ "Select All"
                              selected_choices <- setdiff(subsegment_selection(), subsegment_selection()) 
                              updateSelectInput(session, "AB_subsegment", selected = selected_choices)
                              }
                              })   
  

                              
              sales <- reactive({
                        data_new<- raw_data[raw_data$Product == input$product,]
                        data_new<- data_new[data_new$AB.Segment.Value == input$AB_segment,]  
                        data_new<- data_new[data_new$AB.Subsegment.Value == input$AB_subsegment,]

                     
                        dates <- max(raw_data$Date)
                        data_new <- as.data.frame(data_new)
                         data_new <- data_new[data_new$Date %in% mapply(`:`, from=dates, to=dates-(((input$year)*365)-1)),]
                        data_new <- na.omit(data_new)
                        
                        
                        data_new <- na.omit(data_new) %>% 
                                  group_by_("Product",input$par[1],input$par[2],input$par[3],input$par[4]) %>% 
                                   summarise(Sales = sum(Sales),Volume.Sales = sum(Volume.Sales))
                        
                        data_new <- data_new %>% 
                          group_by_(input$xaxis) %>% 
                          summarise(Sales = sum(Sales),Volume.Sales = sum(Volume.Sales))
                        
                        plot.df <-  data_new[,c(input$xaxis,input$yaxis1,input$yaxis2)]
                        colnames(plot.df) <- c("x","y1","y2")

                        plot.df <- plot.df[order(-plot.df$y1),]
                        plot.df <- head(plot.df,10)
                        plot.df <- droplevels(plot.df)


                        m <- list(
                          l = 50,
                          # r = 50,
                          r = 70,
                          b = 100,
                          t = 100,
                          pad = 4
                        )
                        ay <- list(
                          tickfont = list(color = "black"),
                          #showline = TRUE,

                          overlaying = "y1",
                          side = "right"
                        )


                        plot <-  plot_ly(plot.df) %>%

                          add_bars( x = ~x, y = ~y1, yaxis = "y1", showlegend = F,
                                    marker = list(color = "#4fbecb"), name = input$yaxis1) %>%

                          add_lines(x = ~x, y = ~y2, yaxis = "y2", line = list(color = "#20b520"), name = input$yaxis2) %>%
                          layout(yaxis2=ay) %>%
                          #
                          layout(title = paste(input$yaxis1,",",input$yaxis2,"vs", input$xaxis)) %>%
                          #
                          layout(xaxis = list(title = input$xaxis,
                                              showgrid = F, ticklen = 4, ticks = "inside",
                                              domain = c(0, 0.9)),
                                 yaxis = list(title = input$yaxis1, gridwidth = 2, domain = c(0, 0.9)),
                                 yaxis2 = list(title = input$yaxis2, overlaying = "y1", side = "right", showgrid = F)) %>%
                          layout(showlegend = FALSE)

                         print(data_new)
                         return(plot)
                        # return(data_new)
                        
                            
                        })
                              
                              
                             
                              output$sales_plot <- renderPlotly({
                                
                                sales()
                                
                              })

})