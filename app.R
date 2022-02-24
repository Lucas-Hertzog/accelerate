library(shiny)
library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(DT)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

world_accelerators <-readRDS("world_accelerators.RDS")

# Define UI for application
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Accelerators Dashboard (Beta Version)</a>'), id="nav",
             windowTitle = "Accelerators Dashboard (Beta Version)",
      
      tabPanel("Map",
               div(class="outer",
                   tags$head(includeCSS("styles.css")),
                   leafletOutput("accmap", width="100%", height="100%"))
               ),
      
      tabPanel("Data", DT::dataTableOutput("data")),
      
      
      tabPanel("Costing (Policy Maker Module)", 
               sidebarLayout(
                 sidebarPanel(
                   
                    pickerInput("country_select", "Country:",   
                               choices = as.character(wp2[order(wp2$SOVEREIGNT),]$SOVEREIGNT), 
                               options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                               selected = as.character(wp2[order(wp2$SOVEREIGNT),]$SOVEREIGNT),
                               multiple = FALSE),

                    pickerInput("acc_select", "Accelerator",   
                             choices = as.character(wp2[order(wp2$world_all_N_Accelerators),]$world_all_N_Accelerators), 
                             options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                             selected = as.character(wp2[order(wp2$world_all_N_Accelerators),]$world_all_N_Accelerators)[1:3],
                             multiple = TRUE),
                    
                    sliderInput("format", "How many adolescents would you like to support? (in millions)",
                                min = 0, max = 540,
                                value = 0, step = 10,
                                pre = "N=", sep = ",",
                                animate = TRUE),
                    
                    sliderInput("format", "For how long?",
                                min = 1, max = 10,
                                value = 1, step = 1,
                                pre = "Years", sep = ",",
                                animate = TRUE),

                 ),
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Plot", includeMarkdown("cost.rmd"))))
                   )
               ),
      
      tabPanel("Costing (Donor Module)", 
               sidebarLayout(
                 sidebarPanel(
                   
                   sliderInput("format", "How much would you like to invest?",
                               min = 0, max = 10000000,
                               value = 10000, step = 100000,
                               pre = "US$", sep = ",",
                               animate = TRUE),
                   
                   pickerInput("sex_select", "Would like to target your investment for girls or boys?",   
                               choices = c("Girls", "Boys", "Both"), 
                               selected = c("Girls"),
                               multiple = FALSE),
                   
                 ),
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("List of possible investments by country", includeMarkdown("cost2.rmd"))))
               )
      ),
     
     
      tabPanel("Submit an Accelerator",  
               span(tags$i(h3("Our team of researchers will analyse your study and get back to you as soon as possible. Please include a published manuscript and additional information you consider to be relevant in a single PDF file")), style="color:#045a8d"),
               
               fileInput(
                 "file", label = h4("Submit your study to join our database")),
               hr(),
               fluidRow(column(4, 
                               verbatimTextOutput("value")))),
      
      tabPanel("References",includeMarkdown("references.rmd")),
      
      tabPanel("Contact Us",includeMarkdown("contact.rmd"))
  )
)


      



#Define server logic
server <- function(input, output) {

  #colour palette
  pal <- colorNumeric("Blues", 
                      world$world_all_N_Accelerators,
                      na.color = "#808080",
                      alpha = FALSE,
                      reverse = FALSE)
  
  #pop-ups
  world$popup <- paste("<strong>",world$SOVEREIGNT,"</strong>", "</br>", "</br>",
                       "Total Accelerators Identified:", prettyNum(world$world_all_N_Accelerators, big.mark = ","), 
                       "</br>","</br>", world$world_all_Accelerators)
  
  NoDataAfrica$popup <- paste("<strong>",NoDataAfrica$SOVEREIGNT,"</strong>", "</br>",
                              "No Data Available (yet)")
  
  #leaflet map
  output$accmap <- renderLeaflet({
      leaflet(wp2) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Basemap - greyscale") %>%
      addPolygons(data = wp2,
                  stroke = TRUE, 
                  weight = 0.2,
                  color = "#ABABAB",
                  smoothFactor = 0.2, #add a smoothing factor
                  opacity = 1,
                  popup = ~world$popup,
                  fillColor = ~accPal(world_all_N_Accelerators),
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "#1DAC88", weight = 2.5, 
                                                      fillOpacity = 0.8), #add hover effect
                  group = "Accelerators Library")%>% #add groups
      
      addPolygons(data = NoDataAfrica,
                  stroke = TRUE, 
                  weight = 0.2,
                  color = "#ABABAB",
                  smoothFactor = 0.3, #add a smoothing factor
                  opacity =0.8,
                  fillColor = "#ABABAB",
                  fillOpacity = 0.45,
                  highlightOptions = highlightOptions(color = "#1DAC88", weight = 2.5, 
                                                      fillOpacity = 0.5), #add hover effect
                  popup = NoDataAfrica$popup, 
                  group = "No Data")%>% 
      
      #Create legends
      addLegend("bottomright",
                pal = accPal, values = world$world_all_N_Accelerators,
                opacity = 1,
                title = "Number of Accelerators",
                labels= c("1-2", "3", "4", "5+")
      )%>%
      
      #Set layer visibility control
      addLayersControl(
        overlayGroups = c("Accelerators Library"),
        options = layersControlOptions(collapsed = FALSE))%>% 
      
      #Set which layers should initially not be visible
      #hideGroup("VACS") %>%
      
      #Add buttons to zoom to location/world
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1) }"))) %>% # ; removed after (1)
      
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Locate Me",
        onClick = JS("function(btn, map){map.locate({setView: TRUE}) }") # ; removed after TRUE}) 
        
      ))
    })
  
  #data panel
  output$data <-DT::renderDataTable(datatable(
    data[,c(1,2,3,4)],filter = 'top',
    colnames = c("Country", "Continent", "N Accelerators", "Accelerators")))
  
  #submit a file panel
  output$file <- renderPrint({
    str(input$file)
    

  }
  )
}

# Run the application 
shinyApp (ui = ui, server = server)