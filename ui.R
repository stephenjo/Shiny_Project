#filename: ui.R

# ui.R for shiny_test

## ui.R ##


shinyUI(
  dashboardPage(
    dashboardHeader(title = "Citi Bike Data"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Density  Map", tabName = "Dmap", icon = icon("map")),
        menuItem("Trip Map", tabName = "Tmap", icon = icon("map")),
        menuItem("Data", tabName = "data", icon = icon("database"))
      ),
      
      selectizeInput(inputId = "bikeid",
                     label = "Bike ID",
                     choices = sort(unique(ctbk$bikeid))),
      selectizeInput(inputId = "date",
                     label = "Day of travel",
                     choices = (unique(ctbk$startdate))),
      selectizeInput(inputId = "chooseTime",
                     label = "Choose Hour of Day",
                     choices = (hr))
    ),
    
    
    dashboardBody(
      tabItems(
        ## ui.R ##

        tabItem(tabName = "Dmap",
                fluidRow(box(google_mapOutput("Denmap"), height = 420),
                         box(plotOutput("distPlot"),height = 420))),
      
        tabItem(tabName = "Tmap",
              # fluidRow(infoBoxOutput("maxBox"),
              #          infoBoxOutput("minBox"),
              #          infoBoxOutput("avgBox")),
              #mainPanel(google_mapOutput("map"))
              fluidRow((google_mapOutput("Tripmap")))),
        
        tabItem(tabName = "data", # datatable 
                fluidRow(box(DT::dataTableOutput("table"))))
              #box(htmlOutput("hist"), height = 300))
              
              # tabItem(tabName = "map",
              #         # gvisGeoChart
              #         fluidRow(box(htmlOutput("map")), # gvisHistoGram
              #                  box(htmlOutput("hist")))),
              
      )
    )
    
    # dashboardBody(
    #   tabItems(
    #     tabItem(tabName = "map",
    #             # gvisGeoChart
    #             fluidRow(box(htmlOutput("map")), # gvisHistoGram
    #                      box(htmlOutput("hist")))),
    #     
    #     tabItem(tabName = "data", # datatable 
    #             fluidRow(box(DT::dataTableOutput("table"))))#dataTableOutput
    #                         
    #           )
    #              )
  )
)



############### OG CODE THAT WORKS ################################


# fluidPage(
#   titlePanel("Bike Share Data"),
#   sidebarLayout(
#     sidebarPanel(
#       selectizeInput(inputId = "bikeid",
#                      label = "Bike identification",
#                      choices = sort(unique(ctbk$bikeid))),
#       selectizeInput(inputId = "date",
#                      label = "Day of travel",
#                      choices = (unique(ctbk$startdate)))
#     ),
#     mainPanel(google_mapOutput("map"))
#   )
# )