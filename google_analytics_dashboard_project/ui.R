source("global.R")

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel(
    title = tags$h1("Dream Team Project")
  ),
  tabsetPanel(
    tabPanel("User Arrival",
             
             sidebarLayout(
               
               sidebarPanel(
                 tags$h5("Select an area of a graph to zoom in. Double click to reset."),
                 # adds keyword options in checkbox format
                 checkboxGroupInput("keyword", "Select Keywords",
                                    c("Company" = "company", 
                                      "Data" = "data", 
                                      "Event" = "event", 
                                      "Job" = "job", 
                                      "Podcast" = "podcast", 
                                      "Programming" = "programming", 
                                      "Taster" = "taster"), 
                                    selected = c("company", 
                                                 "data", 
                                                 "event", 
                                                 "job", 
                                                 "podcast", 
                                                 "programming", 
                                                 "taster")
                 ),
                 
                 checkboxGroupInput("advert", "Select Advert Content", 
                                    c("Company" = "company", 
                                      "Coding" = "coding", 
                                      "Data" = "data", 
                                      "Developer" = "developer",
                                      "Programming" = "programming",
                                      "Software" = "software",
                                      "UX" = "ux", 
                                      "Web" = "web"),
                                    selected = c("company", 
                                                 "coding", 
                                                 "data", 
                                                 "developer", 
                                                 "programming",
                                                 "software", 
                                                 "ux", 
                                                 "web")
                                    )
                 
               ),
               
               
               mainPanel(
                 # creating grid to improve readability
                 fluidRow(
                   column(6,
                          tags$h4("Search Engine Keywords")),
                   
                   column(6, 
                          tags$h4("Most Popular Search Engines"))
                 ),
                 
                 fluidRow(
                   column(6,
                          plotlyOutput("keyword_plot")),
                   
                   column(6,
                          plotlyOutput("search_engine_plot"))
                   
                 ),
                 
                 fluidRow(
                   column(6,
                          tags$h4("Clicks by Advert Type")),
                   
                   column(6,
                          tags$h4("Clicks by Social Media Company"))
                 ),
                 
                 fluidRow(
                   
                   column(6,
                          plotlyOutput("advert_plot")),
                   
                   
                   column(6,  
                          plotlyOutput("social_media_plot"))
                   
                 )
                 
                 
               )
               
             )
    ),
    
    
    tabPanel(
      "Goal Conversions",
      
      sidebarLayout(
        
        sidebarPanel(
          fluidRow(
            # Adding a date range input so that the data can be filtered by date.
            dateRangeInput(
              "date_range",
              label = tags$h5("Date Range"),
              format = "dd-mm-yyyy",
              start = "2019-09-01",
              end = "2020-02-29",
              min = "2019-09-01",
              max = "2020-02-29",
              startview = "year",
              separator = " - "
            )
          ),
          
          
          # Here are some instructions so that the user knows about and can use all of the interactivity features
          fluidRow(
            "Please select dates to see graphs."
          ),
          
          fluidRow(
            "Please hover over the bars to see the page URL.",
            "Select an area of a graph to zoom in. Double click to reset."
          ),
          
          
          # An interactivity selector so that the user an select for a printer friendly version of the dashboard. 
          fluidRow(
            selectInput(
              "interactivity",
              label = tags$h5("View"),
              choices = c("Interactive", "Printer Friendly"),
              selected = "Interactive"
            ) 
          )
        ),
        
        mainPanel(
          
          fluidRow(
            # This row is adding titles for the graphs that will be underneath it.
            column(6,
                   #Title for page of goal completion graph.
                   tags$h4("Page of Goal Completion")
                   
                   
            ),
            
            column(6,
                   # Title for number of pages visited before goal completion graph.
                   tags$h4("Number of Pages Visited before Goal Completion")
                   
                   
            )
          ),
          
          fluidRow(
            # This row is for the graphs that correspond to the titles above. 
            column(6,
                   # This is the conditional output for the page of goal completion graph, which shows which page
                   # a customer was on when they booked an event.
                   conditionalPanel(
                     condition = "input.interactivity == 'Interactive'", 
                     plotlyOutput("page_of_completion")
                   ),
                   
                   conditionalPanel(
                     condition = "input.interactivity == 'Printer Friendly'",
                     plotOutput("page_of_completion_gg")
                   ) 
                   
                   
                   
            ),
            
            column(6,
                   # This graph shows how many pages users visited before making a booking. 
                   plotlyOutput("pages_visited")
                   
            )
            
          ),
          
          fluidRow(
            # This row is for my second row of images. Probably a graph and a table.
            column(6,
                   # Title for the Page Visited Immediately Before Goal Completion Page.
                   tags$h4("Page Before Goal Completion"),
                   tags$h6("Top 10 pages are displayed")
                   
            ),
            
            # I might take this column out, as I may or may not have a table. 
            column(6,
                   
                   tags$h4("Events Booked per Day"),
                   tags$h6("The blue lines represent each open evening or info taster session that CodeClan held")
                   
            )
            
          ),
          
          fluidRow(
            # Here is the row for the imagy outputs. 
            column(6,
                   
                   
                   # This conditional panel shows the page before completion graph. Shows most common pages 
                   # visited immediately before the user went to the page where they booked an event. 
                   conditionalPanel(
                     condition = "input.interactivity == 'Interactive'", 
                     plotlyOutput("page_before_completion")
                   ),
                   
                   conditionalPanel(
                     condition = "input.interactivity == 'Printer Friendly'",
                     plotOutput("page_before_completion_gg")
                   ) 
                   
                   
            ),
            
            column(6,
                   
                   plotlyOutput("event_timeseries")
                   
            )
          )
        )
      )
    ),
    
    tabPanel("Exits",
             
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   selectInput(
                     "interactivity_1",
                     label = tags$h5("View"),
                     choices = c("Interactive", "Printer Friendly"),
                     selected = "Interactive"
                   ) 
                 )
               ),
               
               mainPanel(
                 fluidRow(
                   # This row is adding titles for the graphs that will be underneath it.
                   column(6,
                          #Number of Exits per total number of Page Views.
                          tags$h4("Number of Exits per total number of Page Views")
                          
                          
                   ),
                   
                   column(6,
                          # Number of Pages visited before Exit.
                          tags$h4("Number of Pages visited before Exit")
                          
                          
                   )
                 ),
                 fluidRow(
                   column(6,       
                          conditionalPanel(
                            condition = "input.interactivity_1 == 'Interactive'",
                            plotlyOutput("exit_rates_plot_1")
                          ),
                          
                          conditionalPanel(
                            condition = "input.interactivity_1 == 'Printer Friendly'",
                            plotOutput("exit_rates_plot_1_1")
                          )
                   ),
                   
                   
                   column(6,
                          conditionalPanel(
                            condition = "input.interactivity_1 == 'Interactive'",
                            plotlyOutput("page_depth_plot_1")
                          ),
                          
                          conditionalPanel(
                            condition = "input.interactivity_1 == 'Printer Friendly'",
                            plotOutput("page_depth_plot_1_1")
                          )
                   )
                 ),
                 fluidRow(
                   # This row is adding titles for the 3rd graph that will be underneath it.
                   column(6,
                          #Number of Exits per total number of Page Views.
                          tags$h4("Session duration and Exit page")
                   )
                 ),
                 fluidRow(
                   column(6,
                          
                          # Conditional panels only show up when a condition is met. This condition is the 
                          # interactive button on the sidepanel. This will change which plot is displayed on the 
                          # shiny app. 
                          conditionalPanel(
                            condition = "input.interactivity_1 == 'Interactive'",
                            plotlyOutput("sessions_and_exits_plot_1")
                          ),
                          
                          conditionalPanel(
                            condition = "input.interactivity_1 == 'Printer Friendly'",
                            plotOutput("sessions_and_exits_plot_1_1")
                          )
                   )
                 )
                 
               )
             )
             
    )
    
    
  )
)


