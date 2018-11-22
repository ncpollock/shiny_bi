


ui <- dashboardPage(
    dashboardHeader(title = "Shiny Business Intelligence Tool",
                    titleWidth = 450),
    
    sidebar <- dashboardSidebar(
        sidebarMenu(
            br()
            # ,img(src="hammick.jpg",height=60,width=100,
            #      style="display: block; margin-left: auto; margin-right: auto;")
            ,HTML("<i class='fa fa-cogs' style='display:block; font-size: 50px; text-align: center;'></i>")
            ,br()
            ,menuItem("1. Upload", tabName = "upload", icon = icon("upload"))
            # ,menuItem("2. Inspect Data", icon = icon("heartbeat"), tabName = "inspect"
            #           # ,badgeLabel = "new", badgeColor = "green"
            #           ,selectInput('who',
            #                        'Learn the story of:',c('Stephanie','Noah'))
            #           ,menuSubItem(text = "Guests", tabName = "guests",icon=icon("glass"))
            #           ,menuSubItem(text = "Bride", tabName = "steph",icon=icon("heart"))
            # )
            ,menuItem("2. Inspect",icon=icon("search"),tabName="inspect")
            ,menuItem("3. Explore",icon=icon("bar-chart"),tabName="explore")
            # ,plotOutput('plot_worth_putting_here')
        )
    ),
    
    body <- dashboardBody(
        tags$head( #from UCM
            tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Roboto:300italic,400,700")
        ),
        style="font-family: 'Roboto';",
        tags$head(tags$style(custom_colors)),
        tabItems(
          tabItem(tabName = "upload",
                  fluidRow(
                    box(title="Upload Data",
                        width=12,
                        p("Only a true CSV file with headers will work with this tool."),
                        fileInput("data_file", "Choose a CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv"))
                        ),
                    box(solidHeader = TRUE,width=12,title = "Preview Data",status = "success",
                        dataTableOutput("full_dataset")))),
          
          tabItem(tabName = "inspect",
                  fluidRow(
                    box(title="Inspect Data",status="primary",
                        width=12,
                        p("Depending on the size of your dataset, this may take a minute or two..."),
                        uiOutput("boxes2")
                    ))),
          
          tabItem(tabName = "explore",
                  fluidRow(
                    column(width = 4,
                      box(title="Define Chart Elements",width=12,collapsible = TRUE,
                        uiOutput("plot_axes")),
                      box(title="Y-Value Stats",width=12,collapsed = TRUE,collapsible = TRUE,
                          selectInput("plot_stats","Y-Value Stats",
                                      c("Count","Sum","Average","Value"))
                          ),
                      box(title="Plot Type",width=12,collapsed = TRUE,collapsible = TRUE,
                          selectInput("plot_type","Plot Type",
                                      c("Column","Bar","Line","Point")))
                    ),
                    column(width=8,
                      box(title="Explore Data",status="primary",collapsible = TRUE,width=12,
                          p("Explore your data visually. This may take some trial and error to get it right!")
                    )
                  )))
                  
            # tabItem(tabName = "overview",
            #         fluidRow(column(12,
            #             # Dynamic valueBoxes
            #             infoBoxOutput("happiness"),
            #             bsPopover("happiness",
            #                       title="Projected Happiness",
            #                       content=paste0("The proportion of lifetime Happiness to Unhappiness",
            #                                      " converted to a percentage."),
            #                       placement="bottom"),
            #             valueBoxOutput("known_days"),
            #             bsPopover("known_days",
            #                       title="Days we have known each other!",
            #                       content=paste0("That is the difference between ",
            #                                      Sys.Date()
            #                                      ," and "
            #                                      ,meet_day
            #                                      ,"!"),
            #                       placement="bottom"),
            #             infoBoxOutput("guest_count"),
            #             bsPopover("guest_count",
            #                       title="Guests at our wedding!",
            #                       content=paste0("The number of individiaul guests",
            #                                      " invited to our wedding."),
            #                       placement="bottom")),
            #             column(12,
            #             valueBoxOutput("days_until_vbox"),
            #             bsPopover("days_until_vbox",
            #                       title="Days Until Our Wedding!",
            #                       content=paste0("That is the difference between ",
            #                                      wedding_day
            #                                      ," and "
            #                                      ,Sys.Date()
            #                                      ,"!"),
            #                       placement="bottom"),
            #             infoBoxOutput("household_count"),
            #             bsPopover("household_count",
            #                       title="Households at our wedding!",
            #                       content=paste0("The number of individual households",
            #                                      " at our wedding."),
            #                       placement="bottom"),
            #             valueBoxOutput("top_name"),
            #             bsPopover("top_name",
            #                       title="Most popular guest name!",
            #                       content=paste0("The most frequent guest name invited",
            #                                      " to our wedding."),
            #                       placement="bottom")
            #             
            #             ),
            #             column(12,
            #                    valueBoxOutput("max_household"),
            #                    bsPopover("max_household",
            #                              title="Largest Household",
            #                              content=paste0("The largest household size.",
            #                                             " Also the average household size was ",
            #                                             guests %>% 
            #                                                 group_by(Household..Name) %>% 
            #                                                 summarise(counts = n()) %>%
            #                                                 summarise(average = mean(counts)), 
            #                                             " average household size."),
            #                              placement="bottom")
            #                    
            #             )
            #             # bsTooltip("know_rate", "Just testing some BS", placement="top"),
            #             
            #         )),
            # 
            # tabItem(tabName = "guests",fluidRow(
            #     box(plotOutput('county_map'))
            #     ,box(title=p("Calendar Heatmap for",textOutput('fisc_year_out')),
            #          width=12,
            #          plotOutput("calendar_heat"),
            #          p("you no render?"))
            #     
            #     ,box(width=12,title="Test Layout!",collapsible=TRUE,collapsed=FALSE,
            #          column(6,
            #                 p('Some rendered text!')),
            #          column(6,
            #                 p('Some more rendered text!')),
            #          column(10,offset=1,
            #                 p('Even more rendered text!')),
            #          column(6,offset=3,
            #                 p('You guessed it, more rendered text!')))
            # )
            # ),
            # tabItem(tabName = "cont_ed",
            #         h2("Continuing Ed e")
            # ),
            # 
            # tabItem(tabName = "about",
            #         h3("Welcome"),
            #         p("Hopefully, this is the start of something beautiful.")
            # )
        )
        )

)
