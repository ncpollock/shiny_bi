


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
            ,menuItem("About",tabName = "about", icon = icon("question"))
            ,menuItem("1. Upload", tabName = "upload", icon = icon("upload"))
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
          tabItem(tabName = "about",
                  h3("Welcome!"),
                  p("This dashboard is a Business Intelligence (BI) tool built off of R using Shiny and related packages.
                      Please consider it a proof of concept rather than a fully featured BI tool. There are many freely available
                      BI tools with much richer feature sets (e.g., Microsoft Power BI, Tableau, Excel). In fact,
                    this tool was heavily inspired by RapidMiner and JMP."),
                  p("Follow the tabs to the left in numeric order to see what this dashboard can do!")
                  # https://en.wikipedia.org/wiki/Business_intelligence_software
                  # https://rapidminer.com/
          ),
          tabItem(tabName = "upload",
                  fluidRow(
                    box(title=p(icon("file-copy",class = "fas"),"Upload Data"),
                        width=12,
                        p("Only a true CSV file with headers will work with this tool."),
                        fileInput("data_file", "Choose a CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        h4("Don't have any data to upload?"),
                        selectInput('select_dataset',"Choose a Dataset Instead: ",
                                    c("Upload","iris","diamonds","PlantGrowth"),
                                    selected = "Upload")
                        ),
                    infoBoxOutput('file_columns'),
                    infoBoxOutput('file_rows'),
                    infoBoxOutput('file_size'),
                    box(solidHeader = TRUE,width=12,title = "Preview Data",status = "success",
                        dataTableOutput("full_dataset")))),
          
          tabItem(tabName = "inspect",
                  fluidRow(
                    box(title="Inspect Data",status="primary",
                        width=12,
                        p("Depending on the size of your dataset, this may take a minute or two..."),
                        uiOutput("inspect_vars")
                    ))),
          
          tabItem(tabName = "explore",
                  fluidRow(
                    column(width = 4,
                      box(title="Plot Type",width=12,collapsible = TRUE,
                      selectInput("plot_type","Plot Type",
                                  c("Bar","Boxplot","Column","Heatmap","Line","Point"),
                                  selected = "Column")),
                      box(title="Plot Axes and Groups",width=12,collapsible = TRUE,
                          uiOutput("plot_axes")),
                      box(title="Y-Value Stats",width=12,collapsed = TRUE,collapsible = TRUE,
                          selectInput("plot_stats","Y-Value Stats",
                                      c("Sum","Average","Value"),
                                      selected = "Value"))
                    ),
                    column(width=8,
                      box(title="Explore Data",status="primary",collapsible = TRUE,width=12,
                          p("Explore your data visually. This may take some trial and error to get it right!"),
                          plotOutput('explore_chart'),
                          hr(syle = "color: black; hieght = 50px")
                          # dataTableOutput("explore_chart_table")
                    )
                  )))
                  
          # tabItem(tabName = "analyze")
                  
          

            #             valueBoxOutput("top_name"),
            #             bsPopover("top_name",
            #                       title="Most popular guest name!",
            #                       content=paste0("The most frequent guest name invited",
            #                                      " to our wedding."),
            #                       placement="bottom")

        )
        )

)
