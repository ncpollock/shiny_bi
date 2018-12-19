


ui <- dashboardPage(
    dashboardHeader(title = "Shiny Business Intelligence Tool",
                    titleWidth = 450),
    
    sidebar <- dashboardSidebar(
        sidebarMenu(
            br()
            ,HTML("<i class='fa fa-cogs' style='display:block; font-size: 50px; text-align: center;'></i>")
            ,br()
            ,menuItem("About",tabName = "about", icon = icon("question"))
            ,menuItem("1. Upload", tabName = "upload", icon = icon("upload"))
            ,menuItem("2. Inspect",icon=icon("search"),tabName="inspect")
            ,menuItem("3. Explore",icon=icon("bar-chart"),tabName="explore")
            ,menuItem("Wishlist",icon=icon("gift"),tabName="wishlist")
            ,br()
            ,br()
            ,p("Developed by: ",
               br(),
               "Noah C. Pollock",
               br(),
               a(href = "https://github.com/ncpollock/shiny_bi", "Code on GitHub"),
               align="center")
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
                  box(title = "Welcome!",solidHeader = TRUE,status = "primary",collapsible = FALSE,width=12,
                  h3("This dashboard is a Business Intelligence (BI) tool built off of R using Shiny and related packages.
                      Please consider it a proof of concept rather than a fully featured BI tool. There are many freely available
                      BI tools with much richer feature sets (e.g., Microsoft Power BI, Tableau, Excel). In fact,
                    this tool was heavily inspired by RapidMiner and JMP."),
                  h2("Follow the tabs to the left in numeric order to see what this dashboard can do!")
          )),
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
                                    c("Upload","Post-Grad Outcomes","iris","diamonds","PlantGrowth"),
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
                        h2("Depending on the size of your dataset, this may take a minute or two..."),
                        uiOutput("inspect_vars")
                    ))),
          
          tabItem(tabName = "explore",
                  fluidRow(
                    column(width = 4,
                      box(title="Plot Type",width=12,
                          collapsible = TRUE,status="danger",solidHeader = TRUE,
                      selectInput("plot_type","Plot Type",
                                  c("Bar","Boxplot","Column","Heatmap","Line","Point"),
                                  selected = "Column")),
                      box(title="Plot Axes and Groups",width=12,
                          collapsible = TRUE,status="danger",solidHeader = TRUE,
                          uiOutput("plot_axes")),
                      box(title="Y-Value Stats",width=12,
                          collapsed = TRUE,collapsible = TRUE,status="danger",solidHeader = TRUE,
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
          
          ,tabItem("wishlist",
                   fluidRow(
                     box(status="primary",
                         width=12,
                         h1("Future updates on the wishlist:"),
                         tags$ul(
                           tags$li("Major improvements to graphing (it barely works as is)."),
                           tags$li("Analyze tab where the user can input variables into statistical models."),
                           tags$li("Toggle variable types (e.g., convert numeric to string and vice versa."),
                           tags$li("Data cleaning and feature engineering (e.g., filter, recode, and mutate)."),
                           tags$li("Join multiple datasets.")
                         )
                         )))
                  
          # tabItem(tabName = "analyze")

            #             valueBoxOutput("top_name"),
            #             bsPopover("top_name",
            #                       title="This is the title!",
            #                       content=paste0("This is the content"),
            #                       placement="bottom")

        )
        )

)
