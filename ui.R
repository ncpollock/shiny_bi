


ui <- tagList(tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css")
              ,dashboardPage(
    dashboardHeader(title = "Shiny Business Intelligence Tool",
                    titleWidth = 450),
    
    sidebar <- dashboardSidebar(
        sidebarMenu(
            br()
            ,HTML("<i class='fa fa-cogs' style='display:block; font-size: 50px; text-align: center;'></i>")
            ,br()
            ,menuItem("1. Upload", tabName = "upload", icon = icon("upload"))
            ,menuItem("2. Inspect",icon=icon("search"),tabName="inspect")
            ,menuItem("3. Explore",icon=icon("bar-chart"),tabName="explore")
            ,menuItem("Wishlist",icon=icon("gift"),tabName="wishlist")
            ,menuItem("About",tabName = "about", icon = icon("question")
                      ,fluidPage(style="white-space: normal;"
                                 , p("This dashboard is a Business Intelligence (BI) tool built with the R language and Shiny-related packages.
                      Please consider it a proof of concept rather than a fully featured production-worthy BI tool. There are many freely available
                          BI tools with much richer feature sets (e.g., Microsoft Power BI, Tableau, Excel). This tool was heavily inspired by RapidMiner and JMP.")
                      ))
            ,br()
            ,br()
            ,p(img(src="headshot.jpg",id="face-img",align="center"),br()
               ,strong("Developed by: "),
               br(),
               a(href="https://ncpollock.github.io/"
                 ,target="_blank"
                 ,"Noah C. Pollock"),
               br(),
               a(href = "https://github.com/ncpollock/shiny_bi"
                 ,target="_blank"
                 ,"Code on GitHub"),
               align="center")
        )
    ),
    
    body <- dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Roboto:300italic,400,700")
        ),
        style="font-family: 'Roboto';",
        tags$head(tags$style(custom_colors)),
        tabItems(
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
                                    c("Upload","Post-Grad Outcomes"
                                      ,"Department of Education API"
                                      ,"iris","diamonds","PlantGrowth"),
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
                        h2("Depending on the size of your dataset, this may take a minute or two...")),
                        uiOutput("inspect_vars")
                    )),
          
          tabItem(tabName = "explore",
                  fluidRow(
                    column(width = 4,
                      # actionButton("init_plot", "Generate Chart!",icon = icon("arrow-circle-right")),
                      box(title=title_collapse("Plot Type"),width=12,
                          collapsible = TRUE,status="danger",solidHeader = TRUE,
                      selectInput("plot_type","Plot Type",
                                  c("Bar","Boxplot","Column","Heatmap","Histogram","Line","Point"),
                                  selected = "Column")),
                      box(title=title_collapse("Plot Axes and Groups"),width=12,
                          collapsible = TRUE,status="danger",solidHeader = TRUE,
                          uiOutput("plot_axes"))
                    ),
                    column(width=8,
                      box(title=strong("Explore Data"),status="primary",width=12,
                          p("Explore your data visually. This may take some trial and error to get it right!"),
                          plotOutput('explore_chart'),
                          hr(syle = "color: black; hieght = 50px")
                          # dataTableOutput("explore_chart_table")
                    )
                  ),
                  column(width=12,
                  box(title=title_collapse("Y-Value Stats"),width=4,
                      collapsed = TRUE,collapsible = TRUE,status="danger",solidHeader = TRUE,
                      selectInput("plot_stats","Y-Value Stats",
                                  c("Sum","Average","Value"),
                                  selected = "Value")),
                  tabBox(title=strong("Plot Features"),width=8,height = "250px"
                         , tabPanel("Text/Labels"
                      , column(width=6,
                             selectizeInput("expl_theme", 
                                            "Toggle Chart Elements: ",
                                            c("X-Axis","Y-Axis"), selected = c("X-Axis","Y-Axis"), multiple = TRUE
                                            ,options = list(placeholder = 'Click to see options...')),
                             sliderInput("expl_size", "Size: ",.5,min=0,max=1,step=.1)),
                      column(width=6,
                             checkboxInput("expl_legend", "Legend", TRUE),
                             checkboxInput("expl_label", "Data Labels", FALSE)))
                      , tabPanel("Color"
                                 # pick from a few paalletes?
                                 # select individual elements?
                      )
                  )
                      #make labels selectize input for style eg round(x), percent(), dollar()
                      # or eg any theme element such as ... x axis label, y axis label, x axis
                      # color pallette and color picker using googlevis? like geom_tile?
                  )
                  ))
          
          ,tabItem("wishlist",
                   fluidRow(
                     box(status="primary",
                         width=12,
                         h1("Future updates on the wishlist:"),
                         tags$ul(
                           tags$li("More feature toggling in Explore tab including: smoothing lines, color pallette"),
                           tags$li("Analyze tab where the user can input variables into statistical models."),
                           tags$li("Toggle variable types (e.g., convert numeric to string and vice versa)."),
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

) # dashboardpage
) # taglist
