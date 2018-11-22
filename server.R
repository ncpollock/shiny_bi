


# Define server logic required to draw a datatable
shinyServer(function(input, output, clientData, session) {
    
    output$top_name <- renderValueBox({
        
        tdata <- guests %>% 
            group_by(First.Name) %>% 
            filter(First.Name != "Guest") %>%
            summarise(name_count = n()) %>% 
            arrange(desc(name_count)) %>%
            top_n(1)
        
        valueBox(tdata$First.Name,
                 "Most Popular Guest Name",
                 icon=icon("vcard-o"),
                 color="maroon")
    })
    
    
    output$county_map <- renderPlot({
        
        #by county
        #map of michigan
        counties <- map_data("county")
        counties <- subset(counties, region %in% c( "michigan"))

        pdata <- households %>%
          filter(administrative_area_level_1=="Michigan") %>%
          group_by(administrative_area_level_2) %>%
          summarise(guests = n()) %>% 
          mutate(administrative_area_level_2 = tolower(administrative_area_level_2)) %>%
          mutate(pdata_counties = tolower(gsub("\\.+| county","",administrative_area_level_2))) %>%
          full_join(counties,by=c("pdata_counties" = "subregion")) %>%
          mutate(guests = ifelse(is.na(guests),0,guests)) %>%
          mutate(test_cat = cut(guests,
                                c(-Inf,0, 1, 5, 10, Inf),
                                labels=c("0","1","2-4",'5-10','>10')))

        #prove that counties from map_data and counties from google are identical
          #after gsub.
          #identical(sort(unique(pdata$pdata_counties)),sort(unique(counties$sub)))

        ggplot(pdata, aes(x=long,y=lat,group=group)) +
          scale_fill_manual(values=c(v_light_gray,med_gray,"black",maroon1)) +
          geom_polygon(aes(fill=test_cat),color='white') +
          xlab(NULL) +
          ylab(NULL) +
          theme(aspect.ratio = 1,
                plot.margin=unit(c(0,5,0,0), "cm"),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line   = element_blank())
        
    })
    
    file_df <- reactive({
      inFile <- input$data_file
      
      if (is.null(inFile))
        return(NULL)
      
      file_df <- read.csv(inFile$datapath,
                          stringsAsFactors = FALSE,
                          na.strings = c("","NA","N/A"," ")
                          #,header = input$header, sep = input$sep, quote = input$quote
      )
      file_df
    })
    
    # data_df <- reactive({
    #   inFile <- input$data_file
    #   
    #   if (is.null(file_df()))
    #     return(NULL)
    # 
    #   # data_df <- file_df() %>%
    #     
    # })
    
    output$full_dataset <- renderDataTable({
      
      # js <- "function(data, type, full){ return '<span class=spark>' + data + '</span>' }"
      # colDefs2 <- list(list(targets = c(1:6), render = JS(js)))
      # bar_string <- "type: 'bar', barColor: 'orange', negBarColor: 'purple', highlightColor: 'black'"
      # cb_bar <- JS(paste0("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { ",
      #                     bar_string, " }); }"), collapse = "")
      # 
      # data_df <- file_df() %>%
      #   filter(dat, Var == "Temperature") %>% group_by(Decade, Month) %>% summarise(Temperature = paste(Val,
      #                                                                                                   collapse = ","))
      # 
      # d1 <- datatable(file_df(), rownames = FALSE, options = list(columnDefs = colDefs2,
      #                                                       fnDrawCallback = cb_bar))
      # d1$dependencies <- append(d1$dependencies, htmlwidgets:::getDependency("sparkline"))
      # d1
      
      datatable(file_df())
      
    })
    
# explore.tab ########################################################
    
    output$plot_axes <- renderUI({
      list(
      selectInput('x_var', 'X-Value', names(file_df())),

      selectInput('y_var', 'Y-Value', names(file_df())),

      selectInput('color_var', 'Color Group', names(file_df())),

      selectInput('facet_var', 'Facet Group', names(file_df()))
      )
    })

    
    
# inspect.tab ###########################################################
    output$inspect_vars <- renderUI({
      
      variable_output <- lapply(names(file_df()), function(i) {

        level_counts <- paste0("table1_",i)
        stat_summaries <- paste0("table2_",i)
        textname_var <- paste("text", i, sep="")
        
        inspect_histogram <- paste0("plot1_",i)
        inspect_bar <- paste0("plot1_",i)
        
        # c(
          if(is.numeric(file_df()[[i]])){
            list(box(width = 12,collapsible = TRUE,collapsed = FALSE,solidHeader = TRUE,status = 'primary',
                       title=p(strong(i),": Variable is numeric"),
                       box(width=4,DT::dataTableOutput(stat_summaries)),
                     box(width=8,plotOutput(inspect_histogram,width="100%",height="600px"))
                     ))
            # ))
          } else {
            list(
              box(width = 12,collapsible = TRUE,collapsed = FALSE,solidHeader = TRUE,status = 'primary',
                  title=p(strong(i),": Variable is numeric"),
              box(DT::dataTableOutput(level_counts),width=4),
              box(width=8,plotOutput(inspect_bar,width="100%",height="600px")))) }
          # ,list(p("Each variable gets either a plot or a table, but every variable gets this nice paragraphs.")))
      })
      local_reactive_inspect_vars()
      do.call(tagList, variable_output)
      # }
    })
    
    
    local_reactive_inspect_vars <- reactive({
      for (i in names(file_df())) {

        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          my_i <- i
          j <- sym(i) # symbolize quoted variable name for use in dplyr programming
          
          #get variable name
          textname_var <- paste("text", my_i, sep="")
          output[[textname_var]] <- renderText(my_i)
          
          level_counts <- paste0("table1_", my_i)
          stat_summaries <- paste0("table2_", my_i)
          inspect_histogram <- paste0("plot1_",my_i)
          inspect_bar <- paste0("plot1_",my_i)
          
          #initialize empty space
          output$none <- renderPlot({})
          
          output[[level_counts]] <- DT::renderDataTable({
            datatable(count(file_df(),!!j)
                      ,rownames = FALSE
                      ,colnames = c(paste0(my_i," Level"),
                                    "Count")
                      ,options = list(dom='ftp'
                                      ,initComplete = dt_column_head
                                      ,search = list(regex = TRUE, caseInsensitive = FALSE)))
          })
          
          output[[stat_summaries]] <- DT::renderDataTable({
            
            tdata <- file_df() %>%
              summarise(Min = min(!!j),
                        Max = max(!!j),
                        Mean = mean(!!j),
                        Median = median(!!j),
                        Distinct = n_distinct(!!j),
                        Missing = sum(ifelse(is.na(!!j),1,0))) %>% 
              gather(stat,value) %>% 
              filter(!is.na(value))
            
            datatable(tdata
                      ,rownames = FALSE
                      ,colnames = c("Statistic",
                                    "Value")
                      ,options = list(
                        paging = FALSE
                        ,searching = FALSE
                        ,pageLength = nrow(tdata) 
                      ))
          })
          
          output[[inspect_histogram]] <- renderPlot({
            hist(file_df()[[my_i]]
                 ,main=paste0(my_i, "Histogram")
                 ,xlab=my_i
                 ,col='red')
          })
          
          output[[inspect_bar]] <- renderPlot({
            barplot(table(file_df()[[my_i]])
                    ,main=paste0(my_i,": Counts for Each Level")
                    ,xlab=my_i
                    ,col='red')
          })
        })
      }
    })
    
    
})