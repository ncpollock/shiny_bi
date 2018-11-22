


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
    
    output$reactive_input_test <- renderUI({
      selectInput('x_val', 'X-Value', names(file_df()))
    })
    
    output$boxes2 <- renderUI({
      
      variable_output <- lapply(names(file_df()), function(i) {

        tablename1 <- paste0("table1_",i)
        tablename2 <- paste0("table2_",i)
        textname_var <- paste("text", i, sep="")
        
        # plotname <- paste("plot", i, sep="")
        plotname1 <- paste0("plot1_",i)
        plotname2 <- paste0("plot1_",i)
        
        c(
          if(is.numeric(file_df()[[i]])){
            list(box(width = 12,collapsible = TRUE,collapsed = FALSE,solidHeader = TRUE,status = 'primary',
                     br(),
                     br(),
                     p(strong(paste0("Figure ",i,":")),
                       "Can do histogram because data is numeric"),
                     plotOutput(plotname1,width="100%",height="600px"),
                     DT::dataTableOutput(tablename2)))
          } else {
            list(
              h2("Variable: ",textOutput(textname_var,inline=TRUE),align="center"),
              DT::dataTableOutput(tablename1),
              plotOutput(plotname2,width="100%",height="600px")) },
          list(p("Each variable gets either a plot or a table, but every variable gets this nice paragraphs."))
        )
      })
      test_reactive()
      do.call(tagList, variable_output)
      # }
    })
    
    
    test_reactive <- reactive({
      for (i in names(file_df())) {
        # t <- try(file_df())
        # if(!("try-error" %in% class(t))){
        # } else {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          my_i <- i
          j <- sym(i) # symbolize quoted variable name for use in dplyr programming
          
          #school/college name
          textname_var <- paste("text", my_i, sep="")
          # icollege <- sort(unique(FDS_df$college))[my_i]
          output[[textname_var]] <- renderText(my_i)
          
          tablename1 <- paste0("table1_", my_i)
          tablename2 <- paste0("table1_", my_i)
          plotname1 <- paste0("plot1_",my_i)
          plotname2 <- paste0("plot1_",my_i)
          
          #initialize empty space
          output$none <- renderPlot({})
          
          output[[tablename1]] <- DT::renderDataTable({
            datatable(count(file_df(),!!j),rownames = FALSE)
          })
          
          output[[tablename2]] <- DT::renderDataTable({
            datatable(file_df() %>%
                        summarise(Min = min(!!j),
                                  Max = max(!!j),
                                  Mean = mean(!!j),
                                  Median = median(!!j),
                                  Distinct = n_distinct(!!j),
                                  Missing = sum(ifelse(is.na(!!j),1,0))) %>% 
                        gather(stat,value) %>% 
                        filter(!is.na(value)),rownames = FALSE,colnames = FALSE)
          })
          
          output[[plotname1]] <- renderPlot({
            # hist(file_df()[[my_i]])
            hist(file_df()[[my_i]]
                 ,main=paste0("Histogram of ",my_i)
                 ,xlab=my_i
                 ,col='red')
          })
          
          output[[plotname2]] <- renderPlot({
            # hist(file_df()[[my_i]])
            barplot(table(file_df()[[my_i]])
                    ,main=paste0("Histogram of ",my_i)
                    ,xlab=my_i
                    ,col='red')
          })
        })
      }
      # }
    })
    
    
})