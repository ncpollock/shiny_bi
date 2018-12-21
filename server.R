


# Define server logic required to draw a datatable
shinyServer(function(input, output, clientData, session) {
    
    file_df <- reactive({
      
      if(input$select_dataset!="Upload"){
        
        pg_outcomes = read.csv("air_df.csv",stringsAsFactors = FALSE)
        
        dataset_list <- list(PlantGrowth = PlantGrowth,
                             iris = iris,
                             diamonds = diamonds,
                             "Post-Grad Outcomes" = pg_outcomes)
        
        file_df <- data.frame(
          dataset_list[[input$select_dataset]])
        
      } else {
        
      inFile <- input$data_file
      
      if (is.null(inFile))
        return(NULL)
      
      file_df <- read.csv(inFile$datapath,
                          stringsAsFactors = FALSE,
                          na.strings = c("","NA","N/A"," ")
                          #,header = input$header, sep = input$sep, quote = input$quote
      )
      }
      
      file_df
    })
    
    output$full_dataset <- renderDataTable({
      
      datatable(file_df())
      
    })
    
    output$file_columns <- renderInfoBox({
      
      tdata <- ifelse(is.null(file_df()),0,ncol(file_df()))
      
      infoBox(tdata,
               title = "Number of Columns",
               icon=icon("columns"),
               color="blue")
    })
    
    output$file_rows <- renderInfoBox({
      
      tdata <- ifelse(is.null(file_df()),0,nrow(file_df()))
      
      infoBox(tdata,
               title = "Number of Rows",
               icon=icon("align-justify"),
               color="blue")
    })
    
    output$file_size <- renderInfoBox({
      
      tdata <- ifelse(is.null(file_df()),0,
                      format(object.size(file_df()),units="b"))

      infoBox(value = p(tdata,br(),
              format(object.size(file_df()),units="Mb")),
               title="Dataset Size",
               icon=icon("expand-arrows-alt"),
               color="blue")
    })
    
# explore.tab ########################################################
    
    output$plot_axes <- renderUI({
      list(
      selectInput('x_var', 'X-Value', names(file_df())),

      selectInput('y_var', 'Y-Value',
                  c("None",names(select_if(file_df(),
                                           is.numeric)))),

      selectInput('color_var', 'Color Group',c("None",names(file_df()))),

      selectInput('facet_var', 'Facet Group', 
                  c("None",names(select_if(file_df(),
                                           function(col) is.character(col)|is.factor(col)))))
      )
    })


    output$explore_chart <- renderPlot({

      # there's probably a better way to accomplish this
      # i have a lot of conditional logic that repeats code
      
      plot_df <- file_df()
      
      #transform text vars into symbols for dplyr programming
      x_var <- input$x_var
      x_var_sym <- sym(x_var)
      
      y_var <- input$y_var
      y_var_sym <- sym(y_var)

      color_var <- input$color_var
      color_var_sym <- sym(color_var)
      
      facet_var <- input$facet_var
      facet_var_sym <- sym(facet_var)
      
      # simplest might be if(){} else {} instead
      plot_df <- plot_df %>%
        mutate(x_var = !!x_var_sym,
               y_var = ifelse(rep(TRUE,nrow(plot_df)) & #so ifelse evaluates for every row
                                input$y_var=="None",1,!!y_var_sym),
               color_var = ifelse(rep(TRUE,nrow(plot_df)) & 
                                    input$color_var == "None","None",as.character(!!color_var_sym)),
               facet_var = ifelse(rep(TRUE,nrow(plot_df)) & 
                                    input$facet_var=="None","None",as.character(!!facet_var_sym)))
      
      # put color and facet groups into vector for grouping
      group_vars <- NULL
      if(input$color_var != "None"){
        group_vars <- c(group_vars,"color_var")
        }
      if(input$facet_var != "None"){ 
        group_vars <- c(group_vars,"facet_var")
        }
        
      plot_df <- plot_df %>%
      # combine color and facet in vector and groub_by_at
        group_by_at(.vars = group_vars)
      
      # apply chosen stat and init ggplot
      if(input$y_var == "None"){
        plot_df <- plot_df %>%
          group_by_at(.vars = vars(x_var,group_vars)) %>%
          summarise(y_var = n())
        gp <- ggplot(plot_df,aes(x=x_var,y=y_var,fill=color_var,group=color_var,color=color_var))
      }
      
      if(input$y_var != "None" &
         input$plot_stats == "Value"){
        gp <- ggplot(plot_df,aes(x=x_var,y=y_var,fill=color_var,group=color_var,color=color_var))
      }
      
      if(input$y_var != "None" &
         input$plot_stats == "Average"){
        plot_df <- plot_df %>%
          group_by_at(.vars = vars(x_var,group_vars)) %>%
          summarise(y_var = mean(!!y_var_sym,na.rm = TRUE))
        gp <- ggplot(plot_df,aes(x=x_var,y=y_var,fill=color_var,group=color_var,color=color_var))
      }
      
      if(input$y_var != "None" &
         input$plot_stats == "Sum"){
        plot_df <- plot_df %>%
          group_by_at(.vars = vars(x_var,group_vars)) %>%
          summarise(y_var = sum(!!y_var_sym,na.rm = TRUE))
        gp <- ggplot(plot_df,aes(x=x_var,y=y_var,fill=color_var,group=color_var,color=color_var))
      }
      
      # apply facet if applicable
      if(input$facet_var != "None") gp <- gp + facet_wrap(~facet_var)
      
      # remove legend if not grouped
      # need a solution for when faceted but not colored!
      if(is.null(group_vars)) gp <- gp + guides(fill=FALSE,color=FALSE)
      
      
      # fix axis labels
      gp <- gp + 
        xlab(input$x_var) +
        ylab(ifelse(input$y_var=="None","Frequency",""))
      
      # apply chosen plot type
      if(input$plot_type == "Column") gp <- gp + geom_col()
      if(input$plot_type == "Bar") gp <- gp + geom_col() + coord_flip()
      if(input$plot_type == "Heatmap") gp <- gp + geom_bin2d()
      if(input$plot_type == "Line") gp <- gp + geom_line()
      if(input$plot_type == "Point") gp <- gp + geom_point()
      # if(input$plot_type == "Boxplot") gp <- gp + geom_boxplot(color="black")
      if(input$plot_type == "Boxplot"){
        if(is.null(group_vars)){
          gp <- gp + geom_boxplot(aes(x=as.character(x_var),fill = NULL, group = NULL, color = NULL),color="black")
        } else {
          gp <- gp + geom_boxplot(aes(x=as.character(x_var),group=as.character(x_var)),color="black")
        }
      } 
      
        # geom_boxplot(aes(group=x_var),color="black")
        # geom_boxplot(aes(x=y_var,y=x_var),color="black")
      
      #apply custom theme
      gp <- gp + my_theme
      
      # add features
      if(input$expl_label == TRUE) gp <- gp + geom_label(aes(label=y_var)
                                                         ,color="black"
                                                         ,position = position_stack(vjust = 0.5))
      if(input$expl_legend == FALSE) gp <- gp + theme(legend.position="none")
     
      gp
      
    })
    
    output$explore_chart_table <- renderDataTable({
      
      # eventually allow the user to choose specific points?
      plot_df <- file_df()
      
      #transform text vars into symbols for dplyr programming
      x_var <- input$x_var
      x_var_sym <- sym(x_var)
      
      y_var <- input$y_var
      y_var_sym <- sym(y_var)
      
      color_var <- input$color_var
      color_var_sym <- sym(color_var)
      
      facet_var <- input$facet_var
      facet_var_sym <- sym(facet_var)
      
      # create dataframe for plots
      plot_df <- plot_df %>%
        mutate(x_var = !!x_var_sym,
               y_var = ifelse(rep(TRUE,nrow(plot_df)) & #so ifelse evaluates for every row
                                input$y_var=="None",1,!!y_var_sym),
               color_var = ifelse(rep(TRUE,nrow(plot_df)) & 
                                    input$color_var == "None","None",as.character(!!color_var_sym)),
               facet_var = ifelse(rep(TRUE,nrow(plot_df)) & 
                                    input$facet_var=="None","None",as.character(!!facet_var_sym)))
      
      # put color and facet groups into vector for grouping
      group_vars <- NULL
      if(input$color_var != "None"){
        group_vars <- c(group_vars,"color_var")
      }
      if(input$facet_var != "None"){ 
        group_vars <- c(group_vars,"facet_var")
      }
      
      plot_df <- plot_df %>%
        # combine color and facet in vector and groub_by_at
        group_by_at(.vars = group_vars)
      
      # apply chosen stat and init ggplot
      if(input$y_var == "None"){
        plot_df <- count(plot_df,x_var)
      }
      if(input$y_var != "None" &
         input$plot_stats == "Value"){
      }
      if(input$y_var != "None" &
         input$plot_stats == "Average"){
        plot_df <- summarise(plot_df,y_var = mean(!!y_var_sym))
      }
      if(input$y_var != "None" &
         input$plot_stats == "Sum"){
        plot_df <- summarise(plot_df,y_var = sum(!!y_var_sym))
      }
      datatable(plot_df)
    })
    
# inspect.tab ###########################################################
    output$inspect_vars <- renderUI({
      
      variable_output <- lapply(names(file_df()), function(i) {

        level_counts <- paste0("table1_",i)
        stat_summaries <- paste0("table2_",i)
        textname_var <- paste("text", i, sep="")
        selected_row <- paste("sr", i, sep="")
        
        inspect_histogram <- paste0("plot1_",i)
        inspect_bar <- paste0("plot2_",i)
        
          if(is.numeric(file_df()[[i]])){
            list(box(width = 12,collapsible = TRUE,collapsed = FALSE,solidHeader = TRUE,status = 'success',
                       title=p(strong(i),": Variable is numeric"),
                       box(width=4,DT::dataTableOutput(stat_summaries)),
                     box(width=8,plotOutput(inspect_histogram),
                         sliderInput(paste0("inspect_bin",i),
                                     "Bins:",
                                     # should be number of non-missing values, not number of rows...
                                     min = 1,  max = 50,value = ifelse(2*nrow(file_df())^(1/3)>50,50,2*nrow(file_df())^(1/3))
                                     ))
                     ))

          } else {
            list(
              box(width = 12,collapsible = TRUE,collapsed = FALSE,solidHeader = TRUE,status = 'warning',
                  title=p(strong(i),": Variable is not numeric"),
              box(DT::dataTableOutput(level_counts),width=4),
              box(width=8,plotOutput(inspect_bar)))) }
        
        # could add in other checks for dates, booleans, etc
        
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
          inspect_bar <- paste0("plot2_",my_i)
          
          #get selected table indice
          selected_row <- paste("sr", my_i, sep="")
          output[[selected_row]] <- renderText(
            as.character(
              distinct(file_df(),!!j)[input[[paste0("table1_",my_i,"_rows_selected")]],]
            )
            )
          
          #initialize empty space
          output$none <- renderPlot({})
          
          output[[level_counts]] <- DT::renderDataTable({
            datatable(count(file_df(),!!j) # should save this as a more global var, used several times
                      ,rownames = FALSE
                      ,colnames = c(paste0(my_i," Level"),
                                    "Count")
                      ,options = list(dom='ftp'
                                      ,initComplete = dt_column_head
                                      ,search = list(regex = TRUE, caseInsensitive = FALSE)
                                      ,ordering=FALSE))
          })
          
          output[[stat_summaries]] <- DT::renderDataTable({
            
            tdata <- file_df() %>%
              summarise(Min = min(!!j,na.rm = TRUE),
                        Max = max(!!j,na.rm = TRUE),
                        Mean = mean(!!j,na.rm = TRUE),
                        SD = sd(!!j,na.rm = TRUE),
                        Median = median(!!j,na.rm = TRUE),
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
                        ,initComplete = dt_column_head
                      ))
          })
          
          output[[inspect_histogram]] <- renderPlot({
            
            pdata <- file_df() %>%
              mutate(fill_val = ifelse(abs(!!j) > (mean(!!j) + (sd(!!j))),"Tail","Not"))
            
            plot_grid(ggplot(pdata,aes(x=!!j)) +
                        geom_histogram(bins = input[[paste0("inspect_bin",my_i)]]) +
                        #stat lines
                        geom_vline(aes(xintercept = median(!!j,na.rm = TRUE),color="median"),size = 2) +
                        geom_vline(aes(xintercept = mean(!!j,na.rm = TRUE),color="mean"),size = 2) +
                        
                        # cutoff lines
                        geom_vline(aes(xintercept=(mean(!!j) + sd(!!j)),color="cutoff"),size=1.5) +
                        geom_vline(aes(xintercept=(mean(!!j) - sd(!!j)),color="cutoff"),size=1.5) +
                        
                        scale_color_manual(name = "Stats", 
                                           values = c(median = "black", mean = "orange",cutoff = "red")) +
                        # to maintain alignment with boxplot
                        scale_x_continuous(limits = c(min(file_df()[[my_i]],na.rm = TRUE)
                                                      ,max(file_df()[[my_i]],na.rm = TRUE))) +
                        my_theme +
                        theme(legend.position = 'top'),
                      ggplot(pdata) +
                        geom_boxplot(aes(y=!!j,x=1),fill = "gray50") +
                        geom_hline(aes(yintercept = median(!!j,na.rm = TRUE),color="median"),size = 1.7) +
                        geom_hline(aes(yintercept = mean(!!j,na.rm = TRUE),color="mean"),size = 2) +
                        scale_color_manual(name = "Stats", values = c(median = "black", mean = "orange")) +
                        coord_flip() +
                        my_theme +
                        theme(axis.title = element_blank()),
                      rel_heights=c(3,1),
                      align = "v",
                      nrow = 2
            )
          })
          
          output[[inspect_bar]] <- renderPlot({
            
            row_indice <- input[[paste0("table1_",my_i,"_rows_selected")]]
            row_value <- as.character(distinct(file_df(),!!j)[row_indice,])
          
            pdata <- file_df() %>%
              count(!!j) %>%
              mutate(selected = ifelse(!!j %in% row_value,"Selected","Not"))
            
            ggplot(pdata,aes(x=!!j,y=n,fill=selected)) +
              scale_fill_manual( values = c("Selected" = "red", "Not" = "gray"), guide = FALSE ) +
              geom_col() +
              coord_flip() +
              my_theme
          })
        })
      }
    })
    
    
})