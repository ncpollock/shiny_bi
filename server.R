


# Define server logic required to draw a datatable
shinyServer(function(input, output, clientData, session) {
    
    file_df <- reactive({
      
      if(input$select_dataset!="Upload"){
        
        pg_outcomes_df <- read.csv("air_df.csv",stringsAsFactors = FALSE)
        ed_api <- read.csv("ed_api_more.csv",stringsAsFactors = FALSE)
        project_df <- data.frame(
          id = 1:8
          , activity = c("Design Reqs","Assign Team","Design Hardware","Code Software"
                         ,"Build and Test HW","Dev. Patent Request","Test Software","Integrate Systems")
          , timeslot = c(1,2,2,3,3,3,4,5)
          , id_before = c(NA,1,1,2,3,3,4,"7,6,5") # predecessor
          , time_needed = c(14,6,34,27,70,22,49,21)
        )
        
        dataset_list <- list(PlantGrowth = PlantGrowth
                             ,iris = iris
                             ,diamonds = diamonds
                             ,"Post-Grad Outcomes" = pg_outcomes_df
                             ,"Department of Education API" = ed_api
                             ,"Project Management" = project_df)
        
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
      
      datatable(file_df(),filter='top',options=list(scrollX=TRUE))

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
      
      if(is.numeric(file_df()[[input$color_var]])) plot_df$color_var <- as.numeric(plot_df$color_var)
      
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
      } # if
      
      # handle stat transformations ---------------------------
      if(input$y_var != "None" &
         input$plot_stats == "Value"){
      } # if
      
      if(input$y_var != "None" &
         input$plot_stats == "Average"){
        plot_df <- plot_df %>%
          group_by_at(.vars = vars(x_var,group_vars)) %>%
          summarise(y_var = mean(!!y_var_sym,na.rm = TRUE))
      } # if
      
      if(input$y_var != "None" &
         input$plot_stats == "Sum"){
        plot_df <- plot_df %>%
          group_by_at(.vars = vars(x_var,group_vars)) %>%
          summarise(y_var = sum(!!y_var_sym,na.rm = TRUE))
      } # if
      
      # handle special chart types ---------------------------
      if(input$plot_type=="Project Network"){
        
        plot_df <- plot_df %>%
          # expand multivalued id_before
          separate_rows(id_before,sep=",") %>%
          mutate_at(vars(c("id","id_before")),as.character)
        
        plot_df <- plot_df %>%
          # going to
          left_join(plot_df %>% 
                      group_by(id_before) %>% 
                      summarise(timeslot_next = max(timeslot)) %>%
                      filter(!is.na(id_before))
                    ,by = c("id" = "id_before"))
        
        plot_df <- plot_df %>%
          left_join(plot_df %>% 
                      distinct(id,timeslot,timeslot_next) %>%
                      group_by(timeslot) %>%
                      # closer timeslots are in lower layers
                      arrange(timeslot_next) %>%
                      mutate(row_layer = 1:n()) %>%
                      ungroup() %>%
                      select(id,row_layer)
                    , by = "id") %>%
          ungroup()  %>%
          # avoid merge activities being added as multiple layers
          group_by(id) %>%
          mutate(row_layer = min(row_layer)) %>%
          ungroup() 
        # coming from
        
        plot_df <- plot_df %>%
          left_join(plot_df %>%
                      mutate(row_layer_pred = row_layer # row_layer before
                             , timeslot_pred = timeslot # timeslot before
                      ) %>%
                      select(id,row_layer_pred,timeslot_pred) %>%
                      distinct(),by = c("id_before" = "id")) %>%
          mutate(row_layer_pred = ifelse(is.na(row_layer_pred),1,row_layer_pred)
                 , activity_width = .5
                 , activity_height = .8)
        
      } # if project network
      
      
      gp <- ggplot(plot_df,aes(x=x_var,y=y_var,fill=color_var,group=color_var,color=color_var))
      
      # apply facet if applicable
      if(input$facet_var != "None") gp <- gp + facet_wrap(~facet_var)
      
      # remove legend if not grouped
      # need a solution for when faceted but not colored!
      if(is.null(group_vars)) gp <- gp + guides(fill=FALSE,color=FALSE)
      
      # fix axis labels
      gp <- gp + 
        xlab(input$x_var) +
        ylab(ifelse(input$y_var=="None","Frequency",input$y_var))
      
      # apply chosen plot type
      if(input$plot_type == "Column") gp <- gp + geom_col(width=input$expl_size)
      if(input$plot_type == "Bar") gp <- gp + geom_col(width=input$expl_size) + coord_flip()
      if(input$plot_type == "Histogram") gp <- ggplot(plot_df,aes(x=x_var,fill=color_var,group=color_var,color=color_var)) + geom_histogram()
      if(input$plot_type == "Heatmap") gp <- gp + geom_bin2d()
      if(input$plot_type == "Line") gp <- gp + geom_line(size=input$expl_size*8)
      if(input$plot_type == "Point") gp <- gp + geom_point(size=input$expl_size*8)
      if(input$plot_type == "Boxplot"){
        if(is.null(group_vars)){
          gp <- gp + geom_boxplot(aes(x=as.character(x_var),fill = NULL, group = NULL, color = NULL),color="black")
        } else {
          gp <- gp + geom_boxplot(aes(x=as.character(x_var),group=as.character(x_var)),color="black")
        }
      } # if boxplot
      if(input$plot_type == "Project Network"){
        
        gp <- ggplot(plot_df, aes(x = timeslot, y = row_layer
                               , width = activity_width
        )) +
          geom_segment(aes(x = timeslot_pred+.25, xend = timeslot-.25, y=row_layer_pred,yend = row_layer)
                       , arrow = arrow(length = unit(input$expl_size/2, "inches"))
                       , size = input$expl_size*2) + 
          geom_tile(aes(fill = time_needed), colour = "grey50", height = input$expl_size) +
          geom_text(aes(label = activity,y = row_layer+.25),color=input$expl_color_data_labels) +
          geom_text(aes(label = paste("Time:",time_needed),y=row_layer),color=input$expl_color_data_labels) +
          geom_text(aes(label = paste("ID:",id),y = row_layer-.25),color=input$expl_color_data_labels) +
          theme_void()
      } # if project network
      
      #apply custom colors
      gp <- gp + my_theme + 
        theme(text = element_text(color = input$expl_color_text)
              , title = element_text(color = input$expl_color_text)
              , plot.background = element_rect(fill = input$expl_color_panel)
              , panel.background = element_rect(fill = input$expl_color_panel)
              , axis.text = element_text(color = input$expl_color_axis)
              , axis.line.x.bottom = element_line(color = input$expl_color_axis)
              , axis.line.y.left = element_line(color = input$expl_color_axis)
              , axis.ticks = element_line(color = input$expl_color_axis)
              )
      
      # add features
      if("Data Labels" %in% input$expl_theme) gp <- gp + geom_text(aes(label=y_var)
                                                         ,color=input$expl_color_data_labels
                                                         ,position = position_stack(vjust = 0.5))
      
      gp <- gp + theme(
        legend.position = if("Legend" %in% input$expl_theme){ input$expl_legend} else {'none'}
        , axis.title.x = if(!("X-Axis Title" %in% input$expl_theme)) element_blank()
        , axis.text.x = if(!("X-Axis Labels" %in% input$expl_theme)) element_blank()
        , axis.title.y = if(!("Y-Axis Title" %in% input$expl_theme)) element_blank()
        , axis.text.y = if(!("Y-Axis Labels" %in% input$expl_theme)) element_blank()
      )
      
      # colors
      expl_pallette <- rep_len(
        c(input$expl_color_1,input$expl_color_2,input$expl_color_3,input$expl_color_4,input$expl_color_5,input$expl_color_6)
        , length.out = nrow(plot_df) #inefficient
      )
      
      # discrete
      if(!(is.numeric(file_df()[[input$color_var]]))){
      gp <- gp + scale_color_manual(values = expl_pallette)
      gp <- gp + scale_fill_manual(values = expl_pallette)
      } else {
        # continuous
        gp <- gp + scale_color_gradient(low = expl_pallette[1], high = expl_pallette[2])
        gp <- gp + scale_fill_gradient(low = expl_pallette[1], high = expl_pallette[2])
      }
      
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
        level_detail <- paste0("table1_1_",i)
        stat_summaries <- paste0("table2_",i)
        textname_var <- paste("text", i, sep="")
        selected_row <- paste("sr", i, sep="")
        
        inspect_histogram <- paste0("plot1_",i)
        
          if(is.numeric(file_df()[[i]])){
            list(box(width = 12,collapsible = TRUE,collapsed = FALSE,solidHeader = TRUE,status = 'success',
                       title=p(title_collapse(i),": Variable is numeric"),
                       box(width=4,DT::dataTableOutput(stat_summaries)),
                     box(width=8,plotOutput(inspect_histogram),
                         sliderInput(paste0("inspect_bin",i),
                                     "Bins:",min = 1,  max = 50
                                     # should be number of non-missing values, not number of rows...
                                     , value = ifelse(2*nrow(file_df())^(1/3)>50,50,2*nrow(file_df())^(1/3))
                                     ))
                     ))

          } else {
            list(
              box(width = 12,collapsible = TRUE,collapsed = FALSE,solidHeader = TRUE,status = 'warning',
                  title=p(title_collapse(i),": Variable is not numeric"),
                  box(DT::dataTableOutput(level_detail),width=4),
                  box(DT::dataTableOutput(level_counts),width=8)
              )) }
        
        # could add in other checks for dates, booleans, lat/lon/geo, etc
        
          # ,list(p("Each variable gets either a plot or a table, but every variable gets this nice paragraphs.")))
      })
      local_reactive_inspect_vars()
      do.call(tagList, variable_output)
    }) # renderUI
    
    
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
          level_detail <- paste0("table1_1_", my_i)
          stat_summaries <- paste0("table2_", my_i)
          inspect_histogram <- paste0("plot1_",my_i)

          #get selected table indice
          selected_row <- paste("sr", my_i, sep="")
          output[[selected_row]] <- renderText(
            as.character(
              distinct(file_df(),!!j)[input[[paste0("table1_",my_i,"_rows_selected")]],]
            )
            )
          
          #initialize empty space
          output$none <- renderPlot({})
          
          output[[level_detail]] <- DT::renderDataTable({

            tdata <- file_df() %>% 
              count(!!j)
            tdata <- bind_rows(
              c(detail = "Levels",
                value = nrow(tdata)),
              c(detail = "Level Count Range",
                value = paste0(range(tdata$n),collapse = " - ")),
              c(detail = "Missing/Blank Values",
                value = (tdata %>% filter(is.na(!!j)))$n))
            
            datatable(tdata
                      ,rownames = FALSE
                      ,colnames = c(paste0(my_i," Detail"),
                                    "Value")
                      ,options = list(dom='t'
                                      ,initComplete = dt_column_head
                                      ,ordering=FALSE
                                      ,columnDefs = list(list(className = 'dt-center', targets = 1))))
          })
          
          output[[level_counts]] <- DT::renderDataTable({

            tdata <- file_df() %>% count(!!j) %>% mutate(pct = percent(n/sum(n),2)) %>% arrange(desc(n))
            datatable(tdata # should save this as a more global var, used several times
                      ,rownames = FALSE
                      ,colnames = c(paste0(my_i," Level"),
                                    "Count",
                                    "Percent")
                      ,options = list(dom='tpf'
                                      ,initComplete = dt_column_head
                                      ,search = list(regex = TRUE, caseInsensitive = FALSE)
                                      ,columnDefs = list(list(className = 'dt-center', targets = 1:2)))) %>%
              formatStyle(
                my_i,'n',
                background = styleColorBar(range(0,max(tdata$n)), v_light_gray2,angle=270),
                backgroundSize = '100% 75%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center',
                fontWeight = 'bold')
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
                        geom_vline(aes(xintercept = median(!!j,na.rm = TRUE),color="Median"),size = 2) +
                        geom_vline(aes(xintercept = mean(!!j,na.rm = TRUE),color="Mean"),size = 2) +
                        
                        # cutoff lines
                        geom_vline(aes(xintercept=(mean(!!j) + sd(!!j)),color="Cutoff"),size=1.5) +
                        geom_vline(aes(xintercept=(mean(!!j) - sd(!!j)),color="Cutoff"),size=1.5) +
                        
                        scale_color_manual(name = "Stats", 
                                           values = c(Median = "black", Mean = "orange",Cutoff = "red")) +
                        # to maintain alignment with boxplot
                        scale_x_continuous(limits = c(min(file_df()[[my_i]],na.rm = TRUE)
                                                      ,max(file_df()[[my_i]],na.rm = TRUE))) +
                        my_theme +
                        theme(legend.position = 'top'
                              , legend.justification = 'center'
                              , axis.title.x = element_blank()),
                      ggplot(pdata) +
                        geom_boxplot(aes(y=!!j,x=1),fill = "gray50") +
                        geom_hline(aes(yintercept = median(!!j,na.rm = TRUE),color="median"),size = 1.7) +
                        geom_hline(aes(yintercept = mean(!!j,na.rm = TRUE),color="mean"),size = 2) +
                        scale_color_manual(name = "Stats", values = c(median = "black", mean = "orange")) +
                        coord_flip() +
                        my_theme +
                        theme(axis.title.y = element_blank()
                              , axis.ticks.y = element_blank()
                              , axis.text.y = element_blank()
                              , legend.position = 'none'),
                      rel_heights=c(3,2),
                      align = "v",
                      nrow = 2
            )
          })
          
        }) # local
      } # for
    }) # reactive
    
    
})