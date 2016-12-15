shinyServer(
  function(input, output, session) {
    
    #' Expression data
    exprs <- reactive({
      exprs <-  switch(input$datasetType,
                       "Platform" = subset(datasets,Platform %in% input$platform), 
                       "DataSource" = subset(datasets,DataSource %in% input$dataset)
      )
      exprs <- rmNA(exprs)
    }) 
    
    #' Return the available histology, to be used in the updateSelectInput 
    histo <- reactive({
      levels(exprs()[,"Pathology"])
    })
    
    #' Return the available subtype, to be used in the updateSelectInput
    subtype <- reactive({
      if (input$histology == "All"){
        df <- exprs()
        subtype <- levels(df$Subtype)
      } else{
        df <- subset(exprs(), Pathology == input$histology)
        subtype <- intersect(levels(df$Subtype),df$Subtype)
      }
      subtype
    })
    
    #' When switching datasets if the selected histo is not available it will choose "All"
    histo_selected <- reactive ({
      if (input$histology %in% c("All", histo())){
        input$histology
      } else {
        "All"
      }
    })
    
    #' When switching datasets if the selected subtype is not available it will choose "All"
    subtype_selected <- reactive ({
      if (input$subtype %in% c("All", subtype())){
        input$subtype
      } else {
        "All"
      }
    })
    
    observe({
      updateSelectInput(session, inputId = "histology", choices = c("All", histo()), selected = histo_selected())
      updateSelectInput(session, inputId = "subtype", choices = c("All", subtype()), selected = subtype_selected())
    })
    
    #' Text matching with the gene names list
    updateSelectizeInput(session, inputId = "gene", choices = gene_names, server = TRUE)
    
    plot_type <- reactive({
      input$plotType
    })
    
    #' Generate a dataframe with the data to plot
    data <- reactive({
      validate(
        need(input$gene != "", "Please, enter a gene name in the panel on the left")%then%
          # Not all genes are available for all the dataset
          need(input$gene %in% names(exprs()),"Gene not available for this platform")
      )
      mRNA <- exprs()[ ,input$gene]
      data <- cbind(mRNA, exprs()[,1:19]) # To combine with pData
      samples <- data[which(duplicated(data$PtID)),]$PtID
      data <- data[data$PtID %in% samples,]
      data
    })
    
    observeEvent(plot_type(), {
      updateRadioButtons(session, inputId = "point_line", selected = "Box plot")
    })
    
    #' Populate Xaxis labels
    observe({
      updateTextInput(session, inputId = "myXlab",value = paste0("\n",plot_type()))
    })
    
    # Tukey plot active only when tukey stat data are shown
    observeEvent(!input$tukeyHSD, {
      updateCheckboxInput(session, inputId = "tukeyPlot", value = FALSE)
    })
    
    observeEvent(input$point_line == 'Scatter plot', {
      updateCheckboxInput(session, inputId = "tukeyPlot", value = FALSE)
      updateCheckboxInput(session, inputId = "paired_tTest", value = FALSE)
      updateCheckboxInput(session, inputId = "tTest", value = FALSE)
    })
    
    #' Reactive function to generate the box plots
    box_Plot <- reactive({
      data <- data() 
      xlabel <- paste("\n", input$myXlab)
      ylabel <- paste(input$myYlab,"\n")
      col <- input$colorP
      shape <- input$shapeP
      if(input$colorP == "None") {
        col <-  NULL
      } 
      if(input$shapeP == "None") {
        shape <-  NULL
      } 
      theme <- theme(axis.text.x = element_text(size = input$axis_text_size, angle = input$xaxisLabelAngle, hjust = ifelse(input$xaxisLabelAngle == 0,0.5,1)), 
                     axis.text.y = element_text(size = input$axis_text_size),
                     legend.text = element_text(size = input$axis_text_size*0.8),
                     legend.title = element_text(size = input$axis_text_size*0.8),
                     axis.title.x = element_text(size = input$axis_title_size), 
                     axis.title.y = element_text(size = input$axis_title_size))
      
      p <- ggplot(data, mapping=aes_string(x=plot_type(), y = "mRNA")) +
        geom_boxplot(outlier.size = 0, outlier.stroke = 0) +
        geom_jitter(position = position_jitter(width = .25), mapping = aes_string(colour = col, shape = shape),
                    size = input$point_size, alpha = input$alpha) +
        ylab(ylabel) + xlab(xlabel) + theme_bw() + theme 
      
      if(input$point_line == "Lines") {
        p <- ggplot(data, mapping=aes_string(x=plot_type(), y = "mRNA", group="PtID", colour = col, shape = shape)) + 
          geom_line() + 
          geom_point(size = input$point_size) +
          ylab(ylabel) + xlab(xlabel) + theme_bw() + theme 
      }
      
      if(input$point_line == "Scatter plot") {
        exp <-  data %>% select(PtID,Progression,mRNA) %>% spread(Progression, mRNA)
        if(input$colorP == "None") {
          p <- ggplot(exp, aes(Initial, Recurrent)) +
            geom_point(size = input$point_size) + geom_smooth(method = "lm", se = TRUE) +
            geom_rug() + theme
        } else {
          group <-  data %>% select_("PtID","Progression",input$colorP) %>% spread_("Progression",input$colorP)
          names(group)[2:3] <- c(paste0(input$colorP,"_Initial"),paste0(input$colorP,"_Recurrence"))
          data <- merge(exp,group,by="PtID")
          p <- ggplot(data, aes(Initial, Recurrent)) + 
            geom_point(size = input$point_size) + geom_smooth(method = "lm", se = TRUE) + 
            geom_rug(sides="b", aes_string(colour = paste0(input$colorP,"_Initial"))) +
            geom_rug(sides="l", aes_string(colour = paste0(input$colorP,"_Recurrence"))) +
            theme + theme(legend.title = element_blank())
        }
        
      }
      
      if (input$tukeyPlot) {
        t <- tukey() %>%
          mutate(comparison = row.names(.)) %>%
          ggplot(aes(reorder(comparison, diff), diff, ymin = lwr, ymax= upr, colour = Significance)) +
          geom_point() + geom_errorbar(width = 0.25) + ylab("\nDifferences in mean levels") + xlab("") + 
          geom_hline(yintercept = 0, colour="darkgray", linetype = "longdash") + coord_flip() + theme
        p <- grid.arrange(p, t, ncol=2, widths = c(3,2))
      }
      
      return(p)
      
    })
    
    box_width <- reactive({
      if(input$tukeyPlot)
        input$plot_width* 1.8 else
          input$plot_width
    })
    
    #' Create the selected plot
    output$plot <- renderPlot({
      box_Plot()
    }, width = box_width, height = function()input$plot_height)
    
    #' Data for the statistic
    stat_data <- reactive({
      mRNA <- data()[ ,"mRNA"]
      group <- data()[ ,plot_type()]
      data <- data.frame(mRNA, group)
      data
    })
    
    #' Summary statistic
    output$summary <- renderTable({    
      data <- stat_data()
      stat <- data %>%
        group_by(group) %>%
        summarise(Sample_count = paste0(n()," (", round(n()*100/dim(data)[1], 2), "%)" ), # prop.table
                  median = median(mRNA, na.rm=T), mad = mad(mRNA, na.rm=T), mean = mean(mRNA, na.rm=T), 
                  sd = sd(mRNA, na.rm=T)) %>%
        data.frame()
      row.names(stat) <- stat$group
      tot <- data %>%
        summarise(Sample_count = n(), median = median(mRNA, na.rm=T), 
                  mad = mad(mRNA, na.rm=T), mean = mean(mRNA, na.rm=T), sd = sd(mRNA, na.rm=T))
      stat <- stat[,-1]
      stat <- rbind(stat,TOTAL = tot)
      stat 
    }, rownames = TRUE, align='rrrrrr')
    
    #' Tukey post-hoc test, to combine it with the boxplot and to render in a table
    tukey <- reactive({
      validate( 
        need(nlevels(stat_data()$group)>1,message = "There is only one category, group comparison cannot be performed")
      )
      data <-  stat_data()
      tukey <- data.frame(TukeyHSD(aov(mRNA ~ group, data = data))[[1]])
      tukey$Significance <- as.factor(starmaker(tukey$p.adj, p.levels = c(.001, .01, .05, 1), symbols=c("***", "**", "*", "ns")))
      tukey <- tukey[order(tukey$diff, decreasing = TRUE), ]
      tukey
    })
    
    #' Render tukey
    output$tukeyTest <- renderTable({    
      tukey()
    },rownames = TRUE , digits = c(2,2,2,-1,2))
    
    #' Pairwise t test
    output$pairwiseTtest <- renderTable({
      validate(
        need(nlevels(stat_data()$group)>1,message = "There is only one category, group comparison cannot be performed")
      )
      data <-  stat_data()
      pttest <- pairwise.t.test(data$mRNA, data$group, na.rm= TRUE, p.adj = "bonferroni", paired = FALSE)
      pttest$p.value
    }, rownames = TRUE, digits = -1)
    
    # Paired t-test active only when Progression plot  are shown
    observeEvent(plot_type() != "Progression", {
      updateCheckboxInput(session, inputId = "paired_tTest", value = FALSE)
    })
    
    #' Paired t-test
    output$pairedTtest <- renderTable({
      req(plot_type() == "Progression")
      data <-  stat_data()  
      pttest <- broom::tidy(t.test(mRNA ~ group, data, paired=TRUE))
      pttest
    },rownames = TRUE)
    
    #' Get the selected download file type.
    download_plot_file_type <- reactive({
      input$downloadPlotFileType  
    })    
    observe({
      plotFileType    <- input$downloadPlotFileType
      plotFileTypePDF <- plotFileType == "pdf"
      plotUnit    <- ifelse(plotFileTypePDF, "inches", "pixels")
      plotUnitDef <- ifelse(plotFileTypePDF, 7, 600)
      plotUnitMin <- ifelse(plotFileTypePDF, 1, 100)
      plotUnitMax <- ifelse(plotFileTypePDF, 12, 2000)
      plotUnitStep <- ifelse(plotFileTypePDF, 0.1, 50)
      
      updateNumericInput(
        session,
        inputId = "downloadPlotHeight",
        label = sprintf("Height (%s)", plotUnit),
        value = plotUnitDef, min = plotUnitMin, max = plotUnitMax, step = plotUnitStep)
      
      updateNumericInput(
        session,
        inputId = "downloadPlotWidth",
        label = sprintf("Width (%s)", plotUnit),
        value = plotUnitDef, min = plotUnitMin, max = plotUnitMax, step = plotUnitStep)
    })
    
    #' Get the download dimensions.
    download_plot_height <- reactive({
      input$downloadPlotHeight
    })
    download_plot_width <- reactive({
      input$downloadPlotWidth
    })
    
    #' Download the Plot
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_", input$gene, "_", input$dataset, "_", input$plotTypeSel,  
               ".", download_plot_file_type())
      },     
      # The argument content below takes filename as a function and returns what's printed to it.
      content = function(file) {
        # Gets the name of the function to use from the downloadFileType reactive element.
        plotFunction <- match.fun(download_plot_file_type())
        plotFunction(file, width = download_plot_width(), height =  download_plot_height())
        if (input$tukeyPlot) {
          grid.draw(box_Plot()) 
        } else {
          print(box_Plot())
        }
        dev.off()
      }
    )
    
    #' Extract the survival data.
    surv_data <- reactive({    
      df <- data()
      # df <- subset(df, !is.na(df$status))
      df <- subset(df,Progression == "Initial")
      if (input$histology != "All"){
        df <- subset(df, Pathology == input$histology)
      }
      if (input$subtype != "All") {
        df <- subset(df, Subtype == input$subtype)
      }
      # exclude G-CIMP is selected
      if (input$gcimpSurv){
        df <- subset(df, GcimpPrediction != "GCIMP")
      }
      df
    })  
    
    #' Create a slider for the manual cutoff of the Kaplan Meier plot
    mRNA_surv <- reactive({
      surv_need()
      req(input$histology %in% c("All", histo()))
      mRNA <- surv_data()[ ,"mRNA"]
      mRNA.values <- round(mRNA[!is.na(mRNA)],2)
      # Generate a vector of continuos values, excluding the first an last value
      mRNA.values <- sort(mRNA.values[mRNA.values != min(mRNA.values) & mRNA.values != max(mRNA.values)]) 
    })
    
    #' Create a rug plot with the mRNA expression value for the manual cutoff
    output$boxmRNA <- renderPlot({    
      req(input$mInput)      
      mRNA <- round(mRNA_surv(),2)
      q <- quantile(mRNA)
      xrange <-range(mRNA)
      par(mar = c(0,0,0,0)) 
      plot(0, 0, type = "n", xlim = c(xrange[1] + 0.25, xrange[2]) , ylim = c(-0.1,  + 0.1), ylab ="", xlab = "", axes = FALSE)
      points(x = mRNA, y = rep(0, length(mRNA)), pch="|", col=rgb(0, 0, 0, 0.25))
      # Add a red line to show which  is the current cutoff.
      points(x = input$mInput, y = 0, pch = "|", col="red", cex = 2.5)
      points(x = q[2:4], y = rep(0,3), pch = "|", col="blue", cex = 2)
    }, bg = "transparent")
    
    #' Generate the slider for the manual cutoff
    output$numericCutoff <- renderUI({
      sliderInput(inputId = "mInput",label = NULL, min = min(mRNA_surv()), max = max(mRNA_surv()), 
                  value = median(mRNA_surv()), step = 0.05, round = -2)
    })
    
    #' Requirements for all the survival plots
    surv_need <- reactive({
      validate(
        need(input$gene != "", "Please, enter a gene name in the panel on the left")%then%
          need(input$gene %in% names(exprs()),"Gene not available for this dataset")
      )
    })
    
    #' busy indicator when switching surv tab
    #' http://stackoverflow.com/questions/18237987/show-that-shiny-is-busy-or-loading-when-changing-tab-panels
    output$activeTabSurv <- reactive({
      return(input$tabSurv)
    })
    outputOptions(output, 'activeTabSurv', suspendWhenHidden=FALSE)
    
    #' Set survival plot height 
    surv_plot_height <- reactive({
      if(input$allSubSurv){
        ifelse(length(subtype())>4, 1300, 650) 
      } else {
        400
      }
    })
    
    #' Create a Kaplan Meier plot with cutoff based on quantiles or manual selection
    output$survPlot <- renderPlot({     
      surv_need ()
      req(input$histology %in% c("All", histo()))   
      # Use 'try' to suppress a message throwed the first time manual cutoff is selected
      if(input$allSubSurv) {
        nrow <- ceiling(length(subtype())/2)
        par(mfrow = c(nrow,2), mar=c(3.5,3.5,3.5,1.5), mgp=c(2.2,.95,0))
        try({
          for (i in subtype()) {
            survivalPlot(surv_data(), surv_type = input$surv_type, gene = input$gene, group = input$histology, subtype = i,
                         cutoff = input$cutoff, numeric = input$mInput, cex = 1.2)
          }
        }, silent = TRUE)
      } else {
        try(survivalPlot(surv_data(), surv_type = input$surv_type, gene = input$gene, group = input$histology, subtype = input$subtype,
                         cutoff = input$cutoff, numeric = input$mInput), silent = TRUE)
      }
    }, height = surv_plot_height, width = function(){if(!input$allSubSurv) {500} else {900}})
    
    #' Download the survPlot
    output$downloadsurvPlot <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_", input$gene, "_", input$dataset, "_survPlot.", download_plot_file_type())
      },
      content = function(file) {
        plotFunction <- match.fun(download_plot_file_type())
        plotFunction(file, width = download_plot_width(), height =  download_plot_height())
        if(input$allSubSurv) {
          nrow <- ceiling(length(subtype())/2)
          par(mfrow = c(nrow,2), mar=c(3.5,3.5,3.5,1.5), mgp=c(2.2,.95,0))
          for (i in subtype()) {
            survivalPlot(surv_data(),surv_type = input$surv_type, gene = input$gene, group = input$histology, subtype = i,
                         cutoff = input$cutoff, numeric = input$mInput, cex = 1.2)
          }
        } else {
          survivalPlot(surv_data(), surv_type = input$surv_type, gene =input$gene, group = input$histology, subtype = input$subtype,
                       cutoff = input$cutoff, numeric = input$mInput)
        }
        dev.off()
      }
    ) 
    
  })
