# UI-elements for Explore tab
tabPanel(title = "Explore", icon = icon("picture-o"), id = "explore",
         
         tagList(
           tags$head(
             includeScript("tools/google-analytics.js"),
             tags$script(type="text/javascript", src = "busy.js"),
             tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
           )
         ),
         
         sidebarLayout(fluid = FALSE,
                       sidebarPanel(
                         wellPanel(
                           h4(class = "outer", "How does it work?"),
                           tags$ol(
                             tags$li("Select a Platform or Data Source"),
                             tags$li('Enter', a("a HGNC-approved", href="http://www.genenames.org"), 'Gene Symbol'),
                             tags$li("Choose one of the available plots ")
                           ),
                           hr(),
                           #' dataset
                           radioButtons(inputId ="datasetType", label = h4("Visualize by:"), choices = c("Platform","DataSource"), selected = "Platform", inline = T),
                           conditionalPanel(
                             condition = "input.datasetType == 'DataSource'",
                             selectInput(inputId = "dataset", label = h4("DataSource"), 
                                         choices = c("Korean_SMC","Philips_PMID16530701","PMID26466313","TCGA_GBM","TCGA_LGG","HF_MDA","Costello_PMID24336570"), 
                                         selected = NULL)
                             # selectizeInput(inputId = "dataset", label = "DataSource", choices = NULL, selected = NULL)
                           ),
                           conditionalPanel(
                             condition = "input.datasetType == 'Platform'",
                             selectInput(inputId = "platform", label = h4("Platform:"), 
                                         choices = c("RSEQ","AffymetrixU133A","IlluminaHumanHT12V4"), 
                                         selected = NULL)
                           ),
                           #' genes
                           selectizeInput(inputId = "gene", label = h4("Gene:"), choices = NULL,  
                                          options = list(placeholder = "Enter gene, eg: EGFR", plugins = list('restore_on_backspace'))),
                           # Tab expression
                           conditionalPanel(
                             condition = "input.tab1 == 1",
                             #' plot type
                             selectInput(inputId = "plotType", label = h4("Plot:"), choices = c("Progression","Pathology","Subtype", "GcimpPrediction")),
                             conditionalPanel(
                               condition = "input.plotType == 'Progression'",
                               radioButtons(inputId ="point_line", label = h4("Type:"), choices = c("Box plot","Lines", "Scatter plot"), selected = "Box plot", inline = T)
                             ),
                             div(class="row",
                                 div(class="col-xs-6",
                                     selectInput("colorP",  label = h4("Color by:"), choices = c("None","GcimpPrediction","Pathology","Subtype","Platform","DataSource"),
                                                 selected = "GcimpPrediction")
                                 ),
                                 conditionalPanel(
                                   condition = "input.point_line != 'Scatter plot'",
                                   div(class="col-xs-6",
                                       selectInput("shapeP",  label = h4("Shape by:"), choices = c("None","GcimpPrediction","Pathology","Subtype","Platform","DataSource"),
                                                   selected = "None")
                                   )
                                 )
                             )
                             
                           ),
                           # Tab Survival
                           conditionalPanel(
                             condition = "input.tab1 == 2 && input.tabSurv =='By Gene'",
                             selectInput(inputId = "histology", label = h4("Histology:"), choices = ""),
                             selectInput(inputId = "subtype", label = h4("Subtype:"), choices = ""),
                             checkboxInput(inputId = "gcimpSurv", label = "Exclude G-CIMP samples", value = FALSE),
                             conditionalPanel(
                               condition = "input.subtype == 'All'",
                               checkboxInput(inputId = "allSubSurv", label = "Separate by subtype", value = FALSE)
                             ),
                             
                             radioButtons(inputId ="surv_type", label = h4("Survival:"), choices = c("Overall","Progression free"), selected = "Overall", inline = T),
                             selectInput(inputId = "cutoff", label = h4("Cutoff:"), 
                                         choices = c("Use a specific mRNA value", "median", "lower quartile", "upper quartile", "high vs low", "quartiles"),
                                         selected = "median"),
                             conditionalPanel(
                               condition = "input.cutoff == 'Use a specific mRNA value'",
                               br(),
                               uiOutput("numericCutoff"),
                               plotOutput(outputId = "boxmRNA", width = "100%", height = 50),
                               helpText("mRNA expression. Blue lines represent 25%, 50% and 75% quartiles. Red line represents the current selection.")
                             )
                             # )
                           )
                         ),
                         
                         conditionalPanel(
                           condition = "input.tab1 == 1",                         
                           # Tab Expression plotting options
                           wellPanel( 
                             h4("Statistic:"),
                             checkboxInput(inputId = "statSummary", label = "Summary statistics", value = FALSE),
                             conditionalPanel(
                               condition = "input.point_line != 'Scatter plot'",
                               conditionalPanel(
                                 condition = "input.plotType == 'Progression'",
                                 checkboxInput(inputId = "paired_tTest", label = "Paired t-test", value = FALSE)
                               ),
                               checkboxInput(inputId = "tukeyHSD", label = "Tukey's HSD", value = FALSE),
                               checkboxInput(inputId = "tTest", label = "Pairwise t tests", value = FALSE)
                             )
                           ),
                           wellPanel( 
                             h4("Plot options:"),
                             p(style = "background-color: #222222; padding-left:10px; border: 1px solid;color: #ffffff;",
                               strong("Plot size")),
                             div(class="row",
                                 div(class="col-xs-6",
                                     numericInput("plot_height", label = "Height (pixels):", min = 100, max = 2000, step = 50, value = 400)
                                 ),
                                 div(class="col-xs-6",
                                     numericInput("plot_width", label = "Width (pixels):", min = 100, max = 2000, step = 50, value = 600)
                                 )
                             ),
                             p(""),      
                             p(style = "background-color: #222222; padding-left:10px; border: 1px solid;color: #ffffff;",
                               strong("Points appearance")),
                             div(class="row",
                                 div(class="col-xs-6",
                                     numericInput(inputId = "point_size",label = "Size", 
                                                  value = 2, min = 0, max = 5, step = 0.5)
                                 ),
                                 div(class="col-xs-6",
                                     numericInput(inputId = "alpha",label = "Transparency",
                                                  value = 0.5, min = 0, max = 1, step = 0.1)
                                 )
                             ),
                             p(style = "background-color: #222222; padding-left:10px; border: 1px solid;color: #ffffff;",
                               strong("Axis labels")),
                             div(class="row",
                                 div(class="col-xs-6",
                                     textInput(inputId = "myXlab", label = "X-axis label:", value = "")
                                 ),
                                 div(class="col-xs-6",
                                     numericInput(inputId = "xaxisLabelAngle",label = "X-axis angle",
                                                  value = 0, min = 0, max = 90, step = 15)
                                 )
                             ),
                             textInput(inputId = "myYlab", label = "Y-axis label:", value = "mRNA expression \n"),
                             div(class="row",
                                 div(class="col-xs-6",
                                     numericInput(inputId = "axis_text_size",label = "Axis text (pt)",
                                                  value = 18, min = 0, max = 25, step = 1)
                                 ),
                                 div(class="col-xs-6",
                                     numericInput(inputId = "axis_title_size",label = "Axis title (pt)",
                                                  value = 18, min = 0, max = 25, step = 1)
                                 )
                             )
                           )
                         ),
                         
                         
                         # Allow the user to set the height and width of the plot download.
                         wellPanel( 
                           h4("Download:"),
                           selectInput(inputId = "downloadPlotFileType", label = strong("Select download file type"),
                                       choices = list("PDF"  = "pdf", "BMP"  = "bmp", "JPEG" = "jpeg", "PNG"  = "png")
                           ),
                           strong("Set download image dimensions"),
                           helpText("(units are inches for PDF, pixels for all other formats)"),
                           div(class="row",
                               div(class="col-xs-6",
                                   numericInput(inputId = "downloadPlotHeight", label = "Height (inches)", 
                                                value = 7, min = 1, max = 100)
                               ),
                               div(class="col-xs-6",
                                   numericInput(inputId = "downloadPlotWidth", label = "Width (inches)", 
                                                value = 7, min = 1, max = 100)
                               )
                           ),
                           br(),
                           #Download buttons
                           conditionalPanel(
                             condition = "input.tab1 == 1",
                             downloadButton(outputId = "downloadPlot", label = "Download", class= "btn-primary")
                           ),
                           conditionalPanel(
                             condition = "input.tab1 == 2",
                             downloadButton(outputId = "downloadsurvPlot", label = "Download", class= "btn-primary")
                           )
                         )
                       ),
                       
                       
                       mainPanel(
                         tabsetPanel(id = "tab1",
                                     tabPanel(title = "Expression", icon = icon("bar-chart-o"), value = 1,
                                              plotOutput(outputId = "plot",height = "100%"),
                                              br(),
                                              conditionalPanel(
                                                condition = "input.statSummary",
                                                p(style = "background-color: #F5F5F5; padding-left:10px; border: 1px solid #E3E3E3;", 
                                                  strong("Summary statistics")),
                                                tableOutput(outputId = "summary"),
                                                hr()
                                              ),
                                              conditionalPanel(
                                                condition = "input.paired_tTest",
                                                p(style = "background-color: #F5F5F5; padding-left:10px; border: 1px solid #E3E3E3;", 
                                                  strong("Paired t-test")),
                                                tableOutput(outputId = "pairedTtest"),
                                                hr()
                                              ),
                                              conditionalPanel(
                                                condition = "input.tukeyHSD",
                                                p(style = "background-color: #F5F5F5; padding-left:10px; border: 1px solid #E3E3E3;",
                                                  strong("Tukey's Honest Significant Difference (HSD)")),
                                                helpText("The table shows the difference between pairs, the 95% confidence interval and the p-value of the pairwise comparisons:"),
                                                checkboxInput(inputId = "tukeyPlot", label = "Show the results in the plot", value = FALSE),
                                                tableOutput(outputId = "tukeyTest"),
                                                hr()
                                              ),
                                              conditionalPanel(
                                                condition = "input.tTest",
                                                p(style = "background-color: #F5F5F5; padding-left:10px; border: 1px solid #E3E3E3;",
                                                  strong("Pairwise t tests")),
                                                helpText("Pairwise comparisons between group levels with corrections for multiple testing (p-values with Bonferroni correction):"),
                                                tableOutput(outputId = "pairwiseTtest")
                                              )
                                     ),
                                     
                                     tabPanel(title = "Survival", icon =  icon("user-md"), value = 2,
                                              tabsetPanel(id = "tabSurv",
                                                          tabPanel(title = "By Gene",
                                                                   p(class = "lead","Kaplan-Meier estimator survival analysis"),
                                                                   plotOutput(outputId = "survPlot", height = "100%")
                                                          ),
                                                          tabPanel(title = "By Subtype",
                                                                   helpText("We apologize, but this feature is still not currently available")
                                                          )
                                              )
                                     )
                         )
                         
                       )
         )
)
