require(shiny)
require(rhandsontable)
require(shinythemes)
source('selectCasesUI.R')
source('radioStanUI.R')

tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                 height: 80px;
                                 -webkit-column-count: 10; /* Chrome, Safari, Opera */ 
                                 -moz-column-count: 10;    /* Firefox */ 
                                 column-count: 10; 
                                 -moz-column-fill: auto;
                                 -column-fill: auto;
                                 } 
                                 ")) 
  ))

shinyUI(
  fluidPage(
    # Code to add tabs to tabsetPanels ----
    tags$head(tags$script(HTML("
                             /* In coherence with the original Shiny way, tab names are created with random numbers. 
                               To avoid duplicate IDs, we collect all generated IDs.  */
                               var hrefCollection = [];
                               
                               Shiny.addCustomMessageHandler('addTabToTabset', function(message){
                               var hrefCodes = [];
                               /* Getting the right tabsetPanel */
                               var tabsetTarget = document.getElementById(message.tabsetName);
                               
                               /* Iterating through all Panel elements */
                               for(var i = 0; i < message.titles.length; i++){
                               /* Creating 6-digit tab ID and check, whether it was already assigned. */
                               do {
                               hrefCodes[i] = Math.floor(Math.random()*100000);
                               } 
                               while(hrefCollection.indexOf(hrefCodes[i]) != -1);
                               hrefCollection = hrefCollection.concat(hrefCodes[i]);
                               
                               /* Creating node in the navigation bar */
                               var navNode = document.createElement('li');
                               var linkNode = document.createElement('a');
                               
                               linkNode.appendChild(document.createTextNode(message.titles[i]));
                               linkNode.setAttribute('data-toggle', 'tab');
                               linkNode.setAttribute('data-value', message.titles[i]);
                               linkNode.setAttribute('href', '#tab-' + hrefCodes[i]);
                               
                               navNode.appendChild(linkNode);
                               tabsetTarget.appendChild(navNode);
                               };
                               
                               /* Move the tabs content to where they are normally stored. Using timeout, because
                               it can take some 20-50 millis until the elements are created. */ 
                               setTimeout(function(){
                               var creationPool = document.getElementById('creationPool').childNodes;
                               var tabContainerTarget = document.getElementsByClassName('tab-content')[0];
                               
                               /* Again iterate through all Panels. */
                               for(var i = 0; i < creationPool.length; i++){
                               var tabContent = creationPool[i];
                               tabContent.setAttribute('id', 'tab-' + hrefCodes[i]);
                               
                               tabContainerTarget.appendChild(tabContent);
                               };
                               }, 100);
                               });
                               "))),
    theme = shinytheme("flatly"),
    navbarPage(
      "MultiSCED",
      # HOME ----
      tabPanel(
        "Home",
        fluidRow(
          HTML("<p>This app helps you to explore and analyze your single-case experimental design (SCED) data with R.</p>"),
          h4("Getting started"),
          HTML("<p>By going over the tabs in the top navigation bar one by one, you can analyze your SCED dataset step by step. Start by uploading a data file in the&nbsp;<strong>Input&nbsp;</strong>tab and assign the variables. Indicate which variables you want to include and prepare your data for analysis. Define the regression models for analysis in the&nbsp;<strong>Model&nbsp;</strong>tab. Perform a simple linear regression analysis per case in the&nbsp;<strong>One-level analysis&nbsp;</strong>tab. Perform a two-level analysis per study (if applicable) in the&nbsp;<strong>Two-level analysis&nbsp;</strong>tab. Perform a three-level (meta-)analysis in the <strong>Three-level analysis&nbsp;</strong>tab.</p>"),
          h4("How to reference this tool"),
          HTML('<p style="margin-left: 24.0pt; text-indent: -24.0pt; line-height: normal; text-autospace: none;">Declercq, L., Cools, W., Beretvas, S. N., Moeyaert, M., Ferron, J. M., &amp; Van den Noortgate, W. (2018). MultiSCED: A tool for (meta-)analyzing single-case experimental data. Manuscript in preparation.</p>'),
          h4("Acknowledgements"),
          HTML("<p>This tool is being built as part of research funded by the Institute of Education Sciences, U.S. Department of Education, grant number R305D150007. The opinions expressed are those of the authors and do not represent views of the Institute or the U.S. Department of Education.</p>")
          )
      ),
      # INPUT ----
      tabPanel(
        "Input",
        navlistPanel(
          widths = c(2,10),
          ## DATA FILE ----
          tabPanel(
             title = "Data file",
             column(4,
                    wellPanel(
                      fileInput("file", "Upload", accept="txt"),
                      checkboxInput("runExample", "Use testdata", FALSE),
                      uiOutput('select.data.separator'),
                      uiOutput('select.data.decimal')
                    )
              )
          ),
          ## VARIABLES ----
          tabPanel(
            "Variables",
            column(
              6,
              h4("Base variables"),
              wellPanel(
                uiOutput("data.select.response"),
                uiOutput("data.select.cases"),
                uiOutput("data.select.studies"),
                uiOutput("cases.check.unique"),
                fluidRow(
                  column(
                    6,
                    uiOutput("data.select.treatment")
                  ),
                  column(
                    6,
                    uiOutput("data.select.control")
                  )
                ),
                uiOutput("data.select.time"),
                uiOutput("data.check.transtime")
              )
            ),
            column(
              6,
              h4("Moderator variables"),
              wellPanel(
                rHandsontableOutput("modtab")
              )
            )
          ),
          ## DATA SUMMARY ----
          tabPanel(
            "Data summary",
            column(
              3,
              wellPanel(
                fluidRow(
                  column(6, 
                         uiOutput("data.checks.selectedStudies"),
                         actionButton("data.check.toggleStudies", label = "(De)select all")),
                  column(6,
                         uiOutput("data.checks.selectedCases")#, actionButton("data.check.toggleCases", label = "(De)select all")
                  ) 
                )
              )
            ),
            column(
              9,
              wellPanel(
                fluidRow(
                  column(8,
                         HTML('<p>Total number of studies</p>')
                  ),
                  column(4,
                         textOutput('nstudies')
                  )
                ),
                fluidRow(
                  column(8,
                         HTML('<p>Total number of cases</p>')
                  ),
                  column(4,
                         textOutput('ncases')
                  )
                ),
                fluidRow(
                  column(8,
                         HTML('<p>Total number of observations</p>')
                  ),
                  column(4,
                         textOutput('nobs')
                  )
                ) 
              ),
              uiOutput("data.link.downloadTable"),
              br(),
              DT::dataTableOutput('data.table.selected')
            )
          )
        )
      ),
      # MODEL ----
      tabPanel(
        "Model",
        navlistPanel(
          id = "Modelnavlist",
          widths = c(2,10),
          ## MODEL SPECIFICATION ----
          tabPanel(
            title = "Model specification",
            withMathJax(),
            column(4,
                   h4("Fixed effects"),
                   wellPanel(
                     uiOutput("model.check.simpleFixed"),
                     uiOutput("model.check.trendFixed"),
                     fluidRow(
                       column(4,
                              uiOutput("model.check.modFixed")
                       ),
                       column(8,
                              uiOutput("model.check.modinterFixed")
                       )
                     )
                   ),
                   h4("Random effects"),
                   wellPanel(
                     uiOutput("model.check.casesRandom"),
                     uiOutput("model.check.studiesRandom")
                   )
            ),
            column(8,
                   fluidRow(
                     column(12,
                            h4("One-level model"),
                            helpText('Used in the case-specific analysis and is estimated per case.'),
                            wellPanel(align = "center",
                                      uiOutput("model.txt.TeXformula1L")
                            ),
                            checkboxInput('checkStandardization','Standardize raw data using the RMSE of this one-level model')
                     )
                   ),
                   fluidRow(
                     column(12,
                            h4("Two-level model"),
                            helpText('Used in the study-specific meta-analysis and is estimated per study.'),
                            wellPanel(align = "center",
                                      uiOutput("model.txt.TeXformula2L1"),
                                      uiOutput("model.txt.TeXformula2L2")
                            )      
                     )
                   ),
                   fluidRow(
                     column(12,
                            h4("Three-level model"),
                            helpText('Used in the full meta-analysis.'),
                            wellPanel(align = "center",
                                      uiOutput("model.txt.TeXformula3L1"),
                                      uiOutput("model.txt.TeXformula3L2"),
                                      uiOutput("model.txt.TeXformula3L3")
                            )      
                     )
                   ),
                   fluidRow(
                     column(12,
                            h4("R formula expressions"),
                            helpText('Used in the R implementation.'),
                            verbatimTextOutput('model.txt.Rformula')
                     )
                   )
            )
          ),
          ## STANDARDIZED DATA ----
          # uiOutput("creationPool", style = "display: none;")
          tabPanel(
            title = "Standardized data",
            fluidRow(
              column(8,
                     conditionalPanel(
                       condition = "input.checkStandardization == true",
                       uiOutput('model.stan.header'),
                       helpText('Is estimated per case and is used to standardize the raw data by dividing by the RMSE.'),
                       wellPanel(align = "center",
                                 uiOutput('model.txt.TeXformula.stan')
                       )
                     )
              )
            ),
            fluidRow(
              column(12,
                     uiOutput('link.table.standardization'),
                     br(),
                     uiOutput('ui.standardization')  
              )
            )
          )
        )
      ),
      # ONE LEVEL ANALYSIS ----
      navbarMenu(
        "One-level analysis",
        ## CASE SUMMARY ----
        tabPanel(
          "Case summary",
          fluidRow(
            column(
              2,
              conditionalPanel('input["checkStandardization"]',
                               radioStanUI('case_1LA_checkStan')),
              selectCasesUI('case_1LA_choices'),
              conditionalPanel(
                condition = "input.Case_table_plot == 'Plot'",
                wellPanel(
                  uiOutput("desc.slider.caseDesign"),
                  actionButton("show.casePlots.1LA", label = "Update case plots")
                )
              )
            ),
            column(
              10,
              tabsetPanel(
                id = "Case_table_plot",
                tabPanel(
                  "Table",
                  uiOutput("link.caseTab.1LA"),
                  br(),
                  DT::dataTableOutput("desc.table.descriptives")
                ),
                tabPanel(
                  "Plot",
                  uiOutput("desc.link.showDownloadDesignPlots"),
                  plotOutput("desc.plot.caseDesign")
                )
              )
            ) 
          )
        ),
        ## REGRESSION RESULTS ----
        tabPanel(
          "Regression results",
          fluidRow(
            column(
              2,
              conditionalPanel('input["checkStandardization"]',
                               radioStanUI('regr_1LA_checkStan')),
              selectCasesUI('regr_1LA_choices'),
              conditionalPanel(
                condition = "input.Regr_table_plot == 'Plot'",
                wellPanel(
                  checkboxGroupInput('checks.regrPlot.1LA',
                                     'Intervals',
                                     choices = list('Confidence interval' = 'Confidence interval',
                                                    'Prediction interval' = 'Prediction interval')),
                  selectInput("set.alpha.regrPlot.1LA", "\\(\\alpha\\)", choices=c(".001",".01",".025",".05",".1"), selected=".05"),
                  uiOutput("slider.regrPlot.1LA"),
                  actionButton("button.regrPlot.1LA", label = "Update regression plots")
                )
              )
            ),
            column(
              2,
              wellPanel(
                HTML("<p><strong>Model</strong></p>"),
                uiOutput("model.txt.TeXformula1L1LA.1", align = "center")
              )
            ),
            column(
              8,
              tabsetPanel(
                id = "Regr_table_plot",
                tabPanel(
                  "Table",
                  uiOutput("link.regrTab.1LA"),
                  br(),
                  DT::dataTableOutput("caseEst.table.estimates")
                ),
                tabPanel(
                  "Plot",
                  uiOutput("link.regrPlot.1LA"),
                  plotOutput("plot.regrPlot.1LA")
                )
              )
            )
          )
        ),
        ## EFFECT SIZE PREDICTIONS ----
        tabPanel(
          "Effect size predictions",
          fluidRow(
            column(
              2,
              conditionalPanel('input["checkStandardization"]',
                               radioStanUI('es_1LA_checkStan')),
              selectCasesUI('es_1LA_choices'),
              conditionalPanel(
                condition = "input.Es_table_plot == 'Table'",
                wellPanel(uiOutput("casePred.slider.timePredict"))
              ),
              conditionalPanel(
                condition = "input.Es_table_plot == 'Plot'",
                wellPanel(
                  uiOutput("casePred.slider.ESplotrange"),
                  uiOutput("slider.1LAES"),
                  actionButton("button.ESPlot.1LA", label = "Update effect size prediction plots") 
                )
              )
            ),
            column(
              4,
              wellPanel(
                uiOutput("casePred.select.ESmodel"),
                uiOutput("casePred.select.alpha")
              )
            ),
            column(
              6,
              tabsetPanel(
                id = "Es_table_plot",
                tabPanel(
                  "Table",
                  uiOutput("casePred.link.downloadEffectSizes"),
                  br(),
                  tableOutput("casePred.table.effectsizes")
                ),
                tabPanel(
                  "Plot",
                  uiOutput("link.showDownload1LAES"),
                  br(),
                  plotOutput("plot.1LAES")
                )
              )
            )
          )
        )
      ),
    # TWO-LEVEL ANALYSIS ----
      tabPanel(
        "Two-level analysis",
        withMathJax(),
        column(
          2,
          wellPanel(
            uiOutput('select.study.2LA')
          ),
          conditionalPanel(
            condition = "input.TwoLA_table_plot == 'Plot'",
            wellPanel(
              sliderInput("2LAplot.height", label = "Plot height", min = 0, max = 30, value = 30),
              checkboxGroupInput('2LA.check.regression',
                                 'Regression lines',
                                 choices = list('Case (one-level analysis)' = 'Case (one-level analysis)',
                                                'Case (two-level analysis)' = 'Case (two-level analysis)',
                                                'Study (two-level analysis)' = 'Study (two-level analysis)'),
                                 selected = c('Case (one-level analysis)','Case (two-level analysis)','Study (two-level analysis)')),
              checkboxGroupInput('2LA.check.predInt',
                                 'Prediction intervals',
                                 choices = list('Case (one-level analysis)' = 'Case (one-level analysis)',
                                                'Case (two-level analysis)' = 'Case (two-level analysis)',
                                                'Study (two-level analysis)' = 'Study (two-level analysis)')),
              selectInput("set.alpha.regrPlot.2LA", "\\(\\alpha\\)", choices=c(".001",".01",".025",".05",".1"), selected=".05"),
              sliderInput("n.sims.2LA", label = "Number of simulations", min = 0, max = 5000, value = 1000),
              actionButton("button.regrPlot.2LA", label = "Update regression plots")
            )
          )
        ),
        column(
          2,
          wellPanel(
            HTML("<p><strong>Model</strong></p>"),
            uiOutput("model.txt.TeXformula2L1.2LA", align = "center"),
            uiOutput("model.txt.TeXformula2L2.2LA", align = "center")
          )
        ),
        column(
          8,
          tabsetPanel(
            id = "TwoLA_table_plot",
            tabPanel(
              "Table",
              #verbatimTextOutput("mytest"),
              HTML("<p><strong>Fixed effects</strong></p>"),
              verbatimTextOutput("fixed2LA"),
              HTML("<p><strong>Random effects</strong></p>"),
              verbatimTextOutput("random2LA")
            ),
            tabPanel(
              "Plot",
              uiOutput("link.regrPlot.2LA"),
              br(),
              plotOutput("plot.regrPlot.2LA")
            )
          )
        )
      ),
    # THREE-LEVEL ANALYSIS ----
      tabPanel(
        "Three-level analysis",
        withMathJax(),
        conditionalPanel(
          condition = "input.ThreeLA_table_plot == 'Plot'",
          column(
            2,
            # wellPanel(
            #   #style = "overflow: hidden;",
            #   selectCasesUI('choices_3LA')
            # ),
            selectCasesUI('choices_3LA'),
            wellPanel(
              sliderInput("3LAplot.height", label = "Plot height", min = 0, max = 30, value = 30),
              checkboxGroupInput('3LA.check.regression',
                                 'Regression lines',
                                 choices = list('Case (one-level analysis)' = 'Case (one-level analysis)',
                                                'Case (two-level analysis)' = 'Case (two-level analysis)',
                                                'Study (two-level analysis)' = 'Study (two-level analysis)',
                                                'Case (three-level analysis)' = 'Case (three-level analysis)',
                                                'Study (three-level analysis)' = 'Study (three-level analysis)',
                                                'Overall (three-level analysis)' = 'Overall (three-level analysis)'),
                                 selected = c('Case (three-level analysis)' = 'Case (three-level analysis)',
                                              'Study (three-level analysis)' = 'Study (three-level analysis)',
                                              'Overall (three-level analysis)' = 'Overall (three-level analysis)')),
              checkboxGroupInput('3LA.check.predInt',
                                 'Prediction intervals',
                                 choices = list('Case (one-level analysis)' = 'Case (one-level analysis)',
                                                'Case (two-level analysis)' = 'Case (two-level analysis)',
                                                'Study (two-level analysis)' = 'Study (two-level analysis)',
                                                'Case (three-level analysis)' = 'Case (three-level analysis)',
                                                'Study (three-level analysis)' = 'Study (three-level analysis)',
                                                'Overall (three-level analysis)' = 'Overall (three-level analysis)')),
              selectInput("set.alpha.regrPlot.3LA", "\\(\\alpha\\)", choices=c(".001",".01",".025",".05",".1"), selected=".05"),
              sliderInput("n.sims.3LA", label = "Number of simulations", min = 0, max = 5000, value = 1000),
              actionButton("button.regrPlot.3LA", label = "Update regression plots")
            )
          )
        ),
        column(
          2,
          wellPanel(
            HTML("<p><strong>Model</strong></p>"),
            uiOutput("model.txt.TeXformula3L1.3LA", align = "center"),
            uiOutput("model.txt.TeXformula3L2.3LA", align = "center"),
            uiOutput("model.txt.TeXformula3L3.3LA", align = "center")
          )
        ),
        column(
          8,
          tabsetPanel(
            id = "ThreeLA_table_plot",
            tabPanel(
              "Table",
              HTML("<p><strong>Fixed effects</strong></p>"),
              verbatimTextOutput("fixed3LA"),
              HTML("<p><strong>Random effects</strong></p>"),
              verbatimTextOutput("random3LA")
            ),
            tabPanel(
              "Plot",
              uiOutput("link.regrPlot.3LA"),
              br(),
              plotOutput("plot.regrPlot.3LA")
            )
          )
        )
      ),
    # ABOUT -----
      tabPanel(
        "About",
        h4("Documentation"),
        HTML('<p>A full and detailed overview of the MultiSCED functionality is illustrated in the <a href="https://kuleuven.box.com/v/MultiSCEDUserGuide">MultiSCED User Guide</a>. The illustration is based on a real data example by Shogren, Faggella-Luby, Bae and Wehmeyer (2004) and the dataset can be downloaded <a href="https://kuleuven.box.com/v/Shogren2004">here</a>.</p>'),
        h4("Contact"),
        HTML('<p><a href="https://www.kuleuven.be/wieiswie/c/nl/person/00106028">Lies Declercq</a><br /><a href="https://www.kuleuven.be/wieiswie/nl/person/00006844">Wim Van den Noortgate</a></p>'),
        h4('References'),
        HTML('<p style="margin-left: 24.0pt; text-indent: -24.0pt; line-height: normal; text-autospace: none;">Declercq, L., Cools, W., Beretvas, S. N., Moeyaert, M., Ferron, J. M., &amp; Van den Noortgate, W. (2018). MultiSCED: A tool for (meta-)analyzing single-case experimental data. Manuscript in preparation.</p>
<p style="margin-left: 24.0pt; text-indent: -24.0pt; line-height: normal; text-autospace: none;">Shogren, K. A., Faggella-Luby, M. N., Bae, S. J., &amp; Wehmeyer, M. L. (2004). The effect of choice-making as an intervention for problem behavior: a meta-analysis. <em>Journal of Positive Behavior Interventions</em>, <em>6</em>(4), 228&ndash;237. <a href="http://doi.org/10.1177/10983007040060040401">http://doi.org/10.1177/10983007040060040401</a></p>')
      )# > tabPanel
    ) # > navbarPage
  ) # > fluidPage
) # > shinyUI