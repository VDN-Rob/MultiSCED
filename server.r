# Setup environment to run the app -----
pckgs <- c('shiny','reshape2','ggplot2','plyr','arm','merTools','rhandsontable','foreach','DT')
todo <- pckgs[!is.element(pckgs, names(installed.packages()[,"Package"]))]
if(length(todo) > 0) install.packages(todo)  #,repos="http://cran.us.r-project.org")

library(shiny)
library(ggplot2)
library(plyr)
library(arm)
library(reshape2)
library(merTools)
library(rhandsontable)
library(foreach)
library(DT)
source('selectCases.R')
source('radioStan.R')

# Shiny server -----
shinyServer(function(input,output,session){
  
  # Input ----
  ## Data file ----
  
  uploadData <- eventReactive(input$file,{
    if(is.null(input$file)){		NULL		}
    else{
      if(input$runExample)	..dta <- read.table(file="testData.txt",sep="\t",dec=".",header=T)
      else ..dta <- read.table(file = input$file$datapath,
                               sep = input$separator,
                               dec = input$decimal,
                               header = T)			
      ..dta
    }
  })

  output$select.data.separator <- renderUI({
    validate(need(input$file, ''))
    selectInput("separator", 
                "Separator character", 
                choices = list("tab" = "\t", "comma" = ",", "semicolon" = ";", "space" = " "),
                selected = "\t")
  })
  output$select.data.decimal <- renderUI({
    validate(need(input$file, ''))
    selectInput("decimal", 
                "Decimal character", 
                choices = list("dot" = ".","comma" = ","), 
                selected = ".")
  })
  
  ## Variables ----

  getVarnames = reactive({
    validate(	    need(input$file, '')	  )
    names(uploadData())
  })
  checkUniqueCases = reactive({
    validate(   
      need(input$file, ''),
      need(input$cases != "---", ''),
      need(input$studies != "---", '')
    )
    ..dta = uploadData()
    cases = unique(..dta[,c(input$studies,input$cases)])[,input$cases]
    anyDuplicated(cases) > 0
    # boolean = anyDuplicated(cases) > 0
    # if(boolean){
    #   ..dta[,input$cases] = factor(paste0(..dta[,input$studies],"_",..dta[,input$cases]))
    #   uniquecases = 
    #   list('check' = boolean,
    #        'data' = ..dta)
    #   }
    # else{list('check' = boolean)}
  })
  getModerators = reactive({
    full = colnames(uploadData())
    mods = full[!(full %in% c(input$studies,
                              input$cases,
                              input$response,
                              input$treatment,
                              input$time
                              ))]
    mods
  })
  getModDF = reactive({
    data.frame('Variable' = as.character(getModerators()),
               'Include' = rep(F, length(getModerators())),
               'Type' = factor(rep('factor', length(getModerators())), levels = c('factor','numeric')),
               stringsAsFactors = F
    )
  })
  
  output$data.select.response <- renderUI({
    validate(      need(input$file, '')	  )
    if(input$runExample) selectInput("response", "Response", choices=c("---",setdiff(getVarnames(), "---")), selected="Y")
    else selectInput("response", "Response", choices=c("---",setdiff(getVarnames(), "---")))
  })
  output$data.select.cases <- renderUI({
    validate(      need(input$file, '')	  )
    if(input$runExample) selectInput("cases", "Case", choices=c("---",setdiff(getVarnames(), "---")), selected="Name")
    else selectInput("cases", "Case", choices=c("---",setdiff(getVarnames(), "---")))
  })
  output$data.check.concatenate <- renderUI({
    validate(   
      need(input$file, ''),
      need(input$time != "---", '')    
    )
    checkboxInput("concatenate", "Concatenate study names to case names", FALSE)
  })
  output$cases.check.unique = renderUI({
    validate(   
      need(input$file, ''),
      need(input$cases != "---", '')    
    )
    if(checkUniqueCases()){
      HTML("<p>
           This case variable does not have uniquely identifiable case values. The case variable will be transformed to ensure each case has a unique ID.
           </p>")
    }
    else{
      checkboxInput("concatenate", "Concatenate study names to case names", FALSE)
    } 
    })
  output$data.select.studies <- renderUI({
    validate(      need(input$file, '')	  )
    if(input$runExample) selectInput("studies", "Study", choices=c("---",setdiff(getVarnames(), "---")), selected="Author")
    else selectInput("studies", "Study", choices=c("---",setdiff(getVarnames(), "---")))
  })
  output$data.select.treatment <- renderUI({
    validate(      need(input$file, '')	  )
    if(input$runExample) selectInput("treatment", "Phase", choices=c("---",setdiff(getVarnames(), "---")), selected="Phase")
    else selectInput("treatment", "Phase", choices=c("---",setdiff(getVarnames(), "---")))
  })
  output$data.select.control <- renderUI({
    validate(      need(input$treatment, '')	  )
    ..choices <- c("---")
    if(input$treatment != "---") ..choices <- c(unique(uploadData()[,input$treatment]))
    if(input$runExample) selectInput("control", "Phase control group", choices=..choices, selected="0")
    else selectInput("control", "Phase control group", choices=..choices)
  })
  output$data.select.time <- renderUI({
    validate(      need(input$file, '')	  )
    if(input$runExample) selectInput("time", "Time", choices=c("---",setdiff(getVarnames(), "---")), selected="Time")
    else selectInput("time", "Time", choices=c("---",setdiff(getVarnames(), "---")))
  })
  output$data.check.transtime <- renderUI({
    validate(   
      need(input$file, ''),
      need(input$time != "---", '')    
    )
    checkboxInput("transtime", "Center time variable", FALSE)
  })
  output$modtab = renderRHandsontable({
    if(nrow(getModDF())>0){
      DF = getModDF()
      rhandsontable(DF, height = 500, stretchH = "all", rowHeaders = NULL) %>%
        hot_col("Variable", readOnly = T)
    }
    else NULL
  })

  ## Data summary ----
  
  asFunc = function(class, x){
    if(class == 'numeric') x = as.numeric(x)
    if(class == 'factor') x = as.factor(x)
    x
  }
  centerTime = function(..tdta){
    ..nr <- min(..tdta[..tdta[,input$treatment]!=input$control,input$time])
    ..tdta[,input$time] <- ..tdta[,input$time] - ..nr
    return(..tdta)
  }
  
  casesInSelectedStudies = reactive({
    ..dta <- getUnStanData()
    selectedStudies = input$selectStudies
    if(is.null(input$selectStudies)) NULL
    else as.character(unique(..dta[..dta[[input$studies]] %in% selectedStudies,input$cases]))
  })
  getModTab = reactive({
    if(!is.null(input$modtab)){
      hot_to_r(input$modtab)
    }
    else NULL
  })
  getUnStanData = reactive({
    validate(
      need(input$file, 'Please upload a data file'),
      need(input$response, 'Please select a response variable'),
      need(input$treatment, 'Please select a phase variable'),
      need(input$studies, 'Please select a study variable'),
      need(input$cases, 'Please select a case variable'),
      need(input$time, 'Please select a time variable')
    )
    ..dta <- uploadData()
    if(input$transtime){
      ..dta <- ddply(..dta,input$cases,function(.x) centerTime(.x))
    }
    if(checkUniqueCases() & input$cases != "---" & input$studies != "---"){
      ..dta[,input$cases] = factor(paste0(..dta[,input$studies],"_",..dta[,input$cases]))
    }
    if(!checkUniqueCases()){
      if(input$concatenate & input$cases != "---" & input$studies != "---") ..dta[,input$cases] <- factor(paste0(..dta[,input$studies],"_",..dta[,input$cases]))
    }
    ..tmp <- ..dta[,input$treatment]
    ..dta[,input$treatment] <- factor(..tmp,levels=c(input$control,unique(..tmp)[!unique(..tmp)%in%input$control]))
    ..dta[,input$response] <- as.numeric(..dta[,input$response])
    ..dta[,input$time] <- as.numeric(..dta[,input$time])
    ..dta = droplevels(..dta)
    ..dta$obs = c(1:nrow(..dta))
    ..dta[[input$studies]] = as.factor(..dta[[input$studies]])
    ..dta[[input$cases]] = as.factor(..dta[[input$cases]])
    ..dta[[input$treatment]] = as.factor(..dta[[input$treatment]])
    ..dta[[input$response]] = as.numeric(..dta[[input$response]])
    ..dta[[input$time]] = as.numeric(..dta[[input$time]])
    if(is.null(getModTab())){
      ..ndta = ..dta[,colnames(..dta) %in% c(input$studies,
                                             input$cases,
                                             input$response,
                                             input$treatment,
                                             input$time,
                                             'obs')]
      ..ndta = ..ndta[c('obs',input$studies,input$cases,input$treatment,input$time,input$response)]
    }
    if(!is.null(getModTab())){
      ..modtab = getModTab()
      ..modtab = ..modtab[..modtab$Include == T,]
      ..modvars = as.character(..modtab[,'Variable'])
      ..ndta = ..dta[,colnames(..dta) %in% c(..modvars,c(input$studies,
                                                         input$cases,
                                                         input$response,
                                                         input$treatment,
                                                         input$time,
                                                         'obs'))]
      for(col in colnames(..ndta)){
        if(col %in% ..modvars) ..ndta[[col]] = asFunc(as.character(..modtab[..modtab$Variable == col,'Type']),..ndta[[col]])
        else ..ndta[[col]] = ..ndta[[col]]
      }
      ..ndta = ..ndta[c('obs',input$studies,input$cases,input$treatment,input$time,input$response,
                        colnames(..ndta)[!(colnames(..ndta) %in% c('obs',input$studies,input$cases,input$treatment,input$time,input$response))])]
    }
    ..ndta
  })
  getSelectedData = reactive({
    validate(
      need(input$selectStudies, 'Please select at least one study to include'),
      need(input$selectCases, 'Please select at least one case to include')
    )
    ..dta = getUnStanData()
    ..dta[..dta[[input$cases]] %in% input$selectCases,]
  })
  
  output$data.checks.selectedStudies <- renderUI({
    validate(      
      need(input$file, ''),
      need(input$studies != "---", 'Please select a study variable.')    
    )
    ..dta <- uploadData()
    ..sdta <- ..dta
    checkboxGroupInput("selectStudies", "Studies", choices=as.character(unique(..dta[,input$studies])), selected=as.character(unique(..sdta[,input$studies])))
  })
  output$data.checks.selectedCases <- renderUI({
    validate(   
      need(input$file, ''),
      need(input$cases != "---", 'Please select a case variable.')#,
      #need(casesInSelectedStudies(), 'Please select at least one study to include.')
    )
    checkboxGroupInput("selectCases", 
                       "Cases", 
                       choices = casesInSelectedStudies(),
                       selected = casesInSelectedStudies()) 
  })
  output$ncases <- renderText({
    ..dta = getSelectedData()
    length(unique(..dta[[input$cases]]))
  })
  output$nstudies <- renderText({
    ..dta = getSelectedData()
    length(unique(..dta[[input$studies]]))
  })
  output$nobs <- renderText({
    ..dta = getSelectedData()
    nrow(..dta)
  })
  output$data.link.downloadTable <- renderUI({
    downloadLink('downloadDataTable', 'Download table (tab delimited .txt)')
  })
  output$data.table.selected <- DT::renderDataTable({
    ..dta = getSelectedData()
    ..dta[ , !(names(..dta) == 'obs')]
  })
  
  output$downloadDataTable <- downloadHandler(
    filename = function() {
      paste('Data_', format(Sys.time(), "%d-%b_%Hh%M"), '.txt', sep='')
    },
    content = function(file) {
      write.table(getSelectedData(), file, sep="\t", row.names=F)
    }
  )	

  # Model ----
  ## Model specification ----
  
  observeEvent(input$modFixed,{
    if(length(input$modFixed) > 0){
      updateCheckboxInput(session,
                          'checkStandardization',
                          'Standardize raw data using the RMSE of this one-level model without moderators')
    }
  })
  
  getCombos = function(mod,basevars){
    combos = c()
    nmods = length(mod)
    if(nmods > 1){
      modcombos = c()
      for(n in 1:nmods){
        modcombos = c(modcombos,unlist(lapply(combn(mod,n,simplify=F),function(.x) paste(.x,collapse=' \\(\\times\\) '))))
      }
      for(combo in modcombos){
        combos = c(combos,paste(combo,basevars,sep=' \\(\\times\\) '))
      }
      setdiff(c(modcombos,combos),mod)
    }
    else{
      paste(mod,basevars,sep=' \\(\\times\\) ')
    }
  }
  getRE2L2formula = function(.x){
    n = which(getAllSelectedPredictors() == .x)-1
    paste0('\\beta_{',
           n,
           "j} &=& \\gamma_{",
           n,
           "0} + v_{",
           n,
           "j}")
  }
  getRE3L2formula = function(.x){
    n = which(getAllSelectedPredictors() == .x)-1
    paste0('\\beta_{',
           n,
           "jk} &=& \\theta_{",
           n,
           "0k} + u_{",
           n,
           "jk}")
  }
  getRE3L3formula = function(.x){
    n = which(getAllSelectedPredictors() == .x)-1
    paste0('\\theta_{',
           n,
           "0k} &=& \\gamma_{",
           n,
           "00} + v_{",
           n,
           "0k}")
  }
  TeXtoR = function(pred){
    if(grepl('\\(\\times\\)', pred, fixed = T)){
      pred = gsub(' \\(\\times\\) ',':',pred, fixed = T)
    }
    if(grepl('\\(\\left(\\right.\\)', pred, fixed = T) & grepl('\\(\\left.\\right)^2\\)', pred, fixed = T)){
      pred = gsub('\\(\\left(\\right.\\)','I(',pred, fixed = T)
      pred = gsub('\\(\\left.\\right)^2\\)','^2)',pred, fixed = T)
    }
    if(grepl('intercept', pred, fixed = T)){
      pred = gsub('intercept','1',pred, fixed = T)
    }
    pred
  }
  addTabToTabset <- function(Panels, tabsetName){
    titles <- lapply(Panels, function(Panel){return(Panel$attribs$title)})
    Panels <- lapply(Panels, function(Panel){Panel$attribs$title <- NULL; return(Panel)})
    
    output$creationPool <- renderUI({Panels})
    session$sendCustomMessage(type = "addTabToTabset", message = list(titles = titles, tabsetName = tabsetName))
  }
  
  getSimpleModelPredictors <- reactive({
    validate( 
      need(input$file, ''),
      need(input$time != "---", ''),
      need(input$treatment != "---", '') 
    )
    c('intercept',input$time,input$treatment,paste0(input$treatment, " \\(\\times\\) ", input$time))
  })
  getModeratorsNames <- reactive({
    validate(
      need(!is.null(getModTab()), 'No moderator variables defined.')
    )
    if(!is.null(getModTab())){
      ..modtab = getModTab()
      ..modtab = ..modtab[..modtab$Include == T,]
      ..modvars = as.character(..modtab[,'Variable'])
      ..modvars
    }
  })
  getPower2 <- reactive({
    out <- ""
    if(length(getSimpleSelectedPredictors())>0){
      if(input$time %in% getSimpleSelectedPredictors())	out <- paste0("\\(\\left(\\right.\\)",input$time,"\\(\\left.\\right)^2\\)")
      if(all(c(input$time,input$treatment) %in% getSimpleSelectedPredictors())) out <- c(out,paste0(input$treatment," \\(\\times\\) ","\\(\\left(\\right.\\)",input$time,"\\(\\left.\\right)^2\\)"))
    }
  })
  getSimpleSelectedPredictors <- reactive({
    validate(      need(input$file, '')	  )
    input$simpleFixed    
  })  
  getAllSelectedPredictors <- reactive({
    validate(      need(input$file, '')	  )
    c(input$simpleFixed,input$trendFixed,input$modFixed,input$modinterFixed)    
  })
  getAllSelectedREs <- reactive({
    validate(      need(input$file, '')	  )
    list('case_level' = c(input$varCases),
         'study_level' = c(input$varStudies))
  })
  getModInter = reactive({
    validate(
      need(!is.null(input$modFixed),'')
    )
    simple_trend = c(input$simpleFixed,input$trendFixed)
    simple_trend = simple_trend[grepl(input$treatment,simple_trend)]
    #simple_trend = simple_trend[!grepl(paste0('^',input$treatment,'$'),simple_trend)]
    #combos = lapply(combn(c(simple_trend,input$modFixed),2,simplify=F),function(.x) paste(.x,collapse = " \\(\\times\\) "))
    getCombos(input$modFixed,simple_trend)
  })
  getStanModel = reactive({
    validate( need(input$file, 'Please upload a data file.') )
    if(length(getAllSelectedPredictors())==0) ..mdl <- paste(input$response,"~ 1")
    else{
      predictors = sapply(c(input$simpleFixed,input$trendFixed), TeXtoR)
      ..mdl <- paste(input$response, "~", paste(predictors,collapse=" + "))
    }
    ..mdl
  })
  get1LRModel = reactive({
    validate( need(input$file, 'Please upload a data file.') )
    if(length(getAllSelectedPredictors())==0) ..mdl <- paste(input$response,"~ 1")
    else{
      predictors = sapply(getAllSelectedPredictors(), TeXtoR)
      ..mdl <- paste(input$response, "~", paste(predictors,collapse=" + "))
    }
    ..mdl
  })
  get2LRModel = reactive({
    validate( need(input$file, 'Please upload a data file.') )
    if(length(input$varCases) == 0) get1LRModel()
    else{
      REs = sapply(input$varCases, TeXtoR)
      TwoLRModel = paste0("(",paste(REs,collapse = " + ")," | ",input$cases,")")
      paste(get1LRModel(),TwoLRModel,sep = " + ") 
    }
  })
  get3LRModel = reactive({
    validate( need(input$file, 'Please upload a data file.') )
    if(length(input$varStudies) == 0) get2LRModel()
    else{
      casREs = sapply(input$varCases, TeXtoR)
      stuREs = sapply(input$varStudies, TeXtoR)
      ThreeLRModelcas = paste0("(",paste(casREs,collapse = " + ")," | ",input$studies,":",input$cases,")")
      ThreeLRModelstu = paste0("(",paste(stuREs,collapse = " + ")," | ",input$studies,")")
      ThreeLRModel = paste(ThreeLRModelstu,ThreeLRModelcas,sep = " + ") 
      paste(get1LRModel(),ThreeLRModel,sep = " + ") 
    }
  })
  getStanDataDisp = reactive({
    validate(need(input$checkStandardization,''))
    ..dta = getSelectedData()
    ..lmL <- dlply(..dta,c(input$cases),function(.x) list(data=.x,lm=lm(as.formula(getStanModel()),data=.x)))
    ..sigmas = ldply(..lmL, function(.x) summary(.x$lm)$sigma)
    ..sigmas$sigma = ..sigmas[,2]
    ..sigmas = ..sigmas[,c(1,3)]
    response_st = paste0(input$response,'_st')
    ..dta[[response_st]] = round(apply(..dta, 1, function(.x, .sigmas){as.numeric(.x[[input$response]])/.sigmas[.sigmas[,input$cases] == .x[[input$cases]],'sigma']}, .sigmas = ..sigmas),2)
    ..dta
  })
  getFinalData = reactive({
    ..dta = getSelectedData()
    if(input$checkStandardization){
      ..dta = getStanDataDisp()
      response_st = paste0(input$response,'_st')
      ..dta[[input$response]] = ..dta[[response_st]]
    }
    ..dta
  })
  
  output$model.check.simpleFixed <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", '')    
    )
    checkboxGroupInput("simpleFixed", "Base variables", 
                       choices=as.list(c(getSimpleModelPredictors())), 
                       selected=as.list(c('intercept',input$treatment))
    )
  })
  output$model.check.trendFixed <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$time %in% input$simpleFixed, '')
    )
    withMathJax(
      checkboxGroupInput("trendFixed", "Quadratic time trend", 
                         choices=as.list(c(getPower2()))) 
    )
  })
  output$model.check.modFixed <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", '')    
    )
    withMathJax(
      checkboxGroupInput("modFixed", "Moderators", choices=as.list(c(getModeratorsNames()))) 
    )
  })
  output$model.check.modinterFixed = renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", '')    
    )
    withMathJax(
      checkboxGroupInput("modinterFixed", "", choices=as.list(c(getModInter()))) 
    )
  })
  output$model.check.casesRandom <- renderUI({
    validate(      
      need(input$file, ''),
      need(input$cases != "---", ''),
      need(input$response != "---", '')
    )
    withMathJax(
      checkboxGroupInput("varCases", "Case level",
                         choices=as.list(c(setdiff(getAllSelectedPredictors(),c(input$modFixed,input$modinterFixed))))
      ) 
    )
  }) 
  output$model.check.studiesRandom <- renderUI({
    validate(      
      need(input$file, ''),
      need(input$studies != "---", '')    
    )
    withMathJax(
      checkboxGroupInput("varStudies", "Study level",
                         choices=as.list(c(setdiff(getAllSelectedPredictors(),c(input$modFixed,input$modinterFixed))))
      ) 
    )
  })	
  output$model.txt.TeXformula1L <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if('intercept' %in% getAllSelectedPredictors()){
      predictors = setdiff(getAllSelectedPredictors(), 'intercept')
      withMathJax(
        paste0(input$response,
              "\\(_{i}\\)",
              " = \\(\\beta_{0}\\) + ",
              paste(sapply(predictors, function(.x){
                i = which(predictors == .x)
                if(grepl('\\(\\times\\)', .x, fixed = T)){
                  .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                }
                paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{i}\\)")
              }), collapse = " + "),
              " + \\(e_{i}\\)"
        )
      )
    }
    else{
      predictors = getAllSelectedPredictors()
      withMathJax(
        paste0(input$response,
              "\\(_{i}\\)",
              " = ",
              paste(sapply(predictors, function(.x){
                i = which(predictors == .x) - 1
                if(grepl('\\(\\times\\)', .x, fixed = T)){
                  .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                }
                paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{i}\\)")
              }), collapse = " + "),
              " + \\(e_{i}\\)")
      )
    }
  })
  output$model.txt.TeXformula2L1 <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if('intercept' %in% getAllSelectedPredictors()){
      predictors = setdiff(getAllSelectedPredictors(), 'intercept')
      withMathJax(
        paste0(input$response,
               "\\(_{ij}\\)",
               " = \\(\\beta_{0}\\) + ",
               paste(sapply(predictors, function(.x){
                 i = which(predictors == .x)
                 if(grepl('\\(\\times\\)', .x, fixed = T)){
                   .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                 }
                 paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{ij}\\)")
               }), collapse = " + "),
               " + \\(e_{ij}\\)"
        )
      )
    }
    else{
      predictors = getAllSelectedPredictors()
      withMathJax(
        paste0(input$response,
              "\\(_{ij}\\)",
              " = ",
              paste(sapply(predictors, function(.x){
                i = which(predictors == .x) - 1
                if(grepl('\\(\\times\\)', .x, fixed = T)){
                  .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                }
                paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{ij}\\)")
              }), collapse = " + "),
              " + \\(e_{ij}\\)")
      )
    }
  })
  output$model.txt.TeXformula2L2 <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if(length(getAllSelectedREs()$case_level) > 0){
      withMathJax(
        paste(
          "
          \\begin{align}
          \\left\\{
          \\begin{matrix}",
          paste(sapply(getAllSelectedREs()$case_level, getRE2L2formula), collapse = " \\\\"),
          "\\end{matrix}
          \\right.
          \\end{align}
          "
        )
        )
    }
    })
  output$model.txt.TeXformula3L1 <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if('intercept' %in% getAllSelectedPredictors()){
      predictors = setdiff(getAllSelectedPredictors(), 'intercept')
      withMathJax(
        paste0(input$response,
               "\\(_{ijk}\\)",
               " = \\(\\beta_{0}\\) + ",
               paste(sapply(predictors, function(.x){
                 i = which(predictors == .x)
                 if(grepl('\\(\\times\\)', .x, fixed = T)){
                   .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                 }
                 paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{ijk}\\)")
               }), collapse = " + "),
               " + \\(e_{ijk}\\)"
        )
      )
    }
    else{
      predictors = getAllSelectedPredictors()
      withMathJax(
        paste0(input$response,
              "\\(_{ijk}\\)",
              " = ",
              paste(sapply(predictors, function(.x){
                i = which(predictors == .x) - 1
                if(grepl('\\(\\times\\)', .x, fixed = T)){
                  .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                }
                paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{ijk}\\)")
              }), collapse = " + "),
              " + \\(e_{ijk}\\)")
      )
    }
  })
  output$model.txt.TeXformula3L2 <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if(length(getAllSelectedREs()$case_level) > 0){
      withMathJax(
        paste(
          "
          \\begin{align}
          \\left\\{
          \\begin{matrix}",
          paste(sapply(getAllSelectedREs()$case_level, getRE3L2formula), collapse = " \\\\"),
          "\\end{matrix}
          \\right.
          \\end{align}
          "
        )
        )
    }
    })
  output$model.txt.TeXformula3L3 <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if(length(getAllSelectedREs()$study_level) > 0){
      withMathJax(
        paste(
          "
          \\begin{align}
          \\left\\{
          \\begin{matrix}",
          paste(sapply(getAllSelectedREs()$study_level, getRE3L3formula), collapse = " \\\\"),
          "\\end{matrix}
          \\right.
          \\end{align}
          "
        )
        )
    }
    })
  output$model.txt.Rformula <- renderPrint({
    cat("### One-level analysis\n",get1LRModel(),
        "\n### Two-level analysis\n",get2LRModel(),
        "\n### Three-level analysis\n",get3LRModel())  		
  })
  output$model.stan.header <- renderUI({
    if(length(input$modFixed) > 0){
      h4("One-level model (without moderators)")
    }
    else{
      h4("One-level model")
    }
  })
  output$model.txt.TeXformula.stan <- renderUI({
    validate(
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if('intercept' %in% getSimpleSelectedPredictors()){
      predictors = setdiff(getSimpleSelectedPredictors(), 'intercept')
      withMathJax(
        # paste("\\(\\left(\\right.\\)",
        #       input$response,
        #       "\\(\\left.\\right)_{i}\\)",
        #       " = \\(\\beta_{0}\\) + ",
        #       paste(sapply(predictors, function(.x) paste0('\\(\\beta_{',which(predictors == .x),'}\\)',"\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)_{i}\\)")), collapse = " + ")
        # )
        paste0(input$response,
               "\\(_{i}\\)",
               " = \\(\\beta_{0}\\) + ",
               paste(sapply(predictors, function(.x){
                 i = which(predictors == .x)
                 if(grepl('\\(\\times\\)', .x, fixed = T)){
                   .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                 }
                 paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{i}\\)")
               }), collapse = " + "),
               " + \\(e_{i}\\)"
        )
      )
    }
    else{
      withMathJax(
        # paste("\\(\\left(\\right.\\)",
        #       input$response,
        #       "\\(\\left.\\right)_{i}\\)",
        #       " = ",
        #       paste(sapply(getSimpleSelectedPredictors(), function(.x) paste0('\\(\\beta_{',which(getSimpleSelectedPredictors() == .x)-1,'}\\)',"\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)_{i}\\)")), collapse = " + ")
        # )
        paste0(input$response,
               "\\(_{i}\\)",
               " = ",
               paste(sapply(predictors, function(.x){
                 i = which(predictors == .x) - 1
                 if(grepl('\\(\\times\\)', .x, fixed = T)){
                   .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                 }
                 paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{i}\\)")
               }), collapse = " + "),
               " + \\(e_{i}\\)")
      )
    }
  })
  output$ui.standardization <- renderUI({
    if(input$checkStandardization){
      output$table.standardized = DT::renderDataTable({
        ..dta = getStanDataDisp()
        ..dta[ , !(names(..dta) == 'obs')]
      })
      DT::dataTableOutput("table.standardized")
    } else{
      HTML('<p>The standardization option under <strong>Model specification</strong> is not checked.<br />
            The one-, two- and three-level analyses will be performed on the unstandardized data.<br />
            View unstandardized data table under&nbsp;<strong>Input &gt; Data summary</strong>.</p>')
    }
  })
  output$link.table.standardization = renderUI({
    validate(need(input$checkStandardization == 1,''))
    downloadLink('download.table.standardized', 'Download table (tab delimited .txt)')
  })
  
  output$downloadDataTable <- downloadHandler(
    filename = function() {
      paste('Data_standardized', format(Sys.time(), "%d-%b_%Hh%M"), '.txt', sep='')
    },
    content = function(file) {
      write.table(getStanDataDisp(), file, sep="\t", row.names=F)
    }
  )
  
  # One-level analysis ----

  cases = reactive({
    ..dta <- getFinalData()		
    ..sdta <- ..dta[..dta[,input$cases]%in%input$selectCases,]
    if(input$studies != "---") ..sdta <- ..dta[..dta[,input$studies]%in%input$selectStudies,]
    ..sdta = unique(..sdta[,c(input$studies,input$cases)])
    cases = dlply(..dta, c(input$studies), function(.x){
      ..cases = as.character(unique(.x[,input$cases]))
      ..list = as.list(..cases)
      setNames(..list,..cases) 
    })
    cases
  })
  
  cases_ids = reactive({
    unlist(cases())
  })
  
  check = reactive({input$checkStandardization})
  
  case_1LA_choices = callModule(selectCases,'case_1LA_choices',choices = cases)
  case_1LA_checkStan <<- callModule(radioStan,'case_1LA_checkStan',check)
  regr_1LA_choices = callModule(selectCases,'regr_1LA_choices',choices = cases)
  regr_1LA_checkStan <<- callModule(radioStan,'regr_1LA_checkStan',checkStan = check)
  es_1LA_choices = callModule(selectCases,'es_1LA_choices',choices = cases)
  es_1LA_checkStan <<- callModule(radioStan,'es_1LA_checkStan',checkStan = check)
  
  ## Case summary ----
  
  theme_apa <- function(){
    theme(
      legend.key = element_rect(color = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title=element_blank()
    )
  }
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(6)
  getHeightFromSliderDesign <- function() {
    validate(need(input$sliderCaseDesign,"Note: generating many plots can be time consuming."))
    input$sliderCaseDesign * 20
  }
  getPlotDesign <- function(){
    if(input$checkStandardization){
      if(case_1LA_checkStan() == 'Use standardized data'){
        ..dta = getFinalData() 
      }
      if(case_1LA_checkStan() == 'Use unstandardized data'){
        ..dta = getUnStanData() 
      }
    } else ..dta = getFinalData()
    ..dta <- ..dta[..dta[,input$cases] %in% case_1LA_choices$selectCases(),]
    ..vline = ddply(..dta, c(input$cases), function(.x){
      c('left' = max(subset(.x, .x[input$treatment] == 0)[,input$time]),
        'right' = min(subset(.x, .x[input$treatment] == 1)[,input$time]))
    })
    ..vline$x = (..vline$right + ..vline$left)/2
    ..plots <- ggplot(..dta, aes_string(x = input$time, 
                                        y = input$response,
                                        group = input$treatment)) +
      geom_line(size=1) +
      geom_point() +
      geom_vline(data = ..vline, aes(xintercept = x), linetype = 'dashed') +
      guides(linetype=FALSE) +
      facet_grid(reformulate(".",input$cases)) +
      theme_bw() +
      theme_apa() +
      theme(legend.position="none")
    ..plots
  }
  
  getcaseTable = reactive({
    if(input$checkStandardization){
      if(case_1LA_checkStan() == 'Use standardized data'){
        ..dta = getFinalData() 
      }
      if(case_1LA_checkStan() == 'Use unstandardized data'){
        ..dta = getUnStanData() 
      }
    } else ..dta = getFinalData()
    ..dta <- ..dta[..dta[,input$cases] %in% case_1LA_choices$selectCases(),]
    ..tmp <- melt(..dta,id.vars=c(input$treatment,input$cases),measure.vars=input$response)
    ..rng <- aggregate(list(range=..tmp[,"value"]),list(..case=..tmp[,input$cases]),range,na.rm=T)
    ..tbl <- aggregate(list(freq=..tmp[,input$treatment]),list(..case=..tmp[,input$cases]),table)
    ..mss <- aggregate(list(Missing=..tmp[,"value"]),list(..case=..tmp[,input$cases]),function(.x) sum(is.na(.x)))
    ..tmp <- merge(..rng,..tbl,by="..case")
    ..tmp <- cbind(..tmp[,"..case"],as.data.frame(..tmp$range),as.data.frame(..tmp$freq))
    names(..tmp)[1:3] <- c("..case","Min","Max")
    ..tmp <- merge(..tmp,..mss,by="..case")
    names(..tmp)[1] <- c(input$cases)
    ..tmp
  })
  caseTable.1La = eventReactive(input$show.casePlots.1LA,{
    getPlotDesign()
  })
  
  output$desc.table.descriptives <- DT::renderDataTable({
    getcaseTable()
  })
  output$link.caseTab.1LA <- renderUI({
    downloadLink('download.caseTab.1LA', 'Download summary table (tab delimited .txt)')
  })
  output$desc.link.showDownloadDesignPlots <- renderUI({
    validate(
      need(input$show.casePlots.1LA,'')
    )
    downloadLink('downloadDesignPlots', 'Download case plots (.png)')
  })
  output$desc.slider.caseDesign <- renderUI({
    sliderInput("sliderCaseDesign", label = "Plot height", min = 0, max = 100, value = 30)
  })
  output$desc.plot.caseDesign <- renderPlot(height=getHeightFromSliderDesign,{
    withProgress(message = 'Rendering plots', caseTable.1La())
  })
  
  output$download.caseTab.1LA <- downloadHandler(
    filename = function() {
      paste('1LA_case_table_', format(Sys.time(), "%d-%b_%Hh%M"), '.txt', sep='')
    },
    content = function(file) {
      write.table(getcaseTable(), file, sep="\t", row.names=F)
    }
  )
  output$downloadDesignPlots <- downloadHandler(
    filename = function() { 
      paste('1LA_case_plot_', format(Sys.time(), "%d-%b_%Hh%M"), '.png', sep='')
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = getPlotDesign(), device = device)
    }
  )
  
  ## Regression results ----
  
  getHeightFromSlider.1LARegr <- function() {
    validate(need(input$slider.regrPlot.1LA,"Note: generating many plots can be time consuming."))
    input$slider.regrPlot.1LA * 20
  }
  
  getCaseRegressionList <- reactive({
    Progr1LARegr = Progress$new()
    on.exit(Progr1LARegr$close())
    Progr1LARegr$set(message = "Retrieving data", value = 0.25)
    if(input$checkStandardization){
      if(regr_1LA_checkStan() == 'Use standardized data'){
        ..dta = getFinalData() 
      }
      if(regr_1LA_checkStan() == 'Use unstandardized data'){
        ..dta = getUnStanData() 
      }
    } else ..dta = getFinalData()
    Progr1LARegr$set(message = "Calculating regression results", value = 0.5)
    ..lmL <- dlply(..dta,c(input$cases),function(.x) list(data=.x,lm=lm(as.formula(get1LRModel()),data=.x)))
    ..lmL
  })
  getCaseRegressionBounds <- reactive({
    Progr1LARegr = Progress$new()
    on.exit(Progr1LARegr$close())
    Progr1LARegr$set(message = "Calculating confidence and prediction intervals", value = 0.5)
    ..lmL <- getCaseRegressionList()
    ..lmCfd <- lapply(..lmL, function(.x) data.frame(.x$data,predict(.x$lm, interval = 'confidence', level = 1 - as.numeric(input$set.alpha.regrPlot.1LA))))
    ..lmPrd <- lapply(..lmL, function(.x) data.frame(.x$data,predict(.x$lm, interval = 'prediction', level = 1 - as.numeric(input$set.alpha.regrPlot.1LA))))
    changeNms <- function(df,txt){
      names(df)[names(df)%in%c("lwr","upr")] <- paste0(txt,c("Lwr","Upr"))
      df
    }
    ..lmCfd <- lapply(..lmCfd, changeNms, "cfd")
    ..lmPrd <- lapply(..lmPrd, changeNms, "prd")
    ..lmX <- Map(function(.x,.y) merge(.x,.y), ..lmCfd,..lmPrd)
    ..lmX	
  })
  getProcessedCaseRegressionCoef <- reactive({
    ..res <- getCaseRegressionList()
    ..res <- ..res[regr_1LA_choices$selectCases()]
    Progr1LARegr = Progress$new()
    on.exit(Progr1LARegr$close())
    Progr1LARegr$set(message = "Summarizing regression results", value = 0.5)
    ..lm <- lapply(..res,function(.x) round(coef(summary(.x$lm)),3))
    ..lm <- lapply(..lm, function(.x) data.frame(..pred=row.names(.x),.x))
    ..Lm <- do.call(rbind, Map(data.frame,..id=names(..lm),..lm))
    row.names(..Lm) <- NULL
    colnames(..Lm) = c(input$cases, 'Regressor', 'Coefficient','Standard Error','t-value','p-value')
    ..Lm
  })
  get1LARegressionPlots = eventReactive(input$button.regrPlot.1LA,{
    ..dta <- getCaseRegressionBounds()
    ..dta <- ..dta[regr_1LA_choices$selectCases()]
    ..dta <- do.call('rbind',..dta)
    ..vline = ddply(..dta, c(input$cases), function(.x){
      c('left' = max(subset(.x, .x[input$treatment] == 0)[,input$time]),
        'right' = min(subset(.x, .x[input$treatment] == 1)[,input$time]))
    })
    ..vline$x = (..vline$right + ..vline$left)/2
    Progr1LARegr = Progress$new()
    on.exit(Progr1LARegr$close())
    Progr1LARegr$set(message = "Rendering plots", value = 0.5)
    ..plots <- ggplot(..dta, aes_string(x = input$time,
                                        y = input$response,
                                        group = input$treatment))
    if('Confidence interval' %in% input$checks.regrPlot.1LA){..plots = ..plots + geom_ribbon(aes(ymin = cfdLwr, ymax = cfdUpr), alpha = I(0.1))}
    if('Prediction interval' %in% input$checks.regrPlot.1LA){..plots = ..plots + geom_ribbon(aes(ymin = prdLwr, ymax = prdUpr), alpha = I(0.1))}
    ..plots = ..plots +
      geom_line(aes(y = fit), color = cols[1], size = 1) +
      geom_point() +
      geom_vline(data = ..vline, aes(xintercept = x), linetype = 'dashed') +
      guides(linetype=FALSE) +
      theme_bw() +
      theme_apa() +
      theme(legend.position="none") +
      facet_grid(reformulate(".",input$cases))
    ..plots
  })
  get1LARegressionModel = reactive({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if('intercept' %in% getAllSelectedPredictors()){
      predictors = setdiff(getAllSelectedPredictors(), 'intercept')
      withMathJax(
        paste0(input$response,
               "\\(_{i}\\)",
               " = \\(\\beta_{0}\\) + ",
               paste(sapply(predictors, function(.x){
                 i = which(predictors == .x)
                 if(grepl('\\(\\times\\)', .x, fixed = T)){
                   .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                 }
                 paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{i}\\)")
               }), collapse = " + "),
               " + \\(e_{i}\\)"
        )
      )
    }
    else{
      predictors = getAllSelectedPredictors()
      withMathJax(
        paste0(input$response,
               "\\(_{i}\\)",
               " = ",
               paste(sapply(predictors, function(.x){
                 i = which(predictors == .x) - 1
                 if(grepl('\\(\\times\\)', .x, fixed = T)){
                   .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                 }
                 paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{i}\\)")
               }), collapse = " + "),
               " + \\(e_{i}\\)")
      )
    }
  })
  
  output$model.txt.TeXformula1L1LA.1 <- renderUI({
    get1LARegressionModel()
  })
  output$model.txt.TeXformula1L1LA.2 <- renderUI({
    get1LARegressionModel()
  })
  output$caseEst.txt.formula <- renderPrint({
    cat(get1LRModel())
  })
  output$caseEst.table.estimates <- DT::renderDataTable({
    getProcessedCaseRegressionCoef()
  })
  output$link.regrTab.1LA <- renderUI({
    downloadLink('download.regrTab.1LA', 'Download regression results (tab delimited .txt)')
  })
  output$slider.regrPlot.1LA <- renderUI({
    sliderInput("slider.regrPlot.1LA", label = "Plot height", min = 0, max = 100, value = 30)
  })
  output$model.txt.TeXformula1L1LA <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if('intercept' %in% getAllSelectedPredictors()){
      predictors = setdiff(getAllSelectedPredictors(), 'intercept')
      withMathJax(
        paste("\\(\\left(\\right.\\)",
              input$response,
              "\\(\\left.\\right)_{i}\\)",
              " = \\(\\beta_{0}\\) + ",
              paste(sapply(predictors, function(.x) paste0('\\(\\beta_{',which(predictors == .x),'}\\)',"\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)_{i}\\)")), collapse = " + ")
        )
      )
    }
    else{
      withMathJax(
        paste("\\(\\left(\\right.\\)",
              input$response,
              "\\(\\left.\\right)_{i}\\)",
              " = ",
              paste(sapply(getAllSelectedPredictors(), function(.x) paste0('\\(\\beta_{',which(getAllSelectedPredictors() == .x)-1,'}\\)',"\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)_{i}\\)")), collapse = " + ")
        )
      )
    }
  })
  output$plot.regrPlot.1LA <- renderPlot(height = getHeightFromSlider.1LARegr,{
    get1LARegressionPlots()
  })
  output$link.regrPlot.1LA <- renderUI({
    validate(
      need(input$button.regrPlot.1LA,'')
    )
    downloadLink('download1LARegr', 'Download regression plots (.png)') 
  })
  
  output$download.regrTab.1LA <- downloadHandler(
    filename = function() {
      paste('1LA_regr_table_', format(Sys.time(), "%d-%b_%Hh%M"), '.txt', sep='')
    },
    content = function(file) {
      write.table(getProcessedCaseRegressionCoef(), file, sep="\t", row.names=F)
    }
  )
  output$download1LARegr <- downloadHandler(
    filename = function() { 
      paste('1LA_regr_plot_',format(Sys.time(), "%d-%b_%Hh%M"), '.png', sep='')
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = get1LARegressionPlots(), device = device)
    }
  )
  
  ## Effect size predictions ----
  
  
  getESRmodel = function(model){
    if(model == 'Model 1') formula = paste0(input$response,' ~ 1 + ',input$treatment)
    if(model == 'Model 2') formula = paste0(input$response,' ~ 1 + ',input$treatment,' + ',input$time,' + ',input$treatment,':',input$time)
    if(model == 'Model 3') formula = paste0(input$response,' ~ 1 + ',input$treatment,' + ',input$time,' + ',input$treatment,':',input$time,' + I(',input$time,'^2) + ',input$treatment,':I(',input$time,'^2)')
    formula
  }
  lmES_pred <- function(..case, alpha, TaT, model, D, DT, DT2){
    case_lm <- ..case$lm
    case_data <- ..case$data
    if(model == "Model 3"){
      ..a=coefficients(case_lm)[D]
      ..b=coefficients(case_lm)[DT]*TaT
      ..c=coefficients(case_lm)[DT2]*TaT^2
      es_hat <- as.numeric(..a+..b+..c)
      t = qt(1-alpha/2, nrow(case_data) - 1)
      V = vcov(case_lm)
      se = as.numeric(sqrt(V[D,D] + V[DT,DT]*TaT^2 + V[DT2,DT2]*TaT^4 + V[D,DT]*TaT + V[D,DT2]*TaT^2 + V[DT,DT2]*TaT^3))
      es_lwr = es_hat - t*(se/sqrt(nrow(case_data)))
      es_upr = es_hat + t*(se/sqrt(nrow(case_data)))
    }
    if(model == "Model 2"){
      ..a=coefficients(case_lm)[D]
      ..b=coefficients(case_lm)[DT]*TaT
      es_hat <- as.numeric(..a+..b)
      t = qt(1-alpha/2, nrow(case_data) - 1)
      V = vcov(case_lm)
      se = as.numeric(sqrt(V[D,D] + V[DT,DT]*TaT^2 + V[D,DT]*TaT))
      es_lwr = es_hat - t*(se/sqrt(nrow(case_data)))
      es_upr = es_hat + t*(se/sqrt(nrow(case_data)))
    }
    if(model == "Model 1"){
      ..a=coefficients(case_lm)[D]
      es_hat <- as.numeric(..a)
      t = qt(1-alpha/2, nrow(case_data) - 1)
      V = vcov(case_lm)
      se = as.numeric(sqrt(V[D,D]))
      es_lwr = es_hat - t*(se/sqrt(nrow(case_data)))
      es_upr = es_hat + t*(se/sqrt(nrow(case_data)))
    }
    data.frame('Time after treatment' = TaT,
               'Effect size' = es_hat,
               'Lower limit' = es_lwr,
               'Upper limit' = es_upr,
               check.names = FALSE)
  }
  getHeightFromSlider.1LAES <- function() {
    validate(need(input$slider.1LAES,"Note: generating many plots can be time consuming."))
    input$slider.1LAES * 20
  }
  calc1LARegression <- function(data, formula, cases){
    dlply(data, c(cases), function(.x) list(data = .x, lm = lm(formula, data=.x)))
  }
  
  getTimeRange <- reactive({
    ..dta <- getFinalData()
    range(..dta[,input$time])%*%c(-1.5,1.5)
  })
  getCaseEffectSize <- reactive({
    Progr1LARegr = Progress$new()
    on.exit(Progr1LARegr$close())
    Progr1LARegr$set(message = "Retrieving data", value = 0.25)
    if(input$checkStandardization){
      if(es_1LA_checkStan() == 'Use standardized data'){
        ..dta = getFinalData() 
      }
      if(es_1LA_checkStan() == 'Use unstandardized data'){
        ..dta = getSelectedData() 
      }
    } else ..dta = getFinalData()
    ..formula = as.formula(getESRmodel(input$setESModel))
    Progr1LARegr$set(message = "Calculating effect size predictions", value = 0.5)
    ..lmL = calc1LARegression(..dta, ..formula, input$cases)
    ..lmL <- ..lmL[es_1LA_choices$selectCases()]
    setAlpha <- as.numeric(input$setAlpha)
    D = paste0(input$treatment,'1')
    DT = paste0(input$treatment,'1:',input$time)
    DT2 = paste0(input$treatment,'1:I(',input$time,'^2)')
    Progr1LARegr$set(message = "Calculating effect size prediction intervals", value = 0.75)
    ..lmES <- ldply(..lmL, function(.x) lmES_pred(.x, setAlpha, input$effectPlotSlider, input$setESModel, D, DT, DT2), .id = input$cases)
  })
  getModelEffectSize = reactive({
    model1 = paste0("\\(\\left(\\right.\\)",input$response,"\\(\\left.\\right)_{i}\\)"," = ",
                   "\\(\\beta_{0i}\\)"," + ",
                   "\\(\\beta_{1i}\\)","\\(\\left(\\right.\\)",input$treatment,"\\(\\left.\\right)_{i}\\)"
                  )
    model2 = paste0(model1," + ",
                   "\\(\\beta_{2i}\\)","\\(\\left(\\right.\\)",input$time,"\\(\\left.\\right)_{i}\\)"," + ",
                   "\\(\\beta_{3i}\\)","\\(\\left(\\right.\\)",input$treatment,"\\(\\times\\)",input$time,"\\(\\left.\\right)_{i}\\)"
                   )
    model3 = paste0(model2," + ",
                   "\\(\\beta_{4i}\\)","\\(\\left(\\right.\\)",input$time,"\\(\\left.\\right)^2_{i}\\)"," + ",
                   "\\(\\beta_{5i}\\)","\\(\\left(\\right.\\)",input$treatment,"\\(\\times\\)","\\(\\left(\\right.\\)",input$time,"\\(\\left.\\right)^2\\)","\\(\\left.\\right)_{i}\\)"
                   )
    list(model1, model2, model3)
  })
  get1LAESPlots = eventReactive(input$button.ESPlot.1LA,{
    Progr1LARegr = Progress$new()
    on.exit(Progr1LARegr$close())
    Progr1LARegr$set(message = "Retrieving data", value = 0.2)
    if(input$checkStandardization){
      if(es_1LA_checkStan() == 'Use standardized data'){
        ..dta = getFinalData() 
      }
      if(es_1LA_checkStan() == 'Use unstandardized data'){
        ..dta = getSelectedData() 
      }
    } else ..dta = getFinalData()
    ..formula = as.formula(getESRmodel(input$setESModel))
    Progr1LARegr$set(message = "Calculating effect size predictions", value = 0.4)
    ..lmL = calc1LARegression(..dta, ..formula, input$cases)
    ..lmL <- ..lmL[es_1LA_choices$selectCases()]
    setAlpha <- as.numeric(input$setAlpha)
    D = paste0(input$treatment,'1')
    DT = paste0(input$treatment,'1:',input$time)
    DT2 = paste0(input$treatment,'1:I(',input$time,'^2)')
    Progr1LARegr$set(message = "Calculating effect size prediction intervals", value = 0.6)
    ..lmESs <- ldply(..lmL, function(.x) lmES_pred(.x, setAlpha, seq(input$slider.ESplotrange[1], input$slider.ESplotrange[2], length=100), input$setESModel, D, DT, DT2), .id = input$cases)
    ..lmESlist <- dlply(..lmESs,input$cases)
    ..lmES = do.call('rbind',..lmESlist)
    ..lmES = plyr::rename(..lmES, c("Time after treatment" = "TaT", "Effect size" = "ES", "Lower limit" = "LL", "Upper limit" = "UL"))
    Progr1LARegr$set(message = "Rendering plots", value = 0.8)
    ..esPlots = ggplot(..lmES, aes(x = TaT, y = ES)) +
      geom_ribbon(aes(ymin = LL, ymax = UL), alpha = I(0.2)) +
      geom_line() +
      ylab('Effect size') + 
      xlab('Time after treatment') +
      facet_grid(reformulate(".",input$cases)) +
      theme_bw() +
      theme_apa() +
      theme(legend.position="none")
    ..esPlots
  })
  
  output$casePred.select.ESmodel = renderUI({
    withMathJax(radioButtons("setESModel",
                 "Model",
                 choiceValues = list("Model 1","Model 2","Model 3"),
                 choiceNames = getModelEffectSize(),
                 selected = "Model 1"))
  })
  output$casePred.select.alpha <- renderUI({
    validate(      need(input$file, '')	  )
    withMathJax(selectInput("setAlpha", "\\(\\alpha\\)", choices=c(".001",".01",".025",".05",".1"), selected=".05"))
  })
  output$casePred.slider.timePredict <- renderUI({
    sliderInput("effectPlotSlider", label = 'Time after treatment', min = 0, max = getTimeRange(), value = getTimeRange()/2)
  })
  output$casePred.link.downloadEffectSizes <- renderUI({
    downloadLink('downloadCaseEffectSizes', 'Download effect size predictions (tab delimited .txt)')
  })
  output$casePred.table.effectsizes <- renderTable({
    getCaseEffectSize()
  })
  output$casePred.slider.ESplotrange = renderUI({
    sliderInput("slider.ESplotrange", label = 'Time after treatment', min = 0, max = getTimeRange(), value = c(0.25*getTimeRange(),0.75*getTimeRange()))
  })
  output$slider.1LAES <- renderUI({
    sliderInput("slider.1LAES", label = "Plot height", min = 0, max = 100, value = 30) 
  })
  output$plot.1LAES <- renderPlot(height = getHeightFromSlider.1LAES,{
    validate(need(input$button.ESPlot.1LA,""))
    get1LAESPlots()
  })
  output$link.showDownload1LAES <- renderUI({
    validate(need(input$button.ESPlot.1LA,""))
    downloadLink('download1LAES', 'Download effect size prediction plots (.png)')
  })
  
  output$downloadCaseEffectSizes <- downloadHandler(
    filename = function() {
      paste('1LA_ES_table_', format(Sys.time(), "%d-%b_%Hh%M"), '.txt', sep='')
    },
    content = function(file) {
      write.table(getCaseEffectSize(), file, sep="\t", row.names=F)
    }
  )
  output$download1LAES <- downloadHandler(
    filename = function() { 
      paste('1LA_ES_plot_',format(Sys.time(), "%d-%b_%Hh%M"), '.png', sep='')
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = get1LAESPlots(), device = device)
    }
  )
  
  # Two-level analysis ----
  
  output$mytest = renderPrint({
    input$choices_2LA
  })
  
  getMultiCaseStudies = reactive({
    ..dta <- getFinalData()
    ..sdta = ddply(..dta,c(input$studies),function(.x){length(unique(.x[[input$cases]])) > 1})
    colnames(..sdta) = c(colnames(..sdta)[1],'>1')
    ..sdta = ..sdta[..sdta$'>1' == T,][[input$studies]]
    sort(as.character(..sdta))
  })
  
  ## Regression results ----
  
  changeNms <- function(df,txt){
    names(df)[names(df)%in%c("lwr","upr")] <- paste0(txt,c("Lwr","Upr"))
    df
  }
  combine1LA2LA = function(data, prdC, prdS, prdInt){
    data$effect = 'Case (one-level analysis)'
    prdC = data.frame('fit' = prdC,
                      'effect' = 'Case (two-level analysis)',
                      'obs_tr' = names(prdC))
    prdS = data.frame('fit' = prdS,
                      'effect' = 'Study (two-level analysis)',
                      'obs_tr' = names(prdS))
    prdInt = dcast(melt(prdInt, id.vars = c('obs_tr','effect')), obs_tr + effect ~ variable)
    colnames(prdInt) = c('obs_tr','effect','fit','prdLwr','prdUpr')
    prdInt$effect = revalue(prdInt$effect, c('combined' = 'Case (two-level analysis)', 'fixed' = 'Study (two-level analysis)'))
    prdInt = prdInt[prdInt$effect %in% c('Case (two-level analysis)','Study (two-level analysis)'),c('obs_tr','effect','prdLwr','prdUpr')]
    prd = merge(prdC, prdS, by = c('obs_tr','effect','fit'), all = T)
    prd2 = merge(prd, prdInt, by = c('obs_tr','effect'), all = T)
    newdata = merge(data[,!names(data) %in% c('fit','cfdLwr','cfdUpr','prdLwr','prdUpr','effect')], prd2, by = c('obs_tr'), all = T)
    data = bind_rows(newdata, data)
    data
  }
  plot_study = function(studydf, analyses, ribbons, treatment, time, response, study, case){
    df = subset(studydf, studydf$effect %in% analyses)
    df_ribbons = subset(df, df$effect %in% ribbons)
    #vline = ddply(df, .(Case), function(x) as.numeric(unique(subset(x, DT == 0)[,time])-.5))
    inter = paste0('interaction(','effect,',colnames(df)[colnames(df) == treatment],')')
    #formula = as.formula(paste0(case,' ~ .'))
    ..vline = ddply(df, c(case), function(.x){
      c('left' = max(subset(.x, .x[treatment] == 0)[,time]),
        'right' = min(subset(.x, .x[treatment] == 1)[,time]))
    })
    ..vline$x = (..vline$right + ..vline$left)/2
    ggplot(df, aes_string(x = colnames(df)[colnames(df) == time], 
                          y = colnames(df)[colnames(df) == response], 
                          colour = 'effect',
                          group = inter, 
                          fill = 'effect')) + 
      geom_vline(data = ..vline, aes(xintercept = x), linetype = 'dashed') +
      geom_ribbon(data = df_ribbons, aes_string(ymin = 'prdLwr', ymax = 'prdUpr'), alpha = I(0.2)) +
      #geom_ribbon(aes(ymin = cfdLwr, ymax = cfdUpr, fill = effect, color = effect)) +
      geom_point(color = "grey50") +
      geom_line(aes(y = fit), size = 1) +
      ggtitle(paste('Study ',unique(df[,study]), sep = '')) +
      facet_grid(reformulate(".",case)) +
      theme_bw() +
      theme_apa() +
      theme(legend.title=element_blank())
  }
  get2LAHeight = function(){
    validate(need(input$`2LAplot.height`,""))
    input$`2LAplot.height`*20
  }
  
  get2LACaseRegressionLists <- reactive({
    Progr2LARegr = Progress$new()
    on.exit(Progr2LARegr$close())
    Progr2LARegr$set(message = "Calculating regression results", value = 0)
    # 1LA models
    ..dta = getFinalData()
    ..dta = ..dta[..dta[[input$studies]] %in% getMultiCaseStudies(),]
    rownames(..dta) = c(1:nrow(..dta))
    ..dta$obs = rownames(..dta)
    Progr2LARegr$set(message = "Calculating one-level regression results per case", value = 0)
    ..lmL = dlply(..dta,c(input$cases),function(.x) list(data=.x,lm=lm(as.formula(get1LRModel()),data=.x)))
    # 1LA coef, PI, CI
    Progr2LARegr$set(message = "Calculating one-level prediction intervals", value = 0.25)
    if(input$TwoLA_table_plot == 'Plot'){
      alpha = as.numeric(input$set.alpha.regrPlot.2LA)
      n.sims = input$n.sims.2LA
    } else{
      alpha = .05
      n.sims = 1000
    }
    ..lmCfd <- lapply(..lmL, function(.x) data.frame(.x$data,predict(.x$lm, interval = 'confidence', level = 1 - alpha)))
    ..lmPrd <- lapply(..lmL, function(.x) data.frame(.x$data,predict(.x$lm, interval = 'prediction', level = 1 - alpha)))
    ..lmCfd <- lapply(..lmCfd, changeNms, "cfd")
    ..lmPrd <- lapply(..lmPrd, changeNms, "prd")
    ..lmX <- Map(function(.x,.y) merge(.x,.y), ..lmCfd,..lmPrd)
    ..lmX = do.call('rbind', ..lmX)
    Progr2LARegr$set(message = "Calculating two-level regression results per study", value = 0.5)
    ..lmerL = dlply(..lmX, c(input$studies), 
                    function(.x){
                      ..lmerL = list(study = unique(.x[[input$studies]]),
                                     data = .x,
                                     lmer = lmer(as.formula(get2LRModel()), data = .x))
                      ..lmerL$data$obs_tr = 1:nrow(..lmerL$data)
                      ..lmerL$prdInt = predictInterval(..lmerL$lmer, which = 'all', n.sims = n.sims, level = 1 - alpha)
                      colnames(..lmerL$prdInt) = c(colnames(..lmerL$prdInt)[1:4],'obs_tr')
                      ..lmerL$prdC = predict(..lmerL$lmer, re.form = NULL)
                      ..lmerL$prdS = predict(..lmerL$lmer, re.form = NA)
                      ..lmerL
                    })
  })
  get2LACaseRegressionDfs = reactive({
    ..lmerL = get2LACaseRegressionLists()
    Progr2LARegrDf = Progress$new()
    on.exit(Progr2LARegrDf$close())
    Progr2LARegrDf$set(message = "Combining one-level and two-level regression results", value = 0.75)
    ..lmerL = llply(..lmerL,function(.x) combine1LA2LA(.x$data, .x$prdC, .x$prdS, .x$prdInt))
    ..lmerL
  })
  get2LAPlots = eventReactive(input$button.regrPlot.2LA,{
    ..lmerL = get2LACaseRegressionDfs()[[input$choices_2LA]]
    studydf = ..lmerL
    analyses = input$'2LA.check.regression'
    ribbons = input$'2LA.check.predInt'
    treatment = input$treatment
    time = input$time
    response = input$response
    study = input$studies
    case = input$cases

    Progr2LAPlot = Progress$new()
    on.exit(Progr2LAPlot$close())
    Progr2LAPlot$set(message = "Rendering plots", value = 0.75)

    df = subset(studydf, studydf$effect %in% analyses)
    df_ribbons = subset(df, df$effect %in% ribbons)
    inter = paste0('interaction(','effect,',colnames(df)[colnames(df) == treatment],')')
    #formula = as.formula(paste0(case,' ~ .'))
    ..vline = ddply(df, c(case), function(.x){
      c('left' = max(subset(.x, .x[treatment] == 0)[,time]),
        'right' = min(subset(.x, .x[treatment] == 1)[,time]))
    })
    ..vline$x = (..vline$right + ..vline$left)/2
    ..plots = ggplot(df, aes_string(x = colnames(df)[colnames(df) == time],
                          y = colnames(df)[colnames(df) == response],
                          colour = 'effect',
                          group = inter,
                          fill = 'effect')) +
      geom_vline(data = ..vline, aes(xintercept = x), linetype = 'dashed') +
      geom_ribbon(data = df_ribbons, aes_string(ymin = 'prdLwr', ymax = 'prdUpr'), alpha = I(0.2)) +
      #geom_ribbon(aes(ymin = cfdLwr, ymax = cfdUpr, fill = effect, color = effect)) +
      geom_point(color = "grey50") +
      geom_line(aes(y = fit), size = 1) +
      ggtitle(paste('Study ',unique(df[,study]), sep = '')) +
      facet_grid(reformulate(".",case)) +
      theme_bw() +
      theme_apa() +
      theme(legend.title=element_blank()) +
      scale_colour_manual(values = c("Case (one-level analysis)" = cols[1],
                                     "Case (two-level analysis)" = cols[2],
                                     "Study (two-level analysis)" = cols[3])) +
      scale_fill_manual(values = c("Case (one-level analysis)" = cols[1],
                                     "Case (two-level analysis)" = cols[2],
                                     "Study (two-level analysis)" = cols[3]))
    ..plots
  })
  
  output$select.study.2LA = renderUI({
    selectInput('choices_2LA',
                'Study',
                choices = getMultiCaseStudies(),
                selectize = T)
  })
  output$model.txt.TeXformula2L1.2LA <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if('intercept' %in% getAllSelectedPredictors()){
      predictors = setdiff(getAllSelectedPredictors(), 'intercept')
      withMathJax(
        paste0(input$response,
               "\\(_{ij}\\)",
               " = \\(\\beta_{0}\\) + ",
               paste(sapply(predictors, function(.x){
                 i = which(predictors == .x)
                 if(grepl('\\(\\times\\)', .x, fixed = T)){
                   .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                 }
                 paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{ij}\\)")
               }), collapse = " + "),
               " + \\(e_{ij}\\)"
        )
      )
    }
    else{
      predictors = getAllSelectedPredictors()
      withMathJax(
        paste0(input$response,
               "\\(_{ij}\\)",
               " = ",
               paste(sapply(predictors, function(.x){
                 i = which(predictors == .x) - 1
                 if(grepl('\\(\\times\\)', .x, fixed = T)){
                   .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                 }
                 paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{ij}\\)")
               }), collapse = " + "),
               " + \\(e_{ij}\\)")
      )
    }
  })
  output$model.txt.TeXformula2L2.2LA <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if(length(getAllSelectedREs()$case_level) > 0){
      withMathJax(
        paste(
          "
          \\begin{align}
          \\left\\{
          \\begin{matrix}",
          paste(sapply(getAllSelectedREs()$case_level, getRE2L2formula), collapse = " \\\\"),
          "\\end{matrix}
          \\right.
          \\end{align}
          "
        )
      )
    }
    })
  output$fixed2LA <- renderPrint({
    ..res <- get2LACaseRegressionLists()[[input$choices_2LA]]
    summary(..res$lmer)$coefficients
  })
  output$random2LA <- renderPrint({
    ..res <- get2LACaseRegressionLists()[[input$choices_2LA]]
    VarCorr(..res$lmer)
  })
  output$plot.regrPlot.2LA <- renderPlot(height = get2LAHeight,{
    get2LAPlots()
  })
  output$link.regrPlot.2LA <- renderUI({
    validate(
      need(input$button.regrPlot.2LA,'')
    )
    downloadLink('download2LA', 'Download regression plots (.png)')
  })
  
  output$download2LA <- downloadHandler(
    filename = function() { 
      paste('2LA_regr_plot',format(Sys.time(), "%d-%b_%Hh%M"), '.png', sep='')
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = get2LAPlots(), device = device)
    }
  )
  
  # Three-level analysis ----
  
  choices_3LA = callModule(selectCases,'choices_3LA',choices = cases)
  
  ## Regression results ----
  
  combine1LA2LA3LA = function(case, dta, lmX, lmer2L, lmer3L, cases, studies){
    lmX = lmX[lmX[[input$cases]] == case,]
    study = as.character(unique(dta[dta[[cases]] == case,c(cases,studies)])[[studies]])
    lmer2L = lmer2L[[study]]
    lmer3L = lmer3L
    lmX$effect = 'Case (one-level analysis)'
    obs = lmX$obs
    if(!is.null(lmer2L)){
      lmer2L$prdC = lmer2L$prdC[names(lmer2L$prdC) %in% obs]
      lmer2L$prdS = lmer2L$prdS[names(lmer2L$prdS) %in% obs]
      lmer2L$prdInt = lmer2L$prdInt[lmer2L$prdInt$obs %in% obs,]
      prdC2L = data.frame('fit' = lmer2L$prdC,
                          'effect' = 'Case (two-level analysis)',
                          'obs' = names(lmer2L$prdC))
      prdS2L = data.frame('fit' = lmer2L$prdS,
                          'effect' = 'Study (two-level analysis)',
                          'obs' = names(lmer2L$prdS))
      prdInt2L = dcast(melt(lmer2L$prdInt[lmer2L$prdInt$obs %in% lmX$obs,], id.vars = c('obs','effect')), obs + effect ~ variable)
      colnames(prdInt2L) = c('obs','effect','fit','prdUpr','prdLwr')
      prdInt2L$effect = revalue(prdInt2L$effect, c('combined' = 'Case (two-level analysis)', 'fixed' = 'Study (two-level analysis)'))
      prdInt2L = prdInt2L[prdInt2L$effect %in% c('Case (two-level analysis)','Study (two-level analysis)'),c('obs','effect','prdLwr','prdUpr')]
      prd2L = merge(prdC2L, prdS2L, by = c('obs','effect','fit'), all = T)
      prd2L = merge(prd2L, prdInt2L, by = c('obs','effect'), all = T)
      data1L2L = merge(lmX[,!names(lmX) %in% c('fit','cfdLwr','cfdUpr','prdLwr','prdUpr','effect')], prd2L, by = c('obs'), all = T)
      data1L2L = bind_rows(data1L2L, lmX)
    } else{
      data1L2L = lmX
    }
    lmer3L$prdC = lmer3L$prdC[names(lmer3L$prdC) %in% obs]
    lmer3L$prdS = lmer3L$prdS[names(lmer3L$prdS) %in% obs]
    lmer3L$prdO = lmer3L$prdO[names(lmer3L$prdO) %in% obs]
    lmer3L$prdInt = lmer3L$prdInt[lmer3L$prdInt$obs %in% obs,]
    prdC3L = data.frame('fit' = lmer3L$prdC,
                        'effect' = 'Case (three-level analysis)',
                        'obs' = names(lmer3L$prdC))
    prdS3L = data.frame('fit' = lmer3L$prdS,
                        'effect' = 'Study (three-level analysis)',
                        'obs' = names(lmer3L$prdS))
    prdO3L = data.frame('fit' = lmer3L$prdO,
                        'effect' = 'Overall (three-level analysis)',
                        'obs' = names(lmer3L$prdO))
    prdInt3L = dcast(melt(lmer3L$prdInt[lmer3L$prdInt$obs %in% lmX$obs,], id.vars = c('obs','effect')), obs + effect ~ variable)
    colnames(prdInt3L) = c('obs','effect','fit','prdUpr','prdLwr')
    prdInt3LS = ddply(prdInt3L, c('obs'), function(.x){
      fit = .x[.x$effect == 'fixed',]$fit + .x[.x$effect == studies,]$fit
      prdLwr = .x[.x$effect == 'fixed',]$prdLwr + .x[.x$effect == studies,]$prdLwr
      prdUpr = .x[.x$effect == 'fixed',]$prdUpr + .x[.x$effect == studies,]$prdUpr
      data.frame('obs' = unique(.x$obs),
                 'effect' = 'Study (three-level analysis)',
                 'fit' = fit,
                 'prdUpr' = prdUpr,
                 'prdLwr' = prdLwr)
    })
    prdInt3L = merge(prdInt3L, prdInt3LS, by = c('obs','effect','fit','prdLwr','prdUpr'), all = T)
    prdInt3L$effect = revalue(prdInt3L$effect, c('combined' = 'Case (three-level analysis)', 'fixed' = 'Overall (three-level analysis)'))
    prdInt3L = prdInt3L[prdInt3L$effect %in% c('Case (three-level analysis)','Study (three-level analysis)','Overall (three-level analysis)'),]
    prd3L = merge(prdC3L, prdS3L, by = c('obs','effect','fit'), all = T)
    prd3L = merge(prd3L, prdO3L, by = c('obs','effect','fit'), all = T)
    prd3L = merge(prd3L, prdInt3L[,!names(prdInt3L) == 'fit'], by = c('obs','effect'), all = T)
    data1L2L3L = merge(data1L2L[,names(data1L2L) %in% c('obs','effect','fit','prdLwr','prdUpr','cfdLwr','cfdUpr')], prd3L, by = c('obs','effect','fit','prdLwr','prdUpr'), all = T)
    data1L2L3L = data1L2L3L[,!names(data1L2L3L) %in% c('cfdLwr','cfdUpr')]
    otherinfo = unique(data1L2L[,!names(data1L2L) %in% c('effect','fit','cfdLwr','cfdUpr','prdLwr','prdUpr','cfdLwr','cfdUpr')])
    data1L2L3L = merge(otherinfo, data1L2L3L, by = c('obs'))
    data1L2L3L
  }
  mapClass = function(checkboxChoice){
    x = checkboxChoice
    if(checkboxChoice == '1LA.Case') x = 'Case (one-level analysis)'
    if(checkboxChoice == '2LA.Case') x = 'Case (two-level analysis)'
    if(checkboxChoice == '2LA.Study') x = 'Study (two-level analysis)'
    if(checkboxChoice == '3LA.Case') x = 'Case (three-level analysis)'
    if(checkboxChoice == '3LA.Study') x = 'Study (three-level analysis)'
    if(checkboxChoice == '3LA.Overall') x = 'Overall (three-level analysis)'
    x
  }
  plot123LARegression = function(analyses, ribbons, treatment, time, response, cases, selectedCases){
    df = get3LARegressionDf()
    df = df[df[,input$cases] %in% selectedCases,]
    df = subset(df, df$effect %in% analyses)
    df_ribbons = subset(df, df$effect %in% ribbons)
    facet = as.formula(paste0(cases,' ~ .'))
    ..vline = ddply(df, c(cases), function(.x){
      c('left' = max(subset(.x, .x[treatment] == 0)[,time]),
        'right' = min(subset(.x, .x[treatment] == 1)[,time]))
    })
    ..vline$x = (..vline$right + ..vline$left)/2
    ggplot(df, aes_string(x = colnames(df)[colnames(df) == time], 
                          y = colnames(df)[colnames(df) == response], 
                          colour = 'effect',
                          group = 'group', 
                          fill = 'effect')) + 
      geom_vline(data = ..vline, aes(xintercept = x), linetype = 'dashed') +
      geom_ribbon(data = df_ribbons, aes_string(ymin = 'prdLwr', ymax = 'prdUpr'), alpha = I(0.2)) +
      geom_point(color = "grey50") +
      geom_line(aes(y = fit), size = 1) +#ggtitle(paste('Study ',unique(df[,input$studies]), sep = '')) +
      facet_grid(reformulate(".",cases)) +
      theme_bw() +
      theme_apa() +
      theme(legend.title=element_blank()) +
      scale_colour_manual(values = c("Case (one-level analysis)" = cols[1],
                                     "Case (two-level analysis)" = cols[2],
                                     "Study (two-level analysis)" = cols[3],
                                     "Case (three-level analysis)" = cols[4],
                                     "Study (three-level analysis)" = cols[5],
                                     "Overall (three-level analysis)" = cols[6])) +
      scale_fill_manual(values = c("Case (one-level analysis)" = cols[1],
                                   "Case (two-level analysis)" = cols[2],
                                   "Study (two-level analysis)" = cols[3],
                                   "Case (three-level analysis)" = cols[4],
                                   "Study (three-level analysis)" = cols[5],
                                   "Overall (three-level analysis)" = cols[6]))
  }
  get3LAHeight = function(){
    input$`3LAplot.height`*20
  }
  
  get3LMetaRegressionList = reactive({
    validate(      
      need(input$file, ''),
      need(input$studies != '---', 'No studies defined')
    )
    withProgress(message = 'Estimating three-level model and predictions',{
      ..dta = getFinalData()
      ..lmerL = list(data=..dta, lmer=lmer(as.formula(get3LRModel()), data=..dta))
      if(input$ThreeLA_table_plot == 'Plot'){
        alpha = as.numeric(input$set.alpha.regrPlot.2LA)
        n.sims = input$n.sims.2LA
      } else{
        alpha = .05
        n.sims = 1000
      }
      ..lmerL$prd = predictInterval(..lmerL$lmer, which = 'all', n.sims = n.sims, level = 1 - alpha)
      ..lmerL
    })
  })
  get3LARegressionDf = reactive({
    Progr1LARegr = Progress$new()
    on.exit(Progr1LARegr$close())
    Progr1LARegr$set(message = "Retrieving data", value = 0.1)
    ..dta = getFinalData()
    Progr1LARegr$set(message = "Estimating one-level model and predictions per case", value = 0.3)
    ..lmL = dlply(..dta,c(input$cases),function(.x) list(data=.x,lm=lm(as.formula(get1LRModel()),data=.x)))
    if(input$ThreeLA_table_plot == 'Plot'){
      alpha = as.numeric(input$set.alpha.regrPlot.2LA)
      n.sims = input$n.sims.2LA
    } else{
      alpha = .05
      n.sims = 1000
    }
    ..lmCfd <- lapply(..lmL, function(.x) data.frame(.x$data,predict(.x$lm, interval = 'confidence', level = 1 - alpha)))
    ..lmPrd <- lapply(..lmL, function(.x) data.frame(.x$data,predict(.x$lm, interval = 'prediction', level = 1 - alpha)))
    ..lmCfd <- lapply(..lmCfd, changeNms, "cfd")
    ..lmPrd <- lapply(..lmPrd, changeNms, "prd")
    ..lmX <- Map(function(.x,.y) merge(.x,.y), ..lmCfd,..lmPrd)
    ..lmX = do.call('rbind', ..lmX)
    ..sdta = ddply(..dta,c(input$studies),function(.x){length(unique(.x[[input$cases]])) > 1})
    colnames(..sdta) = c(colnames(..sdta)[1],'>1')
    ..sdta = ..sdta[..sdta$'>1' == T,][[input$studies]]
    ..ndta = ..dta[..dta[[input$studies]] %in% ..sdta,]
    Progr1LARegr$set(message = "Estimating two-level model and predictions per study", value = 0.5)
    ..lmer2L = dlply(..ndta, c(input$studies), 
                     function(.x){
                       ..lmerL = list(study = unique(.x[[input$studies]]),
                                      data = .x,
                                      lmer = lmer(as.formula(get2LRModel()), data = .x))
                       ..lmerL$data$obs_tr = 1:nrow(..lmerL$data)
                       ..lmerL$prdInt = predictInterval(..lmerL$lmer, which = 'all', n.sims = n.sims, level = 1 - alpha)
                       ..lmerL$prdInt$obs = ..lmerL$data$obs
                       ..lmerL$prdC = predict(..lmerL$lmer, re.form = NULL)
                       names(..lmerL$prdC) = ..lmerL$data$obs
                       ..lmerL$prdS = predict(..lmerL$lmer, re.form = NA)
                       names(..lmerL$prdS) = ..lmerL$data$obs
                       ..lmerL
                     })
    stuREs = sapply(input$varStudies, TeXtoR)
    study.re.form = as.formula(paste0('~',paste0("(",paste(stuREs,collapse = " + ")," | ",input$studies,")")))
    Progr1LARegr$set(message = "Estimating three-level model and predictions", value = 0.7)
    ..lmer3L = list(data = ..dta)
    ..lmer3L$lmer = lmer(as.formula(get3LRModel()), data = ..lmer3L$data)
    ..lmer3L$prdInt = predictInterval(..lmer3L$lmer, which = 'all', n.sims = n.sims, level = 1 - alpha)
    ..lmer3L$prdInt$obs = ..lmer3L$data$obs
    ..lmer3L$prdC = predict(..lmer3L$lmer, re.form = NULL)
    names(..lmer3L$prdC) = ..lmer3L$data$obs
    ..lmer3L$prdS = predict(..lmer3L$lmer, re.form = study.re.form)
    names(..lmer3L$prdS) = ..lmer3L$data$obs
    ..lmer3L$prdO = predict(..lmer3L$lmer, re.form = NA)
    names(..lmer3L$prdO) = ..lmer3L$data$obs
    Progr1LARegr$set(message = "Combining one-, two- and three-level analysis results", value = 0.9)
    df = ldply(lapply(as.character(unique(..dta[,input$cases])),function(.x) combine1LA2LA3LA(.x, ..dta, ..lmX, ..lmer2L, ..lmer3L, input$cases, input$studies)), data.frame)
    df$group = interaction(as.factor(df[,input$treatment]),df$effect)
    df
  })
  threeLAplot = eventReactive(input$button.regrPlot.3LA,{
    analyses = input$'3LA.check.regression'
    ribbons = input$'3LA.check.predInt'
    treatment = input$treatment
    time = input$time
    response = input$response
    cases = input$cases
    selectedCases = choices_3LA$selectCases()
    df = get3LARegressionDf()
    Progr1LARegr = Progress$new()
    on.exit(Progr1LARegr$close())
    Progr1LARegr$set(message = "Rendering plots", value = 0.5)
    df = df[df[,input$cases] %in% selectedCases,]
    df = subset(df, df$effect %in% analyses)
    df_ribbons = subset(df, df$effect %in% ribbons)
    facet = as.formula(paste0(cases,' ~ .'))
    ..vline = ddply(df, c(cases), function(.x){
      c('left' = max(subset(.x, .x[treatment] == 0)[,time]),
        'right' = min(subset(.x, .x[treatment] == 1)[,time]))
    })
    ..vline$x = (..vline$right + ..vline$left)/2
    ggplot(df, aes_string(x = colnames(df)[colnames(df) == time], 
                          y = colnames(df)[colnames(df) == response], 
                          colour = 'effect',
                          group = 'group', 
                          fill = 'effect')) + 
      geom_vline(data = ..vline, aes(xintercept = x), linetype = 'dashed') +
      geom_ribbon(data = df_ribbons, aes_string(ymin = 'prdLwr', ymax = 'prdUpr'), alpha = I(0.2)) +
      geom_point(color = "grey50") +
      geom_line(aes(y = fit), size = 1) +#ggtitle(paste('Study ',unique(df[,input$studies]), sep = '')) +
      facet_grid(reformulate(".",cases)) +
      theme_bw() +
      theme_apa() +
      theme(legend.title=element_blank()) +
      scale_colour_manual(values = c("Case (one-level analysis)" = cols[1],
                                     "Case (two-level analysis)" = cols[2],
                                     "Study (two-level analysis)" = cols[3],
                                     "Case (three-level analysis)" = cols[4],
                                     "Study (three-level analysis)" = cols[5],
                                     "Overall (three-level analysis)" = cols[6])) +
      scale_fill_manual(values = c("Case (one-level analysis)" = cols[1],
                                   "Case (two-level analysis)" = cols[2],
                                   "Study (two-level analysis)" = cols[3],
                                   "Case (three-level analysis)" = cols[4],
                                   "Study (three-level analysis)" = cols[5],
                                   "Overall (three-level analysis)" = cols[6]))
  })
  
  output$model.txt.TeXformula3L1.3LA <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if('intercept' %in% getAllSelectedPredictors()){
      predictors = setdiff(getAllSelectedPredictors(), 'intercept')
      withMathJax(
        paste0(input$response,
               "\\(_{ijk}\\)",
               " = \\(\\beta_{0}\\) + ",
               paste(sapply(predictors, function(.x){
                 i = which(predictors == .x)
                 if(grepl('\\(\\times\\)', .x, fixed = T)){
                   .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                 }
                 paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{ijk}\\)")
               }), collapse = " + "),
               " + \\(e_{ijk}\\)"
        )
      )
    }
    else{
      predictors = getAllSelectedPredictors()
      withMathJax(
        paste0(input$response,
               "\\(_{ijk}\\)",
               " = ",
               paste(sapply(predictors, function(.x){
                 i = which(predictors == .x) - 1
                 if(grepl('\\(\\times\\)', .x, fixed = T)){
                   .x = paste0("\\(\\left(\\right.\\)",.x,"\\(\\left.\\right)\\)")
                 }
                 paste0('\\(\\beta_{',i,'}\\)',.x,"\\(_{ijk}\\)")
               }), collapse = " + "),
               " + \\(e_{ijk}\\)")
      )
    }
  })
  output$model.txt.TeXformula3L2.3LA <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if(length(getAllSelectedREs()$case_level) > 0){
      withMathJax(
        paste(
          "
          \\begin{align}
          \\left\\{
          \\begin{matrix}",
          paste(sapply(getAllSelectedREs()$case_level, getRE3L2formula), collapse = " \\\\"),
          "\\end{matrix}
          \\right.
          \\end{align}
          "
        )
      )
    }
  })
  output$model.txt.TeXformula3L3.3LA <- renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", ''),
      need(input$simpleFixed, '')
    )
    if(length(getAllSelectedREs()$study_level) > 0){
      withMathJax(
        paste(
          "
          \\begin{align}
          \\left\\{
          \\begin{matrix}",
          paste(sapply(getAllSelectedREs()$study_level, getRE3L3formula), collapse = " \\\\"),
          "\\end{matrix}
          \\right.
          \\end{align}
          "
        )
        )
    }
  })
  output$fixed3LA <- renderPrint({
    ..res <- get3LMetaRegressionList()
    summary(..res$lmer)$coefficients
  })
  output$random3LA <- renderPrint({
    ..res <- get3LMetaRegressionList()
    VarCorr(..res$lmer)
  })
  output$link.regrPlot.3LA <- renderUI({
    validate(need(input$button.regrPlot.3LA,''))
    downloadLink('download3LAPlots', 'Download regression plots (.png)')
  })
  output$plot.regrPlot.3LA <- renderPlot(height=get3LAHeight,{
    threeLAplot()
  })
  
  output$download3LAPlots <- downloadHandler(
    filename = function() { 
      paste('3LA_regr_plot_',format(Sys.time(), "%d-%b_%Hh%M"), '.png', sep='')
    },
    content = function(file) {
      ..plotList <- setPredPlots()
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = threeLAplot(), device = device)
    }
  )
  
})