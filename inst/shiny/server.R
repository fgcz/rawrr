#
# This is the server logic of a Shiny web application. 
#

stopifnot(packageVersion('rawrr') >= '1.7.19')

library(shiny)


# Define server logic required to draw a histogram
function(input, output, session) {
  
    config <- list(Streptavidin = list(
        peptides = c("YDSAPATDGSGTALGWTVAWK", "NAHSATTWSGQYVGGAEAR",
                     "INTQWLLTSGTTEANAWK", "STLVGHDTFTK", "VKPSAASIDAAK",
                     "AGVNNGNPLDAVQQ"),
        ion = list(m2Hplus0 = function(x){(protViz::parentIonMass(x) + 1.008) / 2},
                   m3Hplus0 = function(x){(protViz::parentIonMass(x) + 2 * 1.008) / 3},
                   m2Hplus1 = function(x){1.008 + ((protViz::parentIonMass(x) + 1.008) / 2)},
                   m3Hplus1 = function(x){1.008 + ((protViz::parentIonMass(x) + 2 * 1.008) / 3)})),
        iRT = list(
            peptides = c("LGGNEQVTR", "GAGSSEPVTGLDAK", "VEATFGVDESNAK", "YILAGVENSK",
                         "TPVISGGPYEYR", "TPVITGAPYEYR", "DGLDAASYYAPVR",
                         "ADVTPADFSEWSK", "GTFIIDPGGVIR","GTFIIDPAAVIR",
                         "LFLQFGAQGSPFLK"),
            ion = list(m2Hplus0 = function(x){(protViz::parentIonMass(x) + 1.008) / 2},
                       m2Hplus1 = function(x){1.008 + ((protViz::parentIonMass(x) + 1.008) / 2)},
                       m2Hplus2 = function(x){2 * 1.008 + ((protViz::parentIonMass(x) + 1.008) / 2)}))
    )
    
    output$statistics <- renderTable({
        if (is.null(getXIC())){
            n <- 0
        }else{
            n <- processXIC() |> nrow()
        }
        data.frame(
            key = c("nrow",
                    "available ions"),
            value = c(n,
                      getProteins()[[input$proteins]][['ion']] |> names() |> paste(collapse = ", "))
        )
    })
    
    output$header <- renderTable({
      if (is.null(getHeaders())){
        NULL
      }else{
        getHeaders()
      }
    })
    
    output$table <- renderTable({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message="rendering DataTable ...")
      
      subsetXIC()
      })
    
    
    getRootDir <- reactive({
     # "/Users/cp/project/2023/20230403--Streptavidin"
     d <- "/scratch/rawrr_shiny_demo/"
     if (file.exists(d)) return (d)

     d <- file.path(Sys.getenv('HOME'), 'Downloads')
     if (file.exists(d)) return (d)

     stopifnot(file.exists(d))
      d
    })
    
    getRawFiles <- reactive({
      list.files(getRootDir(), pattern = "*.raw$")
    })
    
    getProteins <- reactive({
        config
    })
    
    
    output$rawfiles <- renderUI({
      selectInput(inputId = "rawfiles",
                  label = "rawfile",
                  choices = getRawFiles(),
                  multiple = TRUE)
    })
    
    output$filter <- renderUI({
      selectInput(inputId = "filter",
                  label = "filter",
                  choices = c("Full ms", "Full ms2"),
                  multiple = FALSE)
    })
    
    output$tol <- renderUI({
      numericInput(inputId = "tol",
                  label = "tol ppm",
                  10,
                  min = 1,
                  max = 50)
    })
    
    output$proteins <- renderUI({
      selectInput(inputId = "proteins",
                  label = "proteins",
                  choices = names(getProteins()),
                  multiple = FALSE)
    })
    
    output$process <- renderUI({
      
      multiple = FALSE
 
      selectInput(inputId = "process",
                  label = "process",
                  choices = c("None", "MaxPeak0","MaxPeak0Fit",
                              "MaxPeak", 
                              "MaxPeakFit"),
                  multiple = FALSE)
    })
    
    output$peptides <- renderUI({
      
      multiple = TRUE
      if (input$filter == "Full ms2") {multiple = FALSE}
      
      selectInput(inputId = "peptides",
                  label = "peptides",
                  choices = getProteins()[[input$proteins]]['peptides'],
                  multiple = multiple)
    })
    
    output$ions <- renderUI({
      if (input$filter != "Full ms2") {
        selectInput(inputId = "ions",
                    label = "ions",
                    choices = getProteins()[[input$proteins]][['ion']] |> names(),
                    selected = getProteins()[[input$proteins]][['ion']] |> names(),
                    multiple = TRUE)
      }
    })
    
    output$fragments <- renderUI({
      if (input$filter == "Full ms2") {
        abcxyz <- protViz::fragmentIon(input$peptides, FUN=function(b, y){cbind(y)}) |>
          as.data.frame() |>
          rownames()
        
        selectInput(inputId = "fragments",
                    label = "fragment ion",
                    choices = abcxyz,
                    selected = abcxyz,
                    multiple = TRUE)
      }
    })
    
    output$tabs <- renderUI({
      #if (length(input$peptides) > 0 && length(input$rawfiles) > 0){
        tabsetPanel(
          tabPanel("XIC", list(helpText(paste("displays the XIC of ",
                                              getProteins()[[input$proteins]][['ion']] |> names() |> paste(collapse = ", "),
                                              "peptide sequences.")),
                               plotOutput("xicPlot",
                                          width = 400 * length(pep_d()),
                                          height = 200 * length(rf_d())))),
          tabPanel("header", list(helpText("Shows output of rawrr::readFileHeader() method."),
                                  htmlOutput("header"))),
          tabPanel("data", list(helpText("Shows input for ggplot2"),
                                  htmlOutput("table")))
        )
      #}else{
       #   helpText("select peptide and rawfile.")
      # }
    })
    
    
    
   rf <- reactive({
       if(is.null(input$rawfiles))
           NA
       else
            input$rawfiles
    })
   rf_d <- debounce(rf, 3000)

   
   pep <- reactive({
       if(is.null(input$peptides))
           NA
       else
           input$peptides
   })
   pep_d <- debounce(pep, 3000)
   
   getHeaders <- eventReactive(rf_d(), {
     h <- c("RAW file", "RAW file version", "Instrument name", "Number of scans", "Serial number", "Creation date")
     
     withProgress(message = 'Reading file header of', value = 0, {
       lapply(input$rawfiles, function(x){file.path(getRootDir(), x)}) |>
         lapply(FUN = function(f){
           incProgress(1/length(input$rawfiles), detail = paste( f |> basename()))
           rawrr::readFileHeader(f);
           }) |>
         lapply(function(x){
           x[h] |> as.data.frame()
         }) |> Reduce(f = rbind)
     })})
   
   getXIC <- eventReactive(c(pep_d(), rf_d(), input$tol, input$filter), {
     
     progress <- shiny::Progress$new()
     on.exit(progress$close())
    
     
     if (length(input$peptides) > 0 &&
         length(input$rawfiles) > 0 &&
         input$filter == "Full ms"){
       
       ion.mass <- lapply(getProteins()[[input$proteins]][['ion']] , function(f){f(input$peptides)}) |> unlist() |> as.vector()
       ion.peptide <- rep(input$peptides, getProteins()[[input$proteins]][['ion']] |> length())  |> unlist() |> as.vector()
       ion.charge <- lapply(getProteins()[[input$proteins]][['ion']] |> names(), function(x){rep(x, length(input$peptides))}) |> unlist() |> as.vector()
       
       rf <- lapply(input$rawfiles, function(x){file.path(getRootDir(), x)}) |> unlist()
      
       progress$set(message='rawrr::readChromatogram ...')
       X <- lapply(rf,
                   FUN = function(f, ...){
                     progress$inc(1 / length(rf), detail = paste0('processing file ', f |> basename()))
                     rawrr::readChromatogram(f, ...)},
                   mass = ion.mass,
                   filter = input$filter,
                   tol = input$tol)
       
       stopifnot(length(rf) == length(X))
       
       X <- lapply(X, function(y){
         for(i in 1:length(ion.peptide)){
           y[[i]]$peptide <- ion.peptide[i]
           y[[i]]$charge <- ion.charge[i]
         }
         return(y)
       }) |> unlist(recursive = FALSE)
       return(X)
     }else if (length(input$peptides) > 0 &&
               length(input$rawfiles) > 0 &&
               input$filter == "Full ms2"){
       
       ## here we can have only one peptide!
       abcxyz <- protViz::fragmentIon(input$peptides, FUN=function(b, y){cbind(y)}) |>
         as.data.frame()
       
       rf <- lapply(input$rawfiles, function(x){file.path(getRootDir(), x)}) |> unlist()
       
       ion.mass <- abcxyz
       ion.peptide <- rep(input$peptides, nrow(abcxyz))
       ion.charge <- abcxyz |> rownames()
       
       progress$set(message='rawrr::readChromatogram ...')
       X <- lapply(rf,
                   FUN = function(f, ...){
                     progress$inc(1 / length(rf), detail = paste0('processing file ', f |> basename()))
                     x <- rawrr::readChromatogram(f, ...)
                     stopifnot(all(sapply(x, FUN = rawrr::is.rawrrChromatogram)))
                     return(x)},
                   mass = ion.mass$mass,
                   filter = input$filter,
                   tol = input$tol)
       
       stopifnot(length(rf) == length(X))
       
       X <- lapply(X, function(y){
         for(i in 1:length(ion.peptide)){
           y[[i]]$peptide <- ion.peptide[i]
           y[[i]]$charge <- ion.charge[i]
         }
         return(y)
       }) |> unlist(recursive = FALSE)
       
       
       return(X)
       
       }else{
       return(NULL)
     }
     NULL
   })
         
   .peak0 <- function(y){
     idx.max <- which(y$intensities == max(y$intensities))[1]
     n <- 10
     idx <- seq(max(0, idx.max - n), min(idx.max + n, length(y$intensities)))
     y$times <- y$times[idx]
     y$intensities <- y$intensities[idx]
     y
   }
   
   processXIC <- reactive({
     message("processXIC ...")
     X <- getXIC()
     
     progress <- shiny::Progress$new()
     on.exit(progress$close())
     progress$set(message="Post-processing ...")
     
     if (input$process == "MaxPeak0"){
       X <- X |> lapply(FUN=.peak0) 
     }
     else if (input$process == "MaxPeak0Fit"){
       X <- X |>  lapply(FUN=.peak0) |>
         rawrr:::fitPeak.rawrrChromatogram() |>
         lapply(FUN = function(x){x$times <- x$xx; x$intensities <- x$yp; x})
     }
     else if (input$process == "MaxPeak"){
       X <- X |> rawrr:::pickPeak.rawrrChromatogram()
     }
     else if (input$process == "MaxPeakFit"){
       X <- X |> rawrr:::pickPeak.rawrrChromatogram() |>
         rawrr:::fitPeak.rawrrChromatogram() |>
         lapply(FUN = function(x){x$times <- x$xx; x$intensities <- x$yp; x})
     }
     
     message(paste("lenght of X", length(X)))
     ## reshape to data.frame
     X <- X |> 
       lapply(FUN = function(y){
         message(paste("lenght of y", length(y)))
         #if (length(y) != 7) return(NULL)
         print(head(y$times))
         print(head(y$charge))
         print(head(y$mass))
         print(head(y$peptide))
         data.frame(times = y$times,
                    intensities = y$intensities,
                    mass = y$mass,
                    filter = y$filter,
                    pepitde = y$peptide,
                    charge = y$charge,
                    filename = basename(attributes(y)$filename))
       }) |> 
       Reduce(f = rbind) 
     return(X)
   })
   subsetXIC <- reactive({
     progress <- shiny::Progress$new()
     on.exit(progress$close())
     progress$set(message = "subsetting ...")
     message("subsetting ...")
     message(paste("number of rows = ", nrow(processXIC() )))
     if (input$filter == "Full ms")
       X <- processXIC() |> subset(charge %in% input$ions)
     else if (input$filter == "Full ms2")
       X <- processXIC() |> subset(charge %in% input$fragments)
     else X <- NULL
     return(X)
   })
   
   
   output$xicPlot <- renderPlot({
     if (is.null(processXIC())){
       NULL
     }else if (nrow(processXIC()) > 0){
       progress <- shiny::Progress$new()
       on.exit(progress$close())
       progress$set(message = "GG plotting ...")
       X <- subsetXIC()
       print(head(X))
       
       message(paste("number of rows = ", nrow(X)))
       print(head(X))
       
       gp <- ggplot2::ggplot(X, ggplot2::aes_string(x = "times", y = "intensities")) +
         ggplot2::geom_line(stat='identity', size = 1,
                            ggplot2::aes_string(group = "charge", colour = factor(paste(cbind(X$mass))))) +
         ggplot2::facet_wrap(X$filename ~ X$pepitde, scales="free", nrow = length(input$rawfiles))
       gp
       #+
       # ggplot2::scale_color_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
     }else{NULL}
   })
}
