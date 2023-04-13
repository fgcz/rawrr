#
# This is the server logic of a Shiny web application. 
#


library(shiny)


# Define server logic required to draw a histogram
function(input, output, session) {
  
    config <- list(Streptavidin = list(
        peptides = c("YDSAPATDGSGTALGWTVAWK", "NAHSATTWSGQYVGGAEAR",
                     "INTQWLLTSGTTEANAWK", "STLVGHDTFTK", "VKPSAASIDAAK",
                     "AGVNNGNPLDAVQQ"),
        ion = list(mass2Hplus = function(x){(protViz::parentIonMass(x) + 1.008) / 2},
                   mass3Hplus = function(x){(protViz::parentIonMass(x) + 2 * 1.008) / 3})),
        iRT = list(
            peptides = c("LGGNEQVTR", "GAGSSEPVTGLDAK", "VEATFGVDESNAK", "YILAGVENSK",
                         "TPVISGGPYEYR", "TPVITGAPYEYR", "DGLDAASYYAPVR",
                         "ADVTPADFSEWSK", "GTFIIDPGGVIR","GTFIIDPAAVIR",
                         "LFLQFGAQGSPFLK"),
            ion = list(mass2Hplus0 = function(x){(protViz::parentIonMass(x) + 1.008) / 2},
                       mass2Hplus1 = function(x){1.008 + ((protViz::parentIonMass(x) + 1.008) / 2)},
                       mass2Hplus2 = function(x){2 * 1.008 + ((protViz::parentIonMass(x) + 1.008) / 2)}))
    )
    
    output$statistics <- renderTable({
        if (is.null(getXIC())){
            n <- 0
        }else{
            n <- processXIC() |> nrow()
        }
        data.frame(
            key = c("#files", "#peptides", "#rf_d", "#pep_d", "nrow", "ions"),
            value = c(length(input$rawfiles), length(input$peptides), length(rf_d()), length(pep_d()), n,
                      getProteins()[[input$proteins]][['ion']] |> names() |> paste(collapse = ", "))
        )
    })
    
    getRootDir <- reactive({
     # "/Users/cp/project/2023/20230403--Streptavidin"
     #   "/scratch/rawrr_shiny_demo/"
      d <- file.path(Sys.getenv('HOME'), 'Downloads')
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
    
    output$fragments <- renderUI({
      if (input$filter == "Full ms2") {
        selectInput(inputId = "framents",
                    label = "fragment ion",
                    choices = protViz::fragmentIon(input$peptides, FUN=function(b, y){cbind(b,y)}) |>
                      as.data.frame() |>
                      rownames(),
                    multiple = TRUE)
      }
    })
    
    output$tabs <- renderUI({
      if (length(input$peptides) > 0 && length(input$rawfiles) > 0){
        tabsetPanel(
          tabPanel("XIC", list(helpText(paste("displays the XIC of ",
                                              getProteins()[[input$proteins]][['ion']] |> names() |> paste(collapse = ", "),
                                              "peptide sequences.")),
                               plotOutput("xicPlot",
                                          width = 300 * length(pep_d()),
                                          height = 200 * length(rf_d()))))
        )}else{
          helpText("select peptide and rawfile.")
        }
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
   
   getXIC <- eventReactive(c(pep_d(), rf_d(), input$tol, input$filter), {
     
     progress <- shiny::Progress$new()
     on.exit(progress$close())
     
     message(length(input$peptides))
     if (length(input$peptides) > 0 && length(input$rawfiles) > 0){
       
       ion.mass <- lapply(getProteins()[[input$proteins]][['ion']] , function(f){f(input$peptides)}) |> unlist() |> as.vector()
       ion.peptide <- rep(input$peptides, getProteins()[[input$proteins]][['ion']] |> length())  |> unlist() |> as.vector()
       ion.charge <- lapply(getProteins()[[input$proteins]][['ion']] |> names(), function(x){rep(x, length(input$peptides))}) |> unlist() |> as.vector()
       
       rf <- lapply(input$rawfiles, function(x){file.path(getRootDir(), x)}) |> unlist()
       
       print(rf)
       print(ion.mass)
       print(ion.peptide)
       print(ion.charge)
       
       progress$set(message='rawrr::readChromatogram ...')
       X <- lapply(rf,
                   FUN = function(f, ...){
                     progress$inc(1 / length(rf), detail = paste0('processing file ', f |> basename()))
                     rawrr::readChromatogram(f, ...)},
                   mass = ion.mass,
                   filter = input$filter,
                   tol = input$tol)
       
       print(paste0("number of chromatograms = ", X |> length()))
       
       
       stopifnot(length(rf) == length(X))
       
       
       X <- lapply(X, function(y){
         for(i in 1:length(ion.peptide)){
           y[[i]]$peptide <- ion.peptide[i]
           y[[i]]$charge <- ion.charge[i]
         }
         return(y)
       }) |> unlist(recursive = FALSE)
       return(X)
       
     }
     NULL
   })
         
   .peak <- function(y){
     idx.max <- which(y$intensities == max(y$intensities))[1]
     n <- 10
     idx <- seq(max(0, idx.max - n), min(idx.max + n, length(y$intensities)))
     y$times <- y$times[idx]
     y$intensities <- y$intensities[idx]
     y
   }
   
   processXIC <- reactive({
     progress <- shiny::Progress$new()
     on.exit(progress$close())
     progress$set(message="Post-processing ...")
     
     X <- getXIC()
     
     if (input$process == "MaxPeak0"){
       X <- X |> lapply(FUN=.peak) 
     }
     else if (input$process == "MaxPeak0Fit"){
       X <- X |>  lapply(FUN=.peak) |>
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
     
     ## reshape to data.frame
     X <- X |> 
       lapply(FUN = function(y){
         data.frame(times = y$times,
                    intensities = y$intensities,
                    mass = y$mass,
                    filter = y$filter,
                    pepitde = y$peptide,
                    charge = y$charge,
                    filename = basename(attributes(y)$filename))
       }) |> Reduce(f = rbind)
     return(X)
   })
   
   
       output$xicPlot <- renderPlot({
         X <- processXIC()
         
         if (is.null(X)){
           plot(0,0)
         }else{
           progress <- shiny::Progress$new()
           on.exit(progress$close())
           progress$set(message="GG Plotting ...")
           print(head(X))
           gp <- ggplot2::ggplot(X, ggplot2::aes_string(x = "times", y = "intensities")) +
             ggplot2::geom_line(stat='identity', size = 1,
                                ggplot2::aes_string(group = "charge", colour = factor(paste(cbind(X$mass))))) +
             ggplot2::facet_wrap(X$filename ~ X$pepitde, scales="free", nrow = length(input$rawfiles)) 
           gp 
         }
         
       })
       
}
