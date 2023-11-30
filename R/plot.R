#R
# contains selected rawDiag plot methods
# 

#' reads selected raw file trailer for rawDiag plot functions
#' 
#' @inheritParams readFileHeader
#' @export 
read.raw <- function(rawfile){
  message("reading index for ", basename(rawfile), "...")
  
  rawfile |> 
    rawrr::readIndex() -> rawrrIndex
  rawrrIndex$rawfile <- basename(rawfile)
  
  rawfile |>
    rawrr::readTrailer() -> trailerNames
  
  rawrrIndex$ElapsedScanTimesec <- c(diff(rawrrIndex$StartTime), NA)
  
  if ("LM m/z-Correction (ppm):" %in% trailerNames){
    message("reading LM m/z-Correction (ppm) ...")
    rawfile |> 
      rawrr::readTrailer("LM m/z-Correction (ppm):") |> 
      as.numeric() -> LMCorrection
    rawrrIndex$LMCorrection <- LMCorrection
  }
  
  if ("AGC:" %in% trailerNames){
    message("reading AGC ...")
    rawfile |> 
      rawrr::readTrailer("AGC:") -> AGC
    rawrrIndex$AGC <- AGC
  }
  
  if ("AGC PS Mode:" %in% trailerNames){
    message("reading PrescanMode ...")
    rawfile |> 
      rawrr::readTrailer("AGC PS Mode:") -> PrescanMode
    rawrrIndex$PrescanMode <- PrescanMode
  }
  
  if ("FT Resolution:" %in% trailerNames){
    message("reading FTResolution ...")
    rawfile |> 
      rawrr::readTrailer("FT Resolution:") |>
      as.numeric() -> FTResolution
    rawrrIndex$FTResolution <- FTResolution
  }
  
  rawrrIndex
}

#' lock mass correction plot
#' 
#' @param x a \code{\link{data.frame}} fullfilling the \code{is.rawDiag} column naming criteria.
#' @param method a character 'trellis', 'violin' or 'overlay'.
#' @return a ggplot2 object
#' @author Christian Trachsel (2017), Christian Panse (2023)
#' @references rawDiag \doi{10.1021/acs.jproteome.8b00173}
#' @examples 
#' rawrr::sampleFilePath() |>
#'   read.raw() |>
#'   rawrr::plotLockMassCorrection()
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_line labs scale_x_continuous facet_wrap theme_light
#' @export
plotLockMassCorrection <- function(x, method = 'trellis'){
  stopifnot("LMCorrection" %in% colnames(x))
  
  x |>
    base::subset(x['MSOrder'] == "Ms") -> x
  
  if (method %in% c('trellis')){
    x |>
      ggplot2::ggplot(ggplot2::aes_string(x = "StartTime" , y = "LMCorrection")) +
      ggplot2::geom_hline(yintercept = c(-5, 5), colour = "red3", linetype = "longdash") +
      ggplot2::geom_line(size = 0.3) +
      ggplot2::geom_line(stat = "smooth",
                         method= "gam",
                         formula = y ~ s(x, bs ="cs"),
                         colour = "deepskyblue3", se = FALSE) -> gp
  }else if(method %in% c('overlay')){
    x |>
      ggplot2::ggplot(ggplot2::aes_string(x = "StartTime" , y = "LMCorrection", colour = "rawfile")) +
      ggplot2::geom_hline(yintercept = c(-5, 5), colour = "red3", linetype = "longdash") +
      ggplot2::geom_line(size = 0.3) +
      ggplot2::geom_line(stat = "smooth",
                         method= "gam",
                         formula = y ~ s(x, bs ="cs"),
                         colour = "deepskyblue3", se = FALSE) -> gp
  }else{
    x |>
      ggplot2::ggplot(ggplot2::aes_string(x = "StartTime" , y = "LMCorrection")) +
      ggplot2::geom_hline(yintercept = c(-5, 5), colour = "red3", linetype = "longdash") + 
      ggplot2::geom_violin() -> gp
  }
  gp +
    ggplot2::labs(title = "Lock mass correction plot") +
    ggplot2::labs(subtitle = "Plotting lock mass correction value versus retention time") +
    ggplot2::labs(x = "Retention Time [min]", y = "Lock Mass Correction [ppm]") +
    ggplot2::scale_x_continuous(breaks = base::pretty(8)) +
    # scale_y_continuous(breaks = scales::pretty_breaks(8), limits = c(-10, 10)) +
    ggplot2::facet_wrap(~ rawfile) +
    ggplot2::theme_light() -> gp
  
  gp
}
