#R
# contains selected rawDiag plot methods
# 
#' lock mass correction plot
#' @inheritParams readFileHeader
#' @return a ggplot object
#' @author Christian Trachsel (2017), Christian Panse (2023)
#' @references rawDiag \doi{10.1021/acs.jproteome.8b00173}
#' @examples 
#' rawrr::sampleFilePath() |> rawrr:::.plotLockMassCorrection()
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_line labs scale_x_continuous facet_wrap theme_light
.plotLockMassCorrection <- function(rawfile){
  stopifnot("LM m/z-Correction (ppm):" %in% rawrr::readTrailer(rawfile))
  
  message("Plotting lock mass correction for ", basename(rawfile))
  message("reading index for ", basename(rawfile), "...")
  
  rawfile |> 
    rawrr::readIndex() -> rawrrIndex
  
  message("reading LM m/z-Correction (ppm) ...")
  rawfile |> 
    rawrr::readTrailer("LM m/z-Correction (ppm):") |> 
    as.numeric() ->
    LMCorrection
  
  rawrrIndex$LMCorrection <- LMCorrection
  rawrrIndex$rawfile <- basename(rawfile)
  
  rawrrIndex |>
    base::subset(rawrrIndex$MSOrder == "Ms") |>
    ggplot2::ggplot(ggplot2::aes_string(x = "StartTime" , y = "LMCorrection")) +
    ggplot2::geom_hline(yintercept = c(-5, 5), colour = "red3", linetype = "longdash") +
    ggplot2::geom_line(size = 0.3) +
    ggplot2::geom_line(stat = "smooth",
                       method= "gam",
                       formula = y ~ s(x, bs ="cs"),
                       colour = "deepskyblue3", se = FALSE) +
    ggplot2::labs(title = "Lock mass correction plot") +
    ggplot2::labs(subtitle = "Plotting lock mass correction value versus retention time") +
    ggplot2::labs(x = "Retention Time [min]", y = "Lock Mass Correction [ppm]") +
    ggplot2::scale_x_continuous(breaks = base::pretty(8)) +
    # scale_y_continuous(breaks = scales::pretty_breaks(8), limits = c(-10, 10)) +
    ggplot2::facet_wrap(~ rawfile) +
    ggplot2::theme_light()
}
