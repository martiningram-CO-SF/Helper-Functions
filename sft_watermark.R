#' Add a diagonal watermark to a ggplot
#'
#' @param text String to display in the watermark. Defaults to "DRAFT-SFT".
#' @param text_size Numeric font size (in points). Defaults to 50.
#' @param text_alpha Numeric alpha transparency. Defaults to 0.1.
#' @param text_color Hex code for text colour. Defaults to CO nightfall (#00336D).
#'
#' @return A ggplot2 annotation layer that scales independently of axes.
#' @export
sft_watermark <- function(text = "DRAFT-SFT", text_size = 50, text_alpha = 0.1, text_color = "#00336D") {
    ggplot2::annotation_custom(
        grid::textGrob(
            label = text,
            gp = grid::gpar(
                col = text_color,
                fontsize = text_size,
                alpha = text_alpha,
                fontface = "bold"
            ),
            rot = 45
        ),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    )
}