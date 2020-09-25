## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = FALSE,
  message = FALSE
)

## ----setup, warning=FALSE-----------------------------------------------------
library(ggip)

## ---- echo=FALSE, out.width="100%"--------------------------------------------
knitr::include_graphics("bits_raw.png")

## ---- echo=FALSE, out.width="100%"--------------------------------------------
knitr::include_graphics("bits_half_reduced.png")

## ---- echo=FALSE, out.width="100%"--------------------------------------------
knitr::include_graphics("bits_reduced.png")

## ----plot_func----------------------------------------------------------------
ordinal_suffix <- function(x) {
    suffix <- c("st", "nd", "rd", rep("th", 17))
    suffix[((x-1) %% 10 + 1) + 10*(((x %% 100) %/% 10) == 1)]
}

plot_curve <- function(curve, curve_order) {
  pixel_prefix <- 32L
  canvas_prefix <- as.integer(pixel_prefix - (2 * curve_order))
  canvas_network <- ip_network(ip_address("0.0.0.0"), canvas_prefix)
  n_pixels <- 2 ^ curve_order
  
  ggplot(data.frame(address = seq(canvas_network))) +
    geom_path(aes(address$x, address$y)) +
    coord_ip(
      canvas_network = canvas_network,
      pixel_prefix = pixel_prefix,
      curve = curve,
      expand = TRUE
    ) +
    theme_ip_light() +
    labs(title = paste0(
      curve_order, ordinal_suffix(curve_order),
      " order (", n_pixels, "x", n_pixels, " grid)"
    ))
}

## ----hilbert, fig.show="hold", out.width="30%"--------------------------------
plot_curve("hilbert", 2)
plot_curve("hilbert", 3)
plot_curve("hilbert", 4)

## ----morton, fig.show="hold", out.width="30%"---------------------------------
plot_curve("morton", 2)
plot_curve("morton", 3)
plot_curve("morton", 4)

