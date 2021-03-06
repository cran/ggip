% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coord-ip.R
\name{coord_ip}
\alias{coord_ip}
\title{Coordinate system for IP data}
\usage{
coord_ip(
  canvas_network = ip_network("0.0.0.0/0"),
  pixel_prefix = 16,
  curve = c("hilbert", "morton"),
  expand = FALSE
)
}
\arguments{
\item{canvas_network}{An \code{\link[ipaddress:ip_network]{ip_network}} scalar that
determines the region of IP space visualized by the entire 2D grid. The
default shows the entire IPv4 address space.}

\item{pixel_prefix}{An integer scalar that sets the prefix length of the
network represented by a single pixel. The default value is 16. Increasing
this effectively improves the resolution of the plot.}

\item{curve}{A string to choose the space-filling curve. Choices are
\code{"hilbert"} (default) and \code{"morton"}.}

\item{expand}{If \code{TRUE}, adds a small expanded margin around the data grid.
The default is \code{FALSE}.}
}
\description{
A ggplot2 coordinate system that maps a range of IP address space onto a
two-dimensional grid using a space-filling curve.

\code{coord_ip()} forms the foundation of any ggip plot. It translates all
\code{\link[ipaddress:ip_address]{ip_address}} and \code{\link[ipaddress:ip_network]{ip_network}}
vectors to Cartesian coordinates, ready for use by ggplot2 layers (see
Accessing Coordinates). This ensures all layers use a common mapping.
}
\section{Accessing Coordinates}{

\code{coord_ip()} stores the result of the mapping in a nested data frame column.
This means each \code{\link[ipaddress:ip_address]{ip_address}} or
\code{\link[ipaddress:ip_network]{ip_network}} column in the original data set is
converted to a data frame column. When specifying ggplot2 aesthetics, you'll
need to use \code{$} to access the nested data (see Examples).

Each \code{\link[ipaddress:ip_address]{ip_address}} column will be replaced with a
data frame containing the following columns:\tabular{lll}{
   Column name \tab Data type \tab Description \cr
   \code{ip} \tab \code{ip_address} \tab Original IP data \cr
   \code{x} \tab \code{integer} \tab Pixel x \cr
   \code{y} \tab \code{integer} \tab Pixel y \cr
}


Each \code{\link[ipaddress:ip_network]{ip_network}} column will be replaced with a
data frame containing the following columns:\tabular{lll}{
   Column name \tab Data type \tab Description \cr
   \code{ip} \tab \code{ip_network} \tab Original IP data \cr
   \code{xmin} \tab \code{integer} \tab Bounding box xmin \cr
   \code{ymin} \tab \code{integer} \tab Bounding box ymin \cr
   \code{xmax} \tab \code{integer} \tab Bounding box xmax \cr
   \code{ymax} \tab \code{integer} \tab Bounding box ymax \cr
}
}

\examples{
suppressPackageStartupMessages(library(dplyr))

tibble(address = ip_address(c("0.0.0.0", "128.0.0.0", "192.168.0.1"))) \%>\%
  ggplot(aes(x = address$x, y = address$y, label = address$ip)) +
  geom_point() +
  geom_label(nudge_x = c(10, 0, -10), nudge_y = -10) +
  coord_ip(expand = TRUE) +
  theme_ip_light()

tibble(network = ip_network(c("0.0.0.0/8", "224.0.0.0/4"))) \%>\%
  mutate(
    start = network_address(network),
    end = broadcast_address(network)
  ) \%>\%
  ggplot() +
  geom_point(aes(x = start$x, y = start$y), color = "blue") +
  geom_point(aes(x = end$x, y = end$y), color = "red") +
  geom_rect(
    aes(xmin = network$xmin, xmax = network$xmax, ymin = network$ymin, ymax = network$ymax),
    alpha = 0.5, fill = "grey"
  ) +
  coord_ip(curve = "morton", expand = TRUE) +
  theme_ip_light()
}
\seealso{
\code{vignette("visualizing-ip-data")} describes the mapping in more detail.
}
