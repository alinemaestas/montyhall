% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Host opens a goat door}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{...}{no arguments are used by the function.}
}
\value{
The function returns a length of 1 numeric vector indicating
that a one door has been selected that is different from
the door that the contestant selected.
}
\description{
The host will select a door the contestant did not select.
}
\details{
The host will open a goat door the contestant did not select.
}
\examples{
open_goat_door( game, a.pick)

}
