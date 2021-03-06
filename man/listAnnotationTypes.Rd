% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utilities.R
\name{listAnnotationTypes}
\alias{listAnnotationTypes}
\title{This method takes as parameter a string (the nickname of an organism) and returns a character vector
corresponding to the currently available annotation type for the selected organism.}
\usage{
listAnnotationTypes(organism = "ecoli")
}
\arguments{
\item{organism}{A character containing the organism id: use \code{\link{listOrganisms}} to display
the available organisms.}
}
\value{
A data.frame containing the name and description of the annotation
for the selected organism.
}
\description{
This method takes as parameter a string (the nickname of an organism) and returns a character vector
corresponding to the currently available annotation type for the selected organism.
}
\examples{
\dontrun{
library('Rcolombos')
listAnnotationTypes()
}
}
\references{
http://colombos.net
}

