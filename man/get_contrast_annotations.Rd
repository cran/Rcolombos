% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utilities.R
\name{get_contrast_annotations}
\alias{get_contrast_annotations}
\title{This method allows to retrieve all the annotations for the Reference and Test conditions for a selected organism (nickname) and for a user specified contrast name. Please be aware that only one contrast is allowed in input. It returns a list containing both ReferenceAnnnotation and TestAnnotation.
and return the available entities}
\usage{
get_contrast_annotations(organism = "bsubt",
  contrast_name = "GSM27217.ch2-vs-GSM27217.ch1")
}
\arguments{
\item{organism}{A character containing the organism id: use \code{\link{listOrganisms}} to display
the available organisms.}

\item{contrast_name}{annotation A character containing the selected contrast_name type: use \code{\link{listContrasts}} to display the available contrast names.}
}
\value{
A list of two data.frame, ReferenceAnnnotation and TestAnnotation, containing 2 columns: both the properties and the values for the selected contrast
}
\description{
This method allows to retrieve all the annotations for the Reference and Test conditions for a selected organism (nickname) and for a user specified contrast name. Please be aware that only one contrast is allowed in input. It returns a list containing both ReferenceAnnnotation and TestAnnotation.
and return the available entities
}
\examples{
\dontrun{
 library("Rcolombos")
 out <- get_contrast_annotations(organism="bsubt",
 contrast_name="GSM27217.ch2-vs-GSM27217.ch1")
}
}
\references{
http://colombos.net
}

