% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utilities.R
\name{listEntities}
\alias{listEntities}
\title{This method takes a string containing the nickname for the selected organism and a string containing the annotation type
and return the available entities}
\usage{
listEntities(organism = "ecoli", annotation = "Pathway")
}
\arguments{
\item{organism}{A character containing the organism id: use \code{\link{listOrganisms}} to display
the available organisms.}

\item{annotation}{A character containing the selected annotation type: use \code{\link{listAnnotationTypes}} to display
the available types.}
}
\value{
A vector containing the available entities for the selected annotation type.
}
\description{
This method takes a string containing the nickname for the selected organism and a string containing the annotation type
and return the available entities
}
\examples{
\dontrun{
 library("Rcolombos")
 pathway_entities <- listEntities(organism="bsubt", annotation="Pathway")
 Tr_entities <- listEntities("bsubt","Transcriptional regulation")
}
}
\references{
http://colombos.net
}

