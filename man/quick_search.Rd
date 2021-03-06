% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Rcolombos.R
\name{quick_search}
\alias{quick_search}
\title{This method mimics the quick_search functionality of Colombos.
It takes a string containg the nickname for the selected organism and a vector of string
representing the genes of interest for the specified organism and returns a list containing
the locustags (gene_names), contrasts and M-values for the current selection.}
\usage{
quick_search(organism = "ecoli", genes, geneNames = FALSE)
}
\arguments{
\item{organism}{A character containing the organism id: use \code{\link{listOrganisms}} to display
the available organisms.}

\item{genes}{A vector of strings representing the genes of interest.}

\item{geneNames}{boolean if == FALSE (default) returns the locustag otherwise the gene_name for the selected genes.}
}
\value{
A data.frame containing locustag (gene_names),
contrasts and M-values for the current organism and genes.
}
\description{
This method mimics the quick_search functionality of Colombos.
It takes a string containg the nickname for the selected organism and a vector of string
representing the genes of interest for the specified organism and returns a list containing
the locustags (gene_names), contrasts and M-values for the current selection.
}
\examples{
\dontrun{
 library("Rcolombos")
 my_module <- quick_search(organism="ecoli",
                     genes=c("b0400","b2805","b0567"),
                     geneNames=FALSE)
 heatmap(as.matrix(my_module), col=terrain.colors(15))
}
}
\references{
http://colombos.net
}

