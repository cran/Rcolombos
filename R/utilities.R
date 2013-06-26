#' Returns a character vector corresponding to the currently available organisms.
#'
#' @return A list containing the currently available organisms.
#'
#' @references http://colombos.net
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library('Rcolombos')
#' listOrganisms()
#' }
#'
listOrganisms <- function(){
  header.field = c('Content-Type' = "application/json")
  curl <- getCurlHandle() 
  curlSetOpt(.opts = list(httpheader = header.field, verbose = FALSE), curl = curl) 
  t <- basicTextGatherer()
  h <- basicHeaderGatherer()
  body = curlPerform(url = "http://rest.colombos.net/get_organisms",
                     curl = curl,
                     writefunction = t$update,
                     headerfunction = h$update)
  output <- list(data = t$value(),
                 status = h$value()[['status']],
                 status.message = h$value()[['statusMessage']])
  httpstatus <- as.numeric(output$status)
  if (httpstatus != 200) {
    return(output$status.message)
  } else {
    tmp <- fromJSON(output$data, nullValue = NA)$data;
    response <- data.frame(matrix(unlist(tmp, recursive=F), length(tmp), 3, byrow=T))
    for (i in 1:3) {
      response[,i] <- sapply(response[,i], as.character)
    }
    colnames(response) <- c("name", "description", "nickname")
    return(response)
  }
}

#' This method takes as parameter a single string, representing an organism,
#' and returns a character vector corresponding to the currently available organisms.
#'
#' @param organism A string containing the organism of interest.
#'
#' @return A data.frame containing the locustag and description
#' of all the genes for the selected organism.
#'
#' @references http://colombos.net
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library('Rcolombos')
#' listGenes()
#' }
#'
listGenes <- function(organism="ecoli"){
  header.field = c('Content-Type' = "application/json")
  curl <- getCurlHandle() 
  curlSetOpt(.opts = list(httpheader = header.field, verbose = FALSE), curl = curl) 
  t <- basicTextGatherer()
  h <- basicHeaderGatherer()
  body = curlPerform(url=paste("http://rest.colombos.net/get_genes/", organism, sep=""),
                     curl = curl,
                     writefunction = t$update,
                     headerfunction = h$update)
  output <- list(data = t$value(),
                 status = h$value()[['status']],
                 status.message = h$value()[['statusMessage']])
  httpstatus <- as.numeric(output$status)
  if (httpstatus != 200) {
    return(output$status.message)
  } else {
    tmp <- fromJSON(output$data, nullValue = NA)$data;
    response <- data.frame(matrix(unlist(tmp, recursive=F), length(tmp), 2, byrow=T))
    for (i in 1:2) {
      response[,i] <- sapply(response[,i], as.character) # sanitize the list in the df
    }
    colnames(response) <- c("locustag", "gene_name")
    return(response)
  }
}

#' This method takes as parameter a single string, representing an organism,
#' and returns a character vector corresponding to the currently available organisms.
#'
#' @param organism A string containing the organism of interest.
#'
#' @return A data.frame containing the contrasts and GSM
#' of all the contrasts for the selected organism.
#'
#' @references http://colombos.net
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library('Rcolombos')
#' listContrasts()
#' }
#'
listContrasts <- function(organism="ecoli"){
  header.field = c('Content-Type' = "application/json")
  curl <- getCurlHandle() 
  curlSetOpt(.opts = list(httpheader = header.field, verbose = FALSE), curl = curl) 
  t <- basicTextGatherer()
  h <- basicHeaderGatherer()
  body = curlPerform(url=paste("http://rest.colombos.net/get_contrasts/", organism, sep=""),
                     curl = curl,
                     writefunction = t$update,
                     headerfunction = h$update)
  output <- list(data = t$value(),
                 status = h$value()[['status']],
                 status.message = h$value()[['statusMessage']])
  httpstatus <- as.numeric(output$status)
  if (httpstatus != 200) {
    return(output$status.message)
  } else {
    tmp <- fromJSON(output$data, nullValue = NA)$data;
    response <- data.frame(matrix(unlist(tmp, recursive=F), length(tmp), 2, byrow=T))
    for (i in 1:2) {
      response[,i] <- sapply(response[,i], as.character) # sanitize the list in the df
    }
    colnames(response) <- c("name", "description")
    return(response)
  }
}
