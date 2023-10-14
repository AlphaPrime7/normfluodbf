#' Title: Normfluodbf tracker
#' @description
#' A simple tracker for the package. A fun addition to the package.
#' @import ggplot2
#' @import cranlogs
#'
#' @author Tingwei Adeck
#' @param period takes 'last-month' or 'last-week'; default is NULL
#' @param plot takes 'cum' or 'daily'; default is null
#'
#'
#' @return A data frame of cumulative downloads or a data frame of daily downloads
#' @export
#' @note The tracker is not modular so only works for normfluodbf hence in the package
#'
#' @examples nfd_tracker(package = 'normfluodbf', period = 'last-month', plot = 'cum')

nfd_tracker <- function(package, period=NULL, plot = NULL){
  if(period == 'last-month' || plot == 'cum'){
    cran_downloads(packages=package, when="last-month") #last-week, last-month
    nfd_totals <- cran_downloads(packages=package, from = "2023-08-25", to = Sys.Date()-1)
    sum(nfd_totals[,2])

    cumulative <- cumsum(nfd_totals[,2])
    nfd_cumulative_append <- cbind(nfd_totals,cumulative)

    nfd2 <- ggplot(nfd_cumulative_append, aes(nfd_cumulative_append$date, nfd_cumulative_append$cumulative)) +
      geom_line(colour = "blue",size=1)

    nfd2 + xlab("Time") + ylab("Nr. of downloads") +
      labs(title = paste0("Normfluodbf cumulative downloads until ", Sys.Date()-1))

    print(nfd2)
    return(nfd_cumulative_append)

  } else if(period == 'last-week' || plot == 'daily'){
    cran_downloads(packages=package, when="last-week") #last-week, last-month
    nfd_totals <- cran_downloads(packages=package, from = "2023-08-25", to = Sys.Date()-1)
    sum(nfd_totals[,2])

    nfd <- ggplot(nfd_totals, aes(nfd_totals$date, nfd_totals$count)) +
      geom_line(colour = "red",size=1)

    nfd + xlab("Time") + ylab("Nr. of downloads") +
      labs(title = paste0("Normfluodbf daily downloads ", Sys.Date()-1))

    print(nfd)
    return(nfd_totals)
  }

}
