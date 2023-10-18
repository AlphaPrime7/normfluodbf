#' Title: An adopted function to track normfluodbf
#' @description
#' A function that tracks normfluodbf statistics
#'
#' @author Gary Hutson (in R Bloggers)
#' @author Adopted: Tingwei Adeck
#'
#' @import dlstats
#' @import tibble
#'
#' @param packages A string == name of the package
#'
#' @return A tibble showing download statistics per month, alongside a plot for visualization.
#' @export
#'
#' @seealso [nfd_tracker()]
#'
#' @examples package_trackeR(c("normfluodbf"))

package_trackeR <- function(packages){

  dl <- dlstats::cran_stats(c(packages))

  plot <- ggplot(dl,
                 aes(end, downloads, group=package)) +
    geom_line(aes(color=package),linetype="dashed") +
    geom_point(aes(shape=package, color=package)) +
    theme_minimal()
  plot <- plot + xlab("Download date") +
    ylab("Number of downloads")

  returns_list <- list("download_df"=as_tibble(dl),
                       "downloads_to_date"=sum(dl$downloads),
                       "downloads_plot"=plot)

  return(returns_list)
}

