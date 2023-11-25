#' Title: A time attribute generating function
#'
#' @description
#' A function to help the researcher reproduce the time attribute.
#'
#' @param interval The time interval chosen for the assay often in seconds.
#' @param first_end The end time of the initial run, often the pause for the introduction of a new substance. This can be the cycle number chosen for the initial stop.
#' @param pause_time The time between the first end (pause) and resumption of the assay.
#' @param end_time The final end time of the assay.
#' @param cycles The number of cycles in the assay as selected by the user or researcher.
#' @param time_unit The time unit used, either cycles or minutes. Works with first_end.
#'
#' @return A rounded value with three decimal places when applied to a single value or an attribute with log-transformed values.
#'
#' @export
#'
#' @examples time_test = time_attribute(30,8,136,1276,40)
#' time_test = time_attribute(60,8,136,2460,40)
#' time_test = time_attribute(0.5,4,2.26,21.26,40,'minutes')
#' time_test = time_attribute(1,7,2.26,41,40,'minutes')

time_attribute = function(interval= NULL, first_end = NULL, pause_time=NULL, end_time=NULL, cycles=NULL, time_unit = c('cycles', 'minutes')){

  start_time = 0

  if('cycles' %in% time_unit || is.null(time_unit)){

    first_end = (first_end-1) * interval

    #before_pause
    first_end = seq(from=start_time,to=first_end,by=interval)

    #new sequence start
    timer_resume = tail(first_end,1)  + pause_time

    #after_pause
    after_pause = seq(from=timer_resume,to=end_time,by=interval)

    #final time attribute
    assay_time = append(first_end,after_pause)
    assay_time = assay_time[1:cycles]
    assay_time = as.data.frame(assay_time)
    colnames(assay_time) = c('Time')

    return(assay_time)

  } else if('minutes' %in% time_unit || !is.null(time_unit)) {
    #before_pause
    interval = interval * 60

    first_end = seq(from=start_time*60,to=first_end*60,by=interval)

    first_end = first_end[-length(first_end)]

    #new sequence start
    timer_resume = tail(first_end,1)  + pause_time*60

    #after_pause
    after_pause = seq(from=timer_resume,to=end_time*60,by=interval)

    #final time attribute
    assay_time = append(first_end,after_pause)
    assay_time = assay_time[1:cycles]
    assay_time = as.data.frame(assay_time)
    colnames(assay_time) = c('Time')

    return(assay_time)

  }

}
