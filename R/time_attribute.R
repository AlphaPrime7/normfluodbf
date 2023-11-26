#' Title: A time attribute generating function
#'
#' @description
#' A function to help the researcher reproduce the time attribute.
#'
#' @param interval The time interval chosen for the assay often in seconds.
#' @param first_end The end time of the initial run, often the pause for the introduction of a new substance. This can be the cycle number chosen for the initial stop.
#' @param pause_duration The time between the first end (pause) and resumption of the assay.
#' @param end_time The final end time of the assay.
#' @param cycles The number of cycles in the assay as selected by the user or researcher.
#'
#' @return The time attribute.
#'
#' @export
#'
#' @note
#' The original function had an option for minutes which was for less time conscious people
#' but the final version for this package has no such option. Users MUST provide numbers in
#' seconds.
#'
#' @examples time_test = time_attribute(30,8,136,1276,40)
#' time_test = time_attribute(60,8,136,2460,40)


time_attribute = function(interval= NULL, first_end = NULL, pause_duration=NULL, end_time=NULL, cycles=NULL){

  start_time = 0

  if(is.null(interval)){
    warning('Enter the cycle interval in seconds as setup in the machine')
  }

  if(pause_duration < interval || is.null(pause_duration)){
    pause_duration = interval
  } else{
    pause_duration = pause_duration
  }

  # if('cycles' %in% time_unit || is.null(time_unit) && !is.null(first_end))

  if(!is.null(first_end) && !is.null(end_time) && !is.null(pause_duration)){

      first_end = (first_end-1) * interval

      #before_pause
      first_end = seq(from=start_time,to=first_end,by=interval)

      #new sequence start
      timer_resume = tail(first_end,1)  + pause_duration

      #after_pause
      after_pause = seq(from=timer_resume,to=end_time,by=interval)

      #final time attribute
      assay_time = append(first_end,after_pause)
      assay_time = assay_time[1:cycles]
      assay_time = as.data.frame(assay_time)
      colnames(assay_time) = c('Time')

      return(assay_time)

    } else {

      assay_time = seq(from=start_time,  by = interval, along.with=seq(cycles))
      assay_time = as.data.frame(assay_time)
      colnames(assay_time) = c('Time')

      return(assay_time)

  }

}
