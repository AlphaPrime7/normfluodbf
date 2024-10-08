#' Random Port Normfluodbf Style
#' @return random port
#' @export
#' @details
#' Recursively find a random port that does not fall in the unsafe group
#' Added some more unsafe ports for like PostgresDb (5432), MySQL (3306), StreamlitApp (8501),
#' ngrok (4040), pinggy (4300), Flask (5000), Django (8000) and ReactApp (3000).
#' Learnt a thing or two from Dean with recursion (Really Cool!!!).
find_random_port <- function(){

  if ("random_port" %in% getNamespaceExports("pbdZMQ")) {
    random_port <- pbdZMQ::random_port(min_port = 3000, max_port = 9000)
    unsafe_ports <- c(3000, 3306, 3659, 4040, 4045, 4300, 5000, 5060, 5061, 5432, 6000, 6566, 6665:6669, 6697, 8000, 8501)
  }
  else {
    random_port <- shiny_random_port()
  }

  if (random_port %in% unsafe_ports) {
    find_random_port()
  }
  else {
    return(random_port)
  }
}

#' Random Port gptstudio style
#' @return port
#' @export
shiny_random_port <- function() {
  all_ports <- 3000:9000
  unsafe_ports <- c(3000, 3306, 3659, 4040, 4045, 4300, 5000, 5060, 5061, 5432, 6000, 6566, 6665:6669, 6697, 8000, 8501)
  safe_ports <- setdiff(all_ports, unsafe_ports)
  sample(safe_ports, size = 1)
}

#' Validate URL
#' @param url url
#' @return NULL
wait_for_bg_shinyapp <- function(url) {
  httr2::request(url) %>%
    httr2::req_retry(max_seconds = 10, backoff = function(n) 0.2) %>%
    httr2::req_perform()
}

#' Run Demo Script
#' @param appDir App Dir
#' @param port Port
#' @param host Host
#' @return Script
#' @details
#' A quick script inspired by gptstudio to aid in running the cool normfluodbf demo.
#' @export
run_demo_script <- function(appDir = system.file("shiny/demo", package = "normfluodbf"),
                            port,
                            host) {
  script_file <- tempfile(fileext = ".R")

  if (!file.exists(appDir)) {
    stop("The Shiny app directory does not exist:", appDir)
  }

  line <-
    glue::glue(
      "shiny::runApp(appDir = '{appDir}', port = {port}, host = '{host}')"
    )

  file_con <- file(script_file)
  writeLines(line, con = script_file)
  close(file_con)
  return(script_file)
}

#' Run Demo in Background
#' @param appDir dir
#' @param job_name job name
#' @param host host ip (localhost)
#' @param port port
#' @return NULL (run script)
#' @export
run_demo_in_background <- function(appDir = system.file("shiny/demo", package = "normfluodbf"),
                                   job_name,
                                   host,
                                   port) {
  if (!file.exists(appDir)) {
    stop("The Shiny app directory does not exist:", appDir)
  }

  job_script <- run_demo_script(
    appDir = appDir,
    port = port,
    host = host
  )
  rstudioapi::jobRunScript(job_script,
                           name = job_name)
  cli::cli_alert_success(
    glue::glue("{job_name} initialized as background job in RStudio")
  )
}

#' View App in Viewer Pane
#' @param host host
#' @param port port
#' @return NULL
#' @export
viewerpane_background_normfluodbf <- function(host, port) {
  url <- glue::glue("http://{host}:{port}")
  translated_url <- rstudioapi::translateLocalUrl(url, absolute = TRUE) #no difference except double quotes

  if (host %in% c("127.0.0.1")) {
    cli::cli_inform(c(
      "i" = "Showing app in 'Viewer' pane",
      "i" = "Run {.run rstudioapi::viewer(\"{url}\")} to see it"
    ))
  } else {
    cli::cli_alert_info("Showing app in browser window")
  }

  if (.Platform$OS.type == "unix") {
    wait_for_bg_shinyapp(translated_url)
  }

  tryCatch({
    rstudioapi::viewer(translated_url)
  }, error = function(e) {
    message("Failed to open in RStudio Viewer, opening in default browser instead.")
    browseURL(url)
  })

}
