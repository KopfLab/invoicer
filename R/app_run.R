#' Run the user interface
#' @param data_folder where to find the data for this instance of the app - this folder needs to have, at least, an accounts.xlsx file
#' @param port the localhost port where the app will be accessible, e.g. https://127.0.0.1:5000 (note that if it is a port that is open in your firewall such as 3838, the GUI will be accessible on your local network at your IP address https://xxx.xxx.xxx.xxx:3838)
#' @export
start_app <- function(data_folder, log = TRUE, debug = FALSE, port = 5000) {
  start_gui(
    data_folder = data_folder,
    launch = TRUE,
    log = log,
    debug = debug,
    dev = FALSE,
    port = port
  )
}

#' Run the user interface on a server
#'
#' @export
start_app_server <- function(data_folder, log = TRUE, debug = TRUE) {
  start_gui(
    data_folder = data_folder,
    launch = FALSE,
    log = log,
    debug = debug,
    dev = FALSE,
    port = 3838
  )
}

# start gui
# @param ... parameters passed on to runApp
start_gui <- function(data_folder, launch, log, debug, dev, ...) {

  # safety check for knitting
  if (isTRUE(getOption('knitr.in.progress'))) {
    warning("cannot launch the GUI during knitting", call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  # safety check for parameters
  stopifnot(
    "'data_folder' required" = !missing(data_folder) && is_scalar_character(data_folder),
    "<data_folder>/accounts.xlsx is missing" = file.exists(file.path(data_folder, "accounts.xlsx")),
    "'launch' required" = !missing(launch) && is_scalar_logical(launch),
    "'log' required" = !missing(log) && is_scalar_logical(log),
    "'debug' required" = !missing(debug) && is_scalar_logical(debug),
    "'dev' required" = !missing(dev) && is_scalar_logical(dev)
  )

  # set settings
  if (debug) Sys.setenv("LOG_LEVEL" = "DEBUG")
  else if (log) Sys.setenv("LOG_LEVEL" = "INFO")
  else Sys.setenv("LOG_LEVEL" = "WARN")
  if (dev) Sys.setenv("INVOICER_DEV" = "ON")
  else Sys.setenv("INVOICER_DEV" = "OFF")

  # generate app
  app <- shinyApp(ui = ui(), server = server(data_folder = data_folder))

  # launch if local
  if (launch) {
    runApp(app, display.mode = "normal", ...)
  } else {
    return(app)
  }
}

# internal function to start app in development mode
start_app_dev <- function() {
  start_gui(
    data_folder = "test",
    launch = TRUE,
    log = TRUE,
    debug = TRUE,
    dev = TRUE,
    port = 1234
  )
}

