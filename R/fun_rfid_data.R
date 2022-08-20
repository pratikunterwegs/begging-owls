
#' Classify visits from RFID detections.
#'
#' @param df A data.frame or
#' @param min_interval The minimum interval between detections at the nest box
#' in seconds. Defaults to 120 seconds; 2 minutes.
#'
#' @return A data.table of visits per tag.
#' @export
classify_visits = function(df, min_interval = 120) {
  assertthat::assert_that(
    all(c("date", "time", "tag") %in% colnames(df))
  )
  
  data.table::setDT(df)
  
  df[, date_time := lubridate::dmy_hms(
    sprintf("%s %s", date, time), 
    tz = "Asia/Jerusalem")
  ]
  
  df[, interval := c(0, as.numeric(diff(date_time))), by = "tag"]
  
  df[, visit := 1 + cumsum(
    as.numeric(interval > min_interval)
  ), by = "tag"]
  
  df[, list(
    visit_start = min(date_time),
    visit_end = max(date_time)
  ), by = c("tag", "visit")]
}

#' Get visits from RFID data files.
#'
#' @param path_to_files Paths to files with RFID detection data.
#' @param min_interval The minimum interval between detections at the nest box
#' in seconds. Defaults to 120 seconds; 2 minutes. Passed to `classify_visits`.
#'
#' @return A data.table with visits per tag.
#' @export
visits_from_rfid = function(path_to_files, min_interval) {
  filepath = list.files(
    path = path_to_files,
    pattern = "txt",
    full.names = TRUE,
    recursive = TRUE
  )
  
  # extract nest box numbers as a three number string
  nestbox = stringr::str_extract(filepath, "Ha(\\d{3})")
  
  rfid_data = lapply(
    X = filepath, FUN = fread,
    skip = 1, 
    col.names = c("date", "time", "unit1", 
                  "unit2", "x1", "x2", "tag")
  )
  
  visit_data = lapply(rfid_data, classify_visits)
  
  # add nestbox data
  visit_data = Map(
    visit_data, nestbox, f = function(df, nb) {
      df$nestbox = nb
      df
    })
  
  data.table::rbindlist(visit_data)
}
