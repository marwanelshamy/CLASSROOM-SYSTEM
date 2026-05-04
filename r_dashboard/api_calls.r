# api_calls.R
library(httr); library(jsonlite)

BASE <- "http://127.0.0.1:8000"

login_doctor <- function(username, password) {
  df <- read.csv("C:/Users/Hero/classroom_system/data/doctors.csv")
  match <- df[df$Username == username & df$Password == password, ]
  if (nrow(match) > 0) match else NULL
}

get_schedule <- function(doctor_id) {
  res <- GET(paste0(BASE, "/schedule/", doctor_id))
  fromJSON(content(res, "text", encoding="UTF-8"))
}

start_session <- function(lecture_id, doctor_id) {
  res <- POST(paste0(BASE, "/start_session"),
              query = list(lecture_id=lecture_id, doctor_id=doctor_id))
  fromJSON(content(res, "text", encoding="UTF-8"))
}

get_attendance <- function(session_id) {
  res <- GET(paste0(BASE, "/attendance/", session_id))
  fromJSON(content(res, "text", encoding="UTF-8"))$records
}

end_session <- function(session_id) {
  POST(paste0(BASE, "/end_session/", session_id))
}