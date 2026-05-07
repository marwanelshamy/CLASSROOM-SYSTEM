server <- function(input, output, session) {
  
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(ggrepel)
  students_path <- "C:/Users/Hero/classroom_system/data/StudentPicsDataset.csv"
  parents_path <- "C:/Users/Hero/classroom_system/data/parents.csv"
  attendance_path <- "C:/Users/Hero/classroom_system/data/attendance.csv"
  analytics_path <- "C:/Users/Hero/classroom_system/data/session_analytics.csv"
  
  # ── Reactive values ─────────────────────────────
  rv <- reactiveValues(
    doctor           = NULL,
    student          = NULL,
    parent           = NULL,
    portal_role      = "guest",
    session_id       = NULL,
    selected_lecture = NULL,
    class_students   = NULL,
    attendance       = data.frame(),
    schedule         = data.frame(),
    absent_students  = data.frame(),
    emotion_live     = data.frame(),
    analytics        = NULL,
    batch_meta       = NULL,
    mgmt_doctors     = data.frame(),
    mgmt_courses     = data.frame(),
    analytics_version = 0L,   # increments on session end to force analytics refresh
    schedule_version  = 0L,   # increments when admin adds a schedule entry
    dark_mode        = TRUE    # TRUE = dark, FALSE = light
  )

  output$sidebar_menu <- shinydashboard::renderMenu({
    if (rv$portal_role == "admin") {
      shinydashboard::sidebarMenu(
        id = "tabs",
        selected = "management",
        shinydashboard::menuItem("Login", tabName = "login", icon = icon("sign-in-alt")),
        shinydashboard::menuItem("Management", tabName = "management", icon = icon("user-cog")),
        tags$li(class = "sidebar-footer-btns",
          actionButton("theme_toggle", uiOutput("theme_icon"), class = "btn-theme-toggle"),
          actionButton("logout_btn", "Logout", icon = icon("sign-out-alt"), class = "btn-logout")
        )
      )
    } else if (rv$portal_role == "doctor") {
      shinydashboard::sidebarMenu(
        id = "tabs",
        selected = "schedule",
        shinydashboard::menuItem("Schedule",         tabName = "schedule",         icon = icon("calendar")),
        shinydashboard::menuItem("Session",          tabName = "session",          icon = icon("video")),
        shinydashboard::menuItem("Emotion Detector", tabName = "emotion_detector", icon = icon("smile")),
        shinydashboard::menuItem("Analytics",        tabName = "analytics",        icon = icon("chart-bar")),
        tags$li(class = "sidebar-footer-btns",
          actionButton("theme_toggle", uiOutput("theme_icon"), class = "btn-theme-toggle"),
          actionButton("logout_btn", "Logout", icon = icon("sign-out-alt"), class = "btn-logout")
        )
      )
    } else if (rv$portal_role == "student") {
      shinydashboard::sidebarMenu(
        id = "tabs",
        selected = "student_portal",
        shinydashboard::menuItem("Student Portal", tabName = "student_portal", icon = icon("user-graduate")),
        tags$li(class = "sidebar-footer-btns",
          actionButton("theme_toggle", uiOutput("theme_icon"), class = "btn-theme-toggle"),
          actionButton("logout_btn", "Logout", icon = icon("sign-out-alt"), class = "btn-logout")
        )
      )
    } else if (rv$portal_role == "parent") {
      shinydashboard::sidebarMenu(
        id = "tabs",
        selected = "parent_portal",
        shinydashboard::menuItem("Parent Portal", tabName = "parent_portal", icon = icon("users")),
        tags$li(class = "sidebar-footer-btns",
          actionButton("theme_toggle", uiOutput("theme_icon"), class = "btn-theme-toggle"),
          actionButton("logout_btn", "Logout", icon = icon("sign-out-alt"), class = "btn-logout")
        )
      )
    } else {
      shinydashboard::sidebarMenu(
        id = "tabs",
        selected = "login",
        shinydashboard::menuItem("Login", tabName = "login", icon = icon("sign-in-alt")),
        tags$li(class = "sidebar-footer-btns",
          actionButton("theme_toggle", uiOutput("theme_icon"), class = "btn-theme-toggle")
        )
      )
    }
  })

  extract_group_number <- function(group_value) {
    raw <- trimws(as.character(group_value))
    if (!nzchar(raw)) return(NA_character_)
    digits <- gsub("[^0-9]", "", raw)
    if (!nzchar(digits)) return(NA_character_)
    digits
  }

  # ── Build batch groups for selected class ────────
  observe({
    req(rv$selected_lecture)
    selected_group <- if ("Group_ID" %in% colnames(rv$selected_lecture)) {
      extract_group_number(rv$selected_lecture$Group_ID[1])
    } else {
      NA_character_
    }

    # Doctor should not manually switch groups; lock to selected session group.
    if (!is.na(selected_group)) {
      updateSelectInput(
        session,
        "batch_number",
        choices = setNames(selected_group, paste("Group", selected_group)),
        selected = selected_group
      )
      rv$batch_meta <- list(
        total_batches = as.integer(selected_group),
        total_students = NA_integer_
      )
      return()
    }

    class_id <- rv$selected_lecture$Class_ID
    res <- GET(
      paste0("http://127.0.0.1:8000/student_batches/", class_id),
      query = list(batch_size = input$batch_size)
    )
    parsed <- fromJSON(content(res, "text", encoding = "UTF-8"))
    rv$batch_meta <- parsed

    total_batches <- ifelse(is.null(parsed$total_batches), 1, parsed$total_batches)
    choices <- as.character(seq_len(total_batches))
    names(choices) <- paste("Group", choices)
    current <- as.character(input$batch_number)
    selected <- ifelse(current %in% choices, current, "1")
    updateSelectInput(session, "batch_number", choices = choices, selected = selected)
  })

  # Keep group selector aligned with selected schedule session.
  observeEvent(rv$selected_lecture, {
    req(rv$selected_lecture)
    selected_group <- if ("Group_ID" %in% colnames(rv$selected_lecture)) {
      extract_group_number(rv$selected_lecture$Group_ID[1])
    } else {
      NA_character_
    }

    if (!is.na(selected_group)) {
      updateSelectInput(
        session,
        "batch_number",
        choices = setNames(selected_group, paste("Group", selected_group)),
        selected = selected_group
      )
    }
  }, ignoreInit = TRUE)

  
  # ── Login ───────────────────────────────────────
  observeEvent(input$login_btn, {
    role <- input$portal_role
    if (identical(role, "Admin")) {
      admin_ok <- FALSE
      admins_path <- "C:/Users/Hero/classroom_system/data/admins.csv"
      if (file.exists(admins_path)) {
        admins_df <- read.csv(admins_path)
        if (all(c("Username", "Password") %in% colnames(admins_df))) {
          admin_match <- admins_df[
            admins_df$Username == input$username &
              admins_df$Password == input$password, ]
          admin_ok <- nrow(admin_match) > 0
        }
      } else {
        admin_ok <- identical(input$username, "admin") && identical(input$password, "admin123")
      }

      if (admin_ok) {
        rv$portal_role <- "admin"
        rv$doctor <- NULL
        output$login_msg <- renderText("✅ Admin login successful. Welcome to Admin Portal.")
        shinydashboard::updateTabItems(session, "tabs", "management")
      } else {
        output$login_msg <- renderText("❌ Admin login failed.")
      }
    } else if (identical(role, "Doctor")) {
      doctors_path <- "C:/Users/Hero/classroom_system/data/doctors.csv"
      df <- try(read.csv(doctors_path, stringsAsFactors = FALSE), silent = TRUE)
      if (inherits(df, "try-error")) {
        output$login_msg <- renderText("❌ Doctor login unavailable: doctors.csv is invalid. Please fix/save the file and try again.")
        return()
      }
      if (!all(c("Username", "Password", "Doctor_ID") %in% colnames(df))) {
        output$login_msg <- renderText("❌ doctors.csv must include Username, Password, Doctor_ID columns.")
        return()
      }
      username_input <- tolower(trimws(as.character(input$username)))
      password_input <- trimws(as.character(input$password))
      df_username <- tolower(trimws(as.character(df$Username)))
      df_password <- trimws(as.character(df$Password))
      match <- df[
        df_username == username_input &
          df_password == password_input, ]

      if (nrow(match) > 0) {
        rv$portal_role <- "doctor"
        rv$doctor <- match[1, ]
        output$login_msg <- renderText("✅ Doctor login successful. Welcome to Doctor Portal.")

        schedule_url <- paste0("http://127.0.0.1:8000/schedule/", as.character(match$Doctor_ID[1]))
        res <- try(GET(schedule_url), silent = TRUE)
        if (!inherits(res, "try-error") && identical(status_code(res), 200L)) {
          text <- content(res, "text", encoding = "UTF-8")
          parsed <- try(fromJSON(text), silent = TRUE)
          if (!inherits(parsed, "try-error") && !is.null(parsed) && length(parsed) > 0) {
            rv$schedule <- as.data.frame(parsed)
          } else {
            rv$schedule <- data.frame()
            output$login_msg <- renderText("✅ Doctor login successful. Schedule data is unavailable right now.")
          }
        } else {
          # Fallback: read directly from schedule.csv
          schedule_path <- "C:/Users/Hero/classroom_system/data/schedule.csv"
          if (file.exists(schedule_path)) {
            df <- try(read.csv(schedule_path, stringsAsFactors = FALSE), silent = TRUE)
            if (!inherits(df, "try-error") && nrow(df) > 0 && "Doctor_ID" %in% colnames(df)) {
              doctor_id_str <- trimws(as.character(match$Doctor_ID[1]))
              filtered <- df[trimws(as.character(df$Doctor_ID)) == doctor_id_str, , drop = FALSE]
              rv$schedule <- if (nrow(filtered) > 0) filtered else data.frame()
            } else {
              rv$schedule <- data.frame()
            }
          } else {
            rv$schedule <- data.frame()
          }
          output$login_msg <- renderText("✅ Doctor login successful. Welcome to Doctor Portal.")
        }

        shinydashboard::updateTabItems(session, "tabs", "schedule")

        print("Schedule loaded:")
        print(rv$schedule)
      } else {
        output$login_msg <- renderText("❌ Wrong doctor username or password.")
      }
    } else if (identical(role, "Student")) {
      students_df <- read.csv(students_path, stringsAsFactors = FALSE)
      students_df$Student_ID <- as.character(students_df$Student_ID)
      sid <- as.character(input$username)
      match <- students_df[students_df$Student_ID == sid, ]
      # Initial auth rule: password equals Student_ID.
      if (nrow(match) > 0 && identical(as.character(input$password), sid)) {
        rv$portal_role <- "student"
        rv$student <- match[1, ]
        rv$doctor <- NULL
        rv$parent <- NULL
        output$login_msg <- renderText("✅ Student login successful.")
        shinydashboard::updateTabItems(session, "tabs", "student_portal")
      } else {
        output$login_msg <- renderText("❌ Student login failed. Use Student ID for username and password.")
      }
    } else if (identical(role, "Parent")) {
      if (!file.exists(parents_path)) {
        output$login_msg <- renderText("❌ Parent accounts file not found. Ask admin to create parents.csv.")
        return()
      }
      parents_df <- read.csv(parents_path, stringsAsFactors = FALSE)
      if (!all(c("Username", "Password", "Student_ID") %in% colnames(parents_df))) {
        output$login_msg <- renderText("❌ parents.csv must include Username, Password, Student_ID.")
        return()
      }
      match <- parents_df[
        as.character(parents_df$Username) == as.character(input$username) &
          as.character(parents_df$Password) == as.character(input$password), ]
      if (nrow(match) > 0) {
        rv$portal_role <- "parent"
        rv$parent <- match[1, ]
        rv$doctor <- NULL
        rv$student <- NULL
        output$login_msg <- renderText("✅ Parent login successful.")
        shinydashboard::updateTabItems(session, "tabs", "parent_portal")
      } else {
        output$login_msg <- renderText("❌ Parent login failed.")
      }
    }
  })
  
  # ── Logout ──────────────────────────────────────
  observeEvent(input$logout_btn, {
    rv$portal_role      <- "guest"
    rv$doctor           <- NULL
    rv$student          <- NULL
    rv$parent           <- NULL
    rv$session_id       <- NULL
    rv$selected_lecture <- NULL
    rv$class_students   <- NULL
    rv$attendance       <- data.frame()
    rv$schedule         <- data.frame()
    rv$absent_students  <- data.frame()
    rv$emotion_live     <- data.frame()
    rv$analytics        <- NULL
    rv$batch_meta       <- NULL
    shinydashboard::updateTabItems(session, "tabs", "login")
  })

  # ── Dark / Light mode toggle ─────────────────────
  observeEvent(input$theme_toggle, {
    rv$dark_mode <- !rv$dark_mode
    if (rv$dark_mode) {
      shinyjs::runjs("document.body.setAttribute('data-theme','dark');")
    } else {
      shinyjs::runjs("document.body.setAttribute('data-theme','light');")
    }
  })

  output$theme_icon <- renderUI({
    if (isTRUE(rv$dark_mode)) {
      tags$span(icon("sun"), " Light Mode")
    } else {
      tags$span(icon("moon"), " Dark Mode")
    }
  })

  # ── Doctor info ─────────────────────────────────
  output$doctor_info <- renderUI({
    req(rv$portal_role == "doctor")
    req(rv$doctor)
    h4(paste("Welcome, Dr.", rv$doctor$Name,
             "| Subject:", rv$doctor$Subject))
  })
  
  # ── Schedule reload helper ───────────────────────
  reload_schedule <- function() {
    if (is.null(rv$doctor)) return(FALSE)
    doctor_id <- trimws(as.character(rv$doctor$Doctor_ID[1]))
    if (!nzchar(doctor_id)) return(FALSE)

    # Try backend API first
    res <- try(GET(paste0("http://127.0.0.1:8000/schedule/", doctor_id)), silent = TRUE)
    if (!inherits(res, "try-error") && status_code(res) == 200L) {
      text   <- content(res, "text", encoding = "UTF-8")
      parsed <- try(fromJSON(text), silent = TRUE)
      if (!inherits(parsed, "try-error") && !is.null(parsed) && length(parsed) > 0) {
        rv$schedule         <- as.data.frame(parsed)
        rv$schedule_version <- rv$schedule_version + 1L
        return(TRUE)
      }
    }

    # Fallback: read directly from schedule.csv
    schedule_path <- "C:/Users/Hero/classroom_system/data/schedule.csv"
    if (file.exists(schedule_path)) {
      df <- try(read.csv(schedule_path, stringsAsFactors = FALSE), silent = TRUE)
      if (!inherits(df, "try-error") && nrow(df) > 0 && "Doctor_ID" %in% colnames(df)) {
        filtered <- df[trimws(as.character(df$Doctor_ID)) == doctor_id, , drop = FALSE]
        if (nrow(filtered) > 0) {
          rv$schedule         <- filtered
          rv$schedule_version <- rv$schedule_version + 1L
          return(TRUE)
        }
      }
    }

    rv$schedule <- data.frame()
    return(FALSE)
  }

  # Refresh button
  observeEvent(input$refresh_schedule_btn, {
    ok <- reload_schedule()
    output$schedule_refresh_msg <- renderText({
      if (ok) paste("Schedule updated —", nrow(rv$schedule), "lecture(s) found.")
      else "No schedule found for this doctor yet."
    })
  })

  # Auto-reload whenever doctor navigates to the schedule tab
  observeEvent(input$tabs, {
    if (!is.null(input$tabs) && input$tabs == "schedule" &&
        rv$portal_role == "doctor" && !is.null(rv$doctor)) {
      reload_schedule()
    }
  })

  # ── Schedule table ──────────────────────────────
  output$schedule_table <- DT::renderDT({
    req(rv$portal_role == "doctor")
    req(rv$doctor)

    # React to schedule_version so table refreshes when admin adds entries
    rv$schedule_version

    doctor_id <- as.character(rv$doctor$Doctor_ID[1])

    # Try backend API first
    loaded <- FALSE
    res <- try(GET(paste0("http://127.0.0.1:8000/schedule/", doctor_id)), silent = TRUE)
    if (!inherits(res, "try-error") && status_code(res) == 200L) {
      text   <- content(res, "text", encoding = "UTF-8")
      parsed <- try(fromJSON(text), silent = TRUE)
      if (!inherits(parsed, "try-error") && !is.null(parsed) && length(parsed) > 0) {
        rv$schedule <- as.data.frame(parsed)
        loaded <- TRUE
      }
    }

    # Fallback: read directly from schedule.csv
    if (!loaded) {
      schedule_path <- "C:/Users/Hero/classroom_system/data/schedule.csv"
      if (file.exists(schedule_path)) {
        df <- try(read.csv(schedule_path, stringsAsFactors = FALSE), silent = TRUE)
        if (!inherits(df, "try-error") && nrow(df) > 0 && "Doctor_ID" %in% colnames(df)) {
          filtered <- df[trimws(as.character(df$Doctor_ID)) == trimws(doctor_id), , drop = FALSE]
          if (nrow(filtered) > 0) {
            rv$schedule <- filtered
            loaded <- TRUE
          }
        }
      }
    }

    if (!loaded || is.null(rv$schedule) || nrow(rv$schedule) == 0) {
      return(DT::datatable(
        data.frame(Message = "No schedule found for this doctor yet."),
        options = list(dom = "t")
      ))
    }
    DT::datatable(rv$schedule,
                  selection = "single",
                  options   = list(pageLength = 10))
  })
  
  # ── Lecture selector button ──────────────────────
  output$lecture_selector <- renderUI({
    req(rv$portal_role == "doctor")
    req(nrow(rv$schedule) > 0)
    actionButton("go_session", "▶ Open Selected Lecture",
                 class = "btn-info btn-lg")
  })
  
  # ── Store selected lecture ───────────────────────
  observeEvent(input$schedule_table_rows_selected, {
    selected <- input$schedule_table_rows_selected
    if (!is.null(selected) && length(selected) > 0) {
      rv$selected_lecture <- rv$schedule[selected, ]
      print("Selected lecture:")
      print(rv$selected_lecture)
    }
  })
  
  # ── Go to session tab ────────────────────────────
  observeEvent(input$go_session, {
    req(rv$portal_role == "doctor")
    req(rv$selected_lecture)
    shinydashboard::updateTabItems(session, "tabs", "session")
  })
  
  # ── Selected lecture info ───────────────────────
  output$selected_lecture_info <- renderUI({
    req(rv$portal_role == "doctor")
    if (is.null(rv$selected_lecture)) {
      tagList(
        p("⚠️ No lecture selected."),
        p("Go to Schedule tab and click a row first.")
      )
    } else {
      row <- rv$selected_lecture
      tagList(
        h4(paste("📖 Lecture:", row$Lecture_ID)),
        p(paste("📅 Day:",     row$Day)),
        p(paste("🕐 Time:",    row$Time_Slot)),
        p(paste("🚪 Room:",    row$Room)),
        p(paste("🧩 Group ID:", ifelse("Group_ID" %in% colnames(row), row$Group_ID, "G1"))),
        p(paste("⏱ Duration (days):", ifelse("Duration_Days" %in% colnames(row), row$Duration_Days, 1)))
      )
    }
  })
  
  # ── Lecture details boxes ────────────────────────
  output$lecture_details <- renderUI({
    req(rv$portal_role == "doctor")
    req(rv$selected_lecture)
    row <- rv$selected_lecture
    tagList(
      shinydashboard::infoBox("Lecture", row$Lecture_ID, icon = icon("book"),      color = "blue",   width = 6),
      shinydashboard::infoBox("Day",     row$Day,        icon = icon("calendar"),  color = "green",  width = 6),
      shinydashboard::infoBox("Time",    row$Time_Slot,  icon = icon("clock"),     color = "yellow", width = 6),
      shinydashboard::infoBox("Room",    row$Room,       icon = icon("door-open"), color = "red",    width = 6),
      shinydashboard::infoBox("Group ID", ifelse("Group_ID" %in% colnames(row), row$Group_ID, "G1"), icon = icon("users"), color = "purple", width = 6),
      shinydashboard::infoBox("Duration Days", ifelse("Duration_Days" %in% colnames(row), row$Duration_Days, 1), icon = icon("hourglass-half"), color = "teal", width = 6)
    )
  })

  to_photo_embed_url <- function(link) {
    if (is.null(link) || is.na(link) || !nzchar(link)) return("")
    if (grepl("drive.google.com", link, fixed = TRUE)) {
      id <- ""
      if (grepl("id=", link, fixed = TRUE)) {
        id <- sub(".*id=([^&]+).*", "\\1", link)
      }
      if (grepl("/d/", link, fixed = TRUE)) {
        id <- sub(".*?/d/([^/]+).*", "\\1", link)
      }
      if (nzchar(id)) {
        # Thumbnail endpoint is more stable for <img> embedding in tables.
        return(paste0("https://drive.google.com/thumbnail?id=", id, "&sz=w1000"))
      }
    }
    link
  }

  get_current_batch_students <- function() {
    req(rv$selected_lecture)
    class_id <- rv$selected_lecture$Class_ID
    res <- GET(
      paste0("http://127.0.0.1:8000/students/", class_id),
      query = list(
        batch_number = input$batch_number,
        batch_size = input$batch_size
      )
    )
    parsed <- fromJSON(content(res, "text", encoding = "UTF-8"))
    if (!is.null(parsed$records) && length(parsed$records) > 0) {
      as.data.frame(parsed$records, stringsAsFactors = FALSE)
    } else {
      data.frame(Student_ID = character(0), Student_Name = character(0), Photo_Link = character(0))
    }
  }
  
  # ── Students in this class ───────────────────────
  output$students_table <- DT::renderDT({
    req(rv$portal_role == "doctor")
    students <- get_current_batch_students()
    rv$class_students <- students
    if (nrow(students) == 0) {
      return(DT::datatable(data.frame(Message = "No students found for this group."), options = list(dom = "t")))
    }
    view <- students
    if ("Photo_Link" %in% colnames(view)) {
      view$Photo <- vapply(
        view$Photo_Link,
        function(x) {
          url <- to_photo_embed_url(x)
          if (!nzchar(url)) return("No photo")
          as.character(tags$img(src = url, height = "54", style = "border-radius:8px;border:1px solid rgba(255,255,255,0.15);"))
        },
        character(1)
      )
      view$Photo_Link <- NULL
    } else {
      view$Photo <- "No photo"
    }
    DT::datatable(
      view[, c("Student_ID", "Student_Name", "Photo")],
      escape = FALSE,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })

  output$manual_attendance_table <- DT::renderDT({
    req(rv$portal_role == "doctor")
    students <- get_current_batch_students()
    rv$class_students <- students
    if (nrow(students) == 0) {
      return(DT::datatable(data.frame(Message = "No students found for this group."), options = list(dom = "t")))
    }
    view <- students
    if ("Photo_Link" %in% colnames(view)) {
      view$Photo <- vapply(
        view$Photo_Link,
        function(x) {
          url <- to_photo_embed_url(x)
          if (!nzchar(url)) return("No photo")
          as.character(tags$img(src = url, height = "54", style = "border-radius:8px;border:1px solid rgba(255,255,255,0.15);"))
        },
        character(1)
      )
      view$Photo_Link <- NULL
    } else {
      view$Photo <- "No photo"
    }
    DT::datatable(
      view[, c("Student_ID", "Student_Name", "Photo")],
      selection = "multiple",
      escape = FALSE,
      rownames = FALSE,
      options = list(pageLength = 8)
    )
  })

  output$batch_table <- DT::renderDT({
    req(rv$portal_role == "doctor")
    req(rv$batch_meta)
    batches <- rv$batch_meta$batches
    if (is.null(batches) || length(batches) == 0) {
      return(DT::datatable(data.frame(Message = "No groups available."), options = list(dom = "t")))
    }
    df <- as.data.frame(batches)
    view <- df[, c("Batch_Number", "Batch_Label", "From_Index", "To_Index", "Students_Count")]
    DT::datatable(view, options = list(pageLength = 10), rownames = FALSE)
  })

  output$batch_info <- renderText({
    req(rv$portal_role == "doctor")
    req(rv$batch_meta)
    if (is.null(rv$batch_meta$total_batches)) {
      return("Batch information unavailable.")
    }
    if (!is.null(rv$selected_lecture) && "Group_ID" %in% colnames(rv$selected_lecture)) {
      return(paste("Target group locked to", as.character(rv$selected_lecture$Group_ID[1])))
    }
    paste(
      "Group", input$batch_number, "of", rv$batch_meta$total_batches,
      "| Group size target:", input$batch_size,
      "| Total class students:", rv$batch_meta$total_students
    )
  })
  
  # ── Start session ───────────────────────────────
observeEvent(input$start_btn, {
  req(rv$doctor)
  req(rv$selected_lecture)

  res  <- POST("http://127.0.0.1:8000/start_session",
               query = list(
                 lecture_id   = rv$selected_lecture$Lecture_ID,
                 doctor_id    = rv$doctor$Doctor_ID,
                 batch_number = input$batch_number,
                 batch_size   = input$batch_size,
                 group_id     = ifelse("Group_ID" %in% colnames(rv$selected_lecture),
                                       rv$selected_lecture$Group_ID, ""),
                 week_number  = input$week_number   # ← ADD THIS
               ))
  data          <- fromJSON(content(res, "text", encoding = "UTF-8"))
  rv$session_id <- data$session_id

  output$session_status <- renderText(
    paste0("✅ Session started | Week ", data$week_number,
           " of ", data$total_weeks,
           " | ", rv$session_id)
  )
  print(paste("Session started:", rv$session_id))
})

  # ── Management: refresh tables ───────────────────
  refresh_mgmt <- function() {
    d_res <- try(GET("http://127.0.0.1:8000/doctors"), silent = TRUE)
    if (!inherits(d_res, "try-error") && identical(status_code(d_res), 200L)) {
      parsed_doctors <- try(fromJSON(content(d_res, "text", encoding = "UTF-8")), silent = TRUE)
      if (!inherits(parsed_doctors, "try-error") && !is.null(parsed_doctors)) {
        rv$mgmt_doctors <- as.data.frame(parsed_doctors)
      } else {
        rv$mgmt_doctors <- data.frame()
      }
    } else {
      rv$mgmt_doctors <- data.frame()
    }

    c_res <- try(GET("http://127.0.0.1:8000/courses"), silent = TRUE)
    if (!inherits(c_res, "try-error") && identical(status_code(c_res), 200L)) {
      parsed_courses <- try(fromJSON(content(c_res, "text", encoding = "UTF-8")), silent = TRUE)
      if (!inherits(parsed_courses, "try-error") && !is.null(parsed_courses) && length(parsed_courses) > 0) {
        rv$mgmt_courses <- as.data.frame(parsed_courses)
      } else {
        rv$mgmt_courses <- data.frame()
      }
    } else {
      rv$mgmt_courses <- data.frame()
    }
  }

  safe_admin_post <- function(url, query_args) {
    tryCatch(
      {
        res <- POST(url, query = query_args)
        if (status_code(res) == 200L) {
          return(list(ok = TRUE, response = res, error = NULL))
        }
        body_text <- try(content(res, "text", encoding = "UTF-8"), silent = TRUE)
        if (inherits(body_text, "try-error") || is.null(body_text)) {
          body_text <- ""
        }
        detail <- ""
        if (nzchar(body_text)) {
          parsed_body <- try(fromJSON(body_text), silent = TRUE)
          if (!inherits(parsed_body, "try-error") && !is.null(parsed_body$detail)) {
            detail <- as.character(parsed_body$detail)
          } else {
            detail <- body_text
          }
        }
        list(
          ok = FALSE,
          response = res,
          error = paste0("HTTP ", status_code(res), if (nzchar(detail)) paste0(" - ", detail) else "")
        )
      },
      error = function(e) {
        list(ok = FALSE, response = NULL, error = conditionMessage(e))
      }
    )
  }

  observe({
    req(rv$portal_role == "admin")
    refresh_mgmt()
  })

  observeEvent(input$m_add_lecturer_btn, {
    req(rv$portal_role == "admin")
    res <- safe_admin_post(
      "http://127.0.0.1:8000/add_lecturer",
      list(
        doctor_id = input$m_doctor_id,
        name = input$m_doctor_name,
        username = input$m_doctor_username,
        password = input$m_doctor_password,
        subject = input$m_doctor_subject,
        class_id = input$m_doctor_class
      )
    )
    if (res$ok) {
      output$m_lecturer_msg <- renderText("✅ Lecturer added successfully.")
      refresh_mgmt()
      shinydashboard::updateTabItems(session, "tabs", "management")
    } else {
      err_msg <- if (!is.null(res$error)) paste(":", res$error) else ""
      output$m_lecturer_msg <- renderText(paste0("❌ Failed to add lecturer", err_msg))
    }
  })

  observeEvent(input$m_add_course_btn, {
    req(rv$portal_role == "admin")
    res <- safe_admin_post(
      "http://127.0.0.1:8000/add_course",
      list(
        course_id = input$m_course_id,
        course_name = input$m_course_name,
        class_id = input$m_course_class,
        duration_days = input$m_course_duration
      )
    )
    if (res$ok) {
      output$m_course_msg <- renderText("✅ Course added successfully.")
      refresh_mgmt()
      shinydashboard::updateTabItems(session, "tabs", "management")
    } else {
      err_msg <- if (!is.null(res$error)) paste(":", res$error) else ""
      output$m_course_msg <- renderText(paste0("❌ Failed to add course", err_msg))
    }
  })

  observeEvent(input$m_add_schedule_btn, {
    req(rv$portal_role == "admin")
    total_weeks_val <- as.integer(input$m_s_total_weeks)
    if (is.na(total_weeks_val) || total_weeks_val < 1) total_weeks_val <- 15L

    res <- safe_admin_post(
      "http://127.0.0.1:8000/add_schedule_entry",
      list(
        doctor_id     = input$m_s_doctor_id,
        day           = input$m_s_day,
        time_slot     = input$m_s_time,
        lecture_id    = input$m_s_lecture_id,
        class_id      = input$m_s_class_id,
        room          = input$m_s_room,
        group_id      = input$m_s_group_id,
        duration_days = input$m_s_duration,
        course_id     = input$m_s_course_id,
        total_weeks   = total_weeks_val
      )
    )
    if (res$ok) {
      # Also patch Total_Weeks directly in schedule.csv since the API doesn't support it
      schedule_path <- "C:/Users/Hero/classroom_system/data/schedule.csv"
      tryCatch({
        df <- read.csv(schedule_path, stringsAsFactors = FALSE)
        lid <- trimws(input$m_s_lecture_id)
        did <- trimws(input$m_s_doctor_id)
        idx <- which(trimws(as.character(df$Lecture_ID)) == lid &
                     trimws(as.character(df$Doctor_ID))  == did)
        if (length(idx) > 0) {
          df$Total_Weeks[idx] <- total_weeks_val
          write.csv(df, schedule_path, row.names = FALSE)
        }
      }, error = function(e) {})

      output$m_schedule_msg <- renderText(
        paste0("Schedule entry added — ", total_weeks_val, " weeks.")
      )
      rv$schedule_version <- rv$schedule_version + 1L
      refresh_mgmt()
      shinydashboard::updateTabItems(session, "tabs", "management")
    } else {
      err_msg <- if (!is.null(res$error)) paste(":", res$error) else ""
      output$m_schedule_msg <- renderText(paste0("Failed to add schedule entry", err_msg))
    }
  })

  output$m_doctors_table <- DT::renderDT({
    req(rv$portal_role == "admin")
    if (nrow(rv$mgmt_doctors) == 0) {
      return(DT::datatable(data.frame(Message = "No lecturers yet."), options = list(dom = "t")))
    }
    DT::datatable(rv$mgmt_doctors, options = list(pageLength = 10))
  })

  output$m_courses_table <- DT::renderDT({
    req(rv$portal_role == "admin")
    if (nrow(rv$mgmt_courses) == 0) {
      return(DT::datatable(data.frame(Message = "No courses yet."), options = list(dom = "t")))
    }
    DT::datatable(rv$mgmt_courses, options = list(pageLength = 10))
  })
  
  # ── Start camera ────────────────────────────────
  observeEvent(input$camera_btn, {
    req(rv$portal_role == "doctor")
    req(rv$session_id)
    res  <- POST(paste0("http://127.0.0.1:8000/start_camera/", rv$session_id))
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    output$camera_status <- renderText("📷 Camera started! Recognizing faces...")
    print("Camera started")
  })

  observeEvent(input$mark_manual_attendance_btn, {
    req(rv$portal_role == "doctor")
    req(rv$session_id)
    req(nrow(rv$class_students) > 0)
    selected_rows <- input$manual_attendance_table_rows_selected
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      output$manual_attendance_msg <- renderText("Select at least one student from manual attendance table.")
      return()
    }

    selected_students <- rv$class_students[selected_rows, ]
    marked <- 0
    for (i in seq_len(nrow(selected_students))) {
      sid <- as.character(selected_students$Student_ID[i])
      sname <- as.character(selected_students$Student_Name[i])
      res <- try(
        POST(
          paste0("http://127.0.0.1:8000/mark_attendance_manual/", rv$session_id),
          query = list(student_id = sid, student_name = sname)
        ),
        silent = TRUE
      )
      if (!inherits(res, "try-error") && status_code(res) == 200) {
        marked <- marked + 1
      }
    }
    output$manual_attendance_msg <- renderText(
      paste0("✅ Manually marked ", marked, " student(s) as present.")
    )

    att_res <- try(GET(paste0("http://127.0.0.1:8000/attendance/", rv$session_id)), silent = TRUE)
    if (!inherits(att_res, "try-error")) {
      att_parsed <- fromJSON(content(att_res, "text", encoding = "UTF-8"))
      if (!is.null(att_parsed$records) && length(att_parsed$records) > 0) {
        rv$attendance <- as.data.frame(att_parsed$records)
      }
    }
  })
  
  # ── Poll attendance every 5 seconds ─────────────
  observe({
    req(rv$portal_role == "doctor")
    invalidateLater(5000, session)
    req(rv$session_id)
    res2 <- try(GET(paste0("http://127.0.0.1:8000/attendance/",
                           rv$session_id)), silent = TRUE)
    if (!inherits(res2, "try-error")) {
      parsed <- fromJSON(content(res2, "text", encoding = "UTF-8"))
      if (length(parsed$records) > 0) {
        rv$attendance <- as.data.frame(parsed$records)
      }
    }

    analytics_res <- try(GET(paste0("http://127.0.0.1:8000/analytics/", rv$session_id)), silent = TRUE)
    if (!inherits(analytics_res, "try-error")) {
      rv$analytics <- fromJSON(content(analytics_res, "text", encoding = "UTF-8"))
    }

    absent_res <- try(GET(paste0("http://127.0.0.1:8000/absent_students/", rv$session_id)), silent = TRUE)
    if (!inherits(absent_res, "try-error")) {
      absent_parsed <- fromJSON(content(absent_res, "text", encoding = "UTF-8"))
      if (!is.null(absent_parsed$records) && length(absent_parsed$records) > 0) {
        rv$absent_students <- as.data.frame(absent_parsed$records)
      } else {
        rv$absent_students <- data.frame()
      }
    }

    emotion_res <- try(GET(paste0("http://127.0.0.1:8000/emotion_status/", rv$session_id)), silent = TRUE)
    if (!inherits(emotion_res, "try-error")) {
      emotion_parsed <- fromJSON(content(emotion_res, "text", encoding = "UTF-8"))
      if (!is.null(emotion_parsed$records) && length(emotion_parsed$records) > 0) {
        rv$emotion_live <- as.data.frame(emotion_parsed$records)
      } else {
        rv$emotion_live <- data.frame()
      }
    }
  })
  
  # ── End session ─────────────────────────────────
  observeEvent(input$end_btn, {
    req(rv$portal_role == "doctor")
    req(rv$session_id)
    POST(paste0("http://127.0.0.1:8000/end_session/", rv$session_id))
    output$session_status <- renderText("Session ended. Data saved.")
    output$camera_status  <- renderText("")
    rv$session_id         <- NULL
    rv$attendance         <- data.frame()
    rv$absent_students    <- data.frame()
    rv$emotion_live       <- data.frame()
    rv$analytics          <- NULL
    # Increment version counter so all analytics outputs refresh immediately
    rv$analytics_version  <- rv$analytics_version + 1L
  })

  output$emotion_detector_status <- renderText({
    req(rv$portal_role == "doctor")
    if (is.null(rv$session_id)) {
      return("Start a session first, then open this page to monitor emotions.")
    }
    paste("Active session:", rv$session_id, "| Students with emotion records:", nrow(rv$emotion_live))
  })

  output$emotion_live_table <- DT::renderDT({
    req(rv$portal_role == "doctor")
    if (is.null(rv$session_id)) {
      return(DT::datatable(data.frame(Message = "No active session."), options = list(dom = "t")))
    }
    if (nrow(rv$emotion_live) == 0) {
      return(DT::datatable(data.frame(Message = "No emotion records yet. Start camera and wait a few seconds."), options = list(dom = "t")))
    }
    DT::datatable(rv$emotion_live, options = list(pageLength = 10), rownames = FALSE)
  })

  output$emotion_live_plot <- renderPlot({
    req(rv$portal_role == "doctor")
    req(!is.null(rv$session_id))
    req(nrow(rv$emotion_live) > 0)
    req(all(c("happy", "neutral", "focused", "confused", "sad", "Student_Name") %in% colnames(rv$emotion_live)))

    live_df <- rv$emotion_live %>%
      dplyr::select(Student_Name, happy, neutral, focused, confused, sad) %>%
      tidyr::pivot_longer(
        cols = c(happy, neutral, focused, confused, sad),
        names_to = "Emotion",
        values_to = "Count"
      )

    ggplot(live_df, aes(x = Student_Name, y = Count, fill = Emotion)) +
      geom_col(position = "stack") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
      scale_fill_manual(values = c(
        happy   = "#3ec97a",
        neutral = "#4a90d9",
        focused = "#a78bfa",
        confused= "#ffb432",
        sad     = "#e05555"
      )) +
      labs(
        title = "Live Emotion Counts Per Student",
        x = "Student",
        y = "Count"
      )
  })

  output$student_profile_ui <- renderUI({
    req(rv$portal_role == "student")
    req(rv$student)
    student <- rv$student
    photo_url <- if ("Photo_Link" %in% colnames(student)) to_photo_embed_url(student$Photo_Link) else ""
    tagList(
      fluidRow(
        column(
          width = 2,
          if (nzchar(photo_url)) {
            tags$img(src = photo_url, height = "100", style = "border-radius:10px;border:1px solid rgba(255,255,255,0.15);")
          } else {
            tags$div("No photo", style = "height:100px;display:flex;align-items:center;color:#7a9bb5;")
          }
        ),
        column(
          width = 10,
          h4(paste("Name:", as.character(student$Student_Name))),
          p(paste("Student ID:", as.character(student$Student_ID))),
          p(paste("Class:", as.character(student$Class_ID)))
        )
      )
    )
  })

  normalize_attendance_df <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    names(df) <- trimws(names(df))

    # Recover from malformed header like "Status231006417"
    if (!("Status" %in% names(df))) {
      status_like <- names(df)[grepl("^Status", names(df), ignore.case = FALSE)]
      if (length(status_like) > 0) {
        names(df)[names(df) == status_like[1]] <- "Status"
      }
    }
    df
  }

  # ── Helper: enrich attendance with Doctor Name + Course Name ─────────────
  enrich_attendance <- function(df) {
    sessions_path <- "C:/Users/Hero/classroom_system/data/sessions.csv"
    schedule_path <- "C:/Users/Hero/classroom_system/data/schedule.csv"
    doctors_path  <- "C:/Users/Hero/classroom_system/data/doctors.csv"
    courses_path  <- "C:/Users/Hero/classroom_system/data/courses.csv"

    tryCatch({
      # Load lookup tables
      sessions_df <- if (file.exists(sessions_path))
        read.csv(sessions_path, stringsAsFactors = FALSE, header = TRUE) else NULL
      schedule_df <- if (file.exists(schedule_path))
        read.csv(schedule_path, stringsAsFactors = FALSE) else NULL
      doctors_df  <- if (file.exists(doctors_path))
        read.csv(doctors_path,  stringsAsFactors = FALSE) else NULL
      courses_df  <- if (file.exists(courses_path))
        read.csv(courses_path,  stringsAsFactors = FALSE) else NULL

      # Build Session_ID → Lecture_ID + Doctor_ID mapping from sessions.csv
      if (!is.null(sessions_df) && "Session_ID" %in% colnames(sessions_df)) {
        # sessions.csv has variable columns — grab only what we need safely
        sess_cols <- intersect(c("Session_ID","Lecture_ID","Doctor_ID","Class_ID","Week_Number"), colnames(sessions_df))
        sess_map  <- unique(sessions_df[, sess_cols, drop = FALSE])
        df <- merge(df, sess_map, by = "Session_ID", all.x = TRUE)
      }

      # Build Lecture_ID → Course_ID mapping from schedule.csv
      if (!is.null(schedule_df) && all(c("Lecture_ID","Course_ID") %in% colnames(schedule_df))) {
        sched_map <- unique(schedule_df[, c("Lecture_ID","Course_ID"), drop = FALSE])
        if ("Lecture_ID" %in% colnames(df)) {
          df <- merge(df, sched_map, by = "Lecture_ID", all.x = TRUE)
        }
      }

      # Build Doctor_ID → Doctor Name from doctors.csv
      if (!is.null(doctors_df) && all(c("Doctor_ID","Name") %in% colnames(doctors_df))) {
        doc_map <- unique(doctors_df[, c("Doctor_ID","Name"), drop = FALSE])
        names(doc_map)[names(doc_map) == "Name"] <- "Doctor_Name"
        if ("Doctor_ID" %in% colnames(df)) {
          df <- merge(df, doc_map, by = "Doctor_ID", all.x = TRUE)
        }
      }

      # Build Course_ID → Course Name from courses.csv
      # Fall back to using Class_ID as the course name if courses.csv doesn't match
      if (!is.null(courses_df) && all(c("Course_ID","Course_Name") %in% colnames(courses_df))) {
        crs_map <- unique(courses_df[, c("Course_ID","Course_Name"), drop = FALSE])
        if ("Course_ID" %in% colnames(df)) {
          df <- merge(df, crs_map, by = "Course_ID", all.x = TRUE)
        }
      }

      # If Course_Name is still missing, use Class_ID as a readable fallback
      if (!"Course_Name" %in% colnames(df)) {
        df$Course_Name <- NA_character_
      }
      if ("Class_ID" %in% colnames(df)) {
        df$Course_Name <- ifelse(
          is.na(df$Course_Name) | !nzchar(trimws(as.character(df$Course_Name))),
          as.character(df$Class_ID),
          df$Course_Name
        )
      }
    }, error = function(e) {})  # silently skip enrichment on any error

    df
  }

  output$student_attendance_table <- DT::renderDT({
    req(rv$portal_role == "student")
    req(rv$student)
    sid <- as.character(rv$student$Student_ID)
    if (!file.exists(attendance_path)) {
      return(DT::datatable(data.frame(Message = "No attendance records yet."), options = list(dom = "t")))
    }
    df <- try(read.csv(attendance_path, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(df, "try-error")) {
      return(DT::datatable(data.frame(Message = "Attendance file is corrupted. Please contact admin."), options = list(dom = "t")))
    }
    df <- normalize_attendance_df(df)
    if (!("Student_ID" %in% colnames(df))) {
      return(DT::datatable(data.frame(Message = "Attendance file format is invalid."), options = list(dom = "t")))
    }
    view <- df[as.character(df$Student_ID) == sid, , drop = FALSE]
    if (nrow(view) == 0) {
      return(DT::datatable(data.frame(Message = "No attendance records found."), options = list(dom = "t")))
    }
    # Enrich with Doctor Name + Course Name
    view <- enrich_attendance(view)
    # Select display columns — put Doctor_Name and Course_Name first after Session_ID
    display_cols <- intersect(
      c("Session_ID", "Week_Number", "Doctor_Name", "Course_Name", "Time_In", "Status"),
      colnames(view)
    )
    DT::datatable(view[, display_cols, drop = FALSE],
                  options = list(pageLength = 10), rownames = FALSE)
  })

  output$student_analytics_table <- DT::renderDT({
    req(rv$portal_role == "student")
    req(rv$student)
    sid <- as.character(rv$student$Student_ID)
    if (!file.exists(analytics_path)) {
      return(DT::datatable(data.frame(Message = "No analytics records yet."), options = list(dom = "t")))
    }
    df <- read.csv(analytics_path, stringsAsFactors = FALSE)
    if (!("Student_ID" %in% colnames(df))) {
      return(DT::datatable(data.frame(Message = "Analytics file format is invalid."), options = list(dom = "t")))
    }

    # Fill missing emotion columns
    for (col in c("happy", "neutral", "focused", "confused", "sad")) {
      if (!col %in% colnames(df)) df[[col]] <- 0
      df[[col]] <- suppressWarnings(as.integer(df[[col]]))
      df[[col]][is.na(df[[col]])] <- 0
    }

    # Recompute Dominant_Emotion from actual counts
    emotion_cols <- c("happy", "neutral", "focused", "confused", "sad")
    df$Dominant_Emotion <- apply(df[, emotion_cols], 1, function(row) {
      if (all(row == 0)) return("neutral")
      emotion_cols[which.max(row)]
    })

    keep_cols <- intersect(
      c("Session_ID", "Dominant_Emotion", "Engagement_Score",
        "happy", "neutral", "focused", "confused", "sad"),
      colnames(df)
    )
    view <- df[as.character(df$Student_ID) == sid, keep_cols, drop = FALSE]
    if (nrow(view) == 0) {
      return(DT::datatable(data.frame(Message = "No analytics records found."), options = list(dom = "t")))
    }
    DT::datatable(view, options = list(pageLength = 10), rownames = FALSE)
  })

  output$parent_linked_student_ui <- renderUI({
    req(rv$portal_role == "parent")
    req(rv$parent)
    students_df <- read.csv(students_path, stringsAsFactors = FALSE)
    sid <- as.character(rv$parent$Student_ID)
    match <- students_df[as.character(students_df$Student_ID) == sid, ]
    if (nrow(match) == 0) {
      return(tags$p("Linked student was not found in student dataset."))
    }
    student <- match[1, ]
    photo_url <- to_photo_embed_url(student$Photo_Link)
    tagList(
      fluidRow(
        column(
          width = 2,
          if (nzchar(photo_url)) {
            tags$img(src = photo_url, height = "100", style = "border-radius:10px;border:1px solid rgba(255,255,255,0.15);")
          } else {
            tags$div("No photo", style = "height:100px;display:flex;align-items:center;color:#7a9bb5;")
          }
        ),
        column(
          width = 10,
          h4(paste("Student:", as.character(student$Student_Name))),
          p(paste("Student ID:", as.character(student$Student_ID))),
          p(paste("Class:", as.character(student$Class_ID)))
        )
      )
    )
  })

  output$parent_attendance_table <- DT::renderDT({
    req(rv$portal_role == "parent")
    req(rv$parent)
    sid <- as.character(rv$parent$Student_ID)
    if (!file.exists(attendance_path)) {
      return(DT::datatable(data.frame(Message = "No attendance records yet."), options = list(dom = "t")))
    }
    df <- try(read.csv(attendance_path, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(df, "try-error")) {
      return(DT::datatable(data.frame(Message = "Attendance file is corrupted. Please contact admin."), options = list(dom = "t")))
    }
    df <- normalize_attendance_df(df)
    if (!("Student_ID" %in% colnames(df))) {
      return(DT::datatable(data.frame(Message = "Attendance file format is invalid."), options = list(dom = "t")))
    }
    view <- df[as.character(df$Student_ID) == sid, , drop = FALSE]
    if (nrow(view) == 0) {
      return(DT::datatable(data.frame(Message = "No attendance records found for linked student."), options = list(dom = "t")))
    }
    # Enrich with Doctor Name + Course Name
    view <- enrich_attendance(view)
    display_cols <- intersect(
      c("Session_ID", "Week_Number", "Doctor_Name", "Course_Name", "Time_In", "Status"),
      colnames(view)
    )
    DT::datatable(view[, display_cols, drop = FALSE],
                  options = list(pageLength = 10), rownames = FALSE)
  })

  output$parent_analytics_table <- DT::renderDT({
    req(rv$portal_role == "parent")
    req(rv$parent)
    sid <- as.character(rv$parent$Student_ID)
    if (!file.exists(analytics_path)) {
      return(DT::datatable(data.frame(Message = "No analytics records yet."), options = list(dom = "t")))
    }
    df <- read.csv(analytics_path, stringsAsFactors = FALSE)

    # Fill missing emotion columns
    for (col in c("happy", "neutral", "focused", "confused", "sad")) {
      if (!col %in% colnames(df)) df[[col]] <- 0
      df[[col]] <- suppressWarnings(as.integer(df[[col]]))
      df[[col]][is.na(df[[col]])] <- 0
    }

    # Recompute Dominant_Emotion from actual counts
    emotion_cols <- c("happy", "neutral", "focused", "confused", "sad")
    df$Dominant_Emotion <- apply(df[, emotion_cols], 1, function(row) {
      if (all(row == 0)) return("neutral")
      emotion_cols[which.max(row)]
    })

    keep_cols <- intersect(
      c("Session_ID", "Dominant_Emotion", "Engagement_Score",
        "happy", "neutral", "focused", "confused", "sad"),
      colnames(df)
    )
    view <- df[as.character(df$Student_ID) == sid, keep_cols, drop = FALSE]
    if (nrow(view) == 0) {
      return(DT::datatable(data.frame(Message = "No analytics records found for linked student."), options = list(dom = "t")))
    }
    DT::datatable(view, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ── Attendance count ─────────────────────────────
  output$attendance_count <- renderText({
    req(rv$portal_role == "doctor")
    paste("✅ Present:", nrow(rv$attendance), "students")
  })
  
  # ── Attendance table ─────────────────────────────
  output$attendance_table <- DT::renderDT({
    req(rv$portal_role == "doctor")
    req(nrow(rv$attendance) > 0)
    DT::datatable(rv$attendance,
                  options = list(pageLength = 15))
  })

  output$absent_count <- renderText({
    req(rv$portal_role == "doctor")
    if (is.null(rv$session_id)) {
      return("Start a session to view absentees.")
    }
    paste("Missing:", nrow(rv$absent_students), "students")
  })

  output$absent_table <- DT::renderDT({
    req(rv$portal_role == "doctor")
    if (nrow(rv$absent_students) == 0) {
      return(DT::datatable(data.frame(Message = "No absent students detected yet."), options = list(dom = "t")))
    }
    DT::datatable(rv$absent_students, options = list(pageLength = 10))
  })
  
  # ── Analytics plot ───────────────────────────────
  output$attendance_plot <- renderPlot({
    req(rv$portal_role == "doctor")
    path <- "C:/Users/Hero/classroom_system/data/attendance.csv"
    req(file.exists(path))
    df <- read.csv(path)
    df %>%
      group_by(Session_ID) %>%
      summarise(Count = n()) %>%
      ggplot(aes(x = Session_ID, y = Count, fill = Session_ID)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Attendance Per Session",
           x = "Session", y = "Students Present")
  })
  
  # ── Analytics table ──────────────────────────────
  # ── Helper: load analytics CSV ───────────────────
  get_doctor_session_ids <- function() {
    if (is.null(rv$doctor) || !("Doctor_ID" %in% colnames(rv$doctor))) return(character(0))
    sessions_path <- "C:/Users/Hero/classroom_system/data/sessions.csv"
    if (!file.exists(sessions_path)) return(character(0))
    sessions_df <- try(read.csv(sessions_path, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(sessions_df, "try-error") || nrow(sessions_df) == 0) return(character(0))
    if (!all(c("Doctor_ID", "Session_ID") %in% colnames(sessions_df))) return(character(0))
    doctor_id <- trimws(as.character(rv$doctor$Doctor_ID[1]))
    if (!nzchar(doctor_id)) return(character(0))
    as.character(unique(sessions_df$Session_ID[trimws(as.character(sessions_df$Doctor_ID)) == doctor_id]))
  }

  load_analytics <- function() {
    # React to analytics_version so outputs refresh when a session ends
    rv$analytics_version
    path <- "C:/Users/Hero/classroom_system/data/session_analytics.csv"
    if (!file.exists(path)) return(NULL)
    df <- try(read.csv(path, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(df, "try-error")) return(NULL)
    if (nrow(df) == 0) return(NULL)
    # Doctor analytics view must include only sessions owned by logged-in doctor.
    if (rv$portal_role == "doctor") {
      session_ids <- get_doctor_session_ids()
      if (length(session_ids) == 0 || !("Session_ID" %in% colnames(df))) return(NULL)
      df <- df[as.character(df$Session_ID) %in% session_ids, , drop = FALSE]
      if (nrow(df) == 0) return(NULL)
    }
    return(df)
  }
  
  load_attendance <- function() {
    path <- "C:/Users/Hero/classroom_system/data/attendance.csv"
    if (!file.exists(path)) return(NULL)
    df <- try(read.csv(path, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(df, "try-error")) return(NULL)
    df <- normalize_attendance_df(df)
    if (nrow(df) == 0) return(NULL)
    if (rv$portal_role == "doctor") {
      session_ids <- get_doctor_session_ids()
      if (length(session_ids) == 0 || !("Session_ID" %in% colnames(df))) return(NULL)
      df <- df[as.character(df$Session_ID) %in% session_ids, , drop = FALSE]
      if (nrow(df) == 0) return(NULL)
    }
    return(df)
  }
  
  # ── Info boxes ───────────────────────────────────
  output$info_total_sessions <- shinydashboard::renderInfoBox({
    df <- load_attendance()
    count <- if (is.null(df)) 0 else length(unique(df$Session_ID))
    shinydashboard::infoBox(
      "Total Sessions", count,
      icon  = icon("calendar-check"),
      color = "blue"
    )
  })
  
  output$info_avg_attendance <- shinydashboard::renderInfoBox({
    df <- load_attendance()
    val <- if (is.null(df)) "N/A" else {
      avg <- df %>%
        group_by(Session_ID) %>%
        summarise(n = n()) %>%
        summarise(avg = mean(n)) %>%
        pull(avg)
      paste0(round(avg, 1), " students")
    }
    shinydashboard::infoBox(
      "Avg Attendance", val,
      icon  = icon("users"),
      color = "green"
    )
  })
  
  output$info_avg_engagement <- shinydashboard::renderInfoBox({
    df <- load_analytics()
    val <- if (is.null(df) || !"Engagement_Score" %in% colnames(df)) "N/A" else {
      paste0(round(mean(df$Engagement_Score, na.rm = TRUE) * 100, 1), "%")
    }
    shinydashboard::infoBox(
      "Avg Engagement", val,
      icon  = icon("brain"),
      color = "yellow"
    )
  })
  
  output$info_top_emotion <- shinydashboard::renderInfoBox({
    df <- load_analytics()
    val <- if (is.null(df) || !"Dominant_Emotion" %in% colnames(df)) "N/A" else {
      names(sort(table(df$Dominant_Emotion), decreasing = TRUE))[1]
    }
    shinydashboard::infoBox(
      "Top Emotion", val,
      icon  = icon("smile"),
      color = "orange"
    )
  })
  
  # ── Attendance bar chart ─────────────────────────
  output$attendance_plot <- renderPlot({
    df <- load_attendance()
    req(!is.null(df))
    df %>%
      group_by(Session_ID) %>%
      summarise(Count = n()) %>%
      ggplot(aes(x = reorder(Session_ID, -Count), y = Count, fill = Session_ID)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = Count), vjust = -0.5, size = 3.5) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
        panel.grid.major.x = element_blank()
      ) +
      labs(title = "Students Present Per Session",
           x = "Session ID", y = "Count") +
      scale_fill_brewer(palette = "Set2")
  })
  
  # ── Emotion pie chart ────────────────────────────
  output$emotion_pie_plot <- renderPlot({
    df <- load_analytics()
    req(!is.null(df))

    # Ensure all emotion columns exist
    for (col in c("happy", "neutral", "focused", "confused", "sad")) {
      if (!col %in% colnames(df)) df[[col]] <- 0
      df[[col]] <- suppressWarnings(as.integer(df[[col]]))
      df[[col]][is.na(df[[col]])] <- 0
    }

    # Sum total occurrences of each emotion across all sessions
    # This shows ALL emotions, not just the dominant one per row
    emotion_totals <- data.frame(
      Emotion = c("happy", "neutral", "focused", "confused", "sad"),
      Count   = c(
        sum(df$happy,   na.rm = TRUE),
        sum(df$neutral, na.rm = TRUE),
        sum(df$focused, na.rm = TRUE),
        sum(df$confused,na.rm = TRUE),
        sum(df$sad,     na.rm = TRUE)
      )
    )

    # Only keep emotions that actually occurred
    emotion_totals <- emotion_totals[emotion_totals$Count > 0, ]
    req(nrow(emotion_totals) > 0)

    total <- sum(emotion_totals$Count)
    emotion_totals$Pct   <- round(emotion_totals$Count / total * 100, 1)
    emotion_totals$Label <- paste0(emotion_totals$Emotion, "\n", emotion_totals$Pct, "%")

    ggplot(emotion_totals, aes(x = "", y = Count, fill = Emotion)) +
      geom_col(width = 1, color = "white", linewidth = 0.5) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = Label),
                position = position_stack(vjust = 0.5),
                size = 3.8, color = "white", fontface = "bold") +
      theme_void() +
      theme(
        legend.position  = "bottom",
        legend.text      = element_text(size = 11),
        legend.title     = element_text(size = 12, face = "bold"),
        plot.background  = element_rect(fill = "transparent", color = NA)
      ) +
      scale_fill_manual(
        values = c(
          happy   = "#3ec97a",
          neutral = "#4a90d9",
          focused = "#a78bfa",
          confused= "#ffb432",
          sad     = "#e05555"
        ),
        drop = FALSE
      ) +
      labs(fill = "Emotion",
           title = paste0("Total emotion frames: ", format(total, big.mark = ",")))
  })
  
  # ── Emotion trend line chart ─────────────────────
  output$emotion_trend_plot <- renderPlot({
    df <- load_analytics()
    req(!is.null(df))
    req(all(c("Session_ID", "happy", "neutral") %in% colnames(df)))
    
    trend_df <- df %>%
      group_by(Session_ID) %>%
      summarise(
        Happy   = sum(happy,                          na.rm = TRUE),
        Neutral = sum(neutral,                        na.rm = TRUE),
        Focused = sum(if ("focused"  %in% names(.)) focused  else 0, na.rm = TRUE),
        Confused= sum(if ("confused" %in% names(.)) confused else 0, na.rm = TRUE),
        Sad     = sum(if ("sad"      %in% names(.)) sad      else 0, na.rm = TRUE)
      ) %>%
      tidyr::pivot_longer(
        cols      = c(Happy, Neutral, Focused, Confused, Sad),
        names_to  = "Emotion",
        values_to = "Count"
      )
    
    ggplot(trend_df, aes(x = Session_ID, y = Count,
                         color = Emotion, group = Emotion)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
      scale_color_manual(values = c(
        Happy   = "#3ec97a",
        Neutral = "#4a90d9",
        Focused = "#a78bfa",
        Confused= "#ffb432",
        Sad     = "#e05555"
      )) +
      labs(title = "Emotion Count Trend Across Sessions",
           x = "Session", y = "Count", color = "Emotion")
  })
  
  # ── Engagement bar chart ─────────────────────────
  output$engagement_bar_plot <- renderPlot({
    df <- load_analytics()
    req(!is.null(df))
    req(all(c("Student_Name", "Engagement_Score") %in% colnames(df)))
    
    eng_df <- df %>%
      group_by(Student_Name) %>%
      summarise(Avg_Engagement = mean(Engagement_Score, na.rm = TRUE)) %>%
      arrange(desc(Avg_Engagement)) %>%
      head(20)
    
    ggplot(eng_df, aes(x = reorder(Student_Name, Avg_Engagement),
                       y = Avg_Engagement,
                       fill = Avg_Engagement)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = round(Avg_Engagement, 2)),
                hjust = -0.2, size = 3.5) +
      coord_flip() +
      scale_fill_gradient(low = "#e05555", high = "#3ec97a") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank()) +
      labs(title = "Average Engagement Score Per Student",
           x = "Student", y = "Engagement Score (0-1)") +
      ylim(0, 1.1)
  })
  
  # ── Analytics table ──────────────────────────────
  output$analytics_table <- DT::renderDT({
    df <- load_analytics()
    req(!is.null(df))

    # Ensure numeric emotion columns exist (fill 0 if missing from old data)
    for (col in c("happy", "neutral", "focused", "confused", "sad")) {
      if (!col %in% colnames(df)) df[[col]] <- 0
      df[[col]] <- suppressWarnings(as.integer(df[[col]]))
      df[[col]][is.na(df[[col]])] <- 0
    }

    # Recompute Dominant_Emotion from actual counts — fixes corrupted old rows
    emotion_cols <- c("happy", "neutral", "focused", "confused", "sad")
    df$Dominant_Emotion <- apply(df[, emotion_cols], 1, function(row) {
      if (all(row == 0)) return("neutral")
      emotion_cols[which.max(row)]
    })

    # Recompute Engagement_Score properly
    if ("Engagement_Score" %in% colnames(df)) {
      df$Engagement_Score <- suppressWarnings(as.numeric(df$Engagement_Score))
      df$Engagement_Score[is.na(df$Engagement_Score)] <- 0
    }

    # Build clean summary per student
    view <- df %>%
      group_by(Student_Name) %>%
      summarise(
        Sessions_Tracked = n(),
        Avg_Engagement   = round(mean(Engagement_Score, na.rm = TRUE), 2),
        Happy   = sum(happy,   na.rm = TRUE),
        Neutral = sum(neutral, na.rm = TRUE),
        Focused = sum(focused, na.rm = TRUE),
        Confused= sum(confused,na.rm = TRUE),
        Sad     = sum(sad,     na.rm = TRUE)
      ) %>%
      arrange(desc(Avg_Engagement))

    # Compute Top_Emotion after summarise (safer than inline block)
    view$Top_Emotion <- apply(
      view[, c("Happy","Neutral","Focused","Confused","Sad")], 1,
      function(row) {
        labels <- c("happy","neutral","focused","confused","sad")
        if (all(row == 0)) return("neutral")
        labels[which.max(row)]
      }
    )

    # Reorder columns so Top_Emotion appears early
    view <- view[, c("Student_Name","Sessions_Tracked","Avg_Engagement",
                     "Top_Emotion","Happy","Neutral","Focused","Confused","Sad")]

    DT::datatable(
      view,
      options  = list(pageLength = 10),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Top_Emotion",
        backgroundColor = DT::styleEqual(
          c("happy","neutral","focused","confused","sad"),
          c("#1a4731","#1a2e4a","#2e1a4a","#4a3a1a","#4a1a1a")
        ),
        color = "white"
      )
  })
  # ── Snapshots gallery ────────────────────────────

  # Populate session dropdown with this doctor's sessions
  observe({
    req(rv$portal_role == "doctor")
    session_ids <- get_doctor_session_ids()
    if (length(session_ids) == 0) {
      updateSelectInput(session, "snapshot_session_select",
                        choices = c("-- No sessions yet --" = ""))
    } else {
      choices <- c("-- Select a session --" = "", setNames(session_ids, session_ids))
      updateSelectInput(session, "snapshot_session_select", choices = choices)
    }
  })

  # Load snapshots when button clicked
  snapshot_data <- eventReactive(input$snapshot_load_btn, {
    sid <- input$snapshot_session_select
    if (!nzchar(sid)) return(list(count = 0, files = list()))

    res <- try(
      GET(paste0("http://127.0.0.1:8000/snapshots/", sid)),
      silent = TRUE
    )
    if (inherits(res, "try-error") || status_code(res) != 200) {
      return(list(count = 0, files = list()))
    }
    parsed <- fromJSON(content(res, "text", encoding = "UTF-8"))
    list(
      count = as.integer(parsed$count),
      files = if (is.null(parsed$snapshots) || length(parsed$snapshots) == 0)
                list()
              else
                as.data.frame(parsed$snapshots)
    )
  }, ignoreNULL = FALSE)

  output$snapshot_count_text <- renderText({
    data <- snapshot_data()
    if (data$count == 0) "No snapshots found for this session."
    else paste0(data$count, " snapshot(s) found.")
  })

  output$snapshots_gallery <- renderUI({
    data <- snapshot_data()
    if (data$count == 0 || length(data$files) == 0) {
      return(div(
        style = "text-align:center; padding:40px; color:#7a9bb5;",
        icon("camera"), " No snapshots yet. Start a session and the camera will save snapshots automatically."
      ))
    }

    files_df <- data$files
    base_url <- "http://127.0.0.1:8000"
    sid      <- input$snapshot_session_select

    # Build image cards — 4 per row
    cards <- lapply(seq_len(nrow(files_df)), function(i) {
      fname <- as.character(files_df$filename[i])
      img_url <- paste0(base_url, "/snapshot_file/", sid, "/", fname)
      # Extract label from filename (student_id_emotion_timestamp)
      label <- gsub("_202[0-9].*", "", fname)   # strip timestamp
      label <- gsub("_", " ", label)

      div(
        style = paste0(
          "display:inline-block; width:23%; margin:1%; vertical-align:top;",
          "background:var(--bg-panel); border:1px solid var(--border);",
          "border-radius:10px; overflow:hidden; text-align:center;"
        ),
        tags$img(
          src   = img_url,
          style = "width:100%; height:160px; object-fit:cover;",
          onerror = "this.src=''; this.style.display='none';"
        ),
        div(
          style = "padding:6px 8px; font-size:11px; color:var(--text-muted); word-break:break-all;",
          label
        )
      )
    })

    div(
      style = "max-height:520px; overflow-y:auto; padding:8px;",
      tagList(cards)
    )
  })

} # ── end server ──────────────────────────────────
