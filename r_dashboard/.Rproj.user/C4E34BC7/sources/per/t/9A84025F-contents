server <- function(input, output, session) {
  
  library(httr)
  library(jsonlite)
  library(dplyr)
  
  # ── Reactive values ─────────────────────────────
  rv <- reactiveValues(
    doctor           = NULL,
    portal_role      = "guest",
    session_id       = NULL,
    selected_lecture = NULL,
    class_students   = NULL,
    attendance       = data.frame(),
    schedule         = data.frame(),
    absent_students  = data.frame(),
    analytics        = NULL,
    batch_meta       = NULL,
    mgmt_doctors     = data.frame(),
    mgmt_courses     = data.frame()
  )

  output$sidebar_menu <- shinydashboard::renderMenu({
    if (rv$portal_role == "admin") {
      shinydashboard::sidebarMenu(
        id = "tabs",
        selected = "management",
        shinydashboard::menuItem("Login", tabName = "login", icon = icon("sign-in-alt")),
        shinydashboard::menuItem("Management", tabName = "management", icon = icon("user-cog"))
      )
    } else if (rv$portal_role == "doctor") {
      shinydashboard::sidebarMenu(
        id = "tabs",
        selected = "schedule",
        shinydashboard::menuItem("Login", tabName = "login", icon = icon("sign-in-alt")),
        shinydashboard::menuItem("Schedule", tabName = "schedule", icon = icon("calendar")),
        shinydashboard::menuItem("Session", tabName = "session", icon = icon("video")),
        shinydashboard::menuItem("Analytics", tabName = "analytics", icon = icon("chart-bar"))
      )
    } else {
      shinydashboard::sidebarMenu(
        id = "tabs",
        selected = "login",
        shinydashboard::menuItem("Login", tabName = "login", icon = icon("sign-in-alt"))
      )
    }
  })
  # ── Build batch groups for selected class ────────
  observe({
    req(rv$selected_lecture)
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
    } else {
      df    <- read.csv("C:/Users/Hero/classroom_system/data/doctors.csv")
      match <- df[df$Username == input$username &
                    df$Password == input$password, ]
      
      if (nrow(match) > 0) {
        rv$portal_role <- "doctor"
        rv$doctor <- match[1, ]
        output$login_msg <- renderText("✅ Doctor login successful. Welcome to Doctor Portal.")
        
        res         <- GET(paste0("http://127.0.0.1:8000/schedule/", match$Doctor_ID[1]))
        text        <- content(res, "text", encoding = "UTF-8")
        parsed      <- fromJSON(text)
        rv$schedule <- as.data.frame(parsed)
        
        shinydashboard::updateTabItems(session, "tabs", "schedule")
        
        print("Schedule loaded:")
        print(rv$schedule)
      } else {
        output$login_msg <- renderText("❌ Wrong doctor username or password.")
      }
    }
  })
  
  # ── Doctor info ─────────────────────────────────
  output$doctor_info <- renderUI({
    req(rv$portal_role == "doctor")
    req(rv$doctor)
    h4(paste("Welcome, Dr.", rv$doctor$Name,
             "| Subject:", rv$doctor$Subject))
  })
  
  # ── Schedule table ──────────────────────────────
  output$schedule_table <- DT::renderDT({
    req(rv$portal_role == "doctor")
    req(rv$schedule)
    req(nrow(rv$schedule) > 0)
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
  
  # ── Students in this class ───────────────────────
  output$students_table <- DT::renderDT({
    req(rv$portal_role == "doctor")
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
    students <- if (!is.null(parsed$records) && length(parsed$records) > 0) {
      as.data.frame(parsed$records)
    } else {
      data.frame(Student_ID = character(0), Student_Name = character(0))
    }
    rv$class_students <- students
    DT::datatable(students,
                  options  = list(pageLength = 10),
                  colnames = c("Student ID", "Student Name"))
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
    paste(
      "Group", input$batch_number, "of", rv$batch_meta$total_batches,
      "| Group size target:", input$batch_size,
      "| Total class students:", rv$batch_meta$total_students
    )
  })
  
  # ── Start session ───────────────────────────────
  observeEvent(input$start_btn, {
    req(rv$portal_role == "doctor")
    req(rv$doctor)
    req(rv$selected_lecture)
    
    res  <- POST("http://127.0.0.1:8000/start_session",
                 query = list(
                   lecture_id = rv$selected_lecture$Lecture_ID,
                   doctor_id  = rv$doctor$Doctor_ID,
                   batch_number = input$batch_number,
                   batch_size = input$batch_size,
                   group_id = ifelse("Group_ID" %in% colnames(rv$selected_lecture), rv$selected_lecture$Group_ID, "")
                 ))
    data          <- fromJSON(content(res, "text", encoding = "UTF-8"))
    rv$session_id <- data$session_id
    
    output$session_status <- renderText(
      paste(
        "✅ Session started:", rv$session_id,
        "| Batch", data$batch_number, "of", data$total_batches,
        "| Size", data$batch_size
      )
    )
    print(paste("Session started:", rv$session_id))
  })

  # ── Management: refresh tables ───────────────────
  refresh_mgmt <- function() {
    d_res <- try(GET("http://127.0.0.1:8000/doctors"), silent = TRUE)
    if (!inherits(d_res, "try-error")) {
      parsed <- fromJSON(content(d_res, "text", encoding = "UTF-8"))
      rv$mgmt_doctors <- as.data.frame(parsed)
    }
    c_res <- try(GET("http://127.0.0.1:8000/courses"), silent = TRUE)
    if (!inherits(c_res, "try-error")) {
      parsed <- fromJSON(content(c_res, "text", encoding = "UTF-8"))
      if (!is.null(parsed) && length(parsed) > 0) {
        rv$mgmt_courses <- as.data.frame(parsed)
      } else {
        rv$mgmt_courses <- data.frame()
      }
    }
  }

  observe({
    req(rv$portal_role == "admin")
    refresh_mgmt()
  })

  observeEvent(input$m_add_lecturer_btn, {
    req(rv$portal_role == "admin")
    res <- POST("http://127.0.0.1:8000/add_lecturer",
                query = list(
                  doctor_id = input$m_doctor_id,
                  name = input$m_doctor_name,
                  username = input$m_doctor_username,
                  password = input$m_doctor_password,
                  subject = input$m_doctor_subject,
                  class_id = input$m_doctor_class
                ))
    if (status_code(res) == 200) {
      output$m_lecturer_msg <- renderText("✅ Lecturer added successfully.")
      refresh_mgmt()
    } else {
      output$m_lecturer_msg <- renderText("❌ Failed to add lecturer.")
    }
  })

  observeEvent(input$m_add_course_btn, {
    req(rv$portal_role == "admin")
    res <- POST("http://127.0.0.1:8000/add_course",
                query = list(
                  course_id = input$m_course_id,
                  course_name = input$m_course_name,
                  class_id = input$m_course_class,
                  duration_days = input$m_course_duration
                ))
    if (status_code(res) == 200) {
      output$m_course_msg <- renderText("✅ Course added successfully.")
      refresh_mgmt()
    } else {
      output$m_course_msg <- renderText("❌ Failed to add course.")
    }
  })

  observeEvent(input$m_add_schedule_btn, {
    req(rv$portal_role == "admin")
    res <- POST("http://127.0.0.1:8000/add_schedule_entry",
                query = list(
                  doctor_id = input$m_s_doctor_id,
                  day = input$m_s_day,
                  time_slot = input$m_s_time,
                  lecture_id = input$m_s_lecture_id,
                  class_id = input$m_s_class_id,
                  room = input$m_s_room,
                  group_id = input$m_s_group_id,
                  duration_days = input$m_s_duration,
                  course_id = input$m_s_course_id
                ))
    if (status_code(res) == 200) {
      output$m_schedule_msg <- renderText("✅ Schedule entry added successfully.")
      if (!is.null(rv$doctor)) {
        res_s <- GET(paste0("http://127.0.0.1:8000/schedule/", rv$doctor$Doctor_ID))
        rv$schedule <- as.data.frame(fromJSON(content(res_s, "text", encoding = "UTF-8")))
      }
    } else {
      output$m_schedule_msg <- renderText("❌ Failed to add schedule entry.")
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
  })
  
  # ── End session ─────────────────────────────────
  observeEvent(input$end_btn, {
    req(rv$portal_role == "doctor")
    req(rv$session_id)
    POST(paste0("http://127.0.0.1:8000/end_session/", rv$session_id))
    output$session_status <- renderText("⏹ Session ended. Data saved.")
    output$camera_status  <- renderText("")
    rv$session_id         <- NULL
    rv$attendance         <- data.frame()
    rv$absent_students    <- data.frame()
    rv$analytics          <- NULL
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
  output$analytics_table <- DT::renderDT({
    req(rv$portal_role == "doctor")
    req(!is.null(rv$analytics))
    students <- rv$analytics$students
    if (is.null(students) || length(students) == 0) {
      return(DT::datatable(data.frame(Message = "No analytics yet. Start camera and detect students."), options = list(dom = "t")))
    }
    df <- as.data.frame(students)
    view <- df[, c("Student_Name", "Engagement_Score", "Dominant_Emotion", "happy", "bored", "confused", "neutral")]
    DT::datatable(view, options = list(pageLength = 10))
  })

  output$emotion_plot <- renderPlot({
    req(rv$portal_role == "doctor")
    req(!is.null(rv$analytics))
    dist <- rv$analytics$emotion_distribution
    if (is.null(dist) || length(dist) == 0) {
      return(NULL)
    }
    names_vec <- names(dist)
    values_vec <- as.numeric(unlist(dist))
    emo_df <- data.frame(Emotion = names_vec, Count = values_vec)
    ggplot(emo_df, aes(x = Emotion, y = Count, fill = Emotion)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Class Emotion Distribution", x = "Emotion", y = "Count")
  })

  output$engagement_cluster_plot <- renderPlot({
    req(rv$portal_role == "doctor")
    req(!is.null(rv$analytics))
    students <- rv$analytics$students
    if (is.null(students) || length(students) < 3) {
      return(NULL)
    }
    df <- as.data.frame(students)
    if (!"Engagement_Score" %in% colnames(df)) {
      return(NULL)
    }
    scores <- as.numeric(df$Engagement_Score)
    km <- kmeans(scores, centers = 3)
    cluster_df <- data.frame(
      Student = as.character(df$Student_Name),
      Engagement = scores,
      Cluster = as.factor(km$cluster)
    )
    ggplot(cluster_df, aes(x = Student, y = Engagement, color = Cluster)) +
      geom_point(size = 3) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Student Engagement Clusters", x = "Student", y = "Engagement Score")
  })

  output$behavior_plot <- renderPlot({
    req(rv$portal_role == "doctor")
    req(!is.null(rv$analytics))
    behavior <- rv$analytics$behavior_summary
    if (is.null(behavior) || length(behavior) == 0) {
      return(NULL)
    }
    behavior_df <- data.frame(
      Behavior = c("Phone Usage", "Suspicious Activity"),
      Count = c(
        as.numeric(ifelse(is.null(behavior$phone_usage), 0, behavior$phone_usage)),
        as.numeric(ifelse(is.null(behavior$suspicious_activity), 0, behavior$suspicious_activity))
      )
    )
    ggplot(behavior_df, aes(x = Behavior, y = Count, fill = Behavior)) +
      geom_col() +
      theme_minimal() +
      labs(title = "YOLOv8 Behavior Events", x = "Behavior Type", y = "Event Count")
  })
  
} # ── end server ──────────────────────────────────