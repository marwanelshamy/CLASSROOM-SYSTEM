ui <- shinydashboard::dashboardPage(
  skin = "black",
  shinydashboard::dashboardHeader(
    title = tags$span(
      tags$img(src = "logo.png", height = "28px", style = "margin-right:8px;"),
      "Smart Classroom Pro"
    ),
    titleWidth = 260
  ),
  shinydashboard::dashboardSidebar(
    width = 260,
    tags$head(
      shinyjs::useShinyjs(),
      tags$script(HTML("
        // Apply saved theme on load
        document.addEventListener('DOMContentLoaded', function() {
          var saved = localStorage.getItem('scTheme') || 'dark';
          document.body.setAttribute('data-theme', saved);
        });
        // Persist theme changes
        var themeObserver = new MutationObserver(function(mutations) {
          mutations.forEach(function(m) {
            if (m.attributeName === 'data-theme') {
              localStorage.setItem('scTheme', document.body.getAttribute('data-theme'));
            }
          });
        });
        themeObserver.observe(document.body, { attributes: true });
      ")),
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500&family=Space+Grotesk:wght@500;600;700&display=swap');

        /* ── CSS variables: dark (default) ── */
        :root, body[data-theme='dark'] {
          --bg-page:    #0f1923;
          --bg-panel:   #111d2b;
          --bg-input:   #0f1923;
          --border:     rgba(255,255,255,0.06);
          --text-main:  #c8ddf0;
          --text-head:  #e8edf2;
          --text-muted: #7a9bb5;
          --text-dim:   #3d5570;
          --accent:     #4a90d9;
          --success:    #3ec97a;
          --danger:     #e05555;
          --row-hover:  rgba(74,144,217,0.06);
        }

        /* ── CSS variables: light ── */
        body[data-theme='light'] {
          --bg-page:    #f0f4f8;
          --bg-panel:   #ffffff;
          --bg-input:   #f8fafc;
          --border:     rgba(0,0,0,0.08);
          --text-main:  #1a2e42;
          --text-head:  #0d1f30;
          --text-muted: #4a6580;
          --text-dim:   #8aa0b5;
          --accent:     #1a6fc4;
          --success:    #1e8a4a;
          --danger:     #c0392b;
          --row-hover:  rgba(26,111,196,0.06);
        }

        /* ── Apply variables ── */
        body, .content-wrapper, .main-sidebar, .left-side {
          font-family: 'DM Sans', sans-serif !important;
          background-color: var(--bg-page) !important;
          color: var(--text-main) !important;
          transition: background-color 0.3s, color 0.3s;
        }
        .main-sidebar { background: var(--bg-panel) !important; border-right: 1px solid var(--border) !important; }
        .main-sidebar .sidebar { background: var(--bg-panel) !important; }
        .sidebar-menu > li > a { color: var(--text-muted) !important; font-size: 13.5px !important; padding: 10px 16px !important; margin: 2px 8px !important; border-radius: 8px !important; transition: all 0.2s !important; }
        .sidebar-menu > li > a:hover { background: rgba(74,144,217,0.1) !important; color: var(--text-main) !important; }
        .sidebar-menu > li.active > a { background: rgba(74,144,217,0.15) !important; color: var(--accent) !important; border-left: 3px solid var(--accent) !important; }
        .sidebar-menu .header { color: var(--text-dim) !important; font-size: 10px !important; letter-spacing: 1px !important; }
        .main-header .logo { background: var(--bg-panel) !important; border-bottom: 1px solid var(--border) !important; font-family: 'Space Grotesk', sans-serif !important; font-weight: 700 !important; font-size: 16px !important; color: var(--text-head) !important; }
        .main-header .navbar { background: var(--bg-panel) !important; border-bottom: 1px solid var(--border) !important; }
        .main-header .navbar .nav > li > a { color: var(--text-muted) !important; }
        .content-wrapper { background: var(--bg-page) !important; }
        .content { padding: 28px 32px !important; }
        .box { background: var(--bg-panel) !important; border: 1px solid var(--border) !important; border-top: none !important; border-radius: 12px !important; box-shadow: none !important; color: var(--text-main) !important; }
        .box-header { background: var(--bg-panel) !important; color: var(--text-main) !important; border-bottom: 1px solid var(--border) !important; border-radius: 12px 12px 0 0 !important; font-family: 'Space Grotesk', sans-serif !important; font-size: 14px !important; font-weight: 600 !important; padding: 14px 18px !important; }
        .box-body { padding: 18px !important; }
        .info-box { background: var(--bg-panel) !important; border: 1px solid var(--border) !important; border-radius: 10px !important; box-shadow: none !important; }
        .info-box-content { color: var(--text-main) !important; }
        .info-box-number { color: var(--text-head) !important; font-family: 'Space Grotesk', sans-serif !important; font-size: 24px !important; }
        .info-box-text { color: var(--text-muted) !important; font-size: 12px !important; }
        .btn-primary, .btn-success, .btn-danger, .btn-info, .btn-warning { border-radius: 8px !important; font-weight: 600 !important; font-size: 14px !important; padding: 10px 20px !important; border: none !important; transition: opacity 0.2s !important; }
        .btn-primary  { background: linear-gradient(135deg,#1a6fc4,#4a90d9) !important; color:#fff !important; }
        .btn-success  { background: linear-gradient(135deg,#1e8a4a,#3ec97a) !important; color:#fff !important; }
        .btn-danger   { background: linear-gradient(135deg,#a32d2d,#e05555) !important; color:#fff !important; }
        .btn-info     { background: rgba(74,144,217,0.15) !important; color: var(--accent) !important; border: 1px solid rgba(74,144,217,0.3) !important; }
        .btn:hover    { opacity: 0.85 !important; }
        .btn-lg       { width: 100% !important; margin-bottom: 10px !important; }
        .form-control { background: var(--bg-input) !important; border: 1px solid var(--border) !important; border-radius: 8px !important; color: var(--text-main) !important; font-size: 14px !important; }
        .form-control:focus { border-color: var(--accent) !important; box-shadow: 0 0 0 3px rgba(74,144,217,0.15) !important; outline: none !important; }
        label { color: var(--text-muted) !important; font-size: 12px !important; font-weight: 500 !important; letter-spacing: 0.3px !important; }
        .dataTables_wrapper { color: var(--text-main) !important; }
        table.dataTable thead th { background: var(--bg-input) !important; color: var(--text-muted) !important; font-size: 11px !important; font-weight: 600 !important; letter-spacing: 0.5px !important; text-transform: uppercase !important; border-bottom: 1px solid var(--border) !important; }
        table.dataTable tbody tr { background: var(--bg-panel) !important; color: var(--text-main) !important; }
        table.dataTable tbody tr:hover { background: var(--row-hover) !important; }
        table.dataTable tbody td { border-bottom: 1px solid var(--border) !important; font-size: 13px !important; }
        .dataTables_filter input, .dataTables_length select { background: var(--bg-input) !important; border: 1px solid var(--border) !important; color: var(--text-main) !important; border-radius: 6px !important; }
        .dataTables_info, .dataTables_paginate { color: var(--text-dim) !important; font-size: 12px !important; }
        .paginate_button { color: var(--text-muted) !important; border-radius: 6px !important; }
        .paginate_button.current { background: rgba(74,144,217,0.2) !important; color: var(--accent) !important; border: none !important; }
        .shiny-plot-output { border-radius: 8px !important; overflow: hidden !important; }
        .live-badge { display: inline-flex; align-items: center; gap: 6px; background: rgba(62,201,122,0.1); color: #3ec97a; padding: 4px 12px; border-radius: 20px; font-size: 12px; border: 1px solid rgba(62,201,122,0.2); }
        .live-dot { width: 7px; height: 7px; border-radius: 50%; background: #3ec97a; animation: pulse 1.5s infinite; }
        @keyframes pulse { 0%,100% { opacity:1; } 50% { opacity:0.3; } }
        .pro-hero { background: linear-gradient(135deg, var(--bg-panel), var(--bg-page)); border: 1px solid rgba(74,144,217,0.2); border-radius: 14px; padding: 24px; margin-bottom: 20px; }
        .pro-hero h3 { font-family: 'Space Grotesk',sans-serif; font-size: 20px; font-weight: 700; color: var(--text-head); margin-top: 0; margin-bottom: 6px; }
        .pro-hero p { color: var(--text-muted); font-size: 13px; margin: 0; }
        .small-note { color: var(--text-dim); font-size: 11px; margin-top: 8px; line-height: 1.5; }
        h4 { font-family: 'Space Grotesk', sans-serif !important; font-size: 13px !important; font-weight: 600 !important; color: var(--text-muted) !important; text-transform: uppercase !important; letter-spacing: 0.5px !important; margin: 16px 0 10px !important; }
        .section-divider { border: none; border-top: 1px solid var(--border); margin: 16px 0; }

        /* ── Sidebar footer buttons (theme + logout) ── */
        .sidebar-footer-btns { padding: 12px 16px 8px !important; margin-top: 8px !important; border-top: 1px solid var(--border) !important; display: flex !important; flex-direction: column !important; gap: 6px !important; }
        .btn-theme-toggle { width: 100% !important; background: rgba(74,144,217,0.1) !important; color: var(--text-muted) !important; border: 1px solid var(--border) !important; border-radius: 8px !important; font-size: 12px !important; padding: 7px 12px !important; text-align: left !important; transition: all 0.2s !important; }
        .btn-theme-toggle:hover { background: rgba(74,144,217,0.2) !important; color: var(--accent) !important; }
        .btn-logout { width: 100% !important; background: rgba(224,85,85,0.1) !important; color: #e05555 !important; border: 1px solid rgba(224,85,85,0.2) !important; border-radius: 8px !important; font-size: 12px !important; padding: 7px 12px !important; text-align: left !important; transition: all 0.2s !important; }
        .btn-logout:hover { background: rgba(224,85,85,0.2) !important; }
      "))
    ),
    shinydashboard::sidebarMenuOutput("sidebar_menu")
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "login",
        fluidRow(
          column(width = 4, offset = 4,
            br(), br(),
            div(class = "pro-hero", style = "text-align:center;",
              h3("Smart Classroom Portal"),
              p("AI-powered attendance & emotion analytics system")
            ),
            shinydashboard::box(
              width = 12, status = "primary",
              title = "Sign In",
              selectInput("portal_role", "Login as", choices = c("Doctor", "Student", "Parent", "Admin"), selected = "Doctor"),
              textInput("username", "Username", placeholder = "Enter your username"),
              passwordInput("password", "Password", placeholder = "Enter your password"),
              br(),
              actionButton("login_btn", "Sign In", class = "btn-primary btn-lg", icon = icon("sign-in-alt")),
              br(), br(),
              textOutput("login_msg"),
              div(class = "small-note", "Doctor portal: attendance, sessions & analytics.", br(), "Student portal: personal attendance and engagement.", br(), "Parent portal: monitor linked student progress.", br(), "Admin portal: manage lecturers, courses & schedules.")
            )
          )
        )
      ),
      shinydashboard::tabItem(tabName = "schedule",
        fluidRow(
          shinydashboard::box(
            width = 12, status = "info",
            title = tags$span(icon("calendar-alt"), " Weekly Schedule"),
            uiOutput("doctor_info"),
            hr(class = "section-divider"),
            fluidRow(
              column(width = 2,
                actionButton("refresh_schedule_btn", "Refresh Schedule",
                             icon = icon("sync"), class = "btn-info")
              ),
              column(width = 10,
                textOutput("schedule_refresh_msg")
              )
            ),
            br(),
            DT::DTOutput("schedule_table"),
            br(),
            uiOutput("lecture_selector")
          )
        )
      ),
      shinydashboard::tabItem(tabName = "session",
        fluidRow(
          shinydashboard::box(
            width = 4, status = "success",
            title = tags$span(icon("video"), " Session Control"),
            uiOutput("selected_lecture_info"),
            hr(class = "section-divider"),
            numericInput("batch_size", "Students per group", value = 25, min = 1, step = 1),
            selectInput("batch_number", "Select group", choices = c("1" = 1), selected = 1),
            selectInput("week_number", "Select Week", choices = setNames(1:15, paste("Week", 1:15)), selected = 1),
            hr(class = "section-divider"),
            textOutput("batch_info"),
            hr(class = "section-divider"),
            actionButton("start_btn", "Start Session", class = "btn-success btn-lg", icon = icon("play")),
            actionButton("camera_btn", "Start Camera", class = "btn-info btn-lg", icon = icon("camera")),
            actionButton("end_btn", "End Session", class = "btn-danger btn-lg", icon = icon("stop")),
            hr(class = "section-divider"),
            textOutput("session_status"),
            textOutput("camera_status"),
            div(class = "small-note", icon("info-circle"), " Start camera 5-10 seconds after starting session.")
          ),
          shinydashboard::box(
            width = 8, status = "info",
            title = tags$span(icon("users"), " Lecture Details & Attendance"),
            uiOutput("lecture_details"),
            hr(class = "section-divider"),
            h4(icon("layer-group"), " Student Groups"),
            DT::DTOutput("batch_table"),
            h4(icon("list"), " Class Roster"),
            DT::DTOutput("students_table"),
            h4(icon("check-circle"), " Live Attendance"),
            uiOutput("live_badge"),
            textOutput("attendance_count"),
            br(),
            DT::DTOutput("attendance_table"),
            h4(icon("times-circle"), " Absent Students"),
            textOutput("absent_count"),
            DT::DTOutput("absent_table"),
            h4(icon("user-check"), " Manual Attendance"),
            div(class = "small-note", "If camera is off, select students below and mark them present manually."),
            DT::DTOutput("manual_attendance_table"),
            actionButton("mark_manual_attendance_btn", "Mark Selected As Present", class = "btn-success", icon = icon("check")),
            br(), br(),
            textOutput("manual_attendance_msg")
          )
        )
      ),
      shinydashboard::tabItem(tabName = "emotion_detector",
        fluidRow(
          column(width = 12,
            div(class = "pro-hero",
              h3("Live Emotion Detector"),
              p("Real-time emotion summary separated from attendance view.")
            )
          ),
          shinydashboard::box(
            width = 12, status = "warning",
            title = tags$span(icon("smile"), " Current Session Emotion Status"),
            textOutput("emotion_detector_status"),
            br(),
            DT::DTOutput("emotion_live_table")
          ),
          shinydashboard::box(
            width = 12, status = "info",
            title = tags$span(icon("chart-bar"), " Live Emotion Distribution"),
            plotOutput("emotion_live_plot", height = "320px")
          )
        )
      ),
      shinydashboard::tabItem(tabName = "analytics",
        fluidRow(
          column(width = 12,
            div(class = "pro-hero",
              h3("Presentation-Ready Analytics"),
              p("Emotion trends, engagement scores, and attendance patterns.")
            )
          ),
          shinydashboard::infoBoxOutput("info_total_sessions", width = 3),
          shinydashboard::infoBoxOutput("info_avg_attendance", width = 3),
          shinydashboard::infoBoxOutput("info_avg_engagement", width = 3),
          shinydashboard::infoBoxOutput("info_top_emotion", width = 3),
          shinydashboard::box(
            width = 6, status = "primary",
            title = tags$span(icon("chart-bar"), " Attendance Rate Per Lecture"),
            plotOutput("attendance_plot", height = "280px")
          ),
          shinydashboard::box(
            width = 6, status = "warning",
            title = tags$span(icon("smile"), " Emotion Distribution (Pie)"),
            plotOutput("emotion_pie_plot", height = "280px")
          ),
          shinydashboard::box(
            width = 12, status = "info",
            title = tags$span(icon("chart-line"), " Emotion Trends Over Time"),
            plotOutput("emotion_trend_plot", height = "300px")
          ),
          shinydashboard::box(
            width = 7, status = "success",
            title = tags$span(icon("user-graduate"), " Engagement Score Per Student"),
            plotOutput("engagement_bar_plot", height = "320px")
          ),
          shinydashboard::box(
            width = 5, status = "success",
            title = tags$span(icon("table"), " Engagement Table"),
            DT::DTOutput("analytics_table")
          ),
          shinydashboard::box(
            width = 12, status = "primary",
            title = tags$span(icon("camera"), " Session Snapshots"),
            fluidRow(
              column(width = 4,
                selectInput("snapshot_session_select", "Select Session",
                            choices = c("-- Select a session --" = ""), width = "100%")
              ),
              column(width = 2,
                br(),
                actionButton("snapshot_load_btn", "Load Snapshots",
                             class = "btn-info", icon = icon("images"))
              ),
              column(width = 6,
                br(),
                textOutput("snapshot_count_text")
              )
            ),
            br(),
            uiOutput("snapshots_gallery")
          )
        )
      ),
      shinydashboard::tabItem(tabName = "management",
        fluidRow(
          shinydashboard::box(
            width = 4, status = "primary",
            title = tags$span(icon("user-plus"), " Add Lecturer"),
            textInput("m_doctor_id", "Doctor ID", placeholder = "D03"),
            textInput("m_doctor_name", "Full Name", placeholder = "Dr. Ahmed Ali"),
            textInput("m_doctor_username", "Username", placeholder = "ahmed.ali"),
            textInput("m_doctor_password", "Password", placeholder = "Use a strong password"),
            textInput("m_doctor_subject", "Subject", placeholder = "Data Structures"),
            textInput("m_doctor_class", "Class ID", placeholder = "CS101"),
            actionButton("m_add_lecturer_btn", "Add Lecturer", class = "btn-primary", icon = icon("plus")),
            br(), br(),
            textOutput("m_lecturer_msg")
          ),
          shinydashboard::box(
            width = 4, status = "warning",
            title = tags$span(icon("book"), " Add Course"),
            textInput("m_course_id", "Course ID", placeholder = "CS-AI-01"),
            textInput("m_course_name", "Course Name", placeholder = "Introduction to AI"),
            textInput("m_course_class", "Class ID", placeholder = "CS101"),
            numericInput("m_course_duration", "Duration (days)", value = 14, min = 1),
            actionButton("m_add_course_btn", "Add Course", class = "btn-warning", icon = icon("plus")),
            br(), br(),
            textOutput("m_course_msg")
          ),
          shinydashboard::box(
            width = 4, status = "success",
            title = tags$span(icon("calendar-plus"), " Add Schedule"),
            textInput("m_s_doctor_id", "Doctor ID", placeholder = "D03"),
            textInput("m_s_lecture_id", "Lecture ID", placeholder = "L-2026-001"),
            textInput("m_s_course_id", "Course ID", placeholder = "CS-AI-01"),
            textInput("m_s_class_id", "Class ID", placeholder = "CS101"),
            textInput("m_s_group_id", "Group ID", value = "G1"),
            textInput("m_s_day", "Day", placeholder = "Monday"),
            textInput("m_s_time", "Time Slot", placeholder = "10:00-12:00"),
            textInput("m_s_room", "Room", placeholder = "Hall A"),
            numericInput("m_s_duration", "Duration (days)", value = 1, min = 1),
            numericInput("m_s_total_weeks", "Total Weeks", value = 15, min = 1, max = 52),
            actionButton("m_add_schedule_btn", "Add Schedule", class = "btn-success", icon = icon("plus")),
            br(), br(),
            textOutput("m_schedule_msg")
          ),
          shinydashboard::box(
            width = 6, status = "info",
            title = tags$span(icon("chalkboard-teacher"), " Lecturers"),
            DT::DTOutput("m_doctors_table")
          ),
          shinydashboard::box(
            width = 6, status = "info",
            title = tags$span(icon("graduation-cap"), " Courses"),
            DT::DTOutput("m_courses_table")
          )
        )
      ),
      shinydashboard::tabItem(tabName = "student_portal",
        fluidRow(
          column(width = 12,
            div(class = "pro-hero",
              h3("Student Portal"),
              p("View your profile, attendance history, and engagement records.")
            )
          ),
          shinydashboard::box(
            width = 12, status = "primary",
            title = tags$span(icon("user-graduate"), " Profile"),
            uiOutput("student_profile_ui")
          ),
          shinydashboard::box(
            width = 6, status = "info",
            title = tags$span(icon("clipboard-check"), " Attendance History"),
            DT::DTOutput("student_attendance_table")
          ),
          shinydashboard::box(
            width = 6, status = "success",
            title = tags$span(icon("chart-line"), " Engagement & Emotion"),
            DT::DTOutput("student_analytics_table")
          )
        )
      ),
      shinydashboard::tabItem(tabName = "parent_portal",
        fluidRow(
          column(width = 12,
            div(class = "pro-hero",
              h3("Parent Portal"),
              p("Track your linked student attendance and engagement.")
            )
          ),
          shinydashboard::box(
            width = 12, status = "primary",
            title = tags$span(icon("users"), " Linked Student"),
            uiOutput("parent_linked_student_ui")
          ),
          shinydashboard::box(
            width = 6, status = "info",
            title = tags$span(icon("clipboard-check"), " Student Attendance"),
            DT::DTOutput("parent_attendance_table")
          ),
          shinydashboard::box(
            width = 6, status = "warning",
            title = tags$span(icon("brain"), " Student Analytics"),
            DT::DTOutput("parent_analytics_table")
          )
        )
      )
    )
  )
)