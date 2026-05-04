ui <- shinydashboard::dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(
    title = tags$span(icon("graduation-cap"), " Smart Classroom Pro"),
    titleWidth = 280
  ),
  
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenuOutput("sidebar_menu")
  ),
  
  shinydashboard::dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .logo {
          font-weight: 700;
          font-size: 18px;
          background: linear-gradient(90deg, #1f4e79, #2f80c1) !important;
        }
        .main-header .navbar {
          background: linear-gradient(90deg, #1f4e79, #2f80c1) !important;
        }
        .content-wrapper, .right-side {
          background-color: #f4f7fb !important;
        }
        .box {
          border-radius: 12px !important;
          border-top-width: 3px !important;
          box-shadow: 0 8px 20px rgba(0,0,0,0.08) !important;
        }
        .box-header.with-border {
          border-bottom: 1px solid #eef2f7 !important;
        }
        .pro-hero {
          background: linear-gradient(135deg, #1f4e79, #2f80c1);
          color: #ffffff;
          border-radius: 14px;
          padding: 22px;
          margin-bottom: 16px;
          box-shadow: 0 8px 18px rgba(31,78,121,0.25);
        }
        .pro-hero h3 {
          margin-top: 0;
          font-weight: 700;
        }
        .workflow-note {
          background: #eef6ff;
          border-left: 4px solid #2f80c1;
          padding: 12px;
          border-radius: 8px;
          margin-bottom: 10px;
        }
        .btn {
          border-radius: 8px !important;
          font-weight: 600 !important;
        }
        .small-note {
          color: #5c6b7a;
          font-size: 12px;
          margin-top: 8px;
        }
      "))
    ),
    shinydashboard::tabItems(
      
      # ── Login ──────────────────────────────────
      shinydashboard::tabItem(tabName = "login",
                              fluidRow(
                              
                                shinydashboard::box(
                                  title  = "Portal Login",
                                  width  = 4,
                                  status = "primary",
                                  selectInput(
                                    "portal_role",
                                    "Login as",
                                    choices = c("Doctor", "Admin"),
                                    selected = "Doctor"
                                  ),
                                  textInput("username", "Username"),
                                  passwordInput("password", "Password"),
                                  actionButton("login_btn", "Login", class = "btn-primary btn-lg"),
                                  br(), br(),
                                  textOutput("login_msg"),
                                  div(class = "small-note", "Doctor portal: attendance + analytics. Admin portal: lecturers/courses/schedule management.")
                                ),
                               
                              )
      ),
      
      # ── Schedule ───────────────────────────────
      shinydashboard::tabItem(tabName = "schedule",
                              fluidRow(
                                shinydashboard::box(
                                  title  = "Your Weekly Schedule",
                                  width  = 12,
                                  status = "info",
                                  uiOutput("doctor_info"),
                                  br(),
                                  DT::DTOutput("schedule_table"),
                                  br(),
                                  uiOutput("lecture_selector")
                                )
                              )
      ),
      
      # ── Session ────────────────────────────────
      shinydashboard::tabItem(tabName = "session",
                              fluidRow(
                                shinydashboard::box(
                                  title  = "Session Control",
                                  width  = 4,
                                  status = "success",
                                  uiOutput("selected_lecture_info"),
                                  br(),
                                  numericInput("batch_size", "Students per session", value = 25, min = 1, step = 1),
                                  selectInput("batch_number", "Select group", choices = c("1" = 1), selected = 1),
                                  textOutput("batch_info"),
                                  br(),
                                  actionButton("start_btn",  "▶ Start Session", class = "btn-success btn-lg"),
                                  br(), br(),
                                  actionButton("camera_btn", "📷 Start Camera",  class = "btn-info btn-lg"),
                                  br(), br(),
                                  actionButton("end_btn",    "⏹ End Session",    class = "btn-danger btn-lg"),
                                  br(), br(),
                                  textOutput("session_status"),
                                  br(),
                                  textOutput("camera_status"),
                                  div(class = "small-note", "Recommended: start camera 5-10 seconds after starting session.")
                                ),
                                shinydashboard::box(
                                  title  = "Lecture Details & Students",
                                  width  = 8,
                                  status = "info",
                                  uiOutput("lecture_details"),
                                  br(),
                                  h4("🧩 Student Groups Distribution:"),
                                  DT::DTOutput("batch_table"),
                                  br(),
                                  h4("📋 Students in This Class:"),
                                  DT::DTOutput("students_table"),
                                  br(),
                                  h4("✅ Live Attendance:"),
                                  textOutput("attendance_count"),
                                  br(),
                                  DT::DTOutput("attendance_table"),
                                  br(),
                                  h4("❌ Absent Students Report:"),
                                  textOutput("absent_count"),
                                  DT::DTOutput("absent_table")
                                )
                              )
      ),
      
      # ── Analytics ──────────────────────────────
      shinydashboard::tabItem(tabName = "analytics",
                              fluidRow(
                                column(
                                  width = 12,
                                  div(
                                    class = "pro-hero",
                                    h3("Presentation-Ready Analytics"),
                                    p("Use these visuals during doctor meetings to clearly present participation quality, engagement, and classroom behavior trends.")
                                  )
                                ),
                                shinydashboard::box(
                                  title  = "Attendance Summary",
                                  width  = 6,
                                  status = "primary",
                                  plotOutput("attendance_plot")
                                ),
                                shinydashboard::box(
                                  title  = "Session Statistics",
                                  width  = 6,
                                  status = "info",
                                  DT::DTOutput("analytics_table")
                                ),
                                shinydashboard::box(
                                  title = "Emotion Distribution",
                                  width = 6,
                                  status = "warning",
                                  plotOutput("emotion_plot")
                                ),
                                shinydashboard::box(
                                  title = "Engagement Clustering",
                                  width = 6,
                                  status = "success",
                                  plotOutput("engagement_cluster_plot")
                                ),
                                shinydashboard::box(
                                  title = "Behavior Detection Summary",
                                  width = 12,
                                  status = "danger",
                                  plotOutput("behavior_plot")
                                )
                              )
      ),
      shinydashboard::tabItem(tabName = "management",
                              fluidRow(
                                shinydashboard::box(
                                  title = "Add Lecturer",
                                  width = 4,
                                  status = "primary",
                                  textInput("m_doctor_id", "Doctor ID", placeholder = "D03"),
                                  textInput("m_doctor_name", "Full Name"),
                                  textInput("m_doctor_username", "Username"),
                                  textInput("m_doctor_password", "Password"),
                                  textInput("m_doctor_subject", "Subject"),
                                  textInput("m_doctor_class", "Class ID", placeholder = "CS101"),
                                  actionButton("m_add_lecturer_btn", "Add Lecturer", class = "btn-primary"),
                                  br(), br(),
                                  textOutput("m_lecturer_msg")
                                ),
                                shinydashboard::box(
                                  title = "Add Course",
                                  width = 4,
                                  status = "warning",
                                  textInput("m_course_id", "Course ID", placeholder = "CS-AI-01"),
                                  textInput("m_course_name", "Course Name"),
                                  textInput("m_course_class", "Class ID", placeholder = "CS101"),
                                  numericInput("m_course_duration", "Duration (days)", value = 14, min = 1),
                                  actionButton("m_add_course_btn", "Add Course", class = "btn-warning"),
                                  br(), br(),
                                  textOutput("m_course_msg")
                                ),
                                shinydashboard::box(
                                  title = "Add Lecture Schedule",
                                  width = 4,
                                  status = "success",
                                  textInput("m_s_doctor_id", "Doctor ID"),
                                  textInput("m_s_lecture_id", "Lecture ID"),
                                  textInput("m_s_course_id", "Course ID"),
                                  textInput("m_s_class_id", "Class ID"),
                                  textInput("m_s_group_id", "Group ID", value = "G1"),
                                  textInput("m_s_day", "Day", placeholder = "Monday"),
                                  textInput("m_s_time", "Time Slot", placeholder = "10:00-12:00"),
                                  textInput("m_s_room", "Room", placeholder = "Hall A"),
                                  numericInput("m_s_duration", "Duration (days)", value = 1, min = 1),
                                  actionButton("m_add_schedule_btn", "Add Schedule", class = "btn-success"),
                                  br(), br(),
                                  textOutput("m_schedule_msg")
                                ),
                                shinydashboard::box(
                                  title = "Lecturers",
                                  width = 6,
                                  status = "info",
                                  DT::DTOutput("m_doctors_table")
                                ),
                                shinydashboard::box(
                                  title = "Courses",
                                  width = 6,
                                  status = "info",
                                  DT::DTOutput("m_courses_table")
                                )
                              )
      )
      
    )
  )
)