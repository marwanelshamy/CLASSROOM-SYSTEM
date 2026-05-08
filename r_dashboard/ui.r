ui <- shinydashboard::dashboardPage(
  skin = "black",

  # ── Header ───────────────────────────────────────
  shinydashboard::dashboardHeader(
    title = tags$span(
      tags$i(class = "fa fa-brain", style = "color:#4a90d9; margin-right:8px;"),
      tags$span("EduSense", style = "font-family:'Sora',sans-serif; font-weight:700; letter-spacing:-0.5px;"),
      tags$span(" Pro", style = "color:#4a90d9; font-weight:300;")
    ),
    titleWidth = 260,
    tags$li(
      class = "dropdown",
      style = "padding:8px 16px;",
      actionButton("theme_toggle_btn", "",
                   icon  = icon("circle-half-stroke"),
                   class = "btn-theme-toggle",
                   style = "background:transparent; border:none; color:#7a9bb5; font-size:16px; padding:6px;")
    ),
    tags$li(
      class = "dropdown",
      uiOutput("header_user_badge")
    )
  ),

  # ── Sidebar ──────────────────────────────────────
  shinydashboard::dashboardSidebar(
    width = 240,

    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Sora:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap"),
      shinyjs::useShinyjs(),
      tags$style(HTML("

        /* ══════════════════════════════════════════
           CSS VARIABLES — DARK (default)
        ══════════════════════════════════════════ */
        :root, body[data-theme='dark'] {
          --bg-page:       #080e14;
          --bg-panel:      #0d1520;
          --bg-card:       #111d2e;
          --bg-input:      #0a1018;
          --bg-hover:      rgba(74,144,217,0.07);
          --border:        rgba(74,144,217,0.12);
          --border-strong: rgba(74,144,217,0.25);
          --text-main:     #c8ddf0;
          --text-head:     #e8f2fc;
          --text-muted:    #6a8aaa;
          --text-dim:      #2d4a65;
          --accent:        #4a90d9;
          --accent-glow:   rgba(74,144,217,0.15);
          --success:       #2dd47a;
          --success-bg:    rgba(45,212,122,0.1);
          --danger:        #e05568;
          --danger-bg:     rgba(224,85,104,0.1);
          --warning:       #f0a500;
          --warning-bg:    rgba(240,165,0,0.1);
          --shadow:        0 4px 24px rgba(0,0,0,0.4);
          --radius-sm:     6px;
          --radius-md:     10px;
          --radius-lg:     14px;
        }

        /* ══════════════════════════════════════════
           CSS VARIABLES — LIGHT
        ══════════════════════════════════════════ */
        body[data-theme='light'] {
          --bg-page:       #eef3f8;
          --bg-panel:      #f8fbff;
          --bg-card:       #ffffff;
          --bg-input:      #f0f5fa;
          --bg-hover:      rgba(26,111,196,0.05);
          --border:        rgba(26,111,196,0.12);
          --border-strong: rgba(26,111,196,0.28);
          --text-main:     #1a2e42;
          --text-head:     #0c1e30;
          --text-muted:    #4a6a88;
          --text-dim:      #9ab0c5;
          --accent:        #1a6fc4;
          --accent-glow:   rgba(26,111,196,0.12);
          --success:       #1a9455;
          --success-bg:    rgba(26,148,85,0.08);
          --danger:        #c0392b;
          --danger-bg:     rgba(192,57,43,0.08);
          --warning:       #c07800;
          --warning-bg:    rgba(192,120,0,0.08);
          --shadow:        0 4px 20px rgba(26,111,196,0.08);
          --radius-sm:     6px;
          --radius-md:     10px;
          --radius-lg:     14px;
        }

        /* ══════════════════════════════════════════
           GLOBAL
        ══════════════════════════════════════════ */
        *, *::before, *::after { box-sizing: border-box; }

        body, .content-wrapper, .main-sidebar, .left-side {
          font-family: 'Sora', sans-serif !important;
          background-color: var(--bg-page) !important;
          color: var(--text-main) !important;
          transition: background-color 0.35s ease, color 0.35s ease;
        }

        /* ══════════════════════════════════════════
           SIDEBAR
        ══════════════════════════════════════════ */
        .main-sidebar {
          background: var(--bg-panel) !important;
          border-right: 1px solid var(--border) !important;
          box-shadow: 2px 0 20px rgba(0,0,0,0.2) !important;
        }
        .main-sidebar .sidebar { background: var(--bg-panel) !important; }

        .sidebar-menu > li > a {
          color: var(--text-muted) !important;
          font-size: 13px !important;
          font-weight: 500 !important;
          padding: 11px 18px !important;
          margin: 2px 10px !important;
          border-radius: var(--radius-md) !important;
          transition: all 0.2s ease !important;
          display: flex !important;
          align-items: center !important;
          gap: 10px !important;
        }
        .sidebar-menu > li > a:hover {
          background: var(--bg-hover) !important;
          color: var(--text-main) !important;
          transform: translateX(2px) !important;
        }
        .sidebar-menu > li.active > a {
          background: var(--accent-glow) !important;
          color: var(--accent) !important;
          border-left: 3px solid var(--accent) !important;
          padding-left: 15px !important;
          font-weight: 600 !important;
        }
        .sidebar-menu .header {
          color: var(--text-dim) !important;
          font-size: 9px !important;
          font-weight: 700 !important;
          letter-spacing: 1.5px !important;
          text-transform: uppercase !important;
          padding: 20px 18px 6px !important;
        }
        .sidebar-menu > li > a > .fa,
        .sidebar-menu > li > a > .fas,
        .sidebar-menu > li > a > .far {
          width: 18px !important;
          text-align: center !important;
          opacity: 0.7 !important;
        }
        .sidebar-menu > li.active > a > .fa { opacity: 1 !important; }

        /* ══════════════════════════════════════════
           HEADER
        ══════════════════════════════════════════ */
        .main-header .logo {
          background: var(--bg-panel) !important;
          border-bottom: 1px solid var(--border) !important;
          font-family: 'Sora', sans-serif !important;
          font-weight: 700 !important;
          font-size: 15px !important;
          color: var(--text-head) !important;
          letter-spacing: -0.3px !important;
        }
        .main-header .navbar {
          background: var(--bg-panel) !important;
          border-bottom: 1px solid var(--border) !important;
          box-shadow: 0 1px 0 var(--border) !important;
        }
        .main-header .navbar .nav > li > a {
          color: var(--text-muted) !important;
          transition: color 0.2s !important;
        }
        .main-header .navbar .nav > li > a:hover { color: var(--accent) !important; }

        /* ══════════════════════════════════════════
           CONTENT
        ══════════════════════════════════════════ */
        .content-wrapper { background: var(--bg-page) !important; }
        .content { padding: 24px 28px !important; }

        /* ══════════════════════════════════════════
           BOXES
        ══════════════════════════════════════════ */
        .box {
          background: var(--bg-card) !important;
          border: 1px solid var(--border) !important;
          border-top: none !important;
          border-radius: var(--radius-lg) !important;
          box-shadow: var(--shadow) !important;
          color: var(--text-main) !important;
          transition: border-color 0.2s, box-shadow 0.2s !important;
        }
        .box:hover { border-color: var(--border-strong) !important; }
        .box-header {
          background: var(--bg-card) !important;
          color: var(--text-head) !important;
          border-bottom: 1px solid var(--border) !important;
          border-radius: var(--radius-lg) var(--radius-lg) 0 0 !important;
          font-family: 'Sora', sans-serif !important;
          font-size: 13px !important;
          font-weight: 600 !important;
          padding: 14px 18px !important;
          letter-spacing: 0.2px !important;
        }
        .box-header .fa { color: var(--accent); margin-right: 6px; }
        .box-body { padding: 18px !important; }

        /* ══════════════════════════════════════════
           INFO BOXES
        ══════════════════════════════════════════ */
        .info-box {
          background: var(--bg-card) !important;
          border: 1px solid var(--border) !important;
          border-radius: var(--radius-md) !important;
          box-shadow: none !important;
          transition: border-color 0.2s, transform 0.2s !important;
          min-height: 80px !important;
        }
        .info-box:hover { border-color: var(--border-strong) !important; transform: translateY(-2px) !important; }
        .info-box-icon {
          border-radius: var(--radius-md) 0 0 var(--radius-md) !important;
          width: 70px !important;
        }
        .info-box-content { color: var(--text-main) !important; padding: 10px 14px !important; }
        .info-box-number {
          color: var(--text-head) !important;
          font-family: 'Sora', sans-serif !important;
          font-size: 22px !important;
          font-weight: 700 !important;
        }
        .info-box-text {
          color: var(--text-muted) !important;
          font-size: 11px !important;
          font-weight: 500 !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
        }

        /* ══════════════════════════════════════════
           BUTTONS
        ══════════════════════════════════════════ */
        .btn {
          border-radius: var(--radius-sm) !important;
          font-family: 'Sora', sans-serif !important;
          font-weight: 600 !important;
          font-size: 13px !important;
          padding: 9px 18px !important;
          border: none !important;
          transition: all 0.2s ease !important;
          letter-spacing: 0.2px !important;
        }
        .btn-primary {
          background: linear-gradient(135deg, #1a6fc4, #4a90d9) !important;
          color: #fff !important;
          box-shadow: 0 4px 12px rgba(74,144,217,0.3) !important;
        }
        .btn-success {
          background: linear-gradient(135deg, #0e7a40, #2dd47a) !important;
          color: #fff !important;
          box-shadow: 0 4px 12px rgba(45,212,122,0.25) !important;
        }
        .btn-danger {
          background: linear-gradient(135deg, #a02030, #e05568) !important;
          color: #fff !important;
          box-shadow: 0 4px 12px rgba(224,85,104,0.25) !important;
        }
        .btn-info {
          background: var(--accent-glow) !important;
          color: var(--accent) !important;
          border: 1px solid var(--border-strong) !important;
          box-shadow: none !important;
        }
        .btn-warning {
          background: linear-gradient(135deg, #c07800, #f0a500) !important;
          color: #fff !important;
        }
        .btn:hover { opacity: 0.88 !important; transform: translateY(-1px) !important; }
        .btn:active { transform: translateY(0) !important; }
        .btn-lg { width: 100% !important; margin-bottom: 10px !important; padding: 11px 18px !important; }

        /* ══════════════════════════════════════════
           FORM CONTROLS
        ══════════════════════════════════════════ */
        .form-control {
          background: var(--bg-input) !important;
          border: 1px solid var(--border) !important;
          border-radius: var(--radius-sm) !important;
          color: var(--text-main) !important;
          font-family: 'Sora', sans-serif !important;
          font-size: 13px !important;
          padding: 8px 12px !important;
          transition: border-color 0.2s, box-shadow 0.2s !important;
        }
        .form-control:focus {
          border-color: var(--accent) !important;
          box-shadow: 0 0 0 3px var(--accent-glow) !important;
          outline: none !important;
        }
        .selectize-control .selectize-input {
          background: var(--bg-input) !important;
          border: 1px solid var(--border) !important;
          border-radius: var(--radius-sm) !important;
          color: var(--text-main) !important;
          font-family: 'Sora', sans-serif !important;
          font-size: 13px !important;
        }
        .selectize-dropdown {
          background: var(--bg-card) !important;
          border: 1px solid var(--border-strong) !important;
          border-radius: var(--radius-sm) !important;
          color: var(--text-main) !important;
        }
        .selectize-dropdown .option:hover,
        .selectize-dropdown .option.active {
          background: var(--bg-hover) !important;
          color: var(--accent) !important;
        }
        label {
          color: var(--text-muted) !important;
          font-size: 11px !important;
          font-weight: 600 !important;
          letter-spacing: 0.5px !important;
          text-transform: uppercase !important;
          margin-bottom: 5px !important;
        }

        /* ══════════════════════════════════════════
           DATATABLES
        ══════════════════════════════════════════ */
        .dataTables_wrapper { color: var(--text-main) !important; }
        table.dataTable {
          border-collapse: separate !important;
          border-spacing: 0 !important;
        }
        table.dataTable thead th {
          background: var(--bg-input) !important;
          color: var(--text-muted) !important;
          font-size: 10px !important;
          font-weight: 700 !important;
          letter-spacing: 1px !important;
          text-transform: uppercase !important;
          border-bottom: 1px solid var(--border) !important;
          padding: 10px 14px !important;
        }
        table.dataTable tbody tr {
          background: var(--bg-card) !important;
          color: var(--text-main) !important;
          transition: background 0.15s !important;
        }
        table.dataTable tbody tr:hover { background: var(--bg-hover) !important; }
        table.dataTable tbody td {
          border-bottom: 1px solid var(--border) !important;
          font-size: 13px !important;
          padding: 10px 14px !important;
        }
        .dataTables_filter input,
        .dataTables_length select {
          background: var(--bg-input) !important;
          border: 1px solid var(--border) !important;
          color: var(--text-main) !important;
          border-radius: var(--radius-sm) !important;
          padding: 5px 10px !important;
          font-size: 12px !important;
        }
        .dataTables_info { color: var(--text-dim) !important; font-size: 11px !important; }
        .dataTables_paginate .paginate_button {
          color: var(--text-muted) !important;
          border-radius: var(--radius-sm) !important;
          border: none !important;
          padding: 4px 10px !important;
          font-size: 12px !important;
        }
        .dataTables_paginate .paginate_button.current {
          background: var(--accent-glow) !important;
          color: var(--accent) !important;
          border: 1px solid var(--border-strong) !important;
        }
        .dataTables_paginate .paginate_button:hover:not(.current) {
          background: var(--bg-hover) !important;
          color: var(--text-main) !important;
        }

        /* ══════════════════════════════════════════
           PLOTS
        ══════════════════════════════════════════ */
        .shiny-plot-output {
          border-radius: var(--radius-md) !important;
          overflow: hidden !important;
        }

        /* ══════════════════════════════════════════
           CUSTOM COMPONENTS
        ══════════════════════════════════════════ */

        /* Hero banner */
        .pro-hero {
          background: linear-gradient(135deg, var(--bg-card), var(--bg-panel));
          border: 1px solid var(--border-strong);
          border-radius: var(--radius-lg);
          padding: 22px 26px;
          margin-bottom: 20px;
          position: relative;
          overflow: hidden;
        }
        .pro-hero::before {
          content: '';
          position: absolute;
          top: -40px; right: -40px;
          width: 160px; height: 160px;
          border-radius: 50%;
          background: var(--accent-glow);
        }
        .pro-hero h3 {
          font-family: 'Sora', sans-serif;
          font-size: 18px;
          font-weight: 700;
          color: var(--text-head);
          margin: 0 0 6px;
          letter-spacing: -0.3px;
        }
        .pro-hero p {
          color: var(--text-muted);
          font-size: 13px;
          margin: 0;
          line-height: 1.6;
        }

        /* Live badge */
        .live-badge {
          display: inline-flex;
          align-items: center;
          gap: 7px;
          background: var(--success-bg);
          color: var(--success);
          padding: 5px 14px;
          border-radius: 20px;
          font-size: 12px;
          font-weight: 600;
          border: 1px solid rgba(45,212,122,0.25);
          letter-spacing: 0.3px;
        }
        .live-dot {
          width: 7px; height: 7px;
          border-radius: 50%;
          background: var(--success);
          animation: pulse 1.5s infinite;
          flex-shrink: 0;
        }
        @keyframes pulse {
          0%,100% { opacity:1; transform:scale(1); }
          50%      { opacity:0.4; transform:scale(0.85); }
        }

        /* Status pills */
        .pill {
          display: inline-block;
          padding: 3px 10px;
          border-radius: 20px;
          font-size: 11px;
          font-weight: 600;
          letter-spacing: 0.3px;
        }
        .pill-success { background: var(--success-bg); color: var(--success); }
        .pill-danger  { background: var(--danger-bg);  color: var(--danger);  }
        .pill-warning { background: var(--warning-bg); color: var(--warning); }
        .pill-accent  { background: var(--accent-glow); color: var(--accent); }

        /* Week progress */
        .week-track {
          background: var(--bg-input);
          border-radius: 6px;
          height: 8px;
          overflow: hidden;
          margin: 6px 0;
        }
        .week-fill {
          height: 100%;
          border-radius: 6px;
          background: linear-gradient(90deg, var(--accent), var(--success));
          transition: width 0.6s ease;
        }

        /* Section divider */
        .section-divider {
          border: none;
          border-top: 1px solid var(--border);
          margin: 14px 0;
        }

        /* Small note */
        .small-note {
          color: var(--text-dim);
          font-size: 11px;
          margin-top: 8px;
          line-height: 1.6;
        }

        /* H4 section headers */
        h4 {
          font-family: 'Sora', sans-serif !important;
          font-size: 10px !important;
          font-weight: 700 !important;
          color: var(--text-dim) !important;
          text-transform: uppercase !important;
          letter-spacing: 1.5px !important;
          margin: 18px 0 10px !important;
        }

        /* User badge in header */
        .user-badge {
          display: flex;
          align-items: center;
          gap: 8px;
          padding: 6px 16px;
          color: var(--text-muted);
          font-size: 12px;
          font-weight: 500;
        }
        .user-avatar {
          width: 28px; height: 28px;
          border-radius: 50%;
          background: var(--accent-glow);
          border: 1px solid var(--border-strong);
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 11px;
          font-weight: 700;
          color: var(--accent);
        }

        /* Theme toggle button */
        .btn-theme-toggle {
          background: transparent !important;
          border: none !important;
          color: var(--text-muted) !important;
          font-size: 16px !important;
          padding: 6px 10px !important;
          transition: color 0.2s !important;
        }
        .btn-theme-toggle:hover { color: var(--accent) !important; }

        /* Logout button in sidebar */
        .btn-logout {
          width: 100% !important;
          background: var(--danger-bg) !important;
          color: var(--danger) !important;
          border: 1px solid rgba(224,85,104,0.2) !important;
          border-radius: var(--radius-sm) !important;
          font-size: 12px !important;
          font-weight: 600 !important;
          padding: 8px 14px !important;
          transition: all 0.2s !important;
          text-align: left !important;
        }
        .btn-logout:hover {
          background: rgba(224,85,104,0.2) !important;
          transform: none !important;
        }

        /* Scrollbar */
        ::-webkit-scrollbar { width: 5px; height: 5px; }
        ::-webkit-scrollbar-track { background: var(--bg-page); }
        ::-webkit-scrollbar-thumb { background: var(--border-strong); border-radius: 3px; }
        ::-webkit-scrollbar-thumb:hover { background: var(--accent); }

        /* Animations */
        .box { animation: fadeSlideUp 0.3s ease both; }
        @keyframes fadeSlideUp {
          from { opacity:0; transform:translateY(8px); }
          to   { opacity:1; transform:translateY(0);   }
        }

      "))
    ),

    # ── Sidebar menu (dynamic) ──────────────────────
    shinydashboard::sidebarMenuOutput("sidebar_menu"),

    # ── Logout button (only when logged in) ─────────
    conditionalPanel(
      condition = "output.is_logged_in",
      tags$div(
        style = "position:absolute; bottom:0; left:0; right:0; padding:12px 16px; border-top:1px solid var(--border);",
        actionButton("logout_btn", tags$span(icon("sign-out-alt"), " Sign Out"),
                     class = "btn-logout")
      )
    )
  ),

  # ── Body ─────────────────────────────────────────
  shinydashboard::dashboardBody(
    tags$script(HTML("
      // Theme toggle
      Shiny.addCustomMessageHandler('setTheme', function(theme) {
        document.body.setAttribute('data-theme', theme);
        localStorage.setItem('scTheme', theme);
      });
      // Apply saved theme immediately
      (function() {
        var t = localStorage.getItem('scTheme') || 'dark';
        document.body.setAttribute('data-theme', t);
      })();
    ")),

    shinydashboard::tabItems(

      # ══════════════════════════════════════════════
      # LOGIN
      # ══════════════════════════════════════════════
      shinydashboard::tabItem(tabName = "login",
        fluidRow(
          column(width = 4, offset = 4,
            br(), br(),
            div(class = "pro-hero", style = "text-align:center; margin-bottom:24px;",
              tags$i(class = "fa fa-brain fa-2x",
                     style = "color:var(--accent); margin-bottom:12px; display:block;"),
              h3("EduSense Portal"),
              p("AI-powered classroom intelligence system")
            ),
            shinydashboard::box(
              width = 12, status = "primary",
              title = tags$span(icon("lock"), " Sign In"),
              selectInput("portal_role", "Portal",
                          choices  = c("Doctor" = "Doctor",
                                       "Student" = "Student",
                                       "Parent"  = "Parent",
                                       "Admin"   = "Admin"),
                          selected = "Doctor"),
              textInput("username", "Username",
                        placeholder = "Enter your username"),
              passwordInput("password", "Password",
                            placeholder = "Enter your password"),
              br(),
              actionButton("login_btn", tags$span(icon("sign-in-alt"), " Sign In"),
                           class = "btn-primary btn-lg"),
              br(), br(),
              textOutput("login_msg"),
              hr(class = "section-divider"),
              div(class = "small-note",
                tags$b("Doctor:"), " attendance, sessions & analytics", br(),
                tags$b("Student:"), " personal records & engagement", br(),
                tags$b("Parent:"), " monitor your student", br(),
                tags$b("Admin:"), " system management"
              )
            )
          )
        )
      ),

      # ══════════════════════════════════════════════
      # SCHEDULE
      # ══════════════════════════════════════════════
      shinydashboard::tabItem(tabName = "schedule",
        fluidRow(
          column(width = 12,
            div(class = "pro-hero",
              h3(icon("calendar-alt"), " Weekly Schedule"),
              p("Select a lecture to open a session. Click any row then press Open.")
            )
          ),
          shinydashboard::box(
            width = 12, status = "info",
            title = tags$span(icon("table"), " Lecture Timetable"),
            uiOutput("doctor_info"),
            hr(class = "section-divider"),
            fluidRow(
              column(width = 2,
                actionButton("refresh_schedule_btn", tags$span(icon("sync"), " Refresh"),
                             class = "btn-info")
              ),
              column(width = 10, br(), textOutput("schedule_refresh_msg"))
            ),
            br(),
            DT::DTOutput("schedule_table"),
            br(),
            uiOutput("lecture_selector")
          )
        )
      ),

      # ══════════════════════════════════════════════
      # SESSION
      # ══════════════════════════════════════════════
      shinydashboard::tabItem(tabName = "session",
        fluidRow(

          # ── Control panel ──
          shinydashboard::box(
            width = 4, status = "success",
            title = tags$span(icon("sliders-h"), " Session Control"),

            uiOutput("selected_lecture_info"),
            hr(class = "section-divider"),

            # Week selector with progress
            h4(icon("calendar-week"), " Week"),
            uiOutput("week_progress_bar"),
            selectInput("week_number", NULL,
                        choices  = setNames(1:15, paste("Week", 1:15)),
                        selected = 1),
            textOutput("week_info"),
            hr(class = "section-divider"),

            # Batch selector
            h4(icon("layer-group"), " Student Group"),
            fluidRow(
              column(6, numericInput("batch_size", "Group size",
                                    value = 25, min = 1, step = 1)),
              column(6, selectInput("batch_number", "Group",
                                   choices = c("1" = 1), selected = 1))
            ),
            textOutput("batch_info"),
            hr(class = "section-divider"),

            # Action buttons
            actionButton("start_btn", tags$span(icon("play"), " Start Session"),
                         class = "btn-success btn-lg"),
            actionButton("camera_btn", tags$span(icon("camera"), " Start Camera"),
                         class = "btn-info btn-lg"),
            actionButton("stop_camera_btn", tags$span(icon("video-slash"), " Stop Camera"),
                         class = "btn-warning btn-lg",
                         style = "display:none;"),
            actionButton("snapshot_btn", tags$span(icon("camera-retro"), " Take Snapshot"),
                         class = "btn-primary btn-lg",
                         style = "display:none;"),
            actionButton("end_btn", tags$span(icon("stop"), " End Session"),
                         class = "btn-danger btn-lg"),
            hr(class = "section-divider"),

            textOutput("session_status"),
            br(),
            uiOutput("camera_status_ui"),
            div(class = "small-note",
              icon("info-circle"),
              " Camera runs in-browser and sends frames to the backend automatically.")
          ),

          # ── Main panel ──
          shinydashboard::box(
            width = 8, status = "info",
            title = tags$span(icon("users"), " Live Classroom"),

            # ── Embedded camera feed ──
            tags$div(
              id = "camera_feed_container",
              style = "display:none; margin-bottom:16px;",
              tags$div(
                style = paste0(
                  "position:relative; background:#000; border-radius:10px;",
                  "overflow:hidden; border:2px solid var(--border-strong);",
                  "width:100%; aspect-ratio:16/9;"
                ),
                tags$video(
                  id = "live_video",
                  autoplay = NA,
                  playsinline = NA,
                  muted = NA,
                  style = "width:100%; height:100%; display:block; object-fit:cover; position:absolute; top:0; left:0;"
                ),
                # Overlay canvas for face boxes
                tags$canvas(
                  id = "face_overlay",
                  style = "position:absolute; top:0; left:0; width:100%; height:100%; pointer-events:none;"
                ),
                # Overlay: live badge + snapshot flash
                tags$div(
                  id = "camera_overlay",
                  style = paste0(
                    "position:absolute; top:10px; left:10px;",
                    "display:flex; gap:8px; align-items:center;"
                  ),
                  tags$span(
                    class = "live-badge",
                    tags$span(class = "live-dot"),
                    "LIVE"
                  )
                ),
                tags$div(
                  id = "snapshot_flash",
                  style = paste0(
                    "position:absolute; inset:0; background:white;",
                    "opacity:0; pointer-events:none;",
                    "transition:opacity 0.1s ease;"
                  )
                )
              ),
              # Hidden canvas for frame capture
              tags$canvas(id = "capture_canvas", style = "display:none;"),
              # Snapshot result message
              tags$div(
                id = "snapshot_msg",
                style = "font-size:11px; color:var(--success); margin-top:6px; min-height:16px;"
              )
            ),

            # Camera JS logic
            tags$script(HTML("
              var cameraStream = null;
              var frameInterval = null;
              var sessionId = null;
              var lastRecognized = [];

              // ── Draw face boxes on overlay canvas ──────────
              function drawFaceBoxes(recognized) {
                var video   = document.getElementById('live_video');
                var overlay = document.getElementById('face_overlay');
                if (!overlay || !video) return;

                // Match canvas pixel size to the video's DISPLAYED size (CSS pixels)
                var rect = video.getBoundingClientRect();
                overlay.width  = rect.width  || video.clientWidth  || 640;
                overlay.height = rect.height || video.clientHeight || 480;

                var ctx = overlay.getContext('2d');
                ctx.clearRect(0, 0, overlay.width, overlay.height);

                if (!recognized || recognized.length === 0) return;

                // The backend bbox coords are in native video resolution
                // Scale them to the displayed canvas size
                var nativeW = video.videoWidth  || overlay.width;
                var nativeH = video.videoHeight || overlay.height;
                var scaleX  = overlay.width  / nativeW;
                var scaleY  = overlay.height / nativeH;

                recognized.forEach(function(person) {
                  var box = person.bbox || {};
                  var l = (box.left   || 0) * scaleX;
                  var t = (box.top    || 0) * scaleY;
                  var r = (box.right  || 0) * scaleX;
                  var b = (box.bottom || 0) * scaleY;
                  var w = r - l;
                  var h = b - t;
                  if (w <= 0 || h <= 0) return;

                  var emotion = (person.emotion || 'neutral').toUpperCase();
                  var name    = person.name || '';
                  var sid     = person.id   || '';
                  var label   = sid + '  ' + name + '  ' + emotion;

                  // Green bounding box
                  ctx.strokeStyle = '#2dd47a';
                  ctx.lineWidth   = 2.5;
                  ctx.strokeRect(l, t, w, h);

                  // Corner accent marks
                  var cs = Math.min(w, h) * 0.18;
                  ctx.strokeStyle = '#2dd47a';
                  ctx.lineWidth   = 4;
                  ctx.beginPath(); ctx.moveTo(l, t+cs); ctx.lineTo(l, t); ctx.lineTo(l+cs, t); ctx.stroke();
                  ctx.beginPath(); ctx.moveTo(r-cs, t); ctx.lineTo(r, t); ctx.lineTo(r, t+cs); ctx.stroke();
                  ctx.beginPath(); ctx.moveTo(l, b-cs); ctx.lineTo(l, b); ctx.lineTo(l+cs, b); ctx.stroke();
                  ctx.beginPath(); ctx.moveTo(r-cs, b); ctx.lineTo(r, b); ctx.lineTo(r, b-cs); ctx.stroke();

                  // Label background + text
                  ctx.font = 'bold 13px monospace';
                  var tw = ctx.measureText(label).width;
                  var lx = l;
                  var ly = t > 24 ? t - 6 : b + 20;
                  ctx.fillStyle = 'rgba(13,21,32,0.85)';
                  ctx.fillRect(lx - 2, ly - 15, tw + 12, 20);
                  ctx.fillStyle = '#2dd47a';
                  ctx.fillText(label, lx + 4, ly);
                });
              }

              // ── Start camera ────────────────────────────────
              Shiny.addCustomMessageHandler('startCamera', function(sid) {
                sessionId = sid;
                var video     = document.getElementById('live_video');
                var container = document.getElementById('camera_feed_container');

                navigator.mediaDevices.getUserMedia({
                  video: { width: { ideal: 1280 }, height: { ideal: 720 }, facingMode: 'user' },
                  audio: false
                })
                .then(function(stream) {
                  cameraStream = stream;
                  video.srcObject = stream;
                  container.style.display = 'block';
                  document.getElementById('stop_camera_btn').style.display = '';
                  document.getElementById('snapshot_btn').style.display = '';
                  document.getElementById('camera_btn').style.display = 'none';

                  // Continuously redraw face boxes
                  (function loop() {
                    if (lastRecognized.length > 0) drawFaceBoxes(lastRecognized);
                    requestAnimationFrame(loop);
                  })();

                  frameInterval = setInterval(function() { sendFrame(false); }, 3000);
                })
                .catch(function(err) {
                  var msg  = err.message || err.name || String(err);
                  var hint = '';
                  if (msg.toLowerCase().indexOf('permission') !== -1 ||
                      msg.toLowerCase().indexOf('denied')     !== -1 ||
                      err.name === 'NotAllowedError') {
                    hint = ' — Click the camera icon in your browser address bar, set it to Allow, then try again.';
                  } else if (err.name === 'NotFoundError') {
                    hint = ' — No camera found. Please connect a webcam.';
                  } else if (err.name === 'NotReadableError') {
                    hint = ' — Camera is in use by another app. Close it and try again.';
                  }
                  Shiny.setInputValue('camera_error', msg + hint, {priority: 'event'});
                });
              });

              // ── Receive face results from Shiny → draw boxes ─
              Shiny.addCustomMessageHandler('faceResults', function(data) {
                lastRecognized = data.recognized || [];
                drawFaceBoxes(lastRecognized);
              });

              // ── Stop camera ──────────────────────────────────
              Shiny.addCustomMessageHandler('stopCamera', function(msg) { stopCameraFeed(); });

              function stopCameraFeed() {
                if (frameInterval) { clearInterval(frameInterval); frameInterval = null; }
                if (cameraStream) {
                  cameraStream.getTracks().forEach(function(t) { t.stop(); });
                  cameraStream = null;
                }
                lastRecognized = [];
                var overlay = document.getElementById('face_overlay');
                if (overlay) overlay.getContext('2d').clearRect(0, 0, overlay.width, overlay.height);
                var container = document.getElementById('camera_feed_container');
                if (container) container.style.display = 'none';
                document.getElementById('stop_camera_btn').style.display = 'none';
                document.getElementById('snapshot_btn').style.display = 'none';
                document.getElementById('camera_btn').style.display = '';
              }

              // ── Capture & send frame ─────────────────────────
              function sendFrame(isSnapshot) {
                if (!sessionId) return;
                var video  = document.getElementById('live_video');
                var canvas = document.getElementById('capture_canvas');
                if (!video || !canvas || video.readyState < 2) return;
                canvas.width  = video.videoWidth  || 640;
                canvas.height = video.videoHeight || 480;
                var ctx = canvas.getContext('2d');
                ctx.drawImage(video, 0, 0, canvas.width, canvas.height);
                var base64 = canvas.toDataURL('image/jpeg', 0.85).split(',')[1];
                if (isSnapshot) {
                  Shiny.setInputValue('snapshot_frame', { data: base64, session_id: sessionId }, {priority: 'event'});
                  var flash = document.getElementById('snapshot_flash');
                  flash.style.opacity = '0.7';
                  setTimeout(function() { flash.style.opacity = '0'; }, 300);
                } else {
                  Shiny.setInputValue('camera_frame', { data: base64, session_id: sessionId }, {priority: 'event'});
                }
              }

              // ── Button clicks ────────────────────────────────
              document.addEventListener('click', function(e) {
                var t = e.target;
                if (t && (t.id === 'snapshot_btn' || (t.closest && t.closest('#snapshot_btn')))) {
                  sendFrame(true);
                }
                if (t && (t.id === 'stop_camera_btn' || (t.closest && t.closest('#stop_camera_btn')))) {
                  stopCameraFeed();
                  Shiny.setInputValue('camera_stopped', Date.now(), {priority: 'event'});
                }
              });
            ")),

            # Lecture info boxes
            uiOutput("lecture_details"),
            hr(class = "section-divider"),

            # Live attendance header
            fluidRow(
              column(6, h4(icon("check-circle"), " Live Attendance")),
              column(6, style = "text-align:right;",
                     uiOutput("live_badge"))
            ),
            textOutput("attendance_count"),
            br(),
            DT::DTOutput("attendance_table"),

            hr(class = "section-divider"),
            h4(icon("times-circle"), " Absent Students"),
            textOutput("absent_count"),
            DT::DTOutput("absent_table"),

            hr(class = "section-divider"),
            h4(icon("list-ol"), " Class Roster"),
            DT::DTOutput("students_table"),

            hr(class = "section-divider"),
            h4(icon("layer-group"), " Group Distribution"),
            DT::DTOutput("batch_table"),

            hr(class = "section-divider"),
            h4(icon("user-check"), " Manual Attendance"),
            div(class = "small-note",
              "Select students below and mark them present if camera is unavailable."),
            br(),
            DT::DTOutput("manual_attendance_table"),
            br(),
            actionButton("mark_manual_attendance_btn",
                         tags$span(icon("check"), " Mark Selected Present"),
                         class = "btn-success"),
            br(), br(),
            textOutput("manual_attendance_msg")
          )
        )
      ),

      # ══════════════════════════════════════════════
      # EMOTION DETECTOR
      # ══════════════════════════════════════════════
      shinydashboard::tabItem(tabName = "emotion_detector",
        fluidRow(
          column(width = 12,
            div(class = "pro-hero",
              h3(icon("smile"), " Live Emotion Detector"),
              p("Real-time emotion summary for the active session.")
            )
          ),
          shinydashboard::box(
            width = 5, status = "warning",
            title = tags$span(icon("table"), " Student Emotion Status"),
            textOutput("emotion_detector_status"),
            br(),
            DT::DTOutput("emotion_live_table")
          ),
          shinydashboard::box(
            width = 7, status = "info",
            title = tags$span(icon("chart-bar"), " Live Distribution"),
            plotOutput("emotion_live_plot", height = "320px")
          )
        )
      ),

      # ══════════════════════════════════════════════
      # ANALYTICS
      # ══════════════════════════════════════════════
      shinydashboard::tabItem(tabName = "analytics",
        fluidRow(
          column(width = 12,
            div(class = "pro-hero",
              h3(icon("chart-line"), " Analytics Dashboard"),
              p("Engagement trends, emotion patterns, and weekly progress across all sessions.")
            )
          ),

          # Summary info boxes
          shinydashboard::infoBoxOutput("info_total_sessions", width = 3),
          shinydashboard::infoBoxOutput("info_avg_attendance", width = 3),
          shinydashboard::infoBoxOutput("info_avg_engagement", width = 3),
          shinydashboard::infoBoxOutput("info_top_emotion",    width = 3),

          # Charts row 1
          shinydashboard::box(
            width = 6, status = "primary",
            title = tags$span(icon("chart-bar"), " Attendance Per Session"),
            plotOutput("attendance_plot", height = "260px")
          ),
          shinydashboard::box(
            width = 6, status = "warning",
            title = tags$span(icon("smile"), " Emotion Distribution"),
            plotOutput("emotion_pie_plot", height = "260px")
          ),

          # Weekly progress
          shinydashboard::box(
            width = 12, status = "info",
            title = tags$span(icon("calendar-week"), " Weekly Attendance (15 Weeks)"),
            plotOutput("weekly_attendance_plot", height = "260px")
          ),

          # Trend line
          shinydashboard::box(
            width = 12, status = "info",
            title = tags$span(icon("chart-line"), " Emotion Trends Over Sessions"),
            plotOutput("emotion_trend_plot", height = "280px")
          ),

          # Engagement
          shinydashboard::box(
            width = 7, status = "success",
            title = tags$span(icon("user-graduate"), " Engagement Score Per Student"),
            plotOutput("engagement_bar_plot", height = "300px")
          ),
          shinydashboard::box(
            width = 5, status = "success",
            title = tags$span(icon("table"), " Engagement Summary"),
            DT::DTOutput("analytics_table")
          ),

          # Clustering + Behavior
          shinydashboard::box(
            width = 6, status = "primary",
            title = tags$span(icon("project-diagram"), " K-Means Clustering"),
            plotOutput("engagement_cluster_plot", height = "280px")
          ),
          shinydashboard::box(
            width = 6, status = "danger",
            title = tags$span(icon("exclamation-triangle"), " Behavior Alerts"),
            plotOutput("behavior_plot", height = "280px")
          )
        )
      ),

      # ══════════════════════════════════════════════
      # STUDENTS OVERVIEW (DOCTOR)
      # ══════════════════════════════════════════════
      shinydashboard::tabItem(tabName = "students_overview",
        fluidRow(
          column(width = 12,
            div(class = "pro-hero",
              h3(icon("users"), " Class Students"),
              p("All students in your course — photo, top emotion, and engagement GPA.")
            )
          ),
          shinydashboard::box(
            width = 12, status = "primary",
            title = tags$span(icon("id-card"), " Student Cards"),
            uiOutput("students_overview_ui")
          )
        )
      ),

      # ══════════════════════════════════════════════
      # MANAGEMENT (ADMIN)
      # ══════════════════════════════════════════════
      shinydashboard::tabItem(tabName = "management",
        fluidRow(
          column(width = 12,
            div(class = "pro-hero",
              h3(icon("cog"), " System Management"),
              p("Add lecturers, courses, and schedule entries.")
            )
          ),
          shinydashboard::box(
            width = 4, status = "primary",
            title = tags$span(icon("user-plus"), " Add Lecturer"),
            textInput("m_doctor_id",       "Doctor ID",   placeholder = "D03"),
            textInput("m_doctor_name",     "Full Name",   placeholder = "Dr. Ahmed Ali"),
            textInput("m_doctor_username", "Username",    placeholder = "ahmed.ali"),
            textInput("m_doctor_password", "Password"),
            textInput("m_doctor_subject",  "Subject",     placeholder = "Data Structures"),
            textInput("m_doctor_class",    "Class ID",    placeholder = "CS101"),
            actionButton("m_add_lecturer_btn",
                         tags$span(icon("plus"), " Add Lecturer"),
                         class = "btn-primary"),
            br(), br(),
            textOutput("m_lecturer_msg")
          ),
          shinydashboard::box(
            width = 4, status = "warning",
            title = tags$span(icon("book"), " Add Course"),
            textInput("m_course_id",    "Course ID",   placeholder = "CS-AI-01"),
            textInput("m_course_name",  "Course Name", placeholder = "Introduction to AI"),
            textInput("m_course_class", "Class ID",    placeholder = "CS101"),
            numericInput("m_course_duration", "Duration (days)", value = 14, min = 1),
            actionButton("m_add_course_btn",
                         tags$span(icon("plus"), " Add Course"),
                         class = "btn-warning"),
            br(), br(),
            textOutput("m_course_msg")
          ),
          shinydashboard::box(
            width = 4, status = "success",
            title = tags$span(icon("calendar-plus"), " Add Schedule"),
            textInput("m_s_doctor_id",  "Doctor ID",   placeholder = "D03"),
            textInput("m_s_lecture_id", "Lecture ID",  placeholder = "L-2026-001"),
            textInput("m_s_course_id",  "Course ID",   placeholder = "CS-AI-01"),
            textInput("m_s_class_id",   "Class ID",    placeholder = "CS101"),
            textInput("m_s_group_id",   "Group ID",    value = "G1"),
            textInput("m_s_day",        "Day",         placeholder = "Monday"),
            textInput("m_s_time",       "Time Slot",   placeholder = "10:00-12:00"),
            textInput("m_s_room",       "Room",        placeholder = "Hall A"),
            numericInput("m_s_duration",     "Duration (days)", value = 1,  min = 1),
            numericInput("m_s_total_weeks",  "Total Weeks",     value = 15, min = 1, max = 52),
            actionButton("m_add_schedule_btn",
                         tags$span(icon("plus"), " Add Schedule"),
                         class = "btn-success"),
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

      # ══════════════════════════════════════════════
      # STUDENT PORTAL
      # ══════════════════════════════════════════════
      shinydashboard::tabItem(tabName = "student_portal",
        fluidRow(
          column(width = 12,
            div(class = "pro-hero",
              h3(icon("user-graduate"), " Student Portal"),
              p("Your attendance history, engagement scores, and emotion records.")
            )
          ),
          shinydashboard::box(
            width = 12, status = "primary",
            title = tags$span(icon("id-card"), " Profile"),
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

      # ══════════════════════════════════════════════
      # PARENT PORTAL
      # ══════════════════════════════════════════════
      shinydashboard::tabItem(tabName = "parent_portal",
        fluidRow(
          column(width = 12,
            div(class = "pro-hero",
              h3(icon("users"), " Parent Portal"),
              p("Monitor your student's attendance and classroom engagement.")
            )
          ),
          shinydashboard::box(
            width = 12, status = "primary",
            title = tags$span(icon("link"), " Linked Student"),
            uiOutput("parent_linked_student_ui")
          ),
          shinydashboard::box(
            width = 6, status = "info",
            title = tags$span(icon("clipboard-check"), " Attendance Record"),
            DT::DTOutput("parent_attendance_table")
          ),
          shinydashboard::box(
            width = 6, status = "warning",
            title = tags$span(icon("brain"), " Engagement Analytics"),
            DT::DTOutput("parent_analytics_table")
          )
        )
      )

    ) # end tabItems
  )   # end dashboardBody
)     # end dashboardPage