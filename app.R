# app.R
# Houston Astros | Player Development Analyst — Technical Assessment


# Install packages
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gt)
library(rmarkdown)


# define paths
DATA_DIR <- "data"
GOALS_CSV   <- file.path(DATA_DIR, "pd_analyst_goals.csv")
EVENTS_CSV  <- file.path(DATA_DIR, "pd_analyst_events.csv")
PITCHES_CSV <- file.path(DATA_DIR, "pd_analyst_pitches.csv")

# func to make sure file path exists before reading the csv
safe_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

goals_raw   <- safe_read(GOALS_CSV)
events_raw  <- safe_read(EVENTS_CSV)
pitches_raw <- safe_read(PITCHES_CSV)

validate_data <- function() {
  req(!is.null(goals_raw), "Missing goals CSV.")
  req(!is.null(events_raw), "Missing events CSV.")
  req(!is.null(pitches_raw), "Missing pitches CSV.")
}

# ---- Assumptions (documented in README too) ----
# Columns:
# goals_raw:   player_id, Primary Goal, Secondary Goal, Tertiary Goal
# events_raw:  player_id, sched_date, Level_code, sched_id, event_id, pa, ab, 
# 1b, 2b, 3b, hr, bb, so, hit_exit_speed, hit_vertical_angle, pitch_type,
# play_code
# pitches_raw: player_id, sched_date, Level_code, sched_id, ab_event_id, 
# balls_before, strikes_before, pitch_id, pitch_type, pitch_result, plate_z, 
# plate_x

# check that all expected columns have been read correctly
normalize_columns <- function(df, mapping) {
  for (nm in names(mapping)) {
    if (!is.null(mapping[[nm]]) && mapping[[nm]] %in% names(df)) {
      names(df)[names(df) == mapping[[nm]]] <- nm
    }
  }
  df
}

goals <- if (!is.null(goals_raw)) normalize_columns(
  goals_raw,
  list(
    player_id       = "player_id",
    first_priority  = "Primary Goal",
    second_priority = "Secondary Goal",
    third_priority  = "Tertiary Goal"
  )
) else NULL


events <- if (!is.null(events_raw)) normalize_columns(
  events_raw,
  list(
    sched_date = "sched_date",
    level_code = "Level_code",
    sched_id = "sched_id",
    event_id = "event_id",
    pa = "pa",
    ab = "ab",
    single = "1b",
    double = "2b",
    triple = "3b",
    hr = "hr",
    bb = "bb",
    so = "so",
    hit_exit_speed = "hit_exit_speed",
    hit_vertical_angle = "hit_vertical_angle",
    pitch_type = "pitch_type",
    play_code = "play_code"
  )
) else NULL


pitches <- if (!is.null(pitches_raw)) normalize_columns(
  pitches_raw,
  list(
    player_id = "player_id",
    sched_date="sched_date", 
    level_code="Level_code",
    sched_id="sched_id",
    ab_event_id="ab_event_id",
    balls_before="balls_before",
    strikes_before="strikes_before",
    pitch_id="pitch_id",
    pitch_type="pitch_type",
    pitch_result="pitch_result",
    plate_z="plate_z",
    plate_x="plate_x"
  )
) else NULL

# ---- Canonical player id across all files ----
# Goals already uses player_id
if (!is.null(goals) && "player_id" %in% names(goals)) {
  goals$player_id <- as.character(goals$player_id)
}

# Events may use pitcher_id
if (!is.null(events)) {
  if ("pitcher_id" %in% names(events)) events$player_id <- events$pitcher_id
  if ("pitcherId" %in% names(events))  events$player_id <- events$pitcherId
  if (!"player_id" %in% names(events)) stop("events needs player_id or pitcher_id column.")
  events$player_id <- as.character(events$player_id)
}

# Pitches may use pticher_id (typo) or pitcher_id
if (!is.null(pitches)) {
  if ("pitcher_id" %in% names(pitches)) pitches$player_id <- pitches$pitcher_id
  if ("pitcherId" %in% names(pitches))  pitches$player_id <- pitches$pitcherId
  if (!"player_id" %in% names(pitches)) stop("pitches needs player_id/pticher_id/pitcher_id column.")
  pitches$player_id <- as.character(pitches$player_id)
}


# Coerce types (mm/dd/yyyy)
if (!is.null(events) && "sched_date" %in% names(events)) {
  events$sched_date <- as.Date(events$sched_date, format = "%m/%d/%Y")
}

if (!is.null(pitches) && "sched_date" %in% names(pitches)) {
  pitches$sched_date <- as.Date(pitches$sched_date, format = "%m/%d/%Y")
}


# ---- KPI calculators ----
# Hitting KPIs from events
compute_hitting_kpis <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble(BB_pct = NA_real_, K_pct = NA_real_, SLG = NA_real_, PA = 0L))
  }
  
  get_num <- function(col, default = 0) {
    if (col %in% names(df)) suppressWarnings(as.numeric(df[[col]])) else rep(default, nrow(df))
  }
  get_sum <- function(col) sum(get_num(col, 0), na.rm = TRUE)
  
  # PA/AB (prefer provided cols)
  PA <- if ("pa" %in% names(df)) get_sum("pa") else nrow(df)
  AB <- if ("ab" %in% names(df)) get_sum("ab") else NA_real_
  
  # BB/K
  BB <- if ("bb" %in% names(df)) get_sum("bb") else 0
  K  <- if ("so" %in% names(df)) get_sum("so") else 0
  
  # hits (supports your renamed single/double/triple)
  h1 <- if ("single" %in% names(df)) get_sum("single") else 0
  h2 <- if ("double" %in% names(df)) get_sum("double") else 0
  h3 <- if ("triple" %in% names(df)) get_sum("triple") else 0
  HR <- if ("hr" %in% names(df)) get_sum("hr") else 0
  
  TB <- 1*h1 + 2*h2 + 3*h3 + 4*HR
  
  # fallback if AB missing
  if (is.na(AB)) AB <- max(PA - BB, 0)
  
  tibble(
    BB_pct = ifelse(PA > 0, BB / PA, NA_real_),
    K_pct  = ifelse(PA > 0, K  / PA, NA_real_),
    SLG    = ifelse(AB > 0, TB / AB, NA_real_),
    PA     = as.integer(PA)
  )
}


# Pitching FPS% from pitches (first pitch of each PA that is a strike)
compute_fps <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble(FPS_pct = NA_real_, nPA = 0L))
  }
  
  df %>%
    mutate(
      is_strike = pitch_result %in% c("called_strike", "swinging_strike", "foul")
    ) %>%
    # first pitch of each plate appearance is the pitch thrown at 0-0
    filter(balls_before == 0, strikes_before == 0) %>%
    group_by(ab_event_id) %>%
    # safety: if multiple 0-0 rows exist, take the earliest pitch_id
    slice_min(order_by = pitch_id, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    summarize(
      FPS_pct = mean(is_strike, na.rm = TRUE),
      nPA = n()
    )
}



# Summarize KPIs, given an individual player and timeframe
summarize_window <- function(pid, start_date, end_date) {
  pid <- as.character(pid)
  
  ev <- events %>%
    filter(.data$player_id == pid,
           .data$sched_date >= start_date,
           .data$sched_date <= end_date)
  
  pt <- pitches %>%
    filter(.data$player_id == pid,
           .data$sched_date >= start_date,
           .data$sched_date <= end_date)
  
  bind_cols(
    compute_hitting_kpis(ev),
    compute_fps(pt)
  )
}

trend_table <- function(pid) {
  pid <- as.character(pid)
  
  ev <- events %>% filter(.data$player_id == pid)
  if (nrow(ev) == 0) return(tibble())
  
  daily <- ev %>%
    group_by(.data$sched_date) %>%
    summarise(
      BB_pct = compute_hitting_kpis(pick(everything()))$BB_pct,
      K_pct  = compute_hitting_kpis(pick(everything()))$K_pct,
      SLG    = compute_hitting_kpis(pick(everything()))$SLG,
      PA     = compute_hitting_kpis(pick(everything()))$PA,
      .groups = "drop"
    )
  
  pt <- pitches %>%
    filter(.data$player_id == pid) %>%
    group_by(.data$sched_date) %>%
    summarise(
      FPS_pct = compute_fps(pick(everything()))$FPS_pct,
      .groups = "drop"
    )
  
  daily %>%
    left_join(pt, by = "sched_date") %>%
    arrange(.data$sched_date)
}



# Time bounds (use overall latest date across both files)
all_dates <- c(events$sched_date, pitches$sched_date)
last_date <- max(all_dates, na.rm = TRUE)
first_date <- min(all_dates, na.rm = TRUE)
recent_start <- if (is.finite(last_date)) last_date - 13 else NA


# Player list
player_ids <- sort(unique(goals$player_id))
if (length(player_ids) == 0) player_ids <- ""



# ---- UI ----
ui <- page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  title = "Player Development Dashboard",
  layout_sidebar(
    sidebar = sidebar(
      selectInput("player", "Player ID", choices = player_ids, selected = player_ids[1]),
      uiOutput("date_info"),
      hr(),
      downloadButton("download_report", "Download Player PDF")
    ),
    navset_tab(
      id = "tabs",
      nav_panel("Player Overview",
                fluidRow(
                  column(3, uiOutput("card_bb")),
                  column(3, uiOutput("card_k")),
                  column(3, uiOutput("card_slg")),
                  column(3, uiOutput("card_fps"))
                ),
                br(),
                h4("Stated Performance Goals"),
                uiOutput("goals_ui"),
                br(),
                h4("Goal Status (Recent vs Full Season)"),
                gt_output("goal_status_gt")
      ),
      nav_panel("Trends View",
                p("Quick daily trend lines; minimal interaction required."),
                selectInput("trend_metric", "Metric", choices = c("BB%","K%","SLG","FPS%"), selected="K%"),
                plotOutput("trend_plot", height = "340px"),
                gt_output("recent_last_outing_gt")
      ),
      nav_panel("Goals View",
                p("Progress toward stated targets."),
                gt_output("goals_table_gt")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  validate_data()
  
  output$date_info <- renderUI({
    if (!is.finite(last_date)) return(NULL)
    tagList(
      strong("Date Windows: "),
      tags$ul(
        tags$li(paste0("Recent: ", format(recent_start, "%b %d, %Y"), " — ", format(last_date, "%b %d, %Y"))),
        tags$li(paste0("Full Season: ", format(first_date, "%b %d, %Y"), " — ", format(last_date, "%b %d, %Y")))
      )
    )
  })
  
  # Reactive summaries
  recent_df <- reactive({
    req(input$player)
    summarize_window(input$player, recent_start, last_date)
  })
  
  season_df <- reactive({
    req(input$player)
    summarize_window(input$player, first_date, last_date)
  })
  
  # KPI cards (Recent vs Season)
  pct_fmt <- function(x) ifelse(is.na(x), "—", paste0(round(100 * x, 1), "%"))
  slg_fmt <- function(x) ifelse(is.na(x), "—", sprintf("%.3f", x))
  
  kpi_card <- function(title, recent_val, season_val) {
    bslib::card(
      bslib::card_header(title),
      div(style = "font-size: 1.6rem; font-weight:700;", recent_val),
      div(style = "color:#666;", paste("Season:", season_val))
    )
  }
  
  output$card_bb <- renderUI({
    req(nrow(recent_df()) == 1, nrow(season_df()) == 1)
    kpi_card("BB%", pct_fmt(recent_df()$BB_pct), pct_fmt(season_df()$BB_pct))
  })
  output$card_k <- renderUI({
    req(nrow(recent_df()) == 1, nrow(season_df()) == 1)
    kpi_card("K%", pct_fmt(recent_df()$K_pct), pct_fmt(season_df()$K_pct))
  })
  output$card_slg <- renderUI({
    req(nrow(recent_df()) == 1, nrow(season_df()) == 1)
    kpi_card("SLG", slg_fmt(recent_df()$SLG), slg_fmt(season_df()$SLG))
  })
  output$card_fps <- renderUI({
    req(nrow(recent_df()) == 1, nrow(season_df()) == 1)
    kpi_card("FPS%", pct_fmt(recent_df()$FPS_pct), pct_fmt(season_df()$FPS_pct))
  })
  
  # Goals UI (Primary/Secondary/Tertiary text)
  output$goals_ui <- renderUI({
    if (is.null(goals)) return(HTML("<em>No goals file loaded.</em>"))
    
    g <- goals %>% filter(player_id == input$player)
    if (nrow(g) == 0) return(HTML("<em>No goals recorded for this player.</em>"))
    
    row <- g[1, ]
    items <- list(
      Primary   = row$first_priority,
      Secondary = row$second_priority,
      Tertiary  = row$third_priority
    )
    
    tags$ul(
      lapply(names(items), function(lbl) {
        val <- items[[lbl]]
        if (is.null(val) || is.na(val) || trimws(val) == "") return(NULL)
        tags$li(HTML(paste0("<strong>", lbl, ":</strong> ", val)))
      })
    )
  })
  
  
  # "Goal Status" section: show KPI comparison table (since goals aren't numeric targets)
  output$goal_status_gt <- render_gt({
    req(nrow(recent_df()) == 1, nrow(season_df()) == 1)
    
    tbl <- tibble(
      Metric = c("BB%", "K%", "SLG", "FPS%"),
      Recent = c(recent_df()$BB_pct, recent_df()$K_pct, recent_df()$SLG, recent_df()$FPS_pct),
      Season = c(season_df()$BB_pct, season_df()$K_pct, season_df()$SLG, season_df()$FPS_pct)
    ) %>%
      mutate(
        Recent = ifelse(grepl("%", Metric), round(100 * Recent, 1), round(Recent, 3)),
        Season = ifelse(grepl("%", Metric), round(100 * Season, 1), round(Season, 3))
      )
    
    gt(tbl) %>%
      tab_header(title = "Recent vs Season KPIs") %>%
      fmt_missing(columns = everything(), missing_text = "—")
  })
  
  # Trends
  daily_trends <- reactive({
    req(input$player)
    trend_table(input$player)
  })
  
  output$trend_plot <- renderPlot({
    df <- daily_trends()
    
    validate(
      need(is.data.frame(df), "Trend data not available."),
      need(nrow(df) > 0, "No trend data for this player."),
      need("sched_date" %in% names(df), "Missing sched_date in trend table.")
    )
    
    metric_col <- switch(input$trend_metric,
                         "BB%"  = "BB_pct",
                         "K%"   = "K_pct",
                         "SLG"  = "SLG",
                         "FPS%" = "FPS_pct")
    
    validate(need(metric_col %in% names(df), paste("Missing", metric_col, "in trend table.")))
    
    x <- as.Date(df$sched_date)
    y <- suppressWarnings(as.numeric(df[[metric_col]]))
    
    validate(need(any(is.finite(y)), "Trend metric is all missing for this player."))
    
    # Optional: show % metrics as percent scale
    if (metric_col %in% c("BB_pct", "K_pct", "FPS_pct")) y <- 100 * y
    
    plot(x, y,
         type = "b",
         xlab = "",
         ylab = input$trend_metric,
         pch = 16)
  })
  
  
  # Most recent outing table (KPI summary for most recent sched_date)
  output$recent_last_outing_gt <- render_gt({
    ev <- events %>% filter(player_id == input$player)
    if (nrow(ev) == 0) return(gt(tibble(Message = "No events.")))
    
    last_gd <- max(ev$sched_date, na.rm = TRUE)
    
    ev_last <- ev %>% filter(sched_date == last_gd)
    pt_last <- pitches %>% filter(player_id == input$player, sched_date == last_gd)
    
    k <- bind_cols(compute_hitting_kpis(ev_last), compute_fps(pt_last))
    
    out_tbl <- tibble(
      Metric = c("PA", "BB%", "K%", "SLG", "FPS%"),
      Value  = c(
        k$PA,
        ifelse(is.na(k$BB_pct), "—", paste0(round(100 * k$BB_pct, 1), "%")),
        ifelse(is.na(k$K_pct),  "—", paste0(round(100 * k$K_pct, 1), "%")),
        ifelse(is.na(k$SLG),    "—", sprintf("%.3f", k$SLG)),
        ifelse(is.na(k$FPS_pct),"—", paste0(round(100 * k$FPS_pct, 1), "%"))
      )
    )
    
    gt(out_tbl) %>%
      tab_header(title = paste("Most Recent Outing —", format(last_gd, "%b %d, %Y")))
  })
  
  
  # Goals view: show the three text goals as a table
  output$goals_table_gt <- render_gt({
    if (is.null(goals)) return(gt(tibble(Message = "No goals file loaded.")))
    
    g <- goals %>% filter(player_id == input$player)
    if (nrow(g) == 0) return(gt(tibble(Message = "No goals recorded.")))
    
    gt(g %>% select(first_priority, second_priority, third_priority)) %>%
      tab_header(title = "Stated Performance Goals")
  })
  
  # PDF report download (passes consistent params to report.Rmd)
  output$download_report <- downloadHandler(
    filename = function() paste0("player_report_", input$player, ".pdf"),
    content = function(file) {
      
      # last outing summary (same KPI table as Trends tab)
      last_out <- {
        ev <- events %>% filter(player_id == input$player)
        if (nrow(ev) == 0) {
          tibble()
        } else {
          last_gd <- max(ev$sched_date, na.rm = TRUE)
          ev_last <- ev %>% filter(sched_date == last_gd)
          pt_last <- pitches %>% filter(player_id == input$player, sched_date == last_gd)
          k <- bind_cols(compute_hitting_kpis(ev_last), compute_fps(pt_last))
          tibble(
            sched_date = last_gd,
            PA = k$PA,
            BB_pct = k$BB_pct,
            K_pct = k$K_pct,
            SLG = k$SLG,
            FPS_pct = k$FPS_pct
          )
        }
      }
      
      rmarkdown::render(
        input = normalizePath("report.Rmd"),
        params = list(
          player_id = input$player,
          recent = recent_df(),
          season = season_df(),
          goals = if (is.null(goals)) tibble() else goals %>% filter(player_id == input$player),
          trends = daily_trends(),
          last_outing = last_out,
          recent_window = c(recent_start, last_date),
          full_window   = c(first_date, last_date)
        ),
        output_file = file,
        quiet = TRUE
      )
    }
  )
}

shinyApp(ui, server)
