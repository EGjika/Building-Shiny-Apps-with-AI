# =========================
# libraries
# =========================
library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(stringr)

# =========================
# synthetic data creation
# =========================
set.seed(123)

owners <- c("Daniela Shannon", "Carmela McSmith", "Eric Tuckerman", "Jeffrey Jones", "John Smith", "Raphael Daugherty")
teams <- c("North", "South", "East", "West")
sources <- c("Website", "Prospecting", "Events", "Purchased lead", "Referral")
reasons <- c("Price", "Solution", "Performance", "Personality", "Timing")
statuses <- c("Won", "Open", "Lost", "In Progress")
activity_types <- c("Call", "Email", "Meeting", "Demo", "Proposal")

owner_tbl <- tibble(
  owner = owners,
  team = sample(teams, length(owners), replace = TRUE),
  yearly_target = round(runif(length(owners), 1800000, 3200000), -3)
)

dates <- seq.Date(from = as.Date("2024-01-01"), to = as.Date("2025-12-31"), by = "day")

n_deals <- 2200
sales_tbl <- tibble(
  deal_id = sprintf("DL-%04d", 1:n_deals),
  date = sample(dates, n_deals, replace = TRUE),
  owner = sample(owners, n_deals, replace = TRUE),
  source = sample(sources, n_deals, replace = TRUE, prob = c(0.38, 0.24, 0.12, 0.08, 0.18)),
  reason = sample(reasons, n_deals, replace = TRUE, prob = c(0.45, 0.27, 0.14, 0.08, 0.06)),
  status = sample(statuses, n_deals, replace = TRUE, prob = c(0.43, 0.22, 0.14, 0.21)),
  sold_value = round(rlnorm(n_deals, log(17000), 0.8), 0),
  target_value = round(rlnorm(n_deals, log(15000), 0.7), 0),
  forecast_value = round(rlnorm(n_deals, log(18500), 0.75), 0)
) |>
  left_join(owner_tbl, by = "owner") |>
  mutate(
    month = month(date, label = TRUE, abbr = TRUE),
    month_num = month(date),
    quarter = paste0("Q", quarter(date)),
    year = year(date)
  )

# Requests table linked to owner/team dynamics
n_req <- 420
requests_tbl <- tibble(
  request_id = sprintf("RQ-%04d", 1:n_req),
  request_date = sample(dates, n_req, replace = TRUE),
  owner = sample(owners, n_req, replace = TRUE),
  status = sample(c("Open", "Pending", "Escalated", "Closed"), n_req, replace = TRUE, prob = c(0.42, 0.22, 0.09, 0.27)),
  amount = round(runif(n_req, 90000, 420000), -3)
) |>
  left_join(owner_tbl, by = "owner") |>
  mutate(
    month = month(request_date, label = TRUE, abbr = TRUE),
    quarter = paste0("Q", quarter(request_date)),
    year = year(request_date)
  )

# Activities table
n_activities <- 1800
activities_tbl <- tibble(
  activity_id = sprintf("AC-%05d", 1:n_activities),
  activity_date = sample(dates, n_activities, replace = TRUE),
  owner = sample(owners, n_activities, replace = TRUE),
  activity_type = sample(activity_types, n_activities, replace = TRUE),
  status = sample(c("Completed", "Planned", "Overdue"), n_activities, replace = TRUE, prob = c(0.58, 0.27, 0.15))
) |>
  left_join(owner_tbl, by = "owner") |>
  mutate(
    month = month(activity_date, label = TRUE, abbr = TRUE),
    quarter = paste0("Q", quarter(activity_date)),
    year = year(activity_date)
  )

# Keep month ordering stable
month_levels <- month.abb
sales_tbl$month <- factor(as.character(sales_tbl$month), levels = month_levels)
requests_tbl$month <- factor(as.character(requests_tbl$month), levels = month_levels)
activities_tbl$month <- factor(as.character(activities_tbl$month), levels = month_levels)

# =========================
# helper functions
# =========================
fmt_eur <- function(x) {
  paste0(comma(round(x, 0), big.mark = ","), " EUR")
}

kpi_value <- function(value, suffix = "") {
  ifelse(abs(value) >= 1e6,
         paste0(round(value / 1e6, 2), "M", suffix),
         ifelse(abs(value) >= 1e3,
                paste0(round(value / 1e3, 1), "K", suffix),
                paste0(round(value, 0), suffix)))
}

# =========================
# UI
# =========================
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(" 
      :root {
        --bg: #eef1f2;
        --card: #f8fafb;
        --teal: #0f6c66;
        --teal-2: #38b59a;
        --mint: #9cd4dd;
        --orange: #f47c3c;
        --text: #1f2937;
        --muted: #6b7280;
      }
      body { background: var(--bg); font-family: 'Segoe UI', Roboto, sans-serif; color: var(--text); }
      .app-wrap { display: flex; min-height: 100vh; }
      .sidebar {
        width: 72px; background: linear-gradient(180deg, #0f6c66 0%, #115f5a 100%);
        border-radius: 0 14px 14px 0; color: #fff; padding: 14px 10px; flex-shrink: 0;
        position: sticky; top: 0; height: 100vh;
      }
      .side-logo { font-size: 28px; text-align: center; margin-bottom: 16px; }
      .side-icon { text-align: center; font-size: 20px; margin: 12px 0; opacity: 0.9; }
      .main { flex: 1; padding: 16px; }
      .topbar {
        background: #ffffff; border-radius: 16px; padding: 12px 16px; display: flex;
        align-items: center; justify-content: space-between; box-shadow: 0 6px 14px rgba(15,108,102,0.08);
      }
      .top-search { background: #f3f4f6; border-radius: 30px; padding: 8px 14px; width: min(520px, 58vw); }
      .top-search input { border: none; background: transparent; width: 100%; outline: none; }
      .header-card, .dashboard-card, .kpi-card {
        background: var(--card); border-radius: 18px; box-shadow: 0 5px 12px rgba(17,24,39,0.06);
      }
      .header-card { margin-top: 14px; padding: 14px 18px; }
      .header-title { font-size: 34px; font-weight: 700; color: #0f4a48; margin-bottom: 2px; }
      .header-sub { color: var(--muted); }
      .filters-row { margin-top: 12px; }
      .dashboard-card { padding: 14px; margin-top: 14px; min-height: 280px; }
      .card-title { font-size: 28px; font-weight: 600; margin-bottom: 8px; }
      .kpi-card { padding: 16px; margin-top: 14px; min-height: 130px; }
      .kpi-primary { background: var(--teal); color: #fff; }
      .kpi-title { font-size: 28px; font-weight: 600; }
      .kpi-value { font-size: 44px; font-weight: 700; margin-top: 8px; }
      .kpi-sub { color: rgba(255,255,255,0.85); }
      .kpi-muted .kpi-sub { color: var(--muted); }
      .request-item { display: flex; justify-content: space-between; border-bottom: 1px solid #e6eaec; padding: 10px 0; }
      .request-item:last-child { border-bottom: none; }
      .small-muted { color: var(--muted); font-size: 12px; }
      .selectize-input { border-radius: 12px !important; border: 1px solid #d7dee2 !important; }
      .plotly.html-widget { height: 100% !important; }
      @media (max-width: 992px) {
        .sidebar { display: none; }
        .main { padding: 10px; }
        .header-title { font-size: 26px; }
      }
    "))
  ),

  div(class = "app-wrap",
      div(class = "sidebar",
          div(class = "side-logo", "🦉"),
          div(class = "side-icon", "🏠"),
          div(class = "side-icon", "📊"),
          div(class = "side-icon", "👤"),
          div(class = "side-icon", "📅"),
          div(class = "side-icon", "💰"),
          div(class = "side-icon", "📌"),
          div(class = "side-icon", "🎯")
      ),
      div(class = "main",
          div(class = "topbar",
              div(style = "font-size:22px;font-weight:700;color:#0f6c66;", "Sales Dashboard"),
              div(class = "top-search", icon("search"), tags$input(type = "text", placeholder = "Search for anything")),
              div(style = "font-size:20px;color:#0f6c66;", "🔔  ☰")
          ),

          div(class = "header-card",
              div(class = "header-title", "Key Sales Figures"),
              div(class = "header-sub", "Interactive sales performance dashboard for teams and owners"),
              div(class = "filters-row",
                  fluidRow(
                    column(2, selectInput("filter_month", "Month", choices = c("All", month_levels), selected = "All")),
                    column(2, selectInput("filter_quarter", "Quarter", choices = c("All", paste0("Q", 1:4)), selected = "All")),
                    column(2, selectInput("filter_owner", "Sales Owner", choices = c("All", owners), selected = "All")),
                    column(2, selectInput("filter_team", "Team/Region", choices = c("All", teams), selected = "All")),
                    column(2, selectInput("filter_source", "Source", choices = c("All", sources), selected = "All")),
                    column(2, selectInput("filter_reason", "Reason", choices = c("All", reasons), selected = "All"))
                  ),
                  fluidRow(
                    column(2, selectInput("filter_status", "Status", choices = c("All", statuses), selected = "All")),
                    column(2, selectInput("filter_year", "Year", choices = c("All", sort(unique(sales_tbl$year))), selected = "All")),
                    column(8)
                  )
              )
          ),

          fluidRow(
            column(3, div(class = "dashboard-card", div(class = "card-title", "Top sales reps against target"), uiOutput("top_reps_ui"))),
            column(3, div(class = "dashboard-card", div(class = "card-title", "Forecast this quarter by owner"), plotlyOutput("forecast_owner_plot", height = "220px"))),
            column(3, div(class = "dashboard-card", div(class = "card-title", "Target achievement gauge"), plotlyOutput("gauge_plot", height = "220px"))),
            column(3,
                   div(class = "kpi-card kpi-primary", div(class = "kpi-title", "Sold by me this month"), textOutput("sold_by_me"), div(class = "kpi-sub", textOutput("sold_target_note"))),
                   div(class = "kpi-card kpi-muted", div(class = "kpi-title", "Activities in sales"), textOutput("activities_kpi"), div(class = "small-muted", textOutput("activities_sub")))
            )
          ),

          fluidRow(
            column(3, div(class = "dashboard-card", div(class = "card-title", "Open requests"), uiOutput("open_requests_ui"))),
            column(3, div(class = "dashboard-card", div(class = "card-title", "Won sales by reason"), plotlyOutput("won_reason_plot", height = "220px"))),
            column(3, div(class = "dashboard-card", div(class = "card-title", "Won sales by source"), plotlyOutput("won_source_plot", height = "220px"))),
            column(3, div(class = "dashboard-card", div(class = "card-title", "Forecast this quarter"), plotlyOutput("forecast_quarter_plot", height = "220px")))
          ),

          div(class = "dashboard-card",
              div(class = "card-title", "Dynamic chart explorer"),
              fluidRow(
                column(4, selectInput("x_var", "X variable", choices = c("sold_value", "target_value", "forecast_value", "owner", "team", "source", "reason", "status", "month"), selected = "month")),
                column(4, selectInput("y_var", "Y variable", choices = c("sold_value", "target_value", "forecast_value", "owner", "team", "source", "reason", "status", "month"), selected = "sold_value")),
                column(4, selectInput("color_var", "Color group (optional)", choices = c("None", "owner", "team", "source", "reason", "status", "quarter"), selected = "team"))
              ),
              plotlyOutput("dynamic_plot", height = "300px")
          )
      )
  )
)

# =========================
# server
# =========================
server <- function(input, output, session) {

  filtered_sales <- reactive({
    dat <- sales_tbl

    if (input$filter_month != "All") dat <- dat |> filter(as.character(month) == input$filter_month)
    if (input$filter_quarter != "All") dat <- dat |> filter(quarter == input$filter_quarter)
    if (input$filter_owner != "All") dat <- dat |> filter(owner == input$filter_owner)
    if (input$filter_team != "All") dat <- dat |> filter(team == input$filter_team)
    if (input$filter_source != "All") dat <- dat |> filter(source == input$filter_source)
    if (input$filter_reason != "All") dat <- dat |> filter(reason == input$filter_reason)
    if (input$filter_status != "All") dat <- dat |> filter(status == input$filter_status)
    if (input$filter_year != "All") dat <- dat |> filter(year == as.integer(input$filter_year))

    dat
  })

  filtered_requests <- reactive({
    dat <- requests_tbl
    if (input$filter_month != "All") dat <- dat |> filter(as.character(month) == input$filter_month)
    if (input$filter_quarter != "All") dat <- dat |> filter(quarter == input$filter_quarter)
    if (input$filter_owner != "All") dat <- dat |> filter(owner == input$filter_owner)
    if (input$filter_team != "All") dat <- dat |> filter(team == input$filter_team)
    if (input$filter_year != "All") dat <- dat |> filter(year == as.integer(input$filter_year))
    dat
  })

  filtered_activities <- reactive({
    dat <- activities_tbl
    if (input$filter_month != "All") dat <- dat |> filter(as.character(month) == input$filter_month)
    if (input$filter_quarter != "All") dat <- dat |> filter(quarter == input$filter_quarter)
    if (input$filter_owner != "All") dat <- dat |> filter(owner == input$filter_owner)
    if (input$filter_team != "All") dat <- dat |> filter(team == input$filter_team)
    if (input$filter_year != "All") dat <- dat |> filter(year == as.integer(input$filter_year))
    dat
  })

  output$top_reps_ui <- renderUI({
    dat <- filtered_sales() |>
      group_by(owner) |>
      summarise(sold = sum(sold_value), target = sum(target_value), .groups = "drop") |>
      mutate(attain = sold / pmax(target, 1)) |>
      arrange(desc(attain)) |>
      slice_head(n = 5)

    if (nrow(dat) == 0) return(tags$div("No data for current filters."))

    tagList(lapply(seq_len(nrow(dat)), function(i) {
      tags$div(style = "display:flex;justify-content:space-between;align-items:center;padding:8px 0;border-bottom:1px solid #e8edef;",
               tags$div(tags$strong(dat$owner[i]), tags$div(class = "small-muted", fmt_eur(dat$sold[i]))),
               tags$div(style = "font-size:30px;color:#0f6c66;font-weight:700;", paste0(round(100 * dat$attain[i]), "%"))
      )
    }))
  })

  output$forecast_owner_plot <- renderPlotly({
    dat <- filtered_sales() |>
      filter(quarter == paste0("Q", lubridate::quarter(Sys.Date())), year == year(Sys.Date())) |>
      group_by(owner) |>
      summarise(forecast = sum(forecast_value), .groups = "drop") |>
      arrange(desc(forecast))

    plot_ly(dat, x = ~owner, y = ~forecast, type = "bar",
            marker = list(color = c("#0f6c66", "#38b59a", "#f47c3c", "#9cd4dd", "#7bb2b8", "#7c8f9a")),
            hovertemplate = "<b>%{x}</b><br>Forecast: %{y:$,.0f}<extra></extra>") |>
      layout(yaxis = list(title = "EUR", tickformat = ",.2s"), xaxis = list(title = ""), margin = list(l = 40, r = 10, b = 50, t = 10), showlegend = FALSE)
  })

  output$gauge_plot <- renderPlotly({
    dat <- filtered_sales()
    sold <- sum(dat$sold_value)
    target <- sum(dat$target_value)
    pct <- ifelse(target > 0, 100 * sold / target, 0)

    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = pct,
      number = list(suffix = "%", font = list(size = 40, color = "#0f6c66")),
      gauge = list(
        axis = list(range = list(0, 150), tickwidth = 1),
        bar = list(color = "#4b5563"),
        steps = list(
          list(range = c(0, 50), color = "#d9d9d9"),
          list(range = c(50, 100), color = "#9cd4dd"),
          list(range = c(100, 150), color = "#38b59a")
        )
      )
    ) |>
      layout(margin = list(l = 15, r = 15, t = 25, b = 5))
  })

  output$sold_by_me <- renderText({
    owner_selected <- ifelse(input$filter_owner == "All", owners[1], input$filter_owner)
    m <- month(Sys.Date(), label = TRUE, abbr = TRUE)
    y <- year(Sys.Date())

    val <- sales_tbl |>
      filter(owner == owner_selected, month == m, year == y) |>
      summarise(v = sum(sold_value), .groups = "drop") |>
      pull(v)

    paste0(kpi_value(val, " EUR"))
  })

  output$sold_target_note <- renderText({
    owner_selected <- ifelse(input$filter_owner == "All", owners[1], input$filter_owner)
    target <- owner_tbl |> filter(owner == owner_selected) |> pull(yearly_target)
    paste0("Target (year): ", kpi_value(target, " EUR"))
  })

  output$activities_kpi <- renderText({
    n <- filtered_activities() |> filter(status == "Overdue") |> nrow()
    as.character(n)
  })

  output$activities_sub <- renderText({
    total <- filtered_activities() |> nrow()
    paste0("Overdue activities • Total tracked: ", total)
  })

  output$open_requests_ui <- renderUI({
    dat <- filtered_requests() |>
      filter(status %in% c("Open", "Pending", "Escalated")) |>
      arrange(desc(request_date)) |>
      slice_head(n = 5)

    if (nrow(dat) == 0) return(tags$div("No open requests for current filters."))

    tagList(lapply(seq_len(nrow(dat)), function(i) {
      tags$div(class = "request-item",
               tags$div(
                 tags$div(style = "font-weight:600;color:#0f6c66;", paste0("🎟 ", dat$request_id[i])),
                 tags$div(class = "small-muted", paste(format(dat$request_date[i], "%d.%m.%Y"), "•", dat$owner[i]))
               ),
               tags$div(style = "font-size:30px;color:#0f6c66;", kpi_value(dat$amount[i], " EUR"))
      )
    }))
  })

  output$won_reason_plot <- renderPlotly({
    dat <- filtered_sales() |>
      filter(status == "Won") |>
      group_by(reason) |>
      summarise(value = sum(sold_value), .groups = "drop")

    plot_ly(dat, labels = ~reason, values = ~value, type = "pie", hole = 0.55,
            marker = list(colors = c("#0f6c66", "#38b59a", "#f47c3c", "#9cd4dd", "#b8d8db")),
            textinfo = "label+percent", hovertemplate = "%{label}<br>%{value:$,.0f}<extra></extra>") |>
      layout(showlegend = TRUE, margin = list(l = 10, r = 10, t = 0, b = 0), legend = list(orientation = "h", y = -0.1))
  })

  output$won_source_plot <- renderPlotly({
    dat <- filtered_sales() |>
      filter(status == "Won") |>
      group_by(source) |>
      summarise(value = sum(sold_value), .groups = "drop")

    plot_ly(dat, labels = ~source, values = ~value, type = "pie", hole = 0.55,
            marker = list(colors = c("#0f6c66", "#38b59a", "#f47c3c", "#9cd4dd", "#7bb2b8")),
            textinfo = "label+percent", hovertemplate = "%{label}<br>%{value:$,.0f}<extra></extra>") |>
      layout(showlegend = TRUE, margin = list(l = 10, r = 10, t = 0, b = 0), legend = list(orientation = "h", y = -0.1))
  })

  output$forecast_quarter_plot <- renderPlotly({
    q_now <- paste0("Q", quarter(Sys.Date()))
    y_now <- year(Sys.Date())

    dat <- sales_tbl |>
      filter(year == y_now, quarter == q_now) |>
      group_by(month) |>
      summarise(forecast = sum(forecast_value), sold = sum(sold_value), .groups = "drop") |>
      arrange(match(as.character(month), month_levels))

    plot_ly(dat, x = ~month, y = ~forecast, type = "bar", name = "Forecast", marker = list(color = "#d1d5db"),
            hovertemplate = "Forecast: %{y:$,.0f}<extra></extra>") |>
      add_trace(y = ~sold, name = "Sold", marker = list(color = "#0f6c66"), hovertemplate = "Sold: %{y:$,.0f}<extra></extra>") |>
      layout(barmode = "overlay", yaxis = list(title = "EUR", tickformat = ",.2s"), xaxis = list(title = ""), margin = list(l = 40, r = 10, b = 40, t = 10))
  })

  output$dynamic_plot <- renderPlotly({
    dat <- filtered_sales()
    req(nrow(dat) > 0)

    x_var <- input$x_var
    y_var <- input$y_var
    color_var <- if (input$color_var == "None") NULL else input$color_var

    x_num <- is.numeric(dat[[x_var]])
    y_num <- is.numeric(dat[[y_var]])

    if (x_num && y_num) {
      p <- plot_ly(dat, x = as.formula(paste0("~", x_var)), y = as.formula(paste0("~", y_var)),
                   type = "scatter", mode = "markers",
                   color = if (!is.null(color_var)) as.formula(paste0("~", color_var)) else NULL,
                   colors = "Set2",
                   marker = list(size = 9, opacity = 0.7),
                   hovertemplate = paste0("", x_var, ": %{x}<br>", y_var, ": %{y}<extra></extra>"))
    } else {
      grouped <- dat |>
        group_by(across(all_of(c(x_var, if (!is.null(color_var)) color_var else NULL)))) |>
        summarise(metric = mean(.data[[if (y_num) y_var else "sold_value"]]), .groups = "drop")

      p <- plot_ly(grouped,
                   x = as.formula(paste0("~", x_var)),
                   y = ~metric,
                   type = "bar",
                   color = if (!is.null(color_var)) as.formula(paste0("~", color_var)) else NULL,
                   colors = "Set2",
                   hovertemplate = "Group: %{x}<br>Metric: %{y:$,.0f}<extra></extra>")
    }

    p |> layout(margin = list(l = 50, r = 20, b = 60, t = 10), yaxis = list(title = y_var), xaxis = list(title = x_var))
  })
}

shinyApp(ui, server)
