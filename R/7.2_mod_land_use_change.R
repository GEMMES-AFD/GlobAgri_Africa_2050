# R/7.2_mod_land_use_change.R
# -------------------------------------------------------------------


# Palette spécifique au module Land use change (émissions uniquement)
LAND_USE_CHANGE_COLORS <- c(
  "LUC_pos" = "#EF4444",  # émissions positives
  "LUC_neg" = "#59A14F"   # émissions négatives
)

mod_land_use_change_ui <- function(id, height = "440px"){
  ns <- NS(id)
  tagList(
    div(
      class = "card",
      div(
        class = "card-body",
        h2("Cumulative emissions from land use change (2018-2050)"),
        plotly::plotlyOutput(ns("plot"), height = height),
        
        div(
          class = "u-actions",
          downloadLink(
            ns("dl_csv"),
            label = tagList(icon("download"), "CSV")
          )
        ),
        
        uiOutput(ns("note"))
      )
    )
  )
}

mod_land_use_change_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,
    scen_show_key = NULL,
    BASE_YEAR   = 2018,
    TARGET_YEAR = 2050,
    LUC_ELEMENT   = "Emissions",
    LUC_ITEM      = "Land use change"
){
  moduleServer(id, function(input, output, session){
    
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    BASELINE_CODE <- scenario_code("Année de base")
    
    # --------- Formatage labels -----------------------------------------
    fmt_label <- function(value, unit){
      if (!is.finite(value) || value == 0) return("")
      sign <- ifelse(value > 0, "+", ifelse(value < 0, "−", ""))
      num  <- scales::comma(
        abs(value),
        accuracy     = 1,
        big.mark     = " ",
        decimal.mark = ","
      )
      paste0(sign, " ", num, unit)
    }
    
    # ---- Scalar key for bindCache (scenario set is a vector) ----
    scen_set_key <- reactive({
      req(r_scenarios())
      if (!is.null(scen_show_key)) {
        k <- scen_show_key()
        req(length(k) == 1)
        return(as.character(k))
      }
      paste(r_scenarios(), collapse = "|")
    })
    
    # ---- Scenarios effective for THIS module: r_scenarios ∩ fact (country/LUC only) ----
    scenarios_effective <- reactive({
      req(fact, r_country(), r_scenarios())
      
      df0 <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Element  == LUC_ELEMENT,
          Item     == LUC_ITEM,
          Year     == TARGET_YEAR
        )
      
      scen_in_data <- unique(df0$Scenario)
      
      scen_from_app <- intersect(SCENARIO_LEVELS_DEFAULT, r_scenarios())
      scen_keep     <- scen_from_app[scen_from_app %in% scen_in_data]
      
      scen_keep
    }) %>%
      bindCache(r_country(), scen_set_key())
    
    # ======================= Données brutes (LUC uniquement) ==============================
    data_luc_raw <- reactive({
      req(fact, r_country(), scenarios_effective())
      
      scen_eff <- scenarios_effective()
      scen_future <- setdiff(scen_eff, BASELINE_CODE)
      
      if (length(scen_future) == 0L) {
        return(tibble::tibble(
          Scenario = character(),
          type     = character(),
          unit     = character(),
          value    = numeric()
        ))
      }
      
      fact %>%
        dplyr::filter(
          Region   == r_country(),
          Element  == LUC_ELEMENT,
          Item     == LUC_ITEM,
          Year     == TARGET_YEAR,
          Scenario %in% scen_future
        ) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(
          value = sum(Value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(is.finite(value)) %>%
        dplyr::mutate(
          type = "Land use change emissions",
          unit = " tCO\u2082e"
        )
    }) %>%
      bindCache(r_country(), scen_set_key())
    
    # ======================= Données prêtes pour affichage =======================
    data_luc <- reactive({
      df <- data_luc_raw()
      if (nrow(df) == 0L) return(df)
      
      scen_levels <- scenarios_effective()
      
      df %>%
        dplyr::mutate(
          Scenario = factor(Scenario, levels = scen_levels)
        ) %>%
        dplyr::filter(!is.na(Scenario), is.finite(value)) %>%
        dplyr::arrange(Scenario)
    }) %>%
      bindCache(r_country(), scen_set_key())
    
    # ======================= Graphique principal (1 seul axe Y) ==========================
    output$plot <- plotly::renderPlotly({
      df <- data_luc()
      req(nrow(df) > 0)
      
      # --- tokens R/99 (dark/light) ---
      th <- if (exists("get_plotly_tokens", mode = "function")) {
        get_plotly_tokens()
      } else {
        list(
          font_color     = "#111827",
          muted_color    = "#6B7280",
          axis_linecolor = "rgba(0,0,0,.18)"
        )
      }
      
      # -> Mt CO2e
      df <- df %>%
        dplyr::mutate(
          scen_code  = as.character(Scenario),
          scen_lbl   = vapply(scen_code, scenario_label, FUN.VALUE = character(1)),
          value_plot = value / 1e6,
          unit_plot  = " Mt CO\u2082e",
          label      = purrr::map2_chr(value_plot, unit_plot, fmt_label),
          hover      = paste0(
            "<b>Scenario:</b> ", scen_lbl, "<br>",
            "<b>Net LUC emissions:</b> ",
            scales::comma(value_plot, big.mark = " ", accuracy = 0.01), " Mt CO\u2082e"
          ),
          bar_col = ifelse(value >= 0,
                           LAND_USE_CHANGE_COLORS[["LUC_pos"]],
                           LAND_USE_CHANGE_COLORS[["LUC_neg"]])
        ) %>%
        dplyr::filter(is.finite(value_plot))
      
      scen_levels <- levels(df$Scenario)
      ticktext <- vapply(scen_levels, scenario_label, FUN.VALUE = character(1))
      
      # Range Y avec marge
      vals <- df$value_plot
      y_min <- min(vals, na.rm = TRUE)
      y_max <- max(vals, na.rm = TRUE)
      marge <- 0.25 * (y_max - y_min)
      if (!is.finite(marge) || marge == 0) marge <- max(0.1, 0.05 * max(abs(c(y_min, y_max))))
      y_range <- c(y_min - marge, y_max + marge)
      
      p <- plotly::plot_ly() %>%
        plotly::add_bars(
          data         = df,
          x            = ~scen_code,
          y            = ~value_plot,
          name         = "Net LUC emissions",
          marker       = list(color = ~bar_col),
          text         = ~label,
          textposition = "outside",
          textfont     = list(size = 12),
          hovertext    = ~hover,
          hoverinfo    = "text",
          cliponaxis   = FALSE
        ) %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(
            title         = "",
            zeroline      = FALSE,
            type          = "category",
            categoryorder = "array",
            categoryarray = scen_levels,
            tickmode      = "array",
            tickvals      = scen_levels,
            ticktext      = ticktext,
            tickfont      = list(size = 13)
          ),
          yaxis = list(
            title         = "Land use change emissions (Mt CO\u2082e)",
            range         = y_range,
            zeroline      = TRUE,
            zerolinecolor = th$axis_linecolor %||% "rgba(0,0,0,.18)",
            showgrid      = TRUE
          ),
          showlegend = FALSE,
          margin = list(l = 60, r = 40, t = 10, b = 50)
        )
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      }
      
      p
    }) %>%
      bindCache(r_country(), scen_set_key())
    
    # ======================= Download CSV =================================
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0("land_use_change_emissions_", r_country(), ".csv")
      },
      content = function(file){
        df <- data_luc()
        if (is.null(df) || nrow(df) == 0) {
          utils::write.csv(data.frame(), file, row.names = FALSE)
        } else {
          df_export <- df %>%
            dplyr::mutate(
              Scenario = as.character(Scenario),
              Scenario_label = vapply(Scenario, scenario_label, FUN.VALUE = character(1)),
              type = as.character(type)
            )
          utils::write.csv(df_export, file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      }
    )
    
    # ============================ Note ====================================
    output$note <- renderUI({
      req(r_country())
      df <- data_luc()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      txt <- glue::glue(
        "<p>
        This chart shows the <strong>net CO\u2082 emissions</strong> associated with <strong>land use change</strong> in
        <strong>{TARGET_YEAR}</strong> for the scenarios displayed in the application.
        Values are expressed in <strong>million tonnes of CO\u2082-equivalent (Mt CO\u2082e)</strong>.
        </p>
        <p>
        When the bar is <span style='color:{LAND_USE_CHANGE_COLORS[['LUC_neg']]};font-weight:bold;'>green</span>,
        land use change leads to a net <strong>removal</strong> of CO\u2082.
        When the bar is <span style='color:{LAND_USE_CHANGE_COLORS[['LUC_pos']]};font-weight:bold;'>red</span>,
        land use change generates net <strong>CO\u2082 emissions</strong>.
        </p>"
      )
      
      htmltools::HTML(txt)
    }) %>%
      bindCache(r_country(), scen_set_key())
  })
}
