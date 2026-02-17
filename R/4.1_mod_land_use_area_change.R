# R/4.1_mod_land_use_area_change.R

mod_land_use_area_change_ui <- function(id, height = "600px"){
  ns <- NS(id)
  tagList(
    div(class = "card",
        div(class = "card-body",
            h2("Land use change (in 1000 ha)"),
            plotly::plotlyOutput(ns("plot"), height = height, width = "100%"),
            h2("Focus on Forest land area (in ha)"),
            tags$br(),
            div(id    = ns("kpi_root"),
              class = "surface-cards",
              div(
                id    = ns("kpi_cards_root"),
                style = "--surf-kpi-h:120px;",
                uiOutput(ns("kpi_cards"))
              )
            ),
            tags$br(),
            div(class = "u-actions",
              downloadLink(ns("dl_csv"),label = tagList(icon("download"), "CSV")),
            ),
            uiOutput(ns("note"))
        )
    )
  )
}

mod_land_use_area_change_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,
    scen_show_key = NULL,
    BASE_YEAR   = 2018,
    TARGET_YEAR = 2050,
    AREA_ELEMENT  = "Area",
    CROPLAND_ITEM = "Cropland",
    PASTURE_ITEM  = "Land under perm. meadows and pastures",
    FOREST_ITEM   = "Forest land"
){
  moduleServer(id, function(input, output, session){
    
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    shiny::validate(
      shiny::need(exists("area_colors_for", mode = "function"), "Missing area_colors_for() (palette not sourced)."),
      shiny::need(exists("scenario_code", mode = "function"), "Missing scenario_code()."),
      shiny::need(exists("scenario_label", mode = "function"), "Missing scenario_label()."),
      shiny::need(exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE), "Missing SCENARIO_LEVELS_DEFAULT."),
      shiny::need(is.function(r_scenarios), "r_scenarios must be provided.")
    )
    
    code_fun  <- scenario_code
    label_fun <- scenario_label
    
    BASELINE_CODE <- code_fun("Année de base")
    
    fmt_label <- function(value, unit){
      if (!is.finite(value) || value == 0) return("")
      sign <- ifelse(value > 0, "+", ifelse(value < 0, "−", ""))
      num  <- scales::comma(
        abs(value),
        accuracy     = 0.1,
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
    
    # ============================================================
    # PART A — Δ land-use (Cropland / Pastures / Forest)
    # ============================================================
    
    scenarios_effective <- reactive({
      req(fact, r_country(), r_scenarios())
      
      df0 <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Element  == AREA_ELEMENT,
          Item %in% c(CROPLAND_ITEM, PASTURE_ITEM, FOREST_ITEM)
        )
      
      scen_in_data <- unique(df0$Scenario)
      
      scen_from_app <- intersect(SCENARIO_LEVELS_DEFAULT, r_scenarios())
      scen_keep     <- scen_from_app[scen_from_app %in% scen_in_data]
      
      scen_keep
    }) %>%
      bindCache(r_country(), scen_set_key())
    
    data_area_raw <- reactive({
      req(fact, r_country(), scenarios_effective())
      
      scen_eff <- scenarios_effective()
      
      if (!(BASELINE_CODE %in% scen_eff)) {
        return(tibble::tibble(Scenario = character(), type = character(), unit = character(), value = numeric()))
      }
      
      fact_cty <- fact %>% dplyr::filter(Region == r_country())
      
      # 1) baseline, BASE_YEAR
      area_base <- fact_cty %>%
        dplyr::filter(
          Element  == AREA_ELEMENT,
          Item %in% c(CROPLAND_ITEM, PASTURE_ITEM, FOREST_ITEM),
          Scenario == BASELINE_CODE,
          Year     == BASE_YEAR
        ) %>%
        dplyr::group_by(Item) %>%
        dplyr::summarise(area_base = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(is.finite(area_base))
      
      # 2) TARGET_YEAR for scenarios shown (excluding baseline)
      scen_future <- setdiff(scen_eff, BASELINE_CODE)
      
      area_2050 <- fact_cty %>%
        dplyr::filter(
          Element  == AREA_ELEMENT,
          Item %in% c(CROPLAND_ITEM, PASTURE_ITEM, FOREST_ITEM),
          Year     == TARGET_YEAR,
          Scenario %in% scen_future
        ) %>%
        dplyr::group_by(Scenario, Item) %>%
        dplyr::summarise(area_2050 = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(is.finite(area_2050))
      
      if (nrow(area_base) == 0L || nrow(area_2050) == 0L) {
        return(tibble::tibble(Scenario = character(), type = character(), unit = character(), value = numeric()))
      }
      
      # 3) Delta = 2050 - base
      # Convention: Area en milliers d'ha -> conversion en ha via * 1000
      area_change <- area_2050 %>%
        dplyr::left_join(area_base, by = "Item") %>%
        dplyr::mutate(
          delta_thousand = area_2050 - area_base,   # en 1 000 ha
          value          = delta_thousand * 1000,   # en ha
          type = dplyr::case_when(
            Item == CROPLAND_ITEM ~ "Cropland",
            Item == PASTURE_ITEM  ~ "Pastures and meadows",
            Item == FOREST_ITEM   ~ "Forest land",
            TRUE ~ as.character(Item)
          ),
          unit = " ha"
        ) %>%
        dplyr::select(Scenario, type, unit, value) %>%
        dplyr::filter(is.finite(value))
      
      area_change
    }) %>%
      bindCache(r_country(), scen_set_key())
    
    data_area <- reactive({
      df <- data_area_raw()
      if (nrow(df) == 0L) return(df)
      
      scen_levels <- scenarios_effective()
      
      df %>%
        dplyr::mutate(
          Scenario = factor(Scenario, levels = scen_levels),
          type     = factor(type, levels = c("Cropland", "Pastures and meadows", "Forest land"))
        ) %>%
        dplyr::filter(!is.na(Scenario), !is.na(type), is.finite(value)) %>%
        dplyr::arrange(Scenario, type)
    }) %>%
      bindCache(r_country(), scen_set_key())
    
    output$plot <- plotly::renderPlotly({
      df <- data_area()
      req(nrow(df) > 0)
      
      # --- tokens R/99 (dark/light) ---
      th <- if (exists("get_plotly_tokens", mode = "function")) {
        get_plotly_tokens()
      } else {
        list(axis_linecolor = "rgba(0,0,0,.18)")
      }
      
      # Formatter sans décimales (k ha)
      fmt_kha0 <- scales::label_number(
        accuracy     = 1,   # aucune décimale
        big.mark     = " ",
        decimal.mark = "."  # sans effet ici mais ok
      )
      
      df <- df %>%
        dplyr::mutate(
          value_plot = value / 1000,  # ha -> k ha
          scen_code  = as.character(Scenario),
          scen_lbl   = vapply(scen_code, label_fun, FUN.VALUE = character(1)),
          
          # labels sur les barres (aucune décimale)
          label = dplyr::case_when(
            value_plot > 0 ~ paste0("+ ", fmt_kha0(abs(value_plot)), " k ha"),
            value_plot < 0 ~ paste0("− ", fmt_kha0(abs(value_plot)), " k ha"),
            TRUE           ~ ""
          ),
          
          # hover (aucune décimale)
          hover = paste0(
            "<b>Scenario:</b> ", scen_lbl, "<br>",
            "<b>Type:</b> ", as.character(type), "<br>",
            "<b>Δ area:</b> ",
            fmt_kha0(value_plot), " k ha"
          ),
          
          # Couleurs via palette globale (mapping label UI -> clé palette)
          type_palette = dplyr::case_when(
            as.character(type) == "Pastures and meadows" ~ "Land under perm. meadows and pastures",
            TRUE ~ as.character(type)  # "Cropland", "Forest land"
          ),
          color = area_colors_for(type_palette)
        ) %>%
        dplyr::filter(is.finite(value_plot), !is.na(Scenario))
      
      req(nrow(df) > 0)
      
      # y-range dynamique
      vals <- df$value_plot[is.finite(df$value_plot)]
      y_range <- NULL
      if (length(vals) > 0) {
        vmin <- min(vals, na.rm = TRUE)
        vmax <- max(vals, na.rm = TRUE)
        marge <- 0.35 * (vmax - vmin)
        if (!is.finite(marge) || marge == 0) marge <- max(1, 0.05 * abs(vmax))
        y_range <- c(vmin - marge, vmax + marge)
      }
      
      scen_levels <- levels(df$Scenario)
      ticktext <- vapply(scen_levels, label_fun, FUN.VALUE = character(1))
      
      p <- plotly::plot_ly()
      
      for (tp in levels(df$type)) {
        dft <- df %>% dplyr::filter(type == tp)
        if (nrow(dft) == 0) next
        
        # IMPORTANT: éviter les vecteurs nommés dans plotly/jsonlite
        col_tp <- unname(unique(dft$color)[1])
        
        p <- p %>% plotly::add_bars(
          data         = dft,
          x            = ~scen_code,
          y            = ~value_plot,
          name         = as.character(tp),
          marker       = list(color = col_tp),
          text         = ~label,
          textposition = "outside",
          textfont     = list(size = 12),
          hovertext    = ~hover,
          hoverinfo    = "text",
          offsetgroup  = paste0("grp_", as.character(tp)),
          cliponaxis   = FALSE
        )
      }
      
      p <- p %>% plotly::layout(
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
          title         = "Land use change (1000 ha)",
          range         = y_range,
          zeroline      = TRUE,
          zerolinecolor = th$axis_linecolor %||% "rgba(0,0,0,.18)",
          showgrid      = TRUE
        ),
        legend = list(
          orientation = "h",
          x = 0.25, xanchor = "left",
          y = 1.12
        ),
        margin = list(l = 60, r = 30, t = 10, b = 60)
      )
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      }
      
      p
    }) %>%
      bindCache(r_country(), scen_set_key(), "labels_int_v1")
    
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0("land_use_area_change_", r_country(), ".csv")
      },
      content = function(file){
        df <- data_area()
        if (is.null(df) || nrow(df) == 0) {
          utils::write.csv(data.frame(), file, row.names = FALSE)
        } else {
          df_export <- df %>%
            dplyr::mutate(
              Scenario = as.character(Scenario),
              Scenario_label = vapply(Scenario, label_fun, FUN.VALUE = character(1)),
              type = as.character(type),
              delta_ha  = value,
              delta_kha = value / 1000
            ) %>%
            dplyr::select(Scenario, Scenario_label, type, delta_kha, delta_ha)
          
          utils::write.csv(df_export, file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      }
    )
    
    output$note <- renderUI({
      req(r_country())
      df <- data_area()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      txt <- glue::glue(
        "<p>
        This chart shows how land use areas change between
        <strong>{BASE_YEAR}</strong> (baseline) and <strong>{TARGET_YEAR}</strong> under the scenarios displayed in the application.
        Values are expressed as a difference relative to the baseline (in <strong>thousand hectares, k ha</strong>).
        </p>
        <p>
        For each scenario, the bars show the change in <strong>cropland</strong>,
        <strong>permanent meadows and pastures</strong>, and <strong>forest land</strong>.
        Positive values indicate an expansion of the corresponding land use, while negative values indicate a reduction in area.
        </p>"
      )
      htmltools::HTML(txt)
    }) %>%
      bindCache(r_country(), scen_set_key())
    
    # ============================================================
    # PART B — Forest KPI cards 
    # ============================================================
    
    # Scénarios effectifs pour la forêt (r_scenarios ∩ données forêt)
    forest_df_country <- reactive({
      req(fact, r_country())
      
      df <- fact %>%
        dplyr::filter(
          .data$Region == r_country(),
          stringr::str_to_lower(.data$Element) == stringr::str_to_lower(AREA_ELEMENT),
          .data$Item == FOREST_ITEM
        ) %>%
        dplyr::mutate(Scenario = code_fun(.data$Scenario))
      
      df
    }) %>% bindCache(r_country())
    
    forest_scen_wanted <- reactive({
      sc <- r_scenarios()
      shiny::validate(shiny::need(!is.null(sc) && length(sc) > 0, "No scenarios provided by r_scenarios()."))
      unique(code_fun(sc))
    })
    
    forest_scen_present <- reactive({
      unique(as.character(forest_df_country()$Scenario))
    }) %>% bindCache(r_country())
    
    forest_scen_show <- reactive({
      wanted  <- forest_scen_wanted()
      present <- forest_scen_present()
      out <- wanted[wanted %in% present]
      shiny::validate(shiny::need(length(out) > 0, "No requested scenario is available for Forest land in this country."))
      out
    }) %>% bindCache(r_country(), scen_set_key())
    
    forest_scen_levels <- reactive({
      show <- forest_scen_show()
      ordered <- SCENARIO_LEVELS_DEFAULT[SCENARIO_LEVELS_DEFAULT %in% show]
      missing <- setdiff(show, ordered)
      c(ordered, missing)
    }) %>% bindCache(r_country(), scen_set_key())
    
    forest_key <- reactive({
      paste(forest_scen_levels(), collapse = "|")
    }) %>% bindCache(r_country(), scen_set_key())
    
    # Year used per scenario (logique identique au module forêt d’origine)
    forest_years_by_scenario <- reactive({
      lvls <- forest_scen_levels()
      shiny::validate(shiny::need(length(lvls) > 0, "No forest scenarios available for this country."))
      
      dat <- forest_df_country() %>% dplyr::filter(.data$Scenario %in% lvls)
      
      base_year <- if (any(
        dat$Scenario == BASELINE_CODE &
        dat$Year == BASE_YEAR &
        !is.na(dat$Value)
      )) {
        BASE_YEAR
      } else {
        suppressWarnings(max(dat$Year[dat$Scenario == BASELINE_CODE & !is.na(dat$Value)], na.rm = TRUE))
      }
      
      others <- dat %>%
        dplyr::filter(.data$Scenario != BASELINE_CODE) %>%
        dplyr::group_by(.data$Scenario) %>%
        dplyr::summarise(
          year_used = suppressWarnings(max(.data$Year[!is.na(.data$Value)], na.rm = TRUE)),
          .groups   = "drop"
        )
      
      dplyr::bind_rows(
        tibble::tibble(Scenario = BASELINE_CODE, year_used = base_year),
        others
      ) %>%
        dplyr::filter(is.finite(.data$year_used)) %>%
        dplyr::mutate(Scenario = factor(as.character(.data$Scenario), levels = lvls)) %>%
        dplyr::arrange(.data$Scenario)
    }) %>% bindCache(r_country(), forest_key())
    
    # Forest data (ha) — Value * 1000
    forest_data_all <- reactive({
      yrs <- forest_years_by_scenario()
      shiny::validate(shiny::need(nrow(yrs) > 0, "No year available for forest land area."))
      
      lvls <- levels(yrs$Scenario) %||% as.character(yrs$Scenario)
      
      out <- forest_df_country() %>%
        dplyr::inner_join(
          yrs %>% dplyr::mutate(Scenario = as.character(.data$Scenario)),
          by = "Scenario"
        ) %>%
        dplyr::filter(.data$Year == .data$year_used) %>%
        dplyr::mutate(
          Scenario = factor(as.character(.data$Scenario), levels = lvls),
          Value    = dplyr::if_else(!is.na(.data$Value) & .data$Value < 0, 0, .data$Value)
        ) %>%
        dplyr::group_by(.data$Scenario, .data$year_used) %>%
        dplyr::summarise(value_ha = sum(.data$Value, na.rm = TRUE) * 1000, .groups = "drop") %>%
        dplyr::arrange(.data$Scenario)
      
      shiny::validate(shiny::need(nrow(out) > 0, "No 'Forest land' data for this country."))
      out
    }) %>% bindCache(r_country(), forest_key())
    
    forest_kpi <- reactive({
      df_ha <- forest_data_all()
      req(nrow(df_ha) > 0)
      
      base_ha <- df_ha %>%
        dplyr::filter(as.character(.data$Scenario) == BASELINE_CODE) %>%
        dplyr::pull(.data$value_ha)
      base_ha <- if (length(base_ha)) base_ha[1] else NA_real_
      
      df_ha %>%
        dplyr::mutate(
          diff_ha  = if (is.finite(base_ha)) .data$value_ha - base_ha else NA_real_,
          diff_pct = if (is.finite(base_ha) && base_ha > 0) 100 * (.data$value_ha - base_ha) / base_ha else NA_real_
        )
    }) %>% bindCache(r_country(), forest_key())
    
    output$kpi_cards <- renderUI({
      dat <- forest_kpi()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      
      fmt_ha <- function(x){
        if (!is.finite(x)) return("—")
        format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
      }
      fmt_ha_signed <- function(x){
        if (!is.finite(x)) return("—")
        paste0(if (x > 0) "+" else if (x < 0) "−" else "", fmt_ha(abs(x)))
      }
      fmt_pct <- function(p){
        if (!is.finite(p)) return("—")
        paste0(ifelse(p >= 0, "+", ""), formatC(p, digits = 0, format = "f"), "%")
      }
      
      base_lbl <- label_fun(BASELINE_CODE)
      
      cards <- lapply(seq_len(nrow(dat)), function(i){
        sc_code <- as.character(dat$Scenario[i])
        sc_lbl  <- label_fun(sc_code)
        
        val <- dat$value_ha[i]
        dlt <- dat$diff_pct[i]
        
        subline_pct <- if (identical(sc_code, BASELINE_CODE)) {
          p(class = "u-sub", htmltools::HTML("&nbsp;"))
        } else {
          delta_pct_tag <- if (!is.finite(dlt)) {
            "—"
          } else if (dlt > 0) {
            span(class = "up", fmt_pct(dlt))
          } else if (dlt < 0) {
            span(class = "down", fmt_pct(dlt))
          } else {
            "0%"
          }
          p(class = "u-sub", glue::glue("Vs {base_lbl}: "), delta_pct_tag)
        }
        
        div(
          class = "u-card u-card--flat u-card--hover",
          div(
            class = "u-box",
            p(class = "u-title", sc_lbl),
            p(
              class = "u-value",
              fmt_ha(val),
              span(class = "u-unit", "ha")
            ),
            subline_pct
          )
        )
      })
      
      div(class = "u-row", do.call(tagList, cards))
    }) %>%
      bindCache(r_country(), forest_key())
    
    # --- Note --------------------------------------------------------------
    output$note <- renderUI({
      da <- data_area()
      if (!nrow(da)) return(NULL)
      
      htmltools::HTML(glue::glue(
        "<p>
    This chart shows how land use areas change between
    <strong>base year</strong> and the scenarios displayed in the application. Values are expressed as a difference relative to the baseline.
    </p>
    <p>
    For each scenario, the bars show the change in <strong>cropland</strong>,
    <strong>permanent meadows and pastures</strong>, and <strong>forest land</strong>.
    Positive values indicate an expansion of the corresponding land use, while negative values indicate a reduction in area.
    </p>
    <p>
    The cards below the chart provide a focus on <strong>forest land area</strong> by scenario (in <strong>ha</strong>),
    together with the <strong>absolute change</strong> and <strong>percentage change</strong> compared with the base year.
    </p>"
      ))
    }) %>%
      bindCache(r_country(), scen_set_key())
  })
}