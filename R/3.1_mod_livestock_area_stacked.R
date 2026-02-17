# R/3.1_mod_livestock_area_stacked.R
# ---------------------------------------------------------------

mod_livestock_area_stacked_ui <- function(id, height = "520px", full_width = TRUE){
  ns <- NS(id)
  
  div(
    class = if (isTRUE(full_width)) "card full-bleed" else "card",
    div(
      class = "card-body",
      
      h2(class = "card-title", "Area allocated to livestock activity (in million hectares)"),
      
      plotly::plotlyOutput(ns("stack_ls"), height = "420px", width = "100%"),
      
      h2("Total livestock area"),
      div(
        id    = ns("kpi_root"),
        class = "surface-cards",
        div(
          id    = ns("kpi_cards_root"),
          style = "--surf-kpi-h:120px;",
          uiOutput(ns("kpi_cards"))
        )
      ),
      tags$br(),
      
      div(
        class = "text-right",
        div(
          class = "u-actions",
          downloadLink(
            ns("dl_livestock_csv"),
            label = tagList(icon("download"), "CSV")
          )
        )
      ),
      tags$br(),
      
      uiOutput(ns("note"))
    )
  )
}

mod_livestock_area_stacked_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,         # <--- REQUIRED: reactive/function returning scenario CODES (fact$Scenario)
    element_name     = "Area",
    items_keep       = c("Dairy","Beef cattle","Meat sheep and goats"),
    value_multiplier = NULL,
    baseline_label   = "Total use of land for pastures and meadows at base-year"
){
  moduleServer(id, function(input, output, session){
    
    `%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.numeric(a) && !is.finite(a))) b else a
    
    # --- Dépendances "scénarios" (single source of truth) -------------------
    if (is.null(r_scenarios) || !is.function(r_scenarios)) {
      stop("mod_livestock_area_stacked_server(): 'r_scenarios' must be provided as a reactive/function returning scenario CODES.")
    }
    if (!exists("scenario_label", mode = "function", inherits = TRUE)) {
      stop("mod_livestock_area_stacked_server(): missing dependency 'scenario_label(code)'.")
    }
    if (!exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      stop("mod_livestock_area_stacked_server(): missing dependency 'SCENARIO_LEVELS_DEFAULT'.")
    }
    
    scenario_label_vec <- function(x){
      x <- as.character(x)
      vapply(x, scenario_label, character(1))
    }
    
    # --- Display labels for items (ONLY UI) --------------------------------
    ITEM_LABELS_LS_AREA <- c(
      "Dairy"               = "Dairy",
      "Beef cattle"         = "Beef cattle",
      "Meat sheep and goats"= "Small meat ruminant"
    )
    
    item_label <- function(x){
      x <- as.character(x)
      out <- unname(ITEM_LABELS_LS_AREA[x])
      ifelse(is.na(out) | out == "", x, out)
    }
    
    # --- Palette ------------------------------------------------------------
    livestock_colors_for <- function(items){
      if (exists("emissions_animal_colors_for", mode = "function", inherits = TRUE)) {
        unname(emissions_animal_colors_for(items))
      } else if (exists("pal_livestock", mode = "function", inherits = TRUE)) {
        unname(pal_livestock(items))
      } else if (exists("pal_crops", mode = "function", inherits = TRUE)) {
        unname(pal_crops(items))
      } else {
        scales::hue_pal()(length(items))
      }
    }
    
    # --- Scénarios: codes ordonnés via config ------------------------------
    scen_codes_ordered <- reactive({
      sc <- unique(as.character(r_scenarios()))
      sc <- sc[!is.na(sc) & nzchar(sc)]
      
      known   <- intersect(SCENARIO_LEVELS_DEFAULT, sc)
      unknown <- setdiff(sc, SCENARIO_LEVELS_DEFAULT)
      
      c(known, sort(unknown))
    })
    
    scen_levels_all <- reactive({
      sc <- scen_codes_ordered()
      c(SCENARIO_LEVELS_DEFAULT, setdiff(sc, SCENARIO_LEVELS_DEFAULT))
    })
    
    baseline_code <- reactive({
      b <- intersect(SCENARIO_LEVELS_DEFAULT, unique(as.character(r_scenarios())))
      b <- b[!is.na(b) & nzchar(b)]
      b[1] %||% NA_character_
    })
    
    # --- bindCache key (scalaire) ------------------------------------------
    cache_key_base <- reactive({
      req(r_country())
      paste0(
        "ls_area_stacked|",
        r_country(), "|el=", element_name,
        "|sc=", paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    # --- Années par scénario 
    years_by_scenario <- reactive({
      req(r_country())
      sc_req <- scen_codes_ordered()
      validate(need(length(sc_req) > 0, "No scenario selected."))
      
      dat <- fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(stringr::str_trim(Element)) ==
            stringr::str_to_lower(stringr::str_trim(element_name)),
          Scenario %in% sc_req
        )

      b <- baseline_code()
      
      yrs_obs <- dat %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(
          year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          Scenario_code = as.character(Scenario),
          year_used = dplyr::if_else(is.finite(year_used), as.integer(year_used), NA_integer_)
        ) %>%
        dplyr::select(Scenario_code, year_used)
      
      # Grille complète de tous les scénarios demandés
      out <- tibble::tibble(Scenario_code = sc_req) %>%
        dplyr::left_join(yrs_obs, by = "Scenario_code")
      
      # Baseline: si 2018 existe, forcer 2018
      if (!is.na(b) && nzchar(b) && nrow(dat) > 0) {
        has_2018 <- any(dat$Scenario == b & dat$Year == 2018 & !is.na(dat$Value))
        if (isTRUE(has_2018)) out$year_used[out$Scenario_code == b] <- 2018L
      }
      
      out %>%
        dplyr::mutate(
          Scenario_f = factor(Scenario_code, levels = scen_levels_all())
        ) %>%
        dplyr::arrange(Scenario_f)
    }) %>% bindCache(cache_key_base())
    
    
    # --- Données principales élevage ---------------------------------------
    data_ls <- reactive({
      yrs <- years_by_scenario()
      validate(need(nrow(yrs) > 0, "No scenario available for this country/element."))
      
      # Année cible par défaut si year_used est NA (cas scénario absent/0)
      # On prend 2050 (ou le max global observé si tu préfères)
      dat_all_el <- fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(stringr::str_trim(Element)) ==
            stringr::str_to_lower(stringr::str_trim(element_name)),
          Scenario %in% yrs$Scenario_code
        )
      
      # Si on a des années observées quelque part, on peut utiliser le max global comme cible.
      fallback_year <- suppressWarnings(max(dat_all_el$Year[!is.na(dat_all_el$Value)], na.rm = TRUE))
      if (!is.finite(fallback_year)) fallback_year <- 2050L
      
      # (Option baseline déjà traitée dans years_by_scenario; ici on ne fait que remplir les NA)
      yrs2 <- yrs %>%
        dplyr::mutate(
          year_used = dplyr::if_else(is.na(year_used), as.integer(fallback_year), as.integer(year_used))
        )
      
      # Données brutes sur items_keep
      dat0 <- fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(stringr::str_trim(Element)) ==
            stringr::str_to_lower(stringr::str_trim(element_name)),
          Scenario %in% yrs2$Scenario_code,
          Item %in% items_keep
        ) %>%
        dplyr::inner_join(
          dplyr::select(yrs2, Scenario_code, year_used),
          by = c("Scenario" = "Scenario_code")
        ) %>%
        dplyr::filter(Year == year_used)
      
      # Unité + multiplicateur
      unit_vals <- unique(stats::na.omit(dat0$Unit))
      mult_auto <- if (length(unit_vals) && any(grepl("1000", unit_vals, fixed = TRUE))) 1000 else 1
      mult <- if (is.null(value_multiplier)) mult_auto else value_multiplier
      
      # Agrégation observée
      dat_obs <- dat0 %>%
        dplyr::group_by(Scenario, Item, year_used) %>%
        dplyr::summarise(value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(
          Scenario_code = as.character(Scenario),
          value = value * mult,
          year  = year_used
        ) %>%
        dplyr::select(Scenario_code, Item, year, value)
      
      # Grille complète Scenario x Item (permet d'afficher même 0)
      grid <- tidyr::expand_grid(
        Scenario_code = yrs2$Scenario_code,
        Item          = items_keep
      ) %>%
        dplyr::left_join(
          dplyr::select(yrs2, Scenario_code, year_used, Scenario_f),
          by = "Scenario_code"
        ) %>%
        dplyr::left_join(dat_obs, by = c("Scenario_code", "Item")) %>%
        dplyr::mutate(
          year  = dplyr::coalesce(year, year_used),
          value = dplyr::coalesce(value, 0),
          Scenario_f = factor(Scenario_code, levels = scen_levels_all())
        )
      
      # Ordre des Items basé sur la baseline (ou sur total global si baseline absente)
      b <- baseline_code()
      base_order <- grid %>%
        dplyr::group_by(Item) %>%
        dplyr::summarise(
          tot = sum(value[Scenario_code == b], na.rm = TRUE),
          tot_all = sum(value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(rank_val = dplyr::if_else(is.finite(tot) & tot > 0, tot, tot_all)) %>%
        dplyr::arrange(dplyr::desc(rank_val)) %>%
        dplyr::pull(Item)
      
      grid %>%
        dplyr::mutate(
          Item   = factor(Item, levels = base_order),
          scen_i = as.integer(Scenario_f),
          scen_t = scenario_label_vec(Scenario_code)
        ) %>%
        dplyr::arrange(Scenario_f, Item) %>%
        droplevels()
    }) %>% bindCache(cache_key_base())
    
    
    # --- KPI : totaux + deltas vs baseline ---------------------------------
    kpi_livestock <- reactive({
      da <- data_ls()
      validate(need(nrow(da) > 0, "No data available to compute KPIs."))
      
      agg <- da %>%
        group_by(Scenario_code, Scenario_f) %>%
        summarise(value_ha = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(Scenario_f)
      
      b <- baseline_code()
      base_total <- agg$value_ha[agg$Scenario_code == b][1] %||% NA_real_
      
      agg %>%
        mutate(
          diff_pct = if (is.finite(base_total) && base_total > 0) {
            100 * (value_ha - base_total) / base_total
          } else {
            NA_real_
          }
        )
    }) %>% bindCache(cache_key_base())
    
    # --- Graphique principal -----------------------------------------------
    output$stack_ls <- plotly::renderPlotly({
      da <- data_ls()
      req(nrow(da) > 0)
      
      th <- get_plotly_tokens()
      
      scen_codes_used <- levels(da$Scenario_f)
      tick_vals <- seq_along(scen_codes_used)
      tick_text <- scenario_label_vec(scen_codes_used)
      
      cols <- livestock_colors_for(levels(da$Item))
      
      p <- plotly::plot_ly() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title    = "",
            tickmode = "array",
            tickvals = tick_vals,
            ticktext = tick_text
          ),
          yaxis = list(
            title = "ha",
            separatethousands = TRUE
          ),
          legend = list(
            title       = list(text = ""),
            orientation = "h",
            x           = 0.5,
            xanchor     = "center",
            y           = 1.05,
            yanchor     = "bottom"
          ),
          margin = list(t = 20, r = 40)
        )
      
      items_vec <- levels(da$Item)
      if (is.null(items_vec)) items_vec <- unique(as.character(da$Item))
      
      for (it in items_vec) {
        sub <- da %>% filter(Item == it)
        if (nrow(sub) == 0) next
        
        it_lab <- item_label(it)
        
        sub <- sub %>%
          mutate(
            hover_value = if_else(
              is.finite(value),
              format(round(value), big.mark = " ", scientific = FALSE, trim = TRUE),
              "—"
            )
          )
        
        idx    <- match(it, items_vec)
        col_it <- if (!is.na(idx)) unname(cols[idx]) else NULL
        
        p <- plotly::add_bars(
          p,
          data  = sub,
          x     = ~scen_i,
          y     = ~value,
          name  = it_lab,            # <<< display name only
          legendgroup = it_lab,      # <<< display name only
          marker = list(color = col_it),
          text       = ~scen_t,
          textposition = "none",
          customdata = ~hover_value,
          hovertemplate = paste0(
            "%{text}<br>", it_lab, " : %{customdata} ha<extra></extra>"
          )
        )
      }
      
      # Ligne baseline (si baseline présente dans les données utilisées)
      b <- baseline_code()
      base_total <- da %>%
        filter(Scenario_code == b) %>%
        summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
        pull(tot) %||% NA_real_
      
      if (is.finite(base_total)) {
        x_min <- 0.5
        x_max <- length(scen_codes_used) + 0.5
        
        p <- p %>%
          plotly::add_trace(
            x = c(x_min, x_max),
            y = c(base_total, base_total),
            type = "scatter",
            mode = "lines",
            line = list(
              dash  = "dash",
              color = th$baseline_color,
              width = 1.5
            ),
            name        = baseline_label,
            hoverinfo   = "none",
            legendgroup = "baseline_line",
            showlegend  = TRUE
          )
      }
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      p
    }) %>% bindCache(cache_key_base())
    
    # --- Encadrés KPI -------------------------------------------------------
    output$kpi_cards <- renderUI({
      dat <- kpi_livestock()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      
      fmt_num <- function(x){
        ifelse(
          is.finite(x),
          format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE),
          "—"
        )
      }
      fmt_pct <- function(p){
        if (!is.finite(p)) return("—")
        paste0(ifelse(p >= 0, "+", ""), formatC(p, digits = 0, format = "f"), "%")
      }
      
      b <- baseline_code()
      
      cards <- lapply(seq_len(nrow(dat)), function(i){
        sc_code <- as.character(dat$Scenario_code[i])
        sc_lab  <- scenario_label(sc_code)
        val     <- dat$value_ha[i]
        dlt     <- dat$diff_pct[i]
        
        delta_tag <- if (!is.finite(dlt) || identical(sc_code, b)) {
          NULL
        } else if (dlt > 0) {
          span(class = "up", fmt_pct(dlt))
        } else if (dlt < 0) {
          span(class = "down", fmt_pct(dlt))
        } else {
          "0%"
        }
        
        subline_base <- if (identical(sc_code, b)) {
          p(class = "u-sub", htmltools::HTML("&nbsp;"))
        } else {
          p(class = "u-sub", "Vs base year: ", delta_tag)
        }
        
        div(
          class = "u-card u-card--flat u-card--hover",
          div(
            class = "u-box",
            p(class = "u-title", sc_lab),
            p(class = "u-value", fmt_num(val), span(class = "u-unit", "ha")),
            subline_base
          )
        )
      })
      
      div(class = "u-row", do.call(tagList, cards))
    }) %>% bindCache(cache_key_base())
    
    # --- Export CSV ---------------------------------------------------------
    output$dl_livestock_csv <- downloadHandler(
      filename = function(){
        paste0("Livestock_Area_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file){
        da <- data_ls()
        req(nrow(da) > 0)
        
        out <- da %>%
          transmute(
            Country        = r_country(),
            Scenario_code  = Scenario_code,
            Scenario_label = scenario_label_vec(Scenario_code),
            Year           = year,
            Element        = element_name,
            Item           = as.character(Item),
            Value_ha       = value
          )
        
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # --- Note explicative ---------------------------------------------------
    output$note <- renderUI({
      da <- data_ls()
      if (nrow(da) == 0) return(NULL)
      
      htmltools::HTML(glue::glue(
        "<p>
  This chart shows, for the selected country, the <strong>agricultural area used for livestock</strong>
  in the base-year and under the selected scenarios.
  Each stacked bar represents the total area (in hectares) allocated to livestock farming (pastures + meadows),
  broken down into <strong>dairy</strong>, <strong>beef cattle</strong> and <strong>small ruminants meat</strong>.
  </p>
  <p>
  In this module:
  <ul>
    <li><strong>Dairy</strong> : refers to livestock raised primarily for milk production (dairy cows, dairy goats, dairy sheeps and related replacement stock).</li>
    <li><strong>Beef cattle</strong> : refers to bovine livestock raised primarily for meat production (non-dairy cattle categories oriented toward beef production).</li>
    <li><strong>Small ruminants meat</strong> : refers to sheep and goats raised primarily for meat production (small ruminant herds oriented toward meat).</li>
  </ul>
  </p>
  <p>
  The cards below the chart summarise, for each scenario, the total area allocated to livestock farming (in hectares)
  and its percentage change relative to the base-year.
  </p>"
      ))
    })
    
    invisible(NULL)
  })
}
