# R/1.1_mod_area_stacked.R — Area stacked
# ---------------------------------------------------------------

mod_area_stacked_ui <- function(
    id,
    height = "445px",
    wrap = c("card", "none")
){
  ns <- NS(id)
  wrap <- match.arg(wrap)
  
  content <- tagList(
    h2("Breakdown of agricultural and forest areas by scenario"),
    plotly::plotlyOutput(ns("stack_area"), height = height, width = "100%"),
    
    h2("Total agricultural area (excluding forest)"),
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
        downloadLink(ns("dl_area_csv"), label = tagList(icon("download"), "CSV"))
      )
    ),
    
    uiOutput(ns("note"))
  )
  
  if (wrap == "card") {
    div(class = "u-card u-card--flat u-card--hover", content)
  } else {
    content
  }
}

mod_area_stacked_server <- function(
    id, fact, r_country,
    r_scenarios = shiny::reactive(NULL),
    r_area_item = shiny::reactive(NULL),
    ...
){
  moduleServer(id, function(input, output, session){
    
    # --- Normalisation / labels -------------------------------------------
    sc_norm <- if (exists("scenario_code", mode = "function", inherits = TRUE)) scenario_code else {
      function(x) stringr::str_squish(as.character(x))
    }
    
    sc_label <- if (exists("scenario_label", mode = "function", inherits = TRUE)) scenario_label else {
      function(x) sc_norm(x)
    }
    
    AREA_ELT <- if (exists("AREA_ELEMENT", inherits = TRUE)) AREA_ELEMENT else "Area"
    
    # --- Ordres / références depuis le CONFIG (fallback si absent) ----------
    SCEN_LEVELS_MASTER <- if (exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      sc_norm(SCENARIO_LEVELS_DEFAULT)
    } else {
      sort(unique(sc_norm(fact$Scenario)))
    }
    
    scen_base <- if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
      sc_norm(SCENARIO_BASE_YEAR_CODE)
    } else if (exists("SCENARIO_BASE_YEAR", inherits = TRUE)) {
      sc_norm(SCENARIO_BASE_YEAR)
    } else {
      sc_norm("Année de base")
    }
    
    # Stack order
    ITEMS_ORDER <- c(
      "Cropland",
      "Land under perm. meadows and pastures",
      "Forest land"
    )
    
    # --- Scénarios demandés par le global (CODES) --------------------------
    # Important: on force l’inclusion de la baseline, même si r_scenarios() l’oublie.
    scen_requested <- reactive({
      rs <- NULL
      if (!is.null(r_scenarios)) rs <- r_scenarios()
      rs <- sc_norm(rs %||% character(0))
      
      out <- unique(c(scen_base, rs))
      if (!length(out)) out <- SCEN_LEVELS_MASTER
      out
    })
    
    # Niveaux finaux pour les factors : ordre master + tout scénario demandé hors master
    levels_all <- reactive({
      reqd <- scen_requested()
      extra <- reqd[!(reqd %in% SCEN_LEVELS_MASTER)]
      unique(c(SCEN_LEVELS_MASTER, extra))
    })
    
    # --- Year used per scenario -------------------------------------------
    years_by_scenario <- reactive({
      scen_use <- scen_requested()
      shiny::validate(shiny::need(length(scen_use) > 0, "No scenario available for this country."))
      
      dat <- fact %>%
        dplyr::filter(Region == r_country()) %>%
        dplyr::mutate(
          Scenario = sc_norm(Scenario),
          Element  = sc_norm(Element),
          Item     = sc_norm(Item)
        ) %>%
        dplyr::filter(
          stringr::str_to_lower(Element) == stringr::str_to_lower(sc_norm(AREA_ELT)),
          Scenario %in% sc_norm(scen_use)
        )
      
      shiny::validate(shiny::need(nrow(dat) > 0, "No 'Area' data for this country."))
      
      base_year <- if (any(dat$Scenario == scen_base & dat$Year == 2018 & !is.na(dat$Value))) {
        2018
      } else {
        suppressWarnings(max(dat$Year[dat$Scenario == scen_base & !is.na(dat$Value)], na.rm = TRUE))
      }
      
      others <- dat %>%
        dplyr::filter(Scenario != scen_base) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(
          year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
          .groups   = "drop"
        )
      
      yrs <- dplyr::bind_rows(
        tibble::tibble(Scenario = scen_base, year_used = base_year),
        others
      ) %>%
        dplyr::filter(is.finite(year_used)) %>%
        dplyr::mutate(
          Scenario = factor(sc_norm(as.character(Scenario)), levels = levels_all())
        ) %>%
        dplyr::arrange(Scenario)
      
      yrs
    }) %>% bindCache(r_country(), paste(scen_requested(), collapse = "|"))
    
    # --- Data (Area) -------------------------------------------------------
    data_area <- reactive({
      yrs <- years_by_scenario()
      shiny::validate(shiny::need(nrow(yrs) > 0, "No year available for 'Area'."))
      
      items_target <- sc_norm(c("Forest land", AREA_CHILDREN))
      
      yrs_join <- yrs %>%
        dplyr::transmute(
          Scenario  = sc_norm(as.character(Scenario)),
          year_used = year_used
        )
      
      da <- fact %>%
        dplyr::mutate(
          Scenario = sc_norm(Scenario),
          Element  = sc_norm(Element),
          Item     = sc_norm(Item)
        ) %>%
        dplyr::inner_join(yrs_join, by = "Scenario") %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(Element) == stringr::str_to_lower(sc_norm(AREA_ELT)),
          Year == year_used,
          Item %in% items_target
        ) %>%
        dplyr::mutate(
          Value = dplyr::if_else(Item == "Forest land" & !is.na(Value) & Value < 0, 0, Value)
        ) %>%
        dplyr::group_by(Scenario, Item, year_used) %>%
        dplyr::summarise(value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(
          Scenario = factor(sc_norm(as.character(Scenario)), levels = levels_all()),
          Scenario_label = sc_label(sc_norm(as.character(Scenario))),
          value    = value * 1000,
          Item     = factor(sc_norm(as.character(Item)), levels = ITEMS_ORDER),
          year     = year_used
        ) %>%
        dplyr::arrange(Scenario, Item)
      
      da
    }) %>% bindCache(r_country(), paste(scen_requested(), collapse = "|"))
    
    # Scénarios effectivement présents (dans l’ordre levels_all)
    scen_levels_effective <- reactive({
      da <- data_area()
      if (!nrow(da)) return(character(0))
      scen_chr <- unique(sc_norm(as.character(da$Scenario)))
      levels_all()[levels_all() %in% scen_chr]
    })
    
    # --- Plot --------------------------------------------------------------
    output$stack_area <- plotly::renderPlotly({
      da_full <- data_area()
      req(nrow(da_full) > 0)
      
      th <- get_plotly_tokens()
      
      scen_used <- scen_levels_effective()
      req(length(scen_used) > 0)
      
      da <- da_full %>%
        dplyr::filter(sc_norm(as.character(Scenario)) %in% scen_used) %>%
        dplyr::mutate(
          Scenario = factor(sc_norm(as.character(Scenario)), levels = scen_used),
          Scenario_label = sc_label(sc_norm(as.character(Scenario))),
          Item = factor(sc_norm(as.character(Item)), levels = levels(da_full$Item))
        )
      req(nrow(da) > 0)
      
      base_total <- da %>%
        dplyr::filter(sc_norm(as.character(Scenario)) == scen_base) %>%
        dplyr::summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(tot)
      req(length(base_total) == 1, is.finite(base_total))
      
      cols <- area_colors_for(levels(da$Item))
      
      da <- da %>%
        dplyr::group_by(Scenario) %>%
        dplyr::mutate(
          cum_prev = dplyr::lag(cumsum(value), default = 0),
          covered  = pmin(value, pmax(base_total - cum_prev, 0)),
          excess   = pmax(value - covered, 0)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          hover_covered = dplyr::if_else(is.finite(covered), format(round(covered), big.mark=" ", scientific=FALSE, trim=TRUE), "—"),
          hover_excess  = dplyr::if_else(is.finite(excess),  format(round(excess),  big.mark=" ", scientific=FALSE, trim=TRUE), "—")
        )
      
      scen_lvls_chr <- levels(da$Scenario)
      scen_tick_txt <- vapply(scen_lvls_chr, function(x) sc_label(x), character(1))
      bar_w <- 0.85
      
      ref_line_cols <- c(
        "Cropland" = "rgba(230, 65, 65, 0.95)",
        "Land under perm. meadows and pastures" = "#006400"
      )
      
      p <- plotly::plot_ly() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title    = "",
            type     = "linear",
            tickmode = "array",
            tickvals = seq_along(scen_lvls_chr),
            ticktext = scen_tick_txt
          ),
          yaxis = list(title = "ha", separatethousands = TRUE),
          legend = list(
            title = list(text = ""),
            orientation = "h",
            x = 0, xanchor = "left",
            y = 1.12, yanchor = "bottom"
          ),
          margin = list(t = 15)
        )
      
      # Covered (available within max)
      for (it in levels(da$Item)) {
        sub <- da %>% dplyr::filter(Item == it)
        if (!nrow(sub)) next
        
        p <- plotly::add_bars(
          p, data = sub,
          x = ~match(Scenario, scen_lvls_chr),
          y = ~covered,
          width = bar_w,
          name = it, legendgroup = it,
          marker = list(color = cols[match(it, levels(da$Item))]),
          text = ~Scenario_label,
          textposition = "none",
          customdata = ~hover_covered,
          hovertemplate = paste0("%{text}<br>", it, " : %{customdata} ha<extra></extra>")
        )
      }
      
      # Excess (non-available)
      first_excess <- TRUE
      for (it in levels(da$Item)) {
        sub <- da %>% dplyr::filter(Item == it, excess > 0)
        if (!nrow(sub)) next
        
        col_it <- cols[match(it, levels(da$Item))]
        
        p <- plotly::add_bars(
          p, data = sub,
          x = ~match(Scenario, scen_lvls_chr),
          y = ~excess,
          width = bar_w,
          name = "Non-available land",
          showlegend = first_excess,
          legendgroup = "excess",
          marker = list(
            color = col_it,
            line  = list(color = I(col_it), width = 0.5),
            pattern = list(
              shape    = "/",
              bgcolor  = col_it,
              fgcolor  = "rgba(40,40,40,0.7)",
              solidity = 0.5
            )
          ),
          text = ~Scenario_label,
          textposition = "none",
          customdata = ~hover_excess,
          hovertemplate = paste0("%{text}<br>", it, " — Non-available land : %{customdata} ha<extra></extra>")
        )
        
        first_excess <- FALSE
      }
      
      # Base-year reference levels (cropland + agricultural land)
      ref_items <- c("Cropland", "Land under perm. meadows and pastures")
      
      ref_levels <- da %>%
        dplyr::filter(sc_norm(as.character(Scenario)) == scen_base, Item %in% ref_items) %>%
        dplyr::group_by(Item) %>%
        dplyr::summarise(v = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(Item = factor(sc_norm(as.character(Item)), levels = levels(da$Item))) %>%
        dplyr::arrange(Item) %>%
        dplyr::mutate(
          y = cumsum(v),
          ref_name = dplyr::case_when(
            as.character(Item) == "Cropland" ~ "Base-year level - Cropland",
            TRUE ~ "Base-year level - Agricultural land"
          )
        )
      
      x_base  <- match(scen_base, scen_lvls_chr)
      x_start <- x_base + bar_w/2
      x_end   <- length(scen_lvls_chr) + bar_w/2
      
      if (nrow(ref_levels) > 0 && is.finite(x_start) && is.finite(x_end)) {
        for (i in seq_len(nrow(ref_levels))) {
          it_chr <- as.character(ref_levels$Item[i])
          
          p <- plotly::add_trace(
            p, type="scatter", mode="lines",
            x = c(x_start, x_end),
            y = c(ref_levels$y[i], ref_levels$y[i]),
            name = ref_levels$ref_name[i],
            legendgroup = paste0("base_year_levels_", i),
            showlegend = TRUE,
            hoverinfo = "skip",
            line = list(dash="solid", color = ref_line_cols[[it_chr]] %||% "rgba(60,60,60,0.9)", width=1.4)
          )
        }
      }
      
      # Max available area
      p <- plotly::add_trace(
        p, type="scatter", mode="lines",
        x = c(1 - bar_w/2, length(scen_lvls_chr) + bar_w/2),
        y = c(base_total, base_total),
        name = "maximum available area",
        legendgroup = "baseline",
        hoverinfo = "skip",
        line = list(dash="dash", color = th$baseline_color, width=1.2)
      )
      
      plotly_apply_global_theme(p, bg="transparent", grid="y")
    })
    
    # --- KPI cards ---------------------------------------------------------
    kpi_area <- reactive({
      da <- data_area()
      req(nrow(da) > 0)
      
      agg <- da %>%
        dplyr::filter(as.character(Item) != "Forest land") %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(value_ha = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(
          Scenario = factor(sc_norm(as.character(Scenario)), levels = levels_all()),
          Scenario_label = sc_label(sc_norm(as.character(Scenario)))
        ) %>%
        dplyr::arrange(Scenario)
      
      base_total <- agg %>%
        dplyr::filter(sc_norm(as.character(Scenario)) == scen_base) %>%
        dplyr::pull(value_ha)
      base_total <- if (length(base_total)) base_total[1] else NA_real_
      
      agg %>%
        dplyr::mutate(
          diff_abs = if (is.finite(base_total)) (value_ha - base_total) else NA_real_,
          diff_pct = if (is.finite(base_total) && base_total > 0) 100 * (value_ha - base_total) / base_total else NA_real_
        )
    }) %>% bindCache(r_country(), paste(scen_requested(), collapse = "|"))
    
    # --- OUTPUT - KPI cards ---------------------------------------------------------
    
    output$kpi_cards <- renderUI({
      dat <- kpi_area()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      
      fmt_num <- function(x){
        ifelse(is.finite(x), format(round(x), big.mark=" ", scientific=FALSE, trim=TRUE), "—")
      }
      fmt_pct <- function(p){
        if (!is.finite(p)) return("—")
        paste0(ifelse(p >= 0, "+", ""), formatC(p, digits=0, format="f"), "%")
      }
      fmt_abs <- function(x){
        if (!is.finite(x)) return("—")
        paste0(ifelse(x >= 0, "+", ""), format(round(x), big.mark=" ", scientific=FALSE, trim=TRUE))
      }
      
      cards <- lapply(seq_len(nrow(dat)), function(i){
        sc_code <- as.character(dat$Scenario[i])
        sc_lab  <- as.character(dat$Scenario_label[i])
        val     <- dat$value_ha[i]
        dlt_pct <- dat$diff_pct[i]
        dlt_abs <- dat$diff_abs[i]
        
        delta_pct_tag <- if (!is.finite(dlt_pct) || sc_code == scen_base) {
          NULL
        } else if (dlt_pct > 0) {
          span(class="up", fmt_pct(dlt_pct))
        } else if (dlt_pct < 0) {
          span(class="down", fmt_pct(dlt_pct))
        } else {
          "0%"
        }
        
        abs_class <- if (!is.finite(dlt_abs) || dlt_abs == 0) "" else if (dlt_abs > 0) "up" else "down"
        delta_abs_tag <- if (!is.finite(dlt_abs) || sc_code == scen_base) {
          NULL
        } else {
          span(class = abs_class, fmt_abs(dlt_abs))
        }
        
        subline_pct <- if (sc_code == scen_base) {
          p(class="u-sub", htmltools::HTML("&nbsp;"))
        } else {
          p(class="u-sub u-sub--abs", htmltools::HTML("&Delta;"), "vs base year: ", delta_pct_tag)
        }
        
        subline_abs <- if (sc_code == scen_base) {
          NULL
        } else {
          p(class="u-sub u-sub--abs", htmltools::HTML("&Delta;"), " vs base year: ", delta_abs_tag, " ","ha")
        }
        
        div(
          class="u-card u-card--flat u-card--hover",
          div(
            class="u-box",
            p(class="u-title", sc_lab),
            p(class="u-value", fmt_num(val), span(class="u-unit","ha")),
            subline_pct,
            subline_abs
          )
        )
      })
      
      div(class="u-row", do.call(tagList, cards))
    })
    
    # --- Export ------------------------------------------------------------
    output$dl_area_csv <- downloadHandler(
      filename = function(){ paste0("Area_", gsub(" ", "_", r_country()), ".csv") },
      content = function(file){
        da <- data_area()
        req(nrow(da) > 0)
        out <- da %>%
          dplyr::transmute(
            Pays      = r_country(),
            Scenario  = sc_norm(as.character(Scenario)),
            Scenario_label = Scenario_label,
            Annee     = year,
            Item      = as.character(Item),
            Valeur_ha = value
          ) %>%
          dplyr::arrange(Scenario, Annee, Item)
        readr::write_delim(out, file, delim=";")
      }
    )
    
    # --- Note --------------------------------------------------------------
    output$note <- renderUI({
      da <- data_area()
      if (!nrow(da)) return(NULL)
      
      htmltools::HTML(glue::glue(
        "<p>
        This chart shows, for the selected country, the decomposition of
        <strong>\"available land\"</strong> (cropland + pastures and meadows + forest land) by scenario.
        </p>
        <p>
        The <strong>dashed horizontal line</strong> corresponds to the
        exploitable agricultural area (cropland + pastures and meadows) in the base scenario. The hatched segments labelled
        <em>\"Non-available land\"</em> represent the <strong>additional land</strong> that would
        be required to meet the scenario's land demand beyond this limit.
        </p>
        <p>
        The two thin solid horizontal lines indicate <strong>base-scenario reference levels</strong>
        projected across scenarios: the <em>Cropland</em> and the
        <em>Agricultural land</em> observed in the base scenario.
        </p>
        <p>
        The cards below the chart summarise, for each scenario displayed,
        the <strong>total agricultural area</strong> excluding forest and its
        percentage change compared with the base scenario.
        </p>"
      ))
    })
    
    list(
      scen_levels_effective = scen_levels_effective
    )
  })
}
