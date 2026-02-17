# R/2.2_mod_harvested_stacked.R
# ---------------------------------------------------------------

mod_harvested_stacked_ui <- function(id, height = "480px", full_width = TRUE){
  ns <- NS(id)
  div(
    class = if (isTRUE(full_width)) "card full-bleed" else "card",
    div(
      class = "card-body",
      
      h2("Area harvested per crop according to the scenarios (in million hectares)"),
      
      uiOutput(ns("limit_msg")),
      
      plotly::plotlyOutput(ns("stack_harv"), height = "490px", width = "100%"),
      
      h2("Total harvested area"),
      
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
            ns("dl_harvested_csv"),
            label = tagList(icon("download"), "CSV")
          )
        )
      ),
      tags$br(),
      
      uiOutput(ns("note"))
    )
  )
}


mod_harvested_stacked_server <- function(
    id,
    fact,
    r_country,
    r_scenarios = NULL,
    harvest_element = "Area harvested",
    exclude_items = c(
      "All products","All crops","Agricultural land occupation (Farm)",
      "Cropland","Forest land","Land under perm. meadows and pastures"
    ),
    value_multiplier = 1,
    baseline_label = "Total use of land for crops at base-year",
    group_var = NULL,
    plot_theme = c("dark", "light")
){
  plot_theme <- match.arg(plot_theme)
  
  moduleServer(id, function(input, output, session){
    
    # -------- Helpers config-first -----------------------------------------
    sc_norm <- if (exists("scenario_code", mode = "function", inherits = TRUE)) scenario_code else {
      function(x) stringr::str_squish(as.character(x))
    }
    sc_label <- if (exists("scenario_label", mode = "function", inherits = TRUE)) scenario_label else {
      function(x) sc_norm(x)
    }
    
    harvest_colors_for <- function(items){
      if (exists("pal_crops", mode = "function", inherits = TRUE)) {
        pal_crops(items)
      } else {
        scales::hue_pal()(length(items))
      }
    }
    
    rename_harvest_items <- function(df){
      if (!"Item" %in% names(df)) return(df)
      
      old <- "Tea, cocoa, coffee, oilpalm, sugar cane"
      new <- "Perennial plants and stimulants"
      
      if (is.factor(df$Item)) {
        if (old %in% levels(df$Item)) {
          df$Item <- forcats::fct_recode(df$Item, !!new := old)
        }
      } else {
        df$Item <- ifelse(df$Item == old, new, df$Item)
      }
      df
    }
    
    # -----------------------------------------------------------
    # NEW: Item grouping + ordering (legend order fixed)
    # Priority:
    #  1) Config vectors if they exist (strict control)
    #     - CROP_ITEMS_CEREALS
    #     - CROP_ITEMS_PULSES
    #     - CROP_ITEMS_ROOTS_TUBERS
    #     - CROP_ITEMS_OILSEEDS
    #  2) Fallback: name-based heuristics
    # -----------------------------------------------------------
    norm_item <- function(x){
      stringr::str_to_lower(stringr::str_squish(as.character(x)))
    }
    
    get_cfg_vec <- function(name){
      if (exists(name, inherits = TRUE)) get(name, inherits = TRUE) else NULL
    }
    
    CFG_CEREALS      <- get_cfg_vec("CROP_ITEMS_CEREALS")
    CFG_PULSES       <- get_cfg_vec("CROP_ITEMS_PULSES")
    CFG_ROOTS_TUBERS <- get_cfg_vec("CROP_ITEMS_ROOTS_TUBERS")
    CFG_OILSEEDS     <- get_cfg_vec("CROP_ITEMS_OILSEEDS")
    CFG_PERENNIAL_STIM <- get_cfg_vec("CROP_ITEMS_PERENNIAL_STIMULANTS")
    
    if (!is.null(CFG_CEREALS))      CFG_CEREALS      <- norm_item(CFG_CEREALS)
    if (!is.null(CFG_PULSES))       CFG_PULSES       <- norm_item(CFG_PULSES)
    if (!is.null(CFG_ROOTS_TUBERS)) CFG_ROOTS_TUBERS <- norm_item(CFG_ROOTS_TUBERS)
    if (!is.null(CFG_OILSEEDS))     CFG_OILSEEDS     <- norm_item(CFG_OILSEEDS)
    if (!is.null(CFG_PERENNIAL_STIM)) CFG_PERENNIAL_STIM <- norm_item(CFG_PERENNIAL_STIM)
    
    crop_group_from_item <- function(item_chr){
      it <- norm_item(item_chr)
      
      # strict config first
      if (!is.null(CFG_CEREALS)      && it %in% CFG_CEREALS)      return("Cereals")
      if (!is.null(CFG_PULSES)       && it %in% CFG_PULSES)       return("Pulses")
      if (!is.null(CFG_ROOTS_TUBERS) && it %in% CFG_ROOTS_TUBERS) return("Roots and tubers")
      if (!is.null(CFG_OILSEEDS)     && it %in% CFG_OILSEEDS)     return("Oilseeds")
      if (!is.null(CFG_PERENNIAL_STIM) && it %in% CFG_PERENNIAL_STIM) return("Perennial crops and stimulants")
      
      # fallback heuristics (only on label text; does not touch data)
      if (stringr::str_detect(it, "\\bcereal|wheat|maize|corn|rice|barley|sorghum|millet|oat|rye\\b")) return("Cereals")
      if (stringr::str_detect(it, "\\bpulse|bean|peas|pea|lentil|chickpea|cowpea|pigeon\\s*pea\\b"))   return("Pulses")
      if (stringr::str_detect(it, "\\broot|tuber|cassava|yam|potato|sweet\\s*potato\\b"))              return("Roots and tubers")
      if (stringr::str_detect(it, "\\boilseed|oilcrops|soy|soya|groundnut|peanut|rapeseed|canola|sunflower|sesame\\b")) return("Oilseeds")
      if (stringr::str_detect(it, "\\bperennial\\b|stimulant|tea|cocoa|coffee|oilpalm|sugar\\s*cane")) return("Perennial crops and stimulants")
      
      
      "Other"
    }
    
      group_order_levels <- c("Cereals","Pulses","Roots and tubers","Oilseeds","Perennial crops and stimulants","Other")
    
    # -----------------------------------------------------------
    # Noyau commun : scénarios centralisés + helper global
    # -----------------------------------------------------------
    core <- harvested_core(
      fact             = fact,
      r_country         = r_country,
      harvest_element   = harvest_element,
      exclude_items     = exclude_items,
      value_multiplier  = value_multiplier,
      group_var         = group_var,
      r_scenarios       = r_scenarios
    )
    
    scen_base            <- core$scen_base
    scen_ref             <- core$scen_ref
    scen_show            <- core$scen_show
    scen_extra_selected  <- core$scen_extra_selected
    levels_all           <- core$levels_all
    years_by_scenario    <- core$years_by_scenario
    data_harvested       <- core$data_harvested
    constraint_info      <- core$constraint_info
    scen_diets_effective <- core$scen_diets_effective
    
    scen_show_key <- reactive({
      paste(as.character(scen_show()), collapse = "|")
    })
    
    # -----------------------------------------------------------
    # KPI : inchangé
    # -----------------------------------------------------------
    kpi_harvested <- reactive({
      da <- rename_harvest_items(data_harvested())
      
      scen_show_codes <- scen_show()
      req(length(scen_show_codes) > 0)
      
      extra_codes <- if (exists("SCENARIOS_EXTRA_CODES", inherits = TRUE)) {
        as.character(SCENARIOS_EXTRA_CODES)
      } else {
        character(0)
      }
      extra_codes <- intersect(as.character(scen_show_codes), extra_codes)
      
      agg_raw <- da %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(value_ha = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(Scenario = as.character(Scenario))
      
      skeleton <- tibble::tibble(
        Scenario = as.character(scen_show_codes)
      )
      
      agg <- skeleton %>%
        dplyr::left_join(agg_raw, by = "Scenario") %>%
        dplyr::mutate(
          Scenario = factor(Scenario, levels = as.character(scen_show_codes))
        ) %>%
        dplyr::arrange(Scenario)
      
      base_total <- agg %>%
        dplyr::filter(as.character(Scenario) == scen_base) %>%
        dplyr::pull(value_ha)
      base_total <- if (length(base_total)) base_total[1] else NA_real_
      
      ref_total <- agg %>%
        dplyr::filter(as.character(Scenario) == scen_ref) %>%
        dplyr::pull(value_ha)
      ref_total <- if (length(ref_total)) ref_total[1] else NA_real_
      
      agg %>%
        dplyr::mutate(
          diff_pct = dplyr::if_else(
            is.finite(base_total) & base_total > 0 & is.finite(value_ha),
            100 * (value_ha - base_total) / base_total,
            NA_real_
          ),
          diff_ha_ref = dplyr::if_else(
            is.finite(ref_total) & is.finite(value_ha),
            value_ha - ref_total,
            NA_real_
          ),
          Scenario_code  = as.character(Scenario),
          Scenario_label = sc_label(as.character(Scenario)),
          is_extra       = as.character(Scenario) %in% extra_codes
        )
    }) %>% bindCache(r_country(), harvest_element, scen_show_key())
    
    # -----------------------------------------------------------
    # Graphique empilé (Plotly) — MODIFIÉ:
    #  - ordre items imposé
    #  - tooltip enrichi avec Δ% vs base-year (par item)
    # -----------------------------------------------------------
    output$stack_harv <- plotly::renderPlotly({
      da_full <- rename_harvest_items(data_harvested())
      req(nrow(da_full) > 0)
      
      th <- get_plotly_tokens()
      
      # Total baseline (tous items)
      base_total <- da_full %>%
        dplyr::filter(as.character(Scenario) == scen_base) %>%
        dplyr::summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(tot)
      req(length(base_total) == 1, is.finite(base_total))
      
      scen_used <- c(scen_base, scen_diets_effective())
      
      da <- da_full %>%
        dplyr::filter(as.character(Scenario) %in% as.character(scen_used)) %>%
        droplevels()
      req(nrow(da) > 0)
      
      # --- Base-year value per ITEM (for Δ% tooltip)
      base_by_item <- da_full %>%
        dplyr::filter(as.character(Scenario) == scen_base) %>%
        dplyr::group_by(Item) %>%
        dplyr::summarise(base_value = sum(value, na.rm = TRUE), .groups = "drop")
      
      # --- FIX ITEM ORDER (legend/stack)
      items_current <- levels(da$Item)
      if (is.null(items_current)) items_current <- unique(as.character(da$Item))
      
      item_groups <- vapply(items_current, crop_group_from_item, character(1))
      item_groups <- factor(item_groups, levels = group_order_levels)
      
      # --- Cereals with forced internal order
      cereals_items <- items_current[item_groups == "Cereals"]
      pref <- c("Wheat","Maize", "Rice", "Millet and Sorghum", "Other Cereals")
      
      pref <- pref[pref %in% cereals_items]  # keep only existing
      cereals_items <- c(pref, setdiff(cereals_items, pref))
      
      ordered_items <- c(
        cereals_items,
        items_current[item_groups == "Pulses"],
        items_current[item_groups == "Roots and tubers"],
        items_current[item_groups == "Oilseeds"],
        items_current[item_groups == "Perennial crops and stimulants"],
        items_current[item_groups == "Other"]
      )
      
      # apply factor levels
      da <- da %>%
        dplyr::mutate(Item = factor(as.character(Item), levels = ordered_items)) %>%
        droplevels()
      
      # Colors follow ordered levels
      cols <- harvest_colors_for(ordered_items)
      
      scen_lvls_chr   <- levels(da$Scenario)
      tick_vals       <- seq_along(scen_lvls_chr)
      tick_text_label <- vapply(scen_lvls_chr, sc_label, character(1))
      
      p <- plotly::plot_ly() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title    = "",
            type     = "linear",
            tickmode = "array",
            tickvals = tick_vals,
            ticktext = tick_text_label
          ),
          yaxis = list(title = "ha", separatethousands = TRUE),
          legend = list(
            title       = list(text = ""),
            orientation = "h",
            x           = 0.5,
            xanchor     = "center",
            y           = 1.05,
            yanchor     = "bottom"
          ),
          margin = list(t = 90, r = 40)
        )
      

      items_vec <- levels(da$Item)
      if (is.null(items_vec)) items_vec <- unique(as.character(da$Item))
      
      for (it in items_vec) {
        sub <- da %>% dplyr::filter(Item == it)
        if (nrow(sub) == 0) next
        sub <- sub %>%
          dplyr::left_join(base_by_item, by = "Item") %>%
          dplyr::mutate(
            scen_label = sc_label(as.character(Scenario)),
            
            # valeur en ha formatée
            hover_value = dplyr::if_else(
              is.finite(value),
              format(round(value), big.mark = " ", scientific = FALSE, trim = TRUE),
              "—"
            ),
            
            # Δ% vs SCENARIO_BASE_YEAR_CODE (ici: scen_base)
            delta_pct = dplyr::case_when(
              is.finite(base_value) & base_value > 0 & is.finite(value) ~ 100 * (value - base_value) / base_value,
              TRUE ~ NA_real_
            ),
            hover_delta = dplyr::if_else(
              is.finite(delta_pct),
              paste0(ifelse(delta_pct >= 0, "+", ""), formatC(delta_pct, digits = 0, format = "f"), "%"),
              "—"
            ),
            
            # TEXTE COMPLET DU TOOLTIP (2 lignes)
            hover_txt = paste0(
              scen_label, "<br>",
              it, " : ", hover_value, " ha",
              "<br>Change vs base year (", scen_base, "): ", hover_delta
            )
          )
        
        idx    <- match(it, items_vec)
        col_it <- if (!is.na(idx)) unname(cols[idx]) else NULL
        
        p <- plotly::add_bars(
          p, data = sub,
          x = ~scen_i, y = ~value,
          name = it, legendgroup = it,
          marker = list(color = col_it),
          
          # on met TOUT dans text et on force le tooltip à lire text
          text = ~hover_txt,
          textposition = "none",
          hovertemplate = "%{text}<extra></extra>"
        )
      }
      
      x_min <- 0.5
      x_max <- length(scen_lvls_chr) + 0.5
      p <- p %>%
        plotly::add_trace(
          x = c(x_min, x_max),
          y = c(base_total, base_total),
          type  = "scatter",
          mode  = "lines",
          line  = list(dash = "dash", width = 1.5, color = th$baseline_color),
          name        = baseline_label,
          hoverinfo   = "none",
          legendgroup = "baseline_line",
          showlegend  = TRUE
        )
      
      plotly_apply_global_theme(p, bg = "transparent", grid = "y")
    })
    
    # -----------------------------------------------------------
    # Cartes KPI : inchangé
    # -----------------------------------------------------------
    output$kpi_cards <- renderUI({
      dat <- kpi_harvested()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      
      fmt_num <- function(x){
        ifelse(is.finite(x), format(round(x), big.mark=" ", scientific=FALSE, trim=TRUE), "—")
      }
      fmt_pct <- function(p){
        if (!is.finite(p)) return("—")
        paste0(ifelse(p >= 0, "+", ""), formatC(p, digits=0, format="f"), "%")
      }
      fmt_ha_signed <- function(x){
        if (!is.finite(x)) return("—")
        paste0(if (x > 0) "+" else "", format(round(x), big.mark=" ", scientific=FALSE, trim=TRUE))
      }
      
      cards <- lapply(seq_len(nrow(dat)), function(i){
        sc_code  <- dat$Scenario_code[i]
        sc_lbl   <- dat$Scenario_label[i]
        val      <- dat$value_ha[i]
        dlt      <- dat$diff_pct[i]
        dha_ref  <- dat$diff_ha_ref[i]
        is_extra <- isTRUE(dat$is_extra[i])
        
        delta_tag <- if (!is.finite(dlt) || sc_code == scen_base) {
          NULL
        } else if (dlt > 0) {
          span(class="up", fmt_pct(dlt))
        } else if (dlt < 0) {
          span(class="down", fmt_pct(dlt))
        } else {
          "0%"
        }
        
        subline_base <- if (sc_code == scen_base) {
          p(class="u-sub", htmltools::HTML("&nbsp;"))
        } else {
          p(class="u-sub", "Vs base year: ", delta_tag)
        }
        
        subline_ref <- if (is_extra && is.finite(dha_ref)) {
          cls <- if (dha_ref < 0) "down" else if (dha_ref > 0) "up" else ""
          p(
            class = "u-sub",
            "Vs trend diet: ",
            span(class = cls, paste0(fmt_ha_signed(dha_ref), " ha"))
          )
        } else {
          NULL
        }
        
        div(
          class="u-card u-card--flat u-card--hover",
          div(
            class="u-box",
            p(class="u-title", sc_lbl),
            p(class="u-value", fmt_num(val), span(class="u-unit","ha")),
            subline_base,
            subline_ref
          )
        )
      })
      
      div(class="u-row", do.call(tagList, cards))
    })
    
    # -----------------------------------------------------------
    # Export CSV 
    # -----------------------------------------------------------
    output$dl_harvested_csv <- downloadHandler(
      filename = function(){
        paste0("AreaHarvested_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file){
        da <- rename_harvest_items(data_harvested())
        req(nrow(da) > 0)
        
        out <- da %>%
          dplyr::transmute(
            Pays          = r_country(),
            Scenario_code = as.character(Scenario),
            Scenario      = sc_label(as.character(Scenario)),
            Annee         = year,
            Item          = as.character(Item),
            Valeur_ha     = value
          ) %>%
          dplyr::arrange(Scenario_code, Annee, Item)
        
        readr::write_delim(out, file, delim = ";")
      }
    )
    

    # -----------------------------------------------------------
    # Note
    # -----------------------------------------------------------
    output$note <- renderUI({
      da <- data_harvested()
      if (nrow(da) == 0) return(NULL)

      htmltools::HTML(glue::glue(
        "<p>
        This chart shows, for the selected country, the <strong>harvested area</strong> in the
        base year and under the different scenarios.<br>
        Each stacked bar represents the total harvested area in million hectares, broken down
        by crop. The dashed line marks the harvested area in the base year.
        </p>
        <p>
        The cards below the chart summarise, for each scenario displayed, the total harvested area and its
        percentage change relative to the base year.
        </p>"
      ))
    })
    invisible(NULL)
  })
}