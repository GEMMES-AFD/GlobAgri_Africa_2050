# R/7.1_mod_emissions_stacked.R
# ----------------------------------------------------------

mod_emissions_stacked_ui <- function(id,
                                     plot_height = "420px"){
  ns <- NS(id)
  
  tagList(
    div(
      class = "card",
      div(
        class = "card-body",
        
        h2("Agricultural annual GHG emissions by scenario (without land use change)"),
        tags$div(style = "height:8px"),
        
        div(
          class = "u-controls u-controls--inline",
          div(
            style = "display:flex; flex-direction:column; gap:4px;",
            selectInput(
              inputId = ns("gas"),
              label   = "GHG shown :",
              choices = c(
                "Total emissions in CO2 equivalent"   = "total",
                "Methane in CO2 equivalent"           = "ch4",
                "Nitrous oxide in CO2 equivalent"     = "n2o"
              ),
              selected = "total",
              width = "320px"
            )
          )
        ),
        
        tags$div(style = "height:8px"),
        
        plotly::plotlyOutput(ns("plot"), height = plot_height),
        
        tags$div(style = "height:10px"),
        uiOutput(ns("note"))
      )
    )
  )
}


mod_emissions_stacked_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,
    scen_show_key = NULL
){
  moduleServer(id, function(input, output, session){
    
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    # -----------------------------------------------------------------------
    # Dependencies (non-palette)
    # -----------------------------------------------------------------------
    if (!exists("scenario_label", mode = "function", inherits = TRUE)) {
      stop("mod_emissions_stacked_server(): missing dependency 'scenario_label(code)'.")
    }
    if (!exists("scenario_code", mode = "function", inherits = TRUE)) {
      stop("mod_emissions_stacked_server(): missing dependency 'scenario_code(label)'.")
    }
    if (!exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      stop("mod_emissions_stacked_server(): missing dependency 'SCENARIO_LEVELS_DEFAULT'.")
    }
    
    BASELINE_CODE <- scenario_code("Année de base")
    
    # -----------------------------------------------------------------------
    # Local color system (families -> base colors -> shades)
    # -----------------------------------------------------------------------
    
    # Map a raw Group label -> a family key
    source_family_key <- function(lbl){
      x <- tolower(trimws(as.character(lbl)))
      
      # ENERGY family (covers: "Energy", "On-Farm energy use", etc.)
      if (grepl("\\bon[- ]farm\\s+energy\\s+use\\b", x)) return("Energy")
      if (grepl("\\benergy\\b", x))                      return("Energy")
      
      # FERTILIZER APPLIED family (covers: synthetic/manure/residues fertilizer applied)
      if (grepl("synthetic.*fertil", x)) return("Fertilizer_applied")
      if (grepl("manure.*fertil", x))    return("Fertilizer_applied")
      if (grepl("residues.*fertil", x))  return("Fertilizer_applied")
      if (grepl("\\bfertilizer\\b.*appl", x)) return("Fertilizer_applied")
      
      # MANURE MANAGEMENT family
      if (grepl("manure\\s+management", x)) return("Manure_management")
      
      # PASTURE family (excreta left on pasture)
      if (grepl("left\\s+on\\s+pasture", x) || grepl("\\bpasture\\b", x)) return("Pasture")
      
      # CROP RESIDUES family (non fertilizer-applied wording)
      if (grepl("\\bcrop\\s+resid", x)) return("Crop_residues")
      if (grepl("\\bresidues\\b", x) && !grepl("fertil", x)) return("Crop_residues")
      
      # ENTERIC family (for total CO2e items like "Enteric" / "Enteric fermentation")
      if (grepl("\\benteric\\b", x)) return("Enteric")
      
      # OTHER / catch-all
      "Other"
    }
    
    # Base colors per family (chosen to read well on dark background)
    FAMILY_BASE_COL <- c(
      "Enteric"            = "#59A14F", # green
      "Energy"             = "#4E79A7", # blue
      "Fertilizer_applied" = "#F28E2B", # orange
      "Manure_management"  = "#76B7B2", # teal
      "Pasture"            = "#8CD17D", # light green
      "Crop_residues"      = "#E15759", # red
      "Other"              = "#BAB0AC"  # grey
    )
    
    # Create n shades around a base color (muted -> base -> slightly lighter)
    # --- replace make_shades() (no scales::lighten) ------------------------------
    
    mix_with <- function(hex, mix = c("#FFFFFF", "#000000"), w = 0.2){
      # w in [0,1]: 0 => unchanged; 1 => fully "mix"
      w <- max(0, min(1, as.numeric(w)))
      rgb1 <- grDevices::col2rgb(hex)
      rgb2 <- grDevices::col2rgb(mix)
      rgb  <- round((1 - w) * rgb1 + w * rgb2)
      grDevices::rgb(rgb[1,], rgb[2,], rgb[3,], maxColorValue = 255)
    }
    
    make_shades <- function(base_hex, n){
      n <- max(1L, as.integer(n))
      
      # darker + muted, then base, then lighter (by mixing with black/white)
      dark   <- mix_with(base_hex, "#000000", w = 0.35)
      light  <- mix_with(base_hex, "#FFFFFF", w = 0.25)
      
      ramp <- grDevices::colorRampPalette(c(dark, base_hex, light))
      ramp(n)
    }
    
    
    # Build a named vector: names = Group labels, values = hex colors
    colors_for_groups <- function(group_labels){
      labs <- as.character(group_labels)
      fam  <- vapply(labs, source_family_key, character(1))
      
      out <- rep(NA_character_, length(labs))
      names(out) <- labs
      
      for (k in unique(fam)){
        idx <- which(fam == k)
        fam_labels <- labs[idx]
        fam_labels_ord <- sort(unique(fam_labels))
        
        base <- FAMILY_BASE_COL[[k]]
        if (is.null(base) || is.na(base) || !nzchar(base)) base <- "#BAB0AC"
        
        shades <- make_shades(base, length(fam_labels_ord))
        names(shades) <- fam_labels_ord
        
        out[idx] <- shades[fam_labels]
      }
      
      out
    }
    
    # -----------------------------------------------------------------------
    # Helpers
    # -----------------------------------------------------------------------
    is_blank <- function(x) is.na(x) | trimws(x) == ""
    
    clean_emission_item <- function(x){
      dplyr::recode(
        x,
        "Fertilizer application and production and pesticides" =
          "Production and application of fertilizer and pesticides",
        .default = x
      )
    }
    
    gas_choice <- reactive({
      g <- input$gas
      if (is.null(g) || !g %in% c("total","ch4","n2o")) "total" else g
    })
    
    gas_label <- reactive({
      switch(
        gas_choice(),
        "total" = "Total CO2e",
        "ch4"   = "CH4 as CO2e",
        "n2o"   = "N2O as CO2e"
      )
    })
    
    # Extract last parentheses => "Emissions (N2O as CO2eq) (Manure management)" -> "Manure management"
    element_source <- function(x){
      x <- as.character(x)
      m <- stringr::str_match(x, "\\(([^()]*)\\)\\s*$")
      out <- m[, 2]
      out <- trimws(out)
      dplyr::na_if(out, "")
    }
    
    prefer_blank_level <- function(df, col){
      if (!col %in% names(df)) return(df)
      v <- as.character(df[[col]])
      blank <- is_blank(v)
      if (any(blank)) df[blank, , drop = FALSE] else df
    }
    
    element_filter <- function(df, gas){
      if (!"Element" %in% names(df)) return(df[0, , drop = FALSE])
      el <- as.character(df$Element)
      
      if (gas == "total") return(df[el == "Emissions", , drop = FALSE])
      if (gas == "ch4")   return(df[grepl("CH4\\s+as\\s+CO2eq", el, ignore.case = TRUE), , drop = FALSE])
      if (gas == "n2o")   return(df[grepl("N2O\\s+as\\s+CO2eq", el, ignore.case = TRUE), , drop = FALSE])
      
      df[0, , drop = FALSE]
    }
    
    subset_emis_raw <- function(df, gas){
      df <- element_filter(df, gas)
      if (nrow(df) == 0) return(df)
      df <- prefer_blank_level(df, "System")
      df <- prefer_blank_level(df, "Animal")
      df
    }
    
    # ---- Cache key for scenarios
    scen_set_key <- reactive({
      req(r_scenarios())
      if (!is.null(scen_show_key)) {
        k <- scen_show_key()
        req(length(k) == 1)
        return(as.character(k))
      }
      paste(r_scenarios(), collapse = "|")
    })
    
    scenarios_effective <- reactive({
      req(fact, r_country(), r_scenarios())
      
      shiny::validate(
        shiny::need(all(c("Region","Scenario","Element","Item","Value") %in% names(fact)),
                    "Emissions module: required columns are missing.")
      )
      
      scen_from_app <- intersect(SCENARIO_LEVELS_DEFAULT, r_scenarios())
      
      df0 <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Scenario %in% scen_from_app,
          Item     != "Land use change"
        )
      
      df0 <- subset_emis_raw(df0, gas_choice())
      scen_in_data <- unique(df0$Scenario)
      
      scen_from_app[scen_from_app %in% scen_in_data]
    }) %>%
      bindCache(r_country(), scen_set_key(), gas_choice())
    
    # ---- Top stacked bars data (Mt CO2e)
    emissions_data <- reactive({
      req(r_country())
      scen_keep <- scenarios_effective()
      g         <- gas_choice()
      
      shiny::validate(
        shiny::need(length(scen_keep) > 0, "No scenario available for this country / gas selection.")
      )
      
      df0 <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Scenario %in% scen_keep,
          Item     != "Land use change"
        )
      
      df0 <- subset_emis_raw(df0, g)
      
      shiny::validate(
        shiny::need(nrow(df0) > 0,
                    paste0("No data found for selected gas (", gas_label(), ")."))
      )
      
      if (g == "total") {
        df0 <- df0 %>% dplyr::mutate(Item = clean_emission_item(Item))
        dfA <- df0 %>%
          dplyr::group_by(Scenario, Group = Item) %>%
          dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
      } else {
        df0 <- df0 %>% dplyr::mutate(Source = element_source(Element))
        shiny::validate(
          shiny::need(any(!is.na(df0$Source)),
                      "CH4/N2O selection: could not extract emission source from 'Element' (expected a second parenthesis).")
        )
        dfA <- df0 %>%
          dplyr::filter(!is.na(Source)) %>%
          dplyr::group_by(Scenario, Group = Source) %>%
          dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
      }
      
      ordre_scenarios <- scenarios_effective()
      
      group_levels <- dfA %>%
        dplyr::filter(Scenario == BASELINE_CODE) %>%
        dplyr::arrange(dplyr::desc(Value)) %>%
        dplyr::pull(Group) %>%
        as.character()
      if (!length(group_levels)) group_levels <- unique(as.character(dfA$Group))
      
      dfA %>%
        dplyr::mutate(
          Scenario   = factor(Scenario, levels = ordre_scenarios),
          Group      = factor(Group, levels = unique(group_levels)),
          Value_plot = Value / 1e6,          # tonnes -> Mt
          hover = paste0(
            "<b>Scenario:</b> ", vapply(as.character(Scenario), scenario_label, FUN.VALUE = character(1)), "<br>",
            "<b>Source:</b> ", as.character(Group), "<br>",
            "<b>Emissions (", gas_label(), "):</b> ",
            scales::comma(Value_plot, accuracy = 0.01, big.mark = " "),
            " Mt CO\u2082e"
          )
        ) %>%
        dplyr::arrange(Scenario, Group)
    }) %>%
      bindCache(r_country(), scen_set_key(), gas_choice())
    
    # ---- Plot
    output$plot <- plotly::renderPlotly({
      df <- emissions_data()
      req(nrow(df) > 0)
      
      lv <- levels(df$Group)
      if (is.null(lv) || !length(lv)) lv <- unique(as.character(df$Group))
      
      # local palette (named vector, keys = Group labels)
      pal <- colors_for_groups(lv)
      
      scen_codes <- levels(df$Scenario)
      scen_ticks <- vapply(scen_codes, scenario_label, FUN.VALUE = character(1))
      
      p <- plotly::plot_ly()
      
      for (grp in lv) {
        dfg <- df %>% dplyr::filter(as.character(Group) == grp)
        if (nrow(dfg) == 0) next
        
        p <- p %>%
          plotly::add_trace(
            data = dfg,
            x    = ~as.character(Scenario),
            y    = ~Value_plot,
            type = "bar",
            name = grp,
            marker = list(color = unname(pal[grp])),
            text   = ~hover,
            textposition = "none",
            hoverinfo = "text",
            showlegend = TRUE,
            inherit = FALSE
          )
      }
      
      # Totals (Mt) + % change vs baseline
      df_tot <- df %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(Total = sum(Value_plot, na.rm = TRUE), .groups = "drop")
      
      base_total <- df_tot %>%
        dplyr::filter(as.character(Scenario) == BASELINE_CODE) %>%
        dplyr::pull(Total)
      
      if (length(base_total) == 0 || is.na(base_total) || base_total == 0) {
        df_tot <- df_tot %>% dplyr::mutate(label = scales::comma(Total, accuracy = 0.01, big.mark = " "))
      } else {
        df_tot <- df_tot %>%
          dplyr::mutate(
            pct_change = dplyr::if_else(as.character(Scenario) == BASELINE_CODE, NA_real_, 100 * (Total / base_total - 1)),
            sign_symbol = dplyr::case_when(is.na(pct_change) ~ "", pct_change > 0 ~ "+", TRUE ~ ""),
            pct_str = dplyr::case_when(is.na(pct_change) ~ "", TRUE ~ paste0(" (", sign_symbol, sprintf("%.0f", pct_change), "%)")),
            label = paste0(scales::comma(Total, accuracy = 0.01, big.mark = " "), pct_str)
          )
      }
      
      p <- p %>%
        plotly::layout(
          barmode = "relative",
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0,
            xanchor = "left",
            y = 1.17,
            title = list(text = "")
          ),
          yaxis = list(
            title = "Emissions (Mt CO\u2082e)",
            rangemode = "tozero",
            tickformat = ",.2f",
            separatethousands = TRUE,
            zeroline = TRUE
          ),
          xaxis = list(
            title = "",
            type = "category",
            categoryorder = "array",
            categoryarray = scen_codes,
            tickmode = "array",
            tickvals = scen_codes,
            ticktext = scen_ticks,
            tickfont = list(size = 13)
          )
        ) %>%
        plotly::add_trace(
          data = df_tot %>% dplyr::mutate(Scenario = as.character(Scenario)),
          x    = ~Scenario,
          y    = ~Total,
          type = "scatter",
          mode = "text",
          text = ~label,
          textposition = "top center",
          textfont = list(size = 13),
          hoverinfo = "none",
          showlegend = FALSE,
          inherit = FALSE
        )
      
      # If you have a global theme function, keep it; it should not be required for colors anymore.
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      } else if (exists("plotly_theme_transparent", mode = "function")) {
        p <- plotly_theme_transparent(p)
      }
      
      # Force colors after theme (robust)
      for (i in seq_along(p$x$data)) {
        nm <- p$x$data[[i]]$name
        if (!is.null(nm) && nzchar(nm) && nm %in% names(pal)) {
          p$x$data[[i]]$marker$color <- unname(pal[nm])
        }
      }
      
      p
    }) %>%
      bindCache(r_country(), scen_set_key(), gas_choice())
    
    # ---- Note
    output$note <- renderUI({
      req(r_country())
      df <- emissions_data()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      g <- gas_choice()
      baseline_lab <- scenario_label(BASELINE_CODE)
      
      breakdown_txt <- if (g == "total") {
        "<strong>Emission sources</strong> correspond to <strong>Item</strong> (FAO inventory categories) for total CO\u2082e."
      } else {
        "For CH4/N2O, emission sources correspond to the <strong>last parenthesis</strong> in the <strong>Element</strong> field
         (e.g., “Emissions (N2O as CO2eq) (Manure management)” → <em>Manure management</em>)."
      }
      
      txt <- glue::glue(
        "<p>
        This chart shows <strong>agricultural greenhouse gas emissions</strong> (excluding <strong>land use change</strong>)
        for the scenarios displayed in the application, for <strong>{r_country()}</strong>.
        Values are expressed in <strong>million tonnes of CO\u2082-equivalent (Mt CO\u2082e)</strong>.
        </p>
        <p>
        Each bar is a <strong>stacked decomposition</strong> by emission <strong>source</strong>.
        The value displayed above each bar is the <strong>total</strong> (sum of all stacked components);
        when shown, the percentage in parentheses is the change relative to the <strong>{baseline_lab}</strong> scenario.
        </p>
        <p>
        <strong>Colors:</strong> sources that refer to the same topic are shown as <strong>shades of one base color</strong>
        (e.g., <em>Energy</em> and <em>On-Farm energy use</em>; fertilizer-applied variants).
        </p>
        <p>
        {breakdown_txt}
        </p>"
      )
      
      htmltools::HTML(txt)
    }) %>%
      bindCache(r_country(), scen_set_key(), gas_choice())
    
  })
}
