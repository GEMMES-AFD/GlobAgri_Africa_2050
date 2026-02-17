# R/5.2_mod_animal_items_structure.R
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# UI
# -------------------------------------------------------------------
mod_animal_items_structure_ui <- function(id){
  ns <- NS(id)
  tagList(uiOutput(ns("block")))
}

# -------------------------------------------------------------------
# Server
# -------------------------------------------------------------------
mod_animal_items_structure_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,                 # reactive/function returning scenario CODES to display
    animal_items = c(
      "Bovine meat",
      "Dairy",
      "Pork meat",
      "Poultry meat",
      "Small ruminant meat",
      "Eggs",
      "Aquatic animal products"
    ),
    value_multiplier = 1,
    value_multiplier_energy = 1
){
  moduleServer(id, function(input, output, session){
    
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    
    # --- Hard dependencies --------------------------------------------------
    if (is.null(r_scenarios) || !is.function(r_scenarios)) {
      stop("mod_animal_items_structure_server(): 'r_scenarios' must be provided as a reactive/function returning scenario CODES.")
    }
    if (!exists("scenario_label", mode = "function", inherits = TRUE)) {
      stop("mod_animal_items_structure_server(): missing dependency 'scenario_label(code)'.")
    }
    if (!exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      stop("mod_animal_items_structure_server(): missing dependency 'SCENARIO_LEVELS_DEFAULT'.")
    }
    
    scenario_label_vec <- function(x){
      x <- as.character(x)
      vapply(x, scenario_label, character(1))
    }
    clean_scenario_label <- function(x){
      gsub("\\s*\\(\\d{4}\\)\\s*$", "", x)
    }
    
    # === ENERGY ONLY (exact Elements) ======================================
    unit_lbl <- reactive("Gcal")
    
    ELEMENT_CHOICES <- c(
      "Energy Production"               = "Energy Production",
      "Energy Import Quantity"          = "Energy Import Quantity",
      "Energy Export Quantity"          = "Energy Export Quantity",
      "Energy Domestic supply quantity" = "Energy Domestic supply quantity"
    )
    
    element_ui_label <- reactive({
      req(input$element)
      names(ELEMENT_CHOICES)[match(input$element, ELEMENT_CHOICES)] %||% input$element
    })
    
    # --- Scenario codes to display (ordered centrally) ----------------------
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
    
    # --- Element selected (fact$Element) -----------------------------------
    element_fact <- reactive({
      req(input$element)
      validate(need(input$element %in% unname(ELEMENT_CHOICES), "Unsupported element selected."))
      
      sc <- scen_codes_ordered()
      n_here <- fact %>%
        filter(
          Region == r_country(),
          Scenario %in% sc,
          stringr::str_trim(Element) == input$element
        ) %>%
        summarise(n = n()) %>%
        pull(n)
      
      validate(need(isTRUE(n_here > 0),
                    paste0("No data for ", input$element, " in this country.")))
      
      input$element
    })
    
    # Display multiplier (energy only)
    value_mult <- reactive({
      as.numeric(value_multiplier %||% 1) * as.numeric(value_multiplier_energy %||% 1)
    })
    
    # --- bindCache keys (scalar) -------------------------------------------
    cache_key_years <- reactive({
      req(element_fact(), r_country())
      paste0(
        "animal_items_structure|years|",
        r_country(), "|",
        element_fact(), "|sc=",
        paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    cache_key_data <- reactive({
      req(element_fact(), r_country())
      paste0(
        "animal_items_structure|data|",
        r_country(), "|",
        element_fact(), "|sc=",
        paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    cache_key_plot <- reactive({
      req(element_fact(), r_country())
      paste0(
        "animal_items_structure|plot|",
        r_country(), "|",
        element_fact(), "|sc=",
        paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    # --- Years by scenario for the selected element ------------------------
    years_by_scenario <- reactive({
      req(element_fact())
      sc_req <- scen_codes_ordered()
      validate(need(length(sc_req) > 0, "No scenario selected."))
      
      fact %>%
        filter(
          Region == r_country(),
          stringr::str_trim(Element) == element_fact(),
          Scenario %in% sc_req
        ) %>%
        group_by(Scenario) %>%
        summarise(
          year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
          .groups   = "drop"
        ) %>%
        filter(is.finite(year_used)) %>%
        mutate(
          Scenario_code  = as.character(Scenario),
          Scenario       = factor(Scenario_code, levels = scen_levels_all())
        ) %>%
        arrange(Scenario) %>%
        select(Scenario_code, Scenario, year_used)
    }) %>% bindCache(cache_key_years())
    
    # --- Colors for items (fixed palette + mapping) ----------------------------
    
    # Mapping: fact$Item (your labels) -> palette keys
    ANIMAL_ITEM_TO_COLOR_KEY <- c(
      "Bovine meat"         = "Beef cattle",
      "Small ruminant meat" = "Meat sheep and goats",
      "Eggs"                = "Poultry eggs",
      "Poultry meat"        = "Poultry meat",
      "Pork meat"           = "Pork meat",
      "Dairy"               = "Dairy"
      # "Aquatic animal products" not in palette -> will fallback (hue) automatically
    )
    
    palette_items <- reactive({
      its <- as.character(animal_items)
      
      # Convert items -> palette keys (fallback to itself if no mapping)
      keys <- unname(ANIMAL_ITEM_TO_COLOR_KEY[its])
      keys[is.na(keys)] <- its[is.na(keys)]
      
      # Use your palette function if available, otherwise fallback
      if (exists("emissions_animal_colors_for", mode = "function", inherits = TRUE)) {
        cols_keys <- emissions_animal_colors_for(keys)
      } else {
        cols_keys <- setNames(scales::hue_pal()(length(keys)), keys)
      }
      
      # Return colors named by ORIGINAL item labels (so marker lookup uses Item)
      cols_items <- setNames(unname(cols_keys), its)
      cols_items
    })
    
    # --- Aggregated data by ITEM (no groups) --------------------------------
    data_items <- reactive({
      req(element_fact())
      yrs <- years_by_scenario()
      
      validate(need(nrow(yrs) > 0,
                    sprintf("No data for %s in this country.", element_fact())))
      
      scen_present <- yrs$Scenario_code
      mult <- value_mult()
      uout <- unit_lbl()
      
      df_raw <- fact %>%
        filter(
          Region == r_country(),
          stringr::str_trim(Element) == element_fact(),
          Scenario %in% scen_present,
          Item %in% animal_items
        ) %>%
        inner_join(select(yrs, Scenario_code, year_used), by = c("Scenario" = "Scenario_code")) %>%
        filter(Year == year_used) %>%
        group_by(Scenario, Item) %>%
        summarise(
          value   = sum(Value, na.rm = TRUE) * mult,
          unit    = uout,
          year    = dplyr::first(Year),
          .groups = "drop"
        ) %>%
        mutate(
          Scenario = factor(as.character(Scenario), levels = scen_levels_all())
        )
      
      grid <- tidyr::expand_grid(
        Scenario = factor(scen_present, levels = scen_levels_all()),
        Item     = factor(animal_items, levels = animal_items)
      )
      
      df <- grid %>%
        left_join(df_raw, by = c("Scenario","Item")) %>%
        left_join(select(yrs, Scenario, year_used), by = "Scenario") %>%
        mutate(
          value = coalesce(value, 0),
          unit  = coalesce(unit, uout),
          year  = coalesce(year, year_used),
          Item  = factor(as.character(Item), levels = animal_items)
        ) %>%
        group_by(Scenario) %>%
        mutate(
          total = sum(value, na.rm = TRUE),
          share = if_else(total > 0, value / total, NA_real_)
        ) %>%
        ungroup()
      
      df
    }) %>% bindCache(cache_key_data())
    
    # === BLOCK UI ===========================================================
    output$block <- renderUI({
      ns <- session$ns
      
      div(
        class = "card",
        div(
          class = "card-body",
          h2(textOutput(ns("title"))),
          tags$div(style="height:8px"),
          div(
            class = "u-controls",
            selectInput(
              inputId  = ns("element"),
              label    = "Flow considered",
              choices  = ELEMENT_CHOICES,
              selected = "Energy Production",
              width    = "320px"
            )
          ),
          
          plotly::plotlyOutput(ns("pie_animals"), height = "460px", width = "100%"),
          
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
    })
    
    output$title <- renderText({
      paste0("Structure of animal product flows (Gcal)")
    })
    
    # --- Pies ---------------------------------------------------------------
    output$pie_animals <- renderPlotly({
      pd <- data_items()
      req(nrow(pd) > 0)
      
      validate(need(sum(pd$value, na.rm = TRUE) > 0,
                    "No non-zero value to display."))
      
      th <- get_plotly_tokens()
      cols <- palette_items()
      u_lbl <- unit_lbl()
      
      df_share <- pd %>%
        filter(total > 0) %>%
        mutate(
          label_pct = if_else(!is.na(share) & share >= 0.01, scales::percent(share, accuracy = 1), ""),
          text_pos  = if_else(
            !is.na(share) & share < 0.01, "none",
            if_else(share < 0.05, "outside", "inside")
          )
        )
      
      totals <- df_share %>%
        distinct(Scenario) %>%
        arrange(Scenario) %>%
        mutate(
          Scenario_code  = as.character(Scenario),
          Scenario_label = clean_scenario_label(scenario_label_vec(Scenario_code))
        )
      
      scen_facets <- totals$Scenario_code
      n_pies <- length(scen_facets)
      validate(need(n_pies > 0, "No scenario available."))
      
      # equal size pies
      x_slots <- lapply(seq_len(n_pies), function(i){
        c((i - 1) / n_pies, i / n_pies)
      })
      dom_y_fixed <- c(0.22, 0.90)
      padding <- 0.92
      
      p <- plotly::plot_ly()
      annotations <- vector("list", n_pies)
      
      for (i in seq_len(n_pies)) {
        sc_code <- scen_facets[i]
        sc_lab  <- totals$Scenario_label[totals$Scenario_code == sc_code][1] %||% sc_code
        
        slot <- x_slots[[i]]
        slot_center <- mean(slot)
        slot_half   <- diff(slot) / 2
        
        dom_x <- c(
          slot_center - slot_half * padding,
          slot_center + slot_half * padding
        )
        
        d <- df_share %>%
          filter(as.character(Scenario) == sc_code, value > 0) %>%
          mutate(Item = factor(as.character(Item), levels = animal_items)) %>%
          arrange(Item)
        
        if (nrow(d) == 0) next
        
        show_leg <- (i == 1)
        
        p <- p %>%
          plotly::add_pie(
            data   = d,
            labels = ~Item,
            values = ~share,
            text   = ~label_pct,
            textinfo = "text",
            name   = sc_lab,
            domain = list(x = dom_x, y = dom_y_fixed),
            sort   = FALSE,
            textposition = ~text_pos,
            textfont     = list(color = th$font_color, size = 12),
            insidetextorientation = "horizontal",
            marker       = list(colors = cols[as.character(d$Item)]),
            customdata   = ~value,
            hovertemplate = paste0(
              "<b>", sc_lab, "</b><br>",
              "%{label}<br>",
              "Share: %{percent}<br>",
              "Value: %{customdata:,} ", u_lbl,
              "<extra></extra>"
            ),
            showlegend  = show_leg
          )
        
        # under each pie: scenario name only
        annotations[[i]] <- list(
          x = slot_center,
          y = 0.12,
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          align = "center",
          text = paste0(
            "<span style='color:", th$font_color, ";'>",
            sc_lab,
            "</span>"
          ),
          font = list(size = 12, color = th$font_color)
        )
      }
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "none")
      
      p %>%
        plotly::layout(
          title         = NULL,
          annotations   = annotations,
          margin        = list(t = 80, b = 20, l = 40, r = 40),
          showlegend    = TRUE,
          legend        = list(
            orientation = "h",
            x = 0.5, xanchor = "center",
            y = 1.02, yanchor = "bottom",
            font = list(color = th$font_color, size = 12),
            traceorder = "normal"
          ),
          paper_bgcolor = APP_TRANSPARENT,
          plot_bgcolor  = APP_TRANSPARENT,
          hoverlabel    = list(
            bgcolor = th$hover_bg,
            font    = list(color = th$hover_font)
          ),
          font          = list(color = th$font_color)
        ) %>%
        plotly::config(displaylogo = FALSE)
    }) %>% bindCache(cache_key_plot())
    
    # --- CSV export ---------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0(
          "Animal_items_structure_",
          gsub(" ", "_", r_country()),
          "_",
          gsub(" ", "_", element_fact()),
          "_energy.csv"
        )
      },
      content = function(file){
        pd <- data_items()
        out <- pd %>%
          transmute(
            Country        = r_country(),
            Scenario_code  = as.character(Scenario),
            Scenario_label = clean_scenario_label(scenario_label_vec(as.character(Scenario))),
            Year           = year,
            Element_UI     = element_ui_label(),
            Element_fact   = element_fact(),
            Unit           = unit_lbl(),
            Item           = as.character(Item),
            Value          = value,
            Total_selected_items = total,
            Share_selected_items = share
          )
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # --- Note ---------------------------------------------------------------
    output$note <- renderUI({
      e_txt <- element_fact() %||% "Energy Production"
      items_txt <- paste(animal_items, collapse = ", ")
      
      htmltools::HTML(glue::glue(
        "<p>
        Each pie chart shows, for the selected country, the <strong>structure</strong> of <strong>{e_txt}</strong>
        across the following animal items: <em>{items_txt}</em>.
        </p>
        <p>
        <strong>All pies have the same size</strong>: they represent <strong>percentage shares only</strong> among the selected items (composition),
        not the absolute level of the flow. To complete the analysis, look at the quantitative level of the considered flow. 
        </p>
        <p>
        Percentage labels are hidden for slices below <strong>1%</strong>.
        </p>"
      ))
    })
    
  })
}
