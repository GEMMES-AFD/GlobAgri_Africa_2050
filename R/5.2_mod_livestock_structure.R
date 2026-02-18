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
        dplyr::filter(
          Region == r_country(),
          Scenario %in% sc,
          stringr::str_trim(Element) == input$element
        ) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::pull(n)
      
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
        dplyr::filter(
          Region == r_country(),
          stringr::str_trim(Element) == element_fact(),
          Scenario %in% sc_req
        ) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(
          year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
          .groups   = "drop"
        ) %>%
        dplyr::filter(is.finite(year_used)) %>%
        dplyr::mutate(
          Scenario_code  = as.character(Scenario),
          Scenario       = factor(Scenario_code, levels = scen_levels_all())
        ) %>%
        dplyr::arrange(Scenario) %>%
        dplyr::select(Scenario_code, Scenario, year_used)
    }) %>% bindCache(cache_key_years())
    
    # -----------------------------------------------------------------------
    # SECURE COLOURS: one fixed colour per displayed Item (legend + slices)
    # -----------------------------------------------------------------------
    # Keys MUST match EXACTLY the Item labels used in `animal_items`.
    # You can change colours here only.
    ITEM_COLORS <- c(
      "Bovine meat"             = "#E15759",
      "Dairy"                   = "#4E79A7",
      "Pork meat"               = "#9C755F",
      "Poultry meat"            = "#F28E2B",
      "Small ruminant meat"     = "#59A14F",
      "Eggs"                    = "#EDC948",
      "Aquatic animal products" = "#76B7B2"
    )
    
    ensure_palette <- function(item_levels, pal_named){
      lv <- as.character(item_levels)
      miss <- setdiff(lv, names(pal_named))
      if (length(miss) > 0) {
        stop("Missing ITEM_COLORS for: ", paste(miss, collapse = ", "))
        # alternative non-bloquante :
        # pal_named[miss] <- "#BAB0AC"
      }
      stats::setNames(unname(pal_named[lv]), lv)
    }
    
    palette_items <- reactive({
      its <- as.character(animal_items)
      ensure_palette(its, ITEM_COLORS)
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
        dplyr::filter(
          Region == r_country(),
          stringr::str_trim(Element) == element_fact(),
          Scenario %in% scen_present,
          Item %in% animal_items
        ) %>%
        dplyr::inner_join(dplyr::select(yrs, Scenario_code, year_used), by = c("Scenario" = "Scenario_code")) %>%
        dplyr::filter(Year == year_used) %>%
        dplyr::group_by(Scenario, Item) %>%
        dplyr::summarise(
          value   = sum(Value, na.rm = TRUE) * mult,
          unit    = uout,
          year    = dplyr::first(Year),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          Scenario = factor(as.character(Scenario), levels = scen_levels_all())
        )
      
      grid <- tidyr::expand_grid(
        Scenario = factor(scen_present, levels = scen_levels_all()),
        Item     = factor(animal_items, levels = animal_items)
      )
      
      df <- grid %>%
        dplyr::left_join(df_raw, by = c("Scenario","Item")) %>%
        dplyr::left_join(dplyr::select(yrs, Scenario, year_used), by = "Scenario") %>%
        dplyr::mutate(
          value = dplyr::coalesce(value, 0),
          unit  = dplyr::coalesce(unit, uout),
          year  = dplyr::coalesce(year, year_used),
          Item  = factor(as.character(Item), levels = animal_items)
        ) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::mutate(
          total = sum(value, na.rm = TRUE),
          share = dplyr::if_else(total > 0, value / total, NA_real_)
        ) %>%
        dplyr::ungroup()
      
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
      cols <- palette_items()   # named by Item label
      u_lbl <- unit_lbl()
      
      df_share <- pd %>%
        dplyr::filter(total > 0) %>%
        dplyr::mutate(
          label_pct = dplyr::if_else(!is.na(share) & share >= 0.01, scales::percent(share, accuracy = 1), ""),
          text_pos  = dplyr::if_else(
            !is.na(share) & share < 0.01, "none",
            dplyr::if_else(share < 0.05, "outside", "inside")
          )
        )
      
      totals <- df_share %>%
        dplyr::distinct(Scenario) %>%
        dplyr::arrange(Scenario) %>%
        dplyr::mutate(
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
          dplyr::filter(as.character(Scenario) == sc_code, value > 0) %>%
          dplyr::mutate(Item = factor(as.character(Item), levels = animal_items)) %>%
          dplyr::arrange(Item)
        
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
            marker       = list(colors = unname(cols[as.character(d$Item)])),
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
      
      # Apply global theme (can set colorway) then force colours again
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "none")
      
      # ---- FORCE colours AFTER theme (robust local/prod)
      for (i in seq_along(p$x$data)) {
        # For pie traces, colors are stored in marker$colors
        labs <- p$x$data[[i]]$labels
        if (!is.null(labs)) {
          labs_chr <- as.character(labs)
          if (all(labs_chr %in% names(cols))) {
            p$x$data[[i]]$marker$colors <- unname(cols[labs_chr])
          }
        }
      }
      
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
          dplyr::transmute(
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
