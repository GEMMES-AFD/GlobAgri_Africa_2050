# R/5.1_mod_crop_structure.R
# -------------------------------------------------------------------

# --- Crop groups -----------------------------------------------------------
# 👉 Grass & fodder is EXCLUDED from groups and from the total
CROP_GROUPS <- list(
  "All crop products" = c(
    "Cake Other Oilcrops","Fibers etc.","Fruits and vegetables",
    # "Grass",  # excluded from total
    "Maize","Millet and Sorghum","Oil Other Oilcrops","Oilpalm fruit",
    "Olive Oil","Olives","Other Oilcrops","Other cereals",
    "Other plant products","Other products","Palm Products Oil",
    "Palmkernel Cake","Pulses","Rape and Mustard Cake","Rape and Mustard Oil",
    "Rape and Mustardseed","Rice","Roots and Tuber","Soyabean Cake",
    "Soyabean Oil","Soyabeans","Sugar plants and products",
    "Sunflowerseed","Sunflowerseed Cake","Sunflowerseed Oil","Wheat"
  ),
  "Cereals" = c("Maize","Millet and Sorghum","Other cereals","Rice","Wheat"),
  "Roots and tubers" = c("Roots and Tuber"),
  "Pulses"           = c("Pulses"),
  "Oilcrops (incl. cakes & oils)" = c(
    "Cake Other Oilcrops","Oil Other Oilcrops","Oilpalm fruit","Olive Oil",
    "Olives","Other Oilcrops","Palm Products Oil","Palmkernel Cake",
    "Rape and Mustard Cake","Rape and Mustard Oil","Rape and Mustardseed",
    "Soyabean Cake","Soyabean Oil","Soyabeans","Sunflowerseed",
    "Sunflowerseed Cake","Sunflowerseed Oil"
  ),
  "Fruits & vegetables"     = c("Fruits and vegetables"),
  "Sugar crops"             = c("Sugar plants and products"),
  # "Grass & fodder"        = c("Grass"),  # removed
  "Fibres & other products" = c("Fibers etc.","Other plant products","Other products")
)

CROP_LABELS <- c(
  "All crop products"             = "crop products",
  "Cereals"                       = "cereals",
  "Roots and tubers"              = "roots and tubers",
  "Pulses"                        = "pulses",
  "Oilcrops (incl. cakes & oils)" = "oilcrops",
  "Fruits & vegetables"           = "fruits and vegetables",
  "Sugar crops"                   = "sugar crops",
  # "Grass & fodder"              = "grass and fodder crops",
  "Fibres & other products"       = "fibre and other plant products"
)

# Item -> crop_group table
CROP_ITEM_GROUP <- tibble::tibble(
  Item       = unlist(CROP_GROUPS[names(CROP_GROUPS) != "All crop products"]),
  crop_group = rep(
    names(CROP_GROUPS)[names(CROP_GROUPS) != "All crop products"],
    lengths(CROP_GROUPS[names(CROP_GROUPS) != "All crop products"])
  )
) |>
  distinct(Item, .keep_all = TRUE)

# Map crop group labels -> ITEM_COLORS families (defined in R/02)
CROP_TO_ITEM_FAMILY <- c(
  "cereals"                        = "Cereals",
  "roots and tubers"               = "Roots and tubers",
  "pulses"                         = "Pulses",
  "oilcrops"                       = "Oil",
  "fruits and vegetables"          = "Vegetables and fruits",
  "sugar crops"                    = "Sugar",
  "fibre and other plant products" = "Other"
)

# -------------------------------------------------------------------
# UI
# -------------------------------------------------------------------
mod_crop_structure_ui <- function(id){
  ns <- NS(id)
  tagList(uiOutput(ns("block")))
}

# -------------------------------------------------------------------
# Server
# -------------------------------------------------------------------
mod_crop_structure_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,         # REQUIRED: reactive/function returning scenario CODES to display
    group_var = NULL,
    harvest_element = "Area harvested",
    exclude_items = c(
      "All products","All crops","Agricultural land occupation (Farm)",
      "Cropland","Forest land","Land under perm. meadows and pastures"
    ),
    value_multiplier = 1,
    value_multiplier_energy = 1
){
  moduleServer(id, function(input, output, session){
    
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    
    # --- Hard dependencies --------------------------------------------------
    if (is.null(r_scenarios) || !is.function(r_scenarios)) {
      stop("mod_crop_structure_server(): 'r_scenarios' must be provided as a reactive/function returning scenario CODES.")
    }
    if (!exists("scenario_label", mode = "function", inherits = TRUE)) {
      stop("mod_crop_structure_server(): missing dependency 'scenario_label(code)'.")
    }
    if (!exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      stop("mod_crop_structure_server(): missing dependency 'SCENARIO_LEVELS_DEFAULT'.")
    }
    
    scenario_label_vec <- function(x){
      x <- as.character(x)
      vapply(x, scenario_label, character(1))
    }
    
    clean_scenario_label <- function(x){
      # remove trailing " (YYYY)" only
      gsub("\\s*\\(\\d{4}\\)\\s*$", "", x)
    }
    
    # === ENERGY ONLY (exact Elements) ======================================
    UNIT_MODE_FIXED <- "energy"
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
        "crop_structure|years|",
        r_country(), "|",
        element_fact(), "|unit=", UNIT_MODE_FIXED, "|sc=",
        paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    cache_key_data <- reactive({
      req(element_fact(), r_country())
      paste0(
        "crop_structure|data|",
        r_country(), "|",
        element_fact(), "|unit=", UNIT_MODE_FIXED, "|sc=",
        paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    cache_key_plot <- reactive({
      req(element_fact(), r_country())
      paste0(
        "crop_structure|plot|",
        r_country(), "|",
        element_fact(), "|unit=", UNIT_MODE_FIXED, "|sc=",
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
    
    # Palette based on ITEM_COLORS (R/02)
    if (!exists("crop_group_colors_for", mode = "function")) {
      crop_group_colors_for <- function(groups){
        groups <- as.character(groups)
        if (exists("ITEM_COLORS", inherits = TRUE)) {
          cols_item <- ITEM_COLORS
          fams <- CROP_TO_ITEM_FAMILY[groups]
          vapply(seq_along(groups), function(i){
            fam <- fams[i]
            if (!is.na(fam) && fam %in% names(cols_item)) {
              cols_item[[fam]]
            } else {
              "#CCCCCC"
            }
          }, character(1))
        } else {
          scales::hue_pal()(length(groups))
        }
      }
    }
    
    # --- Aggregated data by crop group -------------------------------------
    data_groups <- reactive({
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
          Item %in% CROP_GROUPS[["All crop products"]]
        ) %>%
        inner_join(select(yrs, Scenario_code, year_used), by = c("Scenario" = "Scenario_code")) %>%
        filter(Year == year_used) %>%
        left_join(CROP_ITEM_GROUP, by = "Item") %>%
        group_by(Scenario, crop_group) %>%
        summarise(
          value   = sum(Value, na.rm = TRUE) * mult,
          unit    = uout,
          year    = dplyr::first(Year),
          .groups = "drop"
        ) %>%
        mutate(
          Scenario = factor(as.character(Scenario), levels = scen_levels_all())
        )
      
      groups_all <- names(CROP_LABELS)[names(CROP_LABELS) != "All crop products"]
      
      grid <- tidyr::expand_grid(
        Scenario   = factor(scen_present, levels = scen_levels_all()),
        crop_group = groups_all
      )
      
      df <- grid %>%
        left_join(df_raw, by = c("Scenario","crop_group")) %>%
        left_join(select(yrs, Scenario, year_used), by = "Scenario") %>%
        mutate(
          value = coalesce(value, 0),
          unit  = coalesce(unit, uout),
          year  = coalesce(year, year_used),
          crop_group_label = CROP_LABELS[crop_group]
        ) %>%
        group_by(Scenario) %>%
        mutate(
          total = sum(value, na.rm = TRUE),
          share = if_else(total > 0, value / total, NA_real_)
        ) %>%
        ungroup()
      
      df
    }) %>% bindCache(cache_key_data())
    
    # --- Palette ------------------------------------------------------------
    palette_groups <- reactive({
      pd <- data_groups()
      
      group_levels <- CROP_LABELS[names(CROP_LABELS) != "All crop products"]
      levs <- group_levels[group_levels %in% unique(pd$crop_group_label)]
      if (length(levs) == 0) levs <- unique(pd$crop_group_label)
      
      cols <- crop_group_colors_for(levs)
      names(cols) <- levs
      cols
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
          
          plotly::plotlyOutput(ns("pie_crops"), height = "460px", width = "100%"),
          
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
      paste0("Structure of crop flows (Gcal)")
    })
    
    # --- Pies ---------------------------------------------------------------
    output$pie_crops <- renderPlotly({
      pd <- data_groups()
      req(nrow(pd) > 0)
      
      validate(need(sum(pd$value, na.rm = TRUE) > 0,
                    "No non-zero value to display."))
      
      # >>> THEME GLOBAL (R/99)
      th <- get_plotly_tokens()
      
      group_levels <- CROP_LABELS[names(CROP_LABELS) != "All crop products"]
      
      pd <- pd %>%
        mutate(crop_group_label = factor(crop_group_label, levels = group_levels))
      
      cols <- palette_groups()
      if (any(!pd$crop_group_label %in% names(cols))) {
        extra <- setdiff(as.character(pd$crop_group_label), names(cols))
        add_cols <- crop_group_colors_for(extra)
        names(add_cols) <- extra
        cols <- c(cols, add_cols)
      }
      
      df_share <- pd %>%
        filter(total > 0) %>%
        mutate(
          # hide % labels under 1%
          label_pct = if_else(!is.na(share) & share >= 0.01, scales::percent(share, accuracy = 1), ""),
          text_pos  = if_else(
            !is.na(share) & share < 0.01, "none",
            if_else(share < 0.05, "outside", "inside")
          )
        )
      
      u_lbl <- unit_lbl()
      
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
      
      # equal size pies (no scaling by totals)
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
          filter(as.character(Scenario) == sc_code, value > 0)
        
        if (nrow(d) == 0) next
        
        show_leg <- (i == 1)
        
        p <- p %>%
          plotly::add_pie(
            data   = d,
            labels = ~crop_group_label,
            values = ~share,
            text   = ~label_pct,
            textinfo = "text",
            name   = sc_lab,
            domain = list(x = dom_x, y = dom_y_fixed),
            sort   = FALSE,
            textposition = ~text_pos,
            textfont     = list(color = th$font_color, size = 12),
            insidetextorientation = "horizontal",
            marker       = list(colors = cols[as.character(d$crop_group_label)]),
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
        
        # under each pie: scenario name only (no year, no totals)
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
          "Crop_structure_",
          gsub(" ", "_", r_country()),
          "_",
          gsub(" ", "_", element_fact()),
          "_energy.csv"
        )
      },
      content = function(file){
        pd <- data_groups()
        out <- pd %>%
          transmute(
            Country        = r_country(),
            Scenario_code  = as.character(Scenario),
            Scenario_label = clean_scenario_label(scenario_label_vec(as.character(Scenario))),
            Year           = year,
            Element_UI     = element_ui_label(),
            Element_fact   = element_fact(),
            Unit           = unit_lbl(),
            Crop_group     = crop_group_label,
            Value          = value,
            Total          = total,
            Share          = share
          )
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # --- Note ---------------------------------------------------------------
    output$note <- renderUI({
      e_txt <- element_fact() %||% "Energy Production"
      
      htmltools::HTML(glue::glue(
        "<p>
        Each pie chart shows, for the selected country, the <strong>structure</strong> of crop <strong>{e_txt}</strong> by broad crop groups
        (cereals, roots and tubers, pulses, oilcrops, fruits and vegetables, sugar crops, fibre and other plant products),
        excluding forage and grass crops.
        </p>
        <p>
        <strong>All pies have the same size</strong>: they represent <strong>percentage shares only</strong> (composition), not the absolute level of the flow. 
        To complete the analysis, look at the quantitative level of the considered flow. 
        </p>
        <p>
        Percentage labels are hidden for slices below <strong>1%</strong>.
        </p>"
      ))
    })
    
  })
}
