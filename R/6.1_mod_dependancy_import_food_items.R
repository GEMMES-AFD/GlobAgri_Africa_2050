# R/6.1_mod_dependancy_import_food_items.R
# -------------------------------------------------

mod_dependancy_import_food_items_ui <- function(id, wrap_in_card = TRUE){
  ns <- NS(id)
  
  content <- tagList(
    h2("Import dependency indicators by product (in tons)"),
    
    # --- Ratio explainer cards (above the chart) ---
    div(
      class = "u-row diet-cards",
      div(
        class = "u-card u-card--flat diet-card",
        div(class = "diet-card-title", "Food Dependency"),
        div(
          class = "diet-card-text",
          "Measures dependence relative to the food use of the same item. It is computed as :",
          tags$br(),
          tags$strong("Food Dependency (in %) = Import / Food × 100"),
          tags$br(),
          "Items with Food = 0 are excluded because the ratio is not defined."
        )
      ),
      div(
        class = "u-card u-card--flat diet-card",
        div(class = "diet-card-title", "Import Dependency Ratio (IDR)"),
        div(
          class = "diet-card-text",
          "Share of domestic supply covered by imports :",
          tags$br(),
          tags$strong("IDR (in %) = Imports × 100 / (Production + Imports − Exports)"),
          tags$br(),
          "Interpretation is easier when imports mainly serve domestic use and are not massively re-exported."
        )
      ),
      div(
        class = "u-card u-card--flat diet-card",
        div(class = "diet-card-title", "Self-Sufficiency Ratio (SSR)"),
        div(
          class = "diet-card-text",
          "Mirror indicator of self-sufficiency :",
          tags$br(),
          tags$strong("SSR (in %) = Production × 100 / (Production + Imports − Exports)"),
          tags$br(),
          "It uses the same domestic supply proxy as IDR."
        )
      )
    ),
    
    tags$br(),
    
    # Tight spacing for controls only
    tags$style(htmltools::HTML(paste0(
      "#", ns("controls"), " .form-group { margin-bottom: 0 !important; }",
      "#", ns("controls"), " small { margin: 0 0 2px 0 !important; line-height: 1.1; display:block; }"
    ))),
    
    # ---- Controls ----
    div(
      id = ns("controls"),
      class = "row",
      div(
        class = "col-sm-4",
        selectInput(
          ns("view_mode"),
          label = "Indicator :",
          choices = c(
            "Volumes (Imports vs Food, Mt)"         = "volumes",
            "Food dependency (Import / Food, %)"         = "ratio",
            "Import Dependency Ratio (IDR, %)"      = "idr",
            "Self-Sufficiency Ratio (SSR, %)"       = "ssr"
          ),
          selected = "volumes"
        )
      ),
      div(
        class = "col-sm-4",
        selectInput(
          ns("scenario_sel"),
          label = "Scenario :",
          choices  = character(0),
          selected = NULL
        )
      ),
      div(
        class = "col-sm-4",
        selectInput(
          ns("rank_by"),
          label = "Ranking :",
          choices = c(
            "Rank by imports"      = "imports",
            "Rank by food"         = "food",
            "Rank by production"   = "production",
            "Rank by Food Dependancy" = "ratio",
            "Rank by IDR"          = "idr",
            "Rank by SSR"          = "ssr"
          ),
          selected = "imports"
        )
      )
    ),
    
    div(
      class = "row",
      div(
        class = "col-sm-4",
        tags$br(),
        sliderInput(
          ns("top_n"),
          label = "Top items",
          min = 5, max = 30, value = 15, step = 1
        )
      )
    ),
    
    plotly::plotlyOutput(ns("plot"), height = "auto"),
    
    div(
      class = "u-actions",
      downloadLink(
        ns("dl_csv"),
        label = tagList(icon("download"), "CSV")
      )
    ),
    uiOutput(ns("note"))
  )
  
  if (isTRUE(wrap_in_card)) {
    div(class = "card", div(class = "card-body", content))
  } else {
    content
  }
}


mod_dependancy_import_food_items_server <- function(id,
                                                    fact,
                                                    r_country,
                                                    r_scenarios,
                                                    scen_show_key = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    # ---- Expected config dependencies ----
    if (is.null(r_scenarios) || !is.function(r_scenarios)) {
      stop("mod_dependancy_import_food_items_server(): 'r_scenarios' must be provided as a reactive/function returning scenario CODES.")
    }
    if (!exists("scenario_label", mode = "function", inherits = TRUE)) {
      stop("mod_dependancy_import_food_items_server(): missing dependency 'scenario_label(code)'.")
    }
    if (!exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      stop("mod_dependancy_import_food_items_server(): missing dependency 'SCENARIO_LEVELS_DEFAULT'.")
    }
    if (!exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
      stop("mod_dependancy_import_food_items_server(): missing dependency 'SCENARIO_BASE_YEAR_CODE'.")
    }
    
    # Baseline year (technique) for year picking
    base_year <- if (exists("BASE_YEAR", inherits = TRUE)) as.integer(BASE_YEAR) else 2018L
    
    # Elements required
    ELEMENTS_QTY <- c("Import Quantity", "Food", "Production", "Export Quantity")
    
    # Items for which Production must be taken ONLY on aggregated lines (Animal/System empty)
    ADJUST_PROD_ITEMS <- c("Dairy", "Beef cattle")
    
    # ---------------------------
    # Units helper (internal = kt)
    # ---------------------------
    # Convert any unit to kt (thousand tonnes) for robust computations
    to_kt <- function(value, unit){
      v <- suppressWarnings(as.numeric(value))
      u <- tolower(trimws(as.character(unit %||% "")))
      
      if (!is.finite(v)) return(NA_real_)
      
      # priority: explicit 1000 / kt
      if (grepl("1000", u) || grepl("\\bkt\\b", u) || grepl("kton", u) || grepl("thousand", u)) {
        return(v) # already kt (i.e., 1000 tonnes)
      }
      # million tonnes / Mt
      if (grepl("\\bmt\\b", u) || grepl("million", u)) {
        return(v * 1000) # Mt -> kt
      }
      # tonnes / t
      if (grepl("tonne", u) || grepl("\\bt\\b", u)) {
        return(v / 1000) # t -> kt
      }
      
      # fallback: assume kt (most of your fact is 1000 tonnes)
      v
    }
    
    fmt_qty_auto <- function(x_kt){
      x <- suppressWarnings(as.numeric(x_kt))
      
      out <- rep("NA", length(x))
      ok  <- is.finite(x)
      
      big   <- ok & abs(x) >= 1000     # >= 1000 kt => Mt
      small <- ok & !big               # < 1000 kt  => kt
      
      out[big]   <- paste0(scales::comma(x[big] / 1000, accuracy = 0.01), " Mt")
      out[small] <- paste0(scales::comma(x[small],       accuracy = 1),   " kt")
      
      out
    }
    
    
    # ---- Scalar key for scenario set ----
    scen_set_key <- reactive({
      req(r_scenarios())
      if (!is.null(scen_show_key)) {
        k <- scen_show_key()
        req(length(k) == 1)
        return(as.character(k))
      }
      paste(as.character(r_scenarios()), collapse = "|")
    })
    
    # ---- Scenarios offered: ordered via config + present in data ----
    scenarios_effective <- reactive({
      req(fact, r_country(), r_scenarios())
      
      sc_app <- unique(as.character(r_scenarios()))
      sc_app <- sc_app[!is.na(sc_app) & nzchar(sc_app)]
      validate(need(length(sc_app) > 0, "No scenario provided to the module."))
      
      known   <- intersect(SCENARIO_LEVELS_DEFAULT, sc_app)
      unknown <- setdiff(sc_app, SCENARIO_LEVELS_DEFAULT)
      sc_ord  <- c(known, sort(unknown))
      
      scen_in_data <- fact %>%
        dplyr::filter(
          Region  == r_country(),
          Element %in% ELEMENTS_QTY
        ) %>%
        dplyr::distinct(Scenario) %>%
        dplyr::pull(Scenario) %>%
        as.character()
      
      sc_keep <- sc_ord[sc_ord %in% scen_in_data]
      sc_keep
    }) %>% bindCache(r_country(), scen_set_key())
    
    # ---- Baseline scenario code (effective) from config only ----
    baseline_code <- reactive({
      scs <- scenarios_effective()
      if (!length(scs)) return(NA_character_)
      b <- as.character(get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE))
      if (nzchar(b) && b %in% scs) return(b)
      scs[1]
    }) %>% bindCache(r_country(), scen_set_key())
    
    # ---- Update scenario selector ----
    observeEvent(scenarios_effective(), {
      codes <- scenarios_effective()
      
      if (length(codes) == 0) {
        shiny::updateSelectInput(session, "scenario_sel", choices = character(0), selected = character(0))
        return(NULL)
      }
      
      choices <- stats::setNames(
        codes,
        vapply(codes, scenario_label, FUN.VALUE = character(1))
      )
      
      cur <- input$scenario_sel %||% ""
      if (nzchar(cur) && cur %in% codes) {
        sel <- cur
      } else {
        b <- baseline_code()
        sel <- if (!is.na(b) && nzchar(b) && b %in% codes) b else codes[[1]]
      }
      
      shiny::updateSelectInput(session, "scenario_sel", choices = choices, selected = sel)
    }, ignoreInit = FALSE)
    
    # Suggest ranking by indicator
    observeEvent(input$view_mode, {
      v <- input$view_mode %||% "volumes"
      sel <- dplyr::case_when(
        v == "volumes" ~ "imports",
        v == "ratio"   ~ "ratio",
        v == "idr"     ~ "idr",
        v == "ssr"     ~ "ssr",
        TRUE           ~ "imports"
      )
      shiny::updateSelectInput(session, "rank_by", selected = sel)
    }, ignoreInit = TRUE)
    
    # ---- Auto year selection based on baseline_code() ----
    year_for_scenario <- reactive({
      req(fact, r_country(), input$scenario_sel)
      
      years <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Scenario == input$scenario_sel,
          Element  %in% ELEMENTS_QTY
        ) %>%
        dplyr::distinct(Year) %>%
        dplyr::pull(Year)
      
      if (!length(years)) return(NA_integer_)
      
      b <- baseline_code()
      if (!is.na(b) && identical(input$scenario_sel, b)) {
        if (base_year %in% years) return(as.integer(base_year))
        return(as.integer(min(years, na.rm = TRUE)))
      } else {
        if (2050 %in% years) return(2050L)
        return(as.integer(max(years, na.rm = TRUE)))
      }
    }) %>% bindCache(r_country(), input$scenario_sel, scen_set_key())
    
    # ---- Raw data (single scenario + selected year) ----
    data_filtered <- reactive({
      req(fact, r_country(), input$scenario_sel)
      
      yr <- year_for_scenario()
      req(is.finite(yr))
      
      df <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Scenario == input$scenario_sel,
          Year     == yr,
          Element  %in% ELEMENTS_QTY,
          !is.na(Item),
          Item != "All"
        ) %>%
        dplyr::mutate(
          Unit_chr = as.character(Unit),
          qty_kt   = mapply(to_kt, Value, Unit_chr)
        )
      
      # Production correction for Dairy + Beef cattle:
      # keep ONLY rows where Animal and System are empty (aggregated lines)
      df <- df %>%
        dplyr::mutate(
          sys_empty = is.na(System) | trimws(as.character(System)) == "" | trimws(as.character(System)) == "NA",
          ani_empty = is.na(Animal) | trimws(as.character(Animal)) == "" | trimws(as.character(Animal)) == "NA",
          keep_row  = !(Element == "Production" & Item %in% ADJUST_PROD_ITEMS) | (sys_empty & ani_empty)
        ) %>%
        dplyr::filter(keep_row) %>%
        dplyr::select(-Unit_chr, -sys_empty, -ani_empty, -keep_row)
      
      df
    }) %>% bindCache(r_country(), input$scenario_sel, year_for_scenario())
    
    # ---- Item-level metrics (internal unit = kt) ----
    data_items <- reactive({
      df <- data_filtered()
      req(nrow(df) > 0)
      
      df_sum <- df %>%
        dplyr::group_by(Item, Element) %>%
        dplyr::summarise(
          qty_kt = sum(qty_kt, na.rm = TRUE),
          .groups = "drop"
        )
      
      wide <- df_sum %>%
        tidyr::pivot_wider(
          names_from  = Element,
          values_from = qty_kt,
          values_fill = 0
        ) %>%
        dplyr::rename(
          import_kt = `Import Quantity`,
          food_kt   = Food,
          prod_kt   = Production,
          export_kt = `Export Quantity`
        )
      
      # keep Food > 0 (as per Food Dependency definition card)
      wide %>%
        dplyr::filter(food_kt > 0) %>%
        dplyr::mutate(
          dep_share_pct = 100 * (import_kt / food_kt),
          dom_supply_kt = prod_kt + import_kt - export_kt,
          idr_pct       = dplyr::if_else(dom_supply_kt > 0, 100 * import_kt / dom_supply_kt, NA_real_),
          ssr_pct       = dplyr::if_else(dom_supply_kt > 0, 100 * prod_kt   / dom_supply_kt, NA_real_)
        )
    }) %>% bindCache(r_country(), input$scenario_sel, year_for_scenario())
    
    # ---- Top N selection ----
    data_topn <- reactive({
      df <- data_items()
      req(nrow(df) > 0)
      
      rank_by <- input$rank_by %||% "imports"
      top_n   <- input$top_n %||% 15
      
      df %>%
        dplyr::mutate(
          rank_metric = dplyr::case_when(
            rank_by == "imports"     ~ import_kt,
            rank_by == "food"        ~ food_kt,
            rank_by == "production"  ~ prod_kt,
            rank_by == "ratio"       ~ dep_share_pct,
            rank_by == "idr"         ~ idr_pct,
            rank_by == "ssr"         ~ ssr_pct,
            TRUE                     ~ import_kt
          )
        ) %>%
        dplyr::arrange(dplyr::desc(rank_metric)) %>%
        dplyr::slice_head(n = top_n) %>%
        dplyr::mutate(Item = factor(Item, levels = unique(Item)))
    }) %>% bindCache(r_country(), input$scenario_sel, year_for_scenario(), input$rank_by, input$top_n)
    
    # ---- Plot ----
    output$plot <- plotly::renderPlotly({
      df <- data_topn()
      req(nrow(df) > 0)
      
      view <- input$view_mode %||% "volumes"
      
      # >>> R/99 tokens (dark/light)
      th <- if (exists("get_plotly_tokens", mode = "function")) get_plotly_tokens() else list(
        font_color      = "#111827",
        muted_color     = "#6B7280",
        axis_linecolor  = "rgba(0,0,0,.18)",
        hover_bg        = "#FFFFFF",
        hover_font      = "#111827"
      )
      
      gg_txt   <- th$font_color %||% "#111827"
      gg_muted <- th$muted_color %||% "#6B7280"
      gg_grid  <- scales::alpha(gg_txt, 0.18)
      
      # helper colors
      get_flow_color <- function(flow) {
        if (exists("sankey_node_palette")) {
          base_cols <- sankey_node_palette()
          col <- base_cols[flow]
          if (!is.na(col)) return(unname(col)[1])
        }
        scales::hue_pal()(1)
      }
      
      if (view == "volumes") {
        
        # Tooltip STRICT: only what's used to build the bar (Imports vs Food)
        df_long <- df %>%
          dplyr::select(Item, import_kt, food_kt) %>%
          tidyr::pivot_longer(
            cols      = c(import_kt, food_kt),
            names_to  = "Flow",
            values_to = "kt"
          ) %>%
          dplyr::mutate(
            Value_Mt = kt / 1000, # kt -> Mt for axis/plot
            Flow     = dplyr::recode(Flow, import_kt = "Imports", food_kt = "Food"),
            Flow     = factor(Flow, levels = c("Imports", "Food")),
            tooltip  = paste0(
              "Item: ", as.character(Item), "<br>",
              "Flow: ", as.character(Flow), "<br>",
              "Value: ", fmt_qty_auto(kt)
            )
          )
        
        col_imports <- get_flow_color("Imports")
        col_food    <- get_flow_color("Food")
        
        gg <- ggplot2::ggplot(
          df_long,
          ggplot2::aes(
            x    = Item,
            y    = Value_Mt,
            fill = Flow,
            text = tooltip
          )
        ) +
          ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
          ggplot2::scale_fill_manual(
            name   = NULL,
            values = unname(c(col_imports, col_food)),
            breaks = c("Imports", "Food"),
            limits = c("Imports", "Food"),
            drop   = FALSE
          ) +
          ggplot2::labs(
            x = NULL,
            y = "Quantity (million tonnes, Mt)"
          ) +
          ggplot2::scale_y_continuous(
            labels = scales::label_comma(),
            expand = ggplot2::expansion(mult = c(0, 0.10))
          ) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(colour = gg_grid),
            
            legend.position  = "bottom",
            legend.text      = ggplot2::element_text(colour = gg_muted),
            legend.title     = ggplot2::element_blank(),
            
            axis.text.x      = ggplot2::element_text(
              angle = 45, hjust = 1, vjust = 1,
              margin = ggplot2::margin(t = 10),
              colour = gg_txt
            ),
            axis.text.y      = ggplot2::element_text(colour = gg_txt),
            axis.title.y     = ggplot2::element_text(colour = gg_txt),
            
            plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
            panel.background = ggplot2::element_rect(fill = "transparent", colour = NA)
          )
        
        p <- suppressWarnings(plotly::ggplotly(gg, tooltip = "text"))
        p <- plotly::layout(
          p,
          xaxis = list(categoryorder = "array", categoryarray = levels(df_long$Item))
        )
        
        # Remove Plotly "extra" box line
        p <- plotly::style(p, hovertemplate = "%{text}<extra></extra>")
        
      } else {
        
        metric_col <- dplyr::case_when(
          view == "ratio" ~ "dep_share_pct",
          view == "idr"   ~ "idr_pct",
          view == "ssr"   ~ "ssr_pct",
          TRUE            ~ "dep_share_pct"
        )
        
        y_title <- dplyr::case_when(
          view == "ratio" ~ "Dependency share (Import / Food, %)",
          view == "idr"   ~ "Import Dependency Ratio (IDR, %)",
          view == "ssr"   ~ "Self-Sufficiency Ratio (SSR, %)",
          TRUE            ~ "Dependency share (Import / Food, %)"
        )
        
        # Tooltip STRICT: only what's used to build the metric bar
        df_metric <- df %>%
          dplyr::mutate(
            metric = .data[[metric_col]],
            lbl    = dplyr::if_else(is.finite(metric), paste0(round(metric), "%"), NA_character_),
            tooltip = dplyr::case_when(
              view == "ratio" ~ paste0(
                "Item: ", as.character(Item), "<br>",
                "Imports: ", fmt_qty_auto(import_kt), "<br>",
                "Food: ",    fmt_qty_auto(food_kt), "<br>",
                "Import / Food: ",
                dplyr::if_else(is.finite(dep_share_pct), paste0(round(dep_share_pct, 1), "%"), "NA")
              ),
              view == "idr" ~ paste0(
                "Item: ", as.character(Item), "<br>",
                "Imports: ", fmt_qty_auto(import_kt), "<br>",
                "Domestic supply (Prod + Imp − Exp): ",
                dplyr::if_else(is.finite(dom_supply_kt), fmt_qty_auto(dom_supply_kt), "NA"), "<br>",
                "IDR: ",
                dplyr::if_else(is.finite(idr_pct), paste0(round(idr_pct, 1), "%"), "NA")
              ),
              view == "ssr" ~ paste0(
                "Item: ", as.character(Item), "<br>",
                "Production: ", fmt_qty_auto(prod_kt), "<br>",
                "Domestic supply (Prod + Imp − Exp): ",
                dplyr::if_else(is.finite(dom_supply_kt), fmt_qty_auto(dom_supply_kt), "NA"), "<br>",
                "SSR: ",
                dplyr::if_else(is.finite(ssr_pct), paste0(round(ssr_pct, 1), "%"), "NA")
              ),
              TRUE ~ paste0("Item: ", as.character(Item))
            )
          )
        
        req(any(is.finite(df_metric$metric)))
        
        col_main <- unname(get_flow_color("Imports"))[1]
        label_nudge <- 0.03 * max(df_metric$metric, na.rm = TRUE)
        
        gg <- ggplot2::ggplot(
          df_metric,
          ggplot2::aes(
            x    = Item,
            y    = metric,
            text = tooltip
          )
        ) +
          ggplot2::geom_col(fill = col_main, width = 0.7, na.rm = TRUE) +
          ggplot2::geom_text(
            ggplot2::aes(label = lbl),
            nudge_y  = label_nudge,
            vjust    = 0,
            fontface = "bold",
            size     = 3.5,
            na.rm    = TRUE,
            colour   = gg_txt
          ) +
          ggplot2::labs(x = NULL, y = y_title) +
          ggplot2::scale_y_continuous(
            labels = scales::label_comma(),
            expand = ggplot2::expansion(mult = c(0, 0.30))
          ) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(colour = gg_grid),
            
            axis.text.x      = ggplot2::element_text(
              angle = 45, hjust = 1, vjust = 1,
              margin = ggplot2::margin(t = 10),
              colour = gg_txt
            ),
            axis.text.y      = ggplot2::element_text(colour = gg_txt),
            axis.title.y     = ggplot2::element_text(colour = gg_txt),
            
            plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
            panel.background = ggplot2::element_rect(fill = "transparent", colour = NA)
          )
        
        p <- suppressWarnings(plotly::ggplotly(gg, tooltip = "text"))
        p <- plotly::layout(
          p,
          xaxis = list(categoryorder = "array", categoryarray = levels(df_metric$Item))
        )
        
        # Remove Plotly "extra" box line
        p <- plotly::style(p, hovertemplate = "%{text}<extra></extra>")
      }
      
      # Marges + thème plotly indexé R/99
      p <- plotly::layout(p, margin = list(l = 40, r = 20, t = 20, b = 90))
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      } else if (exists("plotly_theme_transparent", mode = "function")) {
        p <- plotly_theme_transparent(p)
      }
      
      p
    }) %>% bindCache(r_country(), input$scenario_sel, year_for_scenario(), input$view_mode, input$rank_by, input$top_n)
    
    # CSV export (kt values in output to keep coherence; you can post-format if needed)
    output$dl_csv <- downloadHandler(
      filename = function(){
        ctry <- r_country()
        yr   <- year_for_scenario()
        sc   <- input$scenario_sel %||% "scenario"
        sc_lbl <- if (!is.null(sc) && nzchar(sc)) scenario_label(sc) else "scenario"
        sc2  <- gsub("[^A-Za-z0-9_\\-]", "_", sc_lbl)
        paste0("import_dependency_indicators_", ctry, "_", yr, "_", sc2, ".csv")
      },
      content = function(file){
        df <- data_topn()
        readr::write_csv(
          df %>%
            dplyr::mutate(Item = as.character(Item)) %>%
            dplyr::select(
              Item,
              prod_kt, import_kt, export_kt, food_kt,
              dom_supply_kt,
              dep_share_pct, idr_pct, ssr_pct
            ),
          file
        )
      }
    )
    
    output$note <- renderUI({
      df_all <- data_items()
      if (is.null(df_all) || nrow(df_all) == 0) return(NULL)
      
      yr     <- year_for_scenario()
      sc     <- input$scenario_sel %||% ""
      sc_lbl <- if (nzchar(sc)) scenario_label(sc) else ""
      top_n  <- input$top_n %||% 15
      rk     <- input$rank_by %||% "imports"
      view   <- input$view_mode %||% "volumes"
      
      rk_txt <- dplyr::case_when(
        rk == "imports"     ~ "imports (Import Quantity)",
        rk == "food"        ~ "food quantities (Food)",
        rk == "production"  ~ "production (Production)",
        rk == "ratio"       ~ "Food dependency (Import / Food, %)",
        rk == "idr"         ~ "Import Dependency Ratio (IDR, %)",
        rk == "ssr"         ~ "Self-Sufficiency Ratio (SSR, %)",
        TRUE                ~ "imports (Import Quantity)"
      )
      
      what_txt <- dplyr::case_when(
        view == "volumes" ~ paste0(
          "The chart compares, for each product item, <strong>Imports</strong> and <strong>Food use</strong> (in physical quantities). ",
          "Each item is displayed with two side-by-side bars: one for <strong>Imports</strong> and one for <strong>Food</strong>."
        ),
        view == "ratio" ~ paste0(
          "The chart shows the <strong>Food dependency</strong> by item, defined as ",
          "<strong>Import / Food × 100</strong>. ",
          "Items with <strong>Food = 0</strong> are excluded because the ratio is not defined."
        ),
        view == "idr" ~ paste0(
          "The chart shows the <strong>Import Dependency Ratio (IDR)</strong> by item, defined as ",
          "<strong>Imports × 100 / (Production + Imports − Exports)</strong>. ",
          "The denominator is a proxy for <strong>domestic supply</strong>."
        ),
        view == "ssr" ~ paste0(
          "The chart shows the <strong>Self-Sufficiency Ratio (SSR)</strong> by item, defined as ",
          "<strong>Production × 100 / (Production + Imports − Exports)</strong>. ",
          "It uses the same domestic supply proxy as IDR."
        ),
        TRUE ~ ""
      )
      
      unit_txt <- if (view == "volumes") {
        "Quantities are plotted in <strong>million tonnes (Mt)</strong>. Tooltips automatically display <strong>kt</strong> or <strong>Mt</strong> depending on magnitude."
      } else {
        "Values are plotted in <strong>%</strong> (ratios). Tooltips show only the components required to compute the selected indicator."
      }
      
      txt <- glue::glue(
        "<p>
      This figure reports import dependency indicators <strong>by product item</strong> for
      <strong>{sc_lbl}</strong>.<br>
      {what_txt}<br>
      <strong>Top items:</strong> only the <strong>Top {top_n}</strong> items are displayed, based on the selected
      <strong>Ranking</strong> criterion (<strong>{rk_txt}</strong>).<br>
      {unit_txt}
    </p>"
      )
      
      htmltools::HTML(txt)
    }) %>% bindCache(r_country(), input$scenario_sel, year_for_scenario(), input$view_mode, input$rank_by, input$top_n)
    
  })
}
