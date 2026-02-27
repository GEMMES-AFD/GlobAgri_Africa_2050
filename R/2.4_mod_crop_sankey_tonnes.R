# R/2.4_mod_crop_sankey_tonnes.R
# -------------------------------------------------------------------

CROP_GROUPS_CROP_SANKEY <- list(
  "All crop products" = c(
    "Cake Other Oilcrops","Fibers etc.","Fruits and vegetables",
    "Maize","Millet and Sorghum","Oil Other Oilcrops","Oilpalm fruit",
    "Olive Oil","Olives","Other Oilcrops","Other cereals",
    "Other plant products","Other products","Palm Products Oil",
    "Palmkernel Cake","Pulses","Rape and Mustard Cake","Rape and Mustard Oil",
    "Rape and Mustardseed","Rice","Roots and Tuber","Soyabean Cake",
    "Soyabean Oil","Soyabeans","Sugar plants and products",
    "Sunflowerseed","Sunflowerseed Cake","Sunflowerseed Oil","Wheat"
  ),
  "Cereals" = c("Maize", "Millet and Sorghum", "Other cereals", "Rice", "Wheat"),
  "Roots and tubers" = c("Roots and Tuber"),
  "Pulses" = c("Pulses"),
  "Oilcrops (primary)" = c(
    "Oilpalm fruit", "Olives","Other Oilcrops","Rape and Mustardseed","Soyabeans","Sunflowerseed"
  ),
  "Fruits & vegetables" = c("Fruits and vegetables"),
  "Sugar crops"         = c("Sugar plants and products"),
  "Fibres & other products" = c("Fibers etc.", "Other plant products", "Other products")
)

CROP_LABELS_CROP_SANKEY <- c(
  "All crop products"             = "crop products",
  "Cereals"                       = "cereals",
  "Roots and tubers"              = "roots and tubers",
  "Pulses"                        = "pulses",
  "Oilcrops (primary)"            = "oilcrops",
  "Fruits & vegetables"           = "fruits and vegetables",
  "Sugar crops"                   = "sugar crops",
  "Fibres & other products"       = "fibre and other plant products"
)

order_scen_by_config <- function(x){
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  known   <- SCENARIO_LEVELS_DEFAULT[SCENARIO_LEVELS_DEFAULT %in% x]
  unknown <- setdiff(x, SCENARIO_LEVELS_DEFAULT)
  c(known, sort(unknown))
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

mod_crop_sankey_tonnes_ui <- function(id, plot_height = "500px"){
  ns <- NS(id)
  tagList(
    div(
      class = "card",
      div(
        class = "card-body",
        h2(textOutput(ns("title_domestic"))),
        tags$div(style="height:12px"),
        tags$label("Select a product group :", `for` = ns("prod_sel"), class = "form-label mb-1"),
        selectInput(
          ns("prod_sel"), NULL,
          choices  = names(CROP_GROUPS_CROP_SANKEY),
          selected = "All crop products",
          width    = "220px"
        ),
        radioButtons(
          ns("unit"),
          label    = NULL,
          choices  = c("Energy (Gcal)" = "energy", "Mass (tonnes)" = "mass"),
          selected = "energy",
          inline   = TRUE
        ),
        tags$div(style="height:8px"),
        h4(tags$em("(Click on the scenario box you want to see)")),
        uiOutput(ns("tiles")),
        tags$div(style="height:20px"),
        h2(textOutput(ns("title_flow"))),
        div(
          class = "d-flex gap-3 flex-wrap align-items-center",
          div(
            class = "ms-auto",
            checkboxInput(
              ns("as_pct"),
              "Show as percentage (%)",
              value = FALSE,
              width = "auto"
            )
          )
        ),
        
        plotly::plotlyOutput(ns("sankey"), height = plot_height),
        div(
          class = "text-right",
          div(
            class = "u-actions",
            downloadLink(ns("dl_csv"), label = tagList(icon("download"), "CSV"))
          )
        ),
        uiOutput(ns("note"))
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

mod_crop_sankey_tonnes_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,
    harvest_element = "Area harvested",
    exclude_items = c(
      "All products","All crops","Agricultural land occupation (Farm)",
      "Cropland","Forest land","Land under perm. meadows and pastures"
    ),
    value_multiplier = 1,
    value_multiplier_energy = 1,
    group_var = NULL
){
  moduleServer(id, function(input, output, session){
    
    `%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.numeric(a) && !is.finite(a))) b else a
    
    if (is.null(r_scenarios) || !is.function(r_scenarios)) {
      stop("mod_crop_sankey_tonnes_server(): 'r_scenarios' must be a reactive/function returning scenario CODES.")
    }
    if (!exists("scenario_label", mode = "function", inherits = TRUE)) {
      stop("mod_crop_sankey_tonnes_server(): missing dependency 'scenario_label(code)'.")
    }
    if (!exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      stop("mod_crop_sankey_tonnes_server(): missing dependency 'SCENARIO_LEVELS_DEFAULT'.")
    }
    
    if (!exists("APP_TRANSPARENT", inherits = TRUE)) {
      APP_TRANSPARENT <- "rgba(0,0,0,0)"
    }
    if (!exists("plotly_theme_transparent", mode = "function")) {
      plotly_theme_transparent <- function(p = NULL){
        if (is.null(p)) return(NULL)
        plotly::layout(p, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
      }
    }
    
    is_blank <- function(x) is.na(x) | trimws(x) == "" | trimws(x) == "NA"
    
    # Domestic supply from P/M/E (same plumbing)
    calc_ds_from_pme <- function(P, M, E){
      P <- pmax(0, P); M <- pmax(0, M); E <- pmax(0, E)
      
      exp_from_prod    <- pmax(0, pmin(P, E))
      exp_from_imp     <- pmax(0, E - exp_from_prod)
      to_dom_from_prod <- pmax(0, P - exp_from_prod)
      to_dom_from_imp  <- pmax(0, M - exp_from_imp)
      
      list(
        exp_from_prod    = exp_from_prod,
        exp_from_imp     = exp_from_imp,
        to_dom_from_prod = to_dom_from_prod,
        to_dom_from_imp  = to_dom_from_imp,
        DS_calc          = to_dom_from_prod + to_dom_from_imp
      )
    }
    
    # --- Elements mapping (Processing removed from USES) -------------------
    FACTMAP <- list(
      mass = list(
        P  = "Production",
        M  = "Import Quantity",
        E  = "Export Quantity",
        DS = "Domestic supply quantity",
        PROC = "Processing",
        uses = c("Feed","Food","Losses","Seed","Other uses (non-food)","Unused") # no Processing
      ),
      energy = list(
        P  = "Energy Production",
        M  = "Energy Import Quantity",
        E  = "Energy Export Quantity",
        DS = "Energy Domestic supply quantity",
        PROC = "Energy Processing",
        uses = c("Energy Food","Energy Feed","Energy Losses",
                 "Energy Other uses (non-food)", "Energy Unused", "Energy Processing") # no Energy Processing
      )
    )
    
    NODEMAP <- list(
      mass = list(
        uses_nodes = c("Feed","Food","Losses","Seed","Other uses (non-food)","Unused"),
        uses_fact  = c("Feed","Food","Losses","Seed","Other uses (non-food)","Unused")
      ),
      energy = list(
        uses_nodes = c("Food","Feed","Losses","Other uses (non-food)","Unused"),
        uses_fact  = c("Energy Food","Energy Feed","Energy Losses",
                       "Energy Other uses (non-food)","Energy Unused", "Energy Processing")
      )
    )
    
    unit_mode <- reactive({
      u <- input$unit %||% "energy"
      if (!u %in% c("energy","mass")) u <- "energy"
      u
    })
    
    unit_label <- reactive({
      if (identical(unit_mode(), "energy")) "Gcal" else "tonnes"
    })
    
    unit_symbol <- reactive({
      if (identical(unit_mode(), "energy")) "Gcal" else "t"
    })
    
    unit_multiplier <- reactive({
      if (identical(unit_mode(), "energy")) {
        as.numeric(value_multiplier %||% 1) * as.numeric(value_multiplier_energy %||% 1)
      } else {
        as.numeric(value_multiplier %||% 1) * 1000
      }
    })
    
    # --- Scénarios : ordre config -----------------------------------------
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
      if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
        b <- as.character(get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE))
        if (nzchar(b)) return(b)
      }
      b <- intersect(SCENARIO_LEVELS_DEFAULT, unique(as.character(r_scenarios())))
      b <- b[!is.na(b) & nzchar(b)]
      b[1] %||% NA_character_
    })
    
    # -----------------------------------------------------------------------
    # Sélection produits + titres
    # -----------------------------------------------------------------------
    items_selected <- reactive({
      grp <- input$prod_sel %||% "All crop products"
      CROP_GROUPS_CROP_SANKEY[[grp]] %||% CROP_GROUPS_CROP_SANKEY[["All crop products"]]
    })
    
    label_selected <- reactive({
      grp <- input$prod_sel %||% "All crop products"
      CROP_LABELS_CROP_SANKEY[[grp]] %||% "crop products"
    })
    
    output$title_domestic <- renderText({
      paste0("Domestic supply of ", label_selected(), " (", unit_label(), ")")
    })
    output$title_flow <- renderText({
      paste0("Total flow of ", label_selected(), " (", unit_label(), ")")
    })
    
    # -----------------------------------------------------------------------
    # Scénario sélectionné (code)
    # -----------------------------------------------------------------------
    r_selected <- reactiveVal(NULL)
    
    # -----------------------------------------------------------------------
    # bindCache keys
    # -----------------------------------------------------------------------
    cache_key_years <- reactive({
      req(r_country(), input$prod_sel, unit_mode())
      paste0(
        "crop_sankey|years|",
        r_country(), "|prod=", input$prod_sel,
        "|unit=", unit_mode(),
        "|sc=", paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    cache_key_tiles <- reactive({
      req(r_country(), input$prod_sel, unit_mode())
      paste0(
        "crop_sankey|tiles|",
        r_country(), "|prod=", input$prod_sel,
        "|unit=", unit_mode(),
        "|sc=", paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    cache_key_sankey_data <- reactive({
      req(r_country(), input$prod_sel, unit_mode())
      paste0(
        "crop_sankey|sankeydata|",
        r_country(), "|prod=", input$prod_sel,
        "|unit=", unit_mode(),
        "|sel=", (r_selected() %||% ""),
        "|sc=", paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    cache_key_plot <- reactive({
      req(r_country(), input$prod_sel, unit_mode())
      paste0(
        "crop_sankey|plot|",
        r_country(), "|prod=", input$prod_sel,
        "|unit=", unit_mode(),
        "|sel=", (r_selected() %||% ""),
        "|pct=", isTRUE(input$as_pct)
      )
    })
    
    # -----------------------------------------------------------------------
    # Scénarios réellement disponibles (selon unité) : basé sur DS (tous items)
    # -----------------------------------------------------------------------
    years_by_scenario <- reactive({
      sc_req <- scen_codes_ordered()
      req(length(sc_req) > 0, r_country(), items_selected(), unit_mode())
      
      ds_el <- FACTMAP[[unit_mode()]]$DS
      
      fact %>%
        filter(
          Region   == r_country(),
          Scenario %in% sc_req,
          Item     %in% items_selected(),
          stringr::str_trim(Element) == ds_el
        ) %>%
        group_by(Scenario) %>%
        summarise(
          year_used = suppressWarnings(max(Year[is.finite(Value)], na.rm = TRUE)),
          .groups   = "drop"
        ) %>%
        filter(is.finite(year_used)) %>%
        mutate(
          Scenario_code = as.character(Scenario),
          Scenario      = factor(Scenario_code, levels = scen_levels_all())
        ) %>%
        arrange(Scenario) %>%
        select(Scenario_code, Scenario, year_used)
    }) %>% bindCache(cache_key_years())
    
    scen_available <- reactive({
      yrs <- years_by_scenario()
      yrs$Scenario_code %||% character(0)
    }) %>% bindCache(cache_key_years())
    
    observeEvent(list(scen_available(), unit_mode()), {
      avail <- order_scen_by_config(scen_available())
      if (length(avail) == 0) return()
      cur <- r_selected()
      if (is.null(cur) || !cur %in% avail) r_selected(avail[1])
    }, ignoreInit = FALSE)
    
    observeEvent(input$tile_click, {
      req(!is.null(input$tile_click$code))
      code <- as.character(input$tile_click$code)
      if (nzchar(code) && code %in% scen_available()) r_selected(code)
    }, ignoreInit = TRUE)
    
    # -----------------------------------------------------------------------
    # KPI tiles: Domestic supply NET of processing, consistent with Sankey
    # -----------------------------------------------------------------------
    tiles_values <- reactive({
      rg   <- r_country()
      scs  <- order_scen_by_config(scen_available())
      req(nzchar(rg), length(scs) > 0, unit_mode())
      
      um   <- unit_mode()
      mult <- unit_multiplier()
      
      yrs_join <- years_by_scenario() %>%
        dplyr::transmute(Scenario = Scenario_code, year_used)
      
      elP    <- FACTMAP[[um]]$P
      elM    <- FACTMAP[[um]]$M
      elE    <- FACTMAP[[um]]$E
      elPROC <- FACTMAP[[um]]$PROC
      
      items_all <- items_selected()
      
      # M/E/PROC (all items)
      df_rest <- fact %>%
        dplyr::filter(
          Region   == rg,
          Scenario %in% scs,
          Item     %in% items_all,
          stringr::str_trim(Element) %in% c(elM, elE, elPROC),
          Year     %in% yrs_join$year_used,
          (is.na(System) | System == "" | System == "NA" | trimws(System) == ""),
          (is.na(Animal) | Animal == "" | Animal == "NA" | trimws(Animal) == "")
        ) %>%
        dplyr::inner_join(yrs_join, by = "Scenario") %>%
        dplyr::filter(Year == year_used) %>%
        dplyr::mutate(Element = stringr::str_trim(Element)) %>%
        dplyr::group_by(Scenario, Element) %>%
        dplyr::summarise(val = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = Element, values_from = val, values_fill = 0)
      
      # P (all items, both mass and energy)
      df_p <- fact %>%
        dplyr::filter(
          Region   == rg,
          Scenario %in% scs,
          Item     %in% items_all,
          stringr::str_trim(Element) == elP,
          Year     %in% yrs_join$year_used,
          (is.na(System) | System == "" | System == "NA" | trimws(System) == ""),
          (is.na(Animal) | Animal == "" | Animal == "NA" | trimws(Animal) == "")
        ) %>%
        dplyr::inner_join(yrs_join, by = "Scenario") %>%
        dplyr::filter(Year == year_used) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(P = sum(Value, na.rm = TRUE), .groups = "drop")
      
      df <- tibble::tibble(Scenario = scs) %>%
        dplyr::left_join(df_rest, by = "Scenario") %>%
        dplyr::left_join(df_p, by = "Scenario")
      
      if (!elM %in% names(df)) df[[elM]] <- 0
      if (!elE %in% names(df)) df[[elE]] <- 0
      if (!elPROC %in% names(df)) df[[elPROC]] <- 0
      if (!"P" %in% names(df)) df[["P"]] <- 0
      
      df <- df %>%
        dplyr::mutate(
          M_raw    = tidyr::replace_na(.data[[elM]], 0),
          E_raw    = tidyr::replace_na(.data[[elE]], 0),
          PROC_raw = tidyr::replace_na(.data[[elPROC]], 0),
          P_raw    = tidyr::replace_na(P, 0)
        )
      
      P <- mult * as.numeric(df$P_raw)
      M <- mult * as.numeric(df$M_raw)
      E <- mult * as.numeric(df$E_raw)
      PROC <- mult * as.numeric(df$PROC_raw)
      
      ds_parts <- calc_ds_from_pme(P, M, E)
      DS_calc <- ds_parts$DS_calc
      
      DS_net <- pmax(0, DS_calc - pmax(0, PROC))
      
      setNames(as.list(DS_net), df$Scenario)
    }) %>% bindCache(cache_key_tiles())
    
    baseline_value <- reactive({
      b <- baseline_code()
      vals <- tiles_values()
      if (is.na(b) || !nzchar(b)) return(NA_real_)
      v <- as.numeric(vals[[b]] %||% NA_real_)
      if (!is.finite(v)) return(NA_real_)
      v
    }) %>% bindCache(cache_key_tiles())
    
    tiles_deltas <- reactive({
      base <- baseline_value()
      vals <- tiles_values()
      setNames(
        lapply(vals, function(v){
          v <- as.numeric(v %||% NA_real_)
          if (is.finite(base) && base > 0 && is.finite(v)) 100 * (v - base) / base else NA_real_
        }),
        names(vals)
      )
    }) %>% bindCache(cache_key_tiles())
    
    output$tiles <- renderUI({
      ns <- session$ns
      vals <- tiles_values()
      dlt  <- tiles_deltas()
      sel  <- r_selected()
      u_lab <- unit_label()
      
      scen_list <- order_scen_by_config(names(vals))
      if (length(scen_list) == 0) return(NULL)
      
      make_tile <- function(sc_code){
        sc_lab <- scenario_label(sc_code)
        is_active <- identical(sel, sc_code)
        base_code <- baseline_code()
        
        tags$div(
          class = "u-box",
          tags$button(
            type  = "button",
            class = paste("u-card u-card--clickable u-card--focus", if (is_active) "is-active"),
            onclick = sprintf(
              "Shiny.setInputValue('%s', {code:'%s', nonce:Date.now()}, {priority:'event'});",
              ns("tile_click"), sc_code
            ),
            div(class = "u-title", sc_lab),
            div(
              class = "u-value",
              format(round(as.numeric(vals[[sc_code]] %||% 0)), big.mark = " ", scientific = FALSE),
              tags$span(class = "u-unit", u_lab)
            ),
            {
              if (identical(sc_code, base_code)) {
                div(class = "u-sub", HTML("&nbsp;"))
              } else {
                dv  <- as.numeric(dlt[[sc_code]] %||% NA_real_)
                cls <- if (is.finite(dv) && dv >= 0) "up" else "down"
                div(
                  class = "u-sub",
                  HTML(sprintf(
                    "Vs baseline : <span class='%s'>%s</span>",
                    cls,
                    if (is.finite(dv)) scales::percent(dv/100, accuracy = 0.1) else "—"
                  ))
                )
              }
            }
          )
        )
      }
      
      div(class = "u-row", lapply(scen_list, make_tile))
    })
    
    # -----------------------------------------------------------------------
    # Données Sankey
    # -----------------------------------------------------------------------
    make_sankey_data <- reactive({
      sc  <- r_selected(); req(nzchar(sc))
      reg <- r_country();  req(nzchar(reg))
      um  <- unit_mode();  req(um %in% c("mass","energy"))
      
      yrs <- years_by_scenario()
      year_used <- yrs$year_used[yrs$Scenario_code == sc][1] %||% NA
      validate(need(is.finite(year_used), "No elements configured for this unit."))
      
      mult <- unit_multiplier()
      
      items_all <- items_selected()
      
      elP    <- FACTMAP[[um]]$P
      elM    <- FACTMAP[[um]]$M
      elE    <- FACTMAP[[um]]$E
      elDS   <- FACTMAP[[um]]$DS
      elPROC <- FACTMAP[[um]]$PROC
      
      uses_fact  <- NODEMAP[[um]]$uses_fact
      uses_nodes <- NODEMAP[[um]]$uses_nodes
      
      # Pull all needed elements on ALL items
      elements_all <- unique(c(elP, elM, elE, elDS, elPROC, uses_fact))
      
      dat_all <- fact %>%
        dplyr::filter(
          Region   == reg,
          Scenario == sc,
          Item     %in% items_all,
          stringr::str_trim(Element) %in% elements_all,
          Year     == year_used,
          (is.na(System) | System == "" | System == "NA" | trimws(System) == ""),
          (is.na(Animal) | Animal == "" | Animal == "NA" | trimws(Animal) == "")
        ) %>%
        dplyr::mutate(Element = stringr::str_trim(Element))
      
      tot_all <- dat_all %>%
        dplyr::group_by(Element) %>%
        dplyr::summarise(val = sum(Value, na.rm = TRUE), .groups = "drop")
      
      g_all <- function(el){
        v <- tot_all$val[match(el, tot_all$Element)]
        ifelse(is.na(v), 0, as.numeric(v))
      }
      
      # P_raw: ALL items for BOTH modes
      P_raw <- g_all(elP)
      M_raw <- g_all(elM)
      E_raw <- g_all(elE)
      PROC_raw <- g_all(elPROC)
      
      P <- mult * P_raw
      M <- mult * M_raw
      E <- mult * E_raw
      PROC <- mult * PROC_raw
      
      # P/M/E plumbing
      ds_parts <- calc_ds_from_pme(P, M, E)
      
      exp_from_prod    <- ds_parts$exp_from_prod
      exp_from_imp     <- ds_parts$exp_from_imp
      to_dom_from_prod <- ds_parts$to_dom_from_prod
      to_dom_from_imp  <- ds_parts$to_dom_from_imp
      DS_calc          <- ds_parts$DS_calc
      
      # Remove Processing from the system (not counted, not shown)
      DS_net <- max(0, DS_calc - max(0, PROC))
      
      # Scale contributions to Domestic supply so that inflows sum to DS_net
      f <- if (is.finite(DS_calc) && DS_calc > 0) DS_net / DS_calc else 0
      to_dom_from_prod_net <- f * to_dom_from_prod
      to_dom_from_imp_net  <- f * to_dom_from_imp
      
      # Uses (NO processing element, all items)
      use_vals <- stats::setNames(rep(0, length(uses_nodes)), uses_nodes)
      for (i in seq_along(uses_nodes)) {
        use_vals[[uses_nodes[i]]] <- mult * g_all(uses_fact[i])
      }
      
      # Recompute Unused to close DS_net without counting Processing
      if (identical(um, "mass")) {
        uses_no_unused <- sum(use_vals[c("Feed","Food","Losses","Seed","Other uses (non-food)")], na.rm = TRUE)
        residual <- DS_net - uses_no_unused
        tol <- 1
        use_vals[["Unused"]] <- if (is.finite(residual) && residual > tol) residual else 0
      } else {
        uses_no_unused <- sum(use_vals[c("Food","Feed","Losses","Other uses (non-food)")], na.rm = TRUE)
        residual <- DS_net - uses_no_unused
        use_vals[["Unused"]] <- max(0, residual)
      }
      
      has_exports <- is.finite(E) && E > 0
      
      edges <- tibble::tibble(
        from  = c("Production","Imports","Production","Imports", rep("Domestic supply", length(uses_nodes))),
        to    = c("Exports","Exports","Domestic supply","Domestic supply", uses_nodes),
        value = c(exp_from_prod, exp_from_imp, to_dom_from_prod_net, to_dom_from_imp_net, unname(use_vals[uses_nodes]))
      )
      
      if (!has_exports) edges <- edges %>% dplyr::filter(to != "Exports")
      edges <- edges %>% dplyr::filter(is.finite(value), value > 0)
      validate(need(nrow(edges) > 0, "No flow available for this product group and scenario."))
      
      nodes_present <- unique(c(edges$from, edges$to))
      if (is.finite(DS_net) && DS_net > 0 && !("Domestic supply" %in% nodes_present)) {
        nodes_present <- c(nodes_present, "Domestic supply")
      }
      if (!("Food" %in% nodes_present)) nodes_present <- c(nodes_present, "Food")
      
      node_order <- if (identical(um, "energy")) {
        c("Production","Imports","Exports","Domestic supply",
          "Food","Feed","Losses","Other uses (non-food)", "Processing", "Unused")
      } else {
        c("Production","Imports","Exports","Domestic supply",
          "Feed","Food","Losses","Seed","Other uses (non-food)", "Processing", "Unused")
      }
      
      nodes_core <- node_order[node_order %in% nodes_present]
      
      # invisible anchor
      nds <- c(nodes_core, "Food__anchor")
      id  <- stats::setNames(seq_along(nds) - 1L, nds)
      
      src <- unname(id[edges$from])
      trg <- unname(id[edges$to])
      val_u <- as.numeric(edges$value)
      
      # shares: exports vs E; others vs DS_net
      trg_is_exports <- if (has_exports) (edges$to == "Exports") else rep(FALSE, nrow(edges))
      denom_link_real <- ifelse(trg_is_exports, E, DS_net)
      pct_link <- ifelse(denom_link_real > 0, edges$value / denom_link_real, NA_real_)
      
      # anchor link
      eps <- 1e-6
      src   <- c(src, id["Food"])
      trg   <- c(trg, id["Food__anchor"])
      val_u <- c(val_u, eps)
      pct_link <- c(pct_link, NA_real_)
      
      x_map <- c("Production"=0.05, "Imports"=0.05,
                 "Exports"=0.50, "Domestic supply"=0.50,
                 "Feed"=0.93, "Food"=0.93, "Losses"=0.93,
                 "Seed"=0.93, "Other uses (non-food)"=0.93,
                 "Unused"=0.93,
                 "Food__anchor"=0.98)
      
      y_map <- if (identical(um, "energy")) {
        c("Production"=0.20, "Imports"=0.70,
          "Exports"=0.88, "Domestic supply"=0.40,
          "Food"=0.22, "Feed"=0.10, "Losses"=0.45,
          "Other uses (non-food)"=0.78, "Unused"=0.92,
          "Food__anchor"=0.22)
      } else {
        c("Production"=0.20, "Imports"=0.70,
          "Exports"=0.88, "Domestic supply"=0.40,
          "Feed"=0.12, "Food"=0.48, "Losses"=0.72,
          "Seed"=0.82, "Other uses (non-food)"=0.90, "Unused"=0.96,
          "Food__anchor"=0.48)
      }
      
      # For node labels in absolute mode, display IN-SCOPE totals (net of processing)
      P_scope <- exp_from_prod + to_dom_from_prod_net
      M_scope <- exp_from_imp  + to_dom_from_imp_net
      
      list(
        unit_mode      = um,
        unit_label     = unit_label(),
        unit_symbol    = unit_symbol(),
        scenario_code  = sc,
        scenario_label = scenario_label(sc),
        year_used      = year_used,
        nodes          = nds,
        src            = src,
        trg            = trg,
        val_u          = val_u,
        pct_link       = pct_link,
        node_x         = unname(x_map[nds]),
        node_y         = unname(y_map[nds]),
        totals         = list(
          P_scope = P_scope,
          M_scope = M_scope,
          E = E,
          DS_net = DS_net,
          uses = use_vals,
          in_DS_prod = to_dom_from_prod_net,
          in_DS_imp  = to_dom_from_imp_net
        )
      )
    }) %>% bindCache(cache_key_sankey_data())
    
    # -----------------------------------------------------------------------
    # Plotly Sankey
    # -----------------------------------------------------------------------
    output$sankey <- plotly::renderPlotly({
      sd    <- make_sankey_data()
      nds   <- sd$nodes
      src   <- sd$src
      trg   <- sd$trg
      val_u <- sd$val_u
      as_pct <- isTRUE(input$as_pct)
      
      th <- if (exists("get_plotly_tokens", mode = "function", inherits = TRUE)) {
        get_plotly_tokens()
      } else {
        list(
          font_color    = "#111827",
          node_border   = "rgba(255,255,255,0.25)",
          hover_bg      = "rgba(17,24,39,0.95)",
          hover_font    = "#FFFFFF"
        )
      }
      
      node_cols <- if (exists("sankey_node_colors_for", inherits = TRUE)) {
        cols <- try(sankey_node_colors_for(nds), silent = TRUE)
        if (inherits(cols, "try-error") || length(cols) != length(nds) ||
            any(!nzchar(cols) | is.na(cols))) rep("#CCCCCC", length(nds)) else cols
      } else rep("#CCCCCC", length(nds))
      
      anchor_node_idx <- which(nds == "Food__anchor")
      if (length(anchor_node_idx) == 1L) node_cols[anchor_node_idx] <- "rgba(0,0,0,0)"
      
      link_cols <- try({
        if (exists("sankey_link_colors_from_src", inherits = TRUE)) {
          sankey_link_colors_from_src(nds[src + 1], alpha = 0.35)
        } else {
          base <- node_cols[src + 1]
          to_rgba <- function(hex, a = 0.35){
            if (is.na(hex) || !nzchar(hex)) return(sprintf("rgba(204,204,204,%.2f)", a))
            r <- strtoi(substr(hex, 2, 3), 16)
            g <- strtoi(substr(hex, 4, 5), 16)
            b <- strtoi(substr(hex, 6, 7), 16)
            sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, a)
          }
          vapply(base, to_rgba, character(1))
        }
      }, silent = TRUE)
      if (inherits(link_cols, "try-error") || length(link_cols) != length(src)) {
        link_cols <- rep("rgba(204,204,204,0.5)", length(src))
      }
      
      fmt_u <- function(x) format(round(x), big.mark = " ", scientific = FALSE)
      
      P_scope <- sd$totals$P_scope
      M_scope <- sd$totals$M_scope
      E       <- sd$totals$E
      DS_net  <- sd$totals$DS_net
      uses    <- sd$totals$uses
      in_DS_prod <- sd$totals$in_DS_prod
      in_DS_imp  <- sd$totals$in_DS_imp
      u_sym <- sd$unit_symbol
      
      sum_DS_E <- DS_net + E
      share_DS <- if (sum_DS_E > 0) DS_net/sum_DS_E else NA_real_
      share_E  <- if (sum_DS_E > 0) E/sum_DS_E      else NA_real_
      
      pct_or_dash <- function(x, denom){
        if (is.finite(denom) && denom > 0) scales::percent(x/denom, accuracy = 0.1) else "—"
      }
      
      mk_node_label <- function(name){
        if (name == "Food__anchor") return("")
        
        if (as_pct) {
          val_pct <- switch(
            name,
            "Production"        = pct_or_dash(in_DS_prod, DS_net),
            "Imports"           = pct_or_dash(in_DS_imp,  DS_net),
            "Exports"           = if (is.finite(share_E))  scales::percent(share_E,  accuracy = 0.1) else "—",
            "Domestic supply"   = if (is.finite(share_DS)) scales::percent(share_DS, accuracy = 0.1) else "—",
            "Feed"              = pct_or_dash(uses[["Feed"]],   DS_net),
            "Food"              = pct_or_dash(uses[["Food"]],   DS_net),
            "Losses"            = pct_or_dash(uses[["Losses"]], DS_net),
            "Seed"              = pct_or_dash(uses[["Seed"]],   DS_net),
            "Other uses (non-food)" = pct_or_dash(uses[["Other uses (non-food)"]], DS_net),
            "Unused"            = pct_or_dash(uses[["Unused"]], DS_net),
            "—"
          )
          paste0(name, "<br><span>", val_pct, "</span>")
        } else {
          val_num <- switch(
            name,
            "Production"        = P_scope,
            "Imports"           = M_scope,
            "Exports"           = E,
            "Domestic supply"   = DS_net,
            "Feed"              = uses[["Feed"]],
            "Food"              = uses[["Food"]],
            "Losses"            = uses[["Losses"]],
            "Seed"              = uses[["Seed"]],
            "Other uses (non-food)" = uses[["Other uses (non-food)"]],
            "Unused"            = uses[["Unused"]],
            NA_real_
          )
          if (!is.finite(val_num)) return("")
          paste0(name, "<br><span>", fmt_u(val_num), " ", u_sym, "</span>")
        }
      }
      
      node_label <- vapply(nds, mk_node_label, character(1))
      
      link_label <- if (as_pct) {
        paste0(
          nds[src + 1], " → ", nds[trg + 1],
          "<br>", scales::percent(sd$pct_link, accuracy = 0.1),
          " (", fmt_u(val_u), " ", u_sym, ")",
          "<extra></extra>"
        )
      } else {
        paste0(
          nds[src + 1], " → ", nds[trg + 1],
          "<br>", fmt_u(val_u), " ", u_sym,
          ifelse(is.finite(sd$pct_link),
                 paste0(" (", scales::percent(sd$pct_link, accuracy = 0.1), ")"), ""),
          "<extra></extra>"
        )
      }
      
      # anchor invisible link
      anchor_idx <- length(val_u)
      link_cols[anchor_idx]  <- "rgba(0,0,0,0)"
      link_label[anchor_idx] <- "<extra></extra>"
      
      p <- plotly::plot_ly(
        type = "sankey",
        arrangement = "snap",
        domain = list(x = c(0, 1), y = c(0.20, 0.98)),
        node = list(
          label = node_label,
          color = node_cols,
          x = sd$node_x,
          y = sd$node_y,
          pad = 25,
          thickness = 32,
          line = list(color = th$node_border, width = 0.5)
        ),
        link = list(
          source = src,
          target = trg,
          value  = val_u,
          color  = link_cols,
          hovertemplate = link_label
        )
      ) %>%
        plotly::layout(
          margin = list(l = 10, r = 30, t = 0, b = 45),
          font   = list(size = 12, color = th$font_color),
          paper_bgcolor = APP_TRANSPARENT,
          plot_bgcolor  = APP_TRANSPARENT,
          hoverlabel = list(
            bgcolor = th$hover_bg,
            font    = list(color = th$hover_font)
          )
        ) %>%
        plotly::config(displaylogo = FALSE)
      
      if (exists("plotly_apply_global_theme", mode = "function", inherits = TRUE)) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "none")
      } else {
        p <- plotly_theme_transparent(p)
      }
      
      p
    }) %>% bindCache(cache_key_plot())
    
    # -----------------------------------------------------------------------
    # Export CSV
    # -----------------------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        sprintf("sankey_%s_%s_%s_%s.csv",
                gsub("\\s+","_", r_country() %||% "country"),
                gsub("\\s+","_", scenario_label(r_selected() %||% "scenario")),
                gsub("\\s+","_", input$prod_sel %||% "All_crop_products"),
                unit_mode() %||% "unit")
      },
      content = function(file){
        sd  <- make_sankey_data()
        nds <- sd$nodes; src <- sd$src; trg <- sd$trg
        
        out <- tibble::tibble(
          Country        = r_country(),
          Scenario_code  = sd$scenario_code,
          Scenario_label = sd$scenario_label,
          Year           = sd$year_used,
          Product        = input$prod_sel %||% "All crop products",
          Unit           = sd$unit_label,
          Source         = nds[src + 1],
          Target         = nds[trg + 1],
          Value          = sd$val_u,
          Share_ref      = sd$pct_link
        )
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # -----------------------------------------------------------------------
    # Note
    # -----------------------------------------------------------------------
    # --- Only change: add one sentence in the note (English) --------------------
    # Replace ONLY the `output$note <- renderUI({ ... })` block by the one below.
    
    output$note <- renderUI({
      sd <- try(make_sankey_data(), silent = TRUE)
      if (inherits(sd, "try-error") || is.null(sd$nodes) || length(sd$nodes) == 0) return(NULL)
      
      unit_txt <- if (identical(sd$unit_mode, "energy")) "energy (Gcal)" else "mass (tonnes)"
      
      txt <- glue::glue(
        "<p>
This figure shows, for the selected country and product group, how <strong>crop products</strong>
flow through the agri-food system in <strong>{unit_txt}</strong>.
The tiles indicate the total <strong>domestic supply net of processing</strong> for each scenario in <strong>{sd$unit_label}</strong>.
</p>

<p>
If the diagram does not balance well in <strong>energy (Gcal)</strong> for some product groups,
please switch to <strong>mass (tonnes)</strong> for a more consistent check of the flows.
</p>

<p>
The radio buttons above the chart switch between two representations :
</p>
<ul>
  <li><strong>Energy</strong>:
      flows are expressed in Gcal.</li>
  <li><strong>Mass</strong>:
      flows are expressed in tonnes,
      by summing, for each element, all crop and livestock items
      (be careful: tonnes aggregate heterogeneous products, so the
      largest flows reflect volumes and composition effects;
      use <em>Energy (Gcal)</em> for nutritional interpretation).</li>
</ul>

<p>
When the option <strong>\"Show as percentage (%)\"</strong> is ticked,
node and link information is expressed as shares of three reference poles:
</p>
<ul>
  <li><strong>Sources</strong> (left side):
      the percentages at <em>Production</em> and <em>Imports</em>
      indicate the share of total sources, i.e. <em>Production + Imports</em>
      (equivalently <em>Domestic supply + Exports</em>).</li>
  <li><strong>Market balance</strong> (middle):
      the percentages at <em>Domestic supply</em> and <em>Exports</em>
      describe how total marketable quantities (<em>Domestic supply + Exports</em>)
      are split between internal and external uses.</li>
  <li><strong>Uses</strong> (right side):
      the percentages at <em>Food</em>, <em>Feed</em>, <em>Losses</em>,
      <em>Seed</em> and <em>Other uses (non-food)</em> show each use
      as a share of <em>Domestic supply</em>.</li>
</ul>"
      )
      
      htmltools::HTML(txt)
    })
    
    invisible(NULL)
  })
}