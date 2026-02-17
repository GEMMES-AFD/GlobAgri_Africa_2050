# R/3.7_mod_ls_sankey_tonnes.R
# -------------------------------------------------------------------

# --------- Groupes de produits animaux -------------------------------------
LIVESTOCK_GROUPS <- list(
  "All animal products" = c(
    "Bovine meat",
    "Pork meat",
    "Poultry meat",
    "Small ruminant meat",
    "Dairy",
    "Eggs",
    "Aquatic animal products"
  ),
  "Meat (all species)" = c(
    "Bovine meat",
    "Pork meat",
    "Poultry meat",
    "Small ruminant meat",
    "Aquatic animal products"
  ),
  "Ruminant meat & dairy" = c(
    "Bovine meat",
    "Small ruminant meat",
    "Dairy"
  ),
  "Monogastric meat" = c(
    "Pork meat",
    "Poultry meat"
  ),
  "Bovine meat"             = "Bovine meat",
  "Pork meat"               = "Pork meat",
  "Poultry meat"            = "Poultry meat",
  "Small ruminant meat"     = "Small ruminant meat",
  "Dairy"                   = "Dairy",
  "Eggs"                    = "Eggs",
  "Aquatic animal products" = "Aquatic animal products"
)

LIVESTOCK_LABELS <- c(
  "All animal products"      = "animal products",
  "Meat (all species)"       = "animal meat",
  "Ruminant meat & dairy"    = "ruminant meat and dairy products",
  "Monogastric meat"         = "monogastric meat",
  "Bovine meat"              = "bovine meat",
  "Pork meat"                = "pork meat",
  "Poultry meat"             = "poultry meat",
  "Small ruminant meat"      = "small ruminant meat",
  "Dairy"                    = "dairy products",
  "Eggs"                     = "eggs",
  "Aquatic animal products"  = "aquatic animal products"
)

# ---------------------------------------------------------------------------
# UI (ajout toggle unité)
# ---------------------------------------------------------------------------
mod_ls_sankey_tonnes_ui <- function(id, plot_height = "500px"){
  ns <- NS(id)
  tagList(
    div(class = "card",
        div(class = "card-body",
            h2(textOutput(ns("title_domestic"))),
            tags$div(style="height:8px"),
            div(class = "d-flex gap-3 flex-wrap align-items-center",
                tags$label("Product group", `for` = ns("prod_sel"), class = "form-label mb-1"),
                selectInput(
                  ns("prod_sel"), NULL,
                  choices  = names(LIVESTOCK_GROUPS),
                  selected = "Meat (all species)",
                  width    = "260px"
                ),
                radioButtons(
                  ns("unit"),
                  label    = NULL,
                  choices  = c("Mass (tonnes)" = "mass", "Energy (Gcal)" = "energy"), selected = "mass",
                  inline   = TRUE
                ),
                tags$div(style="height:8px"),
                h4(tags$em("(Click on the scenario box you want to see)")),
                uiOutput(ns("tiles")),
                tags$div(style="height:20px"),
            h2(textOutput(ns("title_flow")))
            ),
            # --- Toggle % -----------------------------------------
            div(class = "ms-auto",
              checkboxInput(ns("as_pct"), "Show as percentage (%)",
                            value = FALSE, width = "auto")
            ),
            plotly::plotlyOutput(ns("sankey"), height = plot_height),
            div(class = "text-right",
                div(class = "u-actions",
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
mod_ls_sankey_tonnes_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,
    value_multiplier = 1,
    value_multiplier_energy = 1
){
  moduleServer(id, function(input, output, session){
    
    # -----------------------------------------------------------------------
    # Helpers
    # -----------------------------------------------------------------------
    `%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.numeric(a) && !is.finite(a))) b else a
    
    # dépendances attendues (config scénarios)
    sc_code  <- if (exists("scenario_code", mode = "function", inherits = TRUE)) scenario_code else function(x) stringr::str_squish(as.character(x))
    sc_label <- if (exists("scenario_label", mode = "function", inherits = TRUE)) scenario_label else function(x) sc_code(x)
    
    scen_base_year_raw <- if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE) else sc_code("Année de base")
    scen_base_year     <- sc_code(scen_base_year_raw)  # ✅ code canonique
    
    scen_levels_default <- if (exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE) else NULL
    scen_ref <- if (exists("SCENARIO_REF_CODE", inherits = TRUE)) get("SCENARIO_REF_CODE", inherits = TRUE) else NULL
    
    unit_mode <- reactive({
      u <- input$unit %||% "mass"
      if (!u %in% c("energy","mass")) u <- "mass"
      u
    })
    unit_label  <- reactive(if (identical(unit_mode(), "energy")) "Gcal" else "tonnes")
    unit_symbol <- reactive(if (identical(unit_mode(), "energy")) "Gcal" else "t")
    
    unit_multiplier <- reactive({
      if (identical(unit_mode(), "energy")) {
        as.numeric(value_multiplier %||% 1) * as.numeric(value_multiplier_energy %||% 1)
      } else {
        # MASS: fact en 1000 tonnes -> tonnes
        as.numeric(value_multiplier %||% 1) * 1000
      }
    })
    
    # -----------------------------------------------------------------------
    # ✅ (1) Restriction des product groups quand unit = mass
    # -----------------------------------------------------------------------
    .choices_by_unit <- function(um){
      all <- names(LIVESTOCK_GROUPS)
      if (identical(um, "mass")) {
        # ✅ demande : exclure ces 2 groupes en mass
        setdiff(all, c("All animal products", "Ruminant meat & dairy"))
      } else {
        all
      }
    }
    
    observeEvent(unit_mode(), {
      um <- unit_mode()
      ch <- .choices_by_unit(um)
      cur <- isolate(input$prod_sel) %||% ""
      
      if (!(cur %in% ch)) {
        cur <- ch[1] %||% ""
      }
      
      updateSelectInput(
        session,
        "prod_sel",
        choices  = ch,
        selected = cur
      )
    }, ignoreInit = FALSE)
    
    # -----------------------------------------------------------------------
    # Elements + mappings (unit-aware)
    # -----------------------------------------------------------------------
    ELEMENTS_BY_UNIT <- list(
      mass = c(
        "Production","Import Quantity","Export Quantity",
        "Domestic supply quantity",
        "Feed","Food","Processing","Losses","Seed",
        "Other uses (non-food)","Unused"
      ),
      energy = c(
        "Energy Domestic supply quantity",
        "Energy Export Quantity",
        "Energy Food",
        "Energy Feed",
        "Energy Import Quantity",
        "Energy Losses",
        "Energy Other uses (non-food)",
        "Energy Processing",
        "Energy Production",
        "Energy Unused"
      )
    )
    
    FACTMAP <- list(
      mass = list(
        P  = "Production",
        M  = "Import Quantity",
        E  = "Export Quantity",
        DS = "Domestic supply quantity",
        uses_fact  = c("Feed","Food","Processing","Losses","Seed","Other uses (non-food)","Unused"),
        uses_nodes = c("Feed","Food","Processing","Losses","Seed","Other uses (non-food)","Unused")
      ),
      energy = list(
        P  = "Energy Production",
        M  = "Energy Import Quantity",
        E  = "Energy Export Quantity",
        DS = "Energy Domestic supply quantity",
        uses_fact  = c("Energy Food","Energy Feed","Energy Processing","Energy Losses","Energy Other uses (non-food)","Energy Unused"),
        uses_nodes = c("Food","Feed","Processing","Losses","Other uses (non-food)","Unused")
      )
    )
    
    # -----------------------------------------------------------------------
    # Sélection produits + titres
    # -----------------------------------------------------------------------
    items_selected <- reactive({
      grp <- input$prod_sel %||% "Meat (all species)"
      LIVESTOCK_GROUPS[[grp]] %||% LIVESTOCK_GROUPS[["Meat (all species)"]]
    })
    
    label_selected <- reactive({
      grp <- input$prod_sel %||% "Meat (all species)"
      LIVESTOCK_LABELS[[grp]] %||% "animal products"
    })
    
    output$title_domestic <- renderText({
      paste0("Domestic supply of ", label_selected(), " (", unit_label(), ")")
    })
    output$title_flow <- renderText({
      paste0("Flow of ", label_selected(), " (", unit_label(), ")")
    })
    
    # -----------------------------------------------------------------------
    # Scenarios (codes) + ordre stable
    # -----------------------------------------------------------------------
    r_scen_codes <- reactive({
      sc <- r_scenarios()
      sc <- sc_code(sc)
      sc[!is.na(sc) & nzchar(sc)]
    })
    
    r_scen_levels <- reactive({
      sc <- r_scen_codes()
      req(length(sc) > 0)
      
      if (!is.null(scen_levels_default) && length(scen_levels_default) > 0) {
        ord <- c(intersect(scen_levels_default, sc), setdiff(sc, scen_levels_default))
      } else {
        ord <- sc
      }
      unique(ord)
    })
    
    # sélection courante (code)
    r_selected <- reactiveVal(NULL)
    
    observeEvent(r_scen_levels(), {
      avail <- r_scen_levels()
      if (length(avail) == 0) return()
      
      cur <- r_selected()
      if (!is.null(cur) && cur %in% avail) return()
      
      base <- scen_base_year
      ref  <- if (!is.null(scen_ref)) sc_code(scen_ref) else NA_character_
      
      pick <- if (base %in% avail) {
        base
      } else if (is.character(ref) && nzchar(ref) && ref %in% avail) {
        ref
      } else {
        avail[1]
      }
      
      r_selected(pick)
    }, ignoreInit = FALSE)
    
    observeEvent(input$tile_clicked, {
      code <- input$tile_clicked$code %||% ""
      code <- sc_code(code)
      if (nzchar(code)) r_selected(code)
    }, ignoreInit = TRUE)
    
    # -----------------------------------------------------------------------
    # KPI DS (unit-aware) + delta vs baseline year (SCENARIO_BASE_YEAR_CODE)
    # -----------------------------------------------------------------------
    sum_by_scen <- function(sc, country, um){
      ds_el <- FACTMAP[[um]]$DS
      
      fact %>%
        dplyr::filter(
          Region == !!country,
          Scenario == !!sc,
          Item %in% items_selected(),
          stringr::str_trim(Element) == ds_el
        ) %>%
        dplyr::summarise(val = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(val) %||% 0
    }
    
    key_tiles <- reactive({
      paste0(
        sc_code(r_country() %||% ""),
        "||", (input$prod_sel %||% ""),
        "||unit=", (unit_mode() %||% ""),
        "||", paste(r_scen_levels(), collapse = "|")
      )
    })
    
    # ✅ FIX % erronés : baseline doit dépendre (country + prod + unit) (sinon cache faux)
    baseline_value <- reactive({
      req(r_country(), unit_mode())
      (sum_by_scen(scen_base_year, r_country(), unit_mode()) * unit_multiplier()) %||% 0
    }) %>% shiny::bindCache(
      sc_code(r_country() %||% ""),
      input$prod_sel %||% "",
      unit_mode() %||% ""
    )
    
    tiles_values <- reactive({
      rg <- r_country(); req(rg, unit_mode())
      scs <- r_scen_levels()
      mult <- unit_multiplier()
      setNames(lapply(scs, function(sc) sum_by_scen(sc, rg, unit_mode()) * mult), scs)
    }) %>% shiny::bindCache(key_tiles())
    
    tiles_deltas <- reactive({
      base <- baseline_value()
      vals <- tiles_values()
      
      setNames(
        lapply(names(vals), function(sc){
          v <- vals[[sc]] %||% NA_real_
          # ✅ baseline = SCENARIO_BASE_YEAR_CODE (scen_base_year)
          if (identical(sc, scen_base_year)) return(NA_real_)
          if (is.finite(base) && base > 0 && is.finite(v)) 100 * (v - base) / base else NA_real_
        }),
        names(vals)
      )
    }) %>% shiny::bindCache(key_tiles())
    
    # -----------------------------------------------------------------------
    # Tuiles KPI
    # -----------------------------------------------------------------------
    output$tiles <- renderUI({
      ns <- session$ns
      vals <- tiles_values()
      dlt  <- tiles_deltas()
      sel  <- r_selected()
      
      scen_list <- names(vals)
      if (length(scen_list) == 0) return(NULL)
      
      make_tile <- function(sc_code_i){
        sc_code_i <- sc_code(sc_code_i)
        is_active <- identical(sel, sc_code_i)
        
        sc_lbl   <- sc_label(sc_code_i)
        base_lbl <- sc_label(scen_base_year)
        
        payload <- jsonlite::toJSON(
          list(code = sc_code_i, nonce = as.numeric(Sys.time())),
          auto_unbox = TRUE
        )
        
        tags$div(
          class = "u-box",
          tags$button(
            type  = "button",
            class = paste(
              "u-card u-card--clickable u-card--focus",
              if (is_active) "is-active"
            ),
            onclick = sprintf(
              "Shiny.setInputValue('%s', %s, {priority:'event'});",
              ns("tile_clicked"),
              payload
            ),
            
            div(class = "u-title", sc_lbl),
            
            div(
              class = "u-value",
              format(round(vals[[sc_code_i]] %||% 0),
                     big.mark = " ", scientific = FALSE),
              tags$span(class = "u-unit", unit_label())
            ),
            
            {
              if (identical(sc_code_i, scen_base_year)) {
                div(class = "u-sub", style = "visibility:hidden;", htmltools::HTML("&nbsp;"))
              } else {
                dv  <- dlt[[sc_code_i]]
                cls <- if (is.finite(dv) && dv >= 0) "up" else "down"
                div(
                  class = "u-sub",
                  htmltools::HTML(sprintf(
                    "Vs %s : <span class='%s'>%s</span>",
                    base_lbl,
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
    # Données Sankey (unit-aware, incl. Processing + Unused)
    # -----------------------------------------------------------------------
    key_sankey <- reactive({
      paste0(
        sc_code(r_country() %||% ""),
        "||", (r_selected() %||% ""),
        "||", (input$prod_sel %||% ""),
        "||unit=", (unit_mode() %||% "")
      )
    })
    
    make_sankey_data <- reactive({
      sc  <- r_selected();  req(nzchar(sc))
      reg <- r_country();   req(nzchar(reg))
      um  <- unit_mode();   req(um %in% c("mass","energy"))
      
      elements_needed <- ELEMENTS_BY_UNIT[[um]] %||% character(0)
      validate(shiny::need(length(elements_needed) > 0, "No elements configured for this unit."))
      
      dat0 <- fact %>%
        dplyr::filter(
          Region   == reg,
          Scenario == sc,
          Item %in% items_selected(),
          stringr::str_trim(Element) %in% elements_needed,
          (is.na(System) | System == "" | System == "NA" | trimws(System) == ""),
          (is.na(Animal) | Animal == "" | Animal == "NA" | trimws(Animal) == "")
        )
      
      ds_el <- FACTMAP[[um]]$DS
      
      year_used <- dat0 %>%
        dplyr::filter(stringr::str_trim(Element) == ds_el, is.finite(Value)) %>%
        dplyr::summarise(year_used = suppressWarnings(max(Year, na.rm = TRUE))) %>%
        dplyr::pull(year_used)
      validate(shiny::need(is.finite(year_used), "Aucune année disponible pour ce scénario / cette unité."))
      
      tot <- dat0 %>%
        dplyr::filter(Year == year_used) %>%
        dplyr::mutate(Element = stringr::str_trim(Element)) %>%
        dplyr::group_by(Element) %>%
        dplyr::summarise(val = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = Element, values_from = val, values_fill = 0)
      
      g  <- function(nm) as.numeric(tot[[nm]] %||% 0)
      mult <- unit_multiplier()
      
      P  <- mult * g(FACTMAP[[um]]$P)
      M  <- mult * g(FACTMAP[[um]]$M)
      E  <- mult * g(FACTMAP[[um]]$E)
      DS <- mult * g(FACTMAP[[um]]$DS)
      
      uses_fact  <- FACTMAP[[um]]$uses_fact
      uses_nodes <- FACTMAP[[um]]$uses_nodes
      
      uses <- stats::setNames(rep(0, length(uses_nodes)), uses_nodes)
      for (i in seq_along(uses_nodes)) {
        uses[[uses_nodes[i]]] <- mult * g(uses_fact[i])
      }
      
      exp_from_prod    <- max(0, min(P, E))
      exp_from_imp     <- max(0, E - exp_from_prod)
      to_dom_from_prod <- max(0, P - exp_from_prod)
      to_dom_from_imp  <- max(0, M - exp_from_imp)
      
      DS_calc <- to_dom_from_prod + to_dom_from_imp
      if (is.finite(DS) && abs(DS_calc - DS) > 1e-6) DS <- DS_calc
      
      uses_no_unused <- setdiff(uses_nodes, "Unused")
      residual <- DS - sum(uses[uses_no_unused], na.rm = TRUE)
      
      tol <- if (identical(um, "mass")) 1 else 0
      if (is.finite(residual) && residual > tol) {
        uses[["Unused"]] <- residual
      } else {
        uses[["Unused"]] <- 0
      }
      
      has_exports <- is.finite(E) && E > 0
      
      edges <- tibble::tibble(
        from  = c("Production","Imports","Production","Imports", rep("Domestic supply", length(uses_nodes))),
        to    = c("Exports","Exports","Domestic supply","Domestic supply", uses_nodes),
        value = c(exp_from_prod, exp_from_imp, to_dom_from_prod, to_dom_from_imp, unname(uses[uses_nodes]))
      )
      
      if (!has_exports) {
        edges <- edges %>% dplyr::filter(to != "Exports")
      }
      
      edges <- edges %>% dplyr::filter(is.finite(value), value > 0)
      validate(shiny::need(nrow(edges) > 0, "No flow available for this product group and scenario."))
      
      nodes_present <- unique(c(edges$from, edges$to))
      node_order <- if (identical(um, "energy")) {
        c("Production","Imports","Exports","Domestic supply",
          "Food","Feed","Processing","Losses","Other uses (non-food)","Unused")
      } else {
        c("Production","Imports","Exports","Domestic supply",
          "Feed","Food","Processing","Losses","Seed","Other uses (non-food)","Unused")
      }
      nodes_core <- node_order[node_order %in% nodes_present]
      
      nds <- c(nodes_core, "Food__anchor")
      id  <- stats::setNames(seq_along(nds) - 1L, nds)
      
      src <- unname(id[edges$from])
      trg <- unname(id[edges$to])
      val_u <- edges$value
      
      if (has_exports) {
        trg_is_exports <- edges$to == "Exports"
      } else {
        trg_is_exports <- rep(FALSE, nrow(edges))
      }
      denom_link_real <- ifelse(trg_is_exports, E, DS)
      pct_link <- ifelse(denom_link_real > 0, val_u / denom_link_real, NA_real_)
      
      eps <- 1e-6
      src   <- c(src, id["Food"])
      trg   <- c(trg, id["Food__anchor"])
      val_u <- c(val_u, eps)
      pct_link <- c(pct_link, NA_real_)
      
      x_map <- c("Production"=0.05, "Imports"=0.05,
                 "Exports"=0.50, "Domestic supply"=0.50,
                 "Feed"=0.93, "Food"=0.93, "Processing"=0.93, "Losses"=0.93,
                 "Seed"=0.93, "Other uses (non-food)"=0.93, "Unused"=0.93,
                 "Food__anchor"=0.98)
      
      y_map <- if (identical(um, "energy")) {
        c("Production"=0.20, "Imports"=0.70,
          "Exports"=0.88, "Domestic supply"=0.40,
          "Feed"=0.10, "Food"=0.22, "Processing"=0.55, "Losses"=0.68,
          "Other uses (non-food)"=0.82, "Unused"=0.92,
          "Food__anchor"=0.22)
      } else {
        c("Production"=0.20, "Imports"=0.70,
          "Exports"=0.88, "Domestic supply"=0.40,
          "Feed"=0.10, "Food"=0.30, "Processing"=0.52, "Losses"=0.66,
          "Seed"=0.78, "Other uses (non-food)"=0.88, "Unused"=0.95,
          "Food__anchor"=0.30)
      }
      
      list(
        unit_mode   = um,
        unit_label  = unit_label(),
        unit_symbol = unit_symbol(),
        scenario_code  = sc,
        scenario_label = sc_label(sc),
        year_used = year_used,
        nodes    = nds,
        src      = src,
        trg      = trg,
        val_u    = as.numeric(val_u),
        pct_link = pct_link,
        node_x   = unname(x_map[nds]),
        node_y   = unname(y_map[nds]),
        totals   = list(
          P=P, M=M, E=E, DS=DS,
          uses = uses,
          in_DS_prod = to_dom_from_prod, in_DS_imp = to_dom_from_imp
        )
      )
    }) %>% shiny::bindCache(key_sankey())
    
    # -----------------------------------------------------------------------
    # Plotly Sankey
    # -----------------------------------------------------------------------
    output$sankey <- plotly::renderPlotly({
      sd  <- make_sankey_data()
      nds <- sd$nodes
      src <- sd$src
      trg <- sd$trg
      val_u <- sd$val_u
      as_pct <- isTRUE(input$as_pct)
      
      th <- if (exists("get_plotly_tokens", mode = "function", inherits = TRUE)) get_plotly_tokens() else list(
        font_color     = "#111827",
        muted_color    = "#6B7280",
        axis_linecolor = "rgba(0,0,0,.18)"
      )
      
      node_cols <- if (exists("sankey_node_colors_for", inherits = TRUE)) {
        cols <- try(sankey_node_colors_for(nds), silent = TRUE)
        if (inherits(cols, "try-error") || length(cols) != length(nds) ||
            any(!nzchar(cols) | is.na(cols))) rep("#CCCCCC", length(nds)) else cols
      } else rep("#CCCCCC", length(nds))
      
      anchor_node_idx <- which(nds == "Food__anchor")
      if (length(anchor_node_idx) == 1L) {
        node_cols[anchor_node_idx] <- "rgba(0,0,0,0)"
      }
      
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
      if (inherits(link_cols, "try-error") || length(link_cols) != length(src))
        link_cols <- rep("rgba(204,204,204,0.5)", length(src))
      
      fmt_u <- function(x) format(round(x), big.mark = " ", scientific = FALSE)
      
      P  <- sd$totals$P; M <- sd$totals$M; E <- sd$totals$E; DS <- sd$totals$DS
      uses <- sd$totals$uses
      in_DS_prod <- sd$totals$in_DS_prod
      in_DS_imp  <- sd$totals$in_DS_imp
      u_sym <- sd$unit_symbol
      
      sum_DS_E <- DS + E
      share_DS <- if (sum_DS_E > 0) DS/sum_DS_E else NA_real_
      share_E  <- if (sum_DS_E > 0) E/sum_DS_E  else NA_real_
      
      pct_or_dash <- function(x, denom){
        if (is.finite(denom) && denom > 0) scales::percent(x/denom, accuracy = 0.1) else "—"
      }
      
      mk_node_label <- function(name){
        if (name == "Food__anchor") return("")
        
        if (as_pct) {
          val_pct <- switch(
            name,
            "Production"      = pct_or_dash(in_DS_prod, DS),
            "Imports"         = pct_or_dash(in_DS_imp,  DS),
            "Exports"         = if (is.finite(share_E))  scales::percent(share_E,  accuracy = 0.1) else "—",
            "Domestic supply" = if (is.finite(share_DS)) scales::percent(share_DS, accuracy = 0.1) else "—",
            "Food"            = pct_or_dash(uses[["Food"]], DS),
            "Feed"            = pct_or_dash(uses[["Feed"]], DS),
            "Processing"      = pct_or_dash(uses[["Processing"]], DS),
            "Losses"          = pct_or_dash(uses[["Losses"]], DS),
            "Seed"            = pct_or_dash(uses[["Seed"]], DS),
            "Other uses (non-food)" = pct_or_dash(uses[["Other uses (non-food)"]], DS),
            "Unused"          = pct_or_dash(uses[["Unused"]], DS),
            "—"
          )
          paste0(name, "<br><span>", val_pct, "</span>")
        } else {
          val_num <- switch(
            name,
            "Production"      = P,
            "Imports"         = M,
            "Exports"         = E,
            "Domestic supply" = DS,
            "Food"            = uses[["Food"]],
            "Feed"            = uses[["Feed"]],
            "Processing"      = uses[["Processing"]],
            "Losses"          = uses[["Losses"]],
            "Seed"            = uses[["Seed"]],
            "Other uses (non-food)" = uses[["Other uses (non-food)"]],
            "Unused"          = uses[["Unused"]],
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
      
      anchor_idx <- length(val_u)
      link_cols[anchor_idx]  <- "rgba(0,0,0,0)"
      link_label[anchor_idx] <- "<extra></extra>"
      
      node_border_col <- th$axis_linecolor %||% "rgba(0,0,0,0.25)"
      
      p <- plotly::plot_ly(
        type = "sankey",
        arrangement = "snap",
        domain = list(x = c(0, 1), y = c(0.20, 0.98)),
        node = list(
          label = node_label,
          color = node_cols,
          x = sd$node_x,
          y = sd$node_y,
          pad = 22,
          thickness = 32,
          line = list(color = node_border_col, width = 0.5),
          hovertemplate = "%{label}<extra></extra>"
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
          margin = list(l = 10, r = 30, t = 10, b = 45),
          font = list(size = 12, color = th$font_color %||% "#111827")
        ) %>%
        plotly::config(displaylogo = FALSE)
      
      if (exists("plotly_apply_global_theme", mode = "function", inherits = TRUE)) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "none")
      }
      
      p
    })
    
    # -----------------------------------------------------------------------
    # Export CSV (unit-aware)
    # -----------------------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        sprintf("sankey_livestock_%s_%s_%s_%s.csv",
                gsub("\\s+","_", r_country() %||% "country"),
                gsub("\\s+","_", sc_label(r_selected() %||% "scenario")),
                gsub("\\s+","_", input$prod_sel %||% "product_group"),
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
          Product        = input$prod_sel %||% "product_group",
          Unit           = sd$unit_label,
          Source         = nds[src + 1],
          Target         = nds[trg + 1],
          Value          = sd$val_u,
          Value_tonnes   = if (identical(sd$unit_mode, "mass")) sd$val_u else NA_real_,
          Value_Gcal     = if (identical(sd$unit_mode, "energy")) sd$val_u else NA_real_,
          Share_ref      = sd$pct_link
        )
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # -----------------------------------------------------------------------
    # Note explicative
    # -----------------------------------------------------------------------
    output$note <- renderUI({
      sd <- try(make_sankey_data(), silent = TRUE)
      if (inherits(sd, "try-error") || is.null(sd$nodes) || length(sd$nodes) == 0) {
        return(NULL)
      }
      
      unit_txt <- if (identical(sd$unit_mode, "energy")) "energy (Gcal)" else "mass (tonnes)"
      base_lbl <- sc_label(scen_base_year)
      
      # Seed n’existe que dans le mode mass dans ton mapping
      seed_txt <- if (identical(sd$unit_mode, "mass")) "Seed" else NULL
      uses_list <- c("Food", "Feed", "Processing", "Losses", seed_txt, "Other uses (non-food)", "Unused")
      uses_list <- uses_list[!is.null(uses_list) & nzchar(uses_list)]
      
      txt <- glue::glue(
        "<p>
      This figure shows, for the selected country and product group, how <strong>livestock products</strong>
      flow through the agri-food system in <strong>{unit_txt}</strong>.
    </p>

    <p>
      The tiles above the diagram indicate, for each scenario, the total <strong>domestic supply</strong> of the selected products
      in <strong>{sd$unit_label}</strong>, together with the percentage change compared with <strong>{base_lbl}</strong>.
    </p>

    <p><strong>Representation</strong></p>
    <ul>
      <li><strong>Energy</strong>: flows are expressed in Gcal.</li>
      <li><strong>Mass</strong>: flows are expressed in tonnes, by summing, for each element, all selected livestock items
          (note: tonnes aggregate heterogeneous products, so the largest flows reflect volumes and composition effects;
          use Energy (Gcal) for nutritional interpretation).</li>
    </ul>

    <p><strong>Reading the Sankey</strong></p>
    <ul>
      <li>The diagram details the selected scenario (<strong>{sd$scenario_label}</strong>, <strong>{sd$year_used}</strong>).</li>
      <li>Flows go from <em>Production</em> and <em>Imports</em> to <em>Domestic supply</em> and <em>Exports</em>,
          and then to final uses (<em>{paste(uses_list, collapse = \"</em>, <em>\")}</em>).</li>
    </ul>

    <p><strong>Show as percentage (%)</strong></p>
    <ul>
      <li><strong>Sources</strong> (left side): percentages at Production and Imports indicate the share of total sources,
          i.e. Production + Imports (equivalently Domestic supply + Exports).</li>
      <li><strong>Market balance</strong> (middle): percentages at Domestic supply and Exports describe how total marketable quantities
          (Domestic supply + Exports) are split between internal and external uses.</li>
      <li><strong>Uses</strong> (right side): percentages at Food, Feed, Processing, Losses and Other uses (non-food)
          show each use as a share of Domestic supply.</li>
      <li>Tooltips always include underlying volumes in <strong>{sd$unit_label}</strong>.</li>
    </ul>

    <p><strong>Balancing item</strong></p>
    <ul>
      <li><em>Unused</em> is displayed as the balancing item when needed: it captures the residual between <em>Domestic supply</em>
          and the sum of other internal uses.</li>
    </ul>"
      )
      
      htmltools::HTML(txt)
    })
    
    
    return(list(selected_scenario = reactive(r_selected())))
  })
}
