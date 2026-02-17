# R/1.3_mod_dsq_cards.R

mod_dsq_cards_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2(class = "section-title", textOutput(ns("title"))),
    tags$br(),
    h4(tags$em("(Click on the scenario box you want to see)")),
    
    div(id = ns("root"), uiOutput(ns("cards_row"))),
    tags$script(HTML(sprintf("
      $(document).on('click', '#%s .dsq-card', function(){
        var scen = $(this).data('scenario') || null;
        Shiny.setInputValue('%s', scen, {priority:'event'});
      });
      $(document).on('keydown', '#%s .dsq-card', function(e){
        if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); $(this).click(); }
      });
    ", ns("root"), ns("dsq_card_clicked"), ns("root"))))
  )
}

mod_dsq_cards_server <- function(
    id,
    fact,
    r_country,
    r_flow_mode = NULL,
    r_scenarios_effective = NULL,
    unit_label = "Gcal"
){
  moduleServer(id, function(input, output, session){
    
    fmt_int <- function(x){
      ifelse(is.finite(x),
             format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE),
             "—")
    }
    
    # --- Config fallbacks (CODES) -----------------------------------------
    scen_base_year <- if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
      SCENARIO_BASE_YEAR_CODE
    } else {
      scenario_code("Année de base")
    }
    
    diet_levels <- if (exists("SCENARIOS_BASE_CODES", inherits = TRUE)) {
      SCENARIOS_BASE_CODES
    } else {
      scenario_code(c("Même diète","Diète probable","Diète saine"))
    }
    
    extra_levels <- if (exists("SCENARIOS_EXTRA_CODES", inherits = TRUE)) {
      SCENARIOS_EXTRA_CODES
    } else {
      scenario_code(unname(if (exists("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)) SCENARIOS_EXTRA_CHOICES else c()))
    }
    
    diet_plus_extra <- unique(c(diet_levels, extra_levels))
    
    # --- Mode énergie / masse ---------------------------------------------
    flow_mode <- reactive({
      if (is.null(r_flow_mode)) {
        "energy"
      } else {
        v <- r_flow_mode()
        if (is.null(v) || !v %in% c("energy","mass")) "energy" else v
      }
    })
    
    output$title <- renderText({
      if (flow_mode() == "mass") "Total domestic supply (tons)" else "Total domestic supply (Gcal)"
    })
    
    # --- Scénarios à afficher (CODES), ordre = config ----------------------
    r_diet_scen_order <- reactive({
      eff <- NULL
      if (!is.null(r_scenarios_effective)) {
        eff <- try(r_scenarios_effective(), silent = TRUE)
        if (inherits(eff, "try-error")) eff <- NULL
      }
      eff <- as.character(eff %||% character(0))
      
      # Retire baseline
      eff_diets <- setdiff(eff, scen_base_year)
      
      # Ordre = diet_plus_extra (issu config)
      out <- diet_plus_extra[diet_plus_extra %in% eff_diets]
      
      # Fallback : 3 diètes de base si rien
      if (!length(out)) out <- diet_levels
      out
    })
    
    cards_df <- reactive({
      reg <- r_country(); req(nzchar(reg))
      mode <- flow_mode()
      scen_order <- r_diet_scen_order()
      req(length(scen_order) > 0)
      
      if (mode == "mass") {
        # baseline 2018 (tonnes) : Element == "Domestic supply quantity"
        base18 <- fact %>%
          dplyr::filter(
            Region   == reg,
            Scenario == scen_base_year,
            Element  == "Domestic supply quantity",
            Year     == 2018
          ) %>%
          dplyr::summarise(v = sum(Value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::pull(v)
        base18 <- if (!length(base18)) NA_real_ else as.numeric(base18) * 1000
        
        last_year <- fact %>%
          dplyr::filter(
            Region   == reg,
            Element  == "Domestic supply quantity",
            Scenario %in% scen_order
          ) %>%
          dplyr::group_by(Scenario) %>%
          dplyr::summarise(
            Year = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
            .groups = "drop"
          )
        
        vals <- fact %>%
          dplyr::filter(
            Region   == reg,
            Element  == "Domestic supply quantity",
            Scenario %in% scen_order
          ) %>%
          dplyr::inner_join(last_year, by = c("Scenario","Year")) %>%
          dplyr::group_by(Scenario) %>%
          dplyr::summarise(value = sum(Value, na.rm = TRUE) * 1000, .groups = "drop")
        
      } else {
        # énergie (Gcal) : Element == "Energy Domestic supply quantity", Item == "All", Unit == unit_label
        base18 <- fact %>%
          dplyr::filter(
            Region   == reg,
            Scenario == scen_base_year,
            Element  == "Energy Domestic supply quantity",
            Year     == 2018
          )
        if ("Unit" %in% names(base18)) base18 <- base18 %>% dplyr::filter(Unit == unit_label)
        if ("Item" %in% names(base18)) base18 <- base18 %>% dplyr::filter(Item == "All")
        
        base18 <- base18 %>%
          dplyr::summarise(v = sum(Value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::pull(v)
        base18 <- if (!length(base18)) NA_real_ else as.numeric(base18)
        
        last_year <- fact %>%
          dplyr::filter(
            Region   == reg,
            Element  == "Energy Domestic supply quantity",
            Scenario %in% scen_order
          )
        if ("Unit" %in% names(last_year)) last_year <- last_year %>% dplyr::filter(Unit == unit_label)
        if ("Item" %in% names(last_year)) last_year <- last_year %>% dplyr::filter(Item == "All")
        
        last_year <- last_year %>%
          dplyr::group_by(Scenario) %>%
          dplyr::summarise(
            Year = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
            .groups = "drop"
          )
        
        vals <- fact %>%
          dplyr::filter(
            Region   == reg,
            Element  == "Energy Domestic supply quantity",
            Scenario %in% scen_order
          )
        if ("Unit" %in% names(vals)) vals <- vals %>% dplyr::filter(Unit == unit_label)
        if ("Item" %in% names(vals)) vals <- vals %>% dplyr::filter(Item == "All")
        
        vals <- vals %>%
          dplyr::inner_join(last_year, by = c("Scenario","Year")) %>%
          dplyr::group_by(Scenario) %>%
          dplyr::summarise(value = sum(Value, na.rm = TRUE), .groups = "drop")
      }
      
      vals <- vals %>%
        dplyr::mutate(
          delta = if (is.finite(base18) && base18 > 0) (value - base18) / base18 else NA_real_
        )
      
      vals$Scenario <- factor(as.character(vals$Scenario), levels = scen_order)
      vals %>% dplyr::arrange(Scenario)
    }) %>% bindCache(
      r_country(),
      flow_mode(),
      paste(r_diet_scen_order(), collapse = "|")
    )
    
    # --- Sélection ---------------------------------------------------------
    selected <- reactiveVal(NULL)
    observeEvent(input$dsq_card_clicked, {
      scen <- input$dsq_card_clicked
      cur  <- selected()
      if (!is.null(cur) && identical(cur, scen)) selected(NULL) else selected(scen)
    })
    
    observe({
      df <- cards_df()
      if (is.null(selected()) && nrow(df)) {
        pref <- scenario_code("Même diète")
        scen_char <- as.character(df$Scenario)
        if (!(pref %in% scen_char)) pref <- scen_char[1]
        selected(pref)
      }
    })
    
    # --- UI cards ----------------------------------------------------------
    output$cards_row <- renderUI({
      df <- cards_df()
      if (!nrow(df)) return(div(class="text-muted", "Aucune donnée disponible."))
      
      sel <- selected()
      mode <- flow_mode()
      unit_lbl <- if (mode == "mass") "tonnes" else unit_label
      
      cards <- lapply(seq_len(nrow(df)), function(i){
        scen_code_i <- as.character(df$Scenario[i])
        val  <- df$value[i]
        dlt  <- df$delta[i]
        is_active <- !is.null(sel) && identical(sel, scen_code_i)
        
        delta_node <- if (!is.finite(dlt)) {
          "Vs Année de base : —"
        } else {
          tagList(
            "Vs Année de base : ",
            span(
              class = if (dlt > 0) "up" else if (dlt < 0) "down" else NULL,
              strong(paste0(
                ifelse(dlt >= 0, "+", ""),
                scales::percent(dlt, accuracy = 0.1)
              ))
            )
          )
        }
        
        div(
          class = paste(
            "u-card u-card--flat u-card--hover u-card--clickable u-card--focus",
            "dsq-card energy-kpi", if (is_active) "is-active"
          ),
          tabindex = "0", role = "button",
          `aria-pressed` = if (is_active) "true" else "false",
          `data-scenario` = scen_code_i,
          div(
            class = "u-box",
            p(class = "u-title", scenario_label(scen_code_i)),
            p(
              class = "u-value",
              fmt_int(val),
              span(class="u-unit", unit_lbl)
            ),
            p(class = "u-sub", delta_node)
          )
        )
      })
      
      div(class = "u-row", !!!cards)
    })
    
    return(list(selected_scenario = reactive(selected())))
  })
}
