# R/5.5_mod_energy_balance.R
# -------------------------------------------------

mod_energy_balance_ui <- function(id, wrap_in_card = TRUE){
  ns <- NS(id)
  
  content <- tagList(
    h2("Energy balance by scenario"),
    tags$div(style="height:8px"),
    
    uiOutput(ns("elements_selector")),
    
    checkboxInput(
      inputId = ns("show_abs"),
      label   = "Show absolute values (Gcal)",
      value   = FALSE
    ),
    
    plotly::plotlyOutput(ns("plot"), height = "390px"),
    h2("Sources / Uses ratio"),
    uiOutput(ns("ratio_cards")),
    
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

mod_energy_balance_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,   # reactive(): vector of SCENARIO CODES to display
    ...
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    
    shiny::validate(
      shiny::need(exists("scenario_code", mode = "function"), "Missing scenario_code()."),
      shiny::need(exists("scenario_label", mode = "function"), "Missing scenario_label()."),
      shiny::need(exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE), "Missing SCENARIO_LEVELS_DEFAULT."),
      shiny::need(is.function(r_scenarios), "r_scenarios must be provided to this module.")
    )
    
    # toggle interne pour le bouton Select / Deselect all
    all_selected <- shiny::reactiveVal(TRUE)
    
    # Map label -> code (au cas où fact contient un label UI pour l'extra)
    EXTRA_LABEL_TO_CODE <- if (exists("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)) {
      x <- get("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)
      setNames(unname(x), names(x))
    } else {
      NULL
    }
    
    # ------------------------------------------------------------------
    # Helper: map Element -> Flow (centralisé pour éviter incohérences)
    # ------------------------------------------------------------------
    map_energy_flow <- function(element_chr){
      el <- stringr::str_to_lower(element_chr)
      
      dplyr::case_when(
        stringr::str_detect(el, "production")      ~ "Production",
        stringr::str_detect(el, "import")          ~ "Imports",
        stringr::str_detect(el, "export")          ~ "Exports",
        stringr::str_detect(el, "domestic supply") ~ "Domestic supply",
        stringr::str_detect(el, "other uses")      ~ "Other uses (non-food)",
        stringr::str_detect(el, "unallocated")     ~ "Unallocated",
        stringr::str_detect(el, "loss")            ~ "Losses",
        stringr::str_detect(el, "seed")            ~ "Seed",
        stringr::str_detect(el, "feed")            ~ "Feed",
        stringr::str_detect(el, "food")            ~ "Food",
        TRUE                                       ~ element_chr
      )
    }
    
    # ------------------------------------------------------------------
    # Scalar key for bindCache (NEVER pass the vector directly)
    # ------------------------------------------------------------------
    scen_show_key <- shiny::reactive({
      sc <- scenario_code(r_scenarios())
      paste(sc, collapse = "|")
    })
    
    # ------------------------------------------------------------------
    # Stable scenario levels for THIS module
    # ------------------------------------------------------------------
    scen_levels_effective <- shiny::reactive({
      req(r_country())
      wanted <- r_scenarios()
      shiny::validate(shiny::need(!is.null(wanted) && length(wanted) > 0, "No scenarios provided by r_scenarios()."))
      
      wanted <- scenario_code(wanted)
      lvls_default <- scenario_code(get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE))
      
      present <- fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_starts(Element, "Energy "),
          Item == "All",
          Year %in% c(2018, 2050)
        ) %>%
        dplyr::mutate(
          Scenario = scenario_code(Scenario),
          Scenario = if (!is.null(EXTRA_LABEL_TO_CODE)) dplyr::recode(Scenario, !!!EXTRA_LABEL_TO_CODE, .default = Scenario) else Scenario
        ) %>%
        dplyr::pull(Scenario) %>%
        unique()
      
      lvls_default[lvls_default %in% wanted & lvls_default %in% present]
    }) %>% bindCache(r_country(), scen_show_key())
    
    # -------------------------------------------------
    # 1. Données brutes énergie
    # -------------------------------------------------
    data_raw <- shiny::reactive({
      req(fact, r_country())
      lvls <- scen_levels_effective()
      shiny::validate(shiny::need(length(lvls) > 0, "No energy scenarios available for this country (after r_scenarios ∩ present)."))
      
      fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_starts(Element, "Energy "),
          Item == "All",
          Year %in% c(2018, 2050)
        ) %>%
        dplyr::mutate(
          Scenario = scenario_code(Scenario),
          Scenario = if (!is.null(EXTRA_LABEL_TO_CODE)) dplyr::recode(Scenario, !!!EXTRA_LABEL_TO_CODE, .default = Scenario) else Scenario,
          Scenario = factor(Scenario, levels = lvls),
          Flow     = map_energy_flow(Element)
        ) %>%
        dplyr::filter(!is.na(Scenario)) %>%
        dplyr::mutate(Scenario = forcats::fct_drop(Scenario))
    }) %>% bindCache(r_country(), scen_show_key())
    
    # -------------------------------------------------
    # 2. Données préparées
    # -------------------------------------------------
    data_prepared <- shiny::reactive({
      df <- data_raw()
      req(nrow(df) > 0)
      
      source_flows <- c("Production", "Imports")
      
      # Retire "Domestic supply" (informative, pas une composante du bilan)
      df <- df %>%
        dplyr::filter(Flow != "Domestic supply") %>%
        dplyr::mutate(
          Group = dplyr::case_when(
            Flow %in% source_flows ~ "Sources",
            TRUE                   ~ "Uses"
          ),
          Group = factor(Group, levels = c("Sources", "Uses"))
        )
      
      df_sum <- df %>%
        dplyr::group_by(Scenario, Group, Flow, Unit) %>%
        dplyr::summarise(
          Value = sum(Value, na.rm = TRUE),
          .groups = "drop"
        )
      
      # --- Ajout "Unallocated" (si absent) ou "Residual (balancing)" (si Unallocated existe déjà)
      tot <- df_sum %>%
        dplyr::group_by(Scenario, Unit) %>%
        dplyr::summarise(
          total_sources = sum(Value[Group == "Sources"], na.rm = TRUE),
          total_uses    = sum(Value[Group == "Uses"],    na.rm = TRUE),
          has_unalloc   = any(Flow == "Unallocated"),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          gap = total_sources - total_uses,
          tol = 1e-6 * pmax(abs(total_sources), abs(total_uses), 1),
          need_balance = abs(gap) > tol
        )
      
      balancing_rows <- tot %>%
        dplyr::filter(need_balance) %>%
        dplyr::mutate(
          Flow  = dplyr::if_else(!has_unalloc, "Unallocated", "Residual (balancing)"),
          Group = dplyr::if_else(gap >= 0, "Uses", "Sources"),
          Value = abs(gap)
        ) %>%
        dplyr::select(Scenario, Group, Flow, Unit, Value) %>%
        dplyr::mutate(Group = factor(Group, levels = c("Sources", "Uses")))
      
      if (nrow(balancing_rows) > 0) {
        df_sum <- dplyr::bind_rows(df_sum, balancing_rows)
      }
      
      flow_order_known <- c(
        "Production", "Imports",
        "Exports",
        "Food", "Feed", "Losses", "Seed",
        "Other uses (non-food)",
        "Unallocated",
        "Residual (balancing)"
      )
      flow_extra <- sort(setdiff(unique(df_sum$Flow), flow_order_known))
      flow_levels <- c(flow_order_known[flow_order_known %in% unique(df_sum$Flow)], flow_extra)
      
      df_sum <- df_sum %>%
        dplyr::mutate(Flow = factor(Flow, levels = flow_levels))
      
      df_sum %>%
        dplyr::group_by(Scenario, Group) %>%
        dplyr::mutate(
          group_total = sum(Value, na.rm = TRUE),
          Share       = dplyr::if_else(group_total > 0, 100 * Value / group_total, NA_real_),
          Value_m     = Value / 1e6
        ) %>%
        dplyr::ungroup()
    }) %>% bindCache(r_country(), scen_show_key())
    
    # -------------------------------------------------
    # 3. Liste des flux par groupe
    # -------------------------------------------------
    flow_lists <- shiny::reactive({
      df <- data_prepared()
      req(nrow(df) > 0)
      
      list(
        sources = df %>% dplyr::filter(Group == "Sources") %>% dplyr::pull(Flow) %>% as.character() %>% unique(),
        uses    = df %>% dplyr::filter(Group == "Uses")    %>% dplyr::pull(Flow) %>% as.character() %>% unique()
      )
    })
    
    # -------------------------------------------------
    # 4. Cases à cocher : flux Sources vs Uses + bouton toggle
    # -------------------------------------------------
    output$elements_selector <- shiny::renderUI({
      fl <- flow_lists()
      src <- fl$sources
      use <- fl$uses
      
      tagList(
        div(
          style = "margin-bottom:8px;",
          actionButton(
            inputId = ns("toggle_flows"),
            label   = if (isTRUE(all_selected())) "Deselect all flows" else "Select all flows",
            class   = "btn btn-default btn-sm"
          )
        ),
        div(
          style = "display:flex; gap:24px; align-items:flex-start; flex-wrap:wrap;",
          div(
            style = "min-width:160px;",
            tags$strong("Sources"),
            checkboxGroupInput(
              inputId  = ns("elements_sources"),
              label    = NULL,
              choices  = src,
              selected = src
            )
          ),
          div(
            style = "min-width:220px;",
            tags$strong("Uses"),
            checkboxGroupInput(
              inputId  = ns("elements_uses"),
              label    = NULL,
              choices  = use,
              selected = use
            )
          )
        )
      )
    })
    
    observeEvent(input$toggle_flows, {
      fl <- flow_lists()
      src <- fl$sources
      use <- fl$uses
      
      if (isTRUE(all_selected())) {
        updateCheckboxGroupInput(session, "elements_sources", selected = character(0))
        updateCheckboxGroupInput(session, "elements_uses",    selected = character(0))
        all_selected(FALSE)
      } else {
        updateCheckboxGroupInput(session, "elements_sources", selected = src)
        updateCheckboxGroupInput(session, "elements_uses",    selected = use)
        all_selected(TRUE)
      }
      
      new_label <- if (isTRUE(all_selected())) "Deselect all flows" else "Select all flows"
      updateActionButton(session, "toggle_flows", label = new_label)
    }, ignoreNULL = TRUE)
    
    r_flows_selected <- shiny::reactive({
      c(input$elements_sources %||% character(0),
        input$elements_uses    %||% character(0))
    })
    
    # -------------------------------------------------
    # 5. Résumé : Sources / Uses par scénario (flux visibles)
    # -------------------------------------------------
    ratio_data <- shiny::reactive({
      df_all <- data_prepared()
      if (nrow(df_all) == 0) return(df_all[0, ])
      
      flows_selected <- r_flows_selected()
      if (length(flows_selected) == 0) return(df_all[0, ])
      
      df_vis <- df_all %>% dplyr::filter(as.character(Flow) %in% flows_selected)
      if (nrow(df_vis) == 0) return(df_vis[0, ])
      
      df_vis %>%
        dplyr::group_by(Scenario, Group) %>%
        dplyr::summarise(total = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = Group, values_from = total) %>%
        dplyr::mutate(
          ratio_SU = dplyr::if_else(!is.na(Sources) & !is.na(Uses) & Uses > 0, 100 * Sources / Uses, NA_real_)
        )
    })
    
    # >>> Cards fixed to Sources / Uses — no selector <<<
    output$ratio_cards <- shiny::renderUI({
      df <- ratio_data()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      ratio_label <- "Sources / Uses"
      
      div(
        id    = ns("ratio_cards_root"),
        class = "energy-kpi",
        div(
          class = "u-row",
          lapply(seq_len(nrow(df)), function(i){
            scen_code <- as.character(df$Scenario[i])
            scen_lbl  <- scenario_label(scen_code)
            
            val <- df$ratio_SU[i]
            label_val <- if (is.na(val)) "—" else paste0(round(val), "%")
            
            div(
              class = "u-card u-card--flat u-card--hover",
              div(
                class = "u-box",
                p(class = "u-title", scen_lbl),
                p(class = "u-value", label_val, tags$span(class = "u-unit", ratio_label)),
                p(class = "u-sub", "Based on currently visible flows")
              )
            )
          }) |> do.call(what = tagList)
        )
      )
    })
    
    # -------------------------------------------------
    # 6. Graphique (2 barres par scénario) : % ou absolu (million Gcal)
    # -------------------------------------------------
    output$plot <- plotly::renderPlotly({
      df_all <- data_prepared()
      if (nrow(df_all) == 0) return(NULL)
      
      flows_selected <- r_flows_selected()
      if (length(flows_selected) == 0) return(NULL)
      
      show_abs <- isTRUE(input$show_abs)
      
      th <- if (exists("get_plotly_tokens", mode = "function")) get_plotly_tokens() else list(
        font_color       = "#111827",
        muted_color      = "#6B7280",
        gridcolor        = "rgba(0,0,0,.15)",
        axis_linecolor   = "rgba(0,0,0,.18)"
      )
      gg_txt   <- th$font_color %||% "#111827"
      gg_grid  <- th$gridcolor %||% "rgba(0,0,0,.15)"
      
      offset    <- 0.15
      bar_width <- 0.28
      
      df_all <- df_all %>%
        dplyr::mutate(
          scen_idx = as.numeric(Scenario),
          x_pos = dplyr::case_when(
            Group == "Sources" ~ scen_idx - offset,
            Group == "Uses"    ~ scen_idx + offset,
            TRUE               ~ scen_idx
          )
        )
      
      df <- df_all %>% dplyr::filter(as.character(Flow) %in% flows_selected)
      if (nrow(df) == 0) return(NULL)
      
      flow_levels <- levels(df_all$Flow)
      
      base_cols <- if (exists("sankey_node_palette", mode = "function")) sankey_node_palette() else NULL
      if (!is.null(base_cols)) {
        flow_colors <- base_cols[flow_levels]
        miss <- is.na(flow_colors)
        if (any(miss)) flow_colors[miss] <- scales::hue_pal()(sum(miss))
      } else {
        flow_colors <- setNames(scales::hue_pal()(length(flow_levels)), flow_levels)
      }
      
      unit_label <- paste(unique(df_all$Unit), collapse = ", ")
      
      scen_axis <- df_all %>%
        dplyr::distinct(Scenario, scen_idx) %>%
        dplyr::arrange(scen_idx)
      
      # Y mapping (ABS = million Gcal via Value_m)
      y_col <- if (show_abs) "Value_m" else "Share"
      y_lab <- if (show_abs) paste0("Value (million ", unit_label, ")") else "Share within each bar (%)"
      
      # Labels "Sources/Uses" : position FIXE (comme en %), indépendante des flux cochés
      labels_df <- df_all %>%
        dplyr::distinct(Scenario, Group, scen_idx, x_pos)
      
      if (show_abs) {
        # y_max basé sur le total complet (tous flux), pour ne pas bouger quand on coche/décoche
        bar_totals <- df_all %>%
          dplyr::group_by(Scenario, Group) %>%
          dplyr::summarise(total_m = sum(Value_m, na.rm = TRUE), .groups = "drop")
        
        y_max <- max(bar_totals$total_m, na.rm = TRUE)
        if (!is.finite(y_max) || y_max <= 0) y_max <- 1
        
        labels_df <- labels_df %>% dplyr::mutate(label_y = y_max * 1.08)
        y_limits <- c(0, y_max * 1.15)
      } else {
        labels_df <- labels_df %>% dplyr::mutate(label_y = 103)
        y_limits <- c(0, 110)
      }
      
      # Tooltip : on garde % + valeur en million Gcal
      df <- df %>%
        dplyr::mutate(
          tooltip_text = paste0(
            "Scenario: ", scenario_label(as.character(Scenario)), "<br>",
            "Bar: ", as.character(Group), "<br>",
            "Flow: ", as.character(Flow), "<br>",
            "Share within bar: ", sprintf("%.0f%%", Share), "<br>",
            "Value: ", scales::comma(Value_m, accuracy = 0.1), " million ", unit_label,
            "<extra></extra>"
          )
        )
      
      gg <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = x_pos,
          y = .data[[y_col]],
          fill = Flow,
          text = tooltip_text
        )
      ) +
        ggplot2::geom_col(width = bar_width) +
        ggplot2::geom_text(
          data = labels_df,
          ggplot2::aes(x = x_pos, y = label_y, label = Group),
          inherit.aes = FALSE,
          size  = 3.5,
          vjust = 0,
          colour = gg_txt
        ) +
        ggplot2::scale_fill_manual(values = flow_colors, name = NULL) +
        ggplot2::scale_x_continuous(
          breaks = scen_axis$scen_idx,
          labels = scenario_label(as.character(scen_axis$Scenario)),
          expand = ggplot2::expansion(mult = c(0.02, 0.02))
        ) +
        {
          if (show_abs) {
            ggplot2::scale_y_continuous(
              labels = scales::label_number(accuracy = 0.1, big.mark = ","),
              limits = y_limits,
              expand = ggplot2::expansion(mult = c(0, 0.05))
            )
          } else {
            ggplot2::scale_y_continuous(
              labels = scales::label_percent(accuracy = 1, scale = 1),
              limits = y_limits,
              expand = ggplot2::expansion(mult = c(0, 0.05))
            )
          }
        } +
        ggplot2::labs(x = NULL, y = y_lab) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x      = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 1, colour = gg_txt),
          axis.text.y      = ggplot2::element_text(colour = gg_txt),
          axis.title.y     = ggplot2::element_text(colour = gg_txt),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(colour = gg_grid),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      
      p <- plotly::ggplotly(gg, tooltip = "text")
      p <- plotly::layout(
        p,
        margin = list(l = 40, r = 20, t = 25, b = 40),
        legend = list(
          title = list(text = ""),
          orientation = "h",
          x = 0,12,
          xanchor = "left",
          y = 1.18,
          yanchor = "top"
        )
      )
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      } else if (exists("plotly_theme_transparent", mode = "function")) {
        p <- plotly_theme_transparent(p)
      }
      
      p
    })
    
    # -------------------------------------------------
    # 7. Download CSV
    # -------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0("energy_balance_", r_country(), ".csv")
      },
      content = function(file){
        df <- data_prepared()
        readr::write_csv(df, file)
      }
    )
    
    # -------------------------------------------------
    # 8. Note
    # -------------------------------------------------
    output$note <- renderUI({
      df <- data_prepared()
      if (nrow(df) == 0) return(NULL)
      
      unit_label <- paste(unique(df$Unit), collapse = ", ")
      has_balance <- any(as.character(df$Flow) %in% c("Unallocated", "Residual (balancing)"))
      
      mode_txt <- if (isTRUE(input$show_abs)) {
        paste0("Heights are expressed in <strong>million ", unit_label, "</strong> (absolute values).")
      } else {
        "Heights are expressed in <strong>percentages</strong> within each bar."
      }
      
      hidden_txt <- if (isTRUE(input$show_abs)) {
        "When you uncheck a flow in the list, its segment is hidden from the stacked bars."
      } else {
        "When you uncheck a flow in the list, its segment is hidden but the percentages still refer to the full bar."
      }
      
      htmltools::HTML(glue::glue(
        "<p>
        This chart shows, for each scenario, two stacked bars that describe the
        <strong>energy balance</strong> of the agri-food system:<br>
        <ul>
          <li><strong>Sources</strong> (left): the share of <em>production</em> and <em>imports</em> in the total energy entering the system;</li>
          <li><strong>Uses</strong> (right): the share of <em>exports</em> and internal uses of energy within the agri-food system
              (food, feed, losses, seed, other non-food uses, etc.).</li>
        </ul>
        {mode_txt}<br>
        {hidden_txt}<br>
        The small cards below the chart indicate, for each scenario, the ratio
        <strong>Sources / Uses</strong>, based on the flows currently visible in the chart.
        Volumes in the tooltips are given in <strong>million {unit_label}</strong>.
        </p>"
      ))
    })
    
    invisible(NULL)
  })
}
