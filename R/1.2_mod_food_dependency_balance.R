# R/1.2_mod_food_dependency_balance.R
# -------------------------------------------------


mod_food_dependency_balance_ui <- function(id, wrap_in_card = TRUE, height = "490px"){
  ns <- NS(id)
  
  content <- tagList(
    h2("Simplified balance by scenario (in million Gcal)"),
    tags$br(),
    plotly::plotlyOutput(ns("plot"), height = height),
    
    div(
      h2("Import quantity and ratio"),
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

mod_food_dependency_balance_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,   # reactive(): vector of SCENARIO CODES to display
    ...
){
  moduleServer(id, function(input, output, session){
    
    shiny::validate(
      shiny::need(exists("scenario_code",  mode = "function"), "Missing scenario_code()."),
      shiny::need(exists("scenario_label", mode = "function"), "Missing scenario_label()."),
      shiny::need(exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE), "Missing SCENARIO_LEVELS_DEFAULT."),
      shiny::need(is.function(r_scenarios), "r_scenarios must be provided to this module.")
    )
    
    EXTRA_LABEL_TO_CODE <- if (exists("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)) {
      x <- get("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)
      setNames(unname(x), names(x))
    } else {
      NULL
    }
    
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
    
    scen_show_key <- shiny::reactive({
      sc <- scenario_code(r_scenarios())
      paste(sc, collapse = "|")
    })
    
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
    
    data_prepared <- shiny::reactive({
      df <- data_raw()
      req(nrow(df) > 0)
      
      needed_flows <- c(
        "Production", "Imports", "Exports",
        "Food", "Feed", "Losses", "Seed", "Other uses (non-food)"
      )
      
      df <- df %>% dplyr::filter(Flow %in% needed_flows)
      
      df_sum <- df %>%
        dplyr::group_by(Scenario, Flow, Unit) %>%
        dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
      
      internal_flows <- c("Food", "Feed", "Losses", "Seed", "Other uses (non-food)")
      
      dom <- df_sum %>%
        dplyr::filter(Flow %in% internal_flows) %>%
        dplyr::group_by(Scenario, Unit) %>%
        dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(Group = "Uses", Flow = "Domestic demand")
      
      src <- df_sum %>%
        dplyr::filter(Flow %in% c("Production", "Imports")) %>%
        dplyr::mutate(Group = "Resources")
      
      use_exp <- df_sum %>%
        dplyr::filter(Flow == "Exports") %>%
        dplyr::mutate(Group = "Uses")
      
      out <- dplyr::bind_rows(src, dom, use_exp) %>%
        dplyr::mutate(
          Group = factor(Group, levels = c("Resources", "Uses")),
          Flow  = factor(Flow, levels = c("Production", "Imports", "Domestic demand", "Exports"))
        )
      
      unit_unique <- unique(out$Unit)
      unit_unique <- unit_unique[!is.na(unit_unique)]
      shiny::validate(shiny::need(length(unit_unique) == 1, "Multiple energy units found; cannot plot a single unit chart."))
      
      u0 <- unit_unique[[1]]
      u0_low <- tolower(u0)
      
      mult_to_gcal <- dplyr::case_when(
        stringr::str_detect(u0_low, "gcal") ~ 1,
        stringr::str_detect(u0_low, "kcal") ~ 1 / 1e6,  # 1 Gcal = 1e6 kcal (= 1e9 cal)
        TRUE ~ NA_real_
      )
      
      shiny::validate(
        shiny::need(!is.na(mult_to_gcal),
                    paste0("Unexpected energy unit: '", u0, "'. Expected Gcal or kcal to plot in million Gcal."))
      )
      
      out %>%
        dplyr::mutate(
          Value_gcal  = Value * mult_to_gcal,  # Gcal (for KPI cards)
          Value_mgcal = (Value * mult_to_gcal) / 1e6,  # million Gcal (for chart)
          Unit_plot   = "million Gcal"
        ) %>%
        dplyr::group_by(Scenario, Group) %>%
        dplyr::mutate(
          group_total = sum(Value_mgcal, na.rm = TRUE),
          Share       = dplyr::if_else(group_total > 0, 100 * Value_mgcal / group_total, NA_real_)
        ) %>%
        dplyr::ungroup()
    }) %>% bindCache(r_country(), scen_show_key())
    
    # KPI cards: unit MUST stay in Gcal
    kpi_imports <- shiny::reactive({
      df <- data_prepared()
      if (nrow(df) == 0) return(df[0, ])
      
      imp <- df %>%
        dplyr::filter(as.character(Flow) == "Imports") %>%
        dplyr::select(Scenario, imports_gcal = Value_gcal)
      
      dom <- df %>%
        dplyr::filter(as.character(Flow) == "Domestic demand") %>%
        dplyr::select(Scenario, domdem_gcal = Value_gcal)
      
      imp %>%
        dplyr::left_join(dom, by = "Scenario") %>%
        dplyr::mutate(
          share_imp_dom = dplyr::if_else(!is.na(domdem_gcal) & domdem_gcal > 0,
                                         100 * imports_gcal / domdem_gcal,
                                         NA_real_)
        ) %>%
        dplyr::arrange(Scenario)
    })
    
    output$kpi_cards <- shiny::renderUI({
      dat <- kpi_imports()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      
      fmt_num <- function(x){
        ifelse(is.finite(x), format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE), "—")
      }
      fmt_pct <- function(p){
        if (!is.finite(p)) return("—")
        paste0(formatC(p, digits = 0, format = "f"), "%")
      }
      
      cards <- lapply(seq_len(nrow(dat)), function(i){
        sc_code <- as.character(dat$Scenario[i])
        sc_lab  <- scenario_label(sc_code)
        
        imp_g  <- dat$imports_gcal[i]
        sh_pct <- dat$share_imp_dom[i]
        
        div(
          class = "u-card u-card--flat u-card--hover",
          div(
            class = "u-box",
            p(class = "u-title", sc_lab),
            p(class = "u-value", fmt_num(imp_g), span(class = "u-unit", "Gcal")),
            p(class = "u-sub", "Imports / Domestic demand: ", fmt_pct(sh_pct))
          )
        )
      })
      
      div(class = "u-row", do.call(tagList, cards))
    })
    
    output$plot <- plotly::renderPlotly({
      df <- data_prepared()
      if (nrow(df) == 0) return(NULL)
      
      th <- if (exists("get_plotly_tokens", mode = "function")) get_plotly_tokens() else list(
        font_color       = "#111827",
        muted_color      = "#6B7280",
        gridcolor        = "rgba(0,0,0,.15)",
        axis_linecolor   = "rgba(0,0,0,.18)"
      )
      gg_txt  <- th$font_color %||% "#111827"
      gg_grid <- th$gridcolor  %||% "rgba(0,0,0,.15)"
      
      offset    <- 0.15
      bar_width <- 0.28
      
      df <- df %>%
        dplyr::mutate(
          scen_idx = as.numeric(Scenario),
          x_pos = dplyr::case_when(
            Group == "Resources" ~ scen_idx - offset,
            Group == "Uses"      ~ scen_idx + offset,
            TRUE                 ~ scen_idx
          )
        )
      
      totals <- df %>%
        dplyr::group_by(Scenario, Group, scen_idx) %>%
        dplyr::summarise(total = sum(Value_mgcal, na.rm = TRUE), .groups = "drop")
      
      y_max <- max(totals$total, na.rm = TRUE)
      y_lab <- y_max * 1.03
      y_lim <- y_max * 1.15
      
      labels_df <- df %>%
        dplyr::distinct(Scenario, Group, scen_idx, x_pos) %>%
        dplyr::mutate(label_y = y_lab)
      
      scen_axis <- df %>%
        dplyr::distinct(Scenario, scen_idx) %>%
        dplyr::arrange(scen_idx)
      
      flow_levels <- levels(df$Flow)
      
      base_cols <- if (exists("sankey_node_palette", mode = "function")) sankey_node_palette() else NULL
      flow_colors <- setNames(rep(NA_character_, length(flow_levels)), flow_levels)
      
      if (!is.null(base_cols)) {
        if (!is.na(base_cols["Production"])) flow_colors["Production"] <- base_cols["Production"]
        if (!is.na(base_cols["Imports"]))    flow_colors["Imports"]    <- base_cols["Imports"]
        if (!is.na(base_cols["Exports"]))    flow_colors["Exports"]    <- base_cols["Exports"]
        if (!is.na(base_cols["Food"])) {
          flow_colors["Domestic demand"] <- base_cols["Food"]
        } else if (!is.na(base_cols["Domestic supply"])) {
          flow_colors["Domestic demand"] <- base_cols["Domestic supply"]
        }
      }
      miss <- is.na(flow_colors)
      if (any(miss)) flow_colors[miss] <- scales::hue_pal()(sum(miss))
      
      gg <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = x_pos,
          y = Value_mgcal,
          fill = Flow,
          text = paste0(
            "Scenario: ", scenario_label(as.character(Scenario)), "<br>",
            "Bar: ", as.character(Group), "<br>",
            "Component: ", as.character(Flow), "<br>",
            "Value: ", scales::comma(Value_mgcal, accuracy = 0.1), " million Gcal", "<br>",
            "Share within bar: ", ifelse(is.na(Share), "—", sprintf("%.0f%%", Share))
          )
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
        ggplot2::scale_fill_manual(values = flow_colors) +
        ggplot2::scale_x_continuous(
          breaks = scen_axis$scen_idx,
          labels = scenario_label(as.character(scen_axis$Scenario)),
          expand = ggplot2::expansion(mult = c(0.02, 0.02))
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::comma,
          limits = c(0, y_lim),
          expand = ggplot2::expansion(mult = c(0, 0.02))
        ) +
        ggplot2::labs(x = NULL, y = "million Gcal") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x      = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 1, colour = gg_txt),
          axis.text.y      = ggplot2::element_text(colour = gg_txt),
          axis.title.y     = ggplot2::element_text(colour = gg_txt),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(colour = gg_grid),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          legend.position  = "top"
        )
      
      p <- plotly::ggplotly(gg, tooltip = "text")
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      } else if (exists("plotly_theme_transparent", mode = "function")) {
        p <- plotly_theme_transparent(p)
      }
      
      p <- plotly::layout(
        p,
        margin = list(l = 50, r = 20, t = 70, b = 60),
        legend = list(
          orientation = "h",
          x = 0.25, xanchor = "left",
          y = 1.10, yanchor = "bottom",
          title = list(text = "")
        )
      )
      
      p
    })
    
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0("food_dependency_balance_", r_country(), ".csv")
      },
      content = function(file){
        df <- data_prepared()
        readr::write_csv(df, file)
      }
    )
    
    output$note <- renderUI({
      df <- data_prepared()
      if (nrow(df) == 0) return(NULL)
      
      htmltools::HTML(
        "<p>
        This chart shows, for each scenario, two stacked bars in <strong>million Gcal</strong>:<br>
        <ul>
          <li><strong>Resources</strong> (left): <em>Production</em> + <em>Imports</em></li>
          <li><strong>Uses</strong> (right): <em>Exports</em> + <em>Domestic demand</em></li>
        </ul>
        <strong>Domestic demand</strong> is computed as the sum of: Food, Feed, Losses, Seed, and Other uses (non-food).
        The cards below the chart show <strong>Imports</strong> in <strong>Gcal</strong> and their share in <strong>Domestic demand</strong>.
        </p>"
      )
    })
    
    invisible(NULL)
  })
}