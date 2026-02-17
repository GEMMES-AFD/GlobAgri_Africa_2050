# R/3.3_mod_livestock_stocks_system.R
# ======================================================================

# ---- Local helpers -------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
.get_fact <- function(x) if (is.function(x)) x() else x

LS_SYSTEM_LEVELS <- c("MixedArid","MixedNonArid","PastoralArid","PastoralNonArid","Specific")

RUMINANT_GROUPS <- c(
  "Beef cattle",
  "Dairy cattle",
  "Dairy sheep and goats",
  "Meat sheep and goats"
)

# ---- Display labels (UI only; internal codes unchanged) -------------------
RUMINANT_GROUP_LABELS <- c(
  "Beef cattle"           = "Beef cattle",
  "Dairy cattle"          = "Dairy cattle",
  "Dairy sheep and goats" = "Dairy small ruminants",
  "Meat sheep and goats"  = "Meat small ruminants"
)

rum_group_label <- function(code){
  code <- as.character(code)
  out  <- unname(RUMINANT_GROUP_LABELS[code])
  ifelse(is.na(out) | out == "", code, out)
}

rum_group_label_vec <- function(x){
  x <- as.character(x)
  vapply(x, rum_group_label, character(1))
}

# reverse map (legend label -> internal code)
RUM_LABEL_TO_CODE <- setNames(names(RUMINANT_GROUP_LABELS), unname(RUMINANT_GROUP_LABELS))

# ---- LSU/TLU settings ----------------------------------------------------
LSU_ELEMENT <- "LSU"
LSU_UNITS   <- c("1000TLU", "1000 TLU")   # robust variants

# ---- Colors (EDIT HERE) --------------------------------------------------
COL_RUMINANTS <- c(
  "Beef cattle"           = "#E73121",
  "Dairy cattle"          = "#4E79A7",
  "Dairy sheep and goats" = "#76B7B2",
  "Meat sheep and goats"  = "#FF9793"
)

COL_SYSTEMS <- c(
  "MixedArid"       = "#E8C547",
  "MixedNonArid"    = "#59A14F",
  "PastoralArid"    = "#F28E2B",
  "PastoralNonArid" = "#8CD17D",
  "Specific"        = "#AF7AA1"
)

.empty_plot <- function(msg){
  plotly::plot_ly(
    type = "scatter", mode = "markers",
    x = NA, y = NA, hoverinfo = "none", showlegend = FALSE
  ) %>%
    plotly::layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(
        text = msg, xref = "paper", yref = "paper",
        x = 0.5, y = 0.5, showarrow = FALSE,
        font = list(size = 14)
      )),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      margin = list(l = 10, r = 10, t = 10, b = 10)
    )
}

# ---- UI ------------------------------------------------------------------
mod_livestock_system_stocks_ui <- function(
    id,
    title = "Ruminant herd composition by livestock system (TLU)"
){
  ns <- NS(id)
  
  caption_style <- paste(
    "margin-top:6px;",
    "text-align:center;",
    "font-weight:600;",
    "font-size:13px;"
  )
  
  tagList(
    div(
      class = "card",
      div(
        class = "card-body",
        div(
          h2(class = "card-title", title),
          tags$div(style="height:15px"),
          tags$label("Choose a scenario to compare with base-year",
                     `for` = ns("scen_cmp"),
                     class = "form-label mb-1"),
          selectInput(
            ns("scen_cmp"), NULL,
            choices  = character(0),
            width    = "220px"
          ),
          tags$div(style="height:5px"),
          tags$label("Ruminant groups (show/hide)",
                     `for` = ns("rum_groups"),
                     class = "form-label mb-1"),
          tags$div(
            style = "white-space:nowrap; overflow-x:auto; width:100%;",
            checkboxGroupInput(
              ns("rum_groups"),
              label    = NULL,
              # UI labels, internal values unchanged
              choices  = setNames(RUMINANT_GROUPS, rum_group_label_vec(RUMINANT_GROUPS)),
              selected = RUMINANT_GROUPS,
              inline   = TRUE
            )
          ),
          tags$div(style="height:15px")
        ),
        plotlyOutput(ns("p_bars"), height = "520px"),
        
        # --- Pies --------------------------------------------------------
        h2("Share of systems in total selected ruminant stocks (%)"),
        tags$br(),
        fluidRow(
          column(
            width = 6,
            plotlyOutput(ns("p_pie_base"), height = "320px"),
            tags$br(),
            div(style = caption_style, uiOutput(ns("caption_base_short")))
          ),
          column(
            width = 6,
            plotlyOutput(ns("p_pie_scen"), height = "320px"),
            tags$br(),
            div(style = caption_style, uiOutput(ns("caption_scen_short")))
          )
        ),
        
        # --- Export ------------------------------------------------------
        div(
          class = "text-right",
          div(
            class = "u-actions",
            downloadLink(
              ns("dl_livestock_system_csv"),
              label = tagList(icon("download"), "CSV")
            )
          )
        ),
        
        uiOutput(ns("note"))
      )
    )
  )
}

# ---- SERVER ---------------------------------------------------------------
mod_livestock_system_stocks_server <- function(
    id,
    fact,
    r_country,
    r_scenarios = shiny::reactive(NULL)
){
  moduleServer(id, function(input, output, session){
    
    # ---- Dependencies from CONFIG ----------------------------------------
    if (!exists("scenario_label", mode = "function", inherits = TRUE)) {
      stop("mod_livestock_system_stocks_server(): missing dependency 'scenario_label(code)'.")
    }
    scenario_label_vec <- function(x){
      x <- as.character(x)
      vapply(x, scenario_label, character(1))
    }
    
    scenario_levels_all <- if (exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)
    } else {
      sort(unique(.get_fact(fact)$Scenario))
    }
    
    base_year <- if (exists("BASE_YEAR", inherits = TRUE)) {
      as.integer(get("BASE_YEAR", inherits = TRUE))
    } else {
      2018L
    }
    
    scen_levels_effective <- reactive({
      sc <- scenario_levels_all
      if (!is.null(r_scenarios) && !is.null(r_scenarios())) {
        sc <- intersect(sc, r_scenarios())
      }
      sc
    })
    
    # Baseline scenario code from CONFIG (no hard-coded name)
    scen_base <- reactive({
      sc_eff <- scen_levels_effective()
      b <- if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
        as.character(get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE))
      } else {
        ""
      }
      if (nzchar(b) && b %in% sc_eff) return(b)
      
      b2 <- if (exists("SCENARIOS_BASE_CODES", inherits = TRUE)) {
        intersect(get("SCENARIOS_BASE_CODES", inherits = TRUE), sc_eff)
      } else {
        character(0)
      }
      b2[1] %||% sc_eff[1]
    })
    
    # ---- Palettes ---------------------------------------------------------
    group_palette_for <- function(groups_vec){
      groups_vec <- intersect(RUMINANT_GROUPS, groups_vec)
      if (!length(groups_vec)) groups_vec <- RUMINANT_GROUPS
      cols <- COL_RUMINANTS[groups_vec]
      if (any(is.na(cols))) cols[is.na(cols)] <- scales::hue_pal()(sum(is.na(cols)))
      setNames(cols, groups_vec)
    }
    
    pal_sys <- local({
      out <- COL_SYSTEMS
      miss <- LS_SYSTEM_LEVELS[is.na(out[LS_SYSTEM_LEVELS])]
      if (length(miss)) out[miss] <- scales::hue_pal()(length(miss))
      out[LS_SYSTEM_LEVELS]
    })
    
    # ---- Legend-driven filtering state (no plotly::event_data) ------------
    rv_hidden_groups <- reactiveVal(character(0))
    
    observeEvent(input$rum_groups, {
      rv_hidden_groups(character(0))
    }, ignoreInit = TRUE)
    
    observeEvent(list(r_country(), input$scen_cmp), {
      rv_hidden_groups(character(0))
    }, ignoreInit = TRUE)
    
    observeEvent(input$bars_legend_evt, {
      ed <- input$bars_legend_evt
      req(is.list(ed), ed$type, ed$name)
      
      nm <- as.character(ed$name)
      
      # NEW: legend label -> internal code (fallback if already a code)
      nm_code <- RUM_LABEL_TO_CODE[[nm]] %||% nm
      
      sel <- intersect(RUMINANT_GROUPS, input$rum_groups %||% RUMINANT_GROUPS)
      if (!(nm_code %in% sel)) return()
      
      if (identical(ed$type, "click")) {
        hid <- rv_hidden_groups()
        hid <- if (nm_code %in% hid) setdiff(hid, nm_code) else union(hid, nm_code)
        rv_hidden_groups(intersect(hid, sel))
      }
      
      if (identical(ed$type, "doubleclick")) {
        target_hid <- setdiff(sel, nm_code)
        if (setequal(rv_hidden_groups(), target_hid)) {
          rv_hidden_groups(character(0))
        } else {
          rv_hidden_groups(target_hid)
        }
      }
    }, ignoreInit = TRUE)
    
    groups_for_pies <- reactive({
      sel <- intersect(RUMINANT_GROUPS, input$rum_groups %||% RUMINANT_GROUPS)
      if (!length(sel)) sel <- RUMINANT_GROUPS
      setdiff(sel, rv_hidden_groups())
    })
    
    # ---- Build clean df (LSU / 1000TLU) ----------------------------------
    prep_df <- reactive({
      req(r_country())
      df0 <- .get_fact(fact)
      req(df0)
      
      need_cols <- c("Region","Scenario","Element","Item","Year","System","Animal","Unit","Value")
      miss <- setdiff(need_cols, names(df0))
      validate(need(length(miss) == 0, paste("Missing columns:", paste(miss, collapse = ", "))))
      
      scen_allowed <- scen_levels_effective()
      
      df <- df0 %>%
        dplyr::mutate(
          Element = stringr::str_trim(as.character(.data$Element)),
          Item    = stringr::str_trim(as.character(.data$Item)),
          Unit    = stringr::str_trim(as.character(.data$Unit)),
          System  = dplyr::na_if(stringr::str_trim(as.character(.data$System)), ""),
          Animal  = dplyr::na_if(stringr::str_trim(as.character(.data$Animal)), "")
        ) %>%
        dplyr::filter(
          .data$Region   == r_country(),
          .data$Element  == LSU_ELEMENT,
          .data$Scenario %in% scen_allowed,
          .data$Unit     %in% LSU_UNITS,
          !is.na(.data$System)
        ) %>%
        dplyr::mutate(Year = suppressWarnings(as.integer(.data$Year)))
      
      v_raw <- df$Value
      v_num <- suppressWarnings(as.numeric(v_raw))
      if (anyNA(v_num) && !is.numeric(v_raw)) {
        v_num <- readr::parse_number(
          as.character(v_raw),
          locale = readr::locale(decimal_mark = ",")
        )
      }
      
      df2 <- df %>%
        dplyr::mutate(
          Value_num = v_num,
          mult      = 1000,                     # 1000TLU -> TLU
          head      = .data$Value_num * .data$mult,
          Unit_out  = "TLU",
          
          group_raw = dplyr::coalesce(.data$Animal, .data$Item),
          animal_lc = stringr::str_to_lower(dplyr::coalesce(.data$Animal, "")),
          item_lc   = stringr::str_to_lower(dplyr::coalesce(.data$Item, "")),
          
          is_dairy  = (animal_lc == "dairy") | (stringr::str_to_lower(dplyr::coalesce(.data$group_raw, "")) == "dairy"),
          
          group = dplyr::case_when(
            is_dairy & stringr::str_detect(item_lc, "\\bbeef\\s+cattle\\b") ~ "Beef cattle",
            is_dairy & item_lc == "dairy"                                   ~ "Dairy cattle",
            is_dairy & stringr::str_detect(item_lc, "\\bcattle\\b")         ~ "Dairy cattle",
            is_dairy & stringr::str_detect(item_lc, "\\bgoats\\b|\\bgoat\\b")~ "Dairy sheep and goats",
            is_dairy                                                        ~ NA_character_,
            TRUE                                                            ~ .data$group_raw
          )
        )
      
      dairy_unclassified <- df2 %>%
        dplyr::filter(.data$is_dairy, is.na(.data$group)) %>%
        dplyr::distinct(.data$Item) %>%
        dplyr::pull(.data$Item)
      
      validate(need(
        length(dairy_unclassified) == 0,
        paste0(
          "Some dairy rows could not be classified (Animal='dairy' but Item does not contain 'beef cattle', 'cattle', 'dairy' nor 'goat(s)'). ",
          "Examples of Item: ",
          paste(utils::head(dairy_unclassified, 6), collapse = " | "),
          if (length(dairy_unclassified) > 6) " | ..." else ""
        )
      ))
      
      df2 %>%
        dplyr::filter(.data$group %in% RUMINANT_GROUPS) %>%
        dplyr::mutate(
          System = factor(as.character(.data$System), levels = LS_SYSTEM_LEVELS),
          group  = factor(as.character(.data$group),  levels = RUMINANT_GROUPS)
        ) %>%
        dplyr::filter(!is.na(.data$System)) %>%
        dplyr::select(Region, Scenario, Year, System, group, Unit_out, head)
    })
    
    # ---- Populate scenario dropdown (labels from scenario_label) ----------
    observeEvent(list(r_country(), scen_levels_effective(), scen_base()), {
      sc_eff <- scen_levels_effective()
      b      <- scen_base()
      
      choices_codes <- setdiff(sc_eff, b)
      if (!length(choices_codes)) choices_codes <- sc_eff
      
      updateSelectInput(
        session, "scen_cmp",
        choices  = setNames(choices_codes, scenario_label_vec(choices_codes)),
        selected = if (length(choices_codes)) choices_codes[1] else character(0)
      )
    }, ignoreInit = FALSE)
    
    # scenario year: 2050 if available else max
    year_cmp <- reactive({
      req(input$scen_cmp)
      d <- prep_df()
      
      y <- d %>%
        dplyr::filter(.data$Scenario == input$scen_cmp) %>%
        dplyr::distinct(.data$Year) %>%
        dplyr::pull(.data$Year)
      
      if (!length(y)) return(NA_integer_)
      y <- y[is.finite(y)]
      if (!length(y)) return(NA_integer_)
      
      if (2050L %in% y) return(2050L)
      suppressWarnings(as.integer(max(y, na.rm = TRUE)))
    })
    
    output$caption_base_short <- renderUI({
      b <- scen_base()
      HTML(scenario_label(b))
    })
    
    output$caption_scen_short <- renderUI({
      req(input$scen_cmp)
      HTML(scenario_label(input$scen_cmp))
    })
    
    d_period <- reactive({
      req(input$scen_cmp)
      req(input$rum_groups)
      
      sel_groups <- intersect(RUMINANT_GROUPS, input$rum_groups)
      validate(need(length(sel_groups) > 0, "Select at least one ruminant group."))
      
      d <- prep_df()
      b <- scen_base()
      
      df_base <- d %>%
        dplyr::filter(.data$Scenario == b, .data$Year == base_year, .data$group %in% sel_groups) %>%
        dplyr::mutate(period = "BASE")
      
      yc <- year_cmp()
      validate(need(!is.na(yc), "No data available for selected scenario."))
      
      df_scen <- d %>%
        dplyr::filter(.data$Scenario == input$scen_cmp, .data$Year == yc, .data$group %in% sel_groups) %>%
        dplyr::mutate(period = "SCEN")
      
      out <- dplyr::bind_rows(df_base, df_scen) %>%
        dplyr::group_by(.data$period, .data$System, .data$group) %>%
        dplyr::summarise(head = sum(.data$head, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(
          period = factor(.data$period, levels = c("BASE","SCEN")),
          System = factor(.data$System, levels = LS_SYSTEM_LEVELS),
          group  = factor(as.character(.data$group), levels = sel_groups)
        )
      
      out %>%
        tidyr::complete(
          period = c("BASE","SCEN"),
          System = factor(LS_SYSTEM_LEVELS, levels = LS_SYSTEM_LEVELS),
          group  = factor(sel_groups, levels = sel_groups),
          fill   = list(head = 0)
        )
    })
    
    # ---- Bars -------------------------------------------------------------
    # ---- Bars -------------------------------------------------------------
    output$p_bars <- renderPlotly({
      req(input$scen_cmp)
      
      # ------------------------------------------------------------
      # 1) Mapping "codes internes" -> "labels affichés"
      #    (On NE change pas les codes utilisés pour la data/palette)
      # ------------------------------------------------------------
      RUM_CODE_TO_LABEL <- c(
        "Beef cattle"           = "Beef cattle",
        "Dairy cattle"          = "Dairy cattle",
        "Dairy sheep and goats" = "Small Dairy ruminants",
        "Meat sheep and goats"  = "Small meat ruminants"
      )
      
      rum_group_label <- function(code){
        code <- as.character(code)
        out  <- unname(RUM_CODE_TO_LABEL[code])
        ifelse(is.na(out) | out == "", code, out)
      }
      rum_group_label_vec <- function(x){
        x <- as.character(x)
        vapply(x, rum_group_label, character(1))
      }
      
      # robuste: si input/legend renvoient déjà le label, on le reconvertit en code
      RUM_LABEL_TO_CODE <- setNames(names(RUM_CODE_TO_LABEL), unname(RUM_CODE_TO_LABEL))
      norm_group_code <- function(x){
        x <- stringr::str_squish(as.character(x))
        dplyr::recode(x, !!!RUM_LABEL_TO_CODE, .default = x)
      }
      
      # ------------------------------------------------------------
      # 2) Sélection groupes (on accepte codes OU labels en input)
      # ------------------------------------------------------------
      sel_groups_in <- input$rum_groups %||% character(0)
      sel_groups    <- intersect(RUMINANT_GROUPS, norm_group_code(sel_groups_in))
      if (!length(sel_groups)) return(.empty_plot("Select at least one ruminant group."))
      
      yc <- year_cmp()
      if (is.na(yc)) return(.empty_plot("No year available for selected scenario."))
      
      d <- d_period()
      if (!nrow(d)) return(.empty_plot("No system-level data to display."))
      
      th <- get_plotly_tokens()
      
      max_head <- max(d$head, na.rm = TRUE)
      if (!is.finite(max_head) || max_head <= 0) max_head <- 1
      
      if (max_head >= 1e6) {
        div <- 1e6; ylab <- "Million TLU"
      } else if (max_head >= 1e3) {
        div <- 1e3; ylab <- "Thousand TLU"
      } else {
        div <- 1; ylab <- "TLU"
      }
      
      d2 <- d %>% dplyr::mutate(y = .data$head / div)
      
      ymax_stack <- d2 %>%
        dplyr::group_by(.data$period, .data$System) %>%
        dplyr::summarise(y_tot = sum(.data$y, na.rm = TRUE), .groups = "drop") %>%
        dplyr::summarise(mx = max(.data$y_tot, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(.data$mx)
      
      if (!is.finite(ymax_stack) || ymax_stack <= 0) ymax_stack <- 1
      ymax <- 1.10 * ymax_stack
      
      # palette inchangée (codes -> couleurs)
      pal_code <- group_palette_for(sel_groups) # names = codes
      
      b_code  <- scen_base()
      b_label <- scenario_label(b_code)
      s_label <- scenario_label(input$scen_cmp)
      
      mk <- function(dd, show_legend, y_title, sel_groups){
        
        # --- on switch le fill sur les LABELS, pas sur les codes ---
        dd_plot <- dd %>%
          dplyr::mutate(
            group_code = as.character(.data$group),
            group_lab  = rum_group_label(.data$group_code),
            group_lab  = factor(.data$group_lab, levels = rum_group_label_vec(sel_groups))
          )
        
        # --- même couleurs qu'avant, mais renommées par labels ---
        pal_code_loc <- group_palette_for(sel_groups) # codes -> col
        pal_lab      <- setNames(unname(pal_code_loc), rum_group_label_vec(names(pal_code_loc)))
        
        gg <- ggplot(dd_plot, ggplot2::aes(
          x = System, y = y, fill = group_lab,
          text = paste0(
            "System: ", System, "<br>",
            "Group: ", as.character(group_lab), "<br>",
            "Stock: ", formatC(head, format = "f", digits = 0, big.mark = " ", decimal.mark = "."), " TLU"
          )
        )) +
          ggplot2::geom_col(width = 0.72) +
          ggplot2::scale_x_discrete(drop = FALSE, expand = ggplot2::expansion(add = c(0.55, 1.10))) +
          ggplot2::scale_fill_manual(values = pal_lab, drop = FALSE) +
          ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
          ggplot2::coord_cartesian(ylim = c(0, ymax), expand = FALSE) +
          ggplot2::labs(x = NULL, y = y_title, fill = NULL) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
            legend.position   = if (show_legend) "top" else "none",
            panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
            plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
            panel.border      = ggplot2::element_rect(colour = th$axis_linecolor, fill = NA, linewidth = 0.4),
            legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
            legend.key        = ggplot2::element_rect(fill = "transparent", colour = NA),
            axis.text.x       = ggplot2::element_text(hjust = 0.5, color = th$muted_color),
            axis.text.y       = ggplot2::element_text(color = th$muted_color),
            axis.title.y      = ggplot2::element_text(margin = ggplot2::margin(r = 6), color = th$muted_color)
          )
        
        pl <- plotly::ggplotly(gg, tooltip = "text") %>%
          plotly::layout(
            barmode = "stack",
            xaxis = list(
              ticklabelposition = "outside",
              tickpadding = 6,
              ticklen = 6,
              automargin = TRUE,
              title = list(standoff = 2)
            ),
            yaxis = list(
              ticklabelposition = "outside",
              tickpadding = 6,
              ticklen = 6,
              automargin = TRUE,
              title = list(standoff = 2)
            ),
            margin = list(l = 40, r = 24, b = 36, t = 8, pad = 0)
          )
        
        pl <- plotly_apply_global_theme(pl, bg = "transparent", grid = "y")
        pl
      }
      
      p_left  <- mk(d2 %>% dplyr::filter(period == "BASE"), show_legend = FALSE, y_title = ylab, sel_groups = sel_groups)
      p_right <- mk(d2 %>% dplyr::filter(period == "SCEN"), show_legend = TRUE,  y_title = "",   sel_groups = sel_groups) %>%
        plotly::layout(yaxis = list(title = list(text = "")))
      
      p <- plotly::subplot(p_left, p_right, nrows = 1, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
        plotly::layout(
          barmode = "stack",
          
          # more room on top for legend
          margin = list(l = 65, r = 20, t = 80, b = 60),
          
          # legend above the whole plot
          legend = list(
            orientation = "h",
            x = 0.5, xanchor = "center",
            y = 1.12, yanchor = "bottom",
            font = list(color = th$muted_color)
          ),
          
          xaxis  = list(ticklabelposition="outside", tickpadding=6, ticklen=6, automargin=TRUE, title=list(standoff=2)),
          xaxis2 = list(ticklabelposition="outside", tickpadding=6, ticklen=6, automargin=TRUE, title=list(standoff=2)),
          yaxis  = list(ticklabelposition="outside", tickpadding=6, ticklen=6, automargin=TRUE, title=list(standoff=2)),
          
          shapes = list(
            list(
              type  = "line",
              layer = "above",
              xref  = "paper", yref = "paper",
              x0 = 0.5, x1 = 0.5,
              y0 = 0.04, y1 = 0.96,
              line = list(color = th$baseline_color, width = 2)
            )
          ),
          
          annotations = list(
            list(
              text = b_label,
              x = 0.18, y = 1, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(size = 14)
            ),
            list(
              text = s_label,
              x = 0.78, y = 1, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(size = 14)
            ),
            list(
              x = 0.513, y = 0.50, xref = "paper", yref = "paper",
              ax = -30, ay = 0,
              showarrow  = TRUE,
              arrowhead  = 3,
              arrowsize  = 1.3,
              arrowwidth = 2.5,
              arrowcolor = th$baseline_color,
              text = ""
            )
          )
        ) %>%
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c(
            "lasso2d","select2d","zoom2d","zoomIn2d","zoomOut2d",
            "autoScale2d","pan2d","resetScale2d"
          )
        )
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      
      # Fix legend duplication
      seen <- character(0)
      p$x$data <- lapply(p$x$data, function(tr){
        xa <- tr$xaxis %||% "x"
        nm <- tr$name  %||% ""
        if (xa == "x") {
          tr$showlegend <- FALSE
          return(tr)
        }
        if (nzchar(nm) && nm %in% seen) {
          tr$showlegend <- FALSE
        } else if (nzchar(nm)) {
          tr$showlegend <- TRUE
          seen <<- c(seen, nm)
        } else {
          tr$showlegend <- FALSE
        }
        tr
      })
      
      # Capture legend interactions via JS
      inp_id <- session$ns("bars_legend_evt")
      p <- htmlwidgets::onRender(
        p,
        sprintf(
          "function(el, x){
var gd = document.getElementById(el.id);
if(!gd) return;

if(gd.__legendHandlersAttached) return;
gd.__legendHandlersAttached = true;

function traceNameFromEvent(e){
  if(!gd.data || typeof e.curveNumber === 'undefined') return null;
  var tr = gd.data[e.curveNumber];
  if(!tr) return null;
  return tr.name || null;
}

gd.on('plotly_legendclick', function(e){
  var nm = traceNameFromEvent(e);
  if(window.Shiny && nm){
    Shiny.setInputValue('%s', {type:'click', name:nm, ts:Date.now()}, {priority:'event'});
  }
});

gd.on('plotly_legenddoubleclick', function(e){
  var nm = traceNameFromEvent(e);
  if(window.Shiny && nm){
    Shiny.setInputValue('%s', {type:'doubleclick', name:nm, ts:Date.now()}, {priority:'event'});
  }
});
}", inp_id, inp_id)
      )
      
      p
    })
    
    
    # ---- Pies -------------------------------------------------------------
    pie_data <- reactive({
      req(input$scen_cmp, input$rum_groups)
      yc <- year_cmp()
      req(!is.na(yc))
      
      g_pie <- groups_for_pies()
      validate(need(length(g_pie) > 0, "No ruminant group visible for pies (all hidden)."))
      
      d_period() %>%
        dplyr::filter(.data$group %in% g_pie) %>%
        dplyr::group_by(.data$period, .data$System) %>%
        dplyr::summarise(head = sum(.data$head, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(System = factor(.data$System, levels = LS_SYSTEM_LEVELS)) %>%
        tidyr::complete(
          period = c("BASE","SCEN"),
          System = factor(LS_SYSTEM_LEVELS, levels = LS_SYSTEM_LEVELS),
          fill   = list(head = 0)
        )
    })
    
    make_pie <- function(dd, show_legend = FALSE){
      th <- get_plotly_tokens()
      
      total <- sum(dd$head, na.rm = TRUE)
      dd2 <- dd %>%
        dplyr::mutate(
          pct100  = if (is.finite(total) && total > 0) 100 * head / total else 0,
          pct_txt = ifelse(pct100 < 0.05, "", sprintf("%.1f%%", pct100))
        )
      
      p <- plotly::plot_ly(
        data   = dd2,
        type   = "pie",
        labels = ~System,
        values = ~head,
        text   = ~pct_txt,
        textinfo = "text",
        textposition = "auto",
        insidetextorientation = "horizontal",
        textfont = list(color = th$font_color, size = 12),
        marker = list(colors = unname(pal_sys[as.character(dd2$System)])),
        hovertemplate = paste0(
          "%{label}<br>",
          "%{customdata:.1f}%<br>",
          "%{value:,.0f} TLU",
          "<extra></extra>"
        ),
        customdata = ~pct100,
        showlegend = show_legend
      ) %>%
        plotly::layout(
          margin = list(l = 10, r = 10, t = 10, b = 10),
          legend = list(
            orientation = "v",
            x = 1.02, y = 1,
            font = list(color = th$muted_color)
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "none")
      p
    }
    
    output$p_pie_base <- renderPlotly({
      req(input$scen_cmp, input$rum_groups)
      if (!length(groups_for_pies())) return(.empty_plot("No ruminant group visible for pies (all hidden)."))
      d <- pie_data() %>% dplyr::filter(.data$period == "BASE")
      if (!nrow(d) || sum(d$head, na.rm = TRUE) <= 0) return(.empty_plot("No data for current selection."))
      make_pie(d, show_legend = FALSE)
    })
    
    output$p_pie_scen <- renderPlotly({
      req(input$scen_cmp, input$rum_groups)
      if (!length(groups_for_pies())) return(.empty_plot("No ruminant group visible for pies (all hidden)."))
      d <- pie_data() %>% dplyr::filter(.data$period == "SCEN")
      if (!nrow(d) || sum(d$head, na.rm = TRUE) <= 0) return(.empty_plot("No data for current selection."))
      make_pie(d, show_legend = TRUE)
    })
    
    # ---- CSV export -------------------------------------------------------
    output$dl_livestock_system_csv <- downloadHandler(
      filename = function() {
        paste0("Livestock_ruminants_by_system_", gsub("[^A-Za-z0-9]+","_", r_country()), ".csv")
      },
      content = function(file) {
        req(input$scen_cmp, input$rum_groups)
        yc <- year_cmp()
        req(!is.na(yc))
        
        sel_groups <- intersect(RUMINANT_GROUPS, input$rum_groups)
        req(length(sel_groups) > 0)
        
        b <- scen_base()
        
        df <- prep_df() %>%
          dplyr::filter(.data$group %in% sel_groups) %>%
          dplyr::filter(
            (.data$Scenario == b & .data$Year == base_year) |
              (.data$Scenario == input$scen_cmp & .data$Year == yc)
          ) %>%
          dplyr::mutate(
            System = as.character(.data$System),
            group  = as.character(.data$group),
            Unit   = "TLU"
          ) %>%
          dplyr::select(Region, Scenario, Year, System, group, Unit, head)
        
        df_sys_tot <- df %>%
          dplyr::group_by(Region, Scenario, Year, System) %>%
          dplyr::summarise(head = sum(head, na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(group = "ALL_SELECTED_RUMINANTS", Unit = "TLU") %>%
          dplyr::select(Region, Scenario, Year, System, group, Unit, head)
        
        final <- dplyr::bind_rows(
          df %>% dplyr::mutate(dataset = "system_x_group"),
          df_sys_tot %>% dplyr::mutate(dataset = "system_total")
        ) %>%
          dplyr::select(dataset, Region, Scenario, Year, System, group, Unit, head)
        
        readr::write_delim(final, file, delim = ";", na = "")
      }
    )
    
    # ---- Note -------------------------------------------------------------
    output$note <- renderUI({
      b  <- scen_base()
      yc <- year_cmp()
      yc_txt <- if (is.na(yc)) "n/a" else as.character(yc)
      
      htmltools::HTML(paste0(
        "The purpose of this module is to compare the <strong>quantity of ruminant animals (in TLU)</strong> by <strong>livestock system</strong> ",
        "between the <strong>base year</strong> and the <strong>selected scenarios</strong>.</p>",
        "<p><strong>Stacked bars</strong> show the <strong>absolute stocks (TLU)</strong> by system, with colours indicating the ruminant groups (according to your selection).</p>",
        "<p><strong>Pie charts</strong> present the same information as <strong>shares (%)</strong>, i.e. the proportion of each system in the total selected ruminant stocks.</p>"
      ))
    })
    
    invisible(list(
      scen_levels_effective = scen_levels_effective
    ))
  })
}
