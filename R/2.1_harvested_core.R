# R/2.1_harvested_core.R
# ---------------------------------------------------------------

harvested_core <- function(
    fact,
    r_country,
    harvest_element,
    exclude_items,
    value_multiplier,
    group_var,
    r_scenarios = NULL
){
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  
  # Normalisation "config-first"
  sc_norm <- if (exists("scenario_code", mode = "function", inherits = TRUE)) {
    scenario_code
  } else {
    function(x) stringr::str_squish(as.character(x))
  }
  
  # Codes issus du config (fallbacks sûrs)
  scen_base <- if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
    sc_norm(get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE))
  } else {
    sc_norm("Année de base")
  }
  
  scen_ref <- if (exists("SCENARIO_REF_CODE", inherits = TRUE)) {
    sc_norm(get("SCENARIO_REF_CODE", inherits = TRUE))
  } else {
    sc_norm("Diète probable")
  }
  
  base_codes <- if (exists("SCENARIOS_BASE_CODES", inherits = TRUE)) {
    sc_norm(get("SCENARIOS_BASE_CODES", inherits = TRUE))
  } else {
    sc_norm(c("Même diète", "Diète probable", "Diète saine"))
  }
  
  extra_codes <- if (exists("SCENARIOS_EXTRA_CODES", inherits = TRUE)) {
    sc_norm(get("SCENARIOS_EXTRA_CODES", inherits = TRUE))
  } else {
    character(0)
  }
  
  levels_default <- if (exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
    sc_norm(get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE))
  } else {
    unique(c(scen_base, base_codes, extra_codes))
  }
  
  # --- Scénarios demandés (helper global -> r_scenarios) -------------------
  scen_requested_raw <- shiny::reactive({
    if (!is.null(r_scenarios)) {
      sc <- try(r_scenarios(), silent = TRUE)
      if (!inherits(sc, "try-error") && !is.null(sc) && length(sc) > 0) {
        return(sc_norm(unname(sc)))
      }
    }
    levels_default
  })
  
  # Ordre stable : d'abord levels_default (intersection), puis le reste
  scen_show <- shiny::reactive({
    req <- unique(sc_norm(scen_requested_raw()))
    req <- req[nzchar(req)]
    # baseline toujours présent (les modules harvested en dépendent)
    req <- unique(c(scen_base, req))
    
    ordered <- levels_default[levels_default %in% req]
    tailing <- setdiff(req, ordered)
    unique(c(ordered, tailing))
  })
  
  # Niveaux factor (scénarios affichés)
  levels_all <- shiny::reactive({
    scen_show()
  })
  
  # Diet scenarios = tout sauf base
  scen_diets_effective <- shiny::reactive({
    setdiff(scen_show(), scen_base)
  })
  
  # Extra(s) effectivement demandés (vecteur possible)
  scen_extra_selected <- shiny::reactive({
    intersect(scen_show(), extra_codes)
  })
  
  # ------------------------------------------
  # 1) Années utilisées par scénario (sans logique missing)
  # - si un scénario n'a pas de data : year_used = NA, on ne bloque pas ici
  # ------------------------------------------
  years_by_scenario <- shiny::reactive({
    scen_use <- scen_show()
    shiny::req(length(scen_use) > 0)
    
    dat <- fact %>%
      dplyr::filter(
        Region == r_country(),
        stringr::str_to_lower(stringr::str_squish(Element)) ==
          stringr::str_to_lower(stringr::str_squish(harvest_element))
      ) %>%
      dplyr::mutate(
        Scenario = sc_norm(Scenario)
      ) %>%
      dplyr::filter(Scenario %in% scen_use)
    
    # base = 2018 si dispo, sinon dernier millésime non-NA
    base_year <- NA_real_
    dat_base <- dat %>% dplyr::filter(Scenario == scen_base)
    if (nrow(dat_base) > 0) {
      base_year <- if (any(dat_base$Year == 2018 & !is.na(dat_base$Value))) {
        2018
      } else {
        suppressWarnings(max(dat_base$Year[!is.na(dat_base$Value)], na.rm = TRUE))
      }
    }
    
    yrs_other <- dat %>%
      dplyr::filter(Scenario != scen_base) %>%
      dplyr::group_by(Scenario) %>%
      dplyr::summarise(
        year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
        .groups = "drop"
      )
    
    yrs <- dplyr::bind_rows(
      tibble::tibble(Scenario = scen_base, year_used = base_year),
      yrs_other
    )
    
    # On garde la liste complète des scénarios à afficher, même si year_used est NA
    tibble::tibble(Scenario = scen_use) %>%
      dplyr::left_join(yrs, by = "Scenario") %>%
      dplyr::mutate(
        Scenario = factor(Scenario, levels = scen_use)
      ) %>%
      dplyr::arrange(Scenario)
  }) %>% shiny::bindCache(r_country(), harvest_element, scen_show())
  
  # ------------------------------------------
  # 2) Données empilées (barres) — seulement pour les scénarios avec year_used défini
  # (pas de logique "missing" => on ne stoppe pas si un scénario manque, il aura 0 ligne)
  # ------------------------------------------
  data_harvested <- shiny::reactive({
    yrs <- years_by_scenario()
    scen_use <- scen_show()
    
    dat <- fact %>%
      dplyr::mutate(
        Scenario = sc_norm(Scenario),
        Element  = stringr::str_squish(Element),
        Item     = stringr::str_squish(Item)
      ) %>%
      dplyr::filter(
        Region == r_country(),
        stringr::str_to_lower(Element) == stringr::str_to_lower(stringr::str_squish(harvest_element)),
        Scenario %in% scen_use
      ) %>%
      dplyr::inner_join(
        yrs %>% dplyr::filter(is.finite(year_used)) %>% dplyr::mutate(Scenario = sc_norm(as.character(Scenario))),
        by = "Scenario"
      ) %>%
      dplyr::filter(
        Year == year_used,
        !Item %in% exclude_items
      ) %>%
      dplyr::group_by(Scenario, Item, year_used) %>%
      dplyr::summarise(value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(
        Scenario = factor(as.character(Scenario), levels = scen_use),
        value  = value * value_multiplier,
        year   = year_used,
        scen_i = match(as.character(Scenario), scen_use),
        scen_t = as.character(Scenario)
      )
    
    # Ordre stable des items : décroissant sur la base (si base présente)
    base_order <- dat %>%
      dplyr::filter(as.character(Scenario) == scen_base) %>%
      dplyr::group_by(Item) %>%
      dplyr::summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(tot)) %>%
      dplyr::pull(Item)
    
    items_levels <- unique(c(base_order, setdiff(unique(dat$Item), base_order)))
    
    dat %>%
      dplyr::mutate(Item = factor(as.character(Item), levels = items_levels)) %>%
      dplyr::arrange(Scenario, Item)
  }) %>% shiny::bindCache(r_country(), harvest_element, scen_show())
  
  # ------------------------------------------
  # 3) Données évolution 2018 -> 2050 (group_var / Item)
  # - utilise scen_diets_effective() (donc scénarios demandés, sans base)
  # - si base_year/target_year indisponibles, dataset vide (sans message "missing")
  # ------------------------------------------
  data_harvested_groups <- shiny::reactive({
    yrs <- years_by_scenario()
    scen_diets <- scen_diets_effective()
    
    # base / target years
    base_year <- yrs$year_used[as.character(yrs$Scenario) == scen_base][1] %||% NA_real_
    target_year <- suppressWarnings(max(yrs$year_used[as.character(yrs$Scenario) != scen_base], na.rm = TRUE))
    if (!is.finite(base_year) || !is.finite(target_year) || length(scen_diets) == 0) {
      return(tibble::tibble())
    }
    
    group_col <- if (!is.null(group_var) && group_var %in% names(fact)) group_var else "Item"
    
    base_area <- fact %>%
      dplyr::mutate(
        Scenario = sc_norm(Scenario),
        Element  = stringr::str_squish(Element)
      ) %>%
      dplyr::filter(
        Region == r_country(),
        stringr::str_to_lower(Element) == stringr::str_to_lower(stringr::str_squish(harvest_element)),
        Scenario == scen_base,
        Year == base_year,
        !Item %in% exclude_items
      ) %>%
      dplyr::mutate(group = .data[[group_col]]) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(area_2018 = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(area_2018 = area_2018 * value_multiplier)
    
    harv_2050_area <- fact %>%
      dplyr::mutate(
        Scenario = sc_norm(Scenario),
        Element  = stringr::str_squish(Element)
      ) %>%
      dplyr::filter(
        Region == r_country(),
        stringr::str_to_lower(Element) == stringr::str_to_lower(stringr::str_squish(harvest_element)),
        Scenario %in% sc_norm(scen_diets),
        Year == target_year,
        !Item %in% exclude_items
      ) %>%
      dplyr::mutate(group = .data[[group_col]]) %>%
      dplyr::group_by(Scenario, group) %>%
      dplyr::summarise(area_2050 = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(area_2050 = area_2050 * value_multiplier)
    
    order_groups <- base_area %>%
      dplyr::arrange(dplyr::desc(area_2018)) %>%
      dplyr::pull(group)
    
    base_long <- base_area %>%
      tidyr::crossing(Scenario = scen_diets) %>%
      dplyr::transmute(group, Scenario, Year = base_year, area = area_2018)
    
    scen_long <- harv_2050_area %>%
      dplyr::transmute(group, Scenario, Year = target_year, area = area_2050)
    
    df <- dplyr::bind_rows(base_long, scen_long) %>%
      dplyr::mutate(
        Scenario = factor(sc_norm(as.character(Scenario)), levels = sc_norm(scen_diets)),
        group    = factor(group, levels = order_groups)
      )
    
    base_vals <- df %>%
      dplyr::filter(Year == base_year) %>%
      dplyr::select(group, Scenario, area_base = area)
    
    df %>%
      dplyr::left_join(base_vals, by = c("group", "Scenario")) %>%
      dplyr::mutate(
        pct_change = dplyr::if_else(
          is.finite(area_base) & area_base > 0,
          100 * (area - area_base) / area_base,
          NA_real_
        )
      )
  }) %>% shiny::bindCache(r_country(), harvest_element, scen_show())
  
  list(
    scen_base             = scen_base,
    scen_ref              = scen_ref,
    scen_show             = scen_show,            # reactive : vecteur ordonné des scénarios à afficher
    scen_diets_effective  = scen_diets_effective, # reactive : scen_show sans base
    levels_all            = levels_all,           # reactive : identique à scen_show
    years_by_scenario     = years_by_scenario,
    data_harvested        = data_harvested,
    data_harvested_groups = data_harvested_groups
  )
}
