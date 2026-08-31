# R/00_helper_scenarios.R
# ============================================================
# SCENARIOS — helper central (actifs / effectifs / redondance / labels)
# Dépend du config R/00_config_scenarios.R si présent.
# Règle : on retire un extra de la vue UNIQUEMENT si "redundant" avec la diète probable.
# Si un extra est "missing" dans les données, on ne fait rien de spécial (pas de banner).
#
# CHANGEMENT : le helper ne prend plus un code de scénario "extra" unique
# (r_extra_code) mais un code de VUE (r_view_code), ex. "comparaison_dietes"
# ou "contraintes_terre_autosuffisance" (cf. R/00_config_scenarios.R -> SC$view_choices).
# Chaque vue peut contenir 0, 1 ou plusieurs scénarios "extra" (hors base year / 3 diètes),
# et chacun est testé individuellement pour la redondance.
# ============================================================

sc_get <- function(name, default = NULL){
  if (exists(name, inherits = TRUE)) get(name, inherits = TRUE) else default
}

# Fallback normalisation si jamais le config n'est pas chargé
if (!exists("scenario_code", mode = "function")) {
  scenario_code <- function(x) stringr::str_squish(as.character(x))
}

# Fallback label si jamais le config n'est pas chargé
if (!exists("scenario_label", mode = "function")) {
  scenario_label <- function(code) scenario_code(code)
}

# Helper serveur : retourne un "objet scénarios" (liste de reactives)
scenarios_context_server <- function(
    fact,
    r_country,
    r_view_code,      # <-- CHANGEMENT : reactive() renvoyant le code de VUE choisi (ex: input$scenario_view)
    tol_rel = NULL,    # optionnel, sinon on prend le config
    debug = FALSE
){
  # --- Sources config (CODES uniquement) -----------------------------------
  scen_base_year  <- sc_get("SCENARIO_BASE_YEAR_CODE", scenario_code("Année de base"))
  scen_base_diets <- sc_get("SCENARIOS_BASE_CODES",   scenario_code(c("Même diète","Diète saine","Diète probable")))
  scen_ref        <- sc_get("SCENARIO_REF_CODE",      scenario_code("Diète probable"))

  scen_levels_default <- sc_get(
    "SCENARIO_LEVELS_DEFAULT",
    unique(c(
      scen_base_year,
      scen_base_diets,
      scenario_code(unname(sc_get("SCENARIOS_EXTRA_CHOICES", c())))
    ))
  )

  # --- Vues (code vue -> vecteur de codes scénarios) ------------------------
  view_codes_map <- sc_get("SCENARIO_VIEW_CODES", list())

  # --- tol_rel : valeur effective (relay config) ---------------------------
  tol_rel_cfg <- sc_get("SCENARIO_REDUNDANCE_TOL_REL", NULL)

  # fallback sur SC$tol_rel_default si présent
  if (is.null(tol_rel_cfg) && exists("SC", inherits = TRUE)) {
    SC_obj <- get("SC", inherits = TRUE)
    if (is.list(SC_obj) && !is.null(SC_obj$tol_rel_default)) {
      tol_rel_cfg <- SC_obj$tol_rel_default
    }
  }

  # fallback final si config incomplet
  if (!is.numeric(tol_rel_cfg) || length(tol_rel_cfg) != 1 || !is.finite(tol_rel_cfg) || tol_rel_cfg <= 0) {
    tol_rel_cfg <- 0.005
  }

  tol_rel_effective <- tol_rel_cfg
  if (is.numeric(tol_rel) && length(tol_rel) == 1 && is.finite(tol_rel) && tol_rel > 0) {
    tol_rel_effective <- tol_rel
  }

  # Map éventuel label UI -> code data (utile si un jour fact contient le label)
  extra_choices <- sc_get("SCENARIOS_EXTRA_CHOICES", c())
  EXTRA_LABEL_TO_CODE <- if (length(extra_choices)) setNames(unname(extra_choices), names(extra_choices)) else NULL

  # Normalisation du code de vue choisi
  # IMPORTANT : on n'utilise PAS scenario_code() ici, car cette fonction remplace
  # les "_" par des "-" (utile pour matcher fact$Scenario, ex: "Prob-S-limitee"),
  # alors que les codes de VUE ("comparaison_dietes", "contraintes_terre_autosuffisance")
  # utilisent volontairement des underscores et doivent matcher tels quels les noms
  # de SCENARIO_VIEW_CODES dans le config.
  r_view_code_norm <- shiny::reactive({
    stringr::str_squish(as.character(r_view_code()))
  })

  # Codes de scénarios définis par la vue choisie (ordre conservé)
  r_view_scenario_codes <- shiny::reactive({
    vc <- view_codes_map[[r_view_code_norm()]]
    if (is.null(vc)) {
      if (isTRUE(debug)) message("SC helper: vue inconnue '", r_view_code_norm(), "' -> fallback base_year + base_diets.")
      vc <- c(scen_base_year, scen_base_diets)
    }
    scenario_code(vc)
  })

  # Extras de la vue = scénarios de la vue qui ne sont ni base year ni une des 3 diètes de base
  r_view_extra_codes <- shiny::reactive({
    setdiff(r_view_scenario_codes(), c(scen_base_year, scen_base_diets))
  })

  # Données pays (scenario_norm) — robuste même si fact déjà normalisé
  df_country_all <- shiny::reactive({
    df <- fact %>%
      dplyr::filter(Region == r_country()) %>%
      dplyr::mutate(Scenario = scenario_code(Scenario))

    if (!is.null(EXTRA_LABEL_TO_CODE)) {
      df <- df %>% dplyr::mutate(
        Scenario = dplyr::recode(Scenario, !!!EXTRA_LABEL_TO_CODE, .default = Scenario)
      )
    }
    df
  })

  # Présents dans les données du pays
  r_present <- shiny::reactive({
    dfc <- df_country_all()
    unique(as.character(dfc$Scenario))
  })

  # Actifs = tous les scénarios définis par la vue choisie
  r_scenarios_active <- shiny::reactive({
    unique(r_view_scenario_codes())
  })

  # Statut de CHAQUE extra de la vue : "ok" / "redundant" (missing => "ok", pas de banner)
  # -> named character vector, ex: c("100% forêt conservée" = "ok", "80% autosuff_céréales" = "redundant")
  r_extras_status <- shiny::reactive({
    extras  <- r_view_extra_codes()
    present <- r_present()

    if (length(extras) == 0) return(character(0))

    fun_red <- get0("is_scenario_redundant", mode = "function", inherits = TRUE)

    vapply(extras, function(extra) {
      if (!any(present == extra)) {
        if (isTRUE(debug)) message("SC helper: extra '", extra, "' missing in data -> treated as ok (no action).")
        return("ok")
      }

      if (is.null(fun_red)) {
        if (isTRUE(debug)) message("SC helper: is_scenario_redundant() introuvable -> pas de test redondance.")
        return("ok")
      }

      is_red <- try(
        isTRUE(fun_red(
          df_country     = df_country_all(),
          scenario_ref   = scen_ref,
          scenario_extra = extra,
          tol_rel        = tol_rel_effective
        )),
        silent = TRUE
      )

      if (inherits(is_red, "try-error")) {
        if (isTRUE(debug)) message("SC helper: is_scenario_redundant('", extra, "') error -> treated as ok.")
        return("ok")
      }

      if (is_red) "redundant" else "ok"
    }, character(1))
  })

  # Effectifs = actifs ∩ présents, en retirant les extras marqués "redundant"
  r_scenarios_effective <- shiny::reactive({
    wanted  <- r_scenarios_active()
    status  <- r_extras_status()

    redundant_extras <- names(status)[status == "redundant"]
    if (length(redundant_extras)) {
      wanted <- setdiff(wanted, redundant_extras)
    }

    present <- r_present()
    wanted[wanted %in% present]
  })

  # Niveaux d'affichage stables (ordre global) restreints aux effectifs
  r_scen_levels_effective <- shiny::reactive({
    eff <- r_scenarios_effective()
    scen_levels_default[scen_levels_default %in% eff]
  })

  list(
    scen_base_year          = scen_base_year,
    scen_ref                = scen_ref,
    scen_base_diets         = scen_base_diets,
    scen_levels_default     = scen_levels_default,
    tol_rel_effective       = tol_rel_effective,
    r_view_code_norm        = r_view_code_norm,
    r_view_scenario_codes   = r_view_scenario_codes,
    r_extras_status         = r_extras_status,
    r_scenarios_active      = r_scenarios_active,
    r_scenarios_effective   = r_scenarios_effective,
    r_scen_levels_effective = r_scen_levels_effective,
    scenario_label          = scenario_label
  )
}
