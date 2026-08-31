# R/0.0_config_scenarios.R
# Ce module permet de centraliser l'ordre et le nom des scénarios. Ainsi les noms, l'ordre peuvent être changés pour toute l'application !
# Par ailleurs, ce module nous permet de définir le coefficient qui determine si 2 scénarios sont redondants
# ce qui conditionne ensuite l'affichage : aujourd'hui, il est à 0.05
# ============================================================
# SINGLE SOURCE OF TRUTH — SCENARIOS (CODES in data, LABELS in UI)
# ============================================================

# 0) Normalisation (anti-espaces, anti-facteurs)
scenario_code <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x <- stringr::str_replace_all(x, "_", "-")
  x
}

# 1) Codes (DOIVENT matcher fact$Scenario)
SCENARIO_BASE_YEAR_CODE <- scenario_code("Année de base")

SCENARIOS_BASE_CODES <- scenario_code(c(
  "Même diète",
  "Diète saine",
  "Diète probable"
))

SCENARIO_REF_CODE <- scenario_code("Diète probable")

# Codes des scénarios "contraintes" (utilisés uniquement dans la 2e vue)
SCENARIO_TOTAL_AREA_STRESS_CODE <- scenario_code("Prob-S-limitee")
SCENARIO_FOREST_PRESERVED_CODE  <- scenario_code("100% forêt conservée")
SCENARIO_CEREAL_SELFSUF_CODE    <- scenario_code("80% autosuff_céréales")

# 2) Anciens "extras" : conservés pour compatibilité (labels UI si besoin ailleurs)
SCENARIOS_EXTRA_CHOICES <- c(
  "Total area stress" = SCENARIO_TOTAL_AREA_STRESS_CODE,
  "Forest preserved"  = SCENARIO_FOREST_PRESERVED_CODE,
  "Cereals selfsuf."  = SCENARIO_CEREAL_SELFSUF_CODE
)
SCENARIOS_EXTRA_CODES <- scenario_code(unname(SCENARIOS_EXTRA_CHOICES))

# 3) Labels UI : names = CODES, values = LABELS
SCENARIO_LABELS <- c(
  "Année de base"          = "Base year",
  "Même diète"             = "Same diet",
  "Diète saine"            = "Healthy diet",
  "Diète probable"         = "Likely diet",
  "Prob-S-limitee"         = "Total area stress",
  "100% forêt conservée"   = "Forest preserved",
  "80% autosuff_céréales"  = "Cereals selfsuf."
)

scenario_label <- function(code){
  code <- scenario_code(code)
  out  <- unname(SCENARIO_LABELS[code])
  ifelse(is.na(out) | out == "", code, out)
}

# ============================================================
# 4) VUES (nouveau sélecteur à 2 choix)
# ============================================================
# names  = label affiché dans le selectInput
# values = code interne de la vue (utilisé dans server.R)
SCENARIOS_VIEW_CHOICES <- c(
  "Comparaison diètes"                    = "comparaison_dietes",
  "Contraintes terre ou autosuffisance"   = "contraintes_terre_autosuffisance"
)

# Pour chaque vue, la liste ORDONNÉE des codes de scénarios à afficher
SCENARIO_VIEW_CODES <- list(
  comparaison_dietes = c(
    SCENARIO_BASE_YEAR_CODE,
    SCENARIOS_BASE_CODES              # Même diète, Diète saine, Diète probable
  ),
  contraintes_terre_autosuffisance = c(
    SCENARIO_BASE_YEAR_CODE,
    SCENARIO_REF_CODE,                # Diète probable (= likely diet)
    SCENARIO_FOREST_PRESERVED_CODE,
    SCENARIO_CEREAL_SELFSUF_CODE
  )
)

# Fonction utilitaire : retourne les codes de scénarios à afficher pour une vue donnée
scenario_codes_for_view <- function(view_code) {
  out <- SCENARIO_VIEW_CODES[[view_code]]
  if (is.null(out)) {
    stop("Vue de scénario inconnue : ", view_code)
  }
  out
}

# 5) Ordre global stable : CODES uniquement (utile pour les facteurs/levels)
SCENARIO_LEVELS_DEFAULT <- unique(c(
  SCENARIO_BASE_YEAR_CODE,
  SCENARIOS_BASE_CODES,
  SCENARIOS_EXTRA_CODES
))

# 6) Tolérances / seuil redondance (si tu veux centraliser)
SCENARIO_REDUNDANCE_TOL_REL <- 0.02

# ============================================================
# SCENARIOS CONTRACT OBJECT (unique entrée pour les modules)
# ============================================================
SC <- list(
  code = scenario_code,
  base_year = SCENARIO_BASE_YEAR_CODE,
  base_diets = SCENARIOS_BASE_CODES,
  ref = SCENARIO_REF_CODE,

  extra_choices = SCENARIOS_EXTRA_CHOICES,          # conservé pour compat (names=UI label, values=CODE)
  extra_codes = SCENARIOS_EXTRA_CODES,               # conservé pour compat

  view_choices = SCENARIOS_VIEW_CHOICES,             # NOUVEAU : names=UI label, values=code vue (pour selectInput)
  view_codes = SCENARIO_VIEW_CODES,                  # NOUVEAU : code vue -> vecteur de codes scénarios
  codes_for_view = scenario_codes_for_view,          # NOUVEAU : fonction helper

  labels = SCENARIO_LABELS,                          # names=CODE, values=label UI
  levels_default = SCENARIO_LEVELS_DEFAULT,
  tol_rel_default = SCENARIO_REDUNDANCE_TOL_REL
)

scenario_label <- function(code){
  code <- SC$code(code)
  out  <- unname(SC$labels[code])
  ifelse(is.na(out) | out == "", code, out)
}
