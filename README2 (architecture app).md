# GlobAfrique – Application Shiny (architecture & maintenance)

Cette application Shiny (R) sert à valoriser les résultats d’une étude prospective agricole en Afrique, en affichant des graphiques interactifs par pays/zone, selon plusieurs scénarios (année témoin 2018 + scénarios de diètes, dont une variante avec contrainte d’expansion des terres).

## 1) Vue d’ensemble

L’application est structurée autour de :
1. **Données brutes** (CSV) déposées dans `data_raw/`.
2. **Données préparées** (tables `.rds`) stockées dans `data/` et utilisées par Shiny (plus légères que le CSV brut).
3. **Modules Shiny** (un module ≈ un graphique/onglet) rangés dans `R/`, avec une convention de nommage par onglet.
4. **Habillage** (CSS + assets) dans `www/`.
5. **Gestion d’environnement** via `renv/` et `renv.lock`.
6. **Déploiement shinyapps.io** via `rsconnect/`.

---

## 2) Arborescence (racine)

- `app.R`  
  Point d’entrée de l’application (UI + server + assemblage). C’est le fichier lancé par Shiny.

- `global.R`  
  Code global chargé au démarrage (packages, sourcing des scripts, objets partagés, options globales).  
  Typiquement : chargement des données, chargement des helpers, mise en place de constantes (si utilisé).

- `R/`  
  Contient l’ensemble des scripts R : chargement données, helpers, configuration, modules des onglets.

- `data_raw/`  
  Contient les données brutes (CSV) servant d’entrée au pipeline.

- `data/`  
  Contient les tables `.rds` prêtes à l’emploi (celles qui doivent être embarquées avec l’app pour un run rapide / déploiement).

- `www/`  
  Contient le style CSS et les ressources statiques (logos, images, drapeaux, PDF, etc.).

- `archives/`  
  Dossier de travail / anciens scripts / versions (non critique pour exécuter l’app).  
  À garder propre : rien d’indispensable au run ne doit y rester.

- `renv/` + `renv.lock`  
  Gestion des dépendances R : l’environnement est figé par `renv.lock`.

- `rsconnect/`  
  Métadonnées de déploiement shinyapps.io (connexion + configuration de déploiement).

- Fichiers “RStudio” : `.RData`, `.Rhistory`, `.Rprofile`, `.Renviron`  
  Non indispensables à l’app (à manier avec précaution, surtout en déploiement).

---

## 3) Dossiers clés

### 3.1 `data_raw/` (entrée : données brutes)

Contient les fichiers sources, typiquement :
- `BDD_CLEAN.csv` : base brute normalisée (source principale).  
  Cette base est **générée via une routine Excel** qui standardise le format.

- (éventuellement) `fact_results_tidy.csv` : export “tidy” complet, utilisé pour reconstruire les `.rds` si besoin.

Règle de maintenance :
- Pour **mettre à jour les données affichées par l’application**, il suffit de **remplacer** `BDD_CLEAN.csv` par une nouvelle version **strictement au même format** (mêmes colonnes, mêmes conventions), puis de **reconstruire** les `.rds` (voir section 4).

> Point de vigilance : si le format change (noms de colonnes, unités, modalités de variables), les modules peuvent casser (jointures, filtres, labels).

### 3.2 `data/` (sortie : données prêtes pour Shiny)

Contient des tables `.rds` dérivées des données brutes. Exemples typiques :
- `fact_results_tidy.rds` : faits principaux au format tidy.
- `fact_build_meta.rds` : métadonnées de build (version, date, etc.).
- `dim_scenario.rds`, `dim_method.rds`, `dim_constraint.rds` : dimensions utilisées pour alimenter les sélecteurs et stabiliser les libellés.

Objectif :
- **Réduire le poids** et accélérer le chargement par rapport au CSV brut.
- Stabiliser des tables de référence (dimensions) pour l’UI.

---

## 4) Flux de données (pipeline)

Principe général :
1. L’entrée est `data_raw/BDD_CLEAN.csv` (ou un export tidy équivalent).
2. Un script de préparation construit des tables “facts” et “dims”.
3. Les tables résultantes sont sauvegardées dans `data/*.rds`.
4. Shiny lit prioritairement `data/*.rds` au démarrage.

Scripts impliqués (dans `R/`, selon ta convention) :
- `0.0_load_data.R` : lecture des sources (`data_raw/`), chargement des tables préparées (`data/`) et/ou orchestration.
- `0.0_make_clean_tables.R` : transformation / nettoyage / création des tables `.rds`.
- `0.0_helpers_*.R` : fonctions utilitaires utilisées par le pipeline et/ou par les modules.
- `0.0_config_scenarios.R` : normalisation des scénarios (noms, ordre d’affichage, codes internes).

Bonnes pratiques :
- Les `.rds` doivent être **reproductibles** à partir de `data_raw/`.
- Les dimensions (`dim_*`) doivent être la source de vérité pour l’ordre des scénarios et l’affichage.

---

## 5) Code applicatif dans `R/`

### 5.1 Conventions de nommage des scripts/modules

Les scripts sont rangés par rôle et par onglet.

A. Préfixe `0.0_...`  
Scripts “socle” utilisés partout :
- configuration (scénarios, constantes),
- helpers (fonctions utilitaires),
- chargement/traitement des données,
- palettes / thèmes.

B. Modules d’onglets (convention par numéro)
- `1.x_...` : onglet 1 (et ses graphes)
- `2.x_...` : onglet 2
- `3.x_...` : onglet 3  
etc.

Chaque module encapsule généralement :
- une fonction UI (ex : `mod_xxx_ui(id)`)
- une fonction server (ex : `mod_xxx_server(id, data, inputs...)`)
- éventuellement des fonctions locales de préparation (filtrage, agrégation, formatage)

### 5.2 Organisation logique recommandée (lecture du code)

Ordre logique (du “plus global” au “plus spécifique”) :
1. `0.0_config_*` (paramètres, dictionnaires, ordres)
2. `0.0_helpers_*` (fonctions génériques)
3. `0.0_load_data.R` / `0.0_make_clean_tables.R` (pipeline)
5. `1.x_*`, `2.x_*`, `3.x_*` … (modules par onglet)

---

## 6) Style, thème et assets (`www/` + utilitaires R)

L’habillage est contrôlé par 3 “piliers” :

1. `R/0.0_utils_palette.R`  
   Palette centralisée utilisée dans les graphiques (barres empilées, camemberts, sankeys…).  
   Règle : toute modification des couleurs “dans les graphes” passe par ce fichier.

2. (si présent dans ton projet) `R/99_utils_plotly_theme.R`  
   Définition du thème plotly : polices, tailles, légendes, axes, fond, etc.  
   Deux thèmes peuvent être maintenus (clair/sombre) et sélectionnés au démarrage.

3. `www/app.css`  
   Style global de l’application : typographie, cartes/encadrés, marges, bandeaux, couleurs de fond, composants UI hors-graphes.  
   Règle : toute modification “structure UI” (hors graphiques) passe par ce fichier.

Assets dans `www/` :
- images (`logo_footer.png`, `fond.jpg`, etc.)
- `flags/` : drapeaux
- `continent/` : ressources spécifiques (ex : cartes)
- documents (ex : `Definitions_and_standards.pdf`)
- schémas (`schema_globagri.png`)

---

## 7) Lancer l’app en local

Pré-requis : R + packages verrouillés via `renv`.

Étapes usuelles :
1. Ouvrir le projet dans RStudio.
2. Restaurer l’environnement :
   - `renv::restore()` (si nécessaire)
3. Lancer :
   - ouvrir `app.R` puis Run App, ou `shiny::runApp()` depuis la racine.

---

## 8) Mettre à jour les données

Cas standard : nouvelle base au même format.
1. Remplacer `data_raw/BDD_CLEAN.csv` par la nouvelle version (même format).
2. Exécuter le script de build des tables `.rds` (ex : via `0.0_make_clean_tables.R`).
3. Vérifier que `data/*.rds` a bien été mis à jour (taille/date).
4. Relancer l’app et vérifier :
   - sélecteurs de scénarios
   - cohérence des unités
   - graphiques sensibles (sankey, cartes, empilés)

---

## 9) Déploiement shinyapps.io

- Les fichiers nécessaires au déploiement doivent inclure :
  - `app.R`, `global.R`, `R/`, `data/`, `www/`, `renv.lock` (selon configuration)
- `rsconnect/` contient les paramètres de déploiement.

Recommandation :
- Déployer en embarquant uniquement `data/*.rds` (léger) plutôt que `data_raw/*.csv` (lourd), sauf besoin explicite de rebuild côté serveur.

---

## 10) Ajouter un nouvel onglet / un nouveau graphique

Procédure recommandée :
1. Créer un nouveau module `X.Y_mod_<nom>.R` dans `R/` (où `X` = numéro d’onglet).
2. Exposer `mod_<nom>_ui()` et `mod_<nom>_server()`.
3. Sourcer le fichier (dans `global.R` ou dans la logique de chargement des scripts).
4. Ajouter le module à l’UI (dans `app.R`) et le brancher côté server.
5. Si besoin de nouvelles variables :
   - modifier le pipeline (`0.0_make_clean_tables.R`) pour enrichir `data/*.rds`
   - mettre à jour dimensions/labels si nécessaire

---

## 11) Conventions & points de vigilance

- Les scénarios doivent être pilotés par les dimensions (`dim_scenario`, `dim_constraint`, etc.) afin de garantir :
  - ordre d’affichage stable
  - libellés homogènes
  - compatibilité des filtres
- Éviter les “strings hardcodées” dans les modules (préférer dictionnaires/config).
- Les unités doivent être explicites (kcal/hab/j, Mt, %, etc.) et cohérentes entre modules.
- Les assets `www/` doivent être référencés via chemins relatifs Shiny.