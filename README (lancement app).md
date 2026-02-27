# Application Shiny – Prospective agricole en Afrique

Ce dossier contient le code d’une application **Shiny** développée avec **R** et gérée avec **{renv}** (pour figer les versions de packages).

## 1. Pré-requis

- **R** (version ≥ 4.x recommandée)  
- **RStudio** (facultatif mais conseillé)  
- Accès internet pour que `{renv}` puisse télécharger les packages manquants

## 2. Installation du projet

1. **Dézipper** l’archive dans un dossier de travail sur votre ordinateur  
   (par exemple : `C:/Users/.../GlobAfriqueShiny`).

2. Ouvrir le projet dans RStudio :
   - double-cliquer sur le fichier `.Rproj` fourni dans le dossier  
   - ou via **File → Open Project…** et sélectionner ce fichier.

3. Installer `{renv}` si besoin :

   ```r
   install.packages("renv")
   ```

4. Restaurer l’environnement de packages du projet :

   ```r
   renv::restore()
   ```

   Cette commande lit le fichier `renv.lock` et installe les versions de packages utilisées dans le projet.  
   Selon la connexion, cela peut prendre quelques minutes.

## 3. Mettre à jour les données 

Pour mettre à jour les données et recréer les fichiers RDS nécessaire à la diffusion : 

options(AFD_FORCE_REBUILD = TRUE)
shiny::runApp()

Ensuite pour revenir à la normale (pas de rebuild à chaque lancement):

options(AFD_FORCE_REBUILD = NULL)
Sys.unsetenv("AFD_FORCE_REBUILD")

## 4. Vérifier les fichier envoyés à shinyapps.io :

Commande à mettre dans la console :

rsconnect::listDeploymentFiles(".")


## 5. Lancer l’application Shiny

Une fois les packages installés :

- depuis la console R :

  ```r
  shiny::runApp()
  ```

- ou en ouvrant `app.R` dans RStudio et en cliquant sur **Run App**.

L’application devrait s’ouvrir dans une fenêtre ou un onglet de navigateur.

## 4. Données et chemins

- Certains fichiers de données volumineux ou sensibles peuvent avoir été retirés de l’archive.
- Si des erreurs du type *« fichier introuvable »* apparaissent :
  - vérifier les chemins d’accès dans `global.R` et les scripts du dossier `R/`,  
  - adapter les chemins vers vos propres fichiers de données si nécessaire.
  

### Remarque pour les utilisateurs Linux

L’environnement du projet utilise quelques packages graphiques (notamment `ragg`,
`systemfonts` et `textshaping`) qui s’appuient sur des bibliothèques système
externes.  

- Sous **Windows** et **macOS**, ces packages sont en général installés sous forme
  de binaires précompilés et `renv::restore()` fonctionne sans étape
  supplémentaire.
- Sous **Linux**, il peut être nécessaire d’installer au préalable certaines
  bibliothèques de développement, sinon l’installation de `textshaping`,
  `systemfonts` ou `ragg` peut échouer.

Sur une distribution de type Debian/Ubuntu, vous pouvez installer les dépendances
requises avec :

```bash
sudo apt-get update
sudo apt-get install -y \
  libharfbuzz-dev libfribidi-dev libfreetype6-dev libfontconfig1-dev \
  libpng-dev libjpeg-dev libtiff5-dev libcurl4-openssl-dev libssl-dev \
  libicu-dev