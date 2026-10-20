# Environnement de developpement local (Docker)

Fait tourner le front-end Shiny Regard360 sous R 4.2.2, la version
exigee par `renv.lock`, sans rien installer sur le poste Windows.

Cet environnement ne publie pas. La publication vers shinyapps.io
reste manuelle, depuis le poste de la personne qui en a les acces.

## Emplacement

Ce dossier `docker-dev` va a la RACINE du depot, a cote de
`V_collab_info` :

    Regard360_Frontend/
      docker-dev/          <- ici
      V_collab_info/
      renv/

Le compose monte `../V_collab_info`, donc l'emplacement compte.

## Demarrage

    cd docker-dev
    docker compose up --build

La premiere construction prend 5 a 10 minutes.

Ouvrir ensuite http://localhost:8787 dans le navigateur.
RStudio s'ouvre directement, sans mot de passe.

## Dans RStudio

1. File > Open Project, choisir `projet/Beta_053.Rproj`.
   La console doit afficher une ligne `Project loaded. [renv ...]`.

2. Dans la console :

       renv::restore()

   Compter 10 a 20 minutes la premiere fois. Les paquets arrivent en
   binaire precompile, pas en source.

3. Lancer :

       shiny::runApp(host = "0.0.0.0", port = 3838)

   L'application est accessible sur http://localhost:3838

## Verifications si quelque chose cloche

    R.version.string   # doit afficher 4.2.2
    Sys.which("make")  # doit renvoyer un chemin
    .libPaths()        # doit pointer vers /opt/renv/library
    getwd()            # doit se terminer par /projet

## Notes

- Le code est monte depuis `../V_collab_info`. Les modifications faites
  dans RStudio apparaissent cote Windows, et inversement. Git continue
  de s'utiliser depuis Windows.
- La bibliotheque renv vit dans un volume Docker, pas dans le dossier du
  projet, pour ne pas melanger binaires Linux et bibliotheque Windows.
- Pour repartir a neuf : `docker compose down -v`
