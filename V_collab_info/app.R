# https://appsilon.com/renv-how-to-manage-dependencies-in-r/
# Utilisation de snapshot et cocher la case renv à la création du projet.
# renv::snapshot() À faire lors de la création d'un nouveau projet (option 2 et y ensuite). Cela install l'ensemble des packages reliés au code dans le dossier du projet.
#Première élément à faire lors de la création d'un nouveau projet Shiny :
#1 - Cocher la case renv à la création du projet.
#2 - Copier le dossier renv et le renv.lock du projet précédent. Il est aussi opportun de copier les modules, les bd et les shp.
#3 - Dans le app.R créer par le projet, copier coller le code du programme principal du Shiny
#4 - Dans la console, faire renv::init() et sélectionner 1 pour installer les packages nécessaires pour le projet.


library(shiny)
library(shinythemes)
library(rsconnect)
library(tidyverse)
library(DT)
library(htmltools)
library(reactablefmtr)
library(writexl)
library(sf)
library(mapview)
library(leaflet)
library(scales)
library(shinyWidgets)
library(zoo)
library(RColorBrewer)
library(shinyjs)


options(rsconnect.http.verbose = TRUE)
options(shiny.fullstacktrace = TRUE) # Avec la commande rsconnect::showLogs() dans la console après le déploiement, on peut déceler une erreur.


# Charte de couleur LUM file:///R:/1_ECOBES/E_Projets_Gestion/DevCommunautes/ZZ_Regard360/4320_Soumission_LumDesign/Charte-graphique-REGARD360_officielle.pdf
# 6 couleurs Regard 360 29 mai "#FDE725FF" "#7AD151FF" "#22A884FF" "#00A9CCFF""#007A99FF""#0F2B5FFF"
# 6 couleurs de Lum Design - Bleu au jaune : #003A70 #007298 #36B0C9 #00A887 #93D500 #FCDF51

### Téléchargement des fichiers ####

load(file = "grande_bd_no_dim_29juin2026.rdata")
load(file = "grande_bd_no_dim_all_digits_29juin2026.rdata")
load(file = "Metadata_29juin2026.rdata")
load(file = "Metadata_region_1oct2025.rdata")
load(file = "Metadata_acronyme_2avril.rdata")

colnames(grande_bd_no_dim)

test <- grande_bd_no_dim |>
  select(annee,cle_geo_annee,offre_actuelle_sgee)

# Pour un arrondissement différent pour certains indicateurs, ces tables sont créés avec le fichier Table_shiny_v3.R
load(file = "name_round_1.rdata")
load(file = "name_round_2.rdata")
load(file = "name_round_3.rdata")

source("mod_tabind_beta053.R")
source("mod_enquete_beta053.R")
source("mod_par_coll_beta053.R")
source("mod_primsecond_beta053.R")

shp_mrc_simp <- sf::read_sf("shp_mrc_simp.shp")
shp_ra_simp <- sf::read_sf("shp_ra_simp.shp")
#shp_mtl_laval <- sf::read_sf("shp_mtl_laval_simp.shp")
shp_mtl_laval <- sf::read_sf("shp_mtl_laval_aut2025_simp.shp") # Pour Dorval dans le Sud au lieu du Nord
shp_rss <- sf::read_sf("shp_rss_simp.shp")


###  Fonctions pour le Shiny ############################################################

unique_no.NA <- function(x){
  if (!is.vector(x)) stop('x must be a vector')
  u=unique(x)
  u[!is.na(u)]
}

# Cette fonction utilisé dans leaflet format provient de https://github.com/rstudio/leaflet/issues/455
my_labelFormat <- function(...) {
  fun <- labelFormat(...)
  evalq(formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = FALSE,
           big.mark = ' ', decimal.mark = ',')
    #big.mark = big.mark, decimal.mark = ',')
  }, environment(fun))
  return(fun)
}

ld <- scales::label_number(decimal.mark = ",")
lfrench <- scales::label_number(decimal.mark = ",")


### Création de liste ou groupe  #######################################################################


alpha_reg <-c("Ensemble du Québec","Abitibi-Témiscamingue","Bas-Saint-Laurent","Capitale-Nationale","Centre-du-Québec","Chaudière-Appalaches",
              "Côte-Nord","Estrie","Gaspésie–Îles-de-la-Madeleine","Lanaudière","Laurentides","Laval","Mauricie","Montérégie","Montréal",
              "Nord-du-Québec","Outaouais","Saguenay–Lac-Saint-Jean")
alpha_reg <- factor(alpha_reg,levels=alpha_reg)

list_mrc <- c("Abitibi-Témiscamingue","Bas-Saint-Laurent","Capitale-Nationale","Centre-du-Québec","Chaudière-Appalaches",
              "Côte-Nord","Estrie","Gaspésie–Îles-de-la-Madeleine","Lanaudière","Laurentides","Laval","Mauricie","Montérégie","Montréal",
              "Nord-du-Québec","Outaouais","Saguenay–Lac-Saint-Jean")
list_mrc <- factor(list_mrc,levels=list_mrc)

ind_ra_seulement <- c("Moyenne général au secondaire","Taux de réussite au 1e trimestre","Taux de réussite au 3e trimestre","Taux d'obtention du DEC selon la durée prévue")

ind_decoup_mrc <- metadata |>
  #filter(decoup_ind == "mrc"|decoup_ind == "mrc_seul") |>
  filter(decoup_ind == "mrc") |>
  filter(nom_ind_court != "Population totale (nb)")
ind_decoup_mrc <- unique(ind_decoup_mrc$nom_ind_court)

ind_decoup_ra <- metadata |>
  filter(decoup_ind == "ra")
ind_decoup_ra <- unique(ind_decoup_ra$nom_ind_court)

ind_decoup_rss <- metadata |>
  filter(decoup_ind == "rss")
ind_decoup_rss <- unique(ind_decoup_rss$nom_ind_court)

ind_decoup_mrc_seul <- metadata |>
  filter(decoup_ind == "mrc_seul")
ind_decoup_mrc_seul <- unique(ind_decoup_mrc_seul$nom_ind_court)

#### Liste themes par dimensions #############################################

list <- metadata%>%
  filter(dimension == "Population")
list_pop <-  unique(list$theme)

list <- metadata%>%
  filter(dimension == "Conditions de vie")
list_cond_vie <- unique(list$theme)

list <- metadata%>%
  filter(dimension == "Santé et bien-être")
list_sante <- unique(list$theme)

list <- metadata%>%
  filter(dimension == "Parcours éducatifs")
list_education <- unique(list$theme)

nms_bibli = list(
  "Population" = list_pop,
  "Conditions de vie" = list_cond_vie,
  "Santé et bien-être" = list_sante,
  "Parcours éducatifs" = list_education
)

### Obtention des textes de la plateforme #######################################################

meta_acronyme_select <- filter(metadata_acronyme, Acronymes1 == "RA : Région administrative")
texte_desc_biblio <- filter(metadata_region, spec_region_nom == "desc_biblio")
texte_metho_carte <- filter(metadata_region, spec_region_nom == "metho_carte")
texte_metho_graph <- filter(metadata_region, spec_region_nom == "metho_graph")
texte_metho_tableau <- filter(metadata_region, spec_region_nom == "metho_tableau")
citation_biblio <- filter(metadata_region, spec_region_nom == "citation_biblio")


#-------------- UI principal -----------------------####

ui <- fluidPage(theme = shinytheme("paper"),

                tags$head(includeHTML("google_analytics_plateforme.html")),
                includeCSS("www/style_v48.css"),
                useShinyjs(),

                # Application title
                navbarPage(
                  "Regard 360",
                  position = c("fixed-top"),
                  tags$style(type="text/css", "body {padding-top: 70px;}"),

                  tabPanel("Bibliothèque",

                           fluidRow(column(6,offset = 0,textOutput("titre"),
                                           tags$head(tags$style("#titre{color: #0F2B5FFF;font-size: 24px;font-style: bold;}"))
                           )),

                           fluidRow(column(11,offset = 0,htmlOutput("titre_def"))),

                           fluidRow(
                             column(2,offset = 1,selectInput("theme", label = "Thème", choices = nms_bibli)),
                             column(3,selectInput("ind", "Indicateur", choices = NULL,width = "500px")),
                             column(2,selectInput("ra", "Territoire", choices = NULL)),
                             column(1,selectInput("range", "Année", choices = NULL)),
                             column(2,offset = 0,actionButton("hide_acronyme", "Acronymes",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                                                              style="color: #fff; background-color: #00A887;padding:5px; font-size:85%; border-color: #2e6da4;"))
                           ),

                           fluidRow(
                             column(2,offset = 6,align="right",actionButton("hide_spec_region", "Précisions sur le Territoire",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                                                                            style="color: #fff; background-color: #A6DF48;padding:5px; font-size:85%; border-color: #2e6da4"))
                           ),
                           br(),

                           fluidRow(
                             column(4,offset = 3,hidden(htmlOutput("acronyme_1"))),
                             column(4,hidden(htmlOutput("acronyme_2")))
                           ),

                           fluidRow(
                             column(8,offset = 3,hidden(htmlOutput("region_spec2")))
                           ),


                           fluidRow(column(5,textOutput("theme"),
                                           tags$head(tags$style("#theme{color: #0F2B5FFF;font-size: 20px;font-style: bold;}"))),
                                    column(7,textOutput("nom_indicateur"),
                                           tags$head(tags$style("#nom_indicateur{color: #0F2B5FFF;font-size: 20px;font-style: bold;}")))),

                           fluidRow(column(5,htmlOutput("theme_def")),
                                    column(7,htmlOutput("definition"))),


                           useShinyjs(),
                           fluidRow(
                             column(4,offset = 6,actionButton("hide_precisions", "Précisions sur l'indicateur",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                                                              style="color: #fff; background-color: #36B0C9;padding:5px; font-size:85%; border-color: #2e6da4"))
                           ),
                           br(),

                           fluidRow(
                             column(3,offset =0,align="center",hidden(htmlOutput("def_supp_col1"))),
                             column(3,align="center",hidden(htmlOutput("def_supp_col2"))),
                             column(3,align="center",hidden(htmlOutput("def_supp_col3"))),
                             column(3,align="center",hidden(htmlOutput("def_supp_col4"))),
                             column(3,align="center",offset =0, hidden(htmlOutput("def_supp_col5"))),
                             column(3,align="center", hidden(htmlOutput("def_supp_col6"))),
                           ),
                           br(),

                           fluidRow(column(5,align="center",textOutput("graph_caption2"),tags$head(tags$style("#graph_caption2{color: #007A99FF;font-size: 16px;font-style: bold;}"))),
                                    column(5,offset = 1, align="center",textOutput("titre_map2"),tags$head(tags$style("#titre_map2{color: #007A99FF;font-size: 16px;font-style: bold;}")))),
                           fluidRow(column(5,align="center",textOutput("graph_caption3"),tags$head(tags$style("#graph_caption3{color: #007A99FF;font-size: 16px;font-style: bold;}"))),
                                    column(5, offset = 1, align="center",textOutput("titre_map3"),tags$head(tags$style("#titre_map3{color: #007A99FF;font-size: 16px;font-style: bold;}"))),
                                    column(1,offset = 0,align="center",selectInput("classe", "Nombre de classes", choices = c(2:9), selected = 4))),

                           fluidRow(column(6,plotOutput("hist_ind", width = "100%", height = 600)),
                                    #column(6,mapviewOutput("mapplot", width = "100%", height = 600),
                                    column(6,leafletOutput("mapplot", width = "100%", height = 600)
                                    )),
                           br(),

                           fluidRow(column(6, align="center",
                                           actionButton("hide_meta_hist", "Précisions sur le graphique",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                                                        style="color: #fff; background-color: #BDBFBF; border-color: #2e6da4; padding:5px; font-size:85%")),
                                    column(6, align="center",
                                           actionButton("hide_meta_carte", "Précisions sur la carte",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                                                        style="color: #fff; background-color: #BDBFBF; border-color: #2e6da4; padding:5px; font-size:85%"))),
                           br(),

                           fluidRow(
                             column(6,offset = 0,hidden(htmlOutput("meta_hist"))),
                             column(6,hidden(htmlOutput("meta_carte")))
                           ),


                           fluidRow(column(10,offset = 1,align="center",textOutput("table_caption2"),
                                           tags$head(tags$style("#table_caption2{color: #007A99FF;font-size: 16px;font-style: bold;}")))),
                           fluidRow(column(10,offset = 1,align="center",textOutput("table_caption3"),
                                           tags$head(tags$style("#table_caption3{color: #007A99FF;font-size: 16px;font-style: bold;}")))
                                    # column(5,textOutput("precision_ind"),
                                    #        tags$head(tags$style("#precision_ind{color: #00A9CCFF;font-size: 17px;font-style: bold;}")))
                           ),
                           br(),

                           fluidRow(#column(7,tableOutput("table_ra")),
                             #column(7,reactableOutput("table_ra")),
                             column(10,offset = 1,DT::dataTableOutput("table_ra")),
                             #column(5,htmlOutput("def_supp"))
                           ),
                           br(),
                           fluidRow(column(3,offset = 3, align="left",
                                           actionButton("hide_meta_tableau", "Précisions sur le tableau",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                                                        style="color: #fff;background-color: #BDBFBF;padding:5px; font-size:85%")),
                                    column(3,align="right",downloadButton("download", "Téléchargement du tableau",style="color: #fff; background-color: #3C65CF"))),
                           br(),

                           fluidRow(
                             column(6,offset = 1,hidden(htmlOutput("meta_tableau"))),
                           ),

                           #fluidRow(column(10,offset = 1,htmlOutput("citer_site"))),

                           fluidRow(column(3,offset = 1,
                                           actionButton("hide_meta_citation", "Référence suggérée",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                                                        style="color: #fff; background-color: #36B0C9; border-color: #2e6da4;"))),br(),
                           fluidRow(column(10,offset = 1,hidden(htmlOutput("meta_citation")))),

                           br(),br(),

                           fluidRow(
                             column(11,offset = 1, tags$span("Pour des questions ou commentaires au sujet de la plateforme, veuillez communiquer avec nous par ",
                                                             tags$a(href = "mailto:info@regard360.ca?subject=Question sur la plateforme Regard 360", "courriel",
                                                                    style = "font-size: 16px;text-decoration: none;"),

                                                             style = "font-size: 16px; text-decoration: none;"))),


                           br(),br(),br(),br(),br(),br(),



                  ),

                  #----------- UI des onglets ----------------------------------------####

                  tabPanel(HTML("Tableau personnalisé"),
                           fluidPage(
                             UI_titre_tabind("tab_test"),UI_main_tabind("tab_test"),UI_acronyme_tabind("tab_test"),br(),tabindUI("tab_test"),
                             tabindUI_2("tab_test"),br(),tabindUI_3("tab_test"),br(),br(),
                             UI_hide_meta_tableau("tab_test"),UI_action_citation("tab_test"),br(),
                             UI_citation("tab_test"),br(),br(),br(),br(),br()
                           ),
                  ),
                  tabPanel("Parcours primaire-secondaire",
                           uiModule_parco("pars_second_prim")
                  ),
                  tabPanel("Parcours collégial",

                           fluidPage(
                             UI_titre_postsecond("tab_coll"),
                             UI_titre_def_postsecond("tab_coll"),br(),
                             UI_bouton_territoire("tab_coll"),
                             UI_1_postsecond("tab_coll"),
                             UI_titredef_ind_postsecond("tab_coll"),
                             UI_def_ind_postsecond("tab_coll"),
                             UI_titre_postsecond_section1("tab_coll"),
                             UI_1_1_postsecond("tab_coll"),
                             UI_2b_postsecond("tab_coll"),
                             UI_2c_postsecond("tab_coll"),
                             UI_3_postsecond("tab_coll"),br(),
                             UI_3_coll_action_carte("tab_coll"),br(),
                             UI_3_coll_action_carte_hidden("tab_coll"),
                             UI_4ta_postsecond("tab_coll"),
                             UI_4tb_postsecond("tab_coll"),
                             UI_4_postsecond("tab_coll"),
                             UI_4_coll_action_graph("tab_coll"),br(),
                             UI_4_coll_action_graph_hidden("tab_coll"),
                             UI_5t_postsecond("tab_coll"),
                             UI_5desc_postsecond("tab_coll"),br(),
                             UI_5pick_postsecond("tab_coll"),br(),
                             UI_6pick_postsecond("tab_coll"),
                             UI_7t_postsecond("tab_coll"),
                             UI_7desc_postsecond("tab_coll"),
                             UI_7a_postsecond("tab_coll"),br(),
                             Ui_7_table_coll_titre("tab_coll"),br(),
                             UI_7_postsecond("tab_coll"),br(),
                             UI_7d_postsecond("tab_coll"),br(),
                             UI_7e_hide_meta_tableau("tab_coll"),br(),br(),
                             Ui_titre_cohorte("tab_coll"),
                             Ui_description_cohorte("tab_coll"),
                             Ui_menu_cohorte("tab_coll"),
                             Ui_graphique_cohorte("tab_coll"),br(),
                             #Ui_graphique2_cohorte("tab_coll"),
                             Ui_table_cohorte_titre("tab_coll"),br(),
                             Ui_table_cohorte("tab_coll"),br(),
                             UI_down_table_cohorte("tab_coll"),br(),br(),
                             UI_down_hide_meta_tableau("tab_coll"),br(),br(),
                             UI_action_citation("tab_coll"),br(),
                             UI_citation("tab_coll"),br(),br(),br(),br(),br(),br(),br()

                           ),
                  ),
                  tabPanel("Enquêtes",

                           fluidPage(
                             UI_titre_eqdem("eqdem"),
                             UI_titre_eqdem_def("eqdem"),br(),
                             eqdem_3_choices("eqdem"),
                             #enquete_titre_enquete("eqdem"),
                             bouton_enquete("eqdem"),br(),
                             enquete_def_enquete("eqdem"),
                             eqdem_nom_ind_enquete("eqdem"),
                             eqdem_def_ind_enquete("eqdem"),br(),
                             eqdem_graph("eqdem"),
                             br(),
                             eqdem_download("eqdem"),
                             eqdem_colonnes_def("eqdem"),
                             br(),br(),
                             UI_action_citation("eqdem"),br(),
                             UI_citation("eqdem"),br(),br(),br(),br(),br(),br(),br()
                           ),
                  ),
                  # tabPanel("test",
                  #          uiModule("test")
                  # ),

                )

)

#-------------------- Serveur ------------------- ####

server <- function(input, output, session) {

  ### Les observe event ####

  # Menu "indicateur" dépendant du thème

  list_ind_theme <-reactive({
    filter(metadata, theme == input$theme & onglet_biblio == "oui")
  })

  meta_region_select <-reactive({
    filter(metadata_region, spec_region_nom == input$ra)
  })

  observeEvent(list_ind_theme(),{
    choices <- unique(list_ind_theme()$nom_ind_court)
    freezeReactiveValue(input, "ind")
    updateSelectInput(inputId = "ind", choices = choices)
  })

  observeEvent(input$hide_acronyme, {
    toggle("acronyme_1")
    toggle("acronyme_2")
  })

  observeEvent(input$hide_precisions, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("def_supp_col1")
    toggle("def_supp_col2")
    toggle("def_supp_col3")
    toggle("def_supp_col4")
    toggle("def_supp_col5")
    toggle("def_supp_col6")
  })

  observeEvent(input$hide_spec_region, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("region_spec2")
  })

  observeEvent(input$hide_meta_hist, {toggle("meta_hist")})

  observeEvent(input$hide_meta_carte, {toggle("meta_carte")})

  observeEvent(input$hide_meta_tableau, {toggle("meta_tableau")})

  observeEvent(input$hide_meta_citation, {toggle("meta_citation")})

  # Menu de la région selon l'indicateur

  observeEvent(input$ind,{
    current_value <- input$ra
    if(input$ind %in% ind_decoup_ra){choices <- "Ensemble du Québec"}
    else if(input$ind %in% ind_decoup_rss){choices <- "Ensemble du Québec (RSS)"}
    else if(input$ind %in% ind_decoup_mrc_seul){choices <- list_mrc}
    else{choices <- alpha_reg}

    freezeReactiveValue(input, "ra")

    if(current_value %in% list_mrc && !(input$ind %in% ind_decoup_mrc_seul)) {
      updateSelectInput(inputId = "ra",choices = choices)
    }else if(input$ind == "Population totale (nb)"){
      updateSelectInput(inputId = "ra", choices = choices)
    }else if(input$ind %in% ind_decoup_mrc & current_value %in% c("Ensemble du Québec","Ensemble du Québec (RSS)")){
      updateSelectInput(inputId = "ra", choices = choices)
    }else if(input$ind %in% ind_decoup_mrc){
      updateSelectInput(inputId = "ra", selected = current_value)
    }else if(input$ind %in% ind_decoup_mrc_seul & current_value %in% c("Ensemble du Québec","Ensemble du Québec (RSS)")){
      updateSelectInput(inputId = "ra", choices = choices)
    }else if(input$ind %in% ind_decoup_mrc_seul){
      updateSelectInput(inputId = "ra", selected = current_value)
    }else if(input$ind %in% ind_decoup_ra){updateSelectInput(inputId = "ra", choices = choices)}
    else if(input$ind %in% ind_decoup_rss){updateSelectInput(inputId = "ra", choices = choices)}

  })

  meta_ind <- reactive({
    req(input$ind)
    list_ind_theme() %>%
      filter(nom_ind_court == input$ind)
  })

  annees_ind <- reactive({
    req(input$ind)
    c(meta_ind()$an1,meta_ind()$an2,meta_ind()$an3,meta_ind()$an4,meta_ind()$an5,meta_ind()$an6,meta_ind()$an7,meta_ind()$an8,meta_ind()$an9,meta_ind()$an10,meta_ind()$an11)
  })

  # annees_range <- reactive({
  #   req(input$ind)
  #   if(meta_ind()$type_annee == "plage"){c(meta_ind()$an_max:meta_ind()$an_min)}
  #   else{c(meta_ind()$an_max,meta_ind()$an_min)}
  # })

  annees_range <- reactive({
    req(input$ind)
    if (meta_ind()$type_annee == "plage") {meta_ind()$an_max:meta_ind()$an_min}
    else if (meta_ind()$type_annee == "plage_seq") {seq(meta_ind()$an_max, meta_ind()$an_min, by = -2)}
    else {c(meta_ind()$an_max, meta_ind()$an_min)}
  })

  observeEvent(annees_range(),{
    choices <- annees_range()
    freezeReactiveValue(input, "range")
    updateSelectInput(inputId = "range", choices = choices)
  })


  # Le filtre était pour ne pas présenter la MRC et la RA quand on présente les quartiers/secteurs de Montréal et Laval.
  # Pour Montréal, en raison d'une géographie non constante à travers le temps pour SSD, on garde la MRC pour ces indicateurs (Montréal et Laval).
  grande_bd_no_dim_1 <- reactive({
    if(meta_ind()$div_mtl_laval == "oui" & !(meta_ind()$indicateurs %in% c("nb_sortants","nb_ssdipl","tx_ssdipl"))){
      grande_bd_no_dim |>
        filter(code_geo != 2406066) |>
        filter(code_geo != 2413065)
    }
    else{grande_bd_no_dim}
  })

  ### Les textes et premier sélecteurs #####

  output$titre<-renderText("Bibliothèque")

  output$titre_def <- renderUI({HTML(texte_desc_biblio$spec_region)})

  output$acronyme_1 <- renderUI({HTML(meta_acronyme_select$meta_acronyme_1)})
  output$acronyme_2 <- renderUI({HTML(meta_acronyme_select$meta_acronyme_2)})

  output$region_spec2 <- renderUI({HTML(meta_region_select()$spec_region)})

  output$theme <- renderText(paste("Thème : ",meta_ind()$theme))
  output$theme_def <- renderUI({HTML(meta_ind()$theme_definition)})

  output$nom_indicateur <- renderText(paste("Indicateur : ", meta_ind()$nom_ind_complet_unite))
  output$definition <- renderUI({HTML(meta_ind()$def_ind)})

  output$def_supp_col1 <- renderUI({HTML(meta_ind()$precision_1)})
  output$def_supp_col2 <- renderUI({HTML(meta_ind()$precision_2)})
  output$def_supp_col3 <- renderUI({HTML(meta_ind()$precision_5)})
  output$def_supp_col4 <- renderUI({HTML(meta_ind()$precision_6)})
  output$def_supp_col5 <- renderUI({HTML(meta_ind()$precision_4)})
  output$def_supp_col6 <- renderUI({HTML(meta_ind()$precision_3)})

  #---  diagramme à bandes ####

  bd_graph <- reactive({
    req(input$ind)
    if(input$ra == "Ensemble du Québec" & meta_ind()$unite == "(nb)"){
      grande_bd_no_dim_1()  %>%
        #filter(annee %in% meta_ind()$annee_recente)%>%
        filter(annee %in% input$range)%>%
        filter(decoupage == "ra")%>%
        select(annee,code_geo,nom,meta_ind()$indicateurs,color_ecobes)%>%
        arrange(meta_ind()$indicateurs)
    }else if(input$ra == "Ensemble du Québec" & meta_ind()$unite == "($)" & input$theme == "Culture"){
      grande_bd_no_dim_1() %>%
        #filter(annee %in% meta_ind()$annee_recente)%>%
        filter(annee %in% input$range)%>%
        filter(decoupage == "ra")%>%
        select(annee,code_geo,nom,meta_ind()$indicateurs,color_ecobes)%>%
        arrange(meta_ind()$indicateurs)
    }else if(input$ra == "Ensemble du Québec"){
      grande_bd_no_dim_1() %>%
        #filter(annee %in% meta_ind()$annee_recente)%>%
        filter(annee %in% input$range)%>%
        filter(decoupage == "ra" | nom == "Ensemble du Québec")%>%
        select(annee,code_geo,nom,meta_ind()$indicateurs,color_ecobes)%>%
        arrange(meta_ind()$indicateurs)
    }else if(input$ra == "Ensemble du Québec (RSS)"){
      grande_bd_no_dim_1()  %>%
        #filter(annee %in% meta_ind()$annee_recente)%>%
        filter(annee %in% input$range)%>%
        filter(decoupage == "rss" | nom == "Ensemble du Québec (RSS)")%>%
        select(annee,code_geo,nom,meta_ind()$indicateurs,color_ecobes)%>%
        arrange(meta_ind()$indicateurs)
    }else if(input$ra %in% list_mrc & meta_ind()$unite == "(nb)"){
      grande_bd_no_dim_1()  %>%
        #filter(annee %in% meta_ind()$annee_recente)%>%
        filter(annee %in% input$range)%>%
        filter(ra_app_nom == input$ra)%>%
        select(annee,code_geo,nom,meta_ind()$indicateurs,color_ecobes)%>%
        arrange(meta_ind()$indicateurs)
    }else{
      grande_bd_no_dim_1()  %>%
        #filter(annee %in% meta_ind()$annee_recente)%>%
        filter(annee %in% input$range)%>%
        filter(ra_app_nom == input$ra | nom == "Ensemble du Québec")%>%
        select(annee,code_geo,nom,meta_ind()$indicateurs,color_ecobes)
    }
  })

  output$graph_caption<-renderText(paste("Réprésentation graphique"))
  output$graph_caption2<-renderText(paste("Territoire :",req(input$ra)))
  output$graph_caption3<-renderText(paste("Indicateur :", meta_ind()$nom_ind_complet_unite," - ", input$range))



  bd_graph_no_na <- reactive({bd_graph() |>
      drop_na()})

  output$hist_ind <- renderPlot({
    ggplot(data = bd_graph_no_na(),mapping = aes(x=.data[[meta_ind()$indicateurs]], y= reorder(nom,.data[[meta_ind()$indicateurs]]))) +
      geom_bar(aes(fill = color_ecobes), stat = "identity")+
      scale_fill_identity(guide = "none")+
      geom_text(aes(label = format(.data[[meta_ind()$indicateurs]],accuracy=0.1, big.mark = " ", decimal.mark = ",")),hjust = -0.25, size=5.0)+
      scale_x_continuous(expand = expansion(mult= c(0,0.20)))+
      theme_minimal()+
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )+
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank())+
      theme(legend.position="none")+
      theme(axis.text.y = element_text(size = 14.0, hjust = 1,vjust = 0.5))+
      labs(
        title = ,
        x = "",
        y = "",
        subtitle = "",
        caption = ""
      )
  })

  output$meta_hist <- renderUI({HTML(texte_metho_graph$spec_region)})

  #------  Tableau de données ####

  bd_table_3 <- reactive({
    req(input$ind)
    if(input$ra == "Ensemble du Québec" & input$theme == "Culture"){
      grande_bd_no_dim  %>%
        filter(annee %in% annees_ind())%>%
        filter(decoupage == "ra" | nom == "Ensemble du Québec")%>%
        select(annee,nom,meta_ind()$indicateurs)%>%
        pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs)
    }else if(input$ra == "Ensemble du Québec" & input$theme != "Culture"){
      grande_bd_no_dim  %>%
        filter(annee %in% annees_ind())%>%
        filter(decoupage == "ra" | nom == "Ensemble du Québec")%>%
        filter(!(nom %in% c("Estrie (ancienne)", "Montérégie (ancienne)"))) |>
        select(annee,nom,meta_ind()$indicateurs)%>%
        pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs)
    }else if(input$ra == "Ensemble du Québec (RSS)"){
      grande_bd_no_dim  %>%
        filter(annee %in% annees_ind())%>%
        filter(decoupage == "rss"| nom == "Ensemble du Québec (RSS)")%>%
        select(annee,nom,meta_ind()$indicateurs)%>%
        pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs)
    }else if (input$theme != "Culture"){
      grande_bd_no_dim  %>%
        filter(annee %in% annees_ind())%>%
        filter(decoupage == "mrc"| decoupage == "ra" | nom == "Ensemble du Québec")%>%
        filter(ra_app_nom == input$ra | nom == "Ensemble du Québec")%>%
        filter(!(nom %in% c("Estrie (ancienne)", "Montérégie (ancienne)"))) |>
        select(annee,nom,meta_ind()$indicateurs)%>%
        pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs)
    }else{
      grande_bd_no_dim  %>%
        filter(annee %in% annees_ind())%>%
        filter(decoupage == "mrc"| decoupage == "ra" | nom == "Ensemble du Québec")%>%
        filter(ra_app_nom == input$ra | nom == "Ensemble du Québec")%>%
        select(annee,nom,meta_ind()$indicateurs)%>%
        pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs)
    }

  })


  output$table_caption<-renderText(paste("Tableau de données"))
  output$table_caption2<-renderText(paste("Territoire :",req(input$ra)))
  output$table_caption3<-renderText(paste("Indicateur :", meta_ind()$nom_ind_complet_unite," - (années disponibles)"))

  output$precision_ind<-renderText(paste("Précisions sur l'indicateur"))

  #https://rstudio.github.io/DT/
  output$table_ra <- DT::renderDataTable({
    if(meta_ind()$indicateurs %in% name_round_3){
      dtable <- datatable(bd_table_3(),
                          options = list(pageLength = 20,dom = 'tip', scrollX = TRUE,fixedColumns = list(leftColumns = 2),
                                         language = list(
                                           info = "Observations _START_ à _END_ sur un total de _TOTAL_ entrées",
                                           paginate = list(previous = 'Précédent', `next` = 'Suivant')
                                         ),
                                         columnDefs = list(list(visible = FALSE, targets = 0))),# Pour enlever le première colonne "numéro de la ligne"
                          colnames = c('Territoire' = 'nom'),
                          extensions = "FixedColumns")|>
        formatRound(
          columns=c(2:ncol(bd_table_3())),
          digits = 3,interval = 3, mark = " ", dec.mark = ","
        ) |>
        formatStyle(columns = c(1), width='200px')
      return(dtable)}
    if(meta_ind()$indicateurs %in% name_round_2){
      dtable <- datatable(bd_table_3(),
                          options = list(pageLength = 20,dom = 'tip', scrollX = TRUE,fixedColumns = list(leftColumns = 2),
                                         language = list(
                                           info = "Observations _START_ à _END_ sur un total de _TOTAL_ entrées",
                                           paginate = list(previous = 'Précédent', `next` = 'Suivant')
                                         ),
                                         columnDefs = list(list(visible = FALSE, targets = 0))), # Pour enlever le première colonne "numéro de la ligne"
                          colnames = c('Territoire' = 'nom'),
                          extensions = "FixedColumns")|>
        formatRound(
          columns=c(2:ncol(bd_table_3())),
          digits = 2,interval = 3, mark = " ", dec.mark = ","
        ) |>
        formatStyle(columns = c(1), width='200px')
      return(dtable)}
    else{
      dtable <- datatable(bd_table_3(),
                          options = list(pageLength = 20,dom = 'tip', scrollX = TRUE,fixedColumns = list(leftColumns = 2),
                                         language = list(
                                           info = "Observations _START_ à _END_ sur un total de _TOTAL_ entrées",
                                           paginate = list(previous = 'Précédent', `next` = 'Suivant')
                                         ),
                                         columnDefs = list(list(visible = FALSE, targets = 0))),
                          colnames = c('Territoire' = 'nom'),
                          extensions = "FixedColumns")|>
        formatRound(
          columns=c(2:ncol(bd_table_3())),
          digits = 1,interval = 3, mark = " ", dec.mark = ","
        ) |>
        formatStyle(columns = c(1), width='200px')
      return(dtable)
    }
  })

  bd_table_down <- reactive({
    req(input$ind)
    if(input$ra == "Ensemble du Québec"){
      grande_bd_no_dim_all_digits  %>%
        filter(annee %in% annees_ind())%>%
        filter(decoupage == "ra" | nom == "Ensemble du Québec")%>%
        select(annee,nom,meta_ind()$indicateurs)%>%
        pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs) |>
        rename(`Territoire` = `nom`)
    }else if(input$ra == "Ensemble du Québec (RSS)"){
      grande_bd_no_dim_all_digits  %>%
        filter(annee %in% annees_ind())%>%
        filter(decoupage == "rss"| nom == "Ensemble du Québec (RSS)")%>%
        select(annee,nom,meta_ind()$indicateurs)%>%
        pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs) |>
        rename(`Territoire` = `nom`)
    }else{
      grande_bd_no_dim_all_digits  %>%
        filter(annee %in% annees_ind())%>%
        filter(decoupage == "mrc"| decoupage == "ra" | nom == "Ensemble du Québec")%>%
        filter(ra_app_nom == input$ra | nom == "Ensemble du Québec")%>%
        select(annee,nom,meta_ind()$indicateurs)%>%
        pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs) |>
        rename(`Territoire` = `nom`)
    }

  })


  # source_name <- reactive({
  #   req(input$ind)
  #   source <- meta_ind() |>
  #     select(nom_ind_complet,source_longue) |>
  #     rename(`Indicateurs` = `nom_ind_complet`,`Source` = `source_longue`)
  # })

  info_table <- reactive({
    data.frame(Informations = c(paste0("Indicateur : ",meta_ind()$nom_ind_complet_unite),paste0("Thème : ",input$theme),paste0("Producteur de données : ", meta_ind()$producteur),paste0("Date du téléchargement : ", Sys.Date())))
  })

  table_donnee <- reactive({
    req(input$ind)
    tb_donnee <- bind_rows(info_table(),bd_table_down())
  })

  nom_ficher <- reactive({
    gsub(" ", "_", meta_ind()$nom_court_1)
  })

  output$download <- downloadHandler(
    filename = function() {
      #paste0("table_",meta_ind()$nom_ind_complet,"_", Sys.Date(), ".xlsx")
      paste0("table_",nom_ficher(),"_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(table_donnee(), file)
    }
  )

  output$meta_tableau <- renderUI({HTML(texte_metho_tableau$spec_region)})

  output$meta_citation <- renderUI({HTML(citation_biblio$spec_region)})

  #--- Carte ####

  output$titre_map<-renderText("Représentation Cartographique")
  output$titre_map2<-renderText(paste("Territoire :",req(input$ra)))
  output$titre_map3<-renderText(paste("Indicateur :", meta_ind()$nom_ind_complet_unite," - ", input$range ))

  bd_map <- reactive({
    req(input$ind)
    if(input$ra == "Ensemble du Québec"){
      shp_ra_simp%>%
        left_join(bd_graph(),by = c("code_geo" = "code_geo"))%>%
        filter(!is.na(nom))
    }
    else if(input$ra == "Ensemble du Québec (RSS)"){
      shp_rss%>%
        left_join(bd_graph(),by = c("code_geo" = "code_geo"))%>%
        filter(!is.na(nom))
    }
    else if(input$range < 2021 & input$ra %in%c("Montréal") & input$ind %in% c("Sortants du secondaire (nb)","Sorties sans diplôme ni qualification (nb)","Sorties sans diplôme ni qualification (%)")){
      shp_mrc_simp |>
        left_join(bd_graph(),by = c("code_geo" = "code_geo")) |>
        filter(!is.na(nom))
    }
    else if(meta_ind()$div_mtl_laval == "oui" & input$ra %in%c("Montréal","Laval")){
      shp_mtl_laval%>%
        left_join(bd_graph(),by = c("code_geo" = "code_geo"))%>%
        filter(!is.na(nom))
    }else{
      shp_mrc_simp%>%
        left_join(bd_graph(),by = c("code_geo" = "code_geo"))%>%
        filter(!is.na(nom))
    }

  })

  #binpal <- reactive({colorBin("YlGn", bd_map()[[meta_ind()$indicateurs]], as.numeric(input$classe), pretty = FALSE)})

  binpal <- reactive({
    if(all(is.na(bd_map()[[meta_ind()$indicateurs]]))){colorBin("YlGn", c(0:2), 2, pretty = FALSE)}
    else if(sum((bd_map()[[meta_ind()$indicateurs]]),na.rm=TRUE) == 0){colorBin("YlGn", c(0:2), 2, pretty = FALSE)}
    else if(sum(complete.cases(bd_map()[[meta_ind()$indicateurs]])) == 1){colorBin("YlGn", c(0:500), 2, pretty = FALSE)}
    else{colorBin("YlGn", bd_map()[[meta_ind()$indicateurs]], as.numeric(input$classe), pretty = FALSE)}
  })

  # Version avant le 15 décembre
  output$mapplot <- renderLeaflet({
    req(input$ind)
    # Dans le cas de Montréal pour certains indicateurs, par exemple, il y a seulement 1 données, donc une couleur. Cas particulier avec le "else".
    if(nrow(bd_map())>2){
      m <- leaflet(bd_map(), options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1, wheelPxPerZoomLevel = 120)) |>
        #addProviderTiles("CartoDB.Positron") |> # Ancien fond qui demande maintenant une clé API
        addProviderTiles("Esri.WorldGrayCanvas") |>
        addProviderTiles("CartoDB.VoyagerOnlyLabels") |>
        addPolygons(fillColor = ~binpal()(bd_map()[[meta_ind()$indicateurs]]),weight = 1,color = "#444444",fillOpacity = 0.7,
                    popup=paste0(bd_map()$nom," : ",lfrench(bd_map()[[meta_ind()$indicateurs]])),
                    label=~paste0(bd_map()$nom," : ",lfrench(bd_map()[[meta_ind()$indicateurs]]))
        ) |>
        addLegend("bottomright",pal = binpal(), values = ~bd_map()[[meta_ind()$indicateurs]],
                  title = " ",
                  labFormat = my_labelFormat(digits = 1),
                  opacity = 1
        )
    }
    else{
      m <- leaflet(bd_map()) |>
        #addProviderTiles("CartoDB.Positron") |>
        addProviderTiles("Esri.WorldGrayCanvas") |>
        addProviderTiles("CartoDB.VoyagerOnlyLabels") |>
        addPolygons(fillColor = "green",weight = 1,color = "#444444",fillOpacity = 0.4,
                    popup=paste0(bd_map()$nom," - ",lfrench(bd_map()[[meta_ind()$indicateurs]])),
                    label=~paste0(bd_map()$nom," - ",lfrench(bd_map()[[meta_ind()$indicateurs]]))
        )
    }
  })

  output$meta_carte <- renderUI({HTML(texte_metho_carte$spec_region)})


  tabindServer("tab_test")

  #tabbordServer_2("tab_bord2")

  #server_parsco("tab_parsco")

  server_coll("tab_coll")

  eqdem_server("eqdem")

  #test_server("test")

  test_parco_server("pars_second_prim")

}
shinyApp(ui = ui, server = server)


#---------------------------  EXEMPLES ------------------------------
