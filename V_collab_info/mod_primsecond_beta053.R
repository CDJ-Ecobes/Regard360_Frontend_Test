
load(file = "grande_bd_parsco_29juin2026.rdata")
load(file = "bd_parc_sec_coh_hist_2026-03-12.rdata")
load(file = "Metadata_29juin2026.rdata")
load(file = "Metadata_region_1oct2025.rdata")

#pars_second_cohorte <- bd_parcours_sec_cohorte_3
pars_second_cohorte <- bd_parc_sec_coh_historique_f |>
  filter(!(nom %in% c("Estrie (ancienne)", "Montérégie (ancienne)","Ensemble du Québec (RSS)")))


# unique(bd_parc_sec_coh_historique_f$nom)
# test <-bd_parc_sec_coh_historique_f |>
#   #filter(cohorte %in% c(2006:2017)) |>
#   #filter(nom == "MRC d'Acton",sexe == "Sexes réunis", reseau == "Public")
#   filter(nom == "Ensemble du Québec",sexe == "Sexes réunis", reseau == "Public")


bd_pars_sco <- grande_bd_parsco %>%
  mutate(annee_num = as.numeric(annee))%>%
  mutate(
    color_ecobes = case_when(
      decoupage == "ra" ~ "#6DA3ED",  #"palegreen4",
      decoupage == "prov" ~ "#4774BA", #"steelblue3",
      TRUE ~ "#99DBE5",#"palegreen3"
    ))%>%
  mutate(
    line_comp = case_when(
      decoupage == "ra" ~ "solid",
      decoupage == "prov" ~ "dotdash",
      TRUE ~ "solid"
    )) |>
  mutate_if(is.numeric,
            round,
            digits = 1) |>
  mutate(code_geo = as.numeric(code_geo)) |>
  filter(!(nom %in% c("Estrie (ancienne)", "Montérégie (ancienne)","Ensemble du Québec (RSS)")))

ind_decoup_ra <- metadata |>
  filter(decoup_ind == "ra")
ind_decoup_ra <- unique(ind_decoup_ra$nom_ind_court)

ind_decoup_mrc <- metadata |>
  filter(decoup_ind == "mrc") |>
  filter(nom_ind_court != "Nombre d’élèves (nb)")
ind_decoup_mrc <- unique(ind_decoup_mrc$nom_ind_court)

list_ra_prim <- c("Abitibi-Témiscamingue","Bas-Saint-Laurent","Capitale-Nationale","Centre-du-Québec","Chaudière-Appalaches",
              "Côte-Nord","Estrie","Gaspésie–Îles-de-la-Madeleine","Lanaudière","Laurentides","Laval","Mauricie","Montérégie","Montréal",
              "Nord-du-Québec","Outaouais","Saguenay–Lac-Saint-Jean")
list_ra_prim <- factor(list_ra_prim,levels=list_ra_prim)

### Les indicateurs de sous-population sont seulement réseau public et leur caractéristique dans metadata est "oui2"
ind_reseau_public <- metadata |>
  filter(onglet_sco == "oui_public_seulement")
ind_reseau_public <- unique(ind_reseau_public$nom_ind_court)

fact_reseau <-c("Tous réseaux confondus","Public","Privé")
fact_reseau <- factor(fact_reseau,levels=fact_reseau)


#########################################

shp_mrc_simp <- sf::read_sf("shp_mrc_simp.shp")
shp_ra_simp <- sf::read_sf("shp_ra_simp.shp")
#shp_mtl_laval <- sf::read_sf("shp_mtl_laval_simp.shp")
shp_mtl_laval <- sf::read_sf("shp_mtl_laval_aut2025_simp.shp")
#shp_rss <- sf::read_sf("shp_rss_simp.shp")

##########################################

## Liste de d'indicateur selon la dimension

list <- metadata%>%
  filter(theme_onglet_sco == "primaire")
list_primaire <- unique(list$nom_ind_court)

list <- metadata%>%
  filter(theme_onglet_sco == "cohorte")
list_second_parcours <-  unique(list$nom_ind_court)

list <- metadata%>%
  filter(theme_onglet_sco == "transversale")
list_second_transversale <-  unique(list$nom_ind_court)

list <- metadata |>
  filter(theme_onglet_sco == "sortie")
list_second_dip_sous_pop <- unique(list$nom_ind_court)

nms_ind_parcours = list(
  "Primaire - Données transversales" = list_primaire,
  "Secondaire - Données transversales" = list_second_transversale,
  "Secondaire - Suivi de cohortes" = list_second_parcours,
  "Secondaire - Sorties annuelles" = list_second_dip_sous_pop
)

texte_desc_prim_second <- filter(metadata_region, spec_region_nom == "desc_prim_second")
texte_precision_cohorte <- filter(metadata_region, spec_region_nom == "precision_cohorte")
texte_precision_territoire <- filter(metadata_region, spec_region_nom == "precision_territoire")
texte_precision_reseau <- filter(metadata_region, spec_region_nom == "precision_reseau")
texte_evo_temp_prim_second <- filter(metadata_region, spec_region_nom == "evo_temp_prim_second")
texte_desc_ana_cohorte_prim_second <- filter(metadata_region, spec_region_nom == "desc_ana_cohorte_prim_second")
texte_metho_carte <- filter(metadata_region, spec_region_nom == "metho_carte")
texte_metho_graph <- filter(metadata_region, spec_region_nom == "metho_graph")
texte_metho_tableau <- filter(metadata_region, spec_region_nom == "metho_tableau")
citation_prim_second <- filter(metadata_region, spec_region_nom == "citation_prim_second")
texte_desc_tableau_prim_second <- filter(metadata_region, spec_region_nom == "desc_tableau_prim_second")

#########################################################


unique_no.NA <- function(x){
  if (!is.vector(x)) stop('x must be a vector')
  u=unique(x)
  u[!is.na(u)]
}

# Cette fonction utilisée dans leaflet format provient de https://github.com/rstudio/leaflet/issues/455
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

ind_parco <- metadata |> filter(onglet_sco == "oui")

#################################################################################################################################################
# Module UI
#################################################################################################################################################

uiModule_parco <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(column(6,style = "margin-top: 20px",textOutput(NS(id,"titre")),
                    tags$head(tags$style(paste0("#",NS(id,"titre"),"{color: #0F2B5FFF;font-size: 24px;font-style: bold;}"))))),

    fluidRow(column(11,htmlOutput(NS(id,"titre_def")))),

    fluidRow(column(3,offset=1, align="center",
               actionButton(NS(id,"hide_precision_cohorte"), "Précisions cohorte",icon("pen-nib"),
                            style="color: #fff; background-color: #36B0C9; border-color: #2e6da4;padding:5px;font-size:85%")),
             column(3,offset=0, align="center",
                    actionButton(NS(id,"hide_precision_territoire"), "Précisions territoire",icon("pen-nib"),
                                 style="color: #fff; background-color: #00A887; border-color: #2e6da4; padding:5px; font-size:85%")),
             column(3,offset=0, align="center",
                    actionButton(NS(id,"hide_precision_reseau"), "Précisions réseau",icon("pen-nib"),
                                 style="color: #fff; background-color: #00A887; border-color: #2e6da4; padding:5px; font-size:85%"))),
    fluidRow(br()),
    fluidRow(#column(4,offset = 0,hidden(htmlOutput(NS(id,"precision_cohorte")))),
             column(5,offset = 0,hidden(htmlOutput(NS(id,"precision_territoire")))),
             column(7,offset = 0,hidden(htmlOutput(NS(id,"precision_reseau"))))
             ),



### Premier bloc menu déroulant ##########

    fluidRow(
      # column(2,offset = 2,selectInput(NS(id,"ra"), "Région", choices = NULL)),
      # column(3,selectInput(NS(id,"ind"), label = "Indicateurs", choices = nms_ind_parcours,width="550px")),
      column(3,offset = 2,selectInput(NS(id,"ind"), label = "Indicateur", choices = nms_ind_parcours,width="550px")),
      column(2,selectInput(NS(id,"ra"), "Territoire", choices = NULL)),
      column(2,selectInput(NS(id,"reseau"), "Réseau", choices = NULL))),

    fluidRow(column(10,offset = 1,hidden(htmlOutput(NS(id,"precision_cohorte"))))),

    fluidRow(column(10,offset =1,textOutput(NS(id,"nom_indicateur")),
                tags$head(tags$style(paste0("#",NS(id,"nom_indicateur"),"{color: #0F2B5FFF;font-size: 22px;font-style: bold;}"))))),

    fluidRow(column(10,offset=1,htmlOutput(NS(id,"definition_ind")))),

    fluidRow(column(4,offset =1,textOutput(NS(id,"titre_section1")),
                    tags$head(tags$style(paste0("#",NS(id,"titre_section1"),"{color: #0F2B5FFF;font-size: 24px;font-style: bold;}"))))),

### Deuxième bloc menu déroulant #####

    fluidRow(
      column(1,offset = 4,align="center",selectInput(NS(id,"ann"), "Année", choices = NULL)),
      column(4,selectInput(NS(id,"type_ana"), "Type d'analyse",
                           choices = c("Comparaison des données selon le territoire","Comparaison des données selon le sexe et le territoire"),width="550px"))),

### Visuel de la map/maps ################

    fluidRow(
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le territoire'"),
        column(12,align="center",textOutput(NS(id,"map_gf_caption2")),
               tags$head(tags$style(paste0("#",NS(id,"map_gf_caption2"),"{color: #007A99FF;font-size: 16px;font-style: bold;}")))),

      ),
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le sexe et le territoire'"),
        column(6,align="center",textOutput(NS(id,"map_f_caption2")),
               tags$head(tags$style(paste0("#",NS(id,"map_f_caption2"),"{color: #007A99FF;font-size: 16px;font-style: bold;}")))),
        column(6,align="center",textOutput(NS(id,"map_g_caption2")),
               tags$head(tags$style(paste0("#",NS(id,"map_g_caption2"),"{color: #007A99FF;font-size: 16px;font-style: bold;}"))))
      )
    ),

    fluidRow(
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le territoire'"),
        column(12,align="center",textOutput(NS(id,"map_gf_caption3")),
               tags$head(tags$style(paste0("#",NS(id,"map_gf_caption3"),"{color: #007A99FF;font-size: 16px;font-style: bold;}"))))
      ),
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le sexe et le territoire'"),
        column(6,align="center",textOutput(NS(id,"map_f_caption3")),
               tags$head(tags$style(paste0("#",NS(id,"map_f_caption3"),"{color: #007A99FF;font-size: 16px;font-style: bold;}")))),
        column(6,align="center",textOutput(NS(id,"map_g_caption3")),
               tags$head(tags$style(paste0("#",NS(id,"map_g_caption3"),"{color: #007A99FF;font-size: 16px;font-style: bold;}"))))
      )
    ),

    fluidRow(
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le territoire'"),
        column(6,offset = 3,leafletOutput(NS(id,"map_gf"), width = "100%", height = 500))
      ),
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le sexe et le territoire'"),
        column(6,leafletOutput(NS(id,"map_f"), width = "100%", height = 500)),
        column(6,leafletOutput(NS(id,"map_g"), width = "100%", height = 500))
      )
    ),

    br(),

    fluidRow(
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le territoire'"),
        column(6,offset=3, align="center",
               actionButton(NS(id,"hide_meta_carte"), "Précisions sur la carte",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                            style="color: #fff; background-color: #BDBFBF; border-color: #2e6da4; padding:5px; font-size:85%"))
      ),
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le sexe et le territoire'"),
        column(6, align="center",
               actionButton(NS(id,"hide_meta_carte3"), "Précisions sur la carte",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                            style="color: #fff; background-color: #BDBFBF; border-color: #2e6da4; padding:5px; font-size:85%")),
        column(6, align="center",
               actionButton(NS(id,"hide_meta_carte2"), "Précisions sur la carte",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                            style="color: #fff; background-color: #BDBFBF; border-color: #2e6da4; padding:5px; font-size:85%"))
      )
    ),

    br(),

    fluidRow(
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le territoire'"),
        column(6,offset = 3,hidden(htmlOutput(NS(id,"meta_carte"))))
      ),
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le sexe et le territoire'"),
        column(6,hidden(htmlOutput(NS(id,"meta_carte3")))),
        column(6,hidden(htmlOutput(NS(id,"meta_carte2"))))

      )
    ),

### Visuel de l'histrograme/histrogrames ############

    fluidRow(
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le territoire'"),
        column(12,align="center",textOutput(NS(id,"hist_gf_caption2")),
               tags$head(tags$style(paste0("#",NS(id,"hist_gf_caption2"),"{color: #007A99FF;font-size: 16px;font-style: bold;}"))))
      ),
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le sexe et le territoire'"),
        column(6,align="center",textOutput(NS(id,"hist_f_caption2")),
               tags$head(tags$style(paste0("#",NS(id,"hist_f_caption2"),"{color: #007A99FF;font-size: 16px;font-style: bold;}")))),
        column(6,align="center",textOutput(NS(id,"hist_g_caption2")),
               tags$head(tags$style(paste0("#",NS(id,"hist_g_caption2"),"{color: #007A99FF;font-size: 16px;font-style: bold;}"))))
      )
    ),

    fluidRow(
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le territoire'"),
        column(12,align="center",textOutput(NS(id,"hist_gf_caption3")),
               tags$head(tags$style(paste0("#",NS(id,"hist_gf_caption3"),"{color: #007A99FF;font-size: 16px;font-style: bold;}"))))
      ),
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le sexe et le territoire'"),
        column(6,align="center",textOutput(NS(id,"hist_f_caption3")),
               tags$head(tags$style(paste0("#",NS(id,"hist_f_caption3"),"{color: #007A99FF;font-size: 16px;font-style: bold;}")))),
        column(6,align="center",textOutput(NS(id,"hist_g_caption3")),
               tags$head(tags$style(paste0("#",NS(id,"hist_g_caption3"),"{color: #007A99FF;font-size: 16px;font-style: bold;}"))))
      )
    ),

    fluidRow(
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le territoire'"),
        column(6,offset = 3,plotOutput(NS(id,"hist_mrc_gf"),width = "100%", height = 600))
      ),
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le sexe et le territoire'"),
        column(6,plotOutput(NS(id,"hist_mrc_f"),width = "100%", height = 600)),
        column(6,plotOutput(NS(id,"hist_mrc_g"),width = "100%", height = 600))
      )
    ),

    fluidRow(
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le territoire'"),
        column(6,offset=3, align="center",
               actionButton(NS(id,"hide_meta_hist"), "Précisions sur le graphique",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                            style="color: #fff; background-color: #BDBFBF; border-color: #2e6da4; padding:5px; font-size:85%"))
      ),
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le sexe et le territoire'"),
        column(6, align="center",
               actionButton(NS(id,"hide_meta_hist3"), "Précisions sur le graphique",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                            style="color: #fff; background-color: #BDBFBF; border-color: #2e6da4; padding:5px; font-size:85%")),
        column(6, align="center",
               actionButton(NS(id,"hide_meta_hist2"), "Précisions sur le graphique",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                            style="color: #fff; background-color: #BDBFBF; border-color: #2e6da4; padding:5px; font-size:85%"))
      )
    ),

    br(),

    fluidRow(
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le territoire'"),
        column(6,offset = 3,hidden(htmlOutput(NS(id,"meta_hist"))))
      ),
      conditionalPanel(
        condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le sexe et le territoire'"),
        column(6,hidden(htmlOutput(NS(id,"meta_hist3")))),
        column(6,hidden(htmlOutput(NS(id,"meta_hist2"))))

      )
    ),

## Évolution temporelle ########################

    fluidRow(column(6,offset = 1,align="left",textOutput(NS(id,"titre_evo")),
                    tags$head(tags$style(paste0("#",NS(id,"titre_evo"),"{color: #0F2B5FFF;font-size: 24px;font-style: bold;}"))))),
    fluidRow(column(10,offset = 1,align="left",htmlOutput(NS(id,"description_evo")))),fluidRow(br()),


    fluidRow(
      column(3,offset =1,align="center",pickerInput(NS(id,"nom_evo_temp_2"),"Sélectionner les territoires à comparer (maximum 2)",selected = NULL, choices = NULL,
                                                    pickerOptions(
                                                      #actionsBox = TRUE,
                                                      size = 10,
                                                      #"selected-text-format" = "count > 3",
                                                      maxOptions = 2,
                                                      maxOptionsText = "Le maximum est atteint",
                                                      noneSelectedText = "Aucune sélection",
                                                      selectAllText = "Tout sélectionner",
                                                      deselectAllText = "Désélectionner",
                                                    ),
                                                    multiple=T)),
      column(7,plotOutput(NS(id,"evo_temp_region_sexe")))),

    fluidRow(
      column(3,offset =1,align="center",
             pickerInput(NS(id,"nom_evo_temp_10"),"Sélectionner les territoires à comparer",selected = NULL, choices = NULL,
                         pickerOptions(
                           #actionsBox = TRUE,
                           size = 10,
                           #"selected-text-format" = "count > 3",
                           maxOptions = 9,
                           maxOptionsText = "Le maximum est atteint",
                           noneSelectedText = "Aucune sélection",
                           selectAllText = "Tout sélectionner",
                           deselectAllText = "Désélectionner",
                         ),
                         multiple=T),
             selectInput(NS(id,"sexe"), "Sexe", choices = unique_no.NA(grande_bd_parsco$sexe), selected = "Sexes réunis")
      ),
      column(7,plotOutput(NS(id,"evo_temp_choix_gf")))),

## Tabeau de données ###########################

    fluidRow(br()),
    fluidRow(column(6,offset = 1,align="left",textOutput(NS(id,"titre_tab")),
                    tags$head(tags$style(paste0("#",NS(id,"titre_tab"),"{color: #0F2B5FFF;font-size: 24px;font-style: bold;}"))))),

    fluidRow(column(10,offset = 1,align="left",htmlOutput(NS(id,"description_tab")))),
    # fluidRow(column(10,offset = 1,align="left",textOutput(NS(id,"description_tab")),
    #                 tags$head(tags$style(paste0("#",NS(id,"description_tab"),"{font-size: 16px;font-style: bold;}"))))),

    fluidRow(
      column(2,align="center",offset = 4,selectInput(NS(id,"sexe_2"), "Sexe", choices = c("Tous les regroupements","Filles","Garçons","Sexes réunis"))),
      column(2,align="center",selectInput(NS(id,"reseau_2"), "Réseau", choices = c("Tous les regroupements","Public","Privé","Tous réseaux confondus")))),

    fluidRow(column(12,align="center",textOutput(NS(id,"table_parsco_titre")),
                tags$head(tags$style(paste0("#",NS(id,"table_parsco_titre"),"{color: #007A99FF;font-size: 16px;font-style: bold;}"))))),fluidRow(br()),


    fluidRow(column(12,DT::dataTableOutput(NS(id,"table_parsco")))),
    fluidRow(br()),
    fluidRow(column(3,offset = 3, align="left",
                    actionButton(NS(id,"hide_meta_tableau"), "Précisions sur le tableau",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                                 style="color: #fff;background-color: #BDBFBF;padding:5px; font-size:85%")),
             column(3,align="right",downloadButton(NS(id,"download"), "Téléchargement du tableau",style="color: #fff; background-color: #3C65CF"))),
    fluidRow(br()),
    fluidRow(column(6,offset = 1,hidden(htmlOutput(NS(id,"meta_tableau"))))),
    fluidRow(br()),



## Analyse par cohorte #############################

  fluidRow(hr()),

  fluidRow(column(6,offset = 0,align="left",textOutput(NS(id,"titre_cohorte")),
                tags$head(tags$style(paste0("#",NS(id,"titre_cohorte"),"{color: #36B0C9;font-size: 24px;font-style: bold;}"))))),

  fluidRow(column(11,offset = 0,align="left",htmlOutput(NS(id,"description_cohorte")))),

  fluidRow(
    column(2,offset = 2,selectInput(NS(id,"ra_coh"), "Territoire d'intérêt", choices = ordre_ra)),
    column(3,selectInput(NS(id,"autre_reg"), label = "Territoire affiché", choices = NULL)),
    column(2,selectInput(NS(id,"reseau_coh"), "Réseau", choices = unique_no.NA(pars_second_cohorte$reseau), selected = "Tous réseaux confondus")),
    column(2,selectInput(NS(id,"sexe_coh"), "Sexe", choices = unique_no.NA(pars_second_cohorte$sexe), selected = "Sexes Réunis"))),

  fluidRow(column(6,plotOutput(NS(id,"hist_cohorte_mrc"),width = "100%", height = 500)),
           column(6,plotOutput(NS(id,"evo_cohorte_mrc"),width = "100%", height = 500))),
fluidRow(br()),
fluidRow(br()),

  fluidRow(column(12,align="center",textOutput(NS(id,"table_cohorte_titre")),
         tags$head(tags$style(paste0("#",NS(id,"table_cohorte_titre"),"{color: #007A99FF;font-size: 16px;font-style: bold;}"))))),fluidRow(br()),

  fluidRow(column(12,DT::dataTableOutput(NS(id,"table_cohorte")))),

fluidRow(br()),
fluidRow(br()),

fluidRow(column(3,offset = 3, align="left",
                actionButton(NS(id,"hide_meta_tableau2"), "Précisions sur le tableau",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                             style="color: #fff;background-color: #BDBFBF;padding:5px; font-size:85%")),
         column(3,offset = 9,downloadButton(NS(id,"down_cohorte"), "Téléchargement du tableau",style="color: #fff; background-color: #3C65CF"))),

fluidRow(column(6,offset = 1,hidden(htmlOutput(NS(id,"meta_tableau2"))))),

  fluidRow(column(10,offset = 1, htmlOutput(NS(id,"citer_site")))),fluidRow(br()),

### Citation

fluidRow(column(3,offset = 1,
                actionButton(NS(id,"hide_meta_citation"), "Référence suggérée",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                             style="color: #fff; background-color: #36B0C9; border-color: #2e6da4;"))),br(),

fluidRow(column(10,offset = 1,hidden(htmlOutput(NS(id,"meta_citation"))))),
br(),br(),br(),br(),br(),br(),br(),


  )
}

#################################################################################################################################################
# Module server
#################################################################################################################################################

test_parco_server <- function(id){
  moduleServer(id, function(input,output,session){

    output$titre<-renderText("Parcours primaire-secondaire")

    output$titre_def <- renderUI({HTML(texte_desc_prim_second$spec_region)})

    output$nom_indicateur <- renderText(paste("Indicateur : ", meta_ind()$nom_ind_complet_unite))
    output$definition_ind <- renderUI({HTML(meta_ind()$def_ind_onglet)})

    output$titre_section1<-renderText("Visualisations et comparaisons")

# Pour obtenir une région selon l'indicateur sélectionné
    observeEvent(input$ind,{
      current_ra <- input$ra
      if(input$ind %in% ind_decoup_ra){choices <- "Ensemble du Québec"}
      else{choices <- alpha_reg}
      freezeReactiveValue(input, "ra")
       if(input$ind == "Nombre d’élèves au primaire (nb)"){
         updateSelectInput(inputId = "ra", choices = choices)
      }
      else if(input$ind %in% ind_decoup_mrc & current_ra %in% c("Ensemble du Québec")){
        updateSelectInput(inputId = "ra", choices = choices)
      }
      else if(input$ind %in% ind_decoup_mrc){
        updateSelectInput(inputId = "ra", selected = current_ra)
       }
        else if(input$ind %in% ind_decoup_ra){updateSelectInput(inputId = "ra", choices = choices)}
      else{updateSelectInput(inputId = "ra", choices = choices)}
      #updateSelectInput(inputId = "ra", choices = choices)
    })

# Pour obtenir le choix de réseau selon l'indicateur sélectionné
    observeEvent(input$ind,{
      if(input$ind %in% ind_reseau_public){choices <- "Public"}
      else{choices <- fact_reseau}
      updateSelectInput(inputId = "reseau", choices = choices)
    })

# Pour les actions buttons
    observeEvent(input$hide_meta_carte,{toggle("meta_carte")})
    observeEvent(input$hide_meta_carte2,{toggle("meta_carte2")})
    observeEvent(input$hide_meta_carte3,{toggle("meta_carte3")})

    observeEvent(input$hide_meta_hist,{toggle("meta_hist")})
    observeEvent(input$hide_meta_hist2,{toggle("meta_hist2")})
    observeEvent(input$hide_meta_hist3,{toggle("meta_hist3")})

    observeEvent(input$hide_meta_tableau,{toggle("meta_tableau")})
    observeEvent(input$hide_meta_tableau2,{toggle("meta_tableau2")})

    observeEvent(input$hide_meta_citation, {toggle("meta_citation")})

    observeEvent(input$hide_precision_cohorte,{toggle("precision_cohorte")})
    observeEvent(input$hide_precision_territoire,{toggle("precision_territoire")})
    observeEvent(input$hide_precision_reseau,{toggle("precision_reseau")})


# Pour obtenir la liste autre région selon la région 1 sélectionnée
    list_autre_region <-reactive({
        if(input$ra_coh == "Ensemble du Québec"){pars_second_cohorte |>
          filter(decoupage %in% c("ra","prov"))
        # }else{pars_second_cohorte |> filter(ra_app_nom == input$ra_coh, decoupage == "mrc")}
          }else{pars_second_cohorte |> filter(ra_app_nom == input$ra_coh)}
    })

    observeEvent(list_autre_region(),{
      choices <- unique(list_autre_region()$nom)
      freezeReactiveValue(input, "autre_reg")
      updateSelectInput(inputId = "autre_reg", choices = choices)
    })

# meta_ind() contient les métadata de l'indicateur sélectionné.
    meta_ind <- reactive({metadata %>%
        filter(nom_ind_court == input$ind)
    })

# Pour obtenir la liste des années disponibles selon l'indicateur.
    annees_range <- reactive({
      req(input$ind)
      if(meta_ind()$type_annee == "plage"){c(meta_ind()$an_max:meta_ind()$an_min)}
      else{c(meta_ind()$an_max,meta_ind()$an_min)}
    })

    observeEvent(annees_range(),{
      choices <- annees_range()
      freezeReactiveValue(input, "ann")
      updateSelectInput(inputId = "ann", choices = choices)
    })

# Une autre liste des années disponibles pour l'indicateur, utilisé dans le tableau.
    annees_ind <- reactive({
      req(input$ind)
      c(meta_ind()$an1,meta_ind()$an2,meta_ind()$an3,meta_ind()$an4,meta_ind()$an5,meta_ind()$an6,meta_ind()$an7,meta_ind()$an8,meta_ind()$an9,meta_ind()$an10,meta_ind()$an11)
    })

    annee_min <- reactive({
      c(meta_ind()$an_min)
    })

    annee_max <- reactive({
      c(meta_ind()$an_max)
    })

    bd_mrc <- reactive({bd_pars_sco %>%
        filter(annee_num == input$ann)%>%
        filter(reseau == input$reseau)%>%
        filter(ra_app_nom == input$ra | decoupage == "prov") %>%
        select(annee,nom,code_geo,sexe,meta_ind()$indicateurs,color_ecobes,decoupage)
    })

    bd_ra <- reactive({bd_pars_sco %>%
        filter(annee_num == input$ann)%>%
        filter(decoupage == "ra" | decoupage == "prov") %>%
        filter(reseau == input$reseau)%>%
        select(annee,nom,code_geo,sexe,meta_ind()$indicateurs,color_ecobes,decoupage)
    })

    bd_ra2 <- reactive({
      if(input$ra == "Ensemble du Québec" & meta_ind()$unite == "(nb)"){bd_ra() |> filter(decoupage == "ra")}
      else{bd_ra()}
    })

    bd_mrc2 <- reactive({
      if(input$ra %in% list_ra_prim & meta_ind()$unite == "(nb)"){bd_mrc() |> filter(decoupage != "prov")}
      else{bd_mrc()}
    })

    bd_map_mrc_gf <- reactive({
      if(input$ra == "Ensemble du Québec"){
        shp_ra_simp |>
          left_join(bd_ra2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Sexes réunis") |>
          filter(!is.na(nom))
      }
      else if(input$ra %in%c("Laval")){
        shp_mtl_laval |>
          left_join(bd_mrc2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Sexes réunis") |>
          filter(!is.na(nom))
      }else if(input$ann < 2021 & input$ra %in%c("Montréal") & input$ind %in% c("Sortants du secondaire (nb)","Sorties sans diplôme ni qualification (nb)","Sorties sans diplôme ni qualification (%)")){
        shp_mrc_simp |>
          left_join(bd_mrc2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Sexes réunis") |>
          filter(!is.na(nom))
      }
      else if(input$ra %in%c("Montréal")){
        shp_mtl_laval |>
          left_join(bd_mrc2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Sexes réunis") |>
          filter(!is.na(nom))
      }
      else{
        shp_mrc_simp%>%
          left_join(bd_mrc2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Sexes réunis") |>
          filter(!is.na(nom))
      }

    })

    bd_map_mrc_f <- reactive({
      if(input$ra == "Ensemble du Québec"){
        shp_ra_simp |>
          left_join(bd_ra2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Filles") |>
          filter(!is.na(nom))
      }
      else if(input$ra %in%c("Laval")){
        shp_mtl_laval |>
          left_join(bd_mrc2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Filles") |>
          filter(!is.na(nom))
      }else if(input$ann < 2021 & input$ra %in%c("Montréal") & input$ind %in% c("Sortants du secondaire (nb)","Sorties sans diplôme ni qualification (nb)","Sorties sans diplôme ni qualification (%)")){
        shp_mrc_simp |>
          left_join(bd_mrc2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Filles") |>
          filter(!is.na(nom))
      }
      else if(input$ra %in%c("Montréal")){
        shp_mtl_laval |>
          left_join(bd_mrc2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Filles") |>
          filter(!is.na(nom))
      }
      else{
        shp_mrc_simp%>%
          left_join(bd_mrc2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Filles") |>
          filter(!is.na(nom))
      }

    })


    bd_map_mrc_g <- reactive({
      if(input$ra == "Ensemble du Québec"){
        shp_ra_simp |>
          left_join(bd_ra2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Garçons") |>
          filter(!is.na(nom))
      }
      else if(input$ra %in%c("Laval")){
        shp_mtl_laval |>
          left_join(bd_mrc2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Garçons") |>
          filter(!is.na(nom))
      }else if(input$ann < 2021 & input$ra %in%c("Montréal") & input$ind %in% c("Sortants du secondaire (nb)","Sorties sans diplôme ni qualification (nb)","Sorties sans diplôme ni qualification (%)")){
        shp_mrc_simp |>
          left_join(bd_mrc2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Garçons") |>
          filter(!is.na(nom))
      }
      else if(input$ra %in%c("Montréal")){
        shp_mtl_laval |>
          left_join(bd_mrc2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Garçons") |>
          filter(!is.na(nom))
      }
      else{
        shp_mrc_simp%>%
          left_join(bd_mrc2(),by = c("code_geo" = "code_geo")) |>
          filter(sexe == "Garçons") |>
          filter(!is.na(nom))
      }

    })



    bd_map_g_plus_f <- reactive({bind_rows(bd_map_mrc_g(),bd_map_mrc_f())})

    bd_interval_gf <- reactive({
      if(input$type_ana == "Comparaison des données selon le territoire"){bd_map_mrc_gf()}
      else{bd_map_g_plus_f()}
    })

    #Comparaison des données selon le sexe et le territoire

    #### La carte ####

    #max_binpal <- reactive({max(bd_interval_gf()[[meta_ind()$indicateurs]])})

    binpal <- reactive({
      if(all(is.na(bd_interval_gf()[[meta_ind()$indicateurs]]))){colorBin("YlGn", c(0:2), 2, pretty = FALSE)}
      else if(sum(complete.cases(bd_interval_gf()[[meta_ind()$indicateurs]])) == 1){colorBin("YlGn", c(0:500), 2, pretty = FALSE)}
      else if(sum((bd_interval_gf()[[meta_ind()$indicateurs]]),na.rm=TRUE) == 0){colorBin("YlGn", c(0:2), 2, pretty = FALSE)}
      else{colorBin("YlGn", bd_interval_gf()[[meta_ind()$indicateurs]], 6, pretty = FALSE)}
    })

    output$map_gf_caption1<-renderText(paste("territoire :", req(input$ra)))
    output$map_gf_caption2<-renderText(paste(meta_ind()$nom_ind_complet_unite))
    output$map_gf_caption3<-renderText(paste("Sexes réunis"," - ", req(input$reseau)," - ",req(input$ann)))
    output$map_f_caption1<-renderText(paste("territoire :", req(input$ra)))
    output$map_f_caption2<-renderText(paste(meta_ind()$nom_ind_complet_unite))
    output$map_f_caption3<-renderText(paste("Filles"," - ", req(input$reseau)," - ",req(input$ann)))
    output$map_g_caption1<-renderText(paste("territoire :", req(input$ra)))
    output$map_g_caption2<-renderText(paste(meta_ind()$nom_ind_complet_unite))
    output$map_g_caption3<-renderText(paste("Garçons"," - ", req(input$reseau)," - ",req(input$ann)))

    output$map_gf <- renderLeaflet({
      if(nrow(bd_map_mrc_gf())>2 & input$type_ana == "Comparaison des données selon le territoire"){
        m <- leaflet(bd_map_mrc_gf(),options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1, wheelPxPerZoomLevel = 120)) |>
          addProviderTiles("Esri.WorldGrayCanvas") |>
          addProviderTiles("CartoDB.VoyagerOnlyLabels") |>
          addPolygons(fillColor = ~binpal()(bd_map_mrc_gf()[[meta_ind()$indicateurs]]),weight = 1,color = "#444444",fillOpacity = 0.7,
                      popup=paste0(bd_map_mrc_gf()$nom," - ",lfrench(bd_map_mrc_gf()[[meta_ind()$indicateurs]])),
                      label=~paste0(bd_map_mrc_gf()$nom," - ",lfrench(bd_map_mrc_gf()[[meta_ind()$indicateurs]]))
          ) |>
          addLegend("topright",pal = binpal(), values = ~bd_map_mrc_gf()[[meta_ind()$indicateurs]],
                    title = " ",
                    labFormat = my_labelFormat(digits = 1),
                    opacity = 1
          )
      }
      else{
        m <- leaflet(bd_map_mrc_gf(),options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1, wheelPxPerZoomLevel = 120)) |>
          addProviderTiles("Esri.WorldGrayCanvas") |>
          addProviderTiles("CartoDB.VoyagerOnlyLabels") |>
          addPolygons(fillColor = "green",weight = 1,color = "#444444",fillOpacity = 0.4,
                      popup=paste0(bd_map_mrc_gf()$nom," - ",lfrench(bd_map_mrc_gf()[[meta_ind()$indicateurs]])),
                      label=~paste0(bd_map_mrc_gf()$nom," - ",lfrench(bd_map_mrc_gf()[[meta_ind()$indicateurs]]))
          )
      }
    })



    output$map_g <- renderLeaflet({
      if(nrow(bd_map_mrc_g())>2 & input$type_ana == "Comparaison des données selon le sexe et le territoire"){
        m <- leaflet(bd_map_mrc_g(),options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1, wheelPxPerZoomLevel = 120)) |>
          addProviderTiles("Esri.WorldGrayCanvas") |>
          addProviderTiles("CartoDB.VoyagerOnlyLabels") |>
          addPolygons(fillColor = ~binpal()(bd_map_mrc_g()[[meta_ind()$indicateurs]]),weight = 1,color = "#444444",fillOpacity = 0.7,
                      popup=paste0(bd_map_mrc_g()$nom," - ",lfrench(bd_map_mrc_g()[[meta_ind()$indicateurs]])),
                      label=~paste0(bd_map_mrc_g()$nom," - ",lfrench(bd_map_mrc_g()[[meta_ind()$indicateurs]]))
          ) |>
          addLegend("topright",pal = binpal(), values = ~bd_map_mrc_g()[[meta_ind()$indicateurs]],
                    title = " ",
                    labFormat = my_labelFormat(digits = 1),
                    opacity = 1
          )
      }
      else{
        m <- leaflet(bd_map_mrc_g(),options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1, wheelPxPerZoomLevel = 120)) |>
          addProviderTiles("Esri.WorldGrayCanvas") |>
          addProviderTiles("CartoDB.VoyagerOnlyLabels") |>
          addPolygons(fillColor = "green",weight = 1,color = "#444444",fillOpacity = 0.4,
                      popup=paste0(bd_map_mrc_g()$nom," - ",lfrench(bd_map_mrc_g()[[meta_ind()$indicateurs]])),
                      label=~paste0(bd_map_mrc_g()$nom," - ",lfrench(bd_map_mrc_g()[[meta_ind()$indicateurs]]))
          )
      }
    })



    output$map_f <- renderLeaflet({
      if(nrow(bd_map_mrc_f())>2 & input$type_ana == "Comparaison des données selon le sexe et le territoire"){
        m <- leaflet(bd_map_mrc_f(),options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1, wheelPxPerZoomLevel = 120)) |>
          addProviderTiles("Esri.WorldGrayCanvas") |>
          addProviderTiles("CartoDB.VoyagerOnlyLabels") |>
          addPolygons(fillColor = ~binpal()(bd_map_mrc_f()[[meta_ind()$indicateurs]]),weight = 1,color = "#444444",fillOpacity = 0.7,
                      popup=paste0(bd_map_mrc_f()$nom," - ",lfrench(bd_map_mrc_f()[[meta_ind()$indicateurs]])),
                      label=~paste0(bd_map_mrc_f()$nom," - ",lfrench(bd_map_mrc_f()[[meta_ind()$indicateurs]]))
          ) |>
          addLegend("topright",pal = binpal(), values = ~bd_map_mrc_f()[[meta_ind()$indicateurs]],
                    title = " ",
                    labFormat = my_labelFormat(digits = 1),
                    opacity = 1
          )
      }
      else{
        m <- leaflet(bd_map_mrc_f(),options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1, wheelPxPerZoomLevel = 120)) |>
          addProviderTiles("Esri.WorldGrayCanvas") |>
          addProviderTiles("CartoDB.VoyagerOnlyLabels") |>
          addPolygons(fillColor = "green",weight = 1,color = "#444444",fillOpacity = 0.4,
                      popup=paste0(bd_map_mrc_f()$nom," - ",lfrench(bd_map_mrc_f()[[meta_ind()$indicateurs]])),
                      label=~paste0(bd_map_mrc_f()$nom," - ",lfrench(bd_map_mrc_f()[[meta_ind()$indicateurs]]))
          )
      }
    })

    output$meta_carte <- renderUI({HTML(texte_metho_carte$spec_region)})
    output$meta_carte2 <- renderUI({HTML(texte_metho_carte$spec_region)})
    output$meta_carte3 <- renderUI({HTML(texte_metho_carte$spec_region)})

    output$meta_hist <- renderUI({HTML(texte_metho_graph$spec_region)})
    output$meta_hist2 <- renderUI({HTML(texte_metho_graph$spec_region)})
    output$meta_hist3 <- renderUI({HTML(texte_metho_graph$spec_region)})

    output$meta_tableau <- renderUI({HTML(texte_metho_tableau$spec_region)})
    output$meta_tableau2 <- renderUI({HTML(texte_metho_tableau$spec_region)})

    output$precision_cohorte <- renderUI({HTML(texte_precision_cohorte$spec_region)})
    output$precision_territoire <- renderUI({HTML(texte_precision_territoire$spec_region)})
    output$precision_reseau <- renderUI({HTML(texte_precision_reseau$spec_region)})

  #### Diagramme à barres ####

    output$hist_gf_caption2<-renderText(paste(meta_ind()$nom_ind_complet_unite))
    output$hist_gf_caption3<-renderText(paste("Sexes réunis"," - ", req(input$reseau)," - ",req(input$ann)))

    output$hist_f_caption2<-renderText(paste(meta_ind()$nom_ind_complet_unite))
    output$hist_f_caption3<-renderText(paste("Filles"," - ", req(input$reseau)," - ",req(input$ann)))

    output$hist_g_caption2<-renderText(paste(meta_ind()$nom_ind_complet_unite))
    output$hist_g_caption3<-renderText(paste("Garçons"," - ", req(input$reseau)," - ",req(input$ann)))


    bd_hist_mrc_gf <- reactive({
      if(input$ra == "Ensemble du Québec" & meta_ind()$unite == "(nb)"){bd_ra() |> filter(sexe == "Sexes réunis", decoupage == "ra") |> drop_na()}
      else if(input$ra == "Ensemble du Québec"){bd_ra() |> filter(sexe == "Sexes réunis") |> drop_na()}
      else if(input$ra %in% list_ra_prim & meta_ind()$unite == "(nb)"){bd_mrc() %>%filter(sexe == "Sexes réunis",decoupage != "prov") |> drop_na()}
      else{bd_mrc() %>%filter(sexe == "Sexes réunis") |> drop_na()}
    })

    output$hist_mrc_gf <- renderPlot({
      ggplot(data = bd_hist_mrc_gf(),mapping = aes(x=.data[[meta_ind()$indicateurs]], y= reorder(nom,.data[[meta_ind()$indicateurs]]))) +
        geom_bar(aes(fill = color_ecobes), stat = "identity")+
        scale_fill_identity(guide = "none")+
        geom_text(aes(label = format(.data[[meta_ind()$indicateurs]],accuracy=0.1, big.mark = " ", decimal.mark = ",")),hjust = -0.25, size=5.0)+
        scale_x_continuous(expand = expansion(mult= c(0,0.20)))+
        theme_minimal()+
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank())+
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())+
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 14.0, hjust = 1,vjust = 0.5))+

        labs(title = ,x = "",y = "",subtitle = "",caption = "")
    })


    # bd_hist_mrc_f <- reactive({
    #   if(input$ra == "Ensemble du Québec"){bd_ra() |> filter(sexe == "Filles")|> drop_na()}
    #   else{bd_mrc() %>%filter(sexe == "Filles")|> drop_na()}
    # })

    bd_hist_mrc_f <- reactive({
      if(input$ra == "Ensemble du Québec" & meta_ind()$unite == "(nb)"){bd_ra() |> filter(sexe == "Filles", decoupage == "ra") |> drop_na()}
      else if(input$ra == "Ensemble du Québec"){bd_ra() |> filter(sexe == "Filles") |> drop_na()}
      else if(input$ra %in% list_ra_prim & meta_ind()$unite == "(nb)"){bd_mrc() %>%filter(sexe == "Filles",decoupage != "prov") |> drop_na()}
      else{bd_mrc() %>%filter(sexe == "Filles") |> drop_na()}
    })

    output$hist_mrc_f <- renderPlot({
      ggplot(data = bd_hist_mrc_f(),mapping = aes(x=.data[[meta_ind()$indicateurs]], y= reorder(nom,.data[[meta_ind()$indicateurs]]))) +
        geom_bar(aes(fill = color_ecobes), stat = "identity")+
        scale_fill_identity(guide = "none")+
        geom_text(aes(label = format(.data[[meta_ind()$indicateurs]],accuracy=0.1, big.mark = " ", decimal.mark = ",")),hjust = -0.25, size=5.0)+
        scale_x_continuous(expand = expansion(mult= c(0,0.20)))+
        theme_minimal()+
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank())+
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())+
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 12.0, hjust = 1,vjust = 0.5))+

        labs(title = ,x = "",y = "",subtitle = "",caption = "")
    })

    # bd_hist_mrc_g <- reactive({
    #   if(input$ra == "Ensemble du Québec"){bd_ra() |> filter(sexe == "Garçons")|> drop_na()}
    #   else{bd_mrc() %>%filter(sexe == "Garçons")|> drop_na()}
    # })

    bd_hist_mrc_g <- reactive({
      if(input$ra == "Ensemble du Québec" & meta_ind()$unite == "(nb)"){bd_ra() |> filter(sexe == "Garçons", decoupage == "ra") |> drop_na()}
      else if(input$ra == "Ensemble du Québec"){bd_ra() |> filter(sexe == "Garçons") |> drop_na()}
      else if(input$ra %in% list_ra_prim & meta_ind()$unite == "(nb)"){bd_mrc() %>%filter(sexe == "Garçons",decoupage != "prov") |> drop_na()}
      else{bd_mrc() %>%filter(sexe == "Garçons") |> drop_na()}
    })

    output$hist_mrc_g <- renderPlot({
      ggplot(data = bd_hist_mrc_g(),mapping = aes(x=.data[[meta_ind()$indicateurs]], y= reorder(nom,.data[[meta_ind()$indicateurs]]))) +
        geom_bar(aes(fill = color_ecobes), stat = "identity")+
        scale_fill_identity(guide = "none")+
        #geom_text(aes_string(label = meta_ind()$indicateurs,fontface=2),size=3.8,hjust = 0, nudge_x = 0.5)+
        geom_text(aes(label = format(.data[[meta_ind()$indicateurs]],accuracy=0.1, big.mark = " ", decimal.mark = ",")),hjust = -0.25, size=5.0)+
        scale_x_continuous(expand = expansion(mult= c(0,0.20)))+
        theme_minimal()+
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank())+
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())+
        theme(legend.position="none")+
        theme(axis.text.y = element_text(size = 12.0, hjust = 1,vjust = 0.5))+

        labs(title = ,x = "",y = "",subtitle = "",caption = "")
    })


  #### Evolution temporelle ####

    output$titre_evo<-renderText("Évolution temporelle")
    output$description_evo <- renderUI({HTML(texte_evo_temp_prim_second$spec_region)})

    pars_sco_region <-reactive({
      if(input$ra == "Ensemble du Québec")
      {bd_pars_sco |>
          filter(decoupage == "ra" | decoupage == "prov")}
      else{
        bd_pars_sco |>
          filter(ra_app_nom == input$ra | nom == "Ensemble du Québec") |>
          filter(decoupage %in% c("prov","ra","mrc"))}
    })

    observeEvent(pars_sco_region(),{
      choices <- unique(pars_sco_region()$nom)
      freezeReactiveValue(input, NS(id,"ra"))
      updatePickerInput(session,"nom_evo_temp_2", choices = choices)
      updatePickerInput(session,"nom_evo_temp_10", choices = choices)
    })

    bd_temporel_region_sexe <- reactive({bd_pars_sco %>%
        filter(nom %in% input$nom_evo_temp_2) %>%
        filter(reseau == input$reseau)%>%
        mutate(annee = as.numeric(annee)) |>
        filter(annee %in% annees_ind()) |>
        select(annee,nom,ra_app_nom,sexe,meta_ind()$indicateurs,color_ecobes,line_comp) |>
        group_by(nom,sexe) |>
        #mutate(moving_avg3 = rollmean(.data[[meta_ind()$indicateurs]],k=3, fill=NA,align = "right"))
        mutate(moving_avg3 = .data[[meta_ind()$indicateurs]])

    })

    bd_temporel_choix_gf <- reactive({bd_pars_sco %>%
        filter(nom %in% input$nom_evo_temp_10) %>%
        filter(reseau == input$reseau)%>%
        filter(sexe == input$sexe)%>%
        mutate(annee = as.numeric(annee)) |>
        filter(annee %in% annees_ind()) |>
        select(annee,nom,ra_app_nom,meta_ind()$indicateurs,color_ecobes) |>
        group_by(nom) |>
        #mutate(moving_avg3 = rollmean(.data[[meta_ind()$indicateurs]],k=3, fill=NA,align = "right"))
        mutate(moving_avg3 = .data[[meta_ind()$indicateurs]])
    })

    # https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/

    output$evo_temp_region_sexe <- renderPlot({
      ggplot(data = bd_temporel_region_sexe(),aes_string(x = "annee", y = "moving_avg3",color = "sexe", linetype = "nom")) +
        theme_minimal()+
        geom_line(size = .99)+
        #geom_line(linewidth = .99)+
        geom_point(size = 2.0)+
        scale_color_manual(values=c("#00A9CCFF", "#7AD151FF", "#22A884FF"))+
        #scale_color_manual(values= gold_purple)+
        guides(color=guide_legend(title="Sexe"))+
        guides(linetype=guide_legend(title="territoire"))+
        scale_x_continuous(
          expand = expansion(mult = c(0.05, 0.05)),
          breaks = seq(annee_min(), annee_max(), by = 1)
        )+
        labs(
          title = paste0(meta_ind()$nom_ind_complet_unite," par année\nselon le sexe et le territoire (",input$reseau,")"),
          x = "Année",
          y = "Taux (%)",
          subtitle = "",
          #caption = "Source : Ministère de l'éducation"
        )+
        theme(
          plot.title = element_text(size = 16.0,hjust = .5,color = "#007A99FF", lineheight  = 1.1),
          plot.caption = element_text(size = 10, face = "italic"),
          axis.title = element_text(size = 12.5, face = "bold"),
          axis.title.x = element_text(vjust = 0),
          axis.title.y = element_text(vjust = 2),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(face = "italic"),
          legend.text=element_text(size=12),
          legend.title = element_text(size = 12, face = "bold")
        )

    })


    output$evo_temp_choix_gf <- renderPlot({
      ggplot(data = bd_temporel_choix_gf(),aes_string(x = "annee", y = "moving_avg3",color = "nom")) +
        theme_minimal()+
        geom_line()+
        #geom_smooth(size = .99)+
        #stat_spline()+
        geom_point(size = 2.0)+
        #scale_color_manual(values=c("#6C568CFF", "#9386A6FF","#BFCDD9FF","#7F8C72FF", "#607345FF","#6D8325FF","#D6CFB7FF","#E5AD4FFF","#BD5630FF","#735231FF"))+
        #scale_color_manual(values=c("#2CB11BFF","#95C65CFF","#BDDE9BFF","#1BB6AFFF","#0076C0FF","#172869FF"))+
        scale_color_manual(values=c("#FDE725FF", "#7AD151FF", "#22A884FF", "#00A9CCFF","#007A99FF","#0F2B5FFF","#EC5AB7","#F06EF7","#F13453"))+
        guides(color=guide_legend(title="territoire"))+
        scale_x_continuous(
          expand = expansion(mult = c(0.05, 0.05)),
          breaks = seq(annee_min(), annee_max(), by = 1)
        )+
        labs(
          title = paste0(meta_ind()$nom_ind_complet_unite," par année\nselon le territoire (",input$reseau," - ",input$sexe,")"),
          x = "Année",
          y = "Taux (%)",
          subtitle = "",
          #caption = "Source : Ministère de l'éducation"
        )+
        theme(
          plot.title = element_text(size = 16.0,hjust = .5,color = "#007A99FF", lineheight  = 1.1),
          plot.caption = element_text(size = 10, face = "italic"),
          axis.title = element_text(size = 12.5, face = "bold"),
          axis.title.x = element_text(vjust = 0),
          axis.title.y = element_text(vjust = 2),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(face = "italic"),
          legend.text=element_text(size=12),
          legend.title = element_text(size = 12, face = "bold")
        )
    })

    #### Tableau de données ####


    output$titre_tab<-renderText("Tableau de données")
    output$description_tab <- renderUI({HTML(texte_desc_tableau_prim_second$spec_region)})

    # output$description_tab<-renderText(paste("Le tableau suivant présente les données pour toutes les années disponibles de l'indicateur (",meta_ind()$nom_ind_complet_unite,") et permet
    #                                    de sélectionner les dimensions genre et réseau désirées. Le tableau est téléchargeable."))

    bd_table_parsco <- reactive({
      if(input$sexe_2 == "Tous les regroupements" & input$reseau_2 == "Tous les regroupements"){
        pars_sco_region() |>
          mutate(annee = as.numeric(annee)) |>
          filter(annee %in% annees_ind())%>%
          select(annee,nom,sexe,reseau,meta_ind()$indicateurs)%>%
          pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs) |>
          rename(Territoire = nom, Sexe = sexe, `Réseau` = `reseau`)
      }else if(input$sexe_2 != "Tous les regroupements" & input$reseau_2 == "Tous les regroupements"){
        pars_sco_region() |>
          mutate(annee = as.numeric(annee)) |>
          filter(annee %in% annees_ind())%>%
          filter(sexe == input$sexe_2) |>
          select(annee,nom,sexe,reseau,meta_ind()$indicateurs)%>%
          pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs)|>
          rename(Territoire = nom, Sexe = sexe, `Réseau` = `reseau`)
      }else if(input$sexe_2 == "Tous les regroupements" & input$reseau_2 != "Tous les regroupements"){
        pars_sco_region() |>
          mutate(annee = as.numeric(annee)) |>
          filter(annee %in% annees_ind())%>%
          filter(reseau == input$reseau_2) |>
          select(annee,nom,sexe,reseau,meta_ind()$indicateurs)%>%
          pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs)|>
          rename(Territoire = nom, Sexe = sexe, `Réseau` = `reseau`)
      }else{
        pars_sco_region() |>
          mutate(annee = as.numeric(annee)) |>
          filter(annee %in% annees_ind())%>%
          filter(reseau == input$reseau_2) |>
          filter(sexe == input$sexe_2) |>
          select(annee,nom,sexe,reseau,meta_ind()$indicateurs)%>%
          pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs)|>
          rename(Territoire = nom, Sexe = sexe, `Réseau` = `reseau`)
      }

    })

    output$table_parsco_titre<-renderText(paste(meta_ind()$nom_ind_complet_unite,"- Territoire sélectionné : ",req(input$ra)))

    output$table_parsco <- DT::renderDataTable({
      dtable <- datatable(bd_table_parsco(),
                          options = list(pageLength = 10,dom = 'tip', scrollX = TRUE,#autoWidth = TRUE,
                                         fixedColumns = list(leftColumns = 2),
                                         language = list(
                                           info = "Observations _START_ à _END_ sur un total de _TOTAL_ entrées",
                                           paginate = list(previous = 'Précédent', `next` = 'Suivant')
                                           #lengthMenu = "Afficher _MENU_ obs.",search = "Rechercher"
                                         )),
                          #colnames = c('Territoire' = 'nom','Sexe' = 'sexe', 'Réseau' = 'reseau'),
                          extensions = "FixedColumns")|>
        formatRound(
          columns=c(4:ncol(bd_table_parsco())),
          digits = 1,interval = 3, mark = " ", dec.mark = ","
        ) |>
        formatStyle(columns = c(1), width='200px')
      return(dtable)

    })

    info_table <- reactive({
      data.frame(Informations = c(paste0("Indicateur : ",meta_ind()$nom_ind_complet_unite),paste0("Producteur de données : ", meta_ind()$producteur),paste0("Date du téléchargement : ", Sys.Date())))
    })

    bd_table_parsco_2 <- reactive({
      req(input$ind)
      tb_donnee <- bind_rows(info_table(),bd_table_parsco())
    })

    output$download <- downloadHandler(
      filename = function() {
        paste0("table_",meta_ind()$nom_court_1, Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write_xlsx(bd_table_parsco_2(), file)
      })

########  Analyse cohorte #################

    output$titre_cohorte<-renderText("Analyse par cohorte (données du secondaire)")
    output$description_cohorte <- renderUI({HTML(texte_desc_ana_cohorte_prim_second$spec_region)})

    output$meta_citation <- renderUI({HTML(citation_prim_second$spec_region)})

    # output$citer_site <- renderUI({HTML(paste0(#"<p style='font-size :23px;font-weight:bold;'>",
    #   "<p style='font-size :14px;'>",
    #   "Citation suggérée : ÉCOBES — Recherche et transfert. (2024). Parcours scolaires. Dans Regard 360 [plateforme]. https://cartojeunes.shinyapps.io/versionBeta_016/","</p>"))})

    labels_dip_second <- data.frame(v1=c("dip5","dip6_diff","dip7_diff"),
                                  v2=c("Taux diplomation 5 ans","Écart taux diplomation 5 et 6 ans", "Écart taux diplomation 6 et 7 ans"))

    bd_hist_cohorte_mrc <-reactive({
      pars_second_cohorte |>
        filter(cohorte %in% c(2008:2017)) |>
        filter(nom == input$autre_reg, sexe == input$sexe_coh, reseau == input$reseau_coh) |>
        mutate(dip6_diff = dip6 - dip5, dip7_diff = dip7 - dip6) |>
        select(nom,cohorte,sexe,reseau,dip5,dip6_diff,dip7_diff) |>
        pivot_longer(cols=c("dip5","dip6_diff","dip7_diff"),names_to = "type_ind_dip",values_to = "taux_dip") |>
        left_join(labels_dip_second,by = join_by(type_ind_dip == v1)) |>
        select(-type_ind_dip) |> rename(type_ind_dip = v2) |>
        mutate(type_ind_dip = factor(type_ind_dip, levels = c("Écart taux diplomation 6 et 7 ans","Écart taux diplomation 5 et 6 ans","Taux diplomation 5 ans"))) |>
        # Pour obtenir un postionnement du label différent selon la modalité, on calcul la position du label (centre de la barre) pour chaque modalité et on détermine
        # la position du label par rapport à ce centre (0.5,-0.5...)
        group_by(cohorte) |>
        mutate(pos = cumsum(taux_dip) - taux_dip / 2) |> # Cumulatif de la donnée - la moitié pour obtenir le centre
        mutate(
          label_position = ifelse(type_ind_dip == "Écart taux diplomation 6 et 7 ans", cumsum(taux_dip), pos), # Pours dip7_diff, la position est l'extrémité de la barre
          vjust_value = ifelse(type_ind_dip == "Écart taux diplomation 6 et 7 ans", -0.5, 0.5)) # -0.5 pour au-dessus, 0.5 pour centré
    })

    output$hist_cohorte_mrc <- renderPlot({
      ggplot(data = bd_hist_cohorte_mrc(),mapping = aes(x = cohorte, y = taux_dip, fill=type_ind_dip)) +
        geom_bar(stat = "identity", position = "stack")+
        #geom_text(aes(label = format(taux_dip,accuracy=0.1, big.mark = " ", decimal.mark = ",", digits =2)),position = position_stack(vjust = 0.5), size=4.0)+
        geom_text(aes(y = label_position, label = format(taux_dip,accuracy=0.1, big.mark = " ", decimal.mark = ",", digits =2), vjust = vjust_value), color = "black",size=5.0)+
        theme_minimal()+
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
              legend.text = element_text(size = 12),
              legend.title = element_blank())+
        scale_x_continuous(limits = c(2006,2018),breaks = seq(2008, 2017, by = 1))+
        theme(legend.position='top')+
        theme(axis.text.x = element_text(size = 12.0, hjust = 1,vjust = 0.5))+
        theme(axis.text.y = element_text(size = 12.0, hjust = 1,vjust = 0.5))+
        theme(axis.title.x = element_text(size = 12.0),axis.title.y = element_text(size = 12.0))+
        labs(title = paste0("Taux de diplomation au secondaire par cohorte selon la durée :\n ",input$autre_reg," - ",input$sexe_coh," - ",input$reseau_coh),
             x = "Cohorte",y = "",subtitle = "",caption = "")+
        theme(plot.title = element_text(size = 15.0,hjust = .5,color = "#007A99FF", lineheight  = 1.1)) #family = "Courier"
    })

    labels_ind_acces <- data.frame(v1=c("fp7","fga7","coll7"),
                                    v2=c( "Formation professionnelle","Formation générale des adultes",
                                         "Formation collégiale"))

    bd_graph_cohorte_mrc <-reactive({
      pars_second_cohorte |>
        filter(cohorte %in% c(2008:2017)) |>
        filter(nom == input$autre_reg, sexe == input$sexe_coh, reseau == input$reseau_coh) |>
        select(nom,cohorte,fp7,fga7,coll7) |>
        pivot_longer(cols=c("fp7","fga7","coll7"),names_to = "ind_acces",values_to = "taux_acces") |>
        left_join(labels_ind_acces,by = join_by(ind_acces == v1)) |>
        select(-ind_acces) |> rename(ind_acces = v2)
      #mutate(type_data_dec = factor(type_data_dec, levels = c("dec_dur_pre_diff", "dec_dur_pre")))
    })

    output$evo_cohorte_mrc <- renderPlot({
      ggplot(data = bd_graph_cohorte_mrc(),aes(x = cohorte, y = taux_acces, color = ind_acces)) +
        theme_minimal()+
        theme(legend.position='top',legend.text = element_text(size = 12),legend.title = element_blank())+
        geom_line(size = .99)+
        geom_point(size = 2.0)+
        scale_x_continuous(limits = c(2008,2017),breaks = seq(2008, 2017, by = 1))+
        theme(axis.text.x = element_text(size = 12.0, hjust = 1,vjust = 0.5))+
        theme(axis.text.y = element_text(size = 12.0, hjust = 1,vjust = 0.5))+
        theme(axis.title.x = element_text(size = 12.0),axis.title.y = element_text(size = 12.0))+
        labs(title = paste0("Taux d'accès aux formations après 7 ans au secondaire par cohorte :\n ",input$autre_reg," - ",input$sexe_coh," - ",input$reseau_coh) ,
             x = "Cohorte",y = "Taux d'accès",subtitle = "",caption = "")+
        theme(plot.title = element_text(size = 15.0,hjust = .5,color = "#007A99FF", lineheight  = 1.1))
    })

    labels_ind_second <- data.frame(v1=c("dip5","dip6","dip7","fp7","fga7","coll7"),
                                  v2=c("Diplomation et qualification après 5 ans","Diplomation et qualification après 6 ans","Diplomation et qualification après 7 ans",
                                       "Accès à la formation professionnelle après 7 ans","Accès à la formation générale des adultes après 7 ans",
                                       "Accès à la formation collégiale après 7 ans"))

    bd_table_cohorte <-reactive({
      pars_second_cohorte |>
        filter(cohorte %in% c(2008:2017)) |>
        filter(nom == input$autre_reg) |>
        select(nom,cohorte,sexe,reseau,dip5,dip6,dip7,fp7,fga7,coll7) |>
        pivot_longer(cols=c("dip5","dip6","dip7","fp7","fga7","coll7"),names_to = "ind_second",values_to = "taux_ind_second") |>
        left_join(labels_ind_second,by = join_by(ind_second == v1)) |>
        pivot_wider(names_from = cohorte, values_from = taux_ind_second) |>
        select(-ind_second) |> rename(Indicateur = v2) |>
        filter(sexe == input$sexe_coh, reseau == input$reseau_coh) |>
        select(-sexe,-reseau,-nom) |>
        mutate_if(is.numeric,round,digits = 1)

    })

    # bd_table_cohorte <-reactive({
    #   bd_table_cohorte_down() |>
    #     filter(sexe == input$sexe_coh, reseau == input$reseau_coh) |>
    #     select(-sexe,-reseau,-nom) |>
    #     mutate_if(is.numeric,round,digits = 1)
    # })

    output$table_cohorte_titre<-renderText(paste("Indicateurs de diplomation et d'accès par cohorte : ",req(input$autre_reg)," - ",req(input$reseau_coh),"-",req(input$sexe_coh)))

    output$table_cohorte <- DT::renderDataTable({
      dtable <- datatable(bd_table_cohorte(),
                          options = list(pageLength = 10,dom = 'tip', scrollX = TRUE,#autoWidth = TRUE,
                                         fixedColumns = list(leftColumns = 2),
                                         language = list(
                                           info = "Observations _START_ à _END_ sur un total de _TOTAL_ entrées",
                                           paginate = list(previous = 'Précédent', `next` = 'Suivant')
                                           #lengthMenu = "Afficher _MENU_ obs.",search = "Rechercher"
                                         )),
                           extensions = "FixedColumns") |>
        formatRound(
          columns=c(2:ncol(bd_table_cohorte())),
          digits = 1,interval = 3, mark = " ", dec.mark = ","
        )
      return(dtable)
    })

    bd_table_cohorte_down_1 <-reactive({
      pars_second_cohorte |>
        filter(cohorte %in% c(1998:2017)) |>
        select(nom,cohorte,sexe,reseau,dip5,dip6,dip7,fp7,fga7,coll7) |>
        pivot_longer(cols=c("dip5","dip6","dip7","fp7","fga7","coll7"),names_to = "ind_second",values_to = "taux_ind_second") |>
        left_join(labels_ind_second,by = join_by(ind_second == v1)) |>
        pivot_wider(names_from = cohorte, values_from = taux_ind_second) |>
        select(-ind_second) |> rename(Indicateur = v2)
    })

    bd_table_cohorte_down <- reactive({
        if(input$autre_reg == "Estrie"){bd_table_cohorte_down_1() |> filter(nom %in% c("Estrie","Estrie (ancienne)"))
        }else if(input$autre_reg == "Montérégie"){bd_table_cohorte_down_1() |> filter(nom %in% c("Montérégie","Montérégie (ancienne)"))
            }else(bd_table_cohorte_down_1() |> filter(nom == input$autre_reg))
    })

    info_cohorte <- data.frame(Informations = c("L'année correspond à l'année de la cohorte","Producteur de données : Ministère de l'Éducation",paste0("Date du téléchargement : ", Sys.Date())))

    bd_table_cohorte_down_3 <- reactive({
      req(input$ind)
      tb_donnee <- bind_rows(info_cohorte,bd_table_cohorte_down())
    })

    bd_table_cohorte_down_4 <-reactive({
      bd_table_cohorte_down_3() |>
        select(Informations, Territoire = nom, Indicateur,Sexe = sexe,`Réseau` = reseau,order(colnames(bd_table_cohorte_down_3()), decreasing = FALSE))
    })

    output$down_cohorte <- downloadHandler(
      filename = function() {
        paste0("bd_table_cohorte_second", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write_xlsx(bd_table_cohorte_down_4(), file)
      })


#################

  }
  )}
