
load(file = "pars_coll_bd_cohorte_2026-06_v1.rdata")
load(file = "Metadata_postsecondaire_2026_06_v1.rdata")
#load(file = "pars_coll_bd_cohorte_2025-02_v1.rdata")
#load(file = "Metadata_postsecondaire_2025_02_v1.rdata")
load(file = "Metadata_region_1oct2025.rdata")

#-----------------------------------------------------------

bd_coll_all_digits <- pars_coll_bd_cohorte %>%
  mutate(annee = as.numeric(str_sub(cle_geo_annee,-4,-1))) |>
  mutate(annee_num = as.numeric(annee))%>%
  filter(decoupage %in% c("ra","prov")) |>
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
            digits = 4) |>
  mutate(code_geo = as.numeric(code_geo)) |>
  mutate(dec_dur_pre_diff = dec_dur_pre_2 - dec_dur_pre)

bd_coll <- bd_coll_all_digits |>
  mutate_if(is.numeric,
            round,
            digits = 1)

##############################################################

shp_ra_simp <- sf::read_sf("shp_ra_simp.shp")

#############################################################

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

############################################################################################

alpha_reg <-c("Ensemble du Québec","Abitibi-Témiscamingue","Bas-Saint-Laurent","Capitale-Nationale","Centre-du-Québec","Chaudière-Appalaches",
              "Côte-Nord","Estrie","Gaspésie–Îles-de-la-Madeleine","Lanaudière","Laurentides","Laval","Mauricie","Montérégie","Montréal",
              "Nord-du-Québec","Outaouais","Saguenay–Lac-Saint-Jean")
alpha_reg <- factor(alpha_reg,levels=alpha_reg)

ordre_ra <-c("Ensemble du Québec","Abitibi-Témiscamingue","Bas-Saint-Laurent","Capitale-Nationale","Centre-du-Québec","Chaudière-Appalaches",
             "Côte-Nord","Estrie","Gaspésie–Îles-de-la-Madeleine","Lanaudière","Laurentides","Laval","Mauricie","Montérégie","Montréal",
             "Nord-du-Québec","Outaouais","Saguenay–Lac-Saint-Jean")
ordre_ra <- factor(ordre_ra,levels=ordre_ra)

ordre_ra_seul <-c("Abitibi-Témiscamingue","Bas-Saint-Laurent","Capitale-Nationale","Centre-du-Québec","Chaudière-Appalaches",
                  "Côte-Nord","Estrie","Gaspésie–Îles-de-la-Madeleine","Lanaudière","Laurentides","Laval","Mauricie","Montérégie","Montréal",
                  "Nord-du-Québec","Outaouais","Saguenay–Lac-Saint-Jean")
ordre_ra_seul <- factor(ordre_ra,levels=ordre_ra)

texte_desc_coll <- filter(metadata_region, spec_region_nom == "desc_coll")
texte_coll_territoire <- filter(metadata_region, spec_region_nom == "coll_territoire")
texte_evo_temp_coll <- filter(metadata_region, spec_region_nom == "evo_temp_coll")
texte_desc_ana_cohorte_coll <- filter(metadata_region, spec_region_nom == "desc_ana_cohorte_coll")
texte_metho_carte <- filter(metadata_region, spec_region_nom == "metho_carte")
texte_metho_graph <- filter(metadata_region, spec_region_nom == "metho_graph")
texte_metho_tableau <- filter(metadata_region, spec_region_nom == "metho_tableau")
citation_coll <- filter(metadata_region, spec_region_nom == "citation_coll")
texte_desc_tableau_coll <- filter(metadata_region, spec_region_nom == "desc_tableau_coll")

############################################################################################


UI_titre_postsecond <- function(id){
  ns <- NS(id)
  fluidRow(column(6,textOutput(NS(id,"titre")),
                  tags$head(tags$style(paste0("#",NS(id,"titre"),"{color: #0F2B5FFF;font-size: 24px;font-style: bold;}"))))
  )
}

UI_titre_def_postsecond <- function(id){
  ns <- NS(id)
  fluidRow(column(11,htmlOutput(NS(id,"titre_def"))),
           column(3,offset = 7,align="center",
                  actionButton(NS(id,"hide_coll_territoire"), "Particularité territoriale",icon("pen-nib"),
                               style="color: #fff; background-color: #36B0C9; border-color: #2e6da4; padding:5px; font-size:85%")))
}

UI_bouton_territoire <- function(id){
  ns <- NS(id)
  fluidRow(column(9,offset = 1,hidden(htmlOutput(NS(id,"coll_territoire")))))
}

UI_1_postsecond <- function(id){
  ns <- NS(id)
  fluidRow(
    #column(2,offset = 3,align="center",selectInput(NS(id,"ra"), "Région", choices = ordre_ra)),
    #column(3,align="center",selectInput(NS(id,"ind"), "Indicateurs", choices = unique_no.NA(carac_sco$nom_ind_court),width="550px")),
    column(3,offset = 3,selectInput(NS(id,"ind"), "Indicateur", choices = unique_no.NA(metadata_postsecondaire$nom_ind_court),
                                    selected = "Obtention d’un DEC – 2 ans après la durée prévue  (%)",width="550px")),
    column(2,selectInput(NS(id,"type_form"), "Secteur d'études", choices = unique_no.NA(bd_coll$type_form_nom), selected = "Secteurs d'études réunis"))


  )
}

UI_titredef_ind_postsecond <- function(id){
  ns <- NS(id)
  fluidRow(column(10,offset =1,textOutput(NS(id,"nom_indicateur")),
                  tags$head(tags$style(paste0("#",NS(id,"nom_indicateur"),"{color: #0F2B5FFF;font-size: 22px;font-style: bold;}"))))
  )
}

UI_def_ind_postsecond <- function(id){
  ns <- NS(id)
  fluidRow(column(11,offset=1,htmlOutput(NS(id,"definition_ind"))))
}

UI_titre_postsecond_section1 <- function(id){
  ns <- NS(id)
  fluidRow(column(6,offset = 1,textOutput(NS(id,"titre_section1")),
                  tags$head(tags$style(paste0("#",NS(id,"titre_section1"),"{color: #0F2B5FFF;font-size: 24px;font-style: bold;}"))))
  )
}



UI_1_1_postsecond <- function(id){
  ns <- NS(id)
  fluidRow(
    column(1,offset = 4,align="center",selectInput(NS(id,"ann"), "Cohorte", choices = NULL)),
    column(3,selectInput(NS(id,"type_ana"), "Type d'analyse",
                         choices = c("Comparaison des données selon le territoire","Comparaison des données selon le sexe et le territoire"),width="550px"))
    )
}


UI_2b_postsecond <- function(id){
  ns <- NS(id)
  fluidRow(
    conditionalPanel(
      condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le territoire'"),
      column(12,align="center",textOutput(NS(id,"map_gf_caption2")),
             tags$head(tags$style(paste0("#",NS(id,"map_gf_caption2"),"{color: #007A99FF;font-size: 16px;font-style: bold;}"))))
    ),
    conditionalPanel(
      condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le sexe et le territoire'"),
      column(6,align="center",textOutput(NS(id,"map_f_caption2")),
             tags$head(tags$style(paste0("#",NS(id,"map_f_caption2"),"{color: #007A99FF;font-size: 16px;font-style: bold;}")))),
      column(6,align="center",textOutput(NS(id,"map_g_caption2")),
             tags$head(tags$style(paste0("#",NS(id,"map_g_caption2"),"{color: #007A99FF;font-size: 16px;font-style: bold;}"))))
    )
  )
}

UI_2c_postsecond <- function(id){
  ns <- NS(id)
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
  )
}

UI_3_postsecond <- function(id){
  ns <- NS(id)
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
  )
}
UI_3_coll_action_carte <- function(id){
  ns <- NS(id)
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
  )
}

UI_3_coll_action_carte_hidden <- function(id){
  ns <- NS(id)
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
  )
}

########  Histogramme #############

UI_4ta_postsecond <- function(id){
  ns <- NS(id)
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
  )
}

UI_4tb_postsecond <- function(id){
  ns <- NS(id)
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
  )
}

UI_4_postsecond <- function(id){
  ns <- NS(id)
  fluidRow(
    conditionalPanel(
      condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le territoire'"),
      column(6,offset = 3,plotOutput(NS(id,"hist_mrc_gf"),width = "100%", height = 500))
    ),
    conditionalPanel(
      condition = paste0("input['",ns("type_ana"),"'] == 'Comparaison des données selon le sexe et le territoire'"),
      column(6,plotOutput(NS(id,"hist_mrc_f"),width = "100%", height = 500)),
      column(6,plotOutput(NS(id,"hist_mrc_g"),width = "100%", height = 500))
    )
  )
}

UI_4_coll_action_graph <- function(id){
  ns <- NS(id)
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
  )
}

UI_4_coll_action_graph_hidden <- function(id){
  ns <- NS(id)
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
  )
}

#### Évolution temporelle #########################

UI_5t_postsecond <- function(id){
  ns <- NS(id)
  fluidRow(column(6,offset = 1,align="left",textOutput(NS(id,"titre_evo")),
                  tags$head(tags$style(paste0("#",NS(id,"titre_evo"),"{color: #0F2B5FFF;font-size: 24px;font-style: bold;}"))))
  )
}

UI_5desc_postsecond <- function(id){
  ns <- NS(id)
  fluidRow(column(10,offset = 1,align="left",htmlOutput(NS(id,"description_evo"))))
}


UI_5pick_postsecond <- function(id){
  fluidRow(
    column(3,offset =1,align="center",pickerInput(NS(id,"nom_evo_temp_2"),"Sélectionner les Territoires à comparer (maximum 2)",selected = NULL, choices = NULL,
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
    column(7,plotOutput(NS(id,"evo_temp_region_sexe")))

  )
}


UI_6pick_postsecond <- function(id){
  fluidRow(
    column(3,offset =1,align="center",pickerInput(NS(id,"nom_evo_temp_10"),"Sélectionner les Territoires à comparer (maximum 9)",selected = NULL, choices = NULL,


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
           selectInput(NS(id,"genre"), "Sexe", choices = unique_no.NA(bd_coll$sexe_coll))
    ),
    #column(1,selectInput(NS(id,"sexe"), "Sexe", choices = unique_no.NA(grande_bd$sexe))),
    column(7,plotOutput(NS(id,"evo_temp_choix_gf")))
  )
}

### Tableau de données #########################

UI_7t_postsecond <- function(id){
  ns <- NS(id)
  fluidRow(column(6,offset = 1,align="left",textOutput(NS(id,"titre_tab")),
                  tags$head(tags$style(paste0("#",NS(id,"titre_tab"),"{color: #0F2B5FFF;font-size: 24px;font-style: bold;}"))))
  )
}

UI_7desc_postsecond <- function(id){
  ns <- NS(id)
  # fluidRow(column(10,offset = 1,align="left",textOutput(NS(id,"description_tab")),
  #                 #tags$head(tags$style(paste0("#",NS(id,"description_tab"),"{color: black ;font-size: 15px;font-style: bold;}"))))
  #                 tags$head(tags$style(paste0("#",NS(id,"description_tab"),"{font-size: 16px;font-style: bold;}"))))
  # )
  fluidRow(column(10,offset = 1,align="left",htmlOutput(NS(id,"description_tab"))))
}

UI_7a_postsecond <- function(id){
  fluidRow(
    column(2,align="center",offset = 4,selectInput(NS(id,"genre_2"), "Sexe", choices = c("Tous les regroupements","Féminin","Masculin","Sexes réunis"))),
    column(2,align="center",selectInput(NS(id,"type_form_2"), "Secteurs d'études", choices = c("Tous les regroupements","Préuniversitaire","Technique",
                                                                                               "Tremplin DEC","Secteurs d'études réunis")))
  )
}

Ui_7_table_coll_titre <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12,align="center",textOutput(NS(id,"table_coll_titre")),
           tags$head(tags$style(paste0("#",NS(id,"table_coll_titre"),"{color: #007A99FF;font-size: 16px;font-style: bold;}")))))
}

UI_7_postsecond <- function(id){
  fluidRow(
    column(12,DT::dataTableOutput(NS(id,"table_coll")))
  )
}

UI_7d_postsecond <- function(id){
  fluidRow(column(3,offset = 3, align="left",
                  actionButton(NS(id,"hide_meta_tableau"), "Précisions sur le tableau",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                               style="color: #fff;background-color: #BDBFBF;padding:5px; font-size:85%")),
           column(3,align="right",downloadButton(NS(id,"download"), "Téléchargement du tableau",style="color: #fff; background-color: #3C65CF")))
}

UI_7e_hide_meta_tableau <- function(id){
  fluidRow(column(6,offset = 1,hidden(htmlOutput(NS(id,"meta_tableau")))))
}

citation <- function(id){
  fluidRow(
    column(10,offset = 1, htmlOutput(NS(id,"citer_site")))
  )
}

## Indicateurs par cohorte

Ui_titre_cohorte<- function(id){
  fluidRow(column(6,offset = 1,align="left",textOutput(NS(id,"titre_cohorte")),
                  tags$head(tags$style(paste0("#",NS(id,"titre_cohorte"),"{color: #0F2B5FFF;font-size: 24px;font-style: bold;}")))))}

Ui_description_cohorte<- function(id){
  fluidRow(column(9,offset = 1,align="left",htmlOutput(NS(id,"desc_cohorte"))))
}


Ui_menu_cohorte <- function(id){
  ns <- NS(id)
  fluidRow(
    #column(2,offset=3,align="center",selectInput(NS(id,"region_3"), "Région", choices = unique_no.NA(bd_coll$nom), selected = "Ensemble du Québec")),
    column(2,offset=3,align="center",selectInput(NS(id,"region_3"), "Territoire", choices = ordre_ra, selected = "Ensemble du Québec")),
    column(2,align="center",selectInput(NS(id,"type_form_3"), "Secteur d'études", choices = unique_no.NA(bd_coll$type_form_nom), selected = "Secteurs d'études réunis")),
    column(2,align="center",selectInput(NS(id,"sexe_3"), "Sexe", choices = unique_no.NA(bd_coll$sexe_coll), selected = "Sexes réunis")))
}

Ui_graphique_cohorte <- function(id){
  ns <- NS(id)
  fluidRow(column(6,plotOutput(NS(id,"hist_cohorte"),width = "100%", height = 500)),
           column(6,plotOutput(NS(id,"evo_cohorte"),width = "100%", height = 500)))
}

Ui_table_cohorte_titre <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12,align="center",textOutput(NS(id,"table_cohorte_titre")),
           tags$head(tags$style(paste0("#",NS(id,"table_cohorte_titre"),"{color: #007A99FF;font-size: 16px;font-style: bold;}")))))
}


Ui_table_cohorte <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12,DT::dataTableOutput(NS(id,"table_cohorte"))))
}

UI_down_table_cohorte <- function(id){
  fluidRow(column(3,offset = 3, align="left",
                  actionButton(NS(id,"hide_meta_tableau2"), "Précisions sur le tableau",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                               style="color: #fff;background-color: #BDBFBF;padding:5px; font-size:85%")),
           column(3,offset = 9,downloadButton(NS(id,"down_cohorte"), "Téléchargement du tableau",style="color: #fff; background-color: #3C65CF")))
}

UI_down_hide_meta_tableau <- function(id){
  fluidRow(column(6,offset = 1,hidden(htmlOutput(NS(id,"meta_tableau2")))))
}

UI_action_citation <- function(id){
  fluidRow(column(3,offset = 1,
                  actionButton(NS(id,"hide_meta_citation"), "Référence suggérée",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                               style="color: #fff; background-color: #36B0C9; border-color: #2e6da4;")))

}

UI_citation <- function(id){
  fluidRow(column(10,offset = 1,hidden(htmlOutput(NS(id,"meta_citation")))))
}



##################################################################################################
##################################################################################################
##################################################################################################



server_coll <- function(id){
  moduleServer(id, function(input,output,session){


    output$titre<-renderText("Parcours collégial")

    output$titre_def <- renderUI({HTML(texte_desc_coll$spec_region)})

    output$coll_territoire <- renderUI({HTML(texte_coll_territoire$spec_region)})

    output$nom_indicateur <- renderText(paste("Indicateur : ", meta_ind()$nom_ind_complet_unite))
    output$definition_ind <- renderUI({HTML(meta_ind()$def_ind)})

    output$titre_section1<-renderText("Visualisations et comparaisons")

    meta_ind <-reactive({
      filter(metadata_postsecondaire, nom_ind_court == input$ind)
    })


    annees_ind <- reactive({
      req(input$ind)
      c(meta_ind()$an1,meta_ind()$an2,meta_ind()$an3,meta_ind()$an4,meta_ind()$an5,
        meta_ind()$an6,meta_ind()$an7,meta_ind()$an8,meta_ind()$an9,meta_ind()$an10,
        meta_ind()$an11,meta_ind()$an12,meta_ind()$an13)
    })

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

    # Pour les actions buttons
    observeEvent(input$hide_coll_territoire,{
      toggle("coll_territoire")
    })

    observeEvent(input$hide_meta_carte,{toggle("meta_carte")})
    observeEvent(input$hide_meta_carte2,{toggle("meta_carte2")})
    observeEvent(input$hide_meta_carte3,{toggle("meta_carte3")})

    observeEvent(input$hide_meta_hist,{toggle("meta_hist")})
    observeEvent(input$hide_meta_hist2,{toggle("meta_hist2")})
    observeEvent(input$hide_meta_hist3,{toggle("meta_hist3")})

    observeEvent(input$hide_meta_tableau,{toggle("meta_tableau")})
    observeEvent(input$hide_meta_tableau2,{toggle("meta_tableau2")})

    observeEvent(input$hide_meta_citation,{toggle("meta_citation")})


    bd_ra <- reactive({
      if(meta_ind()$unite == "(nb)"){
        bd_coll %>%
          filter(annee_num == input$ann,
                 decoupage == "ra",
                 type_form_nom == input$type_form) |>
          select(annee,nom,code_geo,sexe_coll,meta_ind()$indicateurs,color_ecobes)
      }
      else{
        bd_coll %>%
          filter(annee_num == input$ann)%>%
          filter(decoupage == "ra" | decoupage == "prov") %>%
          filter(type_form_nom == input$type_form)%>%
          select(annee,nom,code_geo,sexe_coll,meta_ind()$indicateurs,color_ecobes)
      }
    })


    bd_map_mrc_gf <- reactive({
      shp_ra_simp |>
        left_join(bd_ra(),by = c("code_geo" = "code_geo")) |>
        filter(sexe_coll == "Sexes réunis") |>
        filter(!is.na(nom))
    })


    bd_map_mrc_f <- reactive({
      shp_ra_simp |>
        left_join(bd_ra(),by = c("code_geo" = "code_geo")) |>
        filter(sexe_coll == "Féminin") |>
        filter(!is.na(nom))
    })


    bd_map_mrc_g <- reactive({
      shp_ra_simp |>
        left_join(bd_ra(),by = c("code_geo" = "code_geo")) |>
        filter(sexe_coll == "Masculin") |>
        filter(!is.na(nom))
    })

    # bd_interval_gf <- reactive({
    #   if(input$type_ana == "Comparaison des données selon le territoire")
    #   {bd_coll %>%
    #       filter(annee_num == input$ann,
    #              decoupage %in% c("ra"),
    #              sexe_coll %in% c("Sexes réunis"),
    #              type_form_nom == input$type_form) |>
    #       select(annee,nom,code_geo,sexe_coll,meta_ind()$indicateurs,color_ecobes)}
    #   else
    #   {bd_coll %>%
    #       filter(annee_num == input$ann,
    #              decoupage %in% c("ra"),
    #              type_form_nom == input$type_form,
    #              sexe_coll %in% c("Masculin","Féminin")) |>
    #       select(annee,nom,code_geo,sexe_coll,meta_ind()$indicateurs,color_ecobes)}
    #
    # })

    bd_map_g_plus_f <- reactive({bind_rows(bd_map_mrc_g(),bd_map_mrc_f())})

    bd_interval_gf <- reactive({
      if(input$type_ana == "Comparaison des données selon le territoire"){bd_map_mrc_gf()}
      else{bd_map_g_plus_f()}
    })

    #### Map serveur ####

    binpal <- reactive({
      if(all(is.na(bd_interval_gf()[[meta_ind()$indicateurs]]))){colorBin("YlGn", bd_interval_gf()$dip7, 6, pretty = FALSE)}
      else if(sum(complete.cases(bd_interval_gf()[[meta_ind()$indicateurs]])) == 1){colorBin("YlGn", c(0:500), 2, pretty = FALSE)}
      else if(input$ind == "Proportion de nouvelles inscriptions (%)" & input$type_form == "Secteurs d'études réunis"){colorBin("YlGn", c(0:100), 2, pretty = FALSE)}
      else{colorBin("YlGn", bd_interval_gf()[[meta_ind()$indicateurs]], 6, pretty = FALSE)}
    })

    #output$map_gf_caption1<-renderText(paste("Région :", req(input$ra)))
    output$map_gf_caption2<-renderText(paste(meta_ind()$nom_ind_complet_unite))
    output$map_gf_caption3<-renderText(paste("Sexes réunis"," - ", req(input$type_form)," - Cohorte ",req(input$ann)))
    #output$map_f_caption1<-renderText(paste("Région :", req(input$ra)))
    output$map_f_caption2<-renderText(paste(meta_ind()$nom_ind_complet_unite))
    output$map_f_caption3<-renderText(paste("Féminin"," - ", req(input$type_form)," - Cohorte ",req(input$ann)))
    #output$map_g_caption1<-renderText(paste("Région :", req(input$ra)))
    output$map_g_caption2<-renderText(paste(meta_ind()$nom_ind_complet_unite))
    output$map_g_caption3<-renderText(paste("Masculin"," - ", req(input$type_form)," - Cohorte ",req(input$ann)))

    output$map_gf <- renderLeaflet({
      req(input$ind)
      if(nrow(bd_map_mrc_gf())>2){
        m <- leaflet(bd_map_mrc_gf(),options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1, wheelPxPerZoomLevel = 120)) |>
          #addProviderTiles("CartoDB.Positron") |> # Ancien fond qui demande maintenant une clé API
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
          #addProviderTiles("CartoDB.Positron") |>
          addProviderTiles("Esri.WorldGrayCanvas") |>
          addProviderTiles("CartoDB.VoyagerOnlyLabels") |>
          addPolygons(fillColor = "green",weight = 1,color = "#444444",fillOpacity = 0.4,
                      popup=paste0(bd_map_mrc_gf()$nom," - ",lfrench(bd_map_mrc_gf()[[meta_ind()$indicateurs]])),
                      label=~paste0(bd_map_mrc_gf()$nom," - ",lfrench(bd_map_mrc_gf()[[meta_ind()$indicateurs]]))
          )
      }
    })



    output$map_g <- renderLeaflet({
      req(input$ind)
      if(nrow(bd_map_mrc_g())>2& input$type_ana == "Comparaison des données selon le sexe et le territoire"){
        m <- leaflet(bd_map_mrc_g(),options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1, wheelPxPerZoomLevel = 120)) |>
          #addProviderTiles("CartoDB.Positron") |> # Ancien fond qui demande maintenant une clé API
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
          #addProviderTiles("CartoDB.Positron") |> # Ancien fond qui demande maintenant une clé API
          addProviderTiles("Esri.WorldGrayCanvas") |>
          addProviderTiles("CartoDB.VoyagerOnlyLabels") |>
          addPolygons(fillColor = "green",weight = 1,color = "#444444",fillOpacity = 0.4,
                      popup=paste0(bd_map_mrc_g()$nom," - ",lfrench(bd_map_mrc_g()[[meta_ind()$indicateurs]])),
                      label=~paste0(bd_map_mrc_g()$nom," - ",lfrench(bd_map_mrc_g()[[meta_ind()$indicateurs]]))
          )
      }
    })



    output$map_f <- renderLeaflet({
      req(input$ind)
      if(nrow(bd_map_mrc_f())>2& input$type_ana == "Comparaison des données selon le sexe et le territoire"){
        m <- leaflet(bd_map_mrc_f(),options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1, wheelPxPerZoomLevel = 120)) |>
          #addProviderTiles("CartoDB.Positron") |> # Ancien fond qui demande maintenant une clé API
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
          #addProviderTiles("CartoDB.Positron") |> # Ancien fond qui demande maintenant une clé API
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

    #### Diagramme à barres ####

    output$hist_gf_caption2<-renderText(paste(meta_ind()$nom_ind_complet_unite))
    output$hist_gf_caption3<-renderText(paste("Sexes réunis"," - ",req(input$type_form)," - Cohorte ",req(input$ann)))

    output$hist_f_caption2<-renderText(paste(meta_ind()$nom_ind_complet_unite))
    output$hist_f_caption3<-renderText(paste("Féminin"," - ",req(input$type_form)," - Cohorte",req(input$ann)))

    output$hist_g_caption2<-renderText(paste(meta_ind()$nom_ind_complet_unite))
    output$hist_g_caption3<-renderText(paste("Masculin"," - ",req(input$type_form)," - Cohorte",req(input$ann)))


    bd_ra_hist <- reactive({
      if(meta_ind()$unite == "(nb)"){
        bd_coll %>%
          filter(annee_num == input$ann,
                 decoupage == "ra",
                 type_form_nom == input$type_form) |>
          select(annee,nom,code_geo,sexe_coll,meta_ind()$indicateurs,color_ecobes)
      }
      else{
        bd_coll %>%
          filter(annee_num == input$ann)%>%
          filter(decoupage == "ra" | decoupage == "prov") %>%
          filter(type_form_nom == input$type_form)%>%
          select(annee,nom,code_geo,sexe_coll,meta_ind()$indicateurs,color_ecobes)
      }
    })

    bd_hist_mrc_gf <- reactive({
      bd_ra_hist() |> filter(sexe_coll == "Sexes réunis")
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


    bd_hist_mrc_f <- reactive({
      bd_ra_hist() |> filter(sexe_coll == "Féminin")
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

    bd_hist_mrc_g <- reactive({
      bd_ra_hist() |> filter(sexe_coll == "Masculin")
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

    output$description_evo <- renderUI({HTML(texte_evo_temp_coll$spec_region)})

    bd_coll_region <-reactive({
      bd_coll |>
        filter(decoupage == "ra" | decoupage == "prov")
    })

    observeEvent(bd_coll_region(),{
      #choices <- unique(bd_coll_region()$nom)
      choices <- alpha_reg
      freezeReactiveValue(input, NS(id,"ra"))
      updatePickerInput(session,"nom_evo_temp_2", choices = choices)
      updatePickerInput(session,"nom_evo_temp_10", choices = choices)
    })

    bd_temporel_region_sexe <- reactive({bd_coll %>%
        filter(nom %in% input$nom_evo_temp_2) %>%
        filter(type_form_nom == input$type_form)%>%
        mutate(annee = as.numeric(annee)) |>
        filter(annee %in% annees_ind()) |>
        select(annee,nom,ra_app_nom,sexe_coll,meta_ind()$indicateurs,color_ecobes,line_comp) |>
        group_by(nom,sexe_coll) |>
        #mutate(moving_avg3 = rollmean(.data[[meta_ind()$indicateurs]],k=3, fill=NA,align = "right"))
        mutate(moving_avg3 = (.data[[meta_ind()$indicateurs]]))

    })

    bd_temporel_choix_gf <- reactive({bd_coll %>%
        filter(nom %in% input$nom_evo_temp_10) %>%
        filter(type_form_nom == input$type_form)%>%
        filter(sexe_coll == input$genre)%>%
        mutate(annee = as.numeric(annee)) |>
        filter(annee %in% annees_ind()) |>
        select(annee,nom,ra_app_nom,meta_ind()$indicateurs,color_ecobes) |>
        group_by(nom) |>
        #mutate(moving_avg3 = rollmean(.data[[meta_ind()$indicateurs]],k=3, fill=NA,align = "right"))
        mutate(moving_avg3 = (.data[[meta_ind()$indicateurs]]))
    })

    # https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/

    output$evo_temp_region_sexe <- renderPlot({
      ggplot(data = bd_temporel_region_sexe(),aes_string(x = "annee", y = "moving_avg3",color = "sexe_coll", linetype = "nom")) +
        theme_minimal()+
        geom_line(size = .99)+
        #geom_line(linewidth = .99)+
        geom_point(size = 2.0)+
        scale_color_manual(values=c("#00A9CCFF", "#7AD151FF", "#22A884FF"))+
        #scale_color_manual(values= gold_purple)+
        guides(color=guide_legend(title="Sexe"))+
        guides(linetype=guide_legend(title="Territoire"))+
        scale_x_continuous(
          expand = c(0, 0),
          limits = c(2009, 2023),
          breaks = seq(2010, 2022, by = 2)
        )+
        labs(
          title = paste0(meta_ind()$nom_ind_complet_unite," par cohorte\nselon le sexe et le territoire(",input$type_form,")"),
          x = "Cohorte",
          y = "",
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
        geom_line(size = .99)+
        #geom_line(linewidth = .99)+
        geom_point(size = 2.0)+
        #scale_color_manual(values=c("#6C568CFF", "#9386A6FF","#BFCDD9FF","#7F8C72FF", "#607345FF","#6D8325FF","#D6CFB7FF","#E5AD4FFF","#BD5630FF","#735231FF"))+
        #scale_color_manual(values=c("#2CB11BFF","#95C65CFF","#BDDE9BFF","#1BB6AFFF","#0076C0FF","#172869FF"))+
        scale_color_manual(values=c("#FDE725FF", "#7AD151FF", "#22A884FF", "#00A9CCFF","#007A99FF","#0F2B5FFF","#EC5AB7","#F06EF7","#F13453"))+
        guides(color=guide_legend(title="Territoire"))+
        scale_x_continuous(
          expand = c(0, 0),
          limits = c(2009, 2023),
          breaks = seq(2010, 2022, by = 2)
        )+
        labs(
          title = paste0(meta_ind()$nom_ind_complet_unite," par cohorte\nselon le territoire (",input$type_form," - ",input$genre,")"),
          x = "Cohorte",
          y = "",
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

    output$description_tab <- renderUI({HTML(texte_desc_tableau_coll$spec_region)})

    bd_table_coll_all_digits <- reactive({
      if(input$genre_2 == "Tous les regroupements" & input$type_form_2 == "Tous les regroupements"){
        bd_coll_all_digits |>
          filter(decoupage == "ra" | decoupage == "prov") |>
          mutate(annee = as.numeric(annee)) |>
          filter(annee %in% annees_ind())%>%
          rename(`Sexe` = `sexe_coll`,`Type de formation`=`type_form_nom`,`Territoire`=`nom`) |>
          select(annee,`Territoire`,`Sexe`,`Type de formation`,meta_ind()$indicateurs)%>%
          pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs)
      }else if(input$genre_2 != "Tous les regroupements" & input$type_form_2 == "Tous les regroupements"){
        bd_coll_all_digits |>
          filter(decoupage == "ra" | decoupage == "prov") |>
          mutate(annee = as.numeric(annee)) |>
          filter(annee %in% annees_ind())%>%
          filter(sexe_coll == input$genre_2) |>
          rename(`Sexe` = `sexe_coll`,`Type de formation`=`type_form_nom`,`Territoire`=`nom`) |>
          select(annee,`Territoire`,`Sexe`,`Type de formation`,meta_ind()$indicateurs)%>%
          pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs)
      }else if(input$genre_2 == "Tous les regroupements" & input$type_form_2 != "Tous les regroupements"){
        bd_coll_all_digits |>
          filter(decoupage == "ra" | decoupage == "prov") |>
          mutate(annee = as.numeric(annee)) |>
          filter(annee %in% annees_ind()) |>
          filter(type_form_nom == input$type_form_2) |>
          rename(`Sexe` = `sexe_coll`,`Type de formation`=`type_form_nom`,`Territoire`=`nom`) |>
          select(annee,`Territoire`,`Sexe`,`Type de formation`,meta_ind()$indicateurs)%>%
          pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs)
      }else{
        bd_coll_all_digits |>
          filter(decoupage == "ra" | decoupage == "prov") |>
          mutate(annee = as.numeric(annee)) |>
          filter(annee %in% annees_ind())%>%
          filter(type_form_nom == input$type_form_2) |>
          filter(sexe_coll == input$genre_2) |>
          rename(`Sexe` = `sexe_coll`,`Type de formation`=`type_form_nom`,`Territoire`=`nom`) |>
          select(annee,`Territoire`,`Sexe`,`Type de formation`,meta_ind()$indicateurs)%>%
          pivot_wider(names_from = annee, values_from = meta_ind()$indicateurs)
      }

    })

    bd_table_coll <- reactive({
      bd_table_coll_all_digits() |>
        mutate_if(is.numeric,
                  round,
                  digits = 1)

    })

    output$table_coll_titre<-renderText(paste(meta_ind()$nom_ind_complet_unite))

    output$table_coll <- DT::renderDataTable({
      dtable <- datatable(bd_table_coll(),
                          options = list(pageLength = 10,dom = 'tip', scrollX = TRUE,#autoWidth = TRUE,
                                         fixedColumns = list(leftColumns = 2),
                                         language = list(
                                           info = "Observations _START_ à _END_ sur un total de _TOTAL_ entrées",
                                           paginate = list(previous = 'Précédent', `next` = 'Suivant')
                                           #lengthMenu = "Afficher _MENU_ obs.",search = "Rechercher"
                                         )),
                          extensions = "FixedColumns")|>
        formatRound(
          columns=c(4:ncol(bd_table_coll())),
          digits = 1,interval = 3, mark = " ", dec.mark = ","
        ) |>
        formatStyle(columns = c(1), width='200px')
      return(dtable)

    })

    info_table <- reactive({
      data.frame(Informations = c(paste0("Indicateur : ",meta_ind()$nom_ind_complet_unite),paste0("Producteur de données : ", meta_ind()$producteur),"L'année correspond à l'année de la cohorte",paste0("Date du téléchargement : ", Sys.Date())))
    })

    bd_table_coll_all_digits_2 <- reactive({
      req(input$ind)
      tb_donnee <- bind_rows(info_table(),bd_table_coll_all_digits())
    })

    output$download <- downloadHandler(
      filename = function() {
        paste0("table_",meta_ind()$nom_court_1, Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write_xlsx(bd_table_coll_all_digits_2(), file)
      })

    output$meta_citation <- renderUI({HTML(citation_coll$spec_region)})

    # output$citer_site <- renderUI({HTML(paste0(#"<p style='font-size :23px;font-weight:bold;'>",
    #   "<p style='font-size :14px;'>",
    #   "Référence suggérée : ÉCOBES — Recherche et transfert. (2024). Parcours postsecondaires. Dans Regard 360 [plateforme]. https://cartojeunes.shinyapps.io/versionBeta_016/","</p>"))})


#### Analyse par cohorte ####

    output$titre_cohorte<-renderText("Indicateurs multiples par cohorte")

    output$desc_cohorte <- renderUI({HTML(texte_desc_ana_cohorte_coll$spec_region)})

    labels_dec_coll <- data.frame(v1=c("dec_dur_pre_diff","dec_dur_pre"),
                                  v2=c("Écart taux d'obtention d'un DEC (2 ans - durée prévue) ",
                                       "Taux d'obtention d'un DEC durée prévue"))

    bd_hist_cohorte <-reactive({
      bd_coll |>
        filter(annee %in% c(2008:2018)) |>
        filter(nom == input$region_3, sexe_coll == input$sexe_3, type_form_nom == input$type_form_3) |>
        select(nom,cohorte_id,sexe_coll,type_form_nom,dec_dur_pre_diff,dec_dur_pre) |>
        pivot_longer(cols=c("dec_dur_pre_diff","dec_dur_pre"),names_to = "type_data_dec",values_to = "taux_dip_dec") |>
        left_join(labels_dec_coll,by = join_by(type_data_dec == v1)) |>
        select(-type_data_dec) |>
        rename(type_data_dec = v2)
      #mutate(type_data_dec = factor(type_data_dec, levels = c("dec_dur_pre_diff", "dec_dur_pre")))
    })

    output$hist_cohorte <- renderPlot({
      ggplot(data = bd_hist_cohorte(),mapping = aes(x = cohorte_id, y = taux_dip_dec,fill = type_data_dec)) +
        geom_bar(stat = "identity")+
        #geom_text(aes(label = round(taux_dip_dec,digits  =1)),position = position_stack(vjust = 0.5),size=5)+

        geom_text(aes(label = format(taux_dip_dec,accuracy=0.1, big.mark = " ", decimal.mark = ",")),position = position_stack(vjust = 0.5), size=5.0)+

        theme_minimal()+
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
              legend.text = element_text(size = 12),
              legend.title = element_blank()
        )+
        scale_x_continuous(limits = c(2007,2019),breaks = seq(2008, 2018, by = 1))+
        theme(legend.position='top')+
        theme(axis.text.x = element_text(size = 12.0, hjust = 1,vjust = 0.5))+
        theme(axis.text.y = element_text(size = 12.0, hjust = 1,vjust = 0.5))+
        theme(axis.title.x = element_text(size = 12.0),axis.title.y = element_text(size = 12.0))+
        labs(title = paste0("Taux d'obtention d'un DEC par cohorte selon la durée :\n ",input$region_3,"-",input$sexe_3,"-",input$type_form_3),
             x = "Cohorte",y = "",subtitle = "",caption = "")+
        theme(plot.title = element_text(size = 15.0,hjust = .5,color = "#007A99FF", lineheight  = 1.1))
    })

    labels_graph_coll <- data.frame(v1=c("taux_reu_1er_trim","taux_reu_3e_trim","dec_dur_pre","dec_dur_pre_2"),
                                    v2=c("1er trimestre","3e trimestre",
                                         "DEC durée prévue","DEC 2ans après la durée prévue"))

    bd_graph_cohorte <-reactive({
      bd_coll |>
        filter(annee %in% c(2008:2022)) |>
        filter(nom == input$region_3, sexe_coll == input$sexe_3, type_form_nom == input$type_form_3) |>
        select(nom,cohorte_id,type_form_nom,sexe_coll,taux_reu_1er_trim,taux_reu_3e_trim) |>
        pivot_longer(cols=c("taux_reu_1er_trim","taux_reu_3e_trim"),names_to = "ind_coll",values_to = "taux_ind") |>
        left_join(labels_graph_coll,by = join_by(ind_coll == v1)) |>
        select(-ind_coll) |> rename(ind_coll = v2)
      #mutate(type_data_dec = factor(type_data_dec, levels = c("dec_dur_pre_diff", "dec_dur_pre")))
    })

    output$evo_cohorte <- renderPlot({
      ggplot(data = bd_graph_cohorte(),aes(x = cohorte_id, y = taux_ind, color = ind_coll)) +
        theme_minimal()+
        geom_line(size = .99)+
        geom_point(size = 2.0)+
        theme(legend.text = element_text(size = 12),
              legend.title = element_blank(),
              legend.position='top')+
        scale_x_continuous(expand = c(0, 0),limits = c(2007, 2023),breaks = seq(2008, 2022, by = 1))+
        theme(axis.text.x = element_text(size = 12.0, hjust = 1,vjust = 0.5))+
        theme(axis.text.y = element_text(size = 12.0, hjust = 1,vjust = 0.5))+
        labs(title = paste0("Taux de réussite des cours selon le trimestre par cohorte :\n ",input$region_3," - ",input$sexe_3," - ",input$type_form_3),
             x = "Cohorte",y = "Taux de réussite",subtitle = "",caption = "")+
        theme(axis.title.x = element_text(size = 12.0),axis.title.y = element_text(size = 12.0))+
        theme(plot.title = element_text(size = 15.0,hjust = .5,color = "#007A99FF", lineheight  = 1.1))

    })

    labels_ind_coll <- data.frame(v1=c("nbr_ni","contin_sec_col","admis_cond","mgs_moy","taux_reu_1er_trim","insc_3e_trim","taux_reu_3e_trim","dec_dur_pre","dec_dur_pre_2",
                                       "aec_dur_pre","aec_dur_pre_2","dep_dur_pre","dep_dur_pre_2"),
                                  v2=c("Nouveaux inscrits – total (nb)",
                                       "Poursuite sans interruption (%)",
                                       "Admissions conditionnelles (%)",
                                       "Moyenne au secondaire (%)",
                                       "Réussite des cours au 1er trimestre (%)",
                                       "Réinscription au 3e trimestre (%)",
                                       "Réussite des cours au 3e trimestre (%)",
                                       "Obtention d’un DEC – durée prévue (%)",
                                       "Obtention d’un DEC – 2 ans après la durée prévue (%)",
                                       "Obtention d’une AEC – durée prévue (%)",
                                       "Obtention d’une AEC – 2 ans après la durée prévue (%)",
                                       "Obtention d’un DEP – durée prévue (%)",
                                       "Obtention d’un DEP – 2 ans après la durée prévue (%)"))

    bd_table_cohorte_down <-reactive({
      #bd_coll |>
      bd_coll_all_digits |>
        filter(annee %in% c(2008:2023)) |>
        filter(nom == input$region_3) |>
        select(nom,cohorte_id,type_form_nom,sexe_coll,
               nbr_ni,contin_sec_col,admis_cond,mgs_moy,taux_reu_1er_trim,taux_reu_3e_trim,insc_3e_trim,
               dec_dur_pre,dec_dur_pre_2,aec_dur_pre,aec_dur_pre_2,dep_dur_pre,dep_dur_pre_2) |>
        pivot_longer(cols=c("nbr_ni","contin_sec_col","admis_cond","mgs_moy","taux_reu_1er_trim","taux_reu_3e_trim","insc_3e_trim","dec_dur_pre","dec_dur_pre_2",
                            "aec_dur_pre","aec_dur_pre_2","dep_dur_pre","dep_dur_pre_2"),names_to = "ind_coll",values_to = "taux_ind_coll") |>
        left_join(labels_ind_coll,by = join_by(ind_coll == v1)) |>
        pivot_wider(names_from = cohorte_id, values_from = taux_ind_coll) |>
        select(-ind_coll) |>
        rename(Indicateur = v2)
    })

    bd_table_cohorte <-reactive({
      bd_table_cohorte_down() |>
        filter(sexe_coll == input$sexe_3, type_form_nom == input$type_form_3) |>
        select(-sexe_coll,-type_form_nom,-nom)
    })

    output$table_cohorte_titre<-renderText(paste("Indicateurs collégiaux par cohorte : ",req(input$region_3)," - ",req(input$type_form_3),"-",req(input$sexe_3)))

    output$table_cohorte <- DT::renderDataTable({
      dtable <- datatable(bd_table_cohorte(),
                          options = list(pageLength = 14,dom = 'tip', scrollX = TRUE,#autoWidth = TRUE,
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

    bd_table_cohorte_down_2 <-reactive({
      bd_table_cohorte_down() |>
        rename(Territoire = nom, `Type de formation` = `type_form_nom`, Sexe = sexe_coll)
    })

    info_cohorte <- data.frame(Informations = c("Producteur de données : Ministère de l'Enseignement supérieur","L'année correspond à l'année de la cohorte",paste0("Date du téléchargement : ", Sys.Date())))

    bd_table_cohorte_down_3 <- reactive({
      req(input$ind)
      tb_donnee <- bind_rows(info_cohorte,bd_table_cohorte_down_2())
    })

    output$down_cohorte <- downloadHandler(
      filename = function() {
        paste0("bd_table_cohorte_coll", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write_xlsx(bd_table_cohorte_down_3(), file)
      })


    ############

  })

}












#Pour rouler le module de manière autonome

# parsco_App <- function() {
#   ui <- fluidPage(
#     fluidRow(UI_1_parsco("hist1")),
#     #fluidRow(UI_2_parsco("hist1")),
#     fluidRow(UI_3_parsco("hist1")),
#     fluidRow(UI_4_parsco("hist1")),
#     #fluidRow(UI_5_parsco("hist1")),
#     #fluidRow(UI_6_parsco("hist1")),
#     fluidRow(UI_7_parsco("hist1")),
#
#   )
#
#   server <- function(input, output, session) {
#     server_parsco("hist1")
#   }
#   shinyApp(ui, server)
# }
#
# parsco_App()


