
load(file = "bd_ann_recente_29juin2026.rdata")
load(file = "Metadata_29juin2026.rdata")
load(file = "Metadata_acronyme_2avril.rdata")
load(file = "Metadata_region_1oct2025.rdata")

alpha_reg <-c("Ensemble du Québec","Abitibi-Témiscamingue","Bas-Saint-Laurent","Capitale-Nationale","Centre-du-Québec","Chaudière-Appalaches",
              "Côte-Nord","Estrie","Gaspésie–Îles-de-la-Madeleine","Lanaudière","Laurentides","Laval","Mauricie","Montérégie","Montréal",
              "Nord-du-Québec","Outaouais","Saguenay–Lac-Saint-Jean")
alpha_reg <- factor(alpha_reg,levels=alpha_reg)

#### Manipulations pour la Bd annee recente #################

bd_ann_recente_all_digits <- bd_ann_recente |>
  mutate(valeur = round(valeur,digits = 4))

#colnames(bd_ann_recente)[colnames(bd_ann_recente) == "nom_ind_complet_unite"] <- "Indicateur"
colnames(bd_ann_recente)[colnames(bd_ann_recente) == "nom_ind_court"] <- "Indicateur"
colnames(bd_ann_recente)[colnames(bd_ann_recente) == "annee"] <- "Année la plus récente"
colnames(bd_ann_recente_all_digits)[colnames(bd_ann_recente_all_digits) == "nom_ind_court"] <- "Indicateur"
colnames(bd_ann_recente_all_digits)[colnames(bd_ann_recente_all_digits) == "annee"] <- "Année la plus récente"

#### Pour le menu de la liste des noms d'indicateurs ##############

#Population
list <- metadata%>%
  filter(theme == "Démographie")
list_demographie <- unique(list$nom_ind_court)

list <- metadata%>%
  filter(theme == "Familles et ménages")
list_famille <- unique(list$nom_ind_court)

list <- metadata%>%
  filter(theme == "Diversité culturelle")
list_div_culture <- unique(list$nom_ind_court)

#Conditions de vie
list <- metadata%>%
  filter(theme == "Emploi, occupation et revenu")
list_emploi <- unique(list$nom_ind_court)

list <- metadata%>%
  filter(theme == "Logement")
list_logement <- unique(list$nom_ind_court)

list <- metadata%>%
  filter(theme == "Culture")
list_culture <- unique(list$nom_ind_court)

list <- metadata%>%
  filter(theme == "Vitalité du territoire")
list_vitalite <- unique(list$nom_ind_court)

#Parcours éducatifs
list <- metadata%>%
  filter(theme == "Petite enfance et parcours au préscolaire") |>
  filter(indicateurs != "VulMat_5Domaine_pct")
list_presco <- unique(list$nom_ind_court)

list <- metadata%>%
  filter(theme == "Parcours au primaire")
list_primaire <- unique(list$nom_ind_court)

list <- metadata%>%
  filter(theme == "Parcours au secondaire") |>
  filter(indicateurs != "decrochage_pct")
list_pars_sco <- unique(list$nom_ind_court)

list <- metadata%>%
  filter(theme == "Parcours au collégial")
list_cegep <- unique(list$nom_ind_court)

list <- metadata%>%
  filter(theme == "Scolarité des adultes et littératie")
list_sco_adulte <- unique(list$nom_ind_court)

nms_1 = list(
  "Démographie" = list_demographie,
  "Familles et ménages" = list_famille,
  "Diversité culturelle" = list_div_culture
)

nms_2 = list(
  "Emploi, occupation et revenu" = list_emploi,
  "Logement" = list_logement,
  "Culture" = list_culture,
  "Vitalité du territoire" = list_vitalite
)

nms_3 = list(
  "Petite enfance et parcours au préscolaire" = list_presco,
  "Parcours au primaire" = list_primaire,
  "Parcours au secondaire" = list_pars_sco,
  "Parcours au collégial" = list_cegep,
  "Scolarité des adultes et littératie" = list_sco_adulte
)

texte_desc_tab_person <- filter(metadata_region, spec_region_nom == "desc_tab_person")
texte_metho_tableau <- filter(metadata_region, spec_region_nom == "metho_tableau")
citation_tableau <- filter(metadata_region, spec_region_nom == "citation_tableau")

########################################################################

############################################################################################
############################################################################################

UI_titre_tabind <- function(id){
  fluidRow(column(6,offset = 0,textOutput(NS(id,"titre")),
                  tags$head(tags$style(paste0("#",NS(id,"titre"),"{color: #0F2B5FFF;font-size: 24px;font-style: bold;}"))))
  )
}

UI_main_tabind <- function(id){
  fluidRow(column(12,offset = 0,htmlOutput(NS(id,"titre_def"))),
           column(2,offset = 10,actionButton(NS(id,"hide_acronyme"), "Acronymes",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                                            style="color: #fff; background-color: #00A887;padding:5px; font-size:85%; border-color: #2e6da4"))
  )
}

UI_acronyme_tabind <- function(id){
  fluidRow(column(4,offset = 3,hidden(htmlOutput(NS(id,"acronyme_1")))),
           column(4,hidden(htmlOutput(NS(id,"acronyme_2"))))
  )
}

tabindUI <- function(id){
  fluidRow(
    column(2,selectInput(NS(id,"ra"), "Territoire", choices = alpha_reg,selected = "Ensemble du Québec")),
    column(2,pickerInput(NS(id,"nom_ind_1"),label = "Indicateurs - Population",choices = nms_1,multiple = TRUE,
      options = pickerOptions(size = 15,maxOptions = 15, maxOptionsText = "Le maximum est atteint",
                              noneSelectedText = "Aucune sélection",selectAllText = "Tout sélectionner",deselectAllText = "Désélectionner")
    )),
    column(2,pickerInput(NS(id,"nom_ind_2"),label = "Indicateurs - Conditions de vie",choices = nms_2,multiple = TRUE,
      options = pickerOptions(size = 15,maxOptions = 15, maxOptionsText = "Le maximum est atteint",
                              noneSelectedText = "Aucune sélection",selectAllText = "Tout sélectionner",deselectAllText = "Désélectionner")
    )),
    column(3,pickerInput(NS(id,"nom_ind_3"),label = "Indicateurs - Parcours éducatifs ",choices = nms_3,multiple = TRUE,
      options = pickerOptions(size = 15,maxOptions = 15, maxOptionsText = "Le maximum est atteint",
                              noneSelectedText = "Aucune sélection",selectAllText = "Tout sélectionner",deselectAllText = "Désélectionner")
    ))
  )
}

tabindUI_2 <- function(id){
  fluidRow(
    column(12,DT::dataTableOutput(NS(id,"table1")))
  )
}

tabindUI_3 <- function(id){
  # fluidRow(
  #   column(3,downloadButton(NS(id,"download"), "Téléchargement du tableau"))
  # )
  fluidRow(column(3,offset = 3, align="left",
                  actionButton(NS(id,"hide_meta_tableau"), "Précisions sur le tableau",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                               style="color: #fff;background-color: #BDBFBF;padding:5px; font-size:85%")),
           column(3,align="right",downloadButton(NS(id,"download"), "Téléchargement du tableau",style="color: #fff; background-color: #3C65CF")))
}

UI_hide_meta_tableau <- function(id){
  fluidRow(column(6,offset = 1,hidden(htmlOutput(NS(id,"meta_tableau")))))
}


UI_action_citation <- function(id){
  fluidRow(column(3,offset = 1,
                  actionButton(NS(id,"hide_meta_citation"), "Référence suggérée",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                               style="color: #fff; background-color: #36B0C9; border-color: #2e6da4;")))

}

UI_citation <- function(id){
  fluidRow(column(10,offset = 1,hidden(htmlOutput(NS(id,"meta_citation")))))
}

##############################################################################

tabindServer <- function(id){
  moduleServer(id, function(input,output,session){

    observeEvent(input$hide_acronyme, {
      toggle("acronyme_1")
      toggle("acronyme_2")
    })

    observeEvent(input$hide_meta_tableau,{
      toggle("meta_tableau")
    })

    observeEvent(input$hide_meta_citation,{toggle("meta_citation")})

    meta_acronyme_select <-reactive({
      filter(metadata_acronyme, Acronymes1 == "RA : Région administrative")
    })

    output$titre<-renderText("Tableau personnalisé")

    output$titre_def <- renderUI({HTML(texte_desc_tab_person$spec_region)})

    output$acronyme_1 <- renderUI({HTML(meta_acronyme_select()$meta_acronyme_1)})
    output$acronyme_2 <- renderUI({HTML(meta_acronyme_select()$meta_acronyme_2)})


    meta_ind <- reactive({metadata %>%
        filter(nom_ind_court %in% c(input$nom_ind_1,input$nom_ind_2,input$nom_ind_3))
    })

    bd_table_mrc1 <- reactive({
      req(any(nzchar(input$nom_ind_1), nzchar(input$nom_ind_2), nzchar(input$nom_ind_3)))
      if(input$ra == "Ensemble du Québec"){
      bd_ann_recente %>%
        filter(decoupage %in% c("ra") | nom == "Ensemble du Québec" )%>%
        filter(!(nom %in% c("Estrie (ancienne)", "Montérégie (ancienne)"))) |>
        filter(indicateurs %in% meta_ind()$indicateurs)%>%
        select(nom,`Année la plus récente`,indicateurs,`Indicateur`,valeur,source)%>%
        pivot_wider(names_from = nom, values_from = valeur) |>
        select(-indicateurs)%>%
        relocate(`Indicateur`,source,`Année la plus récente`,`Ensemble du Québec`)
      }else{
        bd_ann_recente %>%
          filter(ra_app_nom ==input$ra | nom == "Ensemble du Québec") %>%
          filter(!(nom %in% c("Estrie (ancienne)", "Montérégie (ancienne)"))) |>
          filter(decoupage %in% c("mrc","ra","prov"))%>%
          filter(indicateurs %in% meta_ind()$indicateurs)%>%
          select(nom,`Année la plus récente`,indicateurs,`Indicateur`,valeur,source)%>%
          pivot_wider(names_from = nom, values_from = valeur) |>
          select(-indicateurs)%>%
          relocate(`Indicateur`,source,`Année la plus récente`,`Ensemble du Québec`)
      }
    })

    bd_table_mrc2 <- reactive({
      bd_table_mrc1() |>
        relocate(`Indicateur`,source,`Année la plus récente`,`Ensemble du Québec`)
    })

    # Pour changer des aspects du language : https://thinkr.fr/tableaux-interactifs-avec-r-pour-shiny-et-vos-pages-web/ et https://datatables.net/reference/option/


    output$table1 <- DT::renderDataTable({
      dtable <- datatable(bd_table_mrc2(),
                          options = list(pageLength = 10,searching = FALSE, scrollX = TRUE,#autoWidth = TRUE,
                                         fixedColumns = list(leftColumns = 3),
                                         language = list(
                                           info = "Observations _START_ à _END_ sur un total de _TOTAL_ entrées",
                                           paginate = list(previous = 'Précédent', `next` = 'Suivant'),
                                           lengthMenu = "Afficher _MENU_ obs.",search = "Rechercher"
                                         ),
                                         autoWidth = FALSE,
                                         columnDefs = list(list(visible = FALSE, targets = 0),
                                                           list(width = '200px', targets = c(1,2)),
                                                           list(width = '40px', targets = c(3)),
                                                           list(width = '85px', targets = "_all")
                                                           )
                                         ),
                          colnames = c('Source' = 'source'),
                          extensions = "FixedColumns")|>
        formatRound(
          columns=c(4:ncol(bd_table_mrc2())),
          digits = 1,interval = 3, mark = " ", dec.mark = ","
        )
      return(dtable)
    })

    bd_table_mrc1_all_digits <- reactive({
      req(any(nzchar(input$nom_ind_1), nzchar(input$nom_ind_2), nzchar(input$nom_ind_3)))
      if(input$ra == "Ensemble du Québec"){
        bd_ann_recente_all_digits %>%
          #filter(nom_region ==input$ra | nom_region == "Ensemble du Québec") %>%
          filter(decoupage %in% c("ra") | nom == "Ensemble du Québec" )%>%
          filter(indicateurs %in% meta_ind()$indicateurs)%>%
          select(nom,`Année la plus récente`,indicateurs,`Indicateur`,valeur,source)%>%
          pivot_wider(names_from = nom, values_from = valeur) |>
          select(-indicateurs)%>%
          relocate(`Indicateur`,source,`Année la plus récente`,`Ensemble du Québec`)
      }else{
        bd_ann_recente_all_digits %>%
          filter(ra_app_nom ==input$ra | nom == "Ensemble du Québec") %>%
          filter(decoupage %in% c("mrc","ra","prov"))%>%
          filter(indicateurs %in% meta_ind()$indicateurs)%>%
          select(nom,`Année la plus récente`,indicateurs,`Indicateur`,valeur,source)%>%
          pivot_wider(names_from = nom, values_from = valeur) |>
          select(-indicateurs)%>%
          relocate(`Indicateur`,source,`Année la plus récente`,`Ensemble du Québec`)
      }
    })


    output$download <- downloadHandler(
      filename = function() {
        paste0("table_personnalise_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write_xlsx(bd_table_mrc1_all_digits(), file)
      }
    )


    # output$meta_tableau <- renderUI({HTML(paste0(#"<p style='font-size :23px;font-weight:bold;'>",
    #   "<p style='font-size :15px;'>",
    #   "Les données affichées dans les tableaux de la plateforme contiennent un nombre restreint de décimales en raison d’un arrondi
    # effectué pour épurer l’affichage. Le tableau téléchargé contient toutefois jusqu’à 4 décimales pour les indicateurs dont cette
    # précision est disponible.","</p>"))})

    output$meta_tableau <- renderUI({HTML(texte_metho_tableau$spec_region)})

    output$meta_citation <- renderUI({HTML(citation_tableau$spec_region)})

    # output$citer_site <- renderUI({HTML(paste0(#"<p style='font-size :23px;font-weight:bold;'>",
    #   "<p style='font-size :14px;'>",
    #   "Référence suggérée : ÉCOBES — Recherche et transfert. (2024). Tableau personnalisé. Dans Regard 360 [plateforme]. https://cartojeunes.shinyapps.io/versionBeta_016/","</p>"))})

  })

}

tabindApp <- function(){
  ui <- fluidPage(
    tabindUI("hist1"),
    tabindUI_2("hist1"),
    tabindUI_3("hist1")
  )

  server <- function(input, output, session) {
    tabindServer("hist1")
  }
  shinyApp(ui, server)
}

tabindApp()

