
load(file = "bd_enquete_2026-03-26.rdata")
load(file = "Metadata_enquete_2026_v1.rdata")
load(file = "Metadata_region_1oct2025.rdata")
load(file = "bd_eqsjs_enquete_2026-03-26.rdata")

# Pour enlever les découpages qui ne sont pas des MRC
 bd_enquete_2 <- bd_enquete |>
   filter(!(code_geo %in% c(2413800:2413810))) |>
   filter(!(code_geo %in% c(2406900:2406950)))
#   mutate_at(vars(columns_to_convert), as.numeric)

#------------------------------------
# Les étapes ci-dessous ont pour objectif d'avoir une BD sans l'indice de différence significative avec la province
# pour les RA puisque cet indice n'a pas sa place lorsque l'analyse est pour les MRC d'une RA.

eqdem_ra_pour_mrc <- bd_enquete_2 |>
  filter(annee == 2017| annee == 2022) |>
  filter(is.na(sante)) |>
  filter(decoupage == "ra") |>
  #filter(nom != "Estrie" & nom != "Montérégie") |> # Il y a seulement des données pour les anciennes définitions géographique.
  #La significativité est entre la RA et le Qc, elle n'est donc pas pertinente lorsque la RA est présenté avec ces mrc. On élimine les valeurs.
  mutate(VulMat_SantPhy_signif = "", VulMat_5Domaine_signif= "",VulMat_CompSoci_signif= "",VulMat_HabComm_signif= "",VulMat_DevCogni_signif= "",VulMat_MaturiteAff_signif= "")

bd_eqdem_pour_mrc <- bd_enquete_2 |>
  filter(annee == 2017 | annee == 2022) |>
  filter(is.na(sante)) |>
  filter(decoupage == "mrc") |>
  rbind(eqdem_ra_pour_mrc) |>
  mutate(
    color_ecobes = case_when(
      decoupage == "ra" ~ "palegreen4",
      decoupage == "prov" ~ "steelblue3",
      TRUE ~ "palegreen3"

    ))

bd_eqdem_pour_ra <- bd_enquete_2 |>
  filter(annee == 2017 | annee == 2022) |>
  filter(is.na(sante)) |>
  filter(decoupage == "ra" | decoupage == "prov") |>
  #filter(nom != "Estrie" & nom != "Montérégie") |>  # Il y a seulement des données pour les anciennes définitions géographique.
  mutate(
    color_ecobes = case_when(
      decoupage == "ra" ~ "palegreen4",
      decoupage == "prov" ~ "steelblue3",
      TRUE ~ "palegreen3"

    ))

#--- Les données pour l'EQSJS, c'est une BD différente car la géographie d'intérêt est celle de la santé (RSS)

#bd_eqsjs <- bd_enquete_2 |>
bd_eqsjs <- bd_eqsjs_enquete |>
  subset(sante == TRUE) |>
  #filter(annee == 2017) |>
  filter(decoupage == "prov" | decoupage == "rss") |>
  mutate(
    color_ecobes = case_when(
      decoupage == "rss" ~ "palegreen4",
      decoupage == "prov" ~ "steelblue3",
      TRUE ~ "palegreen3"

    ))

unique_no.NA <- function(x){
  if (!is.vector(x)) stop('x must be a vector')
  u=unique(x)
  u[!is.na(u)]
}

list_region <- unique_no.NA(bd_enquete_2$ra_app_nom)

############################################################################################

annee_eqdem <- c(2022,2017)
annee_eqdem <- factor(annee_eqdem,levels=annee_eqdem)
annee_eqsjs <- c(2023,2017)
annee_eqsjs <- factor(annee_eqsjs,levels=annee_eqsjs)

texte_desc_enquete <- filter(metadata_region, spec_region_nom == "desc_enquete")
citation_enquete <- filter(metadata_region, spec_region_nom == "citation_enquete")

############################################################################################

UI_titre_eqdem <- function(id){
  fluidRow(column(6,offset = 0,textOutput(NS(id,"titre")),
                  tags$head(tags$style(paste0("#",NS(id,"titre"),"{color: #0F2B5FFF;font-size: 24px;font-style: bold;}"))))
  )
}

UI_titre_eqdem_def <- function(id){
  fluidRow(column(12,offset = 0,htmlOutput(NS(id,"titre_def"))))
}


eqdem_colonnes_def <- function(id){
  fluidRow(
      column(10,offset = 1,uiOutput(NS(id,"colonnes_def")),
             tags$head(tags$style(paste0("#",NS(id,"colonnes_def"),"{color: black ;font-size: 12px;font-style: bold;}"))))
    )
}

eqdem_3_choices <- function(id){
  fluidRow(
    column(3,offset=1,selectInput(NS(id,"theme"), "Enquête", choices = unique_no.NA(metadata_enquete$theme),width = "475px",selected = "Enquête québécoise sur le développement des enfants à la maternelle (EQDEM)")),
    column(4,selectInput(NS(id,"ind"), "Indicateur", choices = NULL,width = "500px")),
    column(2,selectInput(NS(id,"ra"), "Territoire", choices = NULL)),
    column(1,align="center",selectInput(NS(id,"ann"), "Année", choices = NULL))
  )
}


bouton_enquete <- function(id){
  useShinyjs()
  fluidRow(
    column(3,offset = 1,align="right",actionButton(NS(id,"hide_enquete"), "Précisions sur l'enquête",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                                   style="color: #fff; background-color: #36B0C9; padding:5px; font-size:85%; border-color: #2e6da4"))
  )
}


enquete_def_enquete <- function(id){
  fluidRow(
    column(10,offset = 1,hidden(htmlOutput(NS(id,"def_enquete"))))
  )
}


eqdem_nom_ind_enquete <- function(id){
  fluidRow(
    column(10,offset = 1,textOutput(NS(id,"nom_ind_enquete")),
                    tags$head(tags$style(paste0("#",NS(id,"nom_ind_enquete"),"{color: #0F2B5FFF ;font-size: 20px;font-style: bold;}"))))
  )
}

eqdem_def_ind_enquete <- function(id){

  # fluidRow(
  #   column(10,offset = 1,htmlOutput(NS(id,"def_ind_enquete")))
  # )


  tagList(
  fluidRow(
      column(10,offset = 1,uiOutput(NS(id,"def_ind_enquete")),
             span("En savoir plus", class = "voir-plus", onclick = "toggleText()"))
    ),
    tags$script(HTML("
    function toggleText() {
      var el = document.getElementById('texteLong');
      el.classList.toggle('expanded');
      var btn = document.querySelector('.voir-plus');
      btn.innerText = el.classList.contains('expanded') ? 'En savoir plus' : 'En savoir plus';
    }
  ")),
  )

}

eqdem_graph <- function(id){
  fluidRow(
    column(4,offset = 4,align="center",reactableOutput(NS(id,"table3"))),
    #column(4,offset = 4, align = "center", dataTableOutput(NS(id,"table3"))),
    #column(6,plotOutput(NS(id,"hist_ind"), width = "100%", height = 400))
    )
}

eqdem_download <- function(id){
  fluidRow(
    column(11,offset = 1,align="center",downloadButton(NS(id,"download"), "Téléchargement du tableau",style="color: #fff; background-color: #3C65CF")),
  )
}

UI_action_citation <- function(id){
  fluidRow(column(3,offset = 1,
                  actionButton(NS(id,"hide_meta_citation"), "Référence suggérée",icon("pen-nib"), #https://fontawesome.com/search?s=light&f=classic&o=r
                               style="color: #fff; background-color: #36B0C9; border-color: #2e6da4;")))

}

UI_citation <- function(id){
  fluidRow(column(10,offset = 1,hidden(htmlOutput(NS(id,"meta_citation")))))
}



############################################################################################

eqdem_server <- function(id){
  moduleServer(id, function(input,output,session){

    list_ind_theme <-reactive({
      filter(metadata_enquete, theme == input$theme)
    })

    observeEvent(list_ind_theme(),{
      choices <- unique(list_ind_theme()$nom_ind_court)
      freezeReactiveValue(input, "ind")
      updateSelectInput(inputId = "ind", choices = choices)
    })

    observeEvent(input$theme,{
      if(input$theme == "Enquête québécoise sur le développement des enfants à la maternelle (EQDEM)"){choices <- alpha_reg}
      else{choices <- "Ensemble du Québec"}
      freezeReactiveValue(input, "ra")
      updateSelectInput(inputId = "ra", choices = choices)
    })

    observeEvent(input$theme,{
      if(input$theme == "Enquête québécoise sur le développement des enfants à la maternelle (EQDEM)"){choices <- annee_eqdem}
      else{choices <- annee_eqsjs}
      freezeReactiveValue(input, "ann")
      updateSelectInput(inputId = "ann", choices = choices)
    })

    observeEvent(input$hide_enquete, {toggle("def_enquete")})

    observeEvent(input$hide_meta_citation,{toggle("meta_citation")})


    meta_ind <- reactive({
      req(input$ind)
      list_ind_theme() %>%
        filter(nom_ind_court == input$ind)
    })

    output$titre<-renderText("Enquêtes")

    output$titre_def <- renderUI({HTML(texte_desc_enquete$spec_region)})

    output$titre_enquete <- renderText(paste("Enquête de l'indicateur : ",meta_ind()$nom_ind_complet))
    output$def_enquete <- renderUI({HTML(meta_ind()$theme_definition)})

    output$nom_ind_enquete <- renderText(paste("Indicateur : ",meta_ind()$nom_ind_complet))
    #output$nom_ind_enquete <- renderText(meta_ind()$nom_ind_complet)
    #output$def_ind_enquete <- renderUI({HTML(meta_ind()$def_ind)})
    output$def_ind_enquete <- renderUI({
      div(id = "texteLong", class = "text-container",HTML(meta_ind()$def_ind))
      })


    bd_tab_enquete_1 <- reactive({
      if (input$theme == "Enquête québécoise sur le développement des enfants à la maternelle (EQDEM)" &
          input$ra == "Ensemble du Québec") {
        bd_eqdem_pour_ra
      } else if (input$theme != "Enquête québécoise sur le développement des enfants à la maternelle (EQDEM)" &
                 input$ra == "Ensemble du Québec") {
        bd_eqsjs #|> arrange()
      } else{bd_eqdem_pour_mrc %>% filter(ra_app_nom == input$ra)}

      })

    output$colonnes_def <-renderUI({
      if(input$theme == "Enquête québécoise sur le développement des enfants à la maternelle (EQDEM)"){
      HTML(paste0("<p style='font-size :14px;color:#484A45 ;'>",
           "<b> Signification des colonnes  </b> <br>
           <em> Taux </em> : Proportion d'enfants vulnérables selon l'indicateur sélectionné <br>
           <em> DS </em>  : Différence significative - Proportion significativement supérieure (+) ou inférieure (-), au seuil de 0,05, entre la MRC et le reste de la région administrative ou entre la région et le reste de la province <br>
           <em> IC </em> : Intervalle de confiance - IC à 99 % pour l'ensemble du Québec - IC à 95 % pour une région administrative - IC à 95 % pour une MRC <br>
           <em> CV </em> : Coefficient de variation -   * CV compris entre 15 % et 25 %; interpréter avec prudence - ** CV supérieur à 25 %; estimation imprécise fournie à titre indicatif seulement"))}
      else{
        HTML(paste0("<p style='font-size :14px;color:#484A45 ;'>",
          "<b> Signification des colonnes  </b> <br>
           <em> Taux </em> : Proportion de jeunes du secondaire ayant la caractéristique de l'indicateur sélectionné. <br>
           <em> DS </em>  : Différence significative - Proportion significativement supérieure (+) ou inférieure (-), au seuil de 0,05, entre la région et le reste de la province <br>
           <em> IC </em> : Intervalle de confiance - IC à 99 % pour l'ensemble du Québec - IC à 95 % pour une région <br>
           <em> CV </em> : Coefficient de variation -   * CV compris entre 15 % et 25 %; interpréter avec prudence - ** CV supérieur à 25 %; estimation imprécise fournie à titre indicatif seulement"))}
    })

    ind_suffixe <- reactive({
      as.character(meta_ind()$suffixe)
    })

    bd_tab_4stat_1 <- reactive({
      if (input$theme == "Enquête québécoise sur le développement des enfants à la maternelle (EQDEM)"){
      df <- bd_tab_enquete_1() %>%
        filter(annee == input$ann) |>
        select(matches("nom"),matches(ind_suffixe()),-ra_app_nom,-matches("diff"))
      patterns <- c("pct","signif","cv","ic")
      new_names <- c("pct","signif","cv","ic")

      for (i in 1:length(colnames(df))) {
        for (j in 1:length(patterns)) {
          if (grepl(patterns[j], colnames(df)[i])) {
            colnames(df)[i] <- new_names[j]
            break  # Exit the loop if pattern is found and column name is changed
          }
        }
      }
      df}
      else{
        bd_tab_enquete_1() |>
          filter(indicateur == meta_ind()$nom_ind_court, sexe == "Sexes réunis") |>
          filter(annee ==  input$ann) |>
          select(nom = nom,pct = taux,signif = signif,ic,cv)
      }
    })



    bd_tab_4stat <- reactive({bd_tab_4stat_1() |> select(nom,pct,signif,ic,cv)})


    output$table3 <- renderReactable({
      reactable(bd_tab_4stat(),
                defaultPageSize = 18,
                #theme = fivethirtyeight(),
                defaultColDef = colDef( align = "center",minWidth = 80, maxWidth = 160),
                defaultSorted = 'pct',
                defaultSortOrder = 'desc',
                highlight = TRUE,
                pagination = FALSE, height = 350, # Pour faire apparaître le scroll vertical ou horizontal
                columns = list(nom = colDef(name = "Territoire",minWidth = 190,maxWidth = 240,sticky = "left",
                                            style = function(value, index) {
                                              nom <- bd_tab_4stat()$nom[index]
                                              if (str_detect(nom, "MRC")) {
                                                #list(fontWeight = "bold", color = "red")
                                                list(backgroundColor = "#f7f7f7")
                                              }
                                              else{
                                                list(fontWeight = "bold",backgroundColor = "#f7f7f7")
                                              }
                                            }),
                               pct = colDef(
                                 name = "Taux",maxWidth = 65,
                                 format = colFormat(separators = TRUE, digits = 1,locales ="fr-CA"),
                                 # style = function(value, index) {
                                 #   signif <- bd_tab_4stat()$signif[index]
                                 #   if (signif == "+") {
                                 #     list(fontWeight = "bold", color = "#760CC9")
                                 #     #list(color = "red")
                                 #   }
                                 #   else if (signif == "-") {
                                 #     list(fontWeight = "bold",color = "#0C7CC9")
                                 #   }
                                 #   else if (signif == "") {
                                 #     list()
                                 #   }
                                 # }
                               ),
                               signif = colDef(name = "DS",maxWidth = 75),
                               cv = colDef(name = "CV",maxWidth = 55),
                               ic = colDef(name = "IC",minWidth = 95,maxWidth = 95)

                )

      )


    })

    # tab_nom_ind <- reactive({
    #   tab1_nom_ind <- meta_ind() |>
    #     select(nom_ind_complet)
    # })

    info_table <- reactive({
      data.frame(Informations = c(paste0("Indicateur : ",meta_ind()$nom_ind_complet),paste0("Enquête : ", input$theme),paste0("Année : ", input$ann),paste0("Date du téléchargement : ", Sys.Date())))
    })

    table_donnee <- reactive({
      req(input$ind)
      tb_donnee <- bind_rows(info_table(),bd_tab_4stat()) |>
        #rename(`Territoire` = `nom`,`Indicateur` = `nom_ind_complet`, Taux = pct, DS = signif, IC = ic, CV = cv)
        rename(`Territoire` = `nom`, Taux = pct, DS = signif, IC = ic, CV = cv)
    })

    output$download <- downloadHandler(
      filename = function() {
        paste0("table_",meta_ind()$nom_ind_court, Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write_xlsx(table_donnee(), file)
      }
    )

    output$meta_citation <- renderUI({HTML(citation_enquete$spec_region)})

    # output$citer_site <- renderUI({HTML(paste0(#"<p style='font-size :23px;font-weight:bold;'>",
    #   "<p style='font-size :14px;'>",
    #   "Citation suggérée : ÉCOBES — Recherche et transfert. (2024). Enquêtes. Dans Regard 360 [plateforme]. https://cartojeunes.shinyapps.io/versionBeta_016/","</p>"))})


  })
}

tabordApp <- function() {
  ui <- fluidPage(
    fluidRow(UI_titre_eqdem("hist1")),
    fluidRow(UI_titre_eqdem_def("hist1")),
    fluidRow(eqdem_colonnes_def("hist1")),
    fluidRow(eqdem_3_choices("hist1")),
    fluidRow(eqdem_graph("hist1"))
  )

  server <- function(input, output, session) {
    eqdem_server("hist1")
  }
  shinyApp(ui, server)
}

tabordApp()

