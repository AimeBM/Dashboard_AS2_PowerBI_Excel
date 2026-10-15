library(shiny)
library(readxl)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(ggplot2)
library(plotly)



# Changer le repertoire des données
setwd("C:/Users/Tchakounte/Documents/Blanchard/Tableau de bord")
addResourcePath("images", "./dashboard")

# Charger les données
base1 <- read_excel("Base de données tableau de bord.xlsx", sheet = "BD_1")
base2 <- read_excel("Base de données tableau de bord.xlsx", sheet = "BD_2")

# Fusionner les bases de données en ajoutant les variables de la base2 dans la base1
base_donnees <- merge(base1, base2, by = "PRODUCT ID")


# Créer les nouvelles colonnes de calcul
base_donnees <- base_donnees %>%
  mutate(
    MONTANT_VENDU = QUANTITY * `SELLING PRICE`,
    MONTANT_ACHETE = QUANTITY * `BUYING PRIZE`,
    BENEFICE = MONTANT_VENDU - MONTANT_ACHETE
  )
# Définir l'interface utilisateur Shiny
ui <- fluidPage(
  # Styles CSS personnalisés pour la barre de titre et le cadre des filtres
  tags$head(
    tags$style(HTML('
      .dashboard-title {
        background-color: #ffffff;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        border: 2px solid #007bff;
        color: #007bff;
        font-size: 44px;
        text-align: center;
        padding: 10px;
      }
      .dashboard-title h1 {
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        border: 2px solid blue;
        color: blue;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);
        font-size: 44px;
        margin: 0;
      }
      .filter-panel {
        background-color: #007bff;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        padding: 10px;
        margin-bottom: 20px;
      }
      .filter-panel .filter-item {
        display: flex;
        align-items: center;
        color: #ffffff;
        font-weight: bold;
        font-size: 14px;
        margin-bottom: 10px;
      }
      .filter-panel .filter-item img {
        margin-right: 5px;
      }
      .filter-panel .filter-item .input-label {
        margin-bottom: 0;
        margin-top: 5px;
        }
      .custom-box {
        border: 2px solid #007bff;
        border-radius: 5px;
        overflow: hidden;
        margin-right: 0;
        box-shadow: 4px 4px 8px rgba(0, 0, 0, 0.6);
      }
      .custom-box .box-icon {
        display: inline-block;
        vertical-align: middle;
        color: #ffffff;
        padding: 0;
        margin-right: 0;
      }
      .custom-box .box-label {
        display: inline-block;
        vertical-align: middle;
        margin-left: 10px;
        font-weight: bold;
        font-size: 16px;
        color: #007bff;
      }
      .custom-box .box-content {
        background-color: #007bff;
        padding: 10px;
        box-shadow: 4px 4px 8px rgba(0, 0, 0, 0.6);
        font-weight: bold;
        font-size: 16px;
        color: #FFFFFF;
      }
    '))
  ),
  
  fluidRow(
    column(
      width = 3,
      align = "left",
      img(src = "images/IMG-20230619-WA0012.jpg", width = 220, height = 100),
      img(src = "images/360_F_308455353_kNs9gRNlQuWLUwqFMgdmE0HKZy2boWfO.jpg", width = 100, height = 100)
    ),
    column(
      width = 6,
      align = "center",
      h1("DASHBOARD", class = "dashboard-title")
    ),
    column(
      width = 3,
      align = "right",
      img(src = "images/360_F_308455353_kNs9gRNlQuWLUwqFMgdmE0HKZy2boWfO.jpg", width = 100, height = 100),
      img(src = "images/IMG-20230619-WA0012.jpg", width = 220, height = 100)
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      align = "center",
      div(class = "filter-panel",
          fluidRow(
            column(
              width = 1,
              align = "center",
              div(class = "filter-item",
                  img(src = "images/2693560.png", width = 30, height = 30),
                  p(class = "input-label", "ANNEE")
              ),
              selectInput(inputId = "annee_filter", label = NULL, choices = c("Tous", unique(format(base_donnees$DATE, "%Y"))), selected = NULL)
            ),
            column(
              width = 2,
              align = "center",
              div(class = "filter-item",
                  img(src = "images/2693560.png", width = 30, height = 30),
                  p(class = "input-label", "MOIS")
              ),
              selectInput(inputId = "mois_filter", label = NULL, choices = c("Tous", unique(format(base_donnees$DATE, "%B"))), selected = NULL)
            ),
            column(
              width = 2,
              align = "center",
              div(class = "filter-item",
                  img(src = "images/moyen-paiement.jpg", width = 30, height = 30),
                  p(class = "input-label", "MODE DE PAIEMENT")
              ),
              selectInput(inputId = "payment_mode_filter", label = NULL, choices = c("Tous", unique(base_donnees$'PAYMENT MODE')), selected = NULL)
            ),
            column(
              width = 2,
              align = "center",
              div(class = "filter-item",
                  img(src = "images/sale.png", width = 30, height = 30),
                  p(class = "input-label", "TYPE")
              ),
              selectInput(inputId = "sale_type_filter", label = NULL, choices = c("Tous", unique(base_donnees$'SALE TYPE')), selected = NULL)
            ),
            column(
              width = 3,
              align = "center",
              div(class = "filter-item",
                  img(src = "images/10861192.jpg", width = 30, height = 30),
                  p(class = "input-label", "CATEGORIE DES PRODUITS")
              ),
              selectInput(inputId = "category_filter", label = NULL, choices = c("Tous", unique(base_donnees$CATEGORY)), selected = NULL)
            ),
            column(
              width = 2,
              align = "center",
              div(class = "filter-item",
                  img(src = "images/10861192.jpg", width = 30, height = 30),
                  p(class = "input-label", "PRODUITS")
              ),
              selectInput(inputId = "product_filter", label = NULL, choices = c("Tous", unique(base_donnees$PRODUCT)), selected = NULL)
            )
          )
      )
    )
  ),
  
  mainPanel(
    fluidRow(
      column(
        width = 3,
        align = "center",
        div(
          class = "custom-box",
          div(
            class = "box-icon",
            img(src = "images/beneficeNet.jpg", width = 30, height = 30)
          ),
          div(
            class = "box-label",
            "Chiffre d'affaires"
          ),
          div(
            class = "box-content",
            textOutput("chiffre_affaires")
          )
        )
      ),

    column(
      width = 3,
      align = "center",
      div(
        class = "custom-box",
        div(
          class = "box-icon",
          img(src = "images/profit.jpg", width = 30, height = 30)
        ),
        div(
          class = "box-label",
          "Bénéfice Total"
        ),
        div(
          class = "box-content",
          textOutput("total_benefice")
        )
      )
    ),
  
    column(
      width = 3,
      align = "center",
      div(
        class = "custom-box",
        div(
          class = "box-icon",
          img(src = "images/panier.jpg", width = 30, height = 30)
        ),
        div(
          class = "box-label",
          "Panier moyen"
        ),
        div(
          class = "box-content",
          textOutput("panier_moyen")
        )
      )
    ),
    column(
      width = 3,
      align = "center",
      div(
        class = "custom-box",
        div(
          class = "box-icon",
          img(src = "images/achat.png", width = 30, height = 30)
        ),
        div(
          class = "box-label",
          "Coût d'achat"
        ),
        div(
          class = "box-content",
          textOutput("cout_achat"),
          
        )
      )
    ),   
    
    ),
    
    tags$hr(style = "margin-top: 0.25cm; margin-bottom: 0.25cm;"),
    fluidRow(
      column(
        width = 3,
        align = "center",
        div(
          class = "custom-box",
          div(
            class = "box-icon",
            img(src = "images/taux.jpg", width = 30, height = 30)
          ),
          div(
            class = "box-label",
            "Taux de Croissance"
          ),
          div(
            class = "box-content",
            uiOutput("taux_croissance_progress")
          )
        )
      ),
      
      column(
        width = 3,
        align = "center",
        div(
          class = "custom-box",
          div(
            class = "box-icon",
            img(src = "images/illustration-3d-verification-finances_785462-182.jpg", width = 30, height = 30)
          ),
          div(
            class = "box-label",
            "Transactions"
          ),
          div(
            class = "box-content",
            textOutput("nombre_transactions"),
            
          )
        )
      ), 
      
      column(
        width = 3,
        align = "center",
        div(
          class = "custom-box",
          div(
            class = "box-icon",
            img(src = "images/achat-ethylotest-grande-quantite.jpg", width = 30, height = 30)
          ),
          div(
            class = "box-label",
            "Quantité Vendue"
          ),
          div(
            class = "box-content",
            textOutput("quantite_vendue"),
            
          )
        )
      ),
      
      column(
        width = 3,
        align = "center",
        div(
          class = "custom-box",
          div(
            class = "box-icon",
            img(src = "images/pourcentage de remise.png", width = 30, height = 30)
            
          ),
          div(
            class = "box-label",
            "Remise Moyenne"
          ),
          div(
            class = "box-content",
            plotlyOutput("remise_moyenne_plot", height = "100px"),
            
          )
        )
      ),
      
    ),
    
    tags$hr(style = "margin-top: 0.25cm; margin-bottom: 0.25cm;"),
    fluidRow(
      column(
        width = 5,
        align = "center",
        div(
          class = "custom-box",
          div(
            class = "box-icon",
            img(src = "images/hand-put-coin-bag-with-money_33882-60.jpg", width = 30, height = 30)
          ),
          div(
            class = "box-label",
            "Ratio de Contribution par Catégorie"
          ),
          div(
            class = "box-content",
            
            plotOutput("graphique_circulaire"),
            
            
          )
        )
      ),
      column(
        width = 7,
        align = "center",
        div(
          class = "custom-box",
          div(
            class = "box-icon",
            img(src = "images/2588403-calendrier-rappel-avec-pieces-dollars-flat-style-icon-gratuit-vectoriel.jpg", width = 30, height = 30)
          ),
          div(
            class = "box-label",
            "Chiffre d'Affaire et Bénéfice en fonction du temps"
          ),
          div(
            class = "box-content",
            plotOutput("graphique_courbe")
            
            
          )
        )
      )
      
      
),


tags$hr(style = "margin-top: 0.25cm; margin-bottom: 0.25cm;"),
fluidRow(
  column(
    width = 12,
    align = "center",
    div(
      class = "custom-box",
      div(
        class = "box-icon",
        img(src = "images/part du marche.jpg", width = 30, height = 30)
      ),
      div(
        class = "box-label",
        "Ratio de Contribution par Produit"
      ),
      div(
        class = "box-content",
        style = "height: 300px; overflow-y: scroll;",
        plotOutput("graphique_barres")
      )
    )
  )
  
  
),
    
  )
)


# Définir le serveur Shiny
server <- function(input, output) {
  filtered_data <- reactive({
    data <- base_donnees
    
    if (input$annee_filter != "Tous") {
      data <- filter(data, year(DATE) == as.integer(input$annee_filter))
    }
    
    if (input$mois_filter != "Tous") {
      mois <- format(data$DATE, "%B")
      data <- data[mois == input$mois_filter, ]
    }
    
    
    if (input$payment_mode_filter != "Tous") {
      data <- filter(data, `PAYMENT MODE` == input$payment_mode_filter)
    }
    
    if (input$sale_type_filter != "Tous") {
      data <- filter(data, `SALE TYPE` == input$sale_type_filter)
      
    }
    
    if (input$category_filter != "Tous") {
      data <- filter(data, CATEGORY == input$category_filter)
    }
    
    if (input$product_filter != "Tous") {
      data <- filter(data, PRODUCT == input$product_filter)
    }
    
    return(data)
  })
  
  filtered_data1 <- reactive({
    data <- base_donnees
    
    if (input$mois_filter != "Tous") {
      mois <- format(data$DATE, "%B")
      data <- data[mois == input$mois_filter, ]
    }
    
    
    if (input$payment_mode_filter != "Tous") {
      data <- filter(data, `PAYMENT MODE` == input$payment_mode_filter)
    }
    
    if (input$sale_type_filter != "Tous") {
      data <- filter(data, `SALE TYPE` == input$sale_type_filter)
      
    }
    
    if (input$category_filter != "Tous") {
      data <- filter(data, CATEGORY == input$category_filter)
    }
    
    if (input$product_filter != "Tous") {
      data <- filter(data, PRODUCT == input$product_filter)
    }
    
    return(data)
  })
  
  filtered_data2 <- reactive({
    data <- base_donnees
    
    if (input$annee_filter != "Tous") {
      data <- filter(data, year(DATE) == as.integer(input$annee_filter))
    }
    
    if (input$mois_filter != "Tous") {
      mois <- format(data$DATE, "%B")
      data <- data[mois == input$mois_filter, ]
    }
    
    
    if (input$payment_mode_filter != "Tous") {
      data <- filter(data, `PAYMENT MODE` == input$payment_mode_filter)
    }
    
    if (input$sale_type_filter != "Tous") {
      data <- filter(data, `SALE TYPE` == input$sale_type_filter)
      
    }
    
    return(data)
  }) 
  
  filtered_data3 <- reactive({
    data <- base_donnees
    
    if (input$annee_filter != "Tous") {
      data <- filter(data, year(DATE) == as.integer(input$annee_filter))
    }
    
    if (input$mois_filter != "Tous") {
      mois <- format(data$DATE, "%B")
      data <- data[mois == input$mois_filter, ]
    }
    
    
    if (input$payment_mode_filter != "Tous") {
      data <- filter(data, `PAYMENT MODE` == input$payment_mode_filter)
    }
    
    if (input$sale_type_filter != "Tous") {
      data <- filter(data, `SALE TYPE` == input$sale_type_filter)
      
    }
    
    if (input$category_filter != "Tous") {
      data <- filter(data, CATEGORY == input$category_filter)
    }
    
    return(data)
  }) 
  
  output$chiffre_affaires <- renderText({
    filtered_data <- filtered_data()  # Récupérer les données filtrées
    
    # Calculer le chiffre d'affaires en fonction des filtres
    chiffre_affaires <- sum(filtered_data$MONTANT_VENDU)
    
    # Formater le chiffre d'affaires avec des séparateurs de milliers
    chiffre_affaires_format <- format(chiffre_affaires, big.mark = " ")
    
    # Retourner le chiffre d'affaires formaté
    paste(chiffre_affaires_format, "€")
  })

output$taux_croissance_progress <- renderUI({
  filtered_data <- filtered_data1()
  # Extraire l'année de la variable "DATE" pour montant_2022
  montant_2022 <- sum(filtered_data[year(filtered_data$DATE) == 2022, "MONTANT_VENDU"])
  
  # Extraire l'année de la variable "DATE" pour montant_2021
  montant_2021 <- sum(filtered_data[year(filtered_data$DATE) == 2021, "MONTANT_VENDU"])
  
  # Calculer la croissance en utilisant les montants calculés
  croissance <- round((montant_2022 - montant_2021) / montant_2021 * 100, 2)
  
  
  progress <- croissance  # Utiliser la valeur de croissance directement
  
  progress_bar <- tags$div(
    class = "progress-bar",
    style = paste0("width: ", abs(progress)/2, "%; background-color: ", ifelse(progress < 0, "red", "green")),
  )
  
  progress_label <- tags$div(
    class = "progress-label",
    style = "color: black",
    paste0(progress, "%")
  )
  
  tags$div(
    class = "progress",
    style = "height: 30px; margin-bottom: 10px;",
    progress_bar,
    progress_label
  )
})

  
  output$total_benefice <- renderText({
  filtered_data <- filtered_data()  # Récupérer les données filtrées
  
  benefice_total <- sum(filtered_data$BENEFICE, na.rm = TRUE)
  
  # Formater le bénéfice avec des séparateurs de milliers
  benefice_total_format <- format(benefice_total, big.mark = " ")
  
  # Retourner le bénéfice formaté
  paste(benefice_total_format, "€")
})
  
output$panier_moyen <- renderText({
  filtered_data <- filtered_data()  # Récupérer les données filtrées
  
  chiffre_affaires <- sum(filtered_data$MONTANT_VENDU, na.rm = TRUE)
  nombre_transactions <- nrow(filtered_data)  # Ou utiliser n_distinct(filtered_data$CLIENT) pour le nombre de clients distincts
  
  panier_moyen <- chiffre_affaires / nombre_transactions
  
  # Formater le panier moyen avec des séparateurs de milliers
  panier_moyen_format <- format(panier_moyen, big.mark = " ")
  
  # Retourner le panier moyen formaté
  paste(panier_moyen_format, "€")
})

output$cout_achat <- renderText({
  filtered_data <- filtered_data()  # Récupérer les données filtrées
  
  cout_achat <- sum(filtered_data$MONTANT_ACHETE, na.rm= TRUE)
  
  cout_achat_format <- format(cout_achat, big.mark = " ")
  
  # Retourner le coût d'achat formaté
  paste(cout_achat_format, "€")
})


output$graphique_circulaire <- renderPlot({
  filtered_data <- filtered_data2()  # Récupérer les données filtrées
  
  # Calculer la somme de vente par catégorie
  somme_vente <- aggregate(data = filtered_data, MONTANT_VENDU ~ CATEGORY, sum)
  
  # Calculer le pourcentage de chaque catégorie
  somme_vente$pourcentage <- somme_vente$MONTANT_VENDU / sum(somme_vente$MONTANT_VENDU) * 100
  
  # Créer le graphique circulaire avec les pourcentages à l'intérieur
  ggplot(somme_vente, aes(x = "", y = MONTANT_VENDU, fill = CATEGORY)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0, clip = "off") +
    geom_text(aes(label = paste0(round(pourcentage), "%")), position = position_stack(vjust = 0.5)) +
    theme_void() +
    theme(legend.position = "bottom", legend.direction = "horizontal",
          plot.margin = margin(-0.2, -0.2, -0.2, -0.2)) +
    guides(fill = guide_legend(ncol = 2)) +
    scale_fill_brewer(palette = "Set3")
})


output$quantite_vendue <- renderText({
  filtered_data <- filtered_data()  # Récupérer les données filtrées
  quantite_vendue <- sum(filtered_data$QUANTITY)
  quantite_vendue
})


output$nombre_transactions <- renderText({
  filtered_data <- filtered_data()  # Récupérer les données filtrées
  nombre_transactions <- nrow(filtered_data)
  nombre_transactions
})


output$remise_moyenne_plot <- renderPlotly({
  filtered_data <- filtered_data()  # Récupérer les données filtrées
  
  # Calcul de la remise moyenne
  remise_moyenne <- mean(filtered_data$DISCOUNT)
  
  # Création du graphique d'anneau
  plot_data <- data.frame(Category = c("Remise moyenne", "Remise restante"),
                          Value = c(remise_moyenne, 100 - remise_moyenne))
  
  plot_ly(plot_data, labels = ~Category, values = ~Value, type = "pie",
          marker = list(colors = c("#2ecc71", "#FFFFFF")),
          showlegend = FALSE,
          textinfo = "none")%>%
    layout(
      margin = list(l = 0, r = 0, t = 0, b = 0)
    )
})


output$graphique_courbe <- renderPlot({
  filtered_data <- filtered_data()  # Récupérer les données filtrées
  
  # Convertir la colonne DATE en format de classe "Date"
  filtered_data$DATE <- as.Date(filtered_data$DATE)
  
  # Calculer les sommes des montants vendus et des bénéfices par date
  somme_montant_vendu <- aggregate(data = filtered_data, MONTANT_VENDU ~ DATE, sum)
  somme_benefice <- aggregate(data = filtered_data, BENEFICE ~ DATE, sum)
  
  # Fusionner les deux dataframes
  data_merged <- merge(somme_montant_vendu, somme_benefice, by = "DATE", all = TRUE)
  
  # Transformer les valeurs manquantes en zéro
  data_merged[is.na(data_merged)] <- 0
  
  # Créer le graphique en courbes empilées
  ggplot(data_merged, aes(x = DATE)) +
    geom_area(aes(y = MONTANT_VENDU, fill = "CA"), color = "black") +
    geom_area(aes(y = BENEFICE, fill = "Bénéfice"), color = "black") +
    scale_fill_manual(values = c("CA" = "blue", "Bénéfice" = "green")) +
    labs(x = "Date", y = "Somme") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    ) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_date(date_breaks = "15 days", date_labels = "%d/%m/%Y") +
    scale_y_continuous(breaks = seq(0, ceiling(max(data_merged$MONTANT_VENDU, data_merged$BENEFICE)), 500),
                       minor_breaks = seq(0, ceiling(max(data_merged$MONTANT_VENDU, data_merged$BENEFICE)), 500),
                       expand = c(0, 0))
})


output$graphique_barres <- renderPlot({
  filtered_data <- filtered_data3()  # Récupérer les données filtrées
  
  # Calculer la somme de vente par produit
  somme_vente <- aggregate(data = filtered_data, MONTANT_VENDU ~ PRODUCT, sum)
  
  # Calculer le pourcentage de chaque catégorie
  somme_vente$pourcentage <- somme_vente$MONTANT_VENDU / sum(somme_vente$MONTANT_VENDU)
  
  # Trier les catégories par ordre décroissant de vente
  somme_vente <- somme_vente[order(-somme_vente$MONTANT_VENDU), ]
  
  # Créer le graphique à barres horizontales avec pourcentage
  ggplot(somme_vente, aes(x = pourcentage, y = reorder(PRODUCT, MONTANT_VENDU), fill = PRODUCT)) +
    geom_bar(width = 0.5, stat = "identity", position = position_dodge(width = 1)) +
    labs(x = "Pourcentage de vente", y = "Produit") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +

    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "white"),
          panel.background = element_blank())
})












  

  
  
  
  
  
  
  
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)
