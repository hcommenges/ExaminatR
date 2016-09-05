library(shiny)

shinyUI(fluidPage(theme = "darkGrey.css",
                  tags$head(tags$link(rel="shortcut icon", href="favicon.png"),
                            includeScript("www/analytics.js")),
  
  # TITLE PANEL ----
  
  titlePanel("ExaminatR : générateur automatique de sujets d'examen"),
  
  sidebarLayout(
    
    # SIDEBAR PANEL ----
    
    sidebarPanel(
      tags$strong("GUIDE D'UTILISATION"),
      htmlOutput("citation"),
      tags$hr(),
      tags$strong("PRODUIRE LE DOCUMENT"),
      radioButtons("docformat", "", c("pdf", "odt", "docx"),
                   inline = TRUE),
      downloadButton("report")
    ),
    
    
    # MAIN PANEL ----
    
    mainPanel(
      tags$hr(),
      # Titre
      tags$h4("Charger les données"),
      tags$hr(),
      
      # Description du sujet
      checkboxInput("checkloadtab", "Charger le tableau (requis)", FALSE),
      conditionalPanel(
        condition = "input.checkloadtab == true",
        checkboxInput("csvSettings", "Options du format CSV", FALSE),
        conditionalPanel(
          condition = "input.csvSettings == true",
          radioButtons("sepcol", "Separateur de colonnes",
                       c(Virgule = ",",
                         Point_virgule = ";",
                         Tabulation = "\t"),
                       ","),
          radioButtons("sepdec", "Separateur décimal",
                       c(Point = ".",
                         Virgule = ","),
                       "."),
          radioButtons("quote", "Guillemets",
                       c(None = "",
                         "Double Quote" = '"',
                         "Single Quote" = "'"),
                       '"')
        ),
        fileInput("file1", "",
                  accept = c("text/csv", 
                             "text/comma-separated-values,text/plain", 
                             ".csv")),
        selectInput("idtab", 
                    "Variable identifiant", 
                    choices = "",
                    selected = "", 
                    multiple = FALSE,
                    width = "50%",
                    selectize = TRUE)
      ),
      tags$hr(),
      
      # Charger le shape
      checkboxInput("checkloadshape", "Charger le fond de carte (optionnel)", FALSE),
      conditionalPanel(
        condition = "input.checkloadshape == true",
      fileInput("file2", "",
                accept = c("application/zip", 
                           "application/x-gzip", 
                           ".zip")),
      selectInput("idshape",
                  "Variable identifiant", 
                  choices = "",
                  selected = "", 
                  multiple = FALSE,
                  width = "50%",
                  selectize = TRUE)),
      tags$hr(),
      
      tags$h4("Décrire le sujet et les données"),
      tags$hr(),

      # Description du sujet
      checkboxInput("checkdescrsujet", "Description du sujet", FALSE),
      conditionalPanel(
        condition = "input.checkdescrsujet == true",
        textInput("nomepreuve", "Entrer le nom de l'épreuve", value = "", width = "100%"),
        textInput("nomsujet", "Entrer le sujet de l'épreuve", value = "", width = "100%"),
        textInput("descrsujet", "Entrer la description du sujet", value = "", width = "100%")
      ),
      tags$hr(),
      
      # Description des données
      checkboxInput("checkdescrdonnees", "Description des données", FALSE),
      conditionalPanel(
        condition = "input.checkdescrdonnees == true",
        selectInput("vartot", 
                    "Choisir les variables à décrire", 
                    choices = "",
                    selected = "", 
                    multiple = TRUE,
                    selectize = TRUE,
                    width = "100%"),
        textInput("descrdonnees", "Entrer la description des variables (séparées par des virgules)", value = "", width = "100%")
      ),
      tags$hr(),
      
      # Titre
      tags$h4("Choisir les analyses à effectuer"),
      tags$hr(),
      
      # Analyse univariée
      checkboxInput("checkunivar", "Analyse univariée", FALSE),
      conditionalPanel(
        condition = "input.checkunivar == true",
        selectInput("varuni", 
                    "Choisir les variables à utiliser", 
                    choices = "",
                    selected = "", 
                    multiple = TRUE,
                    selectize = TRUE)
      ),
      tags$hr(),
      
      # Analyse bivariée
      checkboxInput("checkbivar", "Analyse bivariée", FALSE),
      conditionalPanel(
        condition = "input.checkbivar == true",
        fluidRow(
          column(4, 
                 checkboxInput("checkqualiquali", "Contingence (quali-quali)", FALSE),
                 selectInput("varqualiquali", 
                             "Choisir les variables (Y puis X)", 
                             choices = "",
                             selected = "", 
                             multiple = TRUE,
                             selectize = TRUE)),
          column(4, 
                 checkboxInput("checkquantiquanti", "Régression (quanti-quanti)", FALSE),
                 selectInput("varquantiquanti", 
                             "Choisir les variables (Y puis X)", 
                             choices = "",
                             selected = "", 
                             multiple = TRUE,
                             selectize = TRUE)),
          column(4, 
                 checkboxInput("checkqualiquanti", "ANOVA (quali-quanti)", FALSE),
                 selectInput("varqualiquanti", 
                             "Choisir les variables (Y puis X)", 
                             choices = "",
                             selected = "", 
                             multiple = TRUE,
                             selectize = TRUE)))
      ),
      tags$hr(),
      
      # Analyse trivariée
      checkboxInput("checktrivar", "Analyse trivariée", FALSE),
      conditionalPanel(
        condition = "input.checktrivar == true",
        fluidRow(
          column(4, 
                 checkboxInput("checkregress2", "Régression (X1 et X2 quanti)", FALSE),
                 selectInput("varregress2", 
                             "Choisir les variables (Y, X1, X2)", 
                             choices = "",
                             selected = "", 
                             multiple = TRUE,
                             selectize = TRUE),
                 radioButtons("interactregress2", 
                              label = "Interaction X1-X2", 
                              choices = c("Sans interaction" = FALSE, "Avec interaction" = TRUE), 
                              selected = FALSE, 
                              inline = TRUE)),
          column(4, 
                 checkboxInput("checkanova2", "ANOVA (X1 et X2 quali)", FALSE),
                 selectInput("varanova2", 
                             "Choisir les variables (Y, X1, X2)", 
                             choices = "",
                             selected = "", 
                             multiple = TRUE,
                             selectize = TRUE),
                 radioButtons("interactanova2", 
                              label = "Interaction X1-X2", 
                              choices = c("Sans interaction" = FALSE, "Avec interaction" = TRUE), 
                              selected = FALSE, 
                              inline = TRUE)),
          column(4, 
                 checkboxInput("checkancova", "ANCOVA (X1 quanti, X2 quali)", FALSE),
                 selectInput("varancova", 
                             "Choisir les variables (Y, X1, X2)", 
                             choices = "",
                             selected = "", 
                             multiple = TRUE,
                             selectize = TRUE),
                 radioButtons("interactancova", 
                              label = "Interaction X1-X2", 
                              choices = c("Sans interaction" = FALSE, "Avec interaction" = TRUE), 
                              selected = FALSE, 
                              inline = TRUE)))
      ),
      tags$hr(),
      
      # Analyse en composantes principales
      checkboxInput("checkacp", "Analyse en composantes principales", FALSE),
      conditionalPanel(
        condition = "input.checkacp == true",
        fluidRow(
          column(12, 
                 selectInput("varacp", 
                             "Choisir les variables de l'analyse (n quanti)", 
                             choices = "",
                             selected = "", 
                             multiple = TRUE,
                             selectize = TRUE,
                             width = "100%"),
                 radioButtons("axisacp", 
                              label = "Nombre d'axes à conserver", 
                              choices = c("2 axes" = 2, "3 axes" = 3, "4 axes" = 4), 
                              selected = 2, 
                              inline = TRUE),
                 radioButtons("printlabel",
                              label = "Etiqueter les observations",
                              choices = c("Non (affichage des points)" = FALSE, "Oui (affichage des identifiants)" = TRUE), 
                              selected = FALSE, 
                              inline = TRUE))
        )),
      tags$hr(),
      
      # Classification
      checkboxInput("checkcah", "Classification", FALSE),
      conditionalPanel(
        condition = "input.checkcah == true",
        fluidRow(
          column(12, 
                 selectInput("varcah", 
                             "Choisir les variables de l'analyse (n quanti)", 
                             choices = "",
                             selected = "", 
                             multiple = TRUE,
                             selectize = TRUE,
                             width = "100%"),
                 sliderInput("nbclus", 
                             label = "Nombre de classes",
                             min = 2, 
                             max = 12,
                             value = 2, 
                             step = 1))
        )),
      tags$hr(),
      
      # Cartographie
      checkboxInput("checkcarto", "Cartographie", FALSE),
      conditionalPanel(
        condition = "input.checkcarto == true",
        checkboxInput("checkcartovar", "Cartographier des variables du tableau", FALSE),
        conditionalPanel(
          condition = "input.checkcartovar == true",
          fluidRow(
            column(4,
                   selectInput("varcarto1", 
                               "Variable", 
                               choices = "",
                               selected = "", 
                               multiple = FALSE,
                               selectize = TRUE),
                   selectInput("varcarto2", 
                               "Variable", 
                               choices = "",
                               selected = "", 
                               multiple = FALSE,
                               selectize = TRUE)),
            column(4,
                   selectInput("discret1", 
                               "Discrétisation", 
                               choices = c("Quantiles" = "quantile",
                                           "Seuils naturels" = "fisher-jenks"),
                               selected = "", 
                               multiple = FALSE,
                               selectize = TRUE),
                   selectInput("discret2", 
                               "Discrétisation", 
                               choices = c("Quantiles" = "quantile",
                                           "Seuils naturels" = "fisher-jenks"),
                               selected = "", 
                               multiple = FALSE,
                               selectize = TRUE)),
            column(4,
                   numericInput("nbcl1", 
                                label = "Nombre de classes",
                                min = 2, 
                                max = 12,
                                value = 4, 
                                step = 1),
                   numericInput("nbcl2", 
                                label = "Nombre de classes",
                                min = 2, 
                                max = 12,
                                value = 4, 
                                step = 1)
            ))
          
        ),
        tags$hr(),
        checkboxInput("checkcartoana", "Cartographier des résultats d'analyse", FALSE),
        conditionalPanel(
          condition = "input.checkcartoana == true",
          fluidRow(
            column(4,
                   selectInput("varcarto3", 
                               "Résultats d'analyse", 
                               choices = c("ø" = "ø",
                                           "Régression (Y~X1)" = "reg1",
                                           "Régression (Y~X1+X2)" = "reg2",
                                           "Coordonnées factorielles (ACP)" = "acp",
                                           "Classes (CAH)" = "cah"),
                               selected = "", 
                               multiple = FALSE,
                               selectize = TRUE),
                   selectInput("varcarto4", 
                               "Résultats d'analyse", 
                               choices = c("ø" = "ø",
                                           "Régression (Y~X1)" = "reg1",
                                           "Régression (Y~X1+X2)" = "reg2",
                                           "Coordonnées factorielles (ACP)" = "acp",
                                           "Classes (CAH)" = "cah"),
                               selected = "", 
                               multiple = FALSE,
                               selectize = TRUE)),
            column(4,
                   numericInput("nbcl3", 
                                label = "Nombre de classes",
                                min = 2, 
                                max = 12,
                                value = 4, 
                                step = 1),
                   numericInput("nbcl4", 
                                label = "Nombre de classes",
                                min = 2, 
                                max = 12,
                                value = 4, 
                                step = 1))
          ))
      ),
      tags$hr(),
      checkboxInput("checkdistrib", "Statistiques de test (F, T, Chi2)", FALSE),
      conditionalPanel(
        condition = "input.checkdistrib == true",
        checkboxInput("checkdistribchi", "Distributions de la statistique Chi2", FALSE),
        conditionalPanel(
          condition = "input.checkdistribchi == true",
          fluidRow(
            column(6, numericInput("chidf", label = "Degrés de liberté", value = 4, min = 1, max = 100, step = 1)),
            column(6, numericInput("chialpha", label = "Seuil alpha", value = 0.05, min = 0.01, max = 0.99, step = 0.01))
          )
        ),
        checkboxInput("checkdistribt", "Distributions de la statistique T", FALSE),
        conditionalPanel(
          condition = "input.checkdistribt == true",
          fluidRow(
            column(6, numericInput("tdf", label = "Degrés de liberté", value = 30, min = 1, max = 1000, step = 1)),
            column(6, numericInput("talpha", label = "Seuil alpha", value = 0.05, min = 0.01, max = 0.99, step = 0.01))
          )
        ),
        checkboxInput("checkdistribf", "Distributions de la statistique F", FALSE),
        conditionalPanel(
          condition = "input.checkdistribf == true",
          fluidRow(
            column(4, numericInput("fdf1", label = "Degrés de liberté (1)", value = 5, min = 1, max = 100, step = 1)),
            column(4, numericInput("fdf2", label = "Degrés de liberté (2)", value = 30, min = 1, max = 1000, step = 1)),
            column(4, numericInput("falpha", label = "Seuil alpha", value = 0.05, min = 0.01, max = 0.99, step = 0.01))
          )
        )
      ),
      tags$hr()
    )
  )
))

