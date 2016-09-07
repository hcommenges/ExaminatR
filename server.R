shinyServer(function(session, input, output) {
  
  # READ DATA ----
  
  readData <- reactive({
    req(input$file1)
    mytable <- read.csv(input$file1$datapath, header = TRUE, sep = input$sepcol, dec = input$sepdec, quote = input$quote)
    return(mytable)
  })
  
  observe({
    req(readData())
    columnList <- colnames(readData())
    print(str(columnList))
   
    updateSelectInput(session = session,
                      inputId = "vartot",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "varuni",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "varqualiquali",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "varquantiquanti",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "varqualiquanti",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "varregress2",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "varanova2",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "varancova",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "varacp",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "varacpid",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "varcah",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "varcarto1",
                      choices = c("ø", columnList))
    updateSelectInput(session = session,
                      inputId = "varcarto2",
                      choices = c("ø", columnList))
    updateSelectInput(session = session,
                      inputId = "idtab",
                      choices = c("ø", columnList))
  })
  
  observe({
    req(readSpatialData())
    columnListShape <- colnames(as.data.frame(readSpatialData()))
    
    updateSelectInput(session = session,
                      inputId = "idshape",
                      choices = c("ø", columnListShape))
    
  })
  
  readSpatialData <- reactive({
    req(input$file2)
    oriDir <- getwd()
    setwd(tempdir())
    unzip(zipfile = input$file2$datapath, overwrite = TRUE, exdir = "shpdir")
    fileName <- list.files("shpdir")[1]
    layerName <- substr(fileName, start = 1, stop = nchar(fileName) - 4)
    spObject <- readOGR(dsn = "shpdir", layer = layerName, stringsAsFactors = FALSE)
    setwd(oriDir)
    return(spObject)
    }
  )
  
  
  # OUTPUTS ----
  
  # Guide
  output$citation <- renderText("
     <strong>1. Charger les données</strong> <br/>
 <ul>
  <li>Le tableau est requis, il doit être au format CSV avec les bons paramètres (séparateurs).</li>
  <li>Le fond de carte est optionnel, il doit être au format Esri Shape dans un dossier zippé (ZIP).</li>
  <li>L'identifiant est requis pour la jointure entre les deux fichiers mais aussi quand un tableau seul est fourni.</li>
</ul> 

     <strong>2. Décrire le sujet et les données</strong> <br/>
 <ul>
  <li>Cette étape est optionnelle.</li>
  <li>Par défaut ce sont les intitulés de colonne du tableau qui sont utilisés dans les sorties numériques et graphiques.</li>
</ul> 

     <strong>3. Choisir les analyses à effecture</strong> <br/>
 <ul>
  <li>Les analyses bi- et tri-variées correspondent à des modèles de régression linéaire avec ajustement des moindres carrés.</li>
  <li>L'analyse en composantes principales est calculée sur des variables automatiquement standardisées (ACP normée).</li>
  <li>La classification est une classification ascendante hiérarchique avec distance euclidienne et critère de Ward sur variables standardisées.</li>
  <li>La cartographie peut être effectuée sur des variables initiales ou sur des résultats d'analyse. 
Sur les résultats d'analyses la discrétisation se fait par défaut selon moyenne et écart-type.</li>
  <li>Les statistiques de test affichent les valeurs seuils de trois lois (F, T, Chi2) avec les probabilités et les degrés de liberté choisis.</li>
</ul> 

     <strong>4. Télécharger le document au format voulu</strong> <br/>
 <ul>
  <li>Choisir le format (PDF, ODT, DOCX) et télécharger.</li>
</ul> 
                                ")
  
  
  
  # Report
  output$report <- downloadHandler(
    filename = function() {paste("exam.", sep = "", switch(input$docformat, pdf = "pdf", odt = "odt", docx = "docx"))},
    content = function(file){
      if(input$docformat == "pdf"){
        knit2pdf("exam.Rnw", compiler = "pdflatex")
        file.copy("exam.pdf", file)
        file.remove("exam.pdf", "exam.tex",
                    "exam.aux", "exam.log")
        unlink("figure", recursive = TRUE)
      } else if (input$docformat == "odt"){
        knit2pdf("exam.Rnw", compiler = "pdflatex")
        setwd("figure/")
        file.rename(from = list.files(), to = sapply(list.files(), FUN = function(x) substr(x = x, start = 1, stop = nchar(x) - 4)))
        setwd("..")
        pandoc(input = "exam.tex", format = "odt")
        file.copy("exam_utf8.odt", file)
        file.remove("exam_utf8.odt", 
                    "exam.pdf", "exam.tex",
                    "exam.aux", "exam.log")
        unlink("figure", recursive = TRUE)
      } else if (input$docformat == "docx"){
        knit2pdf("exam.Rnw", compiler = "pdflatex")
        setwd("figure/")
        file.rename(from = list.files(), to = sapply(list.files(), FUN = function(x) substr(x = x, start = 1, stop = nchar(x) - 4)))
        setwd("..")
        pandoc(input = "exam.tex", format = "docx")
        file.copy("exam_utf8.docx", file)
        file.remove("exam_utf8.docx", 
                    "exam.pdf", "exam.tex",
                    "exam.aux", "exam.log")
        unlink("figure", recursive = TRUE)
      }
    }
  )
  
  
})


