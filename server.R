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
    columnListShape <- colnames(as.data.frame(readSpatialData()))
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
    updateSelectInput(session = session,
                      inputId = "idshape",
                      choices = c("ø", columnListShape))
    
  })
  
  readSpatialData <- reactive({
    inFile2 <- input$file2
    if (is.null(inFile2)) {
      return(NULL)
    } else {
      oriDir <- getwd()
      setwd(tempdir())
      unzip(zipfile = inFile2$datapath, overwrite = TRUE, exdir = "shpdir")
      fileName <- list.files("shpdir")[1]
      layerName <- substr(fileName, start = 1, stop = nchar(fileName) - 4)
      spObject <- readOGR(dsn = "shpdir", layer = layerName, stringsAsFactors = FALSE)
      setwd(oriDir)
      return(spObject)
    }
  })
  
  
  # OUTPUTS ----
  
  # Guide
  output$citation <- renderText("
     1. Charger un jeu de données au format CSV <br/>
     2. Charger un fichier spatial (shape zippé) <br/>
     3. Choisir la variable identifiant du tableau <br/>
     4. Choisir la variable identifiant du fichier spatial <br/>
     5. Sélectionner les analyses à effectuer <br/>
     6. Télécharger le document au format voulu")
  
  
  
  # Report
  output$report <- downloadHandler(
    filename = function() {paste("exam.", sep = "", switch(input$docformat, pdf = "pdf", odt = "odt", docx = "docx"))},
    content = function(file){
      if(input$docformat == "pdf"){
        knit2pdf("exam.Rnw")
        file.copy("exam.pdf", file)
        file.remove("exam.pdf", "exam.tex",
                    "exam.aux", "exam.log")
        unlink("figure", recursive = TRUE)
      } else if (input$docformat == "odt"){
        knit2pdf("exam.Rnw")
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
        knit2pdf("exam.Rnw")
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


