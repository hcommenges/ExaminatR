############################
#### global functions ######
############################

# load packages ----

library(shinythemes)
library(shiny)
library(knitr)
library(scatterplot3d)
#library(showtext)
library(ade4)
library(cluster)
library(ggdendro)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(stringr)
library(sp)
library(rgdal)
library(cartography)
library(lazyeval)
library(dplyr)

options(scipen = 10000)


# UNIVARIATE ----

# univariate summaries ----

UnivarDescription <- function(df, varquanti){
  quantiSelec <- df[, varquanti]
  sumTab <- quantiSelec %>% 
    summarise_each(funs(min, max, median, mean, var)) %>% 
    melt() %>% 
    mutate(value = round(value, digits = 2))
  
  sumTabLong <- cbind(sumTab, colsplit(sumTab$variable, pattern = "_", names = c("NOM", "MESURE")))
  sumTabLong$MESURE <- French(sumTabLong$MESURE)
  sumTabWide <- dcast(sumTabLong, formula = MESURE ~ NOM, value.var = "value")
  return(sumTabWide)
}


# histogram

Histogram <- function(df, varquanti, nbins = 15){
  caseNumber <- nrow(df)
  myPlot <- ggplot(df) + 
    geom_histogram(aes_string(x = varquanti), color = "white", fill = "grey30", bins = nbins) +
    scale_y_continuous(paste("Fréquence (n = ", caseNumber, ")", sep = ""))
  return(myPlot)
}


# BIVARIATE ----

# quali-quali ----

ContTab <- function(df, varx, vary){
  chiResults <- chisq.test(df[, varx], df[, vary])
  
  # observed
  obsTab <- addmargins(chiResults$observed)
  colnames(obsTab)[ncol(obsTab)] <- "Somme"
  row.names(obsTab)[nrow(obsTab)] <- "Somme"
  
  # expected
  expTab <- formatC(addmargins(chiResults$expected), digits = 1, format = "f")
  colnames(expTab)[ncol(expTab)] <- "Somme"
  row.names(expTab)[nrow(expTab)] <- "Somme"
  
  # raw residuals
  rrTab <- formatC(addmargins(chiResults$observed - chiResults$expected), digits = 1, format = "f")
  colnames(rrTab)[ncol(rrTab)] <- "Somme"
  row.names(rrTab)[nrow(rrTab)] <- "Somme"
  
  # squared standardized residuals
  srTab <- formatC(addmargins(chiResults$residuals ^ 2), digits = 1, format = "f")
  colnames(srTab)[ncol(srTab)] <- "Somme"
  row.names(srTab)[nrow(srTab)] <- "Somme"
  
  tabList <- list(OBS = obsTab, EXP = expTab, RAWRESID = rrTab, STSQRESID = srTab)
  return(tabList)
}


Mosaicplot <- function(df, varx, vary){
  mosaicplot(table(df[, vary], df[, varx]), main = "")
}

# quanti-quanti ----

ScatterPlot <- function(df, varx, vary){
  # compute linear model
  linMod <- lm(formula = formula(eval(paste(vary, "~", paste(varx, collapse = "+")))), data = df)
  linModSumry <- summary(linMod)
  coefReg <- round(linModSumry$coefficients, digits = 2)[, 1]
  rawR2 <- round(linModSumry$r.squared, digits = 2)
  textEq <- ifelse(coefReg[2] > 0, 
                   paste("y = ", coefReg[1], " + ", coefReg[2], "x", sep = ""), 
                   paste("y = ", coefReg[1], " - ", abs(coefReg[2]), "x", sep = ""))
  textR2 <- paste("R2 = ", rawR2, sep = "")
  
  # get label coordinates
  rangeX <- range(df[, varx])
  rangeY <- range(df[, vary])
  interSpace <- 0.05 * (rangeY[2] - rangeY[1])
  if(coefReg[2] > 0){
    posCoords <- c(rangeX[1], rangeX[1], rangeY[2], rangeY[2] - interSpace)
    sidejust <- 0
  } else {
    posCoords <- c(rangeX[2], rangeX[2], rangeY[2], rangeY[2] - interSpace)
    sidejust <- 1
  }
  scatPlot <- ggplot(df) + 
    geom_point(aes_string(x = varx, y = vary), color = "grey60") + 
    geom_smooth(aes_string(x = varx, y = vary), method = "lm", se = FALSE, color = "chocolate") +
    annotate("text", x = posCoords[1:2], y = posCoords[3:4], label = c(textEq, textR2), hjust = sidejust, fontface = "bold")
  
  return(scatPlot)
}

# quali-quanti ----

AnovaTab <- function(df, varx, vary){
  modalList <- sort(unique(df[, varx]))
  globalMean <- mean(df[, vary])
  globalVar <- Varp(df[, vary])
  
  groupMean <- df %>% 
    group_by_(varx) %>% 
    summarise_(MOYENNE = interp(~mean(x), x = as.name(vary)),
               VARIANCE = interp(~Varp(x), x = as.name(vary)))
  groupMean$MOYENNE <- round(groupMean$MOYENNE, digits = 2)
  groupMean$VARIANCE <- round(groupMean$VARIANCE, digits = 2)
  
  dataAnova <- data.frame(paste("OBS", seq(1, nrow(df), 1), sep = "_"),
                          df[, varx],
                          df[, vary],
                          globalMean,
                          stringsAsFactors = FALSE)
  
  colnames(dataAnova) <- c("ID", varx, vary, "C1")
  mergeAnova <- merge(x = dataAnova, y = groupMean[, 1:2], by = varx)
  mergeAnova <- mergeAnova[, c(2, 1, 3, 4, 5)]
  colnames(mergeAnova)[ncol(mergeAnova)] <- "C2"
  mergeAnova$C3 <- mergeAnova$C2 - mergeAnova$C1
  mergeAnova$C4 <- mergeAnova[, vary] - mergeAnova$C2
  mergeAnova[, 2] <- as.character(mergeAnova[, 2])
  
  meanLine <- c("MOYENNE", "", round(apply(mergeAnova[, 3:7], 2, mean), digits = 2))
  varLine <- c("VARIANCE", "", round(apply(mergeAnova[, 3:7], 2, Varp), digits = 2))
  emptyLine <- rep("...", 7)
  
  sampleAnova1 <- mergeAnova[mergeAnova[, varx] == modalList[1], ] %>% .[1:3, ]
  sampleAnova2 <- mergeAnova[mergeAnova[, varx] == modalList[2], ] %>% .[1:3, ]
  sampleAnova1[, 3:7] <- apply(sampleAnova1[, 3:7], 2, round, digits = 2)
  sampleAnova2[, 3:7] <- apply(sampleAnova2[, 3:7], 2, round, digits = 2)
  
  names(meanLine) <- names(varLine) <- colnames(mergeAnova)
  tabFinal <- rbind(sampleAnova1, emptyLine, sampleAnova2, emptyLine, meanLine, varLine)
  
  tabGroup <- rbind(as.matrix(groupMean), 
                    c("ENSEMBLE", round(globalMean, 2), round(globalVar, 2)))
  
  listResult <- list(EXTRACT = tabFinal, SUMTAB = tabGroup)
  
  return(listResult)
}


# TRIVARIATE ----

# compute linear model ----

ComputeRegression <- function(df, vardep, varindep, ident, interact = FALSE, decompvar = FALSE){
  if(interact == FALSE){
    linMod <- lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "+")))), data = df)
    linModSumry <- summary(linMod)
  } else {
    linMod <- lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "*")))), data = df)
    linModSumry <- summary(linMod)
  }
  coefReg <- round(linModSumry$coefficients, digits = 2)[, 1:2]
  rawR2 <- round(linModSumry$r.squared, digits = 2)
  adjR2 <- round(linModSumry$adj.r.squared, digits = 2)
  
  tabResid <- data.frame(ID = df[, ident],
                         ABSRESID = round(linModSumry$residuals, digits = 3), 
                         RELRESID = round(linModSumry$residuals / (df[, vardep] - linModSumry$residuals), digits = 3),
                         STDRESID = round(rstandard(linMod), digits = 3),
                         stringsAsFactors = FALSE)
  colnames(tabResid)[1] <- ident
  
  tabResults <- data.frame(CONCEPT = c("Coef. de détermination",
                                       "Coef. de détermination ajusté",
                                       row.names(coefReg)),
                           VALEUR = c(rawR2, adjR2, coefReg[, 1]),
                           stringsAsFactors = FALSE)
  
  if(decompvar == TRUE){
    tabVariance <- data.frame(CONCEPT = c("Variance inter", "Variance intra"),
                              VALEUR = round(anova(linMod)[[2]] / nrow(df), digits = 2),
                              stringsAsFactors = FALSE)
  } else {tabVariance <- NULL}
  
  
  return(list(TABCOEF = tabResults, TABRESID = tabResid, TABVAR = tabVariance))
}

# Draw boxplot ----

Boxplot <- function(df, varx, vary){
  boxPlot <- ggplot(df) + 
    geom_boxplot(aes_string(x = varx, y = vary), color = "grey20", fill = "grey70")
  
  return(boxPlot)
}

# Draw boxplot 2 factors ----

Boxplot2 <- function(df, varx, vary, groupx){
  boxPlot <- ggplot(df) + 
    geom_boxplot(aes_string(x = varx, y = vary, fill = groupx), color = "grey20")
  
  return(boxPlot)
}


# draw scatter plot ----

ScatterPlotAncov <- function(df, varx, vary, groupx){
  scatPlot <- ggplot(df) + 
    geom_point(aes_string(x = varx, y = vary, group = groupx, color = groupx)) + 
    geom_smooth(aes_string(x = varx, y = vary, group = groupx, color = groupx), method = "lm", se = FALSE) +
    scale_color_discrete()
  
  return(scatPlot)
}

# draw scatter plot 3D ----

ScatterPlot3D <- function(df, varx, vary, varz){
  scatPlot <- scatterplot3d(x = df[, varx], y = df[, vary], z = df[, varz], pch = 20, highlight.3d = FALSE, angle = 35,
                            xlab = varx, ylab = vary, zlab = varz)
  fitMod <- lm(formula = formula(eval(paste(varz, "~", varx, "+", vary, sep = " "))), data = df)
  scatPlot$plane3d(fitMod, lty.box = "solid")
  
  return(scatPlot)
}


# PRINCIPAL COMPONENTS ANALYSIS ----

# compute principal components analysis ----

ComputePrincipalComp <- function(df, varquanti, ident){
  selecVarQuanti <- df[, varquanti]
  row.names(selecVarQuanti) <- df[, ident]
  dudiObj <- dudi.pca(df = selecVarQuanti, center = TRUE, scale = TRUE, scannf = FALSE, nf = 4)
  
  return(dudiObj)
}

# inertia ----

DecompInertia <- function(dudiobj){
  dimTab <- length(dudiobj$eig)
  summaryPca <- data.frame(
    EIG = dudiobj$eig,
    PCTVAR = 100 * dudiobj$eig / sum(dudiobj$eig),
    CUMPCTVAR = cumsum(100 * dudiobj$eig / sum(dudiobj$eig)),
    COMP = factor(x = seq(1, dimTab, 1),
                  levels = seq(1, dimTab, 1),
                  labels = paste("C", seq(1, dimTab, 1), sep = ""))
  )
  
  decomPlot <- ggplot(summaryPca) + geom_bar(aes(x = COMP, y = PCTVAR), stat = "identity") + 
    scale_x_discrete("Composantes") +
    scale_y_continuous("Pourcentage de l'inertie totale (%)")
  
  return(decomPlot)
}

# correlation plot ----

CorCircle <- function(dudiobj, xaxis, yaxis){
  dfCor <- data.frame(dudiobj$co, XORI = 0, YORI = 0, VARIABLE = row.names(dudiobj$co), stringsAsFactors = FALSE)
  oneCircle <- MakeCircle(coordx = 0, coordy = 0, rad = 1)
  corPlot <- ggplot() + 
    geom_vline(xintercept = 0, color = "grey50") + 
    geom_hline(yintercept = 0, color = "grey50") +
    geom_path(data = oneCircle, aes(x = XC, y = YC), color = "grey50") +
    geom_segment(data = dfCor, 
                 aes_string(x = "XORI", xend = colnames(dfCor)[xaxis], y = "YORI", yend = colnames(dfCor)[yaxis]), 
                 lineend = "round",
                 arrow = arrow(length = unit(0.01, "npc"))) +
    geom_label(data = dfCor, aes_string(x = colnames(dfCor)[xaxis], y = colnames(dfCor)[yaxis], label = "VARIABLE"), size = 3, hjust = "outward", vjust = "outward") +
    scale_x_continuous(name = paste("Composante", xaxis, sep = " "), limits = c(-1.2, 1.2)) +
    scale_y_continuous(name = paste("Composante", yaxis, sep = " "), limits = c(-1.2, 1.2)) +
    coord_equal()
  
  return(corPlot)
}


# correlation matrix ----

CorCompMat <- function(dudiobj, naxis){
  matCor <- round(cor(dudiobj$tab, use = "complete.obs", method = "pearson"), digits = 2)
  compCor <- t(round(dudiobj$co[, 1:naxis], digits = 2))
  finalMatCor <- rbind(compCor, matCor)
  return(finalMatCor)
} 


# individuals plot ----

PlotIndiv <- function(dudiobj, xaxis, yaxis, printlabel = FALSE){
  xString <- paste("Axis", xaxis, sep = "")
  yString <- paste("Axis", yaxis, sep = "")
  coordIndiv <- data.frame(ID = row.names(dudiobj$li),
                           dudiobj$li,
                           stringsAsFactors = FALSE)
  if(printlabel == FALSE){
    pcaIndivPlot <- ggplot(coordIndiv) +
      geom_hline(yintercept = 0, color = "grey50") + geom_vline(xintercept = 0, color = "grey50") +
      geom_point(aes_string(x = xString, y = yString), size = 0.6) +
      scale_x_continuous(name = paste("Composante", xaxis, sep = " ")) +
      scale_y_continuous(name = paste("Composante", yaxis, sep = " "))
  } else {
    pcaIndivPlot <- ggplot(coordIndiv) +
      geom_hline(yintercept = 0, color = "grey50") + geom_vline(xintercept = 0, color = "grey50") +
      geom_text(aes_string(x = xString, y = yString, label = "ID"), size = 3) +
      scale_x_continuous(name = paste("Composante", xaxis, sep = " ")) +
      scale_y_continuous(name = paste("Composante", yaxis, sep = " "))
  }

  
  return(pcaIndivPlot)
}


# contributions ----

ContribVarIndiv <- function(dudiobj){
  inertiaPca <- inertia.dudi(dudiobj, row.inertia = TRUE, col.inertia = TRUE)
  contribVar <- round(0.1 * inertiaPca$col.abs, digits = 0)
  contribInd <- data.frame(ID = row.names(inertiaPca$row.abs), round(0.1 * inertiaPca$row.abs, digits = 0), stringsAsFactors = FALSE)
  
  return(list(CTRVAR = contribVar, CTRIND = contribInd))
}



# HIERARCHICAL CLUSTERING ----

# compute classification ----

ComputeClassif <- function(df, varquanti){
  classifObj <- agnes(x = df[, varquanti], diss = FALSE, metric = "euclidean", stand = TRUE, method = "ward")
  return(classifObj)
}

# plot dendrogram ----

PlotDendro <- function(classifobj){
  dendroPlot <- as.dendrogram(classifobj)
  dendroData <- dendro_data(dendroPlot, type = "rectangle")
  dendroGgplot <- ggplot(segment(dendroData)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    scale_x_continuous("") + scale_y_continuous("")

  return(dendroGgplot)
}

# plot inertia ----

PlotHeight <- function(classifobj){
  sortedHeight <- sort(classifobj$height, decreasing = TRUE)
  relHeigth <- sortedHeight / sum(sortedHeight) * 100
  tabHeight <- data.frame(NODES = factor(1:30),
                          INERTIE = relHeigth[1:30])
  
  heightPlot <- ggplot(tabHeight) +
    geom_bar(aes(x = NODES, y = INERTIE), fill = "grey30", stat = "identity") +
    scale_x_discrete("Nombre de classes") + scale_y_continuous("Pourcentage de l'inertie totale (%)")
  
  return(heightPlot)
}

# plot profile ----

PlotProfile <- function(classifobj, nbclus){
  dfOri <- as.data.frame(classifobj$data, stringsAsFactors = FALSE)
  clusId <- cutree(classifobj, k = nbclus)
  dfOri$CLUS <- factor(clusId,
                       levels = 1:nbclus,
                       labels = paste("CLASSE", 1:nbclus))
  clusProfile <- aggregate(dfOri[, 1:ncol(dfOri)-1],
                           by = list(dfOri$CLUS),
                           mean)
  colnames(clusProfile)[1] <- "CLASSE"
  clusLong <- melt(clusProfile, id.vars = "CLASSE")
  
  profilePlot <- ggplot(clusLong) +
    geom_bar(aes(x = variable, y = value), fill = "grey30", position = "identity", stat = "identity") +
    scale_x_discrete("Variable") + scale_y_continuous("Valeur moyenne par classe") +
    facet_wrap(~ CLASSE) + coord_flip()
  
  return(list(PROFILE = profilePlot, CLUSID = dfOri$CLUS))
}


# CARTOGRAPHY ----

# discretization average-sd ----

avgsd <- function(vec, nbcl){
  if(length(vec) < nbcl) {
    stop("Nombre de classes trop important")
  } else{
    minVec <- min(vec, na.rm = TRUE)
    maxVec <- max(vec, na.rm = TRUE)
    avgVec <- mean(vec, na.rm = TRUE)
    sdVec <- sqrt(sum((vec - avgVec) ^ 2) / length(vec[!is.na(vec)])) # écart-type population
    evenClass <- nbcl %% 2 == 0
    nbSd <- floor((nbcl - 1) / 2)
    seqNbSd <- seq(-nbSd, nbSd, by = 1)
    seqSd <- seqNbSd[seqNbSd != 0] * sdVec
    
    if(avgVec + min(seqSd) < minVec | avgVec + max(seqSd) > maxVec){
      stop("Nombre de classes trop important")
    } else {
      if(evenClass){
        brks <- c(minVec, avgVec + seqSd[1:nbSd], avgVec, avgVec + seqSd[(nbSd+1):length(seqSd)], maxVec)
      } else {
        brks <- c(minVec, avgVec + seqSd[1:nbSd], avgVec + seqSd[(nbSd+1):length(seqSd)], maxVec)
      }
      return(brks[!is.na(brks)])
    }
  }
}




# variable ----

CartoVar <- function(spdf, df, idtab, idshape, varquanti, discret, nbcl){
  allPal <- c("blue.pal", "orange.pal", "red.pal", "brown.pal", "green.pal",
              "purple.pal", "pink.pal", "wine.pal", "grey.pal", "turquoise.pal", 
              "sand.pal", "taupe.pal", "kaki.pal" , "harmo.pal")
  choroLayer(spdf = spdf, 
             df = df,
             spdfid = idshape,
             dfid = idtab,
             var = varquanti,
             method = discret,
             nclass = nbcl,
             border = "grey",
             legend.values.rnd = 1,
             col = carto.pal(pal1 = allPal[sample(x = 1:14, size = 1)], n1 = nbcl, transparency = TRUE))
  layoutLayer(title = varquanti,
              sources = "", 
              author = "",
              frame = TRUE,
              south = TRUE)
}

# résidus de la régression ----

CartoRegression <- function(spdf, df, idtab, idshape, model, nbcl){
  # create table
  df <- cbind(df, model$TABRESID)
  
  # discretize
  discrBrksAbs <- avgsd(vec = df$ABSRESID, nbcl = nbcl)
  discrBrksRel <- avgsd(vec = df$RELRESID, nbcl = nbcl)
  discrBrksStd <- avgsd(vec = df$STDRESID, nbcl = nbcl)
  if(nbcl %% 2 == 0){
    discrPal <- carto.pal(pal1 = "blue.pal", n1 = nbcl / 2, pal2 = "red.pal", n2 = nbcl / 2, middle = FALSE, transparency = TRUE)
  } else {
    discrPal <- carto.pal(pal1 = "blue.pal", n1 = nbcl / 2, pal2 = "red.pal", n2 = nbcl / 2, middle = TRUE, transparency = TRUE)
  }
  
  choroLayer(spdf = spdf, 
             df = df,
             spdfid = idshape,
             dfid = idtab,
             var = "ABSRESID",
             breaks = discrBrksAbs,
             border = "grey",
             legend.values.rnd = 1,
             col = discrPal)
  
  layoutLayer(title = "Résidus bruts de la régression linéaire",
              sources = "", 
              author = "",
              frame = TRUE,
              south = TRUE)
  
  choroLayer(spdf = spdf, 
             df = df,
             spdfid = idshape,
             dfid = idtab,
             var = "RELRESID",
             breaks = discrBrksRel,
             border = "grey",
             legend.values.rnd = 1,
             col = discrPal)
  
  layoutLayer(title = "Résidus relatifs de la régression linéaire",
              sources = "", 
              author = "",
              frame = TRUE,
              south = TRUE)
  
  choroLayer(spdf = spdf, 
             df = df,
             spdfid = idshape,
             dfid = idtab,
             var = "STDRESID",
             breaks = discrBrksStd,
             border = "grey",
             legend.values.rnd = 1,
             col = discrPal)
  
  layoutLayer(title = "Résidus standardisés de la régression linéaire",
              sources = "", 
              author = "",
              frame = TRUE,
              south = TRUE)
}


# coordonnées factorielles ----

CartoFacto <- function(spdf, df, idtab, idshape, model, discret, nbcl){
  # create table
  df <- cbind(df, model$li)

  # discretize
  brksAxis1 <- avgsd(vec = df$Axis1, nbcl = nbcl)
  brksAxis2 <- avgsd(vec = df$Axis2, nbcl = nbcl)
  
  if(nbcl %% 2 == 0){
    discrPal <- carto.pal(pal1 = "kaki.pal", n1 = nbcl / 2, pal2 = "sand.pal", n2 = nbcl / 2, middle = FALSE, transparency = TRUE)
  } else {
    discrPal <- carto.pal(pal1 = "kaki.pal", n1 = nbcl / 2, pal2 = "sand.pal", n2 = nbcl / 2, middle = TRUE, transparency = TRUE)
  }
  
  choroLayer(spdf = spdf, 
             df = df,
             spdfid = idshape,
             dfid = idtab,
             var = "Axis1",
             breaks = brksAxis1,
             border = "grey",
             legend.values.rnd = 1,
             col = discrPal)
  
  layoutLayer(title = "Coordonnées factorielles - Axe 1",
              sources = "", 
              author = "",
              frame = TRUE,
              south = TRUE)
  
  choroLayer(spdf = spdf, 
             df = df,
             spdfid = idshape,
             dfid = idtab,
             var = "Axis2",
             breaks = brksAxis2,
             border = "grey",
             legend.values.rnd = 1,
             col = discrPal)
  
  layoutLayer(title = "Coordonnées factorielles - Axe 2",
              sources = "", 
              author = "",
              frame = TRUE,
              south = TRUE)
}


# classes CAH ----

CartoClass <- function(spdf, df, idtab, idshape, model){
  df$CLUSID <- model$CLUSID
  typoLayer(spdf = spdf, 
             df = df,
             spdfid = idshape,
             dfid = idtab,
             var = "CLUSID",
             border = "grey")
  
  layoutLayer(title = "Classes de la CAH",
              sources = "", 
              author = "",
              frame = TRUE,
              south = TRUE)
}


# DISTRIBUTION F, T, CHI2 ----

# Chi2 distribution ---- 

ChiDistribution <- function(df, alpha){
  # compute probabilities
  lowerBound <- 0
  upperBound <- round(qchisq(p = 1 - alpha, df = df), digits = 2)
  maxX <- round(qchisq(p = 0.999, df = df), digits = 2)
  seqValues <- seq(0, maxX, .01)
  seqProba <- dchisq(x = seqValues, df = df)
  
  # create tables
  tabChi <- data.frame(SEQ = seqValues, PROBA = seqProba)
  valBreak <- which(as.character(tabChi$SEQ) == as.character(upperBound))
  tabZeroZero <- data.frame(SEQ = 0, PROBA = 0)
  tabZeroBound <- data.frame(SEQ = upperBound, PROBA = 0)
  tabZeroMax <- data.frame(SEQ = max(tabChi$SEQ), PROBA = 0)
  tabChiNo <- rbind(tabZeroZero, tabChi[1:valBreak, ], tabZeroBound)
  tabChiYes <- rbind(tabZeroBound, tabChi[valBreak:nrow(tabChi), ], tabZeroMax)
  
  # plot distribution
  chiPlot <- ggplot() + 
    geom_polygon(data = tabChiNo, aes(x = SEQ, y = PROBA), fill = "grey50") +
    geom_polygon(data = tabChiYes, aes(x = SEQ, y = PROBA), fill = "firebrick") +
    geom_line(data = tabChi, aes(x = SEQ, y = PROBA)) +
    annotate(geom = "text", x = upperBound, y = -max(tabChi$PROBA) / 40, label = formatC(upperBound, digits = 1, format = "f"), fontface = 2) +
    scale_x_continuous("Valeur de la statistique Chi2") +
    scale_y_continuous("Densité de probabilité") +
    ggtitle(paste("Valeurs seuil de la statistique Chi2 : degrés de liberté = ", df, " ; alpha = ", alpha, sep = ""))
  
  return(chiPlot)
}


# T distribution ---- 

TDistribution <- function(df, alpha){
  # compute probabilities
  lowerBound <- 0
  upperBound <- round(qt(p = 1 - (alpha/2), df = df), digits = 2)
  maxX <- round(qt(p = 0.999, df = df), digits = 2)
  seqValues <- sort(c(-seq(0, maxX, .01), seq(0, maxX, .01)))
  seqValuesAbs <- seq(0, maxX, .01)
  seqProba <- c(rev(dt(x = seqValuesAbs, df = df)), dt(x = seqValuesAbs, df = df))
  
  # create tables
  tabT <- data.frame(SEQ = seqValues, PROBA = seqProba)
  valBreak <- which(as.character(abs(tabT$SEQ)) == as.character(upperBound))
  tabZeroMin <- data.frame(SEQ = -maxX, PROBA = 0)
  tabZeroMax <-  data.frame(SEQ = maxX, PROBA = 0)
  tabZeroBound1 <- data.frame(SEQ = -upperBound, PROBA = 0)
  tabZeroBound2 <- data.frame(SEQ = upperBound, PROBA = 0)
  tabTNo <- rbind(tabZeroBound1, tabT[valBreak[1]:valBreak[2], ], tabZeroBound2)
  tabTYes1 <- rbind(tabZeroMin, tabT[1:valBreak[1], ], tabZeroBound1)
  tabTYes2 <- rbind(tabZeroBound2, tabT[valBreak[2]:nrow(tabT), ], tabZeroMax)
  
  # plot distribution
  tPlot <- ggplot() + 
    geom_polygon(data = tabTNo, aes(x = SEQ, y = PROBA), fill = "grey50") +
    geom_polygon(data = tabTYes1, aes(x = SEQ, y = PROBA), fill = "firebrick") +
    geom_polygon(data = tabTYes2, aes(x = SEQ, y = PROBA), fill = "firebrick") +
    geom_line(data = tabT, aes(x = SEQ, y = PROBA)) +
    annotate(geom = "text", 
             x = c(-upperBound, upperBound), 
             y = -max(tabT$PROBA) / 40, 
             label = c(formatC(-upperBound, digits = 1, format = "f"), formatC(upperBound, digits = 1, format = "f")), 
             fontface = 2) +
    scale_x_continuous("Valeur du T de Student") +
    scale_y_continuous("Densité de probabilité") +
    ggtitle(paste("Valeurs seuil de la statistique T : degrés de liberté = ", df, " ; alpha = ", alpha, sep = ""))
  
  return(tPlot)
}



# F distribution ---- 

FDistribution <- function(df1, df2, alpha){
  # compute probabilities
  lowerBound <- 0
  upperBound <- round(qf(p = 1 - alpha, df1 = df1, df2 = df2), digits = 2)
  maxX <- round(qf(p = 0.999, df1 = df1, df2 = df2), digits = 2)
  seqValues <- seq(0, maxX, .01)
  seqProba <- df(x = seqValues, df1 = df1, df2 = df2)
  
  # create tables
  tabF <- data.frame(SEQ = seqValues, PROBA = seqProba)
  valBreak <- which(as.character(abs(tabF$SEQ)) == as.character(upperBound))
  tabZeroZero <- data.frame(SEQ = 0, PROBA = 0)
  tabZeroBound <- data.frame(SEQ = upperBound, PROBA = 0)
  tabZeroMax <- data.frame(SEQ = max(tabF$SEQ), PROBA = 0)
  tabFNo <- rbind(tabZeroZero, tabF[1:valBreak, ], tabZeroBound)
  tabFYes <- rbind(tabZeroBound, tabF[valBreak:nrow(tabF), ], tabZeroMax)
  
  # plot distribution
  fPlot <- ggplot() + 
    geom_polygon(data = tabFNo, aes(x = SEQ, y = PROBA), fill = "grey50") +
    geom_polygon(data = tabFYes, aes(x = SEQ, y = PROBA), fill = "firebrick") +
    geom_line(data = tabF, aes(x = SEQ, y = PROBA)) +
    annotate(geom = "text", x = upperBound, y = -max(tabF$PROBA) / 40, label = formatC(upperBound, digits = 1, format = "f"), fontface = 2) +
    scale_x_continuous("Valeur de la statistique F") +
    scale_y_continuous("Densité de probabilité") +
    ggtitle(paste("Valeurs seuil de la statistique F : degrés de liberté = ", df1, " & ", df2, " ; alpha = ", alpha, sep = ""))
  
  return(fPlot)
}



# POT-POURRI ----

# load fonts ----

# LoadFonts <- function(){
#   font.add(family = "Ubu", regular = "Ubuntu-L.ttf", bold = "Ubuntu-R.ttf")
# }


# translate to french ----

French <- function(vec){
  outWords <- c("min", "max", "median", "mean", "var")
  frenchWords <- c("Minimum", "Maximum", "Médiane", "Moyenne", "Variance")
  idMatchOutRaw <- match(x = vec, table = outWords)
  idMatchIn <- which(!is.na(idMatchOutRaw))
  idMatchOut <- as.vector(na.omit(idMatchOutRaw))
  vec[idMatchIn] <- frenchWords[idMatchOut]
  vecFactor <- factor(vec, levels = frenchWords, labels = frenchWords)
  return(vecFactor)
}

# variance for population ----

Varp <- function(vec){
  varPop <- sum((vec - mean(vec)) ^ 2) / length(vec) 
  return(varPop)
}

# parse data description ----

ParseDescription <- function(vec){
  varList <- str_split(vec, pattern = ",")
  varListTrimmed <- str_trim(varList[[1]])
  return(varListTrimmed)
}


# circle coordinates ----

MakeCircle <- function(coordx, coordy, rad = 1, npoints = 150){
  tc <- seq(0, 2 * pi, length.out = npoints)
  xc <- coordx + rad * cos(tc)
  yc <- coordy + rad * sin(tc)
  return(data.frame(XC = xc, YC = yc))
}


