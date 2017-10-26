
# Importo i package
library(readxl)
library(ltm)

#' Legge un file csv
#'
#' @param path Percorso del file
#' @param firstColAsNames Indica se impostare la prima colonna come indici di riga della tabella
#' @return Data frame corrispondente al file csv indicato
#' @examples
#' readCSV("file.csv")
#'
#' readCSV("C:/Users/file.csv")
readCSV <- function(path, firstColAsNames = TRUE) {
    if(firstColAsNames)
        read.csv(path, row.names = 1)
    else
        read.csv(path)
}

#' Legge un file excel
#'
#' @param path Percorso del file
#' @param firstColAsNames Indica se impostare la prima colonna come indici di riga della tabella
#' @return Data frame corrispondente al file excel indicato
#' @examples
#' readEXCEl("file.xlsx")
#'
#' readEXCEl("C:/Users/file.xlsx")
readEXCEl <- function(path, firstColAsNames = TRUE) {
    items <- read_excel(path)
    items <- as.data.frame(items)
    
    if(firstColAsNames){
        rownames(items) <- items[,1]
        items <- items[,-1]
    }
    items
}

#' Modifica la dimensione dei grafici
#'
#' @param width Larghezza dei grafici
#' @param height Altezza dei grafici
#' @examples
#' width <- 7
#' height <- 5
#' sizePlot(width, height)
sizePlot <- function(width, height) {
    options(repr.plot.width = width, repr.plot.height = height)
}

#' Stima le difficolta' degli esercizi senza porre vincoli sul valore del discriminante, esso verra' stimato e posto 
#' uguale per ogni item 
#'
#' @param items Tabella dicotomica avente una riga per studente e una colonna per esercizio
#' @param forNA Definisce il comportamento in caso di NA, di default e' NULL quindi vengono rimossi prima della computazione, 
#' se si usasse forNA = na.exclude la stima verrebbe effettuata eliminando le righe in cui e' presente almeno un NA
#' @return Vettore di difficolta' stimate
#' @examples
#' items <- readEXCEl("file.xlsx")
#' estimateDifficultiesDichotFreeDiscrim(items)
estimateDifficultiesDichotFreeDiscrim <- function(items, forNA = NULL) {
    param <- rasch(items, na.action = forNA)
    tab <- coef(param)
    array(tab[,"Dffclt"])
}

#' Stima le difficolta' degli esercizi ponendo il discriminante ad 1
#'
#' @param items Tabella dicotomica avente una riga per studente e una colonna per esercizio
#' @param forNA Definisce il comportamento in caso di NA, di default e' NULL quindi vengono rimossi prima della computazione, 
#' se si usasse forNA = na.exclude la stima verrebbe effettuata eliminando le righe in cui e' presente almeno un NA
#' @return Vettore di difficolta' stimate
#' @examples
#' items <- readEXCEl("file.xlsx")
#' estimateDifficultiesDichotOneDiscrim(items)
estimateDifficultiesDichotOneDiscrim <- function(items, forNA = NULL) {
    param <- rasch(items, constraint = cbind(ncol(items) + 1, 1), na.action = forNA)
    tab <- coef(param)
    array(tab[,"Dffclt"])
}

#' Stima le abilita' degli studenti
#'
#' @param items Tabella dicotomica avente una riga per studente e una colonna per esercizio
#' @return Vettore di abilita' stimate
#' @examples
#' items <- readEXCEl("file.xlsx")
#' estimateAbilitiesDichot(items)
estimateAbilitiesDichot <- function(items) {
    fit <- rasch(items)
    fs <- factor.scores(fit, resp.patterns = items)
    array(unlist(fs$score.dat["z1"]))
}

#' Genera un grafico per mostrare il livello di difficolta' di ciascun esercizio 
#'
#' @param items Tabella dicotomica avente una riga per studente e una colonna per esercizio
#' @param diff Vettore di difficolta'
#' @param color Colore del grafico, di default e' rosso
#' @param orizontalLabels Definisce se mettere i nomi degli esercizi verticalmente o orizzontalmente
#' @examples
#' items <- readEXCEl("file.xlsx")
#' diff <- estimateDifficultiesDichotOneDiscrim(items)
#' plotDifficultiesDichot(items, diff)
plotDifficultiesDichot <- function(items, diff, color = "red", name = "Difficulties plot", orizontalLabels = TRUE) {
    nItems <- length(diff)
    plot(diff,xlim=c(1,nItems),ylim=c(-5,5),type="b",main=name,xlab="Items",ylab="Difficulty",xaxt="n",col=color,pch=16,lwd=2)
    
    if(orizontalLabels)
        axis(1, at=1:nItems, labels=colnames(items))
    else
        axis(1, at=1:nItems, labels=colnames(items), las = 2)
    par(new = TRUE)
    plot(diff + 4.7, type = "h", xlim = c(1,nItems),ylim = c(0,10), xaxt='n', yaxt = "n", xlab = "", ylab = "", lty = 2)
}

#' Genera un istogramma per mostrare il livello di abilita' degli studenti e la relativa quantita'
#'
#' @param abil Vettore di abilita'
#' @param start Primo valore sull'asse x
#' @param end Ultimo valore sull'asse x
#' @param by Intervallo tra le celle dell'istogramma 
#' @examples
#' items <- readEXCEl("file.xlsx")
#' abil <- estimateAbilitiesDichot(items)
#' plotDifficultiesDichot(abil)
plotAbilitiesDichot <- function(abil, start, end, by) {
    hist(abil,main="Ability histogram",xlab="Ability",ylab="Students",breaks=seq(start,end,by),xlim=c(start,end),col="royalblue3")
    axis(1, at=seq(start, end, by), labels=seq(start, end, by))        
}

#' Attraverso la tecnica di bootstrapping seleziona randomicamente n righe dalla matrice dicotomica, questa funzione e' utlile 
#' se si dispone di pochi soggetti perche' in caso di una scarsa quantita' di osservazioni lo stimatore potrebbe non covergere
#'
#' @param items Tabella dicotomica avente una riga per studente e una colonna per esercizio
#' @param n Numero di estrazioni
#' @return Data frame di difficolta' stimate dalla tabella generata dalle estrazioni casuali
#' @examples
#' items <- readEXCEl("file.xlsx")
#' bootstrapping(items)
bootstrapping <- function(items, n = 500) {
    idxs <- sample(1:nrow(items), n, replace = TRUE)
    items[idxs,]
}

#' Calcola la probabilita' che uno studente risolva correttamente un esercizio
#'
#' @param ability Abilita' dello studente
#' @param difficulty Difficolta' dell'esercizio
#' @return Probabilita' di rispondere correttamente, compresa tra 0 e 1
#' @examples
#' ability <- 2
#' difficulty <- 1
#' pl1(ability, difficulty)
pl1 <- function(ability, difficulty) {
    exp(ability - difficulty) / (1 + exp(ability - difficulty))
}

#' Determina la probabilita' per ogni studente di risolvere correttamente ogni esercizio
#'
#' @param items Tabella dicotomica avente una riga per studente e una colonna per esercizio
#' @param diff Vettore di difficolta'
#' @param abil Vettore di abilita'
#' @return Data frame di probabilita'
#' @examples
#' items <- readEXCEl("file.xlsx")
#' diff <- estimateDifficultiesDichotFreeDiscrim(items)
#' abil <- estimateAbilitiesDichot(items)
#' calcProbabilites(items, diff, abil)
calcProbabilites <- function(items, diff, abil) {   
    probDf <- items
    namesCol <- names(probDf)
    namesRow <- row.names(probDf)
    j <- 1
    for (row in namesRow) {
        i <- 1
        for (col in namesCol) {
            prob <- pl1(abil[j],diff[i])
            probDf[row,col] <- prob          
            i <- i + 1
        }
        j <- j + 1
    }
    probDf
}


#' Migliora la lettura della tabella delle probabilita' per l'utente
#'
#' @param df Data frame di probabilita', output della funzione calcProbabilites
#' @return Data frame di probabilita' da 0 a 100
#' @examples
#' items <- readEXCEl("file.xlsx")
#' diff <- estimateDifficultiesDichotFreeDiscrim(items)
#' abil <- estimateAbilitiesDichot(items)
#' dfProb <- calcProbabilites(items, diff, abil)
#' showProbabilities(dfProb)
showProbabilities <- function(df){
    aux <- trunc(df*100)
    aux <- data.frame(lapply(aux, function(p) paste(p, "%")))
    row.names(aux) <- row.names(df)
    aux
}

#' Genera una tabella dicotomica
#'
#' @param diff vettore di difficolta' scelte
#' @param abil vettore di abilita' scelte
#' @return La tabella dicotomica generata
#' @examples
#' numItems <- 10
#' numStudents <- 30
#' diffInit <- rnorm(numItems)
#' abil <- rnorm(numStudents)
#' generateDichot(diffInit, abil)
generateDichot <- function(diff, abil) {
    items <- data.frame(matrix(0, ncol = length(diff), nrow = length(abil)))
    prob <- calcProbabilites(items, diff, abil)
    namesCol  <- names(items)
    namesRow  <- row.names(items)
    for (row in namesRow) {
        for (col in namesCol) {
            items[row,col] <- rbinom(n = 1, size = 1, prob[row,col])         
        }
    }    
    items
}

#' Valuta la bonta' del mio stimatore misurando lo scostamento tra le difficolta' o le abilita' passate per argomento
#'
#' @param vect1 vettore di difficolta' o abilita'
#' @param vect2 vettore di difficolta' o abilita'
#' @return Data frame con le difficolta' o abilita' passate per argomento e lo scostamento rilevato
#' @examples
#' numItems <- 10
#' numStudents <- 30
#' bootStrapVal <- 10
#' diffInit <- rnorm(numItems)
#' abil <- rnorm(numStudents)
#' genDic <- generateDichot(diffInit, abil)
#' genDicBoot <- bootstrapping(genDic, numStudents * bootStrapVal)
#' diffEstim <- estimateDifficultiesDichotFreeDiscrim(genDicBoot)
#' validationWithDifference(diffInit, diffEstim)
validationWithDifference <- function(vect1, vect2, label1 = "Vettore 1", label2 = "Vettore 2") {
    scost <- abs(unlist(lapply(vect1 - vect2 , round, 2)))
    compare <- data.frame(vect1, vect2, scost)   
    colnames(compare) <- c(label1, label2, "Scostamento")
    compare
}

#' Valuta la bonta' del mio stimatore confrontando graficamente le difficolta' o le abilita' passate per argomento
#'
#' @param vect1 Vettore di difficolta' o abilita'
#' @param vect2 Vettore di difficolta' o abilita'
#' @param label1 Descrizione del vettore 1
#' @param label2 Descrizione del vettore 2
#' @examples
#' numItems <- 10
#' numStudents <- 30
#' bootStrapVal <- 10
#' diffInit <- rnorm(numItems)
#' abil <- rnorm(numStudents)
#' genDic <- generateDichot(diff, abil)
#' genDicBoot <- bootstrapping(genDic, numStudents * bootStrapVal)
#' diffEstim <- estimateDifficultiesDichotFreeDiscrim(genDicBoot)
#' validationWithPlot(diffInit, diffEstim)
validationWithPlot <- function(vect1, vect2, label1 = "Vettore 1", label2 = "Vettore 2") {
    plot(vect1,xlim=c(1,length(vect1)),ylim=c(-4,4),type="b",main="Comparison plot",xlab="Observations",ylab="Values",xaxt="n",col="red",pch=16,lwd=2)
    axis(1, at = 1:length(vect1), labels = 1:length(vect1))
    par(new = TRUE)
    plot(vect2, type = "b", xlim = c(1,length(vect2)),ylim = c(-4,4), xaxt='n', yaxt = "n", xlab = "", ylab = "", col = "blue", pch=16, lwd=2)
    legend(1, 4, legend=c(label1, label2), col=c("red", "blue"), lty=1, cex=1, lwd=2, pch=16)
    NULL
}

#' Valuta la bonta' del mio stimatore calcolando il coefficiente di correlazione lineare
#'
#' @param vect1 vettore di difficolta' o abilita'
#' @param vect2 vettore di difficolta' o abilita'
#' @return Valore numerico compreso tra -1 e 1: se si avvicina a -1 i dati sono correlati negativamente, se si avvicina ad 1 
#' i dati sono correlati positivamente e se e' un valore vicino a 0 allora non vi e' alcun tipo di correlazione lineare
#' @examples
#' numItems <- 10
#' numStudents <- 30
#' bootStrapVal <- 10
#' diffInit <- rnorm(numItems)
#' abil <- rnorm(numStudents)
#' genDic <- generateDichot(diff, abil)
#' genDicBoot <- bootstrapping(genDic, numStudents * bootStrapVal)
#' diffEstim <- estimateDifficultiesDichotFreeDiscrim(genDicBoot)
#' validationWithCor(diffInit, diffEstim)
validationWithCor <- function(vect1, vect2) {
    cor(vect1, vect2)
}


#' Funzione che permette di generare, stimare, validare e, se richiesto, estendere la tabella generata dagli input
#'
#' @param diff Vettore di difficolta'
#' @param abil Vettore di abilita'
#' @param generator Funzione generatrice della tabella
#' @param estimator Funzione di stima della difficolta' o abilita' della tabella generata
#' @param validator Funzione per fornire un indicatore della bonta' dello stimatore
#' @param increaseTable Funzione opzionale di incremento della tabella su cui applicare lo stimatore
#' @param valid Indica se il validatore deve essere applicato sulla difficolta' o l'abilita'
#' @return Lista contenente la tabella generata, le difficolta' o abilita' stimate e output del validatore 
#' @examples
#' numItems <- 10
#' numStudents <- 30
#' bootStrapVal <- 10
#' diffInit <- rnorm(numItems)
#' abil <- rnorm(numStudents)
#' simulate(diffInit, abil, generateDichot, estimateDifficultiesDichotOneDiscrim, validationWithPlot, bootstrapping)
#' simulate(diffInit, abil, generateDichot, estimateAbilitiesDichot, validationWithPlot, diffOrAb = "abil")
simulate <- function(diff, abil, generator, estimator, validator, increaseTable = function(df) df, diffOrAb = "diff") {
    tabDichot <- generator(diff, abil)
    tabDichotBoot <- increaseTable(tabDichot)  
    estim <- estimator(tabDichotBoot)
    
    if(diffOrAb == "diff"){        
        valid <- validator(diff, estim)
    }
    else if(diffOrAb == "abil"){        
        valid <- validator(abil, estim)
    }
    
    list(tabDichot, estim, valid)
}

#' Rimpiazza i valori NA presenti nella tabella passata con 0
#'
#' @param items Tabella dicotomica avente una riga per studente e una colonna per esercizio
#' @return Tabella dicotomica avente una riga per studente e una colonna per esercizio a cui sono stati sostituiti i NA con 0
#' @examples
#' items <- readEXCEl("file.xlsx")
#' replaceNaWithZero(items)
replaceNaWithZero <- function(items) {
    aux <- items
    aux[is.na(aux)] <- 0
    aux
}

#' Sostituisce alcuni 0 con NA nella tabella passata simulando che alcuni studenti invece di consegnare un esercizio sicuramente
#' errato hanno invece deciso di astenersi
#'
#' @param items Tabella dicotomica avente una riga per studente e una colonna per esercizio
#' @param prob Probabilita' con cui decidere se sostituire uno 0 in NA, se e' 0 nessuno verra' sostituito e se e' 1 tutti verranno
#' sostituiti
#' @return Data frame nella quale sono avvenute le sostituizioni 
#' @examples
#' p <- 0.3
#' items <- readEXCEl("file.xlsx")
#' replaceRandomZeroWithNA(items, p)
replaceRandomZeroWithNA <- function(items, prob) {   
    df <- items
    namesCol <- names(df)
    namesRow <- row.names(df)
    for (row in namesRow) {
        for (col in namesCol) {
            if(df[row,col] == 0){
                p <- runif(1)
                if(prob > p){
                    df[row,col] = NA
                }
            }                
        }
    }
    df
}

#' Produce una matrice dicotomica definendo la condizione per cui un esercizio viene definito come passato(1) oppure no(0)
#' @param session Sessione considerata
#' @param vote Stringa che indica come valutare gli esercizi
#' @param maxError Se scelto il criterio "errors", questo campo indica il massimo numero di errori commettibili
#' @param percTestCaseOk Se scelto il criterio "testCase", questo campo indica la percentuale di test case che devono 
#' essere corretti rispetto al totale
#' @param keepNA Paramatreo booleano per indicare se indicare con NA i voti in cui gli studenti non hanno raggiunto la votazione
#' precedente al paramentro di valutazione indicato (es. evaluateSource(session, vote = "compiles", keepNA = TRUE), i voti
#' NA riguarderanno gli studenti che non hanno raggiunto lo step precedente, cioe' l'upload)
#' @return Data frame dicotomico, possiede una riga per studente e una colonna per esercizio, nelle intersezioni vi sono i 
#' risultati delle valutazioni sulla base delle specifiche inserite
#' @examples
#' sourceData <- "data.R"      
#' source(sourceData)
#' numSessions <- 5
#' session <- sessions[numSessions]
#' vote <- "testCase"
#' p <- 0.5
#' evaluateSource(session, vote, percTestCaseOk = p)             
evaluateSource <- function(session, vote, maxError, percTestCaseOk, keepNA = FALSE) {      
    results <- get(paste("results_", session, sep = ""))
    
    if(vote == "uploaded")
        results[,,vote]
    else if(vote == "compiles"){
        dfC <- results[,,vote]
        dfU <- results[,,"uploaded"]
        
        if(keepNA)
            dfC[dfU == 0] <- NA   
        dfC
    }
    else if(vote == "errors"){
        df <- results[,,vote]
        df[df <= maxError] <- 1 
        df[df > maxError] <- 0
        
        if(!keepNA)
            df[is.na(df)] <- 0            
        df
    }
    else if(vote == "testCase"){
        cases <- get(paste("cases_", session, sep = ""))
        df <- results[,,"oks"]
        df <- t((t(df)) / c(cases)) 
        df[df >= percTestCaseOk] <- 1
        df[df < percTestCaseOk] <- 0
        
        if(!keepNA)
            df[is.na(df)] <- 0     
        df
    }        
}

#' Modifica i valori fuori scala nel caso la funzione non convergesse sostituendoli con il massimo o il minimo
#' @param vect Vettore di difficolta'
#' @param min Difficolta' minima
#' @param max Difficolta' massima
#' @return Vettore di difficolta'
#' @examples
#' sourceData <- "data.R"      
#' source(sourceData)
#' numSessions <- 5
#' session <- sessions[numSessions]
#' vote <- "testCase"
#' p <- 0.5
#' numExtraction <- 200
#' min <- -4
#' max <- 4
#' evalWithTestCase <- evaluateSource(session, vote, percTestCaseOk = p)      
#' diffTestCase <- estimateDifficultiesDichotOneDiscrim(bootstrapping(evalWithTestCase, numExtraction))
#' regulateRange(diffTestCase, min, max)
regulateRange <- function(vect, min, max){      
    vect[vect > max] <- max
    vect[vect < min] <- min
    vect
}