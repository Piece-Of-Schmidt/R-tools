


# -------------------------------------------------------------------------
# FUNCTION
# -------------------------------------------------------------------------


#'find_similar_texts
#'
#'compare texts with the help on ngrams and a quanteda document feature matrix. Documents that share a specific amount of ngrams are expected to be duplicates
#'@param texts text object, either as names vector or named list
#'@param ngram ngram values. Single values or vector of numbers allowed 
#'@param thresh relative amount of same ngram values that is evaluated as too high
#'@param print_dups if TRUE duplicated texts are printed for evaluation (the larger the console window the longer the printed texts are)
#'@param keepDTM if TRUE the internally created document feature matrix is saved to the environment
#'
find_similar_texts = function(texts, ngram=4, thresh=0.5, print_dups=T, keepDFM=F){
  
  # safety belt
  if(is.null(names(texts)) | any(duplicated(names(texts)))) stop("texts must have unique names")
  
  # load and install packages if needed
  for(package in c("quanteda", "dplyr", "proxyC", "Matrix")){
    if(!require(package, character.only = T, quietly = T)){
      install = as.logical(as.numeric(readline(paste("  Package", package, "is not installed but required. Shall it be installed now? (NO: 0, YES: 1)  "))))
      if(install) install.packages("xlsx") else break
    }
  }
  
  cat("Calculate tokens (docs = ", length(texts), ", ngram = ", ngram, ", thresh = ", thresh, ")", sep="")
  toks = tokens(unlist(texts), remove_punct = T, remove_symbols = T, remove_numbers = T, remove_url = , remove_separators = T) %>%
    tokens_ngrams(., ngram)
  
  cat("\nCalculate doc similarity\n")
  # create DTM. Save to global environment if desired
  dtm = dfm(toks); if(keepDFM) dtm <<- dtm
  
  # calculate similarities of documents
  comp = dtm %>%
    Matrix(., sparse = TRUE) %>%
    simil(., margin=1)

  # find which documents are most similar
  comp_winners = which(comp>=thresh, arr.ind=TRUE)
  comp_winners = comp_winners[comp_winners[,1]!=comp_winners[,2],] # exclude results for docs matchings against itself

  # print head of texts for evaluation
  if(print_dups & length(comp_winners) > 0){
    dup_ids = sort(unique(rownames(comp_winners)))
    cat("\nSimilar Texts:\n")
    print(
      data.frame(id = dup_ids,
                 text = strtrim(texts[dup_ids], getOption("width")-max(nchar(dup_ids))-5 ),
                 row.names = NULL)
    )
  }
  
  # print small code that helps further investigate the function's output 
  cat("\nTo further analyse similarities use:\n  dups = which(output > thresh, arr.ind=TRUE)\n  dups[dups[,1] != dups[,2],]\n")
  
  # return comparison matrix invisibly
  invisible(comp)
}


# -------------------------------------------------------------------------
# EXAMPLES
# -------------------------------------------------------------------------


texts = c("Buerger muessen sich nicht nur das Virus vom Leib halten, sondern auch Kriminelle, die an ihr Geld wollen. Das sind die neuen Fallen.",
          "Die Tuerkei rutscht in die Rezession. Schuld ist auch die starrsinnige Politik des Praesidenten. Erdogan hat's gegeben, Erdogan hat's genommen.",
          "Studie: 113 Millionen Menschen hungerten 2018 wegen akuter Krisen. Mehr als 113 Millionen Menschen haben einem Bericht zufolge 2018 Hunger leiden müssen.",
          "Unternehmen brauchen topausgebildete Fach- und Fuehrungskraefte, die den Wandel mitgestalten. Digitalisierungsbeauftragte koennen dazu beitragen.",
          "Nur wenige Smartphone-Nutzer verwenden die aktuellste Version des Betriebssystems. Der US-Konzern will nun die Kontrolle ueber die Aktualisierungen uebernehmen.",
          "Die Deutschen besitzen rund 6,5 Prozent der weltweiten Bestaende. Allein in den vergangenen zwei Jahren kamen 250 Tonnen dazu. Die Liebe der Deutschen zu ihren Autos",
          "Die geplante Gesetzesaenderung zur Vermeidung von Dieselfahrverboten in Deutschland koennte Klagen von Handwerkern und von anderen EU-Laendern nach sich ziehen.",
          "Scharfe Kritik am Scheitern der Gespraeche kam vom britischen Unternehmerverband CBI (Confederation of British Industry).  Sechs Wochen verschwendet.",
          "Cain stand auch bei Republikanern in der Kritik US-Praesident Donald Trump muss Ausschau nach einem neuen Kandidaten fuer einen freien Direktorenposte",
          "Millionen Buerger muessen sich nicht nur das Virus vom Leib halten, sondern auch die vielen Betrueger, die jetzt an ihr Geld wollen. So wehren Sie sic")

names(texts) = c("5YTP-8681-JBK9-2014-00000-00",
                 "5VMB-MW71-JBK9-24VP-00000-00",
                 "5VT1-VJ81-JBK9-21CR-00000-00",
                 "5VWT-FS21-DY2B-S2JJ-00000-00",
                 "5VWT-FS21-DY2B-S2KY-00000-00",
                 "5VWT-FS21-DY2B-S2M1-00000-00",
                 "5VHC-2C31-DY2B-S0CF-00000-00",
                 "5W4M-HSK1-DY2B-S1TT-00000-00",
                 "5VYH-7CS1-DY2B-S0BB-00000-00",
                 "5YKR-34T1-JBK9-24BY-00000-00")

out = find_similar_texts(texts, thresh = 0.3, keepDFM = T)
out
