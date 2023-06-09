


# -------------------------------------------------------------------------
# FUNCTION
# -------------------------------------------------------------------------


#'find_similar_texts
#'
#'compare texts with the help on ngrams and a quanteda document feature matrix. Documents that share a specific amount of ngrams are expected to be duplicates
#'@param texts text object, either as named vector or named list
#'@param ngram ngram values. Single values or vector of numbers allowed 
#'@param max_text_length if desired, all texts are shrinked to max_text_length tokens. This makes the code run faster, at the price of losing arguably important information 
#'@param stopwords stopwords to be excluded prior to text comparison (as vector)
#'@param min_occ how often shall an ngram occure in the whole corpus so it is considered in the comparison matrix
#'@param thresh relative amount of same ngram values that is evaluated as too high
#'@param keep_first if duplicates are found, the first one of them is kept, respectively
#'@param print if TRUE progress and duplicated texts are printed for evaluation (the larger the console window the longer the printed texts are)
#'
find_similar_texts = function(texts, ngram=4, max_text_length=NULL, stopwords=NULL, min_occ=1, thresh=0.8, keep_first=T, print=T){
  
  # safety belt
  if(is.null(names(texts)) | any(duplicated(names(texts)))) stop("texts must have unique names")
  
  # load and install packages if needed
  for(package in c("quanteda", "dplyr", "quanteda.textstats")){
    if(!require(package, character.only = T, quietly = T)){
      install = as.logical(as.numeric(readline(paste("  Package", package, "is not installed but required. Shall it be installed now? (NO: 0, YES: 1)  "))))
      if(install) install.packages(package) else break
    }
  }
  
  if(print) cat("Calculate tokens (docs = ", length(texts), ", ngram = ", ngram, ", thresh = ", thresh, ")", sep="")
  
  toks = tokens(unlist(texts), remove_punct = T, remove_symbols = T, remove_numbers = T, remove_url = , remove_separators = T) %>%
    {if(!is.null(max_text_length)) toks = tokens_select(., endpos = max_text_length) else .} %>%
    tokens_tolower() %>%
    {if(!is.null(stopwords)) tokens_remove(., stopwords) else .} %>%
    tokens_ngrams(., ngram)
  
  if(print) cat("\nCreate DFM\n")
  # create DFM
  dtm = dfm(toks) %>%
    dfm_trim(., min_termfreq=min_occ)
  
  if(print) cat("\nCalculate doc similarity\n")
  # calculate similarities of documents
  simils = textstat_simil(dtm, margin="documents", method="cosine")

  # find which documents are most similar
  comp_winners = which(simils>=thresh, arr.ind=TRUE)
  comp_winners = comp_winners[comp_winners[,1]!=comp_winners[,2],] # exclude results for docs matchings against itself
  
  # print head of texts for evaluation
  if(print & length(comp_winners) > 0){
    dup_ids = sort(unique(rownames(comp_winners)))
    cat("\nSimilar Texts:\n")
    print(
      data.frame(id = dup_ids,
                 text = substr(texts[dup_ids], 1, getOption("width")-max(nchar(dup_ids))-5 ),
                 row.names = NULL)
    )
  }
  
  # if(print) cat("\nTo further analyse similarities use:\n  dups = which(simils > thresh, arr.ind=TRUE)\n  dups[dups[,1] != dups[,2],]\n")
  
  # return comparison matrix invisibly
  if(keep_first) not_dups = names(texts)[! names(texts) %in% rownames(unique(t(apply(comp_winners, 1, sort))))]
  else not_dups = names(texts)[! names(texts) %in% rownames(comp_winners)]
  
  invisible(list(duplicates = rownames(comp_winners),
                 locations = comp_winners,
                 not_duplicated_texts = not_dups,
                 simils = simils,
                 dfm = dtm))
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

out = find_similar_texts(texts, thresh = 0.3)
out
