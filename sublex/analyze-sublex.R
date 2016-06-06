library(plyr)
library(reshape)
library(ggplot2)

oldwd <- getwd()
setwd("~/Work/Dissertation/finnishcorpus/sublex/")
#fn <- "sublexical-output-FB-test.txt"
fn <- "output_test-unigram.txt"

categorizeToPatterns <- function(dat){
    dat$base <- as.character(dat$base)
    dat$pattern.1 <- substr(dat$base, nchar(dat$base) - 2, nchar(dat$base)) %in% c("e j", "i j")
    dat$pattern.2 <- substr(dat$base, nchar(dat$base), nchar(dat$base)) %in% c("e", "i")
    dat$pattern.3 <- substr(dat$base, nchar(dat$base), nchar(dat$base)) == "p"
    dat$pattern.4 <- substr(dat$base, nchar(dat$base), nchar(dat$base)) == "l"
    dat$pattern.5 <- substr(dat$base, nchar(dat$base), nchar(dat$base)) == "j" & !dat$pattern.1
    dat$pattern.6 <- substr(dat$base, 1, 2) %in% c("ee", "ei", "ie", "ii")
    dat$pattern <- ""
    dat[dat$pattern.1,]$pattern <- "1"
    dat[dat$pattern.2,]$pattern <- "2"
    dat[dat$pattern.3,]$pattern <- "3"
    dat[dat$pattern.4,]$pattern <- "4"
    dat[dat$pattern.5,]$pattern <- "5"
    # NO 6 YET
    dat
}

sublex.results <- read.table(fn,
                             header = TRUE,
                             sep = "\t",
                             fill = TRUE,
                             check.names = FALSE
                             )
sublex.results <- sublex.results[6:nrow(sublex.results),]
levels(sublex.results$change) <- gsub("Insert \\[(.*)\\].*", "\\1",
                                      levels(sublex.results$change))

names(sublex.results)
sublex.data <- sublex.results[c("base", "base.harmony", "change", "derivative.harmony", "harmony.sum", "probability")]
sublex.molten <- melt(sublex.data, id.vars = c("base", "change"))
head(sublex.molten)
sublex.allvars <- cast(sublex.molten, base + change ~ . | variable, fun.aggregate = mean)
sublex.harmsum <- sublex.allvars$harmony.sum
head(sublex.harmsum)
names(sublex.harmsum)[3] = "harmony"


#### CONSTRAINT INVESTIGATION

main.constraint.weights <- read.table(fn,
                             header = TRUE,
                             sep = "\t",
                             fill = TRUE,
                             check.names = FALSE,
                             stringsAsFactors = FALSE
                             )
main.constraint.weights <- main.constraint.weights[1:3,c(3,8:ncol(main.constraint.weights))]
#print(main.constraint.weights)
names(main.constraint.weights) <- paste(names(main.constraint.weights), as.character(main.constraint.weights[3,]))
names(main.constraint.weights)[1] <- "change"
main.constraint.weights <- main.constraint.weights[1:2,]
mcw.molten <- melt(main.constraint.weights, id.vars = "change")
names(mcw.molten)[names(mcw.molten) == "variable"] <- "constraint"
mcw.molten$constraint <- as.character(mcw.molten$constraint)
mcw.molten$grammar <- ifelse(substr(mcw.molten$constraint,
                                    nchar(mcw.molten$constraint),
                                    nchar(mcw.molten$constraint)) == "P",
                             "proper",
                             "gatekeeper")
mcw.molten$constraint <- substr(mcw.molten$constraint,
                                1,
                                nchar(mcw.molten$constraint) - 3)
mcw.molten$constraint <- gsub(".1","",mcw.molten$constraint)
mcw.molten$suffix <- ifelse(grepl("[B]",mcw.molten$change),
                            "back",
                            "front")
mcw.molten$change <- NULL
names(mcw.molten)[names(mcw.molten) == "value"] <- "score"
mcw.molten
mcw.molten2 <- melt(mcw.molten, id.vars = c("constraint", "grammar", "suffix"))
mcw.molten2$value <- as.numeric(as.character(mcw.molten2$value))


mc.weights <- cast(mcw.molten2[mcw.molten2$grammar == "gatekeeper",], constraint + grammar ~ suffix, fun.aggregate = mean, value = "value")
mc.weights$prefersback <- mc.weights$back - mc.weights$front
mc.weights[order(mc.weights$prefersback),]

featureValues <- function(feature, constraints){
    re <- regexpr(paste0("[\\+\\-]", feature), constraints)
    ifelse(re == -1, "0", substr(constraints, re, re))
}

mc.weights$front <- featureValues("front", mc.weights$constraint)
mc.weights$rounded <- featureValues("rounded", mc.weights$constraint)
mc.weights$high <- featureValues("high", mc.weights$constraint)
mc.weights$low <- featureValues("low", mc.weights$constraint)

getVowel <- function(front, rounded, high, low){
    invert <- function(feature){
        ifelse(feature == "+", "-", "+")
    }
    mc.weights[mc.weights$front != invert(front) &
               mc.weights$rounded != invert(rounded) &
               mc.weights$high != invert(high) &
               mc.weights$low != invert(low),]
}

# F R H L
vowel.prefersback = melt(data.frame(
    i = sum(getVowel("+","-","+","-")$prefersback),
    e = sum(getVowel("+","-","-","-")$prefersback),
    a = sum(getVowel("-","-","-","+")$prefersback),
    ä = sum(getVowel("+","-","-","+")$prefersback),
    ö = sum(getVowel("+","+","-","-")$prefersback),
    y = sum(getVowel("+","+","+","-")$prefersback),
    o = sum(getVowel("-","+","-","-")$prefersback),
    u = sum(getVowel("-","+","+","-")$prefersback)
))
names(vowel.prefersback) <- c("vowel", "prefersback")
ggplot(vowel.prefersback, aes(y = prefersback, x = vowel)) + geom_point()



mc.gkgp <- cast(mcw.molten2, constraint + suffix ~ grammar, fun.aggregate = mean, value = "value")
mc.gkgp$kminusp <- mc.gkgp$gatekeeper - mc.gkgp$proper
mc.gkgp[order(mc.gkgp$kminusp),]




#### BELOW --- for pattern checking
function(){
    sublex.patts <- categorizeToPatterns(sublex.harmsum)
    sublex.avs <- ddply(sublex.patts, .(pattern, change), summarize, harmony=mean(harmony))


    p <- ggplot(data = sublex.patts[!sublex.patts$pattern.6,])
    p <- p + geom_jitter(aes(y = harmony, x = pattern, colour = change))
    print(p)
    qplot(change, harmony, data = sublex.patts, geom="boxplot")

}

#### BELOW -- for summarizing by harmonicity
sublex.hsmolten <- melt(sublex.harmsum, id.vars = "base")
sublex.wugtable <- cast(sublex.hsmolten, base ~ change)
sublex.wugtable$prefersback <- sublex.wugtable$B - sublex.wugtable$F
sublex.wugresults <- analyzeWugResults(sublex.wugtable, FALSE, "base", "prefersback") 
sublex.wugsumm <- ddply(sublex.wugresults, .(pattern), summarise, prefersback=mean(prefersback))
names(sublex.wugsumm)[2] <- "score"
#wugScoresTable(list(a=sublex.wugsumm))

analyzeSLLexicon <- function(){
    lexfile <- read.table("sublex-training.txt", sep="\t", stringsAsFactors = FALSE)
    names(lexfile) <- c("stem", "suffixed")
    lexfile$phonemes <- strsplit(lexfile$stem, " ")
    lexfile$pattern <- sapply(lexfile$phonemes, function(phs){
        paste0(sapply(phs, annotatePhoneme), collapse = "")
    })

    lexfile$suffix <- ifelse(grepl("B",lexfile$suffixed),
                            "back",
                            "front")
    backsuff <- ddply(lexfile, .(pattern), function(line){
        data.frame(n = nrow(line),
                   back = nrow(line[line$suffix == "back",])
                   )
    })
    backsuff$harmonicity <- sapply(backsuff$pattern, patternHarmonicity)
    backsuff
    ## need: harmonicity, type
}
lexicon.backsuffix <- analyzeSLLexicon()

setwd(oldwd)














