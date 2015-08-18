UCLAPL.output <- "~/Work/UCLAPL/finnish/output-wugs/blickTestResults.txt"
all.wugs <- read.table(UCLAPL.output, sep="\t", header=TRUE)
all.wugs <- all.wugs[3:nrow(all.wugs), c("word","score")]

all.wugs$score <- as.numeric(as.character(all.wugs$score))
summary(all.wugs)

zeroes <- all.wugs[all.wugs$score==0,]
n <- nrow(all.wugs)
nzero <- nrow(zeroes)
print(paste0("Total wugs = ", n, ", total 0's = ", nzero, "; ", nzero/n*100, "%"))

## Let's annotate here, not in hanalyze1 :((
zeroes$pattern <- factor("none",levels=c("none","1","2","3","4","5"))
zeroes[grepl("[aeiou] j [aeiou]",zeroes$word),]$pattern <- "1"
zeroes[grepl("[aeiou] [aeiou]",zeroes$word),]$pattern <- "2" ## ouchie
zeroes[grepl(" i[ei]? pp? [aeiou]",zeroes$word),]$pattern <- "3"
zeroes[grepl("[aeiou] ll? [aeiou]",zeroes$word),]$pattern <- "4"
zeroes[grepl("[aeiou] [fsh] j [aeiou]",zeroes$word),]$pattern <- "5"

for(i in levels(zeroes$pattern)){
    print(paste0("Pattern ", i, ": ", nrow(zeroes[zeroes$pattern==i,])))
}


## filter for existence


## let's sample some wugs.

# TOTAL will be 160
# 20 by pattern, 60 none?

create.sample = function(data, n.byPattern = 20, n.none = 60){
    nones = subset(data, pattern == "none")
    sampled = nones[sample(nrow(nones), n.none),]
    for(i in levels(data$pattern)){
        if (i != "none"){
            d = subset(data, pattern == i)
            s = d[sample(nrow(d), n.byPattern),]
            sampled = rbind(sampled, s)
        }
    }
    sampled
}

remove.spaces = function (x){
    rv <- x
    rv$word <- gsub(" ","", x$word)
    rv
}


new.sample <- create.sample(zeroes)
unspaced <- remove.spaces(new.sample)
write.table(new.sample, "wugs-to-test.csv", quote = FALSE, sep = ",", row.names = FALSE)
