library(ggplot2)
library(grid)
library(dplyr)

chr1 <- read.csv("1_chr.csv", header = TRUE)
chr2 <- read.csv("2_chr.csv", header = TRUE)
chr <- rbind(chr1, chr2)
sam1 <- read.csv("1_sam.csv", header = TRUE)
sam2 <- read.csv("2_sam.csv", header = TRUE)
sam <- rbind(sam1, sam2)
lev <- read.csv("lev.csv", header = TRUE)
esth <- read.csv("esth.csv", header = TRUE)


#chr_sam <- rbind(chr, sam)

#clause type
sam_ctyp = as.data.frame(table(sam$ctyp, exclude=""))
sam_ctyp <- cbind(sam_ctyp, prop.table(sam_ctyp$Freq)*100)
names(sam_ctyp) <- c("ctyp", "freq", "prop")
#sam_ctyp <- sam_ctyp[!(sam_ctyp$sam < 200),]
sam_ctyp <- sam_ctyp[order(sam_ctyp$freq,decreasing=T)[1:5],]
sam_ctyp <- arrange(sam_ctyp, desc(freq))

chr_ctyp = as.data.frame(table(chr$ctyp, exclude=""))
chr_ctyp <- cbind(chr_ctyp, prop.table(chr_ctyp$Freq)*100)
names(chr_ctyp) <- c("ctyp", "freq", "prop")
#chr_ctyp <- chr_ctyp[!(chr_ctyp$chr < 200),]
chr_ctyp <- chr_ctyp[order(chr_ctyp$freq,decreasing=T)[1:5],]
chr_ctyp <- arrange(chr_ctyp, desc(freq))

lev_ctyp = as.data.frame(table(lev$ctyp, exclude=""))
lev_ctyp <- cbind(lev_ctyp, prop.table(lev_ctyp$Freq)*100)
names(lev_ctyp) <- c("ctyp", "freq", "prop")
#lev_ctyp <- lev_ctyp[!(lev_ctyp$lev < 200),]
lev_ctyp <- lev_ctyp[order(lev_ctyp$freq,decreasing=T)[1:5],]
lev_ctyp <- arrange(lev_ctyp, desc(freq))

esth_ctyp = as.data.frame(table(esth$ctyp, exclude=""))
esth_ctyp <- cbind(esth_ctyp, prop.table(esth_ctyp$Freq)*100)
names(esth_ctyp) <- c("ctyp", "freq", "prop")
esth_ctyp <- esth_ctyp[order(esth_ctyp$freq,decreasing=T)[1:5],]
esth_ctyp <- arrange(esth_ctyp, desc(freq))

ctyp1 <- ggplot(data=sam_ctyp, aes(x=ctyp, y=prop)) + 
#  geom_bar(stat="identity", width=1) +
  geom_bar(stat="identity") +
  ggtitle("Clause Type of Samuel")

ctyp2 <- ggplot(data=chr_ctyp, aes(x=ctyp, y=prop)) + 
#  geom_bar(stat="identity", width=1) +
  geom_bar(stat="identity") +
  ggtitle("Clause Type of Chronicles")

ctyp3 <- ggplot(data=lev_ctyp, aes(x=ctyp, y=prop)) + 
  #  geom_bar(stat="identity", width=1) +
  geom_bar(stat="identity") +
  ggtitle("Clause Type of Leviticus")

ctyp4 <- ggplot(data=esth_ctyp, aes(x=ctyp, y=prop)) + 
  geom_bar(stat="identity") +
  ggtitle("Clause Type of Esther")


grid.newpage()
grid.draw(rbind(ggplotGrob(ctyp1), ggplotGrob(ctyp2), ggplotGrob(ctyp3), ggplotGrob(ctyp4), size = "last"))


#preposition
sam_prep <- filter(sam, sp == 'prep')
sam_prep <- select(sam_prep, lex)
sam_prep = as.data.frame(table(sam_prep$lex))
sam_prep <- cbind(sam_prep, prop.table(sam_prep$Freq)*100)
names(sam_prep) <- c("prep", "freq", "prop")
sam_prep <- sam_prep[!(sam_prep$prep == 'את'),]
sam_prep <- sam_prep[order(sam_prep$freq,decreasing=T)[1:5],]

chr_prep <- filter(chr, sp == 'prep')
chr_prep <- select(chr_prep, lex)
chr_prep = as.data.frame(table(chr_prep$lex))
chr_prep <- cbind(chr_prep, prop.table(chr_prep$Freq)*100)
names(chr_prep) <- c("prep", "freq", "prop")
chr_prep <- chr_prep[!(chr_prep$prep == 'את'),]
chr_prep <- chr_prep[order(chr_prep$freq,decreasing=T)[1:5],]

lev_prep <- filter(lev, sp == 'prep')
lev_prep <- select(lev_prep, lex)
lev_prep = as.data.frame(table(lev_prep$lex))
lev_prep <- cbind(lev_prep, prop.table(lev_prep$Freq)*100)
names(lev_prep) <- c("prep", "freq", "prop")
lev_prep <- lev_prep[!(lev_prep$prep == 'את'),]
lev_prep <- lev_prep[order(lev_prep$freq,decreasing=T)[1:5],]

esth_prep <- filter(esth, sp == 'prep')
esth_prep <- select(esth_prep, lex)
esth_prep = as.data.frame(table(esth_prep$lex))
esth_prep <- cbind(esth_prep, prop.table(esth_prep$Freq)*100)
names(esth_prep) <- c("prep", "freq", "prop")
esth_prep <- esth_prep[!(esth_prep$prep == 'את'),]
esth_prep <- esth_prep[order(esth_prep$freq,decreasing=T)[1:5],]

prep1 <- ggplot(data=sam_prep, aes(x=prep, y=prop)) + 
  geom_bar(stat="identity") +
  ggtitle("Preposition of Samuel")

prep2 <- ggplot(data=chr_prep, aes(x=prep, y=prop)) + 
  geom_bar(stat="identity") +
  ggtitle("Preposition of Chronicles")

prep3 <- ggplot(data=lev_prep, aes(x=prep, y=prop)) + 
  geom_bar(stat="identity") +
  ggtitle("Preposition of Leviticus")


prep4 <- ggplot(data=esth_prep, aes(x=prep, y=prop)) + 
  geom_bar(stat="identity") +
  ggtitle("Preposition of Esther")

grid.newpage()
grid.draw(rbind(ggplotGrob(prep1), ggplotGrob(prep2), ggplotGrob(prep3), ggplotGrob(prep4), size = "last"))


#proportion of nominal clause of each book
sam_NmCl <- sam_ctyp[(sam_ctyp$ctyp == 'NmCl' | sam_ctyp$ctyp == 'Ptcp' | sam_ctyp$ctyp == 'InfC'),]
sum_sam = sum(sam_NmCl[, c(3)])

chr_NmCl <- chr_ctyp[(chr_ctyp$ctyp == 'NmCl' | chr_ctyp$ctyp == 'Ptcp' | chr_ctyp$ctyp == 'InfC'),]
sum_chr = sum(chr_NmCl[, c(3)])

lev_NmCl <- lev_ctyp[(lev_ctyp$ctyp == 'NmCl' | lev_ctyp$ctyp == 'Ptcp' | lev_ctyp$ctyp == 'InfC'),]
sum_lev = sum(lev_NmCl[, c(3)])

esth_NmCl <- esth_ctyp[(esth_ctyp$ctyp == 'NmCl' | esth_ctyp$ctyp == 'Ptcp' | esth_ctyp$ctyp == 'InfC'),]
sum_esth = sum(esth_NmCl[, c(3)])

book <- c("sam", "chr", "lev", "esth")
NC <- c(sum_sam, sum_chr, sum_lev, sum_esth)
NCtable <- data.frame(book, NC)

ggplot(data=NCtable, aes(x=book, y=NC)) + 
  geom_point() +
  ggtitle("proportion of Nominal Clause")


#proportion of על and אל of each book
sam_al = sam_prep[sam_prep$prep == "על",]
sam_al = sam_al$freq
sam_el = sam_prep[sam_prep$prep == "אל",]
sam_el = sam_el$freq
sam_al_el = sam_al / sam_el

chr_al = chr_prep[chr_prep$prep == "על",]
chr_al = chr_al$freq
chr_el = chr_prep[chr_prep$prep == "אל",]
chr_el = chr_el$freq
chr_al_el = chr_al / chr_el

lev_al = lev_prep[lev_prep$prep == "על",]
lev_al = lev_al$freq
lev_el = lev_prep[lev_prep$prep == "אל",]
lev_el = lev_el$freq
lev_al_el = lev_al / lev_el

esth_al = esth_prep[esth_prep$prep == "על",]
esth_al = esth_al$freq
esth_el = esth_prep[esth_prep$prep == "אל",]
esth_el = esth_el$freq
esth_al_el = esth_al / esth_el

book <- c("sam", "chr", "lev", "esth")
alel <- c(sam_al_el, chr_al_el, lev_al_el, esth_al_el)
aleltable <- data.frame(book, alel)

ggplot(data=aleltable, aes(x=book, y=alel)) + 
  geom_point() +
  ggtitle("Proportion of על and אל")

