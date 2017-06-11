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

#chr_sam <- rbind(chr, sam)

#clause type
sam_ctyp = as.data.frame(table(sam$ctyp, exclude=""))
names(sam_ctyp) <- c("ctyp", "freq")
#sam_ctyp <- sam_ctyp[!(sam_ctyp$sam < 200),]
sam_ctyp <- sam_ctyp[order(sam_ctyp$freq,decreasing=T)[1:5],]
sam_ctyp <- arrange(sam_ctyp, desc(freq))

chr_ctyp = as.data.frame(table(chr$ctyp, exclude=""))
names(chr_ctyp) <- c("ctyp", "freq")
#chr_ctyp <- chr_ctyp[!(chr_ctyp$chr < 200),]
chr_ctyp <- chr_ctyp[order(chr_ctyp$freq,decreasing=T)[1:5],]
chr_ctyp <- arrange(chr_ctyp, desc(freq))

lev_ctyp = as.data.frame(table(lev$ctyp, exclude=""))
names(lev_ctyp) <- c("ctyp", "freq")
#lev_ctyp <- lev_ctyp[!(lev_ctyp$lev < 200),]
lev_ctyp <- lev_ctyp[order(lev_ctyp$freq,decreasing=T)[1:5],]
lev_ctyp <- arrange(lev_ctyp, desc(freq))

ctyp1 <- ggplot(data=sam_ctyp, aes(x=ctyp, y=freq)) + 
#  geom_bar(stat="identity", width=1) +
  geom_bar(stat="identity") +
  ggtitle("Clause Type of Samuel")

ctyp2 <- ggplot(data=chr_ctyp, aes(x=ctyp, y=freq)) + 
#  geom_bar(stat="identity", width=1) +
  geom_bar(stat="identity") +
  ggtitle("Clause Type of Chronicles")

ctyp3 <- ggplot(data=lev_ctyp, aes(x=ctyp, y=freq)) + 
  #  geom_bar(stat="identity", width=1) +
  geom_bar(stat="identity") +
  ggtitle("Clause Type of Leviticus")

grid.newpage()
grid.draw(rbind(ggplotGrob(ctyp1), ggplotGrob(ctyp2), ggplotGrob(ctyp3), size = "last"))


#preposition
sam_prep <- filter(sam, sp == 'prep')
sam_prep <- select(sam_prep, lex)
sam_prep = as.data.frame(table(sam_prep$lex))
names(sam_prep) <- c("prep", "freq")
sam_prep <- sam_prep[!(sam_prep$prep == 'את'),]
sam_prep <- sam_prep[order(sam_prep$freq,decreasing=T)[1:5],]

chr_prep <- filter(chr, sp == 'prep')
chr_prep <- select(chr_prep, lex)
chr_prep = as.data.frame(table(chr_prep$lex))
names(chr_prep) <- c("prep", "freq")
chr_prep <- chr_prep[!(chr_prep$prep == 'את'),]
chr_prep <- chr_prep[order(chr_prep$freq,decreasing=T)[1:5],]

lev_prep <- filter(lev, sp == 'prep')
lev_prep <- select(lev_prep, lex)
lev_prep = as.data.frame(table(lev_prep$lex))
names(lev_prep) <- c("prep", "freq")
lev_prep <- lev_prep[!(lev_prep$prep == 'את'),]
lev_prep <- lev_prep[order(lev_prep$freq,decreasing=T)[1:5],]

prep1 <- ggplot(data=sam_prep, aes(x=prep, y=freq)) + 
  geom_bar(stat="identity") +
  ggtitle("Preposition of Samuel")

prep2 <- ggplot(data=chr_prep, aes(x=prep, y=freq)) + 
  geom_bar(stat="identity") +
  ggtitle("Preposition of Chronicles")

prep3 <- ggplot(data=lev_prep, aes(x=prep, y=freq)) + 
  geom_bar(stat="identity") +
  ggtitle("Preposition of Leviticus")


grid.newpage()
grid.draw(rbind(ggplotGrob(prep1), ggplotGrob(prep2), ggplotGrob(prep3), size = "last"))

