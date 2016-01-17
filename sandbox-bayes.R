library(bnlearn)
train <- read.csv('trainset.csv', header = TRUE, row.names = 1)
train$Made.Donation.in.March.2007 <- factor(train$Made.Donation.in.March.2007, levels = c(0,1), labels = c('No', 'Yes'))

trainset <- data.frame(sinceLast = cut(train$Months.since.Last.Donation, breaks = 5),
                       sinceFirst = cut(train$Months.since.First.Donation, breaks = 5),
                       totalVolume = cut(train$Total.Volume.Donated..c.c.., breaks = 5),
                       donationCount = cut(train$Number.of.Donations, breaks = 5),
                       madeDonation = train$Made.Donation.in.March.2007)

bg <- hc(trainset)
graphviz.plot(bg)

ag <- empty.graph(colnames(trainset))

arcsDf <- data.frame(
    from = c('sinceLast', 'donationCount'),
    to = c('madeDonation'),
    stringsAsFactors = FALSE)
arcs(ag, ignore.cycles = T) <- arcsDf

nodes(ag)
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
graphviz.plot(ag)

fit <- bn.fit(ag, trainset)
fit2 <- bn.fit(bg, trainset)

cut(train$Months.since.Last.Donation, 10)

cpquery(fit2, (madeDonation == 'Yes'),
        (sinceFirst == '(78.8,98.1]'))

cpquery(fit, (Made.Donation.in.March.2007 == 'Yes'),
        ((Months.since.First.Donation == '(88.4,98.1]') & (Months.since.Last.Donation == '(-0.074,14.8]')))


str <- "a"

findProb <- function(row, fit) {
    str <<- paste("(", names(row)[1], "=='",
                 sapply(row[1], as.character), "')",
                 sep = "", collapse = " & ")
    # print(str)
    cpquery(fit, (madeDonation == 'Yes'), eval(parse(text = str)))
}

library(magrittr)

table(trainset$madeDonation, apply(trainset, 1, . %>% findProb(fit)) > 0.5)
table(trainset$madeDonation, apply(trainset, 1, . %>% findProb(fit2)) > 0.25)
str
