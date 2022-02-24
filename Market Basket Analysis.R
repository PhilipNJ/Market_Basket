library(arules)
library(arulesViz)
library(datasets)

data("Groceries")

itemFrequencyPlot(Groceries,topN=20,type="absolute")

View(Groceries)

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

rules_1 <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.7))

options(digits=3)
inspect(rules[1:5])


options(digits=3)
inspect(rules[399:410])


head(Groceries, n=10)

summary(rules)

rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[15:30])

library(colorspace) # for sequential_hcl
plot(rules, control=list(col=sequential_hcl(100))).

plot(rules, shading="order", control=list(main = "Two-key plot",col=rainbow(5)))

subrules <- subset(rules, lift>7)
inspect(subrules[])

plot(subrules, method="matrix", measure="lift")
plot(subrules, method="matrix", measure="lift", control=list(reorder="support/confidence"))



subset.matrix <- is.subset(rules, rules1)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules2<-rules.pruned
summary(rules2)


rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08),
               appearance = list(default="lhs",rhs="rice"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2),
               appearance = list(default="rhs",lhs="bottled beer"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

