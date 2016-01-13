install.packages("arules")
library("arules")
load("./titanic.raw.rdata")

rules_apriori <- apriori(titanic.raw, 
                 parameter = list(supp = 0.01, conf = 0.80, minlen = 2, 
                                  target = "closed frequent itemsets"))

rules_apriori.sorted <- sort(rules_apriori, by="support")
inspect(rules_apriori.sorted[1:10])

rules_eclat <- eclat(titanic.raw, 
      parameter = list(supp = 0.01, minlen = 2, 
                       rhs = c("Survived=Yes", "Survived=No"),
                       target = "closed frequent itemsets"))

rules_eclat.sorted <- sort(rules_eclat, by="support")
inspect(rules_eclat.sorted[1:10])