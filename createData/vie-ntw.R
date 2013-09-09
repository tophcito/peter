library(peter)
vie.lis <- getNetwork.vie()
vie <- as.ntw(vie.lis, toASCII=TRUE)
save(file="./data/vie.RData", vie)
