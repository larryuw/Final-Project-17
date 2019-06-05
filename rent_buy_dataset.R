#read the dataset
rent <- read.csv("data/County_MedianRentalPrice_3Bedroom.csv", stringsAsFactors = F)
buy <- read.csv("data/County_Zhvi_3bedroom.csv", stringsAsFactors = F)

#select useful data and reformat the dataset
rent_wa <- rent %>% filter(State == 'WA')
buy_wa <- buy %>% filter(State == 'WA') 

filtered_rent <- rent_wa[, c(1, 19:118)]
filtered_buy <- buy_wa[, c(2,185:284)]

colnames(filtered_rent) <- gsub("X", "", colnames(filtered_rent))
colnames(filtered_buy) <- gsub("X", "", colnames(filtered_buy))

#reshap_rent <- melt(filtered_rent, id = 'RegionName')
#reshap_buy <- melt(filtered_buy, id = 'RegionName')
