# Create EP1162 plot information RData file
library(readr)

plotDat<-read_csv("data-raw/EP1162_plotInfo.csv",
                         col_types=cols(
                           BA.Target=col_factor(),
                           Plot=col_factor()
                         ))

# save
save(plotDat,file=here::here("data","ep1162_plotData.RData"))
