# load libraries
library(SAFARI)

# load sample
data("rBPS")

# segmentation procedure
rBPS <- binary.segmentation(
    rBPS,
    id = c("NLST", "AA00474", "11030"),
    filter = 150,
    categories = c("geometric", "boundary", "topological")
)
