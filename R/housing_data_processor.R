# Processor for housing data.
# RS05092015
# Data from: https://www.propertypriceregister.ie

library(RJSONIO)

setwd("~/Develop/prop_vis/")

cleanup.prices <- function(prices) {
  # Remove EUR symbol and "," or "."
  # Also convert to numerical.
  as.numeric(gsub("\u0080|,", "", prop.raw$price))
}

# Read data from CSV file.
prop.raw <- read.csv("data/PPR-2015.csv", header = FALSE, encoding = "latin1")[-1, ]
names(prop.raw) <- c("date","address", "pocode", "county", "price", "below.market",
                     "var.excl", "descr", "size.descr")

# Clean prices column.
prop.raw$price <- cleanup.prices(prop.raw$price)

# Now create summary data frame.
median.df <- aggregate(price ~ county, data = prop.raw, median)
max.df <- aggregate(price ~ county, data = prop.raw, max)
min.df <- aggregate(price ~ county, data = prop.raw, min)

prop.summary <- merge(min.df, max.df, by = "county")
prop.summary <- merge(prop.summary, median.df, by = "county")
names(prop.summary) <- c("id", "min_price", "max_price", "median_price")

# Uh-oh, trouble, some of the properties are < 10.000 EUR because they're car parks.
print(prop.raw[which(prop.raw$price==5675), ])

# And some are large developments.
print(prop.raw[which(prop.raw$price==35045714), ])

# Let's define a quantization function.
# Ah...maybe later...
quantize.prices <- function(x) {
  
}

# Now we need to print some money ;-)
# From Stackoverflow post: http://stackoverflow.com/questions/14028995/money-representation-in-r
#print.money <- function(x){
#  if(!is.numeric(x)) {
#    print("MEH!")
#    return(x)
#  }
#  f <- format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".", scientific = FALSE)
#  paste0("EUR ", f)
#}

#prop.summary <- apply(prop.summary, c(1, 2), print.money)

# This is not good yet, but sufficient.
prepjson <- apply(prop.summary, 1, function(x) as.data.frame(t(x)))
js.var = "var pDataRaw = "
cat(paste(js.var, toJSON(prepjson, asIs = TRUE)), 
    file = "data/property_stats.json")

# The below can be used for the coloring:
print(levels(cut(prop.summary$median_price, breaks = 5, dig.lab = 7)))