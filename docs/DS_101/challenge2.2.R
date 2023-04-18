# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web ScrapingS
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# 1.1 COLLECT PRODUCT FAMILIES ----

url_home          <- "https://www.rosebikes.de"

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

# Web scrape the ids for the families
bike_family_categories <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".main-navigation-category-with-tiles__item") %>%
  html_text(trim = TRUE) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "Category") #%>%
bike_family_categories <- bike_family_categories[,c("Category")]


# 1.2 COLLECT PRODUCT CATEGORIES ----

## "#js-navigationList-ROAD, #js-navigationList-MOUNTAIN, #js-navigationList-EBIKES, #js-navigationList-HYBRID-CITY, #js-navigationList-YOUNGHEROES"

# Extract the urls from the href attribute
bike_price_table <- data.frame(Category = character(), .price = character())

for(k in 1:5) {#length(bike_family_categories$Category)) {
  url_home          <- paste("https://www.rosebikes.de/fahrräder/",bike_family_categories$Category[k],sep="")
  
  # Read in the HTML for the entire webpage
  bike_category_tbl         <- read_html(url_home)
  bike_price_table_pre <- bike_category_tbl %>%
    html_nodes(css = ".catalog-category-bikes")%>%
    html_nodes(css = ".row") %>%
    html_nodes(css = ".catalog-category-bikes__price-title") %>%
    html_text(trim = TRUE) %>%
    enframe(name = "Price", value = ".value")
  
  bike_price_table_pre <- bike_price_table_pre[,c(".value")]
  
  for (j in 1:length(bike_price_table_pre$.value)) {
    if(bike_price_table_pre$.value[j] == "") {
      bike_price_table_pre$.value[j] = "0"
    }
  }
  
  categoryList <- c(rep(bike_family_categories$Category[k],length(bike_price_table_pre$.value)))
  bike_price_table_pre <- cbind(categoryList, bike_price_table_pre)
  bike_price_table <- rbind(bike_price_table_pre,bike_price_table)
  bike_price_table_pre <- data.frame(Category = character(), .price = character())
  }
bike_price_table <- unique(bike_price_table)

bike_price_table

