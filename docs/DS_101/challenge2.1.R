# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(R.utils)
# 1.1 COLLECT PRODUCT FAMILIES ----
product_categories <- c("Cakes-Biscuits", "Candys", "Christmas", "Cornflakes-Cereals-USA", "Drinks", "Schokolade", "Snacks", "Harry-Potter", "Sour-Patch", "Twizzlers", "Sonderangebote")

candy_product_list <- data.frame(Title = character(), Price = character())

for(k in 1:length(product_categories)) {
  
  
  
  
  for(i in 1:30) {
    
    url_home          <- paste("https://house-of-sweets.com/",product_categories[k],"_s",toString(i),sep="")
    
    # Read in the HTML for the entire webpage
    html_home         <- read_html(url_home)
    
    # 1.2 COLLECT PRODUCT CATEGORIES ----
    
    # Combine all Ids to one string so that we will get all nodes at once
    # (seperated by the OR operator ",")
    
    # Extract the urls from the href attribute
    candy_product_list_content <- html_home %>%
      # Select nodes by the ids
      html_nodes(css = "#content") %>%
      html_nodes(css = "#product-list")
    
    candy_price_table <- candy_product_list_content %>%
      html_nodes(css = ".price_wrapper") 
    
    empty <- c(0)
    priceList <- c(0)
    
    for (j in 1:length(candy_price_table)) {
      if(is_empty(candy_price_table[j] %>% html_nodes(css=".price"))) {
        #priceList[j] = "0,00 €"
        empty <- c(empty,j)
        #candy_price_table[j] <- paste(candy_price_table[j],"<strong class=\"price\">
        #           <span>0,00 €</span>
        #                               <span class=\"footnote-reference\">*</span>                </strong>",sep=" ")
      } else {
        #value <- candy_price_table[j] %>%
        #   html_nodes(css=".price") %>%
        #  html_text()
        #priceList[j] = value
      }
    }
    
    empty <- empty[-1]
    
    candy_price_table <- candy_price_table %>%
      html_nodes(css = ".price") %>%
      html_text()  %>%
      enframe(name = "Price", value = ".value")
    
    
    candy_price_table <- candy_price_table[,c(".value")]
    names(candy_price_table)[1] <- "Price"
    
    for (i in 1:length(candy_price_table$Price)) {
      candy_price_table$Price[i] = str_extract(candy_price_table$Price[i], "\\d+,\\d+ €")
    }
    
    
    if (!is_empty(empty)) {
      candy_price_table<- data.frame(Price = insert(candy_price_table$Price,empty,values=NA))
    } 
    
    candy_title_table <- candy_product_list_content %>%
      html_nodes(css = ".title") %>%
      html_text(trim = TRUE) %>%
      enframe(name = "Title", value = ".title")
    
    candy_title_table <- candy_title_table[,c(".title")]
    names(candy_title_table)[1] <- "Title"
    
    candy_product_list_pre <- cbind(candy_title_table, candy_price_table)
    
    candy_product_list <- rbind(candy_product_list,candy_product_list_pre)
    
  }
}

candy_product_list <- unique(candy_product_list)
rownames(candy_product_list) <- NULL
#write.xlsx(candy_product_list,"produkt_preis_tabelle.xlsx")