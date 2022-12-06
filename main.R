library(rvest)

httr::set_config(httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36"))

# initializing the lists that will store
# the scraped data
product_urls <- list()
product_images <- list()
product_names <- list()
product_prices <- list()

# initializing the list of pages to scrape with the
# first pagination links
pages_to_scrape <- list("https://scrapeme.live/shop/page/1/")

# initializing the list of pages discovered
pages_discovered <- pages_to_scrape

# current iteration
i <- 1
# max pages to scrape
limit <- 1

# until there is still a page to scrape
while (length(pages_to_scrape) != 0 && i <= limit) {
  # getting the current page to scrape
  page_to_scrape <- pages_to_scrape[[1]]

  # removing the page to scrape from the list
  pages_to_scrape <- pages_to_scrape[-1]

  # retrieving the current page to scrape
  document <- read_html(page_to_scrape,
                        user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:73.0) Gecko/20100101 Firefox/73.0")

  # extracting the list of pagination links
  new_pagination_links <- document %>%
    html_elements("a.page-numbers") %>%
    html_attr("href")

  # iterating over the list of pagination links
  for (new_pagination_link in new_pagination_links) {
    # if the web page discovered is new and should be scraped
    if (!(new_pagination_link %in% pages_discovered) && !(new_pagination_link %in% page_to_scrape)) {
      pages_to_scrape <- append(new_pagination_link, pages_to_scrape)
    }

    # discovering new pages
    pages_discovered <- append(new_pagination_link, pages_discovered)
  }

  # removing duplicates from pages_discovered
  pages_discovered <- pages_discovered[!duplicated(pages_discovered)]

  # selecting the list of product HTML elements
  html_products <- document %>% html_elements("li.product")

  # appending the new results to the lists
  # of scraped data
  product_urls <- c(
    product_urls,
    html_products %>%
      html_element("a") %>%
      html_attr("href")
  )
  product_images <- c(
    product_images,
    html_products %>%
      html_element("img") %>%
      html_attr("src"))
  product_names <- c(
    product_names,
    html_products %>%
      html_element("h2") %>%
      html_text2()
  )
  product_prices <- c(
    product_prices,
    html_products %>%
      html_element("span") %>%
      html_text2()
  )

  # incrementing the iteration counter
  i <- i + 1
}

# converting the lists containg the scraped data
# into a data.frame
products <- data.frame(
  unlist(product_urls),
  unlist(product_images),
  unlist(product_names),
  unlist(product_prices)
)

# changing the column names of the data frame
# before exporting it into CSV
names(products) <- c("url", "image", "name", "price")

# export the data frame containing the scraped data
# to a CSV file
write.csv(products, file = './products.csv', fileEncoding = "UTF-8", row.names = FALSE)