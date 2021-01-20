library(rvest)
#reviewPage <- read_html("https://www.airlinequality.com/airline-reviews/air-canada")
#airplaneUrl <- "/airline-reviews/air-canada"

baseUrl <- "https://www.airlinequality.com"
get_airplane_reviews <- function(airplaneUrl, incProgress = NULL) {
  
  withProgress(message = "Collecting data ", value = 0, {
    
    reviewPage <- read_html(paste(baseUrl, airplaneUrl, sep = ""))
    review <- reviewPage %>%
      html_nodes('.text_content') %>%
      html_text()
    reviewer <- reviewPage %>%
      html_nodes('.userStatusWrapper') %>%
      html_text()
    reviewer <- gsub("\r\n","", reviewer)
    
    reviews <- character()
    reviewers <- character()
    reviews <- c(reviews, review)
    reviewers <- c(reviewers, reviewer)
    
    
    for(pageResult in seq(from = 2, to = 11)){
      nextPage <- paste0(airplaneUrl,"/page/",pageResult,"/")
      
      incProgress(10/length(reviews), detail = paste(length(reviews), " data"))
      print(paste(length(reviews), "data", "collected"))
      
      reviewUrl <- paste(baseUrl, nextPage, sep = "")
      reviewPage <- read_html(reviewUrl)
      
      review <- reviewPage %>%
        html_nodes('.text_content') %>%
        html_text()
      
      reviewer <- reviewPage %>%
        html_nodes('.userStatusWrapper') %>%
        html_text()
      reviewer <- gsub("\r\n","", reviewer)
      
      reviews <- c(reviews, review)
      reviewers <- c(reviewers, reviewer)
      
      print(nextPage)
      print(reviewUrl)
    }
    
    totalReviews <- length(reviews)
    
    print(paste(length(reviews), "data", "collected"))

    
  })
  
  return(data.frame(reviewer = reviewers, review = reviews, stringsAsFactors = FALSE))
}
