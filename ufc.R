library(tidyverse)
library(rvest)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

rm(list = ls())
set.seed(1234)
options(scipen=999)

url <- "https://www.espn.com/mma/fighter/history/_/id/2335796"

getwd()

setwd('/Users/adrianharris')


# One page


tmp <- as.data.frame(read_html(url)  %>% 
  html_table(fill = TRUE))

firstname <-  read_html(url) %>%
  html_nodes(x = ., xpath = "//span[@class='truncate min-w-0 fw-light']") %>%
  html_text(., trim = T)

lastname <-  read_html(url) %>%
  html_nodes(x = ., xpath = "//span[@class='truncate min-w-0']") %>%
  html_text(., trim = T)

tmp$fullName <- paste(firstname,lastname)

head(tmp)

# Another Page

url <- "https://www.espn.com/mma/fighter/history/_/id/2560713/derrick-lewis"

tmp <-  as.data.frame(read_html(url) %>% 
  html_table(fill = TRUE))

firstname <-  read_html(url) %>%
  html_nodes(x = ., xpath = "//span[@class='truncate min-w-0 fw-light']") %>%
  html_text(., trim = T)

lastname <-  read_html(url) %>%
  html_nodes(x = ., xpath = "//span[@class='truncate min-w-0']") %>%
  html_text(., trim = T)

tmp$fullName <- paste(firstname,lastname)
head(tmp)

# All pages
alpha <- tolower(LETTERS)
urls <- c()
for(i in 1:length(alpha)){
  urls[i] <- paste0("https://www.espn.com/mma/fighters?search=",alpha[i]) # Generating fighters a  to z
}

emptySet <- vector("list", length(alpha))
for(i in 1:length(urls)){
  emptySet[[i]]  <-  read_html(urls[i]) %>%
    html_nodes(x = ., xpath = "//tr/td/a") %>%
    str_split(., '"') # Taking the get the ids of the fighters 
}

newUrls <- data.frame(text = unlist(emptySet)) %>%
  filter(str_detect(text, 'mma'))

ids <- substr(newUrls$text, start = 19, stop = nchar(newUrls$text)) # Condensing them down to the Id 
finUrls <- paste0("https://www.espn.com/mma/fighter/history/_/id/",ids) # Anding the ids back to the urls  

write.csv(data.frame(urls = finUrls),"Desktop/ufc/urlsData.csv", row.names = FALSE)



#tmp %>%
#  filter(str_detect( Event, 'UFC'))

urlsData <- read_csv("Desktop/ufc/urlsData.csv")
finUrls <-  urlsData$urls

condition <- 1
y <- NULL
# read in urls 
for(i in 26542:length(finUrls)){
  
  missing <- finUrls[i] == "https://www.espn.com/mma/fighter/history/_/id/"
  
  if(missing == T){next
  }
  
  block <- read_html(finUrls[i]) %>% html_table(fill = TRUE)
  ind <- "  " == paste('',block,'')
  
  if(ind == T){ next 
  } 
  
  block <- as.data.frame(read_html(finUrls[i]) %>% html_table(fill = TRUE))
  ind_two <- sum(grepl("UFC", block$Event, fixed = TRUE))
  ind_next <- ind_two >=  condition

  if(ind_next == F){ next
  }
    tmp <-  as.data.frame(read_html(finUrls[i]) %>% 
                                      html_table(fill = TRUE))
    firstname <-  read_html(finUrls[i]) %>%
      html_nodes(x = ., xpath = "//span[@class='truncate min-w-0 fw-light']") %>%
      html_text(., trim = T)
    lastname <-  read_html(finUrls[i]) %>%
      html_nodes(x = ., xpath = "//span[@class='truncate min-w-0']") %>%
      html_text(., trim = T)
    
    tmp$fullName <- paste(firstname,lastname)

    y <- rbind(y, tmp)
    print(i)
    
    Sys.sleep(sample(10, 1) * 0.7)
}

head(y)
dim(y)
# last i 30549
i

View(y)

write.csv(y,"Desktop/ufc/newdata20.csv", row.names = FALSE)


