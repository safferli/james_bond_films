rm(list = ls()); gc(); gc()
# options(java.parameters = "-Xmx4096m")#8192
# options(java.parameters = "-XX:-UseConcMarkSweepGC")
# options(java.parameters = "-XX:-UseGCOverheadLimit")
options(bitmapType='cairo')
options(scipen = 999)
#library(bbbi)
library(ggplot2)
library(rvest)
library(dplyr)
#library(tidyr)
#library(data.table)

# Define your workspace: "X:/xxx/"
wd <- "D:/github/james_bond_films/"
setwd(wd)

## functions needed

f.clean.titles <- function(x) {
  # remove everything up to and including the first !
  gsub("^.*?!","", x)
}

f.clean.double.names <- function(x) {
  # remove superfluous first half with comma
  # +2, since +1 for comma and start counting at 1
  substr(x, nchar(x)/2+2, nchar(x))
}

f.clean.footnote.marks <- function(x) {
  # remove all text in square brackets []
  # http://stackoverflow.com/questions/23966678/remove-all-text-between-two-brackets
  # [^\]]* any character except: '\]' (0 or more times, matching most amount possible)
  gsub("\\[[^\\]]*\\]", "", x, perl=TRUE)
}

f.retrieve.initial.numbers <- function(x) {
  # get first numbers of the format d*.d
  sub("(^\\d*\\.\\d).*", "\\1", x)
}


## retrieve wikipedia James Bond films page
#bond.url <-  "https://en.wikipedia.org/w/index.php?title=List_of_James_Bond_films&oldid=688916363"
bond.url <- "D:/github/james_bond_films/List of James Bond films - Wikipedia, the free encyclopedia.htm"


## read the page into R
bond.wiki <- read_html(bond.url)


## film data
bond.films <- bond.wiki %>%
  html_nodes("table") %>%
  # first table in the page
  .[[1]] %>%
  # fill, because wiki uses multi-cell formatting :(
  html_table(fill = TRUE) 

# lots of cleaning to work to be done now...
bond.films %<>% 
  # make clean column names (replace space with dot)
  setNames(make.names(names(bond.films), unique = TRUE)) %>% 
  # remove first and last line (inner-table headers)
  head(-1) %>% tail(-1) %>% 
  mutate(
    # clean wiki-related data import problems
    Title = f.clean.titles(Title),
    Bond.actor = f.clean.double.names(Bond.actor), 
    Director = f.clean.double.names(Director),
    # NA coerced here are what we want -- ignore the warnings
    Box.office = as.numeric(f.clean.footnote.marks(Box.office)),
    Budget = as.numeric(f.retrieve.initial.numbers(Budget)), 
    Salary.of.Bond.actor = as.numeric(f.retrieve.initial.numbers(Salary.of.Bond.actor)),
    # rename the 2005 adjusted price values
    Box.office.2005.adj = as.numeric(Box.office.1), 
    Budget.2005.adj = as.numeric(Budget.1), 
    Salary.of.Bond.actor.2005.adj = as.numeric(f.retrieve.initial.numbers(Salary.of.Bond.actor.1))
  ) %>% 
  # remove old uninformative uniqueness naming
  select(-contains("1"))


## ratings on films
bond.ratings <- bond.wiki %>%
  html_nodes("table") %>%
  # second table in the page
  .[[2]] %>%
  # fill, because of multi-cell formatting :(
  html_table(fill = TRUE)

bond.ratings %<>% 
  setNames(make.names(names(bond.ratings), unique = TRUE)) %>% 
  mutate(
    # rename for later merging
    Title = f.clean.titles(Film),
    Actor = f.clean.double.names(Actor) 
  ) %>% 
  # reorder to original order
  select(Title, Year:Awards) %>% 
  separate(Rotten.Tomatoes, c("Rotten.Tomatoes.rating", "Rotten.Tomatoes.reviews"), sep="%") %>% 
  mutate(
    Rotten.Tomatoes.reviews = sub("^ \\((\\d*) reviews\\).*", "\\1", Rotten.Tomatoes.reviews)
  )


## final data set
bond.dta <- bond.films %>% 
  merge(bond.ratings) %>% 
  select(-Actor) %>% 
  arrange(Year)



# http://stackoverflow.com/questions/9968975/using-ggplot2-in-r-how-do-i-make-the-background-of-a-graph-different-colours-in

#Fake data
dat <- data.frame(x = 1:100, y = cumsum(rnorm(100)))
#Breaks for background rectangles
rects <- data.frame(xstart = seq(0,80,20), xend = seq(20,100,20), col = letters[1:5])


#As Baptiste points out, the order of the geom's matters, so putting your data as last will 
#make sure that it is plotted "on top" of the background rectangles. Updated code, but
#did not update the JPEG...I think you'll get the point.

ggplot() + 
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
  geom_line(data = dat, aes(x,y))





