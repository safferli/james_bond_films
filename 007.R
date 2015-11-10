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
library(tidyr)
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
    Salary.of.Bond.actor.2005.adj = as.numeric(f.retrieve.initial.numbers(Salary.of.Bond.actor.1)),
    RoI = Box.office.2005.adj/Budget.2005.adj
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
  # split the RT into rating and number of reviewers
  separate(Rotten.Tomatoes, c("Rotten.Tomatoes.rating", "Rotten.Tomatoes.reviews"), sep="%") %>% 
  mutate(
    Rotten.Tomatoes.reviews = sub("^ \\((\\d*) reviews\\).*", "\\1", Rotten.Tomatoes.reviews)
  )


## final data set
bond.dta <- bond.films %>% 
  merge(bond.ratings) %>% 
  select(-Actor) %>% 
  arrange(Year)


## graphing
bond.dta %>% ggplot()+
  geom_point(aes(x=Year, 
                 y=Box.office.2005.adj, 
                 size=Budget.2005.adj, 
                 colour=as.numeric(Rotten.Tomatoes.rating)
            ))+
  scale_colour_continuous()
ggsave(file="bond-bare-plot.png")

# http://stackoverflow.com/questions/9968975/using-ggplot2-in-r-how-do-i-make-the-background-of-a-graph-different-colours-in
## get Bond actor year grouping for rectangling
actor.grp <- bond.dta %>% 
  # all of this in unneeded, as I'll just let Lazenby "overwrite" Connery in 1969
  # mutate(
  #   grouptemp = ifelse(lag(Bond.actor)==Bond.actor, 0, 1),
  #   grouptemp = ifelse(is.na(grouptemp), 1, grouptemp),
  #   group = cumsum(grouptemp)
  # ) %>% 
  # select(Bond.actor, Year, group) %>% 
  group_by(Bond.actor) %>% 
  summarise(
    yearmin = min(Year), 
    yearmax = max(Year)
  ) %>% 
  ungroup() %>% 
  arrange(yearmin) %>% 
  # George Lazenby only had one film, which makes geom_rect "invisible"
  mutate(
    yearmax = ifelse(yearmin==yearmax, yearmax+1, yearmax)
  )


## plot with actor years of service
ggplot() + 
  # place geom_rect first, then other geoms will "write over" the rectangles
  geom_rect(data = actor.grp, aes(xmin = yearmin, xmax = yearmax, 
                                  ymin = -Inf, ymax = Inf, 
                                  fill = Bond.actor), alpha = 0.3)+
  # write actor names on rectangles
  geom_text(data = actor.grp, aes(x = yearmin, 
                                  # place text rather at the top of the y-axis
                                  y = max(bond.dta$Box.office.2005.adj, na.rm = TRUE), 
                                  label = Bond.actor,
                                  # colour is already mapped to RT.rating (continuous)
                                  # fill=Bond.actor,
                                  angle = 90, 
                                  hjust = 1, vjust = 1
                                  ), alpha = 0.6, size = 5)+
  # film names
  geom_text(data = bond.dta, aes(x = Year, y = 0, 
                                 label = Title, 
                                 angle = 90, 
                                 hjust = 0, vjust = 0.5), size=4)+
  # film data
  geom_point(data = bond.dta, aes(x = Year, 
                                  y = Box.office.2005.adj, 
                                  size = Budget.2005.adj, 
                                  colour = as.numeric(Rotten.Tomatoes.rating)))+
  # Rotten Tomatoes rating gradient
  scale_colour_continuous(low="red", high="green", name = "Rotten Tomatoes rating")+
  # increase minimum point size for readability
  scale_size_continuous(name = "Budget (2005 mil. dollars)", range = c(3, 10))+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  # remove actor names from legend
  guides(fill=FALSE)+
  labs(title = "Box office results, budgets, and ratings of James Bond films\n", 
       x="", y="Box office earnings (in 2005 mil. dollars)")
# export to size that fits everything into graph, use golden ratio
ggsave(file="bond-full.png", width = 30, height = 30/((1+sqrt(5))/2), units = "cm")











