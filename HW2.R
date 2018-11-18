#STATISTISK DATA BEHANDLING (HOMEWORK 2)
#=============================================================================

#----------Läsa in data----------------------------------------------------------------
library(tidyverse)
species_data <- read_csv("/Users/admin/Desktop/Statistisk Databehandling/Homework/HW_data/SpeciesObservations-2018-11-07-14-23-54.csv")
#OBS: tidyverse own read_csv() function was used here
glimpse(species_data)

#============DATA TIDYING======================================================


#CHANGE SPACE TO UNDERSCORE-----------------------------------------------
names(species_data) <- gsub(" ", "_", names(species_data)) 
#byter alla kolumn-namn med innehållande mellanslag till underlines istället


#Ändra format till as.Date ----------------------------------------------
species_data <- species_data %>%
  mutate( Start = as.Date(Start, "%d/%m/%Y")) 
  #Ändra format till DATE på 'Start'-kolumnen

species_data <- species_data %>%
  mutate( End = as.Date(Start, "%d/%m/%Y")) 
  #Ändra format till DATE på 'End'-kolumnen


#Ändra format till BOOL -------------------------------------------------
species_data <- species_data %>%
  mutate( Uncertain_determination = as.logical(Uncertain_determination) )
  #Ändra format till BOOL (logical=TRUE/FALSE) på 'Uncertain_determination'-kolumnen


#Check data after changes -----------------------------------------------
glimpse(species_data) #Checka om formatet stämmer

#Testing weekdays()
weekdays(as.Date("2018-04-06")) #==> [1] "Fredag"


#Create variable Date (that simply is eq to Start) ----------------------
species_data <- species_data %>%
  mutate(Date=Start)
#=============================================================================


#==========TASK 1=========================================================================
# List the most recorded bird species (Organism_group == "Fåglar") in January and in July 
# (two tables with Common_name and number observed as columns). The function knitr::kable 
# is useful for rendering tables in Markdown.

birds_jan_jul <- species_data %>% 
  mutate(what_month=months(Date)) %>% #Translate dates to months
  filter(Organism_group == "Fåglar", what_month == c("Januari", "Juli")) %>% #Filter out what we need
  select(Common_name, what_month) #pick out only name and months

levels(as.factor(birds_jan_jul$what_month)) #==>[1] "Januari" "Juli" #Double checking

NrBirds_Jan_Jul=as.data.frame(table(birds_jan_jul)) 
#'table' summarizes the number-of-birdname-observations in Common_name with resp. to month

NrBirds_Jan <- NrBirds_Jan_Jul %>%
  filter(what_month == "Januari") %>% #We only take out january-birds
  arrange(desc(Freq)) %>%             #We arrange according to Freq
  select(Common_name, Freq) %>%       #Take away column what_month
  head(10)                            #Choose top 10 

NrBirds_Jul <- NrBirds_Jan_Jul %>%
  filter(what_month == "Juli") %>% #We only take out january-birds
  arrange(desc(Freq)) %>%             #We arrange according to Freq
  select(Common_name, Freq) %>%       #Take away column what_month
  head(10)                            #Choose top 10 


Result=as.data.frame(c(NrBirds_Jan, NrBirds_Jul))
colnames(Result) <- c("January Top 10 birds", "Count", "July Top 10 birds", "Count") 
Result

#===================================================================================


#=========================TASK 2====================================================

species_data_weekdays= species_data %>%
  mutate(what_day=as.factor(weekdays(Date))) #change dates to weekdays #transform it to factors

day_levels=c("Måndag", 
               "Tisdag", 
               "Onsdag", 
               "Torsdag", 
               "Fredag", 
               "Lördag", 
               "Söndag")

species_data_weekdays$what_day <- factor(species_data_weekdays$what_day, levels = day_levels)
#change levels in 'what_day'-column
levels(species_data_weekdays$what_day) #double check

ggplot(species_data_weekdays, aes(x=what_day)) +
  geom_bar() #plot the counts-birds-seen on each weekday accumulated over the whole period (jan-oct)


#=====================TASK 3================================================

lovsangare <- species_data %>%
  filter(Common_name == "lövsångare") %>%
  arrange(Date) %>%
  select(Recorded_by, Date) %>%
  head(5)


#=====================TASK 4================================================

birds <- species_data %>% 
  mutate(what_month=months(Date)) %>%
  filter(Common_name %in% c("lövsångare", "talgoxe"))

month_levels=c("Januari",    
               "Februari", 
               "Mars", 
               "April", 
               "Maj", 
               "Juni", 
               "Juli",
               "Augusti",
               "September",
               "Oktober",
               "November",
               "December")

birds$what_month <- factor(birds$what_month, levels = month_levels) 
  # <--- Change levels of months so that they appear in order

#DOUBLE CHECKING:
levels(as.factor(birds$Common_name)) #double check #==> [1] "lövsångare" "talgoxe"  
levels(as.factor(birds$what_month))  #double check #==> [1] "Januari"   "Februari" --osv-->

#PLOTTING COUNT - Talgoxe vs. Lövsångare:
ggplot(birds, aes(x=what_month, fill=Common_name)) +
  geom_bar(position="dodge") +
  ggtitle("Number of observed Lövsångare vs. Talgoxe") +
  geom_density(aes(x=what_month, y=Common_name))






# knitr::kable(df.freq, row.names = FALSE, col.names = c("Parameter", "Enpunktsskattning", "2.5%", "97.5%"),
#              caption = "Frekventistiskt")






#=================BOOLI APPARTMENTS==================================

booli <- read_csv("/Users/admin/Desktop/Statistisk Databehandling/Homework/HW_data/Booli_sold.csv")
glimpse(booli)





filter_slutpris <- booli$soldPrice
filter_bostadsyta <- booli$livingArea
plot(filter_slutpris, filter_bostadsyta)
lines(x = c(0, 6.146), y = c(0, 100), col = "blue")

ggplot() + geom_point(aes(x=filter_slutpris, y=filter_bostadsyta)) + geom_smooth(aes(x=filter_slutpris, y=filter_bostadsyta)) 

which(filter_bostadsyta > 140)
booli$livingArea[60]
