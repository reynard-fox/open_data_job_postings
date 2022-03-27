#https://stackoverflow.com/questions/49886782/remove-special-characters-from-entire-dataframe-in-r
#https://rpubs.com/tonmcg/socrata-discovery
#https://stackoverflow.com/questions/21641522/how-to-remove-specific-special-characters-in-r/21641569

# Text mining NYC jobs


# https://data.cityofnewyork.us/City-Government/NYC-Jobs/kpav-sd4t

rm(list=ls())

library(RSocrata)
library(dplyr)
library(tidytext)
library(tidyverse)
# library(jsonlite)
library(lubridate)



nyc_jobs <- "https://data.cityofnewyork.us/resource/kpav-sd4t.json?$select=*"
nyc_jobs <- read.socrata("https://data.cityofnewyork.us/api/odata/v4/kpav-sd4t",stringsAsFactors=FALSE)


# In a regular script I would also include these two commands to set my working directory. 
script_name <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(script_name))


# prep for analysis
nyc_jobs$floor.date <- floor_date(nyc_jobs$posting_date, "month")
nyc_jobs$career_level <-ifelse(is.na(nyc_jobs$career_level) | nyc_jobs$career_level == "", "Unknown",nyc_jobs$career_level)


# DATA: total job postings by month
job_postings_by_month <- nyc_jobs %>% 
  select(floor.date,number_of_positions) %>%
  group_by(floor.date) %>%
  summarise(new_postings_count = sum(n()),
            total_postings_count = sum(number_of_positions)) %>%
  mutate(new_postings_total = cumsum(new_postings_count),
          total_postings_total = cumsum(total_postings_count))


# LINE GRAPH: postings each month (distinct, cumulative)
ggplot(job_postings_by_month, aes(x = floor.date)) +
  geom_line(aes(y = new_postings_count),color = "darkred") +
  geom_line(aes(y = new_postings_total),color = "darkblue") 
  

# DATA: total job postings by agency
job_postings_by_agency <- nyc_jobs %>% 
  select(floor.date,agency) %>%
  group_by(floor.date,agency) %>%
  summarise(new_postings_count = sum(n())) %>%
  mutate(new_postings_total = cumsum(new_postings_count))


# BAR CHART: total job postings by agency
job_postings_by_agency %>% group_by(agency) %>% 
  summarise(new_postings_count = sum(new_postings_count)) %>% arrange(desc(new_postings_count)) %>%
  ggplot(aes(x=reorder(agency,new_postings_count),y=new_postings_count, fill=agency)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=agency), angle = 90,vjust=.5,hjust=0,size=2) +
  guides(colour = guide_colourbar(order = 1))+
  theme(legend.position = "none",
        axis.text.x=element_blank()) +
  labs(title = "Total Job Postings by Agency", x = "Total Job Postings", y = "Agency") 


# DATA: total job postings by career level & agency
job_postings_by_exp_level_agency <- nyc_jobs %>% 
  select(agency,career_level) %>%
  group_by(agency,career_level) %>%
  summarise(total_postings = n(), .groups ="keep") %>%
  mutate(career_level = ifelse(career_level == "" | is.na(career_level), "Unknown", career_level)) %>% ungroup() %>%
  pivot_wider(names_from = career_level, values_from = total_postings, values_fill = 0) %>%
  rowwise() %>% mutate(Total = rowSums(across(where(is.numeric))))



# I removed keyword "R"
system <- c("R",	"R",	"R",	"R",	"R",	"R",	"R",	"R",	"R",	"R",	"R",	"R")
key_word <- c("RStudio",	"Rstudio",	"R,",	" R,",	" R ",	"DBI",	"tidyverse",	"dplyr",	"tidyr",	"ggplot2",	"ggplot",	"ggmap")
system_words <- as.data.frame(cbind(system,key_word))

system <- c("Python",	"Python",	"Python",	"Python",	"Python",	"Python",	"Python",	"Python",	"Python",	"Python",	"Python")
key_word <- c("Python",	"Anaconda",	"Pycharm",	"PyCharm",	"Pandas",	"SciPy",	"Numpy",	"Matplotlib",	"Seaborn",	"Scikit-Learn",	"TensorFlow")
temp <- as.data.frame(cbind(system,key_word))
system_words <- rbind(system_words,temp)

system <- c("SQL",	"SQL",	"SQL",	"SQL",	"SQL",	"SQL",	"SQL",	"SQL",	"SQL",	"SQL",	"SQL",	"SQL",	"SQL",	"SQL",	"SQL")
key_word <- c("SQL",	"T-SQL",	"Db2",	"Oracle",	"IBM Db2",	"SQLite",	"MySQL",	"Microsoft SQL Server",	"SQL Server",	"SSMS",	"PostgreSQL",	"pgAdmin",	"sql server management studio",	"SQL server management studio",	"SQL Server Management Studio")
temp <- as.data.frame(cbind(system,key_word))
system_words <- rbind(system_words,temp)

system <- c("Excel",	"Excel",	"Excel",	"Excel",	"Excel",	"Excel",	"Excel")
key_word <- c("Excel",	"MS Excel",	"M.S. Excel",	"Microsoft Excel",	"Piivot Tables",	"PivotTables",	"VBA")
temp <- as.data.frame(cbind(system,key_word))
system_words <- rbind(system_words,temp)

system <- c("Access",	"Access",	"Access",	"Access",	"Access")
key_word <- c("MS Access",	"M.S. Access",	"Access Forms",	"Access",	"Microsoft Access")
temp <- as.data.frame(cbind(system,key_word))
system_words <- rbind(system_words,temp)

# I removed keyword "MS"
system <- c("Microsoft",	"Microsoft",	"Microsoft")
key_word <- c("Microsoft",	"MSFT",	"Msft")
temp <- as.data.frame(cbind(system,key_word))
system_words <- rbind(system_words,temp)
rm(temp)

# ACCESS & EXCEL MUST have a match on Microsoft TOO to count

microsoft <- system_words %>% filter(system=="Microsoft") %>% select(key_word) %>% unlist()
ms_access <- system_words %>% filter(system=="Access") %>% select(key_word) %>% unlist()
ms_excel <- system_words %>% filter(system=="Excel") %>% select(key_word) %>% unlist()
sql <- system_words %>% filter(system=="SQL") %>% select(key_word) %>% unlist()
python <- system_words %>% filter(system=="Python") %>% select(key_word) %>% unlist()
rstudio <- system_words %>% filter(system=="R") %>% select(key_word) %>% unlist()


t_microsoft <- paste0(unlist(microsoft),collapse = "|")
t_ms_access <- paste0(unlist(ms_access),collapse = "|")
t_ms_excel <- paste0(unlist(ms_excel),collapse = "|")
t_sql <- paste0(unlist(sql),collapse = "|")
t_python <- paste0(unlist(python),collapse = "|")
t_rstudio <- paste0(unlist(rstudio),collapse = "|")


nyc_jobs <- nyc_jobs %>% 
  mutate(microsoft = if_else(str_detect(preferred_skills,t_microsoft),1,0,missing=0),
         ms_access = if_else(str_detect(preferred_skills,t_ms_access) & str_detect(preferred_skills,t_microsoft),1,0,missing=0),
         ms_excel  = if_else(str_detect(preferred_skills,t_ms_excel) & str_detect(preferred_skills,t_microsoft),1,0,missing=0),
         sql = if_else(str_detect(preferred_skills,t_sql),1,0,missing=0),
         python = if_else(str_detect(preferred_skills,t_python),1,0,missing=0),
         rstudio = if_else(str_detect(preferred_skills,t_rstudio),1,0,missing=0)) 


nyc_jobs %>% summarize(microsoft = sum(microsoft),
                       ms_access = sum(ms_access),
                       ms_excel = sum(ms_excel),
                       sql = sum(sql),
                       python = sum(python),
                       rstudio = sum(rstudio))

# ,
# DATA: total job postings by career level & preferred skill
nyc_jobs %>% 
  select(career_level,microsoft,ms_access,ms_excel,sql,python,rstudio) %>%
  group_by(career_level) %>%
  summarise(total_postings = n(),
            microsoft = sum(microsoft),
            ms_access = sum(ms_access),
            ms_excel = sum(ms_excel),
            sql = sum(sql),
            python = sum(python),
            rstudio = sum(rstudio)) -> skill_career_level

# DATA: total job postings by agency & preferred skill     
nyc_jobs %>% 
  select(agency,microsoft,ms_access,ms_excel,sql,python,rstudio) %>%
  group_by(agency) %>%
  summarise(total_postings = n(),
            microsoft = sum(microsoft),
            ms_access = sum(ms_access),
            ms_excel = sum(ms_excel),
            sql = sum(sql),
            python = sum(python),
            rstudio = sum(rstudio)) -> skill_agency

# STOP
###############################################################
# preferred skill analysis - how does it vary by identified key words? 
# https://towardsdatascience.com/python-vs-r-what-i-learned-from-4-000-job-advertisements-ab41661b7f28


# there are encoding issues to remove
# nyc_jobs$preferred_skills[3] 
# iconv(nyc_jobs$preferred_skills[3], "UTF-8", "ASCII", sub="")
# str_squish(str_trim(iconv(test, "UTF-8", "ASCII", sub="")))

# options include: latin1, ASCII, UTF-8,'ASCII//TRANSLIT'
# str_squish(str_trim(iconv(test$preferred_skills[3], "UTF-8", "ASCII", sub="")))

test[] <- lapply(test, iconv, "UTF-8", "ASCII", sub="")
test[] <- lapply(test, str_trim)
test[] <- lapply(test, str_squish)


to_review <- nyc_jobs %>% 
  select(job_description,minimum_qual_requirements,preferred_skills) %>% 
  as.data.frame()


job_preferred_skills <- test %>% select(preferred_skills) %>% 
  filter(!map_lgl(preferred_skills, is.null)) %>%
  filter(!map_lgl(preferred_skills, is.na)) %>% 
  filter(preferred_skills != "") %>% unnest(preferred_skills)

job_preferred_skills %>%
  mutate_all(funs(gsub("[^[:alnum:] ]", "", .))) -> test

test <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(job_preferred_skills))) 





job_preferred_skills_tokens <- test %>%
  tidytext::unnest_tokens(word, preferred_skills,token = "ngrams", n = 2) %>%
  dplyr::anti_join(stop_words, by = "word")

job_preferred_skills_tokens %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)



unnest_tokens(as.data.frame(test[1,]), token = "words", format = c("text"))


tidytext::unnest_tokens(as.data.frame(test[1,]),word, title) 

%>% # filter out null values
 



%>% as.data.frame() %>% unnest_tokens(word,text)

as.character() %>% %>% unlist()

nyc_jobs %>% select(job_description) %>% as.character() %>% unnest_tokens(word,text)


original_books <- nyc_jobs %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()





