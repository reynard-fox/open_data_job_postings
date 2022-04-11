# Status: Working Draft

#------------------[ REFERENCE MATERIAL]------------------#

# > MAIN References
# https://rpubs.com/tonmcg/socrata-discovery
# https://www.tidytextmining.com/index.html
# https://epirhandbook.com/en/index.html
# https://paldhous.github.io/NICAR/2019/r-text-analysis.html
# http://rstudio-pubs-static.s3.amazonaws.com/256588_57b585da6c054349825cba46685d8464.html
# https://cbail.github.io/SICSS_Dictionary-Based_Text_Analysis.html
# https://bookdown.org/rdpeng/rprogdatascience/
# https://r4ds.had.co.nz/index.html
# https://towardsdatascience.com/python-vs-r-what-i-learned-from-4-000-job-advertisements-ab41661b7f28
# https://r-charts.com/
# https://ggplot2.tidyverse.org/index.html
# http://adv-r.had.co.nz/

# > SECONDARY References
# https://statsandr.com/blog/how-to-upload-r-code-on-github-example-with-an-r-script-on-mac-os/
# https://bookdown.org/yihui/rmarkdown/

# https://stackoverflow.com/questions/21641522/how-to-remove-specific-special-characters-in-r/21641569
# https://stackoverflow.com/questions/49886782/remove-special-characters-from-entire-dataframe-in-r

# https://stackoverflow.com/questions/56955320/file-contain-u00c2-u00a0-convert-to-characters
# https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
# https://stackoverflow.com/questions/60259657/how-to-remove-non-utf-8-characters-from-text

# https://statsandr.com/blog/how-to-upload-r-code-on-github-example-with-an-r-script-on-mac-os/
# https://stackoverflow.com/questions/43456687/using-dplyr-gsub-on-many-columns

# https://r-lang.com/not-in-r/

# Text mining NYC jobs
# https://data.cityofnewyork.us/City-Government/NYC-Jobs/kpav-sd4t

##------------------------------------------------------
## Set up
##------------------------------------------------------


# Clean up
rm(list=ls())


# Set  up
`%!in%` <- Negate(`%in%`)

library(RSocrata)
library(dplyr)
library(tidytext)
library(tidyverse)
# library(jsonlite)
library(lubridate)


# Load data
nyc_jobs <- "https://data.cityofnewyork.us/resource/kpav-sd4t.json?$select=*"
nyc_jobs <- read.socrata("https://data.cityofnewyork.us/api/odata/v4/kpav-sd4t",stringsAsFactors=FALSE)


# Set working GitHub directory
script_name <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(script_name))


# Save data & script in other file. The data is for future reference, the R script is for project history
store <- '~/Desktop/github/project 04 nyc jobs/'
filename <- paste0(store,'nyc_jobs_',as.character(Sys.time(), format="%Y-%m-%d"),'.csv')
write.csv(nyc_jobs,filename)
file.copy(script_name, store)


# Prep data for analysis
nyc_jobs$floor.month <- floor_date(nyc_jobs$posting_date, "month")
nyc_jobs$career_level <-ifelse(is.na(nyc_jobs$career_level) | nyc_jobs$career_level == "", "Unknown",nyc_jobs$career_level)

##------------------------------------------------------
## Job postings by date, agency, and career level
##------------------------------------------------------


# DATA: total job postings by month
job_postings_by_month <- nyc_jobs %>% 
  select(floor.month,number_of_positions) %>%
  group_by(floor.month) %>%
  summarise(new_postings_count = sum(n()),
            total_postings_count = sum(number_of_positions)) %>%
  mutate(new_postings_total = cumsum(new_postings_count),
         total_postings_total = cumsum(total_postings_count),
         floor.month = as.Date(floor.month))


# LINE GRAPH: postings each month (distinct, cumulative)
ggplot(job_postings_by_month, aes(x = floor.month)) +
  geom_line(aes(y = new_postings_count),color = "darkred") +
  geom_line(aes(y = new_postings_total),color = "darkblue") +
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_date(breaks = "6 month", minor_breaks = "3 month",date_labels = "%m-%Y") +
  labs(title = "Total Job Postings each Month Agency", x = "Month", y = "Agency") 


# DATA: total job postings by agency
job_postings_by_agency <- nyc_jobs %>% 
  select(floor.month,agency) %>%
  group_by(floor.month,agency) %>%
  summarise(new_postings_count = sum(n())) %>%
  mutate(new_postings_total = cumsum(new_postings_count))


# BAR CHART: total job postings by agency
job_postings_by_agency %>% group_by(agency) %>% 
  summarise(new_postings_count = sum(new_postings_count)) %>% arrange(desc(new_postings_count)) %>%
  ggplot(aes(x=reorder(agency,new_postings_count),y=new_postings_count, fill=agency)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=agency), angle = 90,vjust=.5,hjust=0,size=2) +
  guides(colour = guide_colourbar(order = 1)) +
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


##------------------------------------------------------
## Preferred skill KEY WORDS
##------------------------------------------------------
# prep list for matching
# >> ACCESS & EXCEL MUST have a match on Microsoft to count
# >> Need to add other tech skills - programming, azure/dev ops, aws, cloud
# >> R key words - I removed keyword "R" - too many false hits
# >> Microsoft key words - I removed keyword "MS"


# Microsoft
microsoft <- c("Excel","MS Excel","M.S. Excel","Microsoft Excel","Piivot Tables","PivotTables","VBA")
t_microsoft <- paste0(unlist(microsoft),collapse = "|")


# Access
ms_access <- c("MS Access","M.S. Access","Access Forms","Access","Microsoft Access")
t_ms_access <- paste0(unlist(ms_access),collapse = "|")


# Excel
ms_excel <- c("Excel","MS Excel","M.S. Excel","Microsoft Excel","Piivot Tables","PivotTables","VBA")
t_ms_excel <- paste0(unlist(ms_excel),collapse = "|")


# SQL
sql <- c("SQL","T-SQL","Db2","Oracle","IBM Db2","SQLite","MySQL","Microsoft SQL Server","SQL Server","SSMS","PostgreSQL",
         "pgAdmin","sql server management studio","SQL server management studio","SQL Server Management Studio")
t_sql <- paste0(unlist(sql),collapse = "|")


# Python
python <- c("Python","Anaconda","Pycharm","PyCharm","Pandas","SciPy","Numpy","Matplotlib","Seaborn","Scikit-Learn","TensorFlow")
t_python <- paste0(unlist(python),collapse = "|")


# R
rstudio <- c("RStudio","Rstudio","R,"," R,"," R ","DBI","tidyverse","dplyr","tidyr","ggplot2","ggplot","ggmap")
t_rstudio <- paste0(unlist(rstudio),collapse = "|")


# Match
nyc_jobs <- nyc_jobs %>% 
  mutate(microsoft = if_else(str_detect(preferred_skills,t_microsoft),1,0,missing=0),
         ms_access = if_else(str_detect(preferred_skills,t_ms_access) & str_detect(preferred_skills,t_microsoft),1,0,missing=0),
         ms_excel  = if_else(str_detect(preferred_skills,t_ms_excel) & str_detect(preferred_skills,t_microsoft),1,0,missing=0),
         sql = if_else(str_detect(preferred_skills,t_sql),1,0,missing=0),
         python = if_else(str_detect(preferred_skills,t_python),1,0,missing=0),
         rstudio = if_else(str_detect(preferred_skills,t_rstudio),1,0,missing=0)) 

# sumamrize findings
nyc_jobs %>% summarize(microsoft = sum(microsoft),
                       ms_access = sum(ms_access),
                       ms_excel = sum(ms_excel),
                       sql = sum(sql),
                       python = sum(python),
                       rstudio = sum(rstudio))


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


##------------------------------------------------------
## Job postings by career level - SQL, R, PYTHON
##------------------------------------------------------

# NEXT - preferred skill analysis - how does it vary by identified key words? 
# https://towardsdatascience.com/python-vs-r-what-i-learned-from-4-000-job-advertisements-ab41661b7f28

# there are encoding issues to remove - these didn't work
# options include: latin1, ASCII, UTF-8,'ASCII//TRANSLIT'


#sql, python, rstudio = SPR
job <- nyc_jobs %>% filter(sql == 1 | python == 1 | rstudio == 1)

# there are encoding issues to remove
job[] <- lapply(job, iconv, "UTF-8", "ASCII", sub="")
job[] <- lapply(job, str_trim)
job[] <- lapply(job, str_squish)

# select just job description stuff
job <- job %>% 
  select(job_description,minimum_qual_requirements,preferred_skills) %>% 
  as.data.frame()

# clean the data - remove blanks/null/missing and UNNEST
job_preferred_skills <- job %>% select(preferred_skills) %>% 
  filter(!map_lgl(preferred_skills, is.null)) %>%
  filter(!map_lgl(preferred_skills, is.na)) %>% 
  filter(preferred_skills != "") %>% unnest(preferred_skills)

# remove non-alphanumeric values
job_preferred_skills <- job_preferred_skills %>%
  mutate(across(everything(),~ gsub("[^[:alnum:] ]", "", .))) %>%
  mutate(across(everything(),~ gsub("[[:punct:]]", "", .)))

# create TWO word pairs/ngrams
job_preferred_skills_tokens <- job_preferred_skills %>%
  tidytext::unnest_tokens(word, preferred_skills,token = "ngrams", n = 2) 

# separate these into pairs
job_preferred_skills_tokens <- job_preferred_skills_tokens %>%  
  separate(word, into = c("word1", "word2"), sep = " ")

# remove ngrams with stop words in either pair
job_preferred_skills_tokens <- job_preferred_skills_tokens %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(word, c(word1, word2), sep = " ")

# re-create ngram pairs
graph_min <- job_preferred_skills_tokens %>% 
  count(word, sort = TRUE) %>% 
  head(15) %>% select(n) %>% min()

# graph
SPR <- job_preferred_skills_tokens
SPR %>%
  count(word, sort = TRUE) %>%
  filter(n > graph_min) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(title = "Python, R, & SQL Ngrams", x = "Term Freq", y = "Term")


##------------------------------------------------------
## Job postings by career level - EXCEL & ACCESS - EA
##------------------------------------------------------

# NEXT - preferred skill analysis - how does it vary by identified key words? 
# https://towardsdatascience.com/python-vs-r-what-i-learned-from-4-000-job-advertisements-ab41661b7f28

# there are encoding issues to remove - these didn't work
# options include: latin1, ASCII, UTF-8,'ASCII//TRANSLIT'


# Excel & Access = EA
job <- nyc_jobs %>% filter(ms_excel == 1 | ms_access == 1)

# there are encoding issues to remove
job[] <- lapply(job, iconv, "UTF-8", "ASCII", sub="")
job[] <- lapply(job, str_trim)
job[] <- lapply(job, str_squish)

# select just job description stuff
job <- job %>% 
  select(job_description,minimum_qual_requirements,preferred_skills) %>% 
  as.data.frame()

# clean the data - remove blanks/null/missing and UNNEST
job_preferred_skills <- job %>% select(preferred_skills) %>% 
  filter(!map_lgl(preferred_skills, is.null)) %>%
  filter(!map_lgl(preferred_skills, is.na)) %>% 
  filter(preferred_skills != "") %>% unnest(preferred_skills)

# remove non-alphanumeric values
job_preferred_skills <- job_preferred_skills %>%
  mutate(across(everything(),~ gsub("[^[:alnum:] ]", "", .))) %>%
  mutate(across(everything(),~ gsub("[[:punct:]]", "", .)))

# create TWO word pairs/ngrams
job_preferred_skills_tokens <- job_preferred_skills %>%
  tidytext::unnest_tokens(word, preferred_skills,token = "ngrams", n = 2) 

# seperate these into pairs
job_preferred_skills_tokens <- job_preferred_skills_tokens %>%  
  separate(word, into = c("word1", "word2"), sep = " ")

# remove ngrams with stop words in either pair
job_preferred_skills_tokens <- job_preferred_skills_tokens %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(word, c(word1, word2), sep = " ")

# re-create ngram pairs
graph_min <- job_preferred_skills_tokens %>% 
  count(word, sort = TRUE) %>% 
  head(13) %>% select(n) %>% min()

# graph
EA <- job_preferred_skills_tokens
EA %>%
  count(word, sort = TRUE) %>%
  filter(n > graph_min) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(title = "Excel & Accces Ngrams", x = "Term Freq", y = "Term")



##------------------------------------------------------
## Job postings by career level - NOT SQL, R, PYTHON
##------------------------------------------------------

# NEXT - preferred skill analysis - how does it vary by identified key words? 
# https://towardsdatascience.com/python-vs-r-what-i-learned-from-4-000-job-advertisements-ab41661b7f28

# there are encoding issues to remove - these didn't work
# options include: latin1, ASCII, UTF-8,'ASCII//TRANSLIT'


#sql, python, rstudio = NOT_SPR
job <- nyc_jobs %>% filter(sql != 1 | python != 1 | rstudio != 1)

# there are encoding issues to remove
job[] <- lapply(job, iconv, "UTF-8", "ASCII", sub="")
job[] <- lapply(job, str_trim)
job[] <- lapply(job, str_squish)

# select just job description stuff
job <- job %>% 
  select(job_description,minimum_qual_requirements,preferred_skills) %>% 
  as.data.frame()

# clean the data - remove blanks/null/missing and UNNEST
job_preferred_skills <- job %>% select(preferred_skills) %>% 
  filter(!map_lgl(preferred_skills, is.null)) %>%
  filter(!map_lgl(preferred_skills, is.na)) %>% 
  filter(preferred_skills != "") %>% unnest(preferred_skills)

# remove non-alphanumeric values
job_preferred_skills <- job_preferred_skills %>%
  mutate(across(everything(),~ gsub("[^[:alnum:] ]", "", .))) %>%
  mutate(across(everything(),~ gsub("[[:punct:]]", "", .)))

# create TWO word pairs/ngrams
job_preferred_skills_tokens <- job_preferred_skills %>%
  tidytext::unnest_tokens(word, preferred_skills,token = "ngrams", n = 2) 

# separate these into pairs
job_preferred_skills_tokens <- job_preferred_skills_tokens %>%  
  separate(word, into = c("word1", "word2"), sep = " ")

# remove ngrams with stop words in either pair
job_preferred_skills_tokens <- job_preferred_skills_tokens %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(word, c(word1, word2), sep = " ")

# re-create ngram pairs
graph_min <- job_preferred_skills_tokens %>% 
  count(word, sort = TRUE) %>% 
  head(15) %>% select(n) %>% min()

# graph
NOT_SPR <- job_preferred_skills_tokens
NOT_SPR %>%
  count(word, sort = TRUE) %>%
  filter(n > graph_min) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(title = "Job Postings WITHOUT Python, R, & SQL Ngrams", x = "Term Freq", y = "Term")


###############################
# STOP

# things to add
# > simplify & cleanup - lot of repetition; see what steps to turn into functions
# > positive/negative sentiment
# > average wage by skill set - excel/access, python/r/sql, all others
# > info on experience level - does sentiment vary 
# > what "machine learning" methods can i use to tease out info - add other vars from posting to ngrams?
# > add in open data sets for each agency as proxy for data sophistication - maps, large datasets, frequency of update all correlated with greater data needs?
# > topic modeling
#     > https://slcladal.github.io/topicmodels.html
# > Write-up in Rmarkdown
# https://www.tidytextmining.com/ngrams.html#counting-and-filtering-n-grams

# model each agency as a book/corpus
# select(agency,job_description,minimum_qual_requirements,preferred_skills) 




job <- nyc_jobs

# there are encoding issues to remove
job[] <- lapply(job, iconv, "UTF-8", "ASCII", sub="")
job[] <- lapply(job, str_trim)
job[] <- lapply(job, str_squish)

# select just job description stuff
job <- job %>% 
  select(job_description,minimum_qual_requirements,preferred_skills) %>% 
  as.data.frame()

# clean the data - remove blanks/null/missing and UNNEST
job_preferred_skills <- job %>% select(preferred_skills) %>% 
  filter(!map_lgl(preferred_skills, is.null)) %>%
  filter(!map_lgl(preferred_skills, is.na)) %>% 
  filter(preferred_skills != "") %>% unnest(preferred_skills)

# remove non-alphanumeric values
job_preferred_skills <- job_preferred_skills %>%  # The ideal candidate shall have 7-10 years of experience
  mutate(across(everything(),~ gsub("[^[:alnum:] ]", " ", .))) %>% # The ideal candidate shall have 710 years of experience
  mutate(across(everything(),~ gsub("[[:punct:]]", " ", .))) %>% # The ideal candidate shall have 710 years of experience
  mutate(across(everything(),~ gsub("[[:digit:]]+", " ", .))) # The ideal candidate shall have  years of experience
  

# create tokens
job_preferred_skills_tokens <- job_preferred_skills %>%
  tidytext::unnest_tokens(word, preferred_skills) %>%
  count(word, sort = TRUE) 
  

total_job_preferred_skills_tokens <- job_preferred_skills_tokens %>% 
  group_by(word) %>% 
  summarize(total = sum(n))

library(ggplot2)

ggplot(job_preferred_skills_tokens, aes(n/sum(n),fill=n)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) 