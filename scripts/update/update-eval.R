# Script used to update the database with evaluation results from the 5th cohort
library(dplyr)
library(RSQLite)
source(here::here("scripts/helpers.R"))

cohort.num <- 4L
dir <- sprintf("downloads/eval/%d/results", cohort.num)

finames <- list.files(dir)

dt <- purrr::map_df(paste(dir, finames, sep = '/'), function(x) {
  df <- read.csv(x)
  df$filename <- basename(x)
  df
})

dt$X <- NULL

# Modules data from the DB
mods <- read_cohort_dbtable("modules")

nd <- dt %>% 
  rename(time = Timestamp) %>% 
  rename(challenging = Was.the.module.content.too.challenging.for.you.to.understand.) %>% 
  rename(too_distant = Did.the.ideas.expressed.in.delivering.the.module.seem.too.distant.from.your.experience.) %>% 
  rename(rating_dur = How.would.you.rate.the.duration.of.the.class.) %>% 
  rename(recomm_dur = How.long.would.you.recommend.that.class.session.lasts.) %>% 
  rename(recomm_chng = What.would.you.want.changed.in.the.module.structure..content.and.design.) %>% 
  rename(fac_comm = Overall..how.would.you.rate.the.communication.skills.of.the.facilitator.) %>% 
  rename(fac_expert = How.would.you.rate.your.facilitator.s.expertise.) %>% 
  rename(fac_comfort = Did.you.feel.comfortable.expressing.yourself.to.your.facilitator.) %>% 
  rename(fulfil_expect = Were.your.training.expectations.fulfilled.) %>% 
  rename(fulfil_explain = If.yes..explain.how.your.training.expectations.were.fulfilled) %>% 
  rename(comment = General.Comments.on.the.Module.) %>% 
  rename(module_code = filename) %>% 
  mutate(module_code = sub("\\.csv$", "", module_code)) %>% 
  right_join(mods, by = "module_code") %>% 
  relocate(id) %>% 
  arrange(id) %>% 
  rename(module_id = id) %>% 
  mutate(cohort_id = cohort.num)

dbversion <- nd %>% 
  select(-c(module_name, module_code, 14:15))

## `dbversion` is then appended to the existing 'evaluation' table in the DB.
# tryCatch({
#   cat("Writing to the database ('evaluation' table) ... ")
#   con <- dbConnect(SQLite(), "data/cohort3.db")
#   if (dbIsValid(con)) {
#     # Write query to count the records before and after the write
#     dbWriteTable(con, "evaluation", dbversion, append = TRUE)
#     dbDisconnect(con)
#   }
#   cat("Done\n")
# }, error = function(e) {
#   cat("Failed\n")
#   warning("Database error: ", conditionMessage(e), call. = FALSE)
#   })
