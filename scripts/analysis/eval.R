# ---- ext-setup ----
knitr::opts_chunk$set(echo = FALSE,
                      message = FALSE,
                      warning = FALSE)

library(dplyr)
library(flextable)
library(ggplot2)
library(xtable)
# library(lubridate)

source(here::here("scripts/helpers.R"))

convert_table <- function(tbl) {
  tbl %>%
    xtable %>%
    xtable_to_flextable() %>%
    set_header_labels(" " = "Module")
}



plotAllModules <- function(var, ...) {
  qvar <- enquo(var)
  ggplot(evaldata, aes({
    {
      qvar
    }
  })) +
    geom_bar(...)
}



plotModuleComparison <- function(var) {
  var.y <- enexpr(var)
  ggplot(evaldata, aes({
    {
      var.y
    }
  })) +
    geom_bar() +
    facet_wrap(vars(module_id))
}



## DATA
## Cleaning
evalraw <- read_cohort_dbtable("evaluation")

evaldata <- evalraw %>%
  as_tibble() %>%
  filter(cohort_id == params$cohort) %>%
  mutate(time = lubridate::parse_date_time(
    time,
    orders = c("%m/%d/%Y %H:%M", "%Y-%m-%d %H:%M:%S"),
    tz = "Africa/Lagos"    # i.e. Sys.timezone()
  )) %>%
  mutate(module_id = factor(module_id, labels = paste("Module", levels(module_id)))) %>%
  mutate(rating_dur = factor(
    rating_dur,
    levels = c("Too short", "Just rignt", "Too long"),
    ordered = TRUE
  )) %>%
  mutate(fac_comm  = factor(
    fac_comm,
    levels = c("Low", "Average", "High"),
    ordered = TRUE
  )) %>%
  mutate(across(contains(
    c('challenging', 'too_distant', "fac_comfort", 'fulfil_expect')
  ), ~ factor(.x, levels = c("Yes", "No"))))

## Apply labels to the variable
questions <- read_cohort_dbtable("eval_ques")
labelled::var_label(evaldata) <-
  c("Module No.", "Timestamp", questions$question_short)
rm(questions)

# attendance <- read_cohort_dbtable("attendance")
# attendata <- attendance %>%
#   filter(cId == params$cohort) %>%
#   group_by(mId) %>%
#   summarise(n = sum(attended, na.rm = TRUE)) %>%
#   select(-1)


# ---- respondents ----
# respondents <- evaldata$module_id %>%
#   table() %>%
#   as.data.frame() %>%
#   bind_cols(attendata) %>%
#   setNames(c("module", "number", "attended")) %>%
#   mutate(percent = round(number / attended * 100, 1))

# ---- respondents-table ----
# flextable(respondents) %>%
#   set_header_labels(
#     values = list(
#       module = "Module",
#       number = "No. of Respondents",
#       attended = "Attendance",
#       percent = "Response Rate (%)"
#     )
#   )

# ---- respondents-chart ----
# barplot(
#   with(respondents, structure(percent, names = levels(module))),
#   ylim = c(0, 40),
#   main = "Evaluation respondents",
#   xlab = "Module Number",
#   ylab = "Percentage of attendees",
#   col = 'cyan'
# )
# grid(NA, NULL, "blue", "dashed", lwd = 1.5)

## Compare evaluation with attendance
# ---- attendance-respondents ----
# att <- query_data("SELECT mid AS moduleId FROM attendance WHERE attended=1;")
# vec.mod <- if (params$cohort == 3) {
#   att$moduleId[att$moduleId != 1]    # Module 1 not evaluated
# } else {
#   att$moduleId
# }
#
# Attendance <- vec.mod |>
#   table() |>
#   as.numeric()
# Evaluation <- evaldata$module_id |>
#   table() |>
#   as.numeric()
#
# plot(Attendance,
#      Evaluation,
#      pch = 17,
#      main = "Relationship between Attendance\nand Response to Evaluation")



# ---- challenging ----
chall <- table(evaldata$challenging)
plotAllModules(challenging)

# ---- module-challenging ----
## Module vs Challenging
with(evaldata, table(module_id, challenging)) |> convert_table()
plotModuleComparison(challenging)

# ---- too-distant ----
table(evaldata$too_distant) |> convert_table()
plotAllModules(too_distant)


# ---- module-too-distant ----
## Module vs Too Distant
with(evaldata, table(module_id, too_distant)) |> convert_table()
plotModuleComparison(too_distant)

# ---- duration ----
table(evaldata$rating_dur) |> convert_table()
plotAllModules(rating_dur)

# ---- module-duration ----
## Module vs Duration
with(evaldata, table(module_id, rating_dur)) |> convert_table()
plotModuleComparison(rating_dur)


# ---- recommend-dur ----
result <- summary(evaldata$recomm_dur)
mu <- mean(evaldata$recomm_dur, na.rm = TRUE)
hist(evaldata$recomm_dur,
     main = "Recommended Module Duration (hrs)",
     xlab = "No. of hours")
abline(v = mu, lwd = 2, col = 'red')
text(mu + 1, 15, paste("Average =", round(mu, 1), "hrs"), col = "red")

# ---- fac-comm ----
table(evaldata$fac_comm) |> convert_table()
plotAllModules(fac_comm)


# ---- module-fac-comm ----
## Module vs Facilitators' Communication Skills
with(evaldata, table(module_id, fac_comm)) |> convert_table()
plotModuleComparison(fac_comm)

# ---- fac-comfort ----
table(evaldata$fac_comfort) |> convert_table()
plotAllModules(fac_comfort)


# ---- module-fac-comfort ----
## Module vs Level of Comfortability with Facilitator
with(evaldata, table(module_id, fac_comfort)) |> convert_table()
plotModuleComparison(fac_comfort)


# ---- expectations ----
table(evaldata$fulfil_expect) |> convert_table()
plotAllModules(fulfil_expect)

# ---- module-expectations ----
## Module vs Expectations Fulfilled
with(evaldata, table(module_id, fulfil_expect)) |> convert_table()
plotModuleComparison(fulfil_expect)
