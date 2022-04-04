# ---- setup ----
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
library(dplyr)
library(flextable)
library(ggplot2)
library(xtable)

source(here::here("scripts/helpers.R"))

evaldata <- read_cohort_dbtable("evaluation")
questions <- read_cohort_dbtable("eval_ques")
attendance <- read_cohort_dbtable("attendance")

## Cleaning
evaldata <- evaldata %>% 
  mutate(time = as.POSIXct(time)) %>% 
  mutate(across(contains(
    c("module_id", "rating_dur", "fac_comm")
  ), factor)) %>%  
  mutate(across(contains(
    c('challenging', 'too_distant', "fac_comfort", 'fulfil_expect')
  ), ~ factor(.x, levels = c("Yes", "No")))) 

attended <- attendance %>% 
  filter(attended == 1) %>% 
  group_by(mId) %>% 
  summarise(n = sum(attended)) %>% 
  filter(mId != 1) %>% 
  select(-1)


## Apply labels to the variable
lbls <- c("Module No.", "Timestamp", questions$question_full)
evaldata <- labelled::set_variable_labels(evaldata, .labels = lbls)
rm(questions)


convert_table <- function(tbl) {
  tbl %>% 
    xtable %>% 
    xtable_to_flextable() %>% 
    set_header_labels(" " = "Module")
}


plotModuleComparison <- function(var) {
  var.y <- enexpr(var)
  ggplot(evaldata, aes(module_id, fill = !!var.y)) +
    geom_bar(position = 'dodge')
}

# ---- respondents ----
respondents <- table(evaldata$module_id) %>% 
  as.data.frame() %>% 
  bind_cols(attended) %>% 
  setNames(c("module", "number", "attended")) %>% 
  mutate(percent = round((number / attended) * 100, 1))

# ---- respondents-table ----
flextable(respondents) %>% 
  set_header_labels(
    values = list(
      module = "Module",
      number = "No. of Respondents",
      attended = "Attendance",
      percent = "Response Rate (%)"
    )
  )

# ---- respondents-chart ----
barplot(
  with(respondents, structure(percent, names = levels(module))),
  ylim = c(0, 40),
  main = "Evaluation respondents",
  xlab = "Module Number",
  ylab = "Percentage of attendees",
  col = 'cyan'
)
grid(NA, NULL, "blue", "dashed", lwd = 1.5)

## Compare evaluation with attendance
# ---- attendance-respondents ----
att <- query_cohort("SELECT mid AS moduleId FROM attendance WHERE attended=1;")
Attendance <- table(att$moduleId[att$moduleId != 1]) |> # Module 1 not evaluated
  as.numeric()
Evaluation <- as.numeric(table(evaldata$module_id))

plot(Attendance, 
     Evaluation, 
     pch = 17, 
     main = "Relationship between Attendance\nand Response to Evaluation")



# ---- challenging ----
chall <- table(evaldata$challenging)
ggplot(evaldata, aes(challenging)) + 
  geom_bar() 

# ---- module-challenging ----
## Module vs Challenging
with(evaldata, table(module_id, challenging)) |> convert_table()

plotModuleComparison(challenging)

# ---- too-distant ----
table(evaldata$too_distant) |> convert_table()
ggplot(evaldata, aes(too_distant)) + 
  geom_bar()


# ---- module-too-distant ----
## Module vs Too Distant
with(evaldata, table(module_id, too_distant)) |> convert_table()
plotModuleComparison(too_distant)

# ---- duration ----
table(evaldata$rating_dur) |> convert_table()
ggplot(evaldata, aes(rating_dur)) + 
  geom_bar()


# ---- module-duration ----
## Module vs Duration
with(evaldata, table(module_id, rating_dur)) |> convert_table()
plotModuleComparison(rating_dur)


# ---- recommend-dur ----
result <- summary(evaldata$recomm_dur)
mu <- mean(evaldata$recomm_dur, na.rm = TRUE)
hist(evaldata$recomm_dur, 
     main = "Recommended Module Duration (hrs)", 
     xlab = "Response")
abline(v = mu, lwd = 2, col = 'red')
text(mu + 1, 15, paste("Average =", round(mu, 1), "hrs"), col = "red")

# ---- fac-comm ----
table(evaldata$fac_comm) |> convert_table()
ggplot(evaldata, aes(fac_comm)) + 
  geom_bar()


# ---- module-fac-comm ----
## Module vs Facilitators' Communication Skills
with(evaldata, table(module_id, fac_comm)) |> convert_table()
plotModuleComparison(fac_comm)

# ---- fac-comfort ----
table(evaldata$fac_comfort) |> convert_table()
ggplot(evaldata, aes(fac_comfort)) + 
  geom_bar()


# ---- module-fac-comfort ----
## Module vs Level of Comfortability with Facilitator
with(evaldata, table(module_id, fac_comfort)) |> convert_table()
plotModuleComparison(fac_comfort)


# ---- expectations ----
table(evaldata$fulfil_expect) |> convert_table()
ggplot(evaldata, aes(fulfil_expect)) + 
  geom_bar()

# ---- module-expectations ----
## Module vs Expectations Fulfilled
with(evaldata, table(module_id, fulfil_expect)) |> convert_table()
plotModuleComparison(fulfil_expect)
