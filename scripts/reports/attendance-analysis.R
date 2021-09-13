library(RSQLite)
library(mosaic)

source(here::here('scripts/helpers.R'))

dat <- read_cohort_dbtable("attendance")

att.cols <- dat[, grepl("^M", names(dat))]

dat <- within(dat, {
  total <- purrr::pmap_int(att.cols, base::sum, na.rm = TRUE)
  percent <- round(total / 8 * 100, 1)
  total <- factor(total)
})

# Participation
df_partc <- subset(dat, select = c("name", "total", "percent"))

df_partc$name <- sub("\\s{2,}", " ", df_partc$name)

df_partc[order(df_partc$percent, decreasing = TRUE), ]

xa <- "Number of times attended"
ya <- "Number of participants"
tot.att <- df_partc$total
freqpolygon(as.numeric(tot.att), main = "Attendance of participants")
barplot(table(tot.att))
tapply(tot.att, sum)
barplot(cumsum(table(tot.att)), main = "Cumulative Distribution of SOGP attendanc", )
