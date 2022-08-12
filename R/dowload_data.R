
library(tidyverse)
library(data.table)
library(janitor)

source(".Rprofile")

gdoc <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vR0dkClo8RWZFr31zc1Lq_5LIi-dLGPZYkmKI7UD7M3K3mh_1g_8aWztHISf6fRLUPzSVdfFOY2dAMm/pubhtml#"

const_results_list <- readGoogleSheet(gdoc)

const_results <- cleanGoogleTable(const_results_list, table=2)
nms_results <- names(const_results)
na_index <- which(is.na(nms_results))
nms_na <- nms_results[is.na(nms_results)] %>% paste0("_", 1:length(na_index)) %>%
    tolower()
names(const_results)[na_index] <- nms_na
const_results <- nms_clean(const_results)
cols_interest <- c("county", "constituency", "code", "registered_voters", 
                   "raila", "ruto", "mwaure", "wajackoyah", "iebc_total", "auto_sum", 
                   "rejected_ballots")

form34b_result <- const_results[, ..cols_interest]

form34b_result <- form34b_result[!is.na(county)]

form34b_result[, counted := ifelse(!is.na(raila), 1, 0)]
brought <- form34b_result[, sum(counted)/.N] * 100 
brought <- round(brought, 2)
brought_perc <- paste(brought, "%")
cols_id_vars <- c("county", "constituency", 
                  "code", "registered_voters", 
                  "iebc_total", "auto_sum", "counted")

vars_num <- c("registered_voters", "raila", "ruto", "mwaure", "wajackoyah", 
              "iebc_total", "auto_sum", "rejected_ballots")



form34b_result[, (vars_num) := lapply(.SD, char_to_num), .SDcols = vars_num]
cand_lev <- c("raila", "ruto", "mwaure", 
              "wajackoyah", "rejected_ballots")

cand_lab <- c("Raila", "Ruto", "Mwaure", 
              "Wajackoyah", "Rejected Ballots")


form34b_resultm <- melt(form34b_result,
                        id.vars = cols_id_vars)

form34b_resultm[, variable := factor(variable, levels = cand_lev, labels = cand_lab)]



tab_president <- form34b_resultm[!is.na(value), .(freq = sum(value)),
                                 by = variable] %>%
    .[, Percentage := round(freq/sum(freq) * 100, 1)]

setorder(tab_president,-Percentage )

tab_president <- tab_president %>%
    adorn_totals(where = "row")

setDT(tab_president)


old_nms <- c("variable", "freq", "Percentage")
new_nms <- c("Candidate", "Votes", "%")


setnames(tab_president, old_nms, new_nms)
write_csv(form34b_result, file = "data/form34b_result.csv")
data_list <- list(tab_president = tab_president, 
                  form34b_resultm = form34b_resultm,
                  form34b_result = form34b_result)

save(data_list, file = "data/data_list.rda")