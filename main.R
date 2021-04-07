topic <- tolower("animal behavior and cognition")

library(httr)
library(glue)
library(dplyr)
library(googlesheets4)

# Store the Google Sheets ID in the GSHEET_ID environment variable
gsheet_id <- Sys.getenv("GSHEET_ID")
# Google Sheets auth. See https://gargle.r-lib.org/articles/get-api-credentials.html#service-account-token-1
json <- gargle:::secret_read("procButils", "procButils.json")
gs4_auth(path = rawToChar(json))

today <- Sys.Date()
current_month <- format(today, "%B %Y")

# It's good to get the preprints from yesterday since:
# 1. We don't have to go too far back and won't get too many results, which
#    would be an issue on GitHub Actions.
# 2. We are sure that all preprints are out for this specific day.
req_yesterday <- glue("https://api.biorxiv.org/details/biorxiv/{today-2}/{today-1}")

yesterday_res <- content(GET(glue("{req_yesterday}/0")))

yesterday_preprints <- yesterday_res$collection

# Manual pagination since httr doesn't support it yet.
total_nb <- yesterday_res$messages[[1]]$total

if (total_nb > 100) {
  for (cursor in seq(100, total_nb, by = 100)) {
    yesterday_res <- content(GET(glue("{req_yesterday}/{cursor}")))

    yesterday_preprints <- c(yesterday_preprints, yesterday_res$collection)
  }
}

preprints <- do.call(rbind.data.frame, yesterday_preprints)

# It would be more efficient if this bot handled many topics at the same time
# (and dispatched each topic to its own sheet in a shared document. It would
# prevent repeated requests to the API.
preprints_topic <- preprints %>%
  filter(category == topic)

# This data.frame will contain the same column as the spreadsheet we have to
# fill for ProcB. This makes copy/pasting easier.
ready_tocopy <- preprints_topic %>%
  transmute(
    Topic = tools::toTitleCase(category),
    `Editorial Team Member` = "",
    `Last name (only!) Corresponding Author` = as.person(author_corresponding)$family,
    `Title of paper` = title,
    `Email of corresponding` = "",
    `link (full text)` = glue("https://www.biorxiv.org/content/{doi}v{version}")
  )


gsheet <- gs4_get(gsheet_id)

if (!current_month %in% sheet_names(gsheet)) {
  gsheet %>%
    sheet_add(current_month) %>%
    sheet_append(list2DF(as.list(colnames(ready_tocopy))), sheet = current_month)
}

gsheet %>%
  sheet_append(ready_tocopy, sheet = current_month)
