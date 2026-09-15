install.packages(c("httr2", "jsonlite", "tidyverse"))
library(httr2)
library(jsonlite)
library(tidyverse)
library(stringr)
library(jsonlite)
#APL
resp <- request("https://remoteok.com/api") |>
  req_user_agent("Mozilla/5.0 (R scraping demo)") |>
  req_perform()
resp_status(resp)

json_text <- resp_body_string(resp)
str_sub(json_text, 1, 500)
raw_data <- fromJSON(json_text)
raw_data <- fromJSON(json_text)

dim(raw_data)
names(raw_data)

jobs <- raw_data |>
  select(
    position,
    company,
    location,
    date,
    tags,
    salary_min,
    salary_max,
    url
  ) |>
  as_tibble()

jobs

jobs <- jobs |>
  filter(!is.na(position))

jobs
jobs$tags[[1]]

jobs <- jobs |>
  mutate(
    tags = map(tags, ~ as.character(unlist(.x)))
  )

tags_long <- jobs |>
  select(position, company, tags) |>
  unnest(tags) |>
  filter(!is.na(tags), tags != "")

tag_counts <- tags_long |>
  count(tags, sort = TRUE)

top_tags <- tag_counts |>
  slice_head(n = 20)

top_tags
tag_counts |> 
  slice_head(n = 60) |> 
  print(n = 60)

tech_skills_clean <- c(
  "golang", "sys admin", "testing", "cloud",
  "excel", "infosec", "microsoft", "saas",
  "web dev", "c", "mobile", "stats",
  "embedded", "api", "backend", "game dev",
  "python", "data science", "salesforce"
)

tech_counts_clean <- tag_counts |>
  filter(tags %in% tech_skills_clean) |>
  arrange(desc(n))

print(tech_counts_clean, n = 19)

#| fig-cap: "RemoteOK 上最常被要求的技术技能"
#| fig-width: 8
#| fig-height: 6

tech_counts_clean |>
  mutate(tags = reorder(tags, n)) |>
  ggplot(aes(x = n, y = tags)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), hjust = -0.3, size = 3.5) +
  labs(
    title = "The most frequently required technical skills on RemoteOK",
    subtitle = "Based on the skill label statistics of 99 remote positions",
    x = "Frequency of occurrence",
    y = NULL,
    caption = "Data from：RemoteOK (https://remoteok.com)"
  ) +
  theme_minimal(base_size = 13) +
  expand_limits(x = max(tech_counts_clean$n) * 1.1)
