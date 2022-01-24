library(tidyverse)

parsefile <- c("TRUE","FALSE","TRUE")
new <- readr::parse_logical(parsefile)

