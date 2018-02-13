library(datasets)
library(dplyr)
data(WorldPhones)
DF = melt(WorldPhones)
colnames(DF) = c("year", "country", "number")
DF = arrange(DF, country)

# Part 2
library(readr)
spec = read_csv("./Google Drive/Grad School/Computing/SPEC_2014.csv.zip")
# 1
spec %>% 
  filter(Parameter.Name == "Bromine PM2.5 LC" & State.Name == "Wisconsin") %>%
  summarize(mean=mean(Sample.Value))
# 2
spec %>%
  group_by(Parameter.Name) %>%
  summarize(mean = mean(Sample.Value)) %>%
  arrange(-mean)
# the rest are in R markdown, quite simple

# Part 3
library(readxl)
aqs_sites = read_excel("./Google Drive/Grad School/Computing/aqs_sites.xlsx")
head(aqs_sites)
aqs_sites %>%
  filter(`Land Use` == "RESIDENTIAL" & `Location Setting` == "SUBURBAN") %>%
  summarize(n_rows = n())

# 2
res_sub_sites = aqs_sites %>% 
  filter(`Land Use` == "RESIDENTIAL" & `Location Setting` == "SUBURBAN") %>% 
  filter(Longitude >= -100) %>%
  select(`State Code`, `County Code`, `Site Number`)

# Change data type of spec to numeric
spec_numeric = mutate(spec, State.Code = as.numeric(State.Code),
                      County.Code = as.numeric(County.Code),
                      Site.Num = as.numeric(Site.Num))

res_sub_spec = inner_join(spec_numeric, res_sub_sites, by = c("State.Code" = "State Code",
                                                              "County.Code" = "County Code",
                                                              "Site.Num" = "Site Number"))
res_sub_spec %>%
  filter(Parameter.Name == "EC PM2.5 LC TOR" | Parameter.Name == "OC PM2.5 LC TOR") %>%
  group_by(Parameter.Name) %>%
  summarize(median = median(Sample.Value))

