# ---------------------------------------------------------------------------------------
# Research paper GGOV 600
# Benedikt Beck
# Fall Term 2021
# Determinants of Humanitarian Interventions
# ---------------------------------------------------------------------------------------

library(tidyverse)
library(countrycode)
library(ggmap)


#### 1. Data #### ===============================================================

# 1.1 Dependent Variable

# Load PRIF data set

if (!file.exists("data/PRIF data set HMI v0-3 interventions.xlsx")) {
  print("Please Download PRIF data set from https://www.humanitarian-military-interventions.com/downloads/")
} else {
  prif <-  readxl::read_xlsx("data/PRIF data set HMI v0-3 interventions.xlsx") %>%
    filter(COLDWAR==0) %>% 
    transmute(
      Intervention = HMIID,
      country_iso3c = WBCC,
      id = UCDPID %>% str_remove_all(" ") %>%  str_split(","),
      ucdp_actor = ACTORID,
      intervention_start = HMISTART,
      intervention_end = HMIEND,
      intervention_endtype = ENDTYPE,
      duration_prior = ifelse(VIODURAT=="-99", NA, (as.numeric(VIODURAT)/12) %>% ceiling()),
      onesided_prior = VIOLENCE,
      nonstate_prior = ifelse(NONSTATE==-99,NA,NONSTATE),
      fatalities_prior = ifelse(FATALITY=="-99",NA,as.numeric(FATALITY)),
      ceasefire_prior = ifelse(ACCORD6==-99,NA,ACCORD6),
      ceasefire_after = ifelse(ACCORDI==-99,NA,ACCORDI),
      unsc_mandate = UNSC,
      intervener1 = ifelse(
        !is.na(countrycode(I1CCC, origin = "cown", destination = "iso3c")),
        countrycode(I1CCC, origin = "cown", destination = "iso3c"),
        INTERVEN1),
      intervener2 = ifelse(
        !is.na(countrycode(I2CCC, origin = "cown", destination = "iso3c")),
        countrycode(I2CCC, origin = "cown", destination = "iso3c"),
        ifelse(I2CCC==-88,NA,INTERVEN2)),
      intervener3 = ifelse(
        !is.na(countrycode(I3CCC, origin = "cown", destination = "iso3c")),
        countrycode(I3CCC, origin = "cown", destination = "iso3c"),
        ifelse(I3CCC==-88,NA,INTERVEN3)),
      colonial = COLONY2,
    ) %>% unnest(id) %>% 
    arrange(id, intervention_start) %>% 
    group_by(id) %>% 
    summarise(
      Intervention = first(Intervention),
      country_iso3c = first(country_iso3c),
      ucdp_actor = first(ucdp_actor),
      intervention_start = first(intervention_start),
      intervention_end = last(intervention_end),
      intervention_endtype = last(intervention_endtype),
      duration_prior = first(duration_prior),
      onesided_prior = first(onesided_prior),
      nonstate_prior = first(nonstate_prior),
      fatalities_prior = first(fatalities_prior),
      ceasefire_prior = first(ceasefire_prior),
      ceasefire_after = last(ceasefire_after),
      unsc_mandate = first(unsc_mandate),
      intervener1 = first(intervener1),
      intervener2 = first(intervener2),
      intervener3 = first(intervener3),
      colonial = first(colonial)
    ) %>% filter(intervener1 != "RUS")
}

# ucdp data set

if (!file.exists("data/ucdp-brd-dyadic-211.csv")) {
  print("Please Download battle related deaths data set from https://ucdp.uu.se/downloads/")
} else {
  ucdp <- read.csv("data/ucdp-brd-dyadic-211.csv") %>% 
    filter(year >= 1990) %>% 
    transmute(
      id = as.character(conflict_id),
      year = year,
      iso3c = location_inc %>% 
                str_remove_all(",.*") %>% 
                str_remove_all("\\(.*") %>% 
                countrycode(origin = "country.name", destination = "iso3c"),
      region = region,
      actor1 = ifelse(gwno_a=="678", "YEM",
                      ifelse(gwno_a=="345", "SRB",
                             gwno_a %>% countrycode(origin = "gwn", destination = "iso3c"))),
      actor2 = gwno_b %>% str_remove_all(",.*") %>%
                countrycode(origin = "gwn", destination = "iso3c"),
      actor3 = ifelse(str_detect(gwno_b,","),
                      str_remove(gwno_b,".*,[:blank:]") %>%
                        countrycode(origin = "gwn", destination = "iso3c"),
                      NA),
      issue = incompatibility,
                      # 1= Incompatibility about territory 
                      # 2= Incompatibility about government
                      # 3= Incompatibility about government AND territory
      conflict_type = type_of_conflict,
                      # 1= extrasystemic
                      # 2= interstate
                      # 3= intrastate
                      # 4= internationalized intrastate
      fatalities = bd_best
    ) %>% 
    arrange(id, year) %>% 
    group_by(id) %>% 
    summarise(
      iso3c = last(iso3c),
      region=last(region),
      start = first(year),
      end = ifelse(last(year)==2020,NA,last(year)),
      duration = last(year)-first(year)+1,
      continous = ifelse(n_distinct(year)!=(last(year)-first(year)+1),0,1),
      conflictyears = n_distinct(year),
      peaceyears = (last(year)-first(year)+1)-n_distinct(year),
      fatalities = sum(fatalities),
      issue = last(issue),
      conflict_type=last(conflict_type)
    )
}

ucdp$intervention <- ifelse(
  ucdp$id %in% prif$id,1,0
)

ucdp <- ucdp %>% left_join(prif, by = "id") %>% 
  mutate(
    fatalities = ifelse(is.na(fatalities_prior),fatalities,
      ifelse(fatalities_prior>fatalities,fatalities_prior,fatalities))
  )

#five year fixed effects

ucdp$tfe <- ifelse(ucdp$start < 1995, 1,
                   ifelse(ucdp$start < 2000,2,
                          ifelse(ucdp$start < 2005,3,
                                 ifelse(ucdp$start < 2010,4,
                                        ifelse(ucdp$start < 2015,5,6)))))

# 1.2. Independent Variable

# Military Alliance (ATOP data set)

if (!file.exists("data/atop5_0dy.csv")) {
  print("Please Download ATOP 5.0 dyadic-year data set from http://www.atopdata.org/data.html")
} else {
  atop <- read.csv("data/atop5_0dy.csv") %>% 
    filter(year>=1990) %>% filter(mem1==2 | mem1==200| mem1==220| mem2==2 | mem2==200| mem2==220) %>%
    transmute(
      start = year,
      iso3c = str_c(
        mem1 %>% countrycode(origin = "cown", destination = "iso3c"),
        ",",
        mem2 %>% countrycode(origin = "cown", destination = "iso3c")),
      alliance = 1
    ) %>% 
    mutate(
      iso3c = iso3c %>% str_split(",")
    ) %>% unnest(iso3c) %>% 
    unique()
}

ucdp <- ucdp %>% left_join(atop, by = c("iso3c","start"))
ucdp$alliance[is.na(ucdp$alliance)] <- 0

# Resources

#Oil

if (!file.exists("data/INT-Export-11-21-2021_13-20-36.csv")) {
  print("Please Download proven oil reserves data set from:
        https://www.eia.gov/international/data/world/petroleum-and-other-liquids/annual-crude-and-lease-condensate-reserves
        and delete 1st & 3rd row and insert 'Country' in head of 2nd column")
} else {
  oil <- read.csv("data/INT-Export-11-21-2021_13-20-36.csv", check.names = T)[,2:44] %>%
    mutate(
      X2020 = as.character(X2020),
      X2021 = as.character(X2021)
    ) %>% 
    pivot_longer(cols = !Country,
                 names_to = "year",
                 values_to = "oil") %>% 
    transmute(
      iso3c = Country %>% str_trim() %>% str_remove_all("Former U.S.S.R.") %>%
        str_remove_all("U.S. Territories") %>% 
        countrycode(origin = "country.name", destination = "iso3c"),
      start = year %>% str_remove_all("X") %>% as.numeric(),
      oil = oil %>% as.numeric()
    ) %>% 
    filter(start>=1990) %>% unique()
}

ucdp <- ucdp %>% left_join(oil, by = c("iso3c","start"))

ucdp$oil[is.na(ucdp$oil)] <- 0

#Gas

if (!file.exists("data/INT-Export-11-21-2021_13-20-36.csv")) {
  print("Please Download proven gas reserves data set from:
        https://www.eia.gov/international/data/world/petroleum-and-other-liquids/annual-crude-and-lease-condensate-reserves
        and delete 1st & 3rd row and insert 'Country' in head of 2nd column")
} else {
  gas <- read.csv("data/INT-Export-11-21-2021_15-22-37.csv", check.names = T)[,2:44] %>%
    mutate(
      X2020 = as.character(X2020),
      X2021 = as.character(X2021)
    ) %>% 
    pivot_longer(cols = !Country,
                 names_to = "year",
                 values_to = "gas") %>% 
    transmute(
      iso3c = Country %>% str_trim() %>% str_remove_all("Former U.S.S.R.") %>%
        str_remove_all("U.S. Territories") %>% 
        countrycode(origin = "country.name", destination = "iso3c"),
      start = year %>% str_remove_all("X") %>% as.numeric(),
      gas = gas %>% as.numeric()
    ) %>% 
    filter(start>=1990) %>% unique()
}

ucdp <- ucdp %>% left_join(gas, by = c("iso3c","start"))

ucdp$gas[is.na(ucdp$gas)] <- 0

# Contiguity

if (!file.exists("data/contcold.csv")) {
  print("Please Download colonial contiguity set from https://correlatesofwar.org/data-sets/colonial-dependency-contiguity")
} else {
  cont <- read.csv("data/contcold.csv") %>% 
    filter(year>=1990) %>% filter(statelno==2 | statelno==200| statelno==220| statehno==2 | statehno==200| statehno==220) %>% 
    transmute(
      iso3c = str_c(
        countrycode(statelno, origin = "cown", destination = "iso3c"),
        ",",
        countrycode(statehno, origin = "cown", destination = "iso3c")),
      start = year,
      cont = 1
    ) %>% 
    mutate(
      iso3c = iso3c %>% str_split(",")
    ) %>% 
    unnest(iso3c) %>% 
    unique()
  }

ucdp <- ucdp %>% left_join(cont, by=c("iso3c","start"))

ucdp$cont[is.na(ucdp$cont)] <- 0

#gdp

if (!file.exists("data/gross-domestic-product.csv")) {
  print("Please Download gdp data set from https://ourworldindata.org/economic-growth")
} else {
  gdp <- read.csv("data/gross-domestic-product.csv") %>% 
    filter(Year>=1990) %>% 
    transmute(
      iso3c = Code,
      start = Year,
      gdp = GDP..constant.2010.US../1000000000
    ) %>% 
    unique()
  }

ucdp <- ucdp %>% left_join(gdp, by=c("iso3c","start"))

# Human rights index

if (!file.exists("data/human-rights-scores.csv")) {
  print("Please Download human rights scores data set from https://ourworldindata.org/grapher/human-rights-scores")
} else {
  hrs <- read.csv("data/human-rights-scores.csv") %>% 
    filter(Year>=1990) %>% 
    transmute(
      iso3c = Code,
      start = Year,
      hrs = Human.Rights.Score..Schnakenberg...Fariss..2014..Fariss..2019.
    ) %>% 
    unique()
  }

ucdp <- ucdp %>% left_join(hrs, by = c("iso3c","start"))

#mean imputation

ucdp$hrs_mid <- ifelse(is.na(ucdp$hrs),1,0)
ucdp$hrs_mi <- ifelse(is.na(ucdp$hrs),mean(ucdp$hrs, na.rm = T),ucdp$hrs)

#Colonies

if (!file.exists("data/Colonial_transformation_data.csv")) {
  print("Please Download human rights scores data set from
        https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/UQZFYA/ADDVQR&version=1.0")
} else {
  colonial <- read.csv("data/Colonial_transformation_data.csv") %>% 
    transmute(
      iso3c = Country.Code.World.Bank %>% countrycode(origin = "wb", destination = "iso3c"),
      colonizer = Main.colonial..motherland...source..Ziltener.KŸnzler,
    ) %>% 
    filter(colonizer %in% c("UK","UK/F","US","F")) %>% 
    transmute(
      iso3c=iso3c,
      colony = 1
    )
}

ucdp <- ucdp %>% left_join(colonial, by="iso3c")

ucdp$colony[is.na(ucdp$colony)] <- 0

# trade



#### 2. description #### ===============================================================

world.map <- map_data("world") %>% 
  mutate(iso3c = region %>% countrycode(origin = "country.name", destination = "iso3c"))

world.map$intervention <- ifelse(world.map$iso3c %in% ucdp$iso3c[ucdp$intervention==1], T,F)

pdf("graphs/Intervention_map.pdf")

ggplot() +
  geom_polygon(data = world.map, aes(x = long, y = lat, group = group, fill = intervention)) +
  labs(title = 'Humanitarian Intervention in Conflicts since 1990'
       ,subtitle = "Data source: Interventions: Peace Research Institute Frankfurt, Conflicts: UCDP") +
  scale_fill_manual(values = c("#CCCCCC","#e60000")) +
  theme(text = element_text(color = "#FFFFFF")
        ,panel.background = element_rect(fill = "#444444")
        ,plot.background = element_rect(fill = "#444444")
        ,panel.grid = element_blank()
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
  )

dev.off()


pdf("graphs/Fatalities_Intervention.pdf")

ggplot(ucdp, aes(x=log(fatalities), y=intervention)) + geom_point() +
  labs(y="Likelihood of Humanitarian Intervention",
       x="log number of fatalities")+
  theme(title = element_text(hjust = 0.5, size = 10))+
  stat_smooth(method="glm", method.args=list(family=binomial(link = "logit")), se=TRUE)+
  theme_bw()

dev.off()

#### 3. Analysis #### ==================================================================


#3.1. Baseline saving strangers

m1 <- glm(intervention ~ log(fatalities), data = ucdp, family = binomial(link = "logit"))

#3.2. Saving strangers

m2 <- glm(intervention ~ log(fatalities) + duration + hrs_mi + hrs_mid, data = ucdp, family = binomial(link = "logit"))

#3.3. Imperial

m3 <- glm(intervention ~ alliance + oil + gas + cont + colony, data = ucdp, family = binomial(link = "logit"))

# 3.4. combined
m4 <- glm(intervention ~ log(fatalities) + duration + alliance + hrs_mi + oil + gas + cont + colony + hrs_mid, data = ucdp, family = binomial(link = "logit"))

# 3.5. combined with tfe
m5 <- glm(intervention ~ log(fatalities) + duration + alliance + hrs_mi + oil + gas + cont + colony + hrs_mid + factor(tfe)-1, data = ucdp, family = binomial(link = "logit"))


stargazer::stargazer(m2, m3, m4, m5, type="latex")

#3.6 Baseline saving strangers

m1_probit <- glm(intervention ~ log(fatalities), data = ucdp, family = binomial(link = "probit"))

#3.7. Saving strangers

m2_probit <- glm(intervention ~ log(fatalities) + duration + hrs_mi + hrs_mid, data = ucdp, family = binomial(link = "probit"))

#3.8. Imperial

m3_probit <- glm(intervention ~ alliance + oil + gas + cont + colony, data = ucdp, family = binomial(link = "probit"))

# 3.9. combined
m4_probit <- glm(intervention ~ log(fatalities) + duration + alliance + hrs_mi + oil + gas + cont + colony + hrs_mid, data = ucdp, family = binomial(link = "probit"))

# 3.5. combined with tfe
m5_probit <- glm(intervention ~ log(fatalities) + duration + alliance + hrs_mi + oil + gas + cont + colony + hrs_mid + factor(tfe)-1, data = ucdp, family = binomial(link = "probit"))


stargazer::stargazer(m2_probit, m3_probit, m4_probit, m5_probit, type="latex")
