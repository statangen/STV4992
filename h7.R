library(readxl)
library(openxlsx)
library(tidyverse)
library(dplyr)
library(haven)
library(ggplot2)
library(tinytable)
library(janitor)
library(stringr)
library(lubridate)
library(rvest)
library(zoo)
library(PxWebApiData)
library(jsonlite)
library(httr)
library(sf)
library(MASS)
library(sandwich)
library(stargazer)
library(moments)
library(DescTools)
library(e1071)
library(lmom)
library(ineq)
library(scales)
library(entropy)
library(tidyr)
options(scipen=999)
#rm(list = ls())
setwd("~/Documents/Studium/STV4992/5. Datagrunnlag/datasett")
b <- read_excel("b.xlsx", 1)
kpi <- read_excel("inflasjon.xlsx", 1)
kpi <- kpi %>%
  dplyr::select(1:2)
b <-     b %>%
  left_join(kpi, by = "år") %>%
  mutate(gb_kpi = gb * (100 / kpi_2015)) %>%
  mutate(bb_kpi = bb * (100 / kpi_2015))
b <- b %>% dplyr::select(!kpi_2015)
b <- b %>% filter(!(bb == 0 & gb == 0))
b <- b %>% mutate(gb_bb_endring = bb_kpi/gb_kpi - 1)
u <- b %>% filter(kap < 3000)
i <- b %>% filter(kap >= 3000)

apc_kap <- function(df) {
  df %>%
    group_by(kap_t, år) %>%
    summarise(
      gb = sum(gb, na.rm = TRUE),
      bb = sum(bb, na.rm = TRUE),
      gb_kpi = sum(gb_kpi, na.rm = TRUE),
      bb_kpi = sum(bb_kpi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(kap_t, år) %>%
    group_by(kap_t) %>%
    mutate(
      lag_år = lag(år),
      lag_gb = lag(gb),
      lag_bb = lag(bb),
      lag_gb_kpi = lag(gb_kpi),
      lag_bb_kpi = lag(bb_kpi),
      
      gb_nomendring = if_else(år - lag_år == 1, gb - lag_gb, NA_real_),
      bb_nomendring = if_else(år - lag_år == 1, bb - lag_bb, NA_real_),
      gb_kpi_nomendring = if_else(år - lag_år == 1, gb_kpi - lag_gb_kpi, NA_real_),
      bb_kpi_nomendring = if_else(år - lag_år == 1, bb_kpi - lag_bb_kpi, NA_real_),
      
      gb_endring = case_when(
        år - lag_år == 1 & lag_gb > 0 & gb == 0 ~ -100,
        år - lag_år == 1 & lag_gb == 0 & gb == 0 ~ 0,
        år - lag_år == 1 ~ (gb - lag_gb) / lag_gb * 100,
        TRUE ~ NA_real_),
      
      bb_endring = case_when(
        år - lag_år == 1 & lag_bb > 0 & bb == 0 ~ -100,
        år - lag_år == 1 & lag_bb == 0 & bb == 0 ~ 0,
        år - lag_år == 1 ~ (bb - lag_bb) / lag_bb * 100,
        TRUE ~ NA_real_),
      
      gb_kpi_endring = case_when(
        år - lag_år == 1 & lag_gb_kpi > 0 & gb_kpi == 0 ~ -100,
        år - lag_år == 1 & lag_gb_kpi == 0 & gb_kpi == 0 ~ 0,
        år - lag_år == 1 ~ (gb_kpi - lag_gb_kpi) / lag_gb_kpi * 100,
        TRUE ~ NA_real_),
      
      bb_kpi_endring = case_when(
        år - lag_år == 1 & lag_bb_kpi > 0 & bb_kpi == 0 ~ -100,
        år - lag_år == 1 & lag_bb_kpi == 0 & bb_kpi == 0 ~ 0,
        år - lag_år == 1 ~ (bb_kpi - lag_bb_kpi) / lag_bb_kpi * 100,
        TRUE ~ NA_real_)
      
    ) %>%
    dplyr::select(-lag_år, -lag_gb, -lag_bb, -lag_gb_kpi, -lag_bb_kpi) %>%
    ungroup()
}
apc_post <- function(df) {
  df %>%
    arrange(as.numeric(post_id), år) %>%
    group_by(post_id) %>%
    mutate(
      lag_år = lag(år),
      lag_gb = lag(gb),
      lag_bb = lag(bb),
      lag_gb_kpi = lag(gb_kpi),
      lag_bb_kpi = lag(bb_kpi),
      
      gb_nomendring = if_else(år - lag_år == 1, gb - lag_gb, NA_real_),
      bb_nomendring = if_else(år - lag_år == 1, bb - lag_bb, NA_real_),
      gb_kpi_nomendring = if_else(år - lag_år == 1, gb_kpi - lag_gb_kpi, NA_real_),
      bb_kpi_nomendring = if_else(år - lag_år == 1, bb_kpi - lag_bb_kpi, NA_real_),
      
      gb_endring = case_when(
        år - lag_år == 1 & lag_gb > 0 & gb == 0 ~ -100,
        år - lag_år == 1 & lag_gb == 0 & gb == 0 ~ 0,
        år - lag_år == 1 ~ (gb - lag_gb) / lag_gb * 100,
        TRUE ~ NA_real_),
      
      bb_endring = case_when(
        år - lag_år == 1 & lag_bb > 0 & bb == 0 ~ -100,
        år - lag_år == 1 & lag_bb == 0 & bb == 0 ~ 0,
        år - lag_år == 1 ~ (bb - lag_bb) / lag_bb * 100,
        TRUE ~ NA_real_),
      
      gb_kpi_endring = case_when(
        år - lag_år == 1 & lag_gb_kpi > 0 & gb_kpi == 0 ~ -100,
        år - lag_år == 1 & lag_gb_kpi == 0 & gb_kpi == 0 ~ 0,
        år - lag_år == 1 ~ (gb_kpi - lag_gb_kpi) / lag_gb_kpi * 100,
        TRUE ~ NA_real_),
      
      bb_kpi_endring = case_when(
        år - lag_år == 1 & lag_bb_kpi > 0 & bb_kpi == 0 ~ -100,
        år - lag_år == 1 & lag_bb_kpi == 0 & bb_kpi == 0 ~ 0,
        år - lag_år == 1 ~ (bb_kpi - lag_bb_kpi) / lag_bb_kpi * 100,
        TRUE ~ NA_real_)
      
    ) %>%
    dplyr::select(-lag_år, -lag_gb, -lag_bb, -lag_gb_kpi, -lag_bb_kpi) %>%
    ungroup()
}
add_kap <- function(add_to, add_from) {
  kaplist <- data.frame(kap_t = unique(add_from$kap_t))
  kaplist <- kaplist %>%
    left_join(
      add_from %>% group_by(kap_t) %>% 
        summarise(kap = first(kap), .groups = "drop"), by = "kap_t")
  add_to <- add_to %>%
    left_join(kaplist, by = "kap_t")
}
ukap <- apc_kap(u)
upost <- apc_post(u)
ikap <- apc_kap(i)
ipost <- apc_post(i)
ukap <- add_kap(ukap, u)
ikap <- add_kap(ikap, i)
rens <- function(df) {
  df <- df %>%
    filter(!is.infinite(gb_endring)) %>%  
    filter(!is.infinite(bb_endring)) %>%
    filter(!is.infinite(gb_kpi_endring)) %>%  
    filter(!is.infinite(bb_kpi_endring)) %>%
    filter(!is.na(gb_endring) & 
             !is.na(bb_endring) & 
             !is.na(gb_kpi_endring) & 
             !is.na(bb_kpi_endring))
}
ukap <- rens(ukap)
upost <- rens(upost)
ikap <- rens(ikap)
ipost <- rens(ipost)
reg <- read_excel("reg.xlsx", 1)
ukap <- ukap %>%
  left_join(reg, by = "år")
ikap <- ikap %>%
  left_join(reg, by = "år")
z <- 3
pe <- function(df1, df2, bok) {
  
  endring <- paste0(bok, "_endring")
  beløp <- paste0(bok, "_nomendring")
  
  median_endring <- median(df1[[endring]], na.rm = TRUE)
  madsd_endring <- mad(df1[[endring]], na.rm = TRUE)
  mean_post <- mean(df2[[bok]], na.rm = TRUE)
  
  df1 %>%
    mutate(
      avvik_fra_median = df1[[endring]] - median_endring,
      prosentkriteriet = abs(avvik_fra_median) > z * madsd_endring,
      beløpskriteriet = abs(df1[[beløp]]) > mean_post * (madsd_endring * z / 100),
      punktering = prosentkriteriet & beløpskriteriet
    )
}
parametere <- function(df1, df2, bok) {
  
  endring <- paste0(bok, "_endring")
  beløp <- paste0(bok, "_nomendring")
  
  median_endring = median(df1[[endring]], na.rm = TRUE)
  sd_endring = sd(df1[[endring]], na.rm = TRUE)
  madsd_endring = mad(df1[[endring]], na.rm = TRUE)
  mean_post = mean(df2[[bok]], na.rm = TRUE)
  
  data.frame(Parameter = c("Median", "MAD", "SD", "Gjennomsnitt", 
                           "Prosentkriteriet", "Beløpskriteriet"),
             Verdi = c(median_endring, madsd_endring, sd_endring, mean_post,
                       z * madsd_endring, mean_post* 
                         (z * madsd_endring / 100)))
  
}

### Justeringer ####
# Statens forretningsdrift
u <- u %>% filter(!(kap >= 2445 & kap <= 2499)) # Utgifter
i <- i %>% filter(!(kap >= 5445 & kap <= 5499)) # Inntekter

# Statsgjeld og statslån
u <- u %>% filter(!kap == 1650) %>% filter(!kap == 1651) # Statsgjeld
i <- i %>% filter(!kap == 5341) # Avdrag på fordringer
i <- i %>% filter(!kap == 5999) # Statslånemidler

# UTGIFTER #####################################################################
ukap_gb_parameter <- parametere(ukap, upost, "gb")

## PUNKTERINGER ####
ukap <- pe(ukap, upost, "gb")
ukap_gb_punkteringer <- ukap %>%
  filter(punktering == TRUE)

sum(ukap$prosentkriteriet)
nrow(ukap_gb_punkteringer)/nrow(ukap)

### Plot: Policypunkteringer per år ####
ukap_gb_punkteringer_plot <- ggplot(ukap_gb_punkteringer, aes(x = år, fill = factor(valgår))) +
  geom_bar(alpha = 0.7, show.legend = FALSE)  +
  geom_hline(yintercept = mean(table(ukap_gb_punkteringer$år)), 
             linetype = "dashed") +
  geom_vline(xintercept = c(2001.5), linetype = "solid", size = 1) +
  geom_vline(xintercept = c(2002.5), linetype = "solid", size = 1) +
  geom_vline(xintercept = c(2005.5), linetype = "solid", size = 1) +
  geom_vline(xintercept = c(2013.5), linetype = "solid", size = 1) +
  geom_vline(xintercept = c(2021.5), linetype = "solid", size = 1) +
  labs(title = "Utgifter",
       x = "År",
       y = "Antall policypunkteringer",
       fill = "Valgår") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(2000, 2023, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))


ukap_unike <- u |> 
  dplyr::select(kap_t, hovedkat_t) |> 
  dplyr::distinct(kap_t, .keep_all = TRUE)

ukap_gb_punkteringer <- ukap_gb_punkteringer |> 
  dplyr::left_join(ukap_unike, by = "kap_t")

ukap_gb_hovedkat <- ukap_gb_punkteringer |> 
  count(hovedkat_t, name = "antall_punkteringer") |> 
  arrange(antall_punkteringer)

ukap_gb_hovedkat$hovedkat_t <- factor(
  ukap_gb_hovedkat$hovedkat_t,
  levels = ukap_gb_hovedkat$hovedkat_t)

### Plot: Policypunkteringer per CAP ####
ukap_gb_hovedkat_plot <- ggplot(ukap_gb_hovedkat, aes(x = hovedkat_t, y = antall_punkteringer)) +
  geom_col(fill = "blue", alpha = 0.7) +
  coord_flip() +  # Gjør søylene liggende
  labs(title = "Utgifter",
       x = "Hovedkategori",
       y = "Antall policypunkteringer") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

## TIDSSERIEANALYSE ####
unike_kap <- unique(ukap_gb_punkteringer$kap_t)

ukap_gb_tidsserier <- ukap %>%
  filter(kap_t %in% unike_kap)

ukap_gb_madsd <- ukap_gb_tidsserier %>%
  group_by(kap_t) %>%
  summarise(madsd = mad(gb_endring, na.rm = TRUE)) %>%
  mutate(gruppering = ntile(madsd, 10))

ukap_gb_madsd <- ukap_gb_madsd %>% arrange(madsd)

### Plot: MADSD per gruppe ####
ukap_gb_madsd_plot1 <- ggplot(ukap_gb_madsd, aes(x = gruppering, y = madsd)) +
  geom_point(colour = "blue", alpha = 0.7) +
  labs(title = "Utgifter",
       x = "Gruppering",
       y = "Skalert MAD") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(1,10,1))

ukap_gb_madsd_plot2 <- ggplot(ukap_gb_madsd, aes(x = gruppering, y = madsd)) +
  geom_point(colour = "blue", alpha = 0.7) +
  labs(title = "Utgifter",
       x = "Gruppering",
       y = "Skalert MAD") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(1,10,1))

ukap_gb_tidsserier <- ukap_gb_tidsserier %>%
  left_join(ukap_gb_madsd, by = "kap_t") 


# Kriterier for plott #
øvre_grense <- ukap_gb_parameter$Verdi[1] + ukap_gb_parameter$Verdi[2] * z
nedre_grense <- ukap_gb_parameter$Verdi[1] - ukap_gb_parameter$Verdi[2] * z

### Plot: Tidsserier etter MADSD-grupper ####
ukap_gb_tidsserier_plot <- ggplot(ukap_gb_tidsserier, aes(x = år, y = gb_endring)) +
  geom_line(aes(color = as.factor(kap_t)), show.legend = FALSE) +
  facet_wrap(~ gruppering, scales = "free_y") +
  labs(title = "Utgifter",
       x = "År",
       y = "Årlig prosentvis endring") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(2000,2023, 4))

### Plot: MADSD-serie ####
gruppe <- ukap_gb_tidsserier %>%
  filter(gruppering == 2)

ggplot(gruppe, aes(x = år, y = gb_endring)) +
  geom_line(aes(color = as.factor(kap_t)), show.legend = FALSE) +
  geom_hline(yintercept = øvre_grense, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = nedre_grense, colour = "black", linetype = "dashed") +
  facet_wrap(~ kap, scales = "free_y") +
  labs(x = "År",
       y = "Årlig prosentvis endring") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(2000,2023, 4))

## KAPITTELANALYSE ####

kapittel <- ukap_gb_tidsserier %>%
  filter(kap == 571)

prosentkriteriet <- kapittel %>%
  filter(gb_endring > øvre_grense | gb_endring < nedre_grense)

beløpskriteriet <- kapittel %>%
  filter(gb_endring > øvre_grense | gb_endring < nedre_grense & 
           abs(kapittel$gb_nomendring) > ukap_gb_parameter$Verdi[5])

### Plot: Kapittelserie ####
plot <- 
ggplot(kapittel, aes(x = år, y = gb_endring)) +
  geom_line(colour = "black", alpha = 0.7) +
  geom_point(data = prosentkriteriet, aes(x = år, y = gb_endring), color = "red", size = 1) +  # Avvikspunkter
  geom_point(data = beløpskriteriet, aes(x = år, y = gb_endring), color = "blue", size = 2) +  # Avvikspunkter
  geom_hline(yintercept = øvre_grense, colour = "red", linetype = "dashed") +
  geom_hline(yintercept = nedre_grense, colour = "red", linetype = "dashed") +
  labs(title = kapittel$kap_t[1],
       x = "År", 
       y = "Årlig prosentvis endring") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(2000, 2023, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
plot


# INNTEKTER ####################################################################

ikap_gb_parameter <- parametere(ikap, upost, "gb")

## PUNKTERINGER ####
ikap <- pe(ikap, upost, "gb")
ikap_gb_punkteringer <- ikap %>%
  filter(punktering == TRUE)

sum(ikap$prosentkriteriet)
nrow(ikap_gb_punkteringer)/nrow(ikap)

### Plot: Policypunkteringer per år ####
ikap_gb_punkteringer_plot <- ggplot(ikap_gb_punkteringer, aes(x = år, fill = factor(valgår))) +
  geom_bar(alpha = 0.7, show.legend = FALSE)  +
  geom_hline(yintercept = mean(table(ikap_gb_punkteringer$år)), 
             linetype = "dashed") +
  geom_vline(xintercept = c(2001.5), linetype = "solid", size = 1) +
  geom_vline(xintercept = c(2002.5), linetype = "solid", size = 1) +
  geom_vline(xintercept = c(2005.5), linetype = "solid", size = 1) +
  geom_vline(xintercept = c(2013.5), linetype = "solid", size = 1) +
  geom_vline(xintercept = c(2021.5), linetype = "solid", size = 1) +
  labs(title = "Inntekter",
       x = "År",
       y = "Antall policypunkteringer",
       fill = "Valgår") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(2000, 2023, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

ikap_unike <- i |> 
  dplyr::select(kap_t, hovedkat_t) |> 
  dplyr::distinct(kap_t, .keep_all = TRUE)

ikap_gb_punkteringer <- ikap_gb_punkteringer |> 
  dplyr::left_join(ikap_unike, by = "kap_t")

ikap_gb_hovedkat <- ikap_gb_punkteringer |> 
  count(hovedkat_t, name = "antall_punkteringer") |> 
  arrange(antall_punkteringer)

ikap_gb_hovedkat$hovedkat_t <- factor(
  ikap_gb_hovedkat$hovedkat_t,
  levels = ikap_gb_hovedkat$hovedkat_t)

### Plot: Policypunkteringer per CAP ####
ikap_gb_hovedkat_plot <- ggplot(ikap_gb_hovedkat, aes(x = hovedkat_t, y = antall_punkteringer)) +
  geom_col(fill = "blue", alpha = 0.7) +
  coord_flip() +  # Gjør søylene liggende
  labs(title = "Inntekter",
       x = "Hovedkategori",
       y = "Antall policypunkteringer") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

## TIDSSERIEANALYSE ####
unike_kap <- unique(ikap_gb_punkteringer$kap_t)

ikap_gb_tidsserier <- ikap %>%
  filter(kap_t %in% unike_kap)

ikap_gb_madsd <- ikap_gb_tidsserier %>%
  group_by(kap_t) %>%
  summarise(madsd = mad(gb_endring, na.rm = TRUE)) %>%
  mutate(gruppering = ntile(madsd, 10))

ikap_gb_madsd <- ikap_gb_madsd %>% arrange(madsd)

### Plot: MADSD per gruppe ####
ikap_gb_madsd_plot1 <- ggplot(ikap_gb_madsd, aes(x = gruppering, y = madsd)) +
  geom_point(colour = "blue", alpha = 0.7) +
  labs(title = "Inntekter",
       x = "Gruppering",
       y = "Skalert MAD") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(1,10,1))

ikap_gb_madsd_plot2 <- ggplot(ikap_gb_madsd, aes(x = gruppering, y = madsd)) +
  geom_point(colour = "blue", alpha = 0.7) +
  labs(title = "Inntekter",
       x = "Gruppering",
       y = "Skalert MAD") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(1,10,1))

ikap_gb_tidsserier <- ikap_gb_tidsserier %>%
  left_join(ikap_gb_madsd, by = "kap_t") 

# Kriterier for plott #
øvre_grense <- ikap_gb_parameter$Verdi[1] + ikap_gb_parameter$Verdi[2] * z
nedre_grense <- ikap_gb_parameter$Verdi[1] - ikap_gb_parameter$Verdi[2] * z

### Plot: Tidsserier etter MADSD-grupper ####
ikap_gb_tidsserier_plot <- ggplot(ikap_gb_tidsserier, aes(x = år, y = gb_endring)) +
  geom_line(aes(color = as.factor(kap_t)), show.legend = FALSE) +
  facet_wrap(~ gruppering, scales = "free_y") +
  labs(title = "Inntekter",
       x = "År",
       y = "Årlig prosentvis endring") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(2000,2023, 4))

### Plot: MADSD-serie ####
gruppe <- ikap_gb_tidsserier %>%
  filter(gruppering == 2)

ggplot(gruppe, aes(x = år, y = gb_endring)) +
  geom_line(aes(color = as.factor(kap_t)), show.legend = FALSE) +
  geom_hline(yintercept = øvre_grense, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = nedre_grense, colour = "black", linetype = "dashed") +
  facet_wrap(~ kap, scales = "free_y") +
  labs(x = "År",
       y = "Årlig prosentvis endring") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(2000,2023, 4))

## KAPITTELANALYSE ####

kapittel <- ikap_gb_tidsserier %>%
  filter(kap == 5507)

prosentkriteriet <- kapittel %>%
  filter(gb_endring > øvre_grense | gb_endring < nedre_grense)

beløpskriteriet <- kapittel %>%
  filter(gb_endring > øvre_grense | gb_endring < nedre_grense & 
           abs(kapittel$gb_nomendring) > ikap_gb_parameter$Verdi[5])

### Plot: Kapittelserie ####
plot <- 
  ggplot(kapittel, aes(x = år, y = gb_endring)) +
  geom_line(colour = "black", alpha = 0.7) +
  geom_point(data = prosentkriteriet, aes(x = år, y = gb_endring), color = "red", size = 1) +  # Avvikspunkter
  geom_point(data = beløpskriteriet, aes(x = år, y = gb_endring), color = "blue", size = 2) +  # Avvikspunkter
  geom_hline(yintercept = øvre_grense, colour = "red", linetype = "dashed") +
  geom_hline(yintercept = nedre_grense, colour = "red", linetype = "dashed") +
  labs(title = kapittel$kap_t[1],
       x = "År", 
       y = "Årlig prosentvis endring") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(2000, 2023, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
plot

# UTSKRIFT #####################################################################
ukap_unike <- u |> 
  dplyr::select(kap_t, norcap) |> 
  dplyr::distinct(kap_t, .keep_all = TRUE)

ukap_gb_punkteringer <- ukap_gb_punkteringer |> 
  dplyr::left_join(ukap_unike, by = "kap_t")

names(ukap_gb_punkteringer)

ikap_unike <- i |> 
  dplyr::select(kap_t, norcap) |> 
  dplyr::distinct(kap_t, .keep_all = TRUE)

ikap_gb_punkteringer <- ikap_gb_punkteringer |> 
  dplyr::left_join(ikap_unike, by = "kap_t")

names(ikap_gb_punkteringer)

# ggsave(filename = "~/Desktop/quicksave.png", plot = plot, width = 6, height = 4, dpi = 300)
#write.xlsx(ukap_gb_punkteringer, "~/Desktop/upunkteringer.xlsx")
#write.xlsx(ikap_gb_punkteringer, "~/Desktop/ipunkteringer.xlsx")

