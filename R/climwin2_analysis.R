

### libraries
library(tidyverse)
library(gridExtra)
library(lubridate)
source("R/functions.R")


### read files
spp_summary <- read_csv("data/end_months.csv")

tmp_results <- read_csv("output/climwin_tmp.csv") %>% 
  mutate(SpeciesAuthor = as.character(sapply(SpeciesAuthor, spp_short))) %>% 
  mutate(sig = ifelse(p_c < 0.05, TRUE, FALSE)) %>% 
  left_join(select(spp_summary, SpeciesAuthor, n_year, ecoreg)) %>% 
  mutate(end_date = as.Date(paste("2000",
                                  formatC(as.integer(end_month), width = 2, flag = "0"),
                                  "01", sep = "-"))) %>% 
  mutate(open = end_date - months(win_open),
         close = end_date - months(win_clos)) %>% 
  arrange(ecoreg, beta) %>% 
  mutate(SpeciesAuthor = factor(SpeciesAuthor, levels = SpeciesAuthor)) %>% 
  mutate(beta = ifelse(beta > 50, NA_real_, beta))

tmp_results_flat <- tmp_results %>% 
  select(SpeciesAuthor, ecoreg, open, close) %>% 
  gather(type, date, open:close) %>% 
  arrange(SpeciesAuthor)

ppt_results <- read_csv("output/climwin_ppt.csv") %>% 
  mutate(SpeciesAuthor = as.character(sapply(SpeciesAuthor, spp_short))) %>% 
  mutate(sig = ifelse(p_c < 0.05, TRUE, FALSE)) %>% 
  left_join(select(spp_summary, SpeciesAuthor, n_year, ecoreg)) %>% 
  mutate(end_date = as.Date(paste("2000",
                                  formatC(as.integer(end_month), width = 2, flag = "0"),
                                  "01", sep = "-"))) %>% 
  mutate(open = end_date - months(win_open),
         close = end_date - months(win_clos)) %>% 
  arrange(ecoreg, beta) %>% 
  mutate(SpeciesAuthor = factor(SpeciesAuthor, levels = SpeciesAuthor))

ppt_results_flat <- ppt_results %>% 
  select(SpeciesAuthor, open, close) %>% 
  gather(type, date, open:close) %>% 
  arrange(SpeciesAuthor)

year_df <- tibble(x = as.Date(c("1998-01-01", "1999-01-01", "2000-01-01")))



### Fig 1
tt <- theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(size = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 6))

p1 <- ggplot(tmp_results_flat, aes(x = SpeciesAuthor)) +
  geom_point(aes(y = date), size = 1.2, color = "#045a8d") +
  geom_point(data = tmp_results, aes(y = end_date), shape = 4, size = 1) +
  geom_segment(data = tmp_results, aes(xend = SpeciesAuthor, y = open, yend = close),
               size = 0.5, color = "#045a8d") +
  geom_hline(data = year_df, aes(yintercept = x), linetype = 2, alpha = 0.5) +
  coord_flip() +
  scale_y_date(date_breaks = "1 months", labels = function(x) month(x)) +
  labs(x = NULL, y = "Date relative to demographic survey") +
  tt

p2 <- ggplot(tmp_results, aes(x = SpeciesAuthor)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  geom_point(aes(y = beta), size = 0.9) +
  geom_point(data = filter(tmp_results, sig == TRUE),
             aes(y = 3), shape = 42, size = 4, color = "darkred") +
  geom_linerange(aes(ymin = beta - 1.96 * se_beta, ymax = beta + 1.96 * se_beta)) +
  coord_flip(ylim = c(-3.3, 3.3)) +
  labs(x = NULL, y = expression(beta)) +
  tt + theme(axis.text.y = element_blank())

gt <- arrangeGrob(p1, p2, nrow = 1, widths = c(1, 0.4))

dev.off()
quartz(height = 6, width = 8, dpi = 150)
grid.arrange(gt)

ggsave("img-raw/fig_1.png", gt, height = 6, width = 8, units = "in", dpi = 300)



### Fig 2
p3 <- ggplot(ppt_results_flat, aes(x = SpeciesAuthor)) +
  geom_point(aes(y = date), size = 1.2, color = "#045a8d") +
  geom_point(data = ppt_results, aes(y = end_date), shape = 4, size = 1) +
  geom_segment(data = ppt_results, aes(xend = SpeciesAuthor, y = open, yend = close),
               size = 0.5, color = "#045a8d") +
  geom_hline(data = year_df, aes(yintercept = x), linetype = 2, alpha = 0.5) +
  coord_flip() +
  scale_y_date(date_breaks = "1 months", labels = function(x) month(x)) +
  labs(x = NULL, y = "Date relative to demographic survey") +
  tt

p4 <- ggplot(ppt_results, aes(x = SpeciesAuthor)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  geom_point(aes(y = beta), size = 0.9) +
  geom_point(data = filter(ppt_results, sig == TRUE),
             aes(y = 2), shape = 42, size = 4, color = "darkred") +
  geom_linerange(aes(ymin = beta - 1.96 * se_beta, ymax = beta + 1.96 * se_beta)) +
  coord_flip(ylim = c(-2.15, 2.15)) +
  labs(x = NULL, y = expression(beta)) +
  tt + theme(axis.text.y = element_blank())

gp <- arrangeGrob(p3, p4, nrow = 1, widths = c(1, 0.4))

dev.off()
quartz(height = 6, width = 8, dpi = 150)
grid.arrange(gp)

ggsave("img-raw/fig_2.png", gp, height = 6, width = 8, units = "in", dpi = 300)



### Fig. 3
extract_windows <- function(win_open, win_clos) {
  tibble(win = seq(win_open, win_clos, by = "1 month"))
}

tmp_windows <- tmp_results %>% 
  mutate(SpeciesAuthor = as.character(SpeciesAuthor)) %>% 
  group_by(SpeciesAuthor) %>% 
  do(extract_windows(.$open, .$close)) %>% 
  ungroup() %>% 
  left_join(spp_summary)

ppt_windows <- ppt_results %>% 
  mutate(SpeciesAuthor = as.character(SpeciesAuthor)) %>% 
  group_by(SpeciesAuthor) %>% 
  do(extract_windows(.$open, .$close)) %>% 
  ungroup() %>% 
  left_join(spp_summary)

tt2 <- theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(size = 0.3),
        axis.text.x = element_text(size = 7.5))

year_lab1 <- tibble(x = as.Date(c("1998-09-01", "1999-07-01", "2000-05-01")),
                   lab = c("Year t-2", "Year t-1", "Year t"),
                   ecoreg = "Desert/Med")

year_lab2 <- tibble(x = as.Date(c("1998-08-01", "1999-07-01", "2000-06-01")),
                   lab = c("Year t-2", "Year t-1", "Year t"),
                   ecoreg = "Desert/Med")

p5 <- ggplot(tmp_windows, aes(win)) +
  geom_vline(data = year_df, aes(xintercept = x), linetype = 2, alpha = 0.5) +
  geom_bar(color = "white") +
  geom_text(data = year_lab1, aes(x, y = 12.5, label = lab), hjust = 0.5, size = 3) +
  scale_x_date(date_breaks = "1 months", labels = function(x) month(x)) + 
  facet_wrap(~ ecoreg, ncol = 1) +
  labs(x = "Month relative to demographic survey", y = "Temperature window frequency") +
  tt2

p6 <- ggplot(ppt_windows, aes(win)) +
  geom_vline(data = year_df, aes(xintercept = x), linetype = 2, alpha = 0.5) +
  geom_bar(color = "white") +
  geom_text(data = year_lab2, aes(x, y = 12.5, label = lab), hjust = 0.5, size = 3) +
  scale_x_date(date_breaks = "1 months", labels = function(x) month(x)) + 
  facet_wrap(~ ecoreg, ncol = 1) +
  labs(x = "Month relative to demographic survey", y = "Precipitation window frequency") +
  tt2

gh <- arrangeGrob(p5, p6, nrow = 1)

dev.off()
quartz(height = 6, width = 7.5, dpi = 150)
grid.arrange(gh)

# ggsave("img-raw/fig_3.png", gh, height = 6, width = 7.5, units = "in", dpi = 300)

