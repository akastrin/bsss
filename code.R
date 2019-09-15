library(tidyverse)
library(psych)
library(nFactors)
library(polycor)
library(cowplot)
library(reshape2)
source("corstarsl.R")

# Uvoz podatkov
data <- read.csv("../../podatki/anketa150259-2018-03-03.csv", sep = ";", skip = 1)

q1 <- as.numeric(gsub("=", "", data[, 8]))
q2 <- as.numeric(gsub("=", "", data[, 9]))
q3 <- as.numeric(gsub("=", "", data[, 10]))
q4 <- as.numeric(gsub("=", "", data[, 11]))
q5 <- as.numeric(gsub("=", "", data[, 12]))
q6 <- as.numeric(gsub("=", "", data[, 13]))
q7 <- as.numeric(gsub("=", "", data[, 14]))
q8 <- as.numeric(gsub("=", "", data[, 15]))
df <- data.frame(q1, q2, q3, q4, q5, q6, q7, q8)

# Determine number of factors to extract
ev <- eigen(cor(df)) # get eigenvalues
ap <- parallel(subject = nrow(df), var = ncol(df), rep = 100, cent = .05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 

# Factor analysis
fit <- fa(r = df, nfactors = 4, rotate = "none")
fit <- fa(r = df, nfactors = 4, rotate = "varimax")
fit <- fa(r = df, nfactors = 4, rotate = "bifactor", fm = "pa")

# Factor analysis with categorical variables
df1 <- df %>% mutate_if(is.numeric, factor, ordered = TRUE)
pc <- hetcor(df1, ML = FALSE)
faPC <- fa(r=pc$correlations, nfactors=4, n.obs=377, rotate="oblimin")
print(fit, digits=2, cut=.2, sort=TRUE)

d <- dist(t(df), method = "euclidean")
hc <- hclust(d, method = "ward.D2")
plot(hc)


alpha(df)

# Postavka 1
q1_tbl <- as.data.frame(prop.table(table(q1)))
plt_q1 <- ggplot(q1_tbl, aes(x = q1, y = Freq, fill = "red")) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(limits = c(0, 0.35)) +
  coord_flip() +
  xlab("Vrednost") +
  ylab("Delež odgovorov") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  ggtitle("Postavka 1")

# Postavka 2
q2_tbl <- as.data.frame(prop.table(table(q2)))
plt_q2 <- ggplot(q2_tbl, aes(x = q2, y = Freq, fill = "red")) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(limits = c(0, 0.35)) +
  coord_flip() +
  xlab("Vrednost") +
  ylab("Delež odgovorov") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  ggtitle("Postavka 2")

# Postavka 3
q3_tbl <- as.data.frame(prop.table(table(q3)))
plt_q3 <- ggplot(q3_tbl, aes(x = q3, y = Freq, fill = "red")) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(limits = c(0, 0.35)) +
  coord_flip() +
  xlab("Vrednost") +
  ylab("Delež odgovorov") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  ggtitle("Postavka 3")

# Postavka 4
q4_tbl <- as.data.frame(prop.table(table(q4)))
plt_q4 <- ggplot(q4_tbl, aes(x = q4, y = Freq, fill = "red")) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(limits = c(0, 0.35)) +
  coord_flip() +
  xlab("Vrednost") +
  ylab("Delež odgovorov") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  ggtitle("Postavka 4")

# Postavka 5
q5_tbl <- as.data.frame(prop.table(table(q5)))
plt_q5 <- ggplot(q5_tbl, aes(x = q5, y = Freq, fill = "red")) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(limits = c(0, 0.35)) +
  coord_flip() +
  xlab("Vrednost") +
  ylab("Delež odgovorov") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  ggtitle("Postavka 5")

# Postavka 6
q6_tbl <- as.data.frame(prop.table(table(q6)))
plt_q6 <- ggplot(q6_tbl, aes(x = q6, y = Freq, fill = "red")) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(limits = c(0, 0.35)) +
  coord_flip() +
  xlab("Vrednost") +
  ylab("Delež odgovorov") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  ggtitle("Postavka 6")

# Postavka 7
q7_tbl <- as.data.frame(prop.table(table(q7)))
plt_q7 <- ggplot(q7_tbl, aes(x = q7, y = Freq, fill = "red")) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(limits = c(0, 0.35)) +
  coord_flip() +
  xlab("Vrednost") +
  ylab("Delež odgovorov") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  ggtitle("Postavka 7")

# Postavka 8
q8_tbl <- as.data.frame(prop.table(table(q8)))
plt_q8 <- ggplot(q8_tbl, aes(x = q8, y = Freq, fill = "red")) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(limits = c(0, 0.35)) +
  coord_flip() +
  xlab("Vrednost") +
  ylab("Delež odgovorov") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  ggtitle("Postavka 8")

all <- plot_grid(plt_q1, NULL, plt_q2, plt_q3, NULL, plt_q4, plt_q5, NULL, plt_q6, plt_q7, NULL, plt_q8, ncol = 3, align = "v", rel_widths = c(1, 0.1, 1,1, 0.05, 1,1, 0.05, 1,1, 0.05, 1,1, 0.05, 1))
save_plot("../../latex/figs/odgovori-po-postavkah.pdf", all, base_height = 12, base_width = 6)


# Demografske spremenljivke

# Spol
q24 <- as.numeric(gsub("=", "", data[, 24]))

# Starost
q25 <- as.numeric(gsub("=", "", data[, 25]))
q25 <- subset(q25, q25 > 0)
q25_tbl <- as.data.frame(prop.table(table(q25)))
plt_q25 <- ggplot(q25_tbl, aes(x = q25, y = Freq)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "cadetblue") +
  scale_y_continuous(limits = c(0, 0.70)) +
  coord_flip() +
  xlab("Vrednost") +
  ylab("Delež odgovorov") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")
ggsave("../../latex/figs/demo-starost.pdf", width = 6, height = 4)


# Struktura po poklicu
q26 <- as.numeric(gsub("=", "", data[, 26]))
q26 <- subset(q26, q26 > 0)
q26_tbl <- as.data.frame(prop.table(table(q26)))
plt_q26 <- ggplot(q26_tbl, aes(x = q26, y = Freq)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "cadetblue") +
  scale_y_continuous(limits = c(0, 0.4)) +
  coord_flip() +
  xlab("Vrednost") +
  ylab("Delež odgovorov") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")
ggsave("../../latex/figs/demo-izobrazba.pdf", width = 6, height = 4)

# Alkoholne pijače
q16 <- as.numeric(gsub("=", "", data[, 16]))
q16 <- subset(q16, q16 >= 0)
q16_tbl <- as.data.frame(q16)
plt_q16 <- ggplot(q16_tbl, aes(x = q16)) +
  geom_histogram(bins = 10, color = "black", fill = "darkolivegreen3") +
  labs(x = "Vrednost", y = "Frekvenca") +
  ggtitle("Alkohol 1")

q17 <- as.numeric(gsub("=", "", data[, 17]))
q16 <- subset(q17, q17 >= 0)
q17_tbl <- as.data.frame(q17)
plt_q17 <- ggplot(q17_tbl, aes(x = q17)) +
  geom_histogram(bins = 10, color = "black", fill = "darkolivegreen3") +
  labs(x = "Vrednost", y = "Frekvenca") +
  ggtitle("Alkohol 2")

q18 <- as.numeric(gsub("=", "", data[, 18]))
q18 <- subset(q18, q18 >= 0)
q18_tbl <- as.data.frame(q18)
plt_q18 <- ggplot(q18_tbl, aes(x = q18)) +
  geom_histogram(bins = 10, color = "black", fill = "darkolivegreen3") +
  labs(x = "Vrednost", y = "Frekvenca") +
  ggtitle("Alkohol 3")

all <- plot_grid(plt_q16, plt_q17, plt_q18, ncol = 3, align = "v")
ggsave("../../latex/figs/covar-alkohol.pdf", width = 12, height = 4)

# Kajenje
q19 <- as.numeric(gsub("=", "", data[, 19]))
q19 <- subset(q19, q19 >= 0)
q19_tbl <- as.data.frame(q19)
plt_q19 <- ggplot(q19_tbl, aes(x = q19)) +
  geom_histogram(bins = 10, color = "black", fill = "bisque2") +
  labs(x = "Vrednost", y = "Frekvenca") +
  ggtitle("Kajenje 1")

q20 <- as.numeric(gsub("=", "", data[, 20]))
q20 <- subset(q20, q20 >= 0)
q20_tbl <- as.data.frame(q20)
plt_q20 <- ggplot(q20_tbl, aes(x = q20)) +
  geom_histogram(bins = 10, color = "black", fill = "bisque2") +
  labs(x = "Vrednost", y = "Frekvenca") +
  ggtitle("Kajenje 2")

q21 <- as.numeric(gsub("=", "", data[, 21]))
q21 <- subset(q21, q21 >= 0)
q21_tbl <- as.data.frame(q21)
plt_q21 <- ggplot(q21_tbl, aes(x = q21)) +
  geom_histogram(bins = 10, color = "black", fill = "bisque2") +
  labs(x = "Vrednost", y = "Frekvenca") +
  ggtitle("Kajenje 3")

all <- plot_grid(plt_q19, plt_q20, plt_q21, ncol = 3, align = "v")
ggsave("../../latex/figs/covar-kajenje.pdf", width = 12, height = 4)

# Droge
q22 <- as.numeric(gsub("=", "", data[, 22]))
q22 <- subset(q22, q22 >= 0)
q22_tbl <- as.data.frame(q22)
plt_q22 <- ggplot(q22_tbl, aes(x = q22)) +
  geom_histogram(bins = 10, color = "black", fill = "darkorange") +
  labs(x = "Vrednost", y = "Frekvenca") +
  ggtitle("Droge 1")

q23 <- as.numeric(gsub("=", "", data[, 23]))
q23 <- subset(q23, q23 >= 0)
q23_tbl <- as.data.frame(q23)
plt_q23 <- ggplot(q23_tbl, aes(x = q23)) +
  geom_histogram(bins = 10, color = "black", fill = "darkorange") +
  labs(x = "Vrednost", y = "Frekvenca") +
  ggtitle("Droge 2")

all <- plot_grid(plt_q22, plt_q23, ncol = 2, align = "v")
ggsave("../../latex/figs/covar-droge.pdf", width = 8, height = 4)

# SSS postavke združimo v eno spremenljivko
ss1 <- as.numeric(gsub("=", "", data[, 8]))
ss2 <- as.numeric(gsub("=", "", data[, 9]))
ss3 <- as.numeric(gsub("=", "", data[, 10]))
ss4 <- as.numeric(gsub("=", "", data[, 11]))
ss5 <- as.numeric(gsub("=", "", data[, 12]))
ss6 <- as.numeric(gsub("=", "", data[, 13]))
ss7 <- as.numeric(gsub("=", "", data[, 14]))
ss8 <- as.numeric(gsub("=", "", data[, 15]))
sss <- ss1 + ss2 + ss3 + ss4 + ss5 + ss6 + ss7 + ss8
alko1 <- as.numeric(gsub("=", "", data[, 16]))
alko2 <- as.numeric(gsub("=", "", data[, 17]))
alko3 <- as.numeric(gsub("=", "", data[, 18]))
smok1 <- as.numeric(gsub("=", "", data[, 19]))
smok2 <- as.numeric(gsub("=", "", data[, 20]))
smok3 <- as.numeric(gsub("=", "", data[, 21]))
drug1 <- as.numeric(gsub("=", "", data[, 22]))
drug2 <- as.numeric(gsub("=", "", data[, 23]))
sex <- as.numeric(gsub("=", "", data[, 24]))
age <- as.numeric(gsub("=", "", data[, 25]))
edu <- as.numeric(gsub("=", "", data[, 26]))
tbl <- data.frame(ss1, ss2, ss3, ss4, ss5, ss6, ss7, ss8, sss, alko1, alko2, alko3, smok1, smok2, smok3, drug1, drug2, sex, age, edu)

# SSS total
plt_sss <- tbl %>% dplyr::select(sss) %>% 
  ggplot(aes(x = sss)) +
  geom_histogram(bins = 10, color = "black", fill = "cornflowerblue") +
  labs(x = "Vrednost", y = "Frekvenca")
ggsave("../../latex/figs/sss-total.pdf", width = 6, height = 4)

# Korelacijski diagrami med SSS in alkoholom
plt_a1 <- tbl %>% dplyr::select(sss, alko1) %>% 
  filter(alko1 >= 0) %>% 
  ggplot(aes(x = sss, y = alko1)) +
  geom_jitter(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_equal() +
  labs(x = "Iskanje dražljajev", y = "Alkohol 1") +
  theme(aspect.ratio = 1)

plt_a2 <- tbl %>% dplyr::select(sss, alko2) %>% 
  filter(alko2 >= 0) %>% 
  ggplot(aes(x = sss, y = alko2)) +
  geom_jitter(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_equal() +
  labs(x = "Iskanje dražljajev", y = "Alkohol 2") +
  theme(aspect.ratio = 1)

plt_a3 <- tbl %>% dplyr::select(sss, alko3) %>% 
  filter(alko3 >= 0) %>% 
  ggplot(aes(x = sss, y = alko3)) +
  geom_jitter(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_equal() +
  labs(x = "Iskanje dražljajev", y = "Alkohol 3") +
  theme(aspect.ratio = 1)

all <- plot_grid(plt_a1, plt_a2, plt_a3, ncol = 3, align = "vh")
ggsave("../../latex/figs/cor-sss-alko.pdf", width = 12, height = 4)

# Korelacijski diagrami med SSS in kajenjem
plt_s1 <- tbl %>% dplyr::select(sss, smok1) %>% 
  filter(smok1 >= 0) %>% 
  ggplot(aes(x = sss, y = smok1)) +
  geom_jitter(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_equal() +
  labs(x = "Iskanje dražljajev", y = "Kajenje 1") +
  theme(aspect.ratio = 1)

plt_s2 <- tbl %>% dplyr::select(sss, smok2) %>% 
  filter(smok2 >= 0) %>% 
  ggplot(aes(x = sss, y = smok2)) +
  geom_jitter(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_equal() +
  labs(x = "Iskanje dražljajev", y = "Kajenje 2") +
  theme(aspect.ratio = 1)

plt_s3 <- tbl %>% dplyr::select(sss, smok3) %>% 
  filter(smok3 >= 0) %>% 
  ggplot(aes(x = sss, y = smok3)) +
  geom_jitter(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_equal() +
  labs(x = "Iskanje dražljajev", y = "Kajenje 3") +
  theme(aspect.ratio = 1)

all <- plot_grid(plt_s1, plt_s2, plt_s3, ncol = 3, align = "vh")
ggsave("../../latex/figs/cor-sss-smok.pdf", width = 12, height = 4)


# Korelacijski diagrami med SSS in kajenjem
plt_d1 <- tbl %>% dplyr::select(sss, drug1) %>% 
  filter(drug1 >= 0) %>% 
  ggplot(aes(x = sss, y = drug1)) +
  geom_jitter(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_equal() +
  labs(x = "Iskanje dražljajev", y = "Droge 1") +
  theme(aspect.ratio = 1)

plt_d2 <- tbl %>% dplyr::select(sss, drug2) %>% 
  filter(drug2 >= 0) %>% 
  ggplot(aes(x = sss, y = drug2)) +
  geom_jitter(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_equal() +
  labs(x = "Iskanje dražljajev", y = "Droge 2") +
  theme(aspect.ratio = 1)

all <- plot_grid(plt_d1, plt_d2, ncol = 2, align = "vh")
ggsave("../../latex/figs/cor-sss-drug.pdf", width = 8, height = 4)

# Korelacijske matrike

# SS - Alko
tbl_ss_alko <- tbl %>%
  dplyr::select(ss1,ss2,ss3,ss4,ss5,ss6,ss7,ss8,alko1,alko2,alko3) %>% 
  mutate(m = do.call(pmin, .)) %>% 
  filter(m >= 0) %>% 
  dplyr::select(-m) %>% 
  as_data_frame() %>% 
  mutate_at(vars(ss1,ss2,ss3,ss4,ss5,ss6,ss7,ss8), factor, ordered = TRUE) %>% 
  as.data.frame()
r1 <- corstarsl(tbl_ss_alko)
xtable(r1)

# SS - Smok
tbl_ss_smok <- tbl %>%
  dplyr::select(ss1,ss2,ss3,ss4,ss5,ss6,ss7,ss8,smok1,smok2,smok3) %>% 
  mutate(m = do.call(pmin, .)) %>% 
  filter(m >= 0) %>% 
  dplyr::select(-m) %>% 
  as_data_frame() %>% 
  mutate_at(vars(ss1,ss2,ss3,ss4,ss5,ss6,ss7,ss8), factor, ordered = TRUE) %>% 
  as.data.frame()
r2 <- corstarsl(tbl_ss_smok)
xtable(r2)

# SS - Drug
tbl_ss_drug <- tbl %>%
  dplyr::select(ss1,ss2,ss3,ss4,ss5,ss6,ss7,ss8,drug1,drug2) %>% 
  mutate(m = do.call(pmin, .)) %>% 
  filter(m >= 0) %>% 
  dplyr::select(-m) %>% 
  as_data_frame() %>% 
  mutate_at(vars(ss1,ss2,ss3,ss4,ss5,ss6,ss7,ss8), factor, ordered = TRUE) %>% 
  as.data.frame()

r3 <- corstarsl(tbl_ss_drug)
xtable(r3)

# SS - Demo
tbl_ss_demo <- tbl %>%
  dplyr::select(ss1,ss2,ss3,ss4,ss5,ss6,ss7,ss8,sex,age,edu) %>% 
  mutate(m = do.call(pmin, .)) %>% 
  filter(m >= 0) %>% 
  dplyr::select(-m) %>% 
  as_data_frame() %>% 
  mutate_at(vars(ss1,ss2,ss3,ss4,ss5,ss6,ss7,ss8,age,edu), factor, ordered = TRUE) %>%
  mutate_at(vars(sex), factor) %>% 
  as.data.frame()

r4 <- corstarsl(tbl_ss_demo)
xtable(r4)

# Preverjanje domnev

# Alkohol
tbl_alko <- tbl %>%
  dplyr::select(-alko1,-alko3,-smok1,-smok3,-drug2) %>% 
  mutate(m = do.call(pmin, .)) %>% 
  filter(m >= 0) %>% 
  dplyr::select(-m) %>%
  mutate(alko = ifelse(alko2 > 0, 1, 0),
         smoke = ifelse(smok2 > 0, 1, 0),
         drug = ifelse(drug1 > 0, 1, 0)) %>% 
  dplyr::select(-alko2,-smok2,-drug1,-sex,-age,-edu,-smoke,-drug) %>% 
  group_by(alko) %>% 
  summarise_all(funs(mean))
tbl_alko_melt <- melt(tbl_alko, id = 'alko')

plt_alko <- ggplot(tbl_alko_melt, aes(x = variable, y = value, group = factor(alko), fill = factor(alko))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Alkohol", breaks=c(0, 1), labels=c("Trezni", "Pijani")) +
  scale_x_discrete(labels = c("Dis-1", "ES-1", "TAS-1", "ES-2", "TAS-2", "BS-1", "Dis-2", "BS-2", "Skupaj")) +
  labs(x = "Podlestvica", y = "Povprečna vrednost") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
ggsave("../../latex/figs/sss-alko.pdf", width = 6, height = 4)
ggsave("../../latex/figs/sss-alko-pres.pdf", width = 10, height = 8)

# Kajenje
tbl_smoke <- tbl %>%
  dplyr::select(-alko1,-alko3,-smok1,-smok3,-drug2) %>% 
  mutate(m = do.call(pmin, .)) %>% 
  filter(m >= 0) %>% 
  dplyr::select(-m) %>%
  mutate(alko = ifelse(alko2 > 0, 1, 0),
         smoke = ifelse(smok2 > 0, 1, 0),
         drug = ifelse(drug1 > 0, 1, 0)) %>% 
  dplyr::select(-alko2,-smok2,-drug1,-sex,-age,-edu,-alko,-drug) %>% 
  group_by(smoke) %>% 
  summarise_all(funs(mean))
tbl_smoke_melt <- melt(tbl_smoke, id = 'smoke')

plt_smoke <- ggplot(tbl_smoke_melt, aes(x = variable, y = value, group = factor(smoke), fill = factor(smoke))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Kajenje", breaks=c(0, 1), labels=c("Nekadilci", "Kadilci")) +
  scale_x_discrete(labels = c("Dis-1", "ES-1", "TAS-1", "ES-2", "TAS-2", "BS-1", "Dis-2", "BS-2", "Skupaj")) +
  labs(x = "Podlestvica", y = "Povprečna vrednost") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
ggsave("../../latex/figs/sss-smoke.pdf", width = 6, height = 4)

# Droge
tbl_drug <- tbl %>%
  dplyr::select(-alko1,-alko3,-smok1,-smok3,-drug2) %>% 
  mutate(m = do.call(pmin, .)) %>% 
  filter(m >= 0) %>% 
  dplyr::select(-m) %>%
  mutate(alko = ifelse(alko2 > 0, 1, 0),
         smoke = ifelse(smok2 > 0, 1, 0),
         drug = ifelse(drug1 > 0, 1, 0)) %>% 
  dplyr::select(-alko2,-smok2,-drug1,-sex,-age,-edu,-alko,-smoke) %>% 
  group_by(drug) %>% 
  summarise_all(funs(mean))
tbl_drug_melt <- melt(tbl_drug, id = 'drug')

plt_drug <- ggplot(tbl_drug_melt, aes(x = variable, y = value, group = factor(drug), fill = factor(drug))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Droge", breaks=c(0, 1), labels=c("Neuživalci", "Uživalci")) +
  scale_x_discrete(labels = c("Dis-1", "ES-1", "TAS-1", "ES-2", "TAS-2", "BS-1", "Dis-2", "BS-2", "Skupaj")) +
  labs(x = "Podlestvica", y = "Povprečna vrednost") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
ggsave("../../latex/figs/sss-drug.pdf", width = 6, height = 4)

# Spol
tbl_sex <- tbl %>%
  dplyr::select(-alko1,-alko2,-alko3,-smok1,-smok2,-smok3,-drug1,-drug2) %>% 
  mutate(m = do.call(pmin, .)) %>% 
  filter(m >= 0) %>% 
  dplyr::select(-m) %>%
  dplyr::select(-age,-edu) %>% 
  group_by(sex) %>% 
  summarise_all(funs(mean))
tbl_sex_melt <- melt(tbl_sex, id = 'sex')

plt_sex <- ggplot(tbl_sex_melt, aes(x = variable, y = value, group = factor(sex), fill = factor(sex))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Spol", breaks=c(1, 2), labels=c("Moški", "Ženski")) +
  scale_x_discrete(labels = c("Dis-1", "ES-1", "TAS-1", "ES-2", "TAS-2", "BS-1", "Dis-2", "BS-2", "Skupaj")) +
  labs(x = "Podlestvica", y = "Povprečna vrednost") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
ggsave("../../latex/figs/sss-sex.pdf", width = 6, height = 4)


# Starost
tbl_age <- tbl %>%
  dplyr::select(-alko1,-alko2,-alko3,-smok1,-smok2,-smok3,-drug1,-drug2) %>% 
  mutate(m = do.call(pmin, .)) %>% 
  filter(m >= 0) %>% 
  dplyr::select(-m) %>%
  dplyr::select(-sex,-edu) %>% 
  group_by(age) %>% 
  summarise_all(funs(mean))
tbl_age_melt <- melt(tbl_age, id = 'age')

plt_age <- ggplot(tbl_age_melt, aes(x = variable, y = value, group = factor(age), fill = factor(age))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Starost", breaks=c(1, 2, 3, 4), labels=c("do 20 let", "21-40 let", "41-60 let", "61 let ali več")) +
  scale_x_discrete(labels = c("Dis-1", "ES-1", "TAS-1", "ES-2", "TAS-2", "BS-1", "Dis-2", "BS-2", "Skupaj")) +
  labs(x = "Podlestvica", y = "Povprečna vrednost") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
ggsave("../../latex/figs/sss-age.pdf", width = 6, height = 4)



mod <- lm(sss ~ alko1 + smok1 + drug1 + sex + age + edu, data = tbl)
summary(mod)

# Confirmatory factor analysis

library(lavaan)
library(semPlot)
# Baseline model
HS.model_1 <- 'sss =~ ss1 + ss2 + ss3 + ss4 + ss5 + ss6 + ss7 + ss8'
# Continuous variables
fit_1_con <- cfa(HS.model_1, data = bla1, estimator = "DWLS")
summary(fit_1_con, fit.measures = TRUE)
modindices(fit_1_con, sort. = TRUE)
fitMeasures(fit_1_con, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
# Categorical variables
fit_1_cat <- cfa(HS.model_1, data = bla1, estimator = "DWLS", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
fitMeasures(fit_1_cat, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
summary(fit_1_cat,fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)
modindices(fit_1_cat, sort. = TRUE)

# Model 1
HS.model_2 <- 'sss =~ ss1 + ss2 + ss3 + ss4 + ss5 + ss6 + ss7 + ss8
               ss2 ~~ ss4'
# Continuous variables
fit_2_con <- cfa(HS.model_2, data = bla1, estimator = "DWLS")
summary(fit_2_con, fit.measures = TRUE)
modindices(fit_2_con, sort. = TRUE)
fitMeasures(fit_2_con, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
# Categorical variables
fit_2_cat <- cfa(HS.model_2, data = bla1, estimator = "DWLS", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
summary(fit_2_cat, fit.measures = TRUE)
modindices(fit_2_cat, sort. = TRUE)
fitMeasures(fit_2_cat, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

# Model B
HS.model_3 <- 'sss =~ ss1 + ss2 + ss3 + ss4 + ss5 + ss6 + ss7 + ss8
               ss2 ~~ ss4
               ss2 ~~ ss3'
fit_3_cat <- cfa(HS.model_3, data = bla1, estimator = "DWLS", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"))
summary(fit_3_cat, fit.measures = TRUE)
modindices(fit_3_cat, sort. = TRUE)
fitMeasures(fit_3_cat, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

inspect(fit_2_cat, what = "std")

# Model C
HS.model_4 <- 'tas =~ a * ss3 + a * ss5
               es =~ a * ss2 + a * ss4
               dis =~ a * ss1 + a * ss7
               bs =~ a * ss6 + a * ss8
               sss =~ a * tas
               sss =~ a * es
               sss =~ a * dis
               sss =~ a * bs'
fit_4_cat <- cfa(HS.model_4, data = bla1, estimator = "DWLS", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
summary(fit_4_cat, fit.measures = TRUE)
modindices(fit_4_cat, sort. = TRUE)

# Tests of measurement invariance
# The configural model is a prerequisite to investigating measurement invariance

library(semTools)
####################################################################################################################################
# For Model 2
# For sex groups
# Models for continuous variables (OK)
configural <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex")
weak <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = "loadings")
strong <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts"))
strict <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts", "residuals"))
anova(configural, weak, strong, strict)
fitMeasures(configural, fit.measures = "all")
fitMeasures(weak, fit.measures = "all")
fitMeasures(strong, fit.measures = "all")
fitMeasures(strict, fit.measures = "all")
# Models for categorical variables (OK) <-- USE THIS ONE
measurementInvarianceCat(model = HS.model_2, data = bla1, estimator = "DWLS", group = "sex", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), strict = TRUE, parameterization = "theta")
configural <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
weak <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = "loadings", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("sss =~ ss5"))
strong <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("sss =~ ss5", "ss1 | t1"))
strict <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts", "residuals"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("sss =~ ss5"))
anova(configural, weak, strong, strict)
fitMeasures(configural, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(weak, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(strong, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(strict, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))

View(lavTestScore(strong)$uni %>% arrange(desc(X2)))
View(parTable(strong))

# For age groups
# Models for categorical variables
measurementInvariance(model = HS.model_2, data = bla1, group = "starost", estimator = "DWLS", strict = TRUE)
# Models for categorical variables (OK)
measurementInvarianceCat(model = HS.model_2, data = bla1, group = "starost", estimator = "DWLS", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), strict = TRUE, parameterization = "theta")
configural <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
weak <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = "loadings", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
strong <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("ss2 | t2", "ss2 | t1", "sss =~ ss5", "ss2 | t3", "sss =~ ss2", "ss5 | t2", "ss5 | t1", "sss =~ ss4"))
strict <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts", "residuals"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("ss2 | t2", "ss2 | t1", "sss =~ ss5", "ss2 | t3", "sss =~ ss2", "ss5 | t2", "ss5 | t1", "sss =~ ss4"))
anova(configural, weak, strong, strict)
fitMeasures(configural, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(weak, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(strong, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(strict, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
####################################################################################################################################
# For Model 3
# For sex groups
# Models for continuous variables (OK)
configural <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "sex")
weak <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "sex", group.equal = "loadings")
strong <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts"))
strict <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts", "residuals"))
anova(configural, weak, strong, strict)
fitMeasures(configural, fit.measures = "all")
fitMeasures(weak, fit.measures = "all")
fitMeasures(strong, fit.measures = "all")
fitMeasures(strict, fit.measures = "all")

View(lavTestScore(weak_invariance)$uni %>% arrange(desc(X2)))
View(parTable(weak_invariance))

# Models for categorical variables
configural <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "sex", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
weak <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "sex", group.equal = "loadings", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("sss =~ ss5"))
strong <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("sss =~ ss5"))
strict <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts", "residuals"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("sss =~ ss5"))
anova(configural, weak, strong, strict)
fitMeasures(configural, fit.measures = "all")
fitMeasures(weak, fit.measures = "all")
fitMeasures(strong, fit.measures = "all")
fitMeasures(strict, fit.measures = "all")

# For age groups
# Models for continuous variables (OK)
configural <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "starost")
weak_invariance <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "starost", group.equal = "loadings")
strong_invariance <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts"), group.partial = c("ss2 ~ 1", "ss5 ~ 1", "ss1 ~ 1"))
strict_invariance <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts", "residuals"), group.partial = c("ss2 ~ 1", "ss5 ~ 1", "ss1 ~ 1", "sss =~ ss3"))
anova(configural, weak_invariance, strong_invariance, strict_invariance)
fitMeasures(configural, fit.measures = "all")

# Models for categorical variables (OK)
configural <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "starost", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
weak_invariance <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "starost", group.equal = "loadings", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
strong_invariance <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("ss2 | t2", "sss =~ s5", "ss2 | t3", "ss2 | t1", "sss ~ ss2", "sss =~ ss4", "ss5 | t2", "ss5 | t1"))
strict_invariance <- cfa(HS.model_3, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts", "residuals"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("ss2 | t2", "sss =~ s5", "ss2 | t3", "ss2 | t1", "sss ~ ss2", "sss =~ ss4", "ss5 | t2", "ss5 | t1"))
anova(configural, weak_invariance, strong_invariance, strict_invariance)
fitMeasures(configural, fit.measures = "all")

lavTestScore(strong_invariance)
View(parTable(strong_invariance))
# fitMeasures(weak_invariance, fit.measures = c("chisq", "df", "pvalue", "rmsea", "rmsea.pvalue"))
fitMeasures(configural, fit.measures = "all")

library(ltm)
library(psych)

sss_df <- bla1 %>% 
  select(ss1:ss8)

# Constrained discrimination
fit1 <- grm(sss_df, constrained = TRUE)
# Unconstrained discrimination
fit2 <- grm(sss_df, constrained = FALSE)
# Which model is better
anova(fit1, fit2)

# Item response category characteristic curve
# Show how the probability of responding in the k-th category, in each item, changes with the values of the latent variabl
plot(fit2)

# Item information curve
# How well and precisely each item measures the latent trait at various levels of the attribute
plot(fit2, type = "IIC", legend = TRUE)

# Test information function curve
# Aggregates the Item Information Curves across all the items
iic_data <- plot(fit2, type = "IIC", items = 0) %>% as_tibble()

plt1 <- ggplot(iic_data, aes(x = z, y = test.info)) +
  geom_line() +
  labs(x = "Sensation seeking", y = "Information") +
  theme_bw()
ggsave("./tmp.pdf", plt1, width = 5, height = 3.5)
system(paste("pdfcrop", "./tmp.pdf", "./information.pdf"))
system("rm ./tmp.pdf")


library(broom)
library(effsize)
# Spol
tbl %>%
  dplyr::select(starts_with("ss"), sex) %>% 
  mutate(m = do.call(pmin, .)) %>% 
  filter(m >= 0) %>% 
  dplyr::select(-m) %>%
  group_by(sex) %>% 
  summarise_all(funs(mean))

# Compute descriptives for total al grouped by sex
tbl %>%
  dplyr::select(starts_with("ss"), sex) %>% 
  mutate(m = do.call(pmin, .)) %>% 
  filter(m >= 0) %>% 
  dplyr::select(-m) %>%
  group_by(sex) %>% 
  summarise_all(funs(sd)) %>% 
  round(digits = 2)

# Compute t.test for all variables
gather(bla, var, val, -sex) %>% 
  group_by(sex, var) %>% 
  summarise(value = list(val)) %>% 
  spread(sex, value) %>% 
  group_by(var) %>% 
  mutate(t_value = t.test(unlist(`1`), unlist(`2`))$statistic,
         p_value = t.test(unlist(`1`), unlist(`2`))$p.value)
  

cohen.d(sss~ as.factor(sex), data = bla, hedges.correction = TRUE)


# Multivariate normality
library(MVN)
mvn(data = sss_df, mvnTest = "hz")


# ANOVA for sex and age differences
library(ggpubr)
library(car)

bla <- tibble(sss, sex, age) %>% 
  mutate(m = do.call(pmin, .)) %>% 
  filter(m >= 0) %>% 
  dplyr::select(-m) %>% 
  mutate(starost = factor(recode(age, `2` = 2, `3` = 2, `4` = 2)),
         spol = factor(sex))

ggboxplot(bla, x = "starost", y = "sss", color = "spol")
ggline(bla3, x = "starost", y = "sss", color = "sex", add = c("mean_se"))



res_aov <- aov(sss ~ sex * starost, data = bla1)
summary(res_aov)

bla %>%
  group_by(spol, starost) %>% 
  summarise(
    n = n(),
    mean = mean(sss),
    sd = sd(sss))

# Homogeneity of variance assumption
plot(res_aov, 1)
leveneTest(sss ~ spol * starost, data = bla)

# Normality assumption
plot(res_aov, 2)
shapiro.test(residuals(object = res_aov))

bla3 <- bla1 %>% 
  mutate(sex = as.factor(sex),
         starost = as.factor(starost))

# We have unbalanced design because of different number of participant in each group
my_anova <- lm(sss ~ sex * starost, data = bla3, contrasts = list(starost = contr.sum, sex = contr.sum))
Anova(my_anova, type = "III")


# Regression analysis
tbl %>%
  mutate_each(funs(replace(., . < 0, NA))) %>% 
  summarise_all(funs(sum(is.na(.))))

# For SPSS export
bla1 <- tbl %>%
  mutate_at(vars(alko1:edu), funs(dplyr::recode(., `-2` = 0, `-3` = 999)))

##############################################################
### THIS IS VERY IMPORTANT
### Od tu skok na Outlier detection
##############################################################
# For lm object -> skok na CFA
bla1 <- tbl %>%
  mutate_at(vars(alko1:edu), funs(dplyr::recode(., `-2` = 0, `-3` = NA_real_))) %>% 
  # mutate_at(vars(ss1:ss8), funs(ordered)) %>% 
  # replace_na(list(sex = 2))
  na.omit() %>% 
  mutate(starost = factor(recode(age, `2` = 2, `3` = 2, `4` = 2)))




library(lm.beta)
fit <- lm(sss ~ alko1 + alko3 + smok1 + smok3 + drug1 + drug2, data = bla1)
summary(lm.beta(fit))
round(c(-0.154170, 0.249927, 0.160015, 0.176540, 0.033576, -0.054507, 0.001731, 0.160390), 2)

library(car)
vif(fit)

influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

library(glmnet)
bla2 <- bla1 %>% dplyr::select(sss, alko1, alko3, smok1, smok3, drug1, drug2)
cooksd <- cooks.distance(fit)
bla3 <- bla2[cooksd < 4 * mean(cooksd, na.rm = T), ]


lambdas <- 10^seq(3, -2, by = -.1)
fit <- glmnet(x = as.matrix(bla2[, 2:7]), y = bla2$sss, alpha = 0, lambda = lambdas)

library(ridge)
summary(linearRidge(sss ~ ., data = bla2))

library(lmridge)
summary(lmridge(sss ~ ., data = bla2))

# Outliers
library(mvoutlier)
res <- dd.plot(bla2[, 1:8])

outliers <- do.call(cbind, res) %>%
  as_tibble() %>%
  mutate(id = 1:nrow(.)) %>% 
  arrange(desc(md.cla)) %>% 
  head(5) %>% 
  pull(id)

bla3 <- bla2[-outliers, ]

# Multivariate normality
library(MVN)
mvn(data = bla1[, 1:8], mvnTest = "hz")

# LAST VERSION - GENDER
configural <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
weak <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = "loadings", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("sss=~ss5"))
strong <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
strict <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts", "residuals"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("sss=~ss5"))
anova(configural, weak, strong, strict)
fitMeasures(configural, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(weak, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(strong, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(strict, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))

# LAST VERSION - AGE
configural <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
weak <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = "loadings", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
strong <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
strong_par <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("ss2 | t2", "ss2 | t3", "ss2 | t1", "sss =~ ss5"))
strict <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts", "residuals"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
strict_par <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts", "residuals"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("ss2 | t2", "ss2 | t3", "sss =~ ss2", "sss =~ ss5"))
anova(configural, weak, strong_par, strict_par)
fitMeasures(configural, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(weak, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(strong_par, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(strict_par, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))

View(lavTestScore(strict)$uni %>% arrange(desc(X2)))
View(parTable(strict))

# Table 3
# Spol
bla1 %>%
  dplyr::select(starts_with("ss"), sex) %>% 
  summarise_all(funs(sd)) %>% 
  round(digits = 2)

bla1 %>%
  dplyr::select(starts_with("ss"), sex) %>% 
  group_by(sex) %>% 
  summarise_all(funs(sd))


# Compute t.test for all variables
bla1 %>% 
  dplyr::select(starts_with("ss"), sex) %>% 
  gather(var, val, -sex) %>% 
  group_by(sex, var) %>% 
  summarise(value = list(val)) %>% 
  spread(sex, value) %>% 
  group_by(var) %>% 
  mutate(t_value = t.test(unlist(`1`), unlist(`2`))$statistic,
         p_value = t.test(unlist(`1`), unlist(`2`))$p.value)

library(effsize)
cohen.d(ss5~ as.factor(sex), data = bla1, hedges.correction = TRUE)


# Starost
bla1 %>%
  dplyr::select(starts_with("ss"), starost) %>% 
  group_by(as.numeric(starost)) %>% 
  summarise_all(funs(sd)) %>% 
  round(digits = 2)

bla1 %>% 
  dplyr::select(starts_with("ss"), starost) %>% 
  gather(var, val, -starost) %>% 
  group_by(starost, var) %>% 
  summarise(value = list(val)) %>% 
  spread(starost, value) %>% 
  group_by(var) %>% 
  mutate(t_value = t.test(unlist(`1`), unlist(`2`))$statistic,
         p_value = t.test(unlist(`1`), unlist(`2`))$p.value)


cohen.d(sss ~ as.factor(starost), data = bla1, hedges.correction = TRUE)


library(psych)
bla1 %>% 
  dplyr::select(ss1:ss8) %>% 
  alpha()

foo <- bla1 %>% 
  dplyr::select(ss1:ss8) %>% 
  mutate_if(is.numeric, factor, ordered = TRUE)



pc <- hetcor(foo, ML = FALSE)
faPC <- fa(r=pc$correlations, nfactors=4, n.obs=363, rotate="oblimin")
print(faPC, digits=2, cut=.4, sort=TRUE)


library(IsingFit)
library(qgraph)

full_m <- cor_auto(foo)
full <- qgraph(full_m, layout = "spring", graph = "glasso", sampleSize = nrow(foo), cut = 0)



#############################################################
#############################################################
# Gender
configural <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
weak <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = "loadings", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
anova(weak, configural)
lavtestScore(weak)
weak_p <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = "loadings", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("sss=~ss5"))
anova(weak_p, configural)

strong <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
anova(strong, weak_p)

strict <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts", "residuals"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
strict_p <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts", "residuals"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("sss=~ss5"))
anova(strict_p, strong)

anova(configural, weak_p, strong, strict_p)

fitMeasures(configural, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(weak_p, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(strong, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))
fitMeasures(strict_p, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi"))

# Age
configural <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
weak <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = "loadings", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
anova(weak, configural)
strong <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
anova(strong, weak)
lavTestScore(strong)
parTable(strong)
strong_p <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("ss2|t2", "ss2|t1", "sss=~ss5", "ss2|t3", "sss=~ss2", "ss5|t2", "ss5|t1", "sss=~ss4"))
anova(strong_p, weak)
strict <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts", "residuals"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
anova(strict, strong_p)
lavTestScore(strict)
parTable(strict)
strict_p <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts", "residuals"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("sss=~ss2", "ss2|t2", "ss2|t3", "sss=~ss5", "ss5|t2", "ss5|t1"))
anova(strict_p, strong_p)


## Old data analysis

data2 <- read_tsv("/home/andrej/Documents/bsss-8/SSS_surovi.txt") %>%
    rename(SSS1 = A1, SSS3 = A3, SSS5 = A5, SSS6 = A6, SSS8 = A8, SSS9 = A9, SSS14 = A14, SSS16 = A16, SSS17 =  A17,
           SSS18 = A18, SSS22 = A22, SSS23 = A23, SSS24 = A24, SSS28 = A28, SSS29 = A29, SSS32 = A32, SSS34 = A34,
           SSS36 = A36, SSS39 = A39) %>%
    rename(SSS2 = A2, SSS4 = A4, SSS7 = A7, SSS10 = A10,  SSS11 = A11, SSS12 = A12, SSS13 = A13, SSS15 = A15,
           SSS19 = A19, SSS20 = A20, SSS21 = A21, SSS25 = A25, SSS26 = A26, SSS27 = A27, SSS30 = A30, SSS31 = A31,
           SSS33 = A33, SSS35 = A35, SSS37 = A37, SSS38 = A38, SSS40 = A40) %>%
    mutate_at(.vars = vars(SSS1, SSS3, SSS5, SSS6, SSS8, SSS9, SSS14, SSS16, SSS17, SSS18, SSS22, SSS23, SSS24,
                           SSS28, SSS29, SSS32, SSS34, SSS36, SSS39), .funs = funs(recode(., "A" = 1, "B" = 0))) %>%
    mutate_at(.vars = vars(SSS2, SSS4, SSS7, SSS10, SSS11, SSS12, SSS13, SSS15, SSS19, SSS20, SSS21, SSS25, SSS26,
                           SSS27, SSS30, SSS31, SSS33, SSS35, SSS37, SSS38, SSS40), .funs = funs(recode(., "A" = 0, "B" = 1))) %>%
    dplyr::select(SSS1, SSS6, SSS11, SSS18, SSS23, SSS24, SSS25, SSS27) %>%
    mutate(SSS = SSS1 + SSS6 + SSS11 + SSS18 + SSS23 + SSS24 + SSS25 + SSS27)

data <- data %>%
    dplyr::select(1:8) %>%
    setNames(paste0("i", 1:8))
    



m1 <- 'f1 =~ SSS1 + SSS6 + SSS11 + SSS18 + SSS23 + SSS24 + SSS25 + SSS27'
# Continuous variables
fit1 <- cfa(m1, data = data, estimator = "DWLS", ordered = c("SSS1", "SSS6", "SSS11", "SSS18", "SSS23", "SSS24", "SSS25", "SSS27"))
summary(fit1, fit.measures = TRUE)
modindices(fit1, sort. = TRUE)


m2 <- 'SSS =~ SSS1 + SSS6 + SSS11 + SSS18 + SSS23 + SSS24 + SSS25 + SSS27
       SSS6 ~~ SSS18'
fit2 <- cfa(m2, data = data, estimator = "DWLS", ordered = c("SSS1", "SSS6", "SSS11", "SSS18", "SSS23", "SSS24", "SSS25", "SSS27"))
summary(fit2, fit.measures = TRUE)
modindices(fit2, sort. = TRUE)








library(lavaan)
library(semTools)





all_results <- matrix(NA, nrow = 3, ncol = 6)



mod_cat <- 'f1 =~ ss1 + ss2 + ss3 + ss4 + ss5 + ss6 + ss7 + ss8'
model1 <- cfa(mod_cat, data = bla2, std.lv = TRUE, estimator = "MLR")
summary(model1, fit.measures = TRUE, standardized = TRUE)
resid(model1, type = "normalized")$cov
modificationindices(model1, standardized = TRUE,sort. = TRUE) %>% slice(1:10)

mod_cat <- 'f1 =~ ss1 + ss2 + ss3 + ss4 + ss5 + ss6 + ss7 + ss8
            ss2 ~~ ss4
            ss2 ~~ ss3'
model1 <- cfa(mod_cat, data = bla2, std.lv = TRUE, estimator = "MLR")
summary(model1, fit.measures = TRUE, standardized = TRUE)
resid(model1, type = "normalized")$cov
modificationindices(model1, standardized = TRUE,sort. = TRUE) %>% slice(1:10)
fitmeasures(model1, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))



## Baseline model
baseline <- measEq.syntax(configural.model = mod_cat, data = bla2,
                          ordered = c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"),
                          parameterization = "delta", ID.fac = "std.lv",
                          ID.cat = "Wu.Estabrook.2016")
model_baseline <- as.character(baseline)

fit_baseline <- cfa(model_baseline, data = bla2,
                    ordered = c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"))
round(data.matrix(fitmeasures(fit_baseline, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits = 3)





baseline <- measEq.syntax(configural.model = mod_cat, #data = bla2,
                          ordered = c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"),
                          parameterization = "delta", ID.fac = "std.lv",
                          ID.cat = "Wu.Estabrook.2016", group = "starost", group.equal = "configural")
model_baseline <- as.character(baseline)

fit_baseline <- cfa(model_baseline, data = bla2, group = "sex",
                    ordered = c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"))
all_results[1, ] <- round(data.matrix(fitmeasures(fit_baseline, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits = 3)




mod_cat <- 'f1 =~ ss1 + ss2 + ss3 + ss4 + ss5 + ss6 + ss7 + ss8'



mod_cat <- 'f1 =~ ss1 + ss2 + ss3 + ss4 + ss5 + ss6 + ss7 + ss8
            ss2 ~~ ss4
            ss2 ~~ ss3' 
baseline_part <- measEq.syntax(configural.model = mod_cat,
                       data = dat,        
                       ordered =  c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"),
                       parameterization = "delta",
                       ID.fac = "std.lv",
                       ID.cat = "Wu.Estabrook.2016",
                       group = "sex",
                       group.equal = "configural")

model_baseline_part <- as.character(baseline_part)
fit_baseline_part <- cfa(model_baseline_part, data = dat, group = "sex",
                 ordered = c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"))
round(data.matrix(fitmeasures(fit_baseline_part, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits = 3)




## Threshold invariance
prop4 <- measEq.syntax(configural.model = mod_cat,
                       data = dat,
                       ordered =  c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"),
                       parameterization = "delta",
                       ID.fac = "std.lv",
                       ID.cat = "Wu.Estabrook.2016",
                       group = "sex",
                       group.equal = c("thresholds"))

model_prop4 <- as.character(prop4)
fit_prop4 <- cfa(model_prop4, data = dat, group = "sex",
                 ordered = c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"))
round(data.matrix(fitmeasures(fit_prop4, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits = 3)

lavTestLRT(fit_baseline, fit_prop4)

prop7 <- measEq.syntax(configural.model = mod_cat,
                       data = dat,
                       ordered =  c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"),
                       parameterization = "delta",
                       ID.fac = "std.lv",
                       ID.cat = "Wu.Estabrook.2016",
                       group = "sex",
                       group.equal = c("thresholds", "loadings"))
model_prop7 <- as.character(prop7)
fit_prop7 <- cfa(model_prop7, data = dat, group = "sex",
                 ordered = c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"))
round(data.matrix(fitmeasures(fit_prop7, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits = 3)

lavTestLRT(fit_prop4, fit_prop7)

mi <- modindices(fit_prop4, free.remove = FALSE)
mi[mi$op == "=~", ]
prop7_part <- measEq.syntax(configural.model = mod_cat, data = bla2,
                       ordered =  c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"),
                       parameterization = "delta",
                       ID.fac = "std.lv",
                       ID.cat = "Wu.Estabrook.2016",
                       group = "sex",
                       group.equal = c("thresholds", "loadings"),
                       group.partial = "sss =~ ss4")
model_prop7_part <- as.character(prop7_part)
fit_prop7_part <- cfa(model_prop7_part, data = bla1, group = "sex",
                      ordered = c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"),
                      group.partial = "sss =~ ss4")
all_results[4, ] <- round(data.matrix(fitmeasures(fit_prop7_part, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits = 3)



bla1 %>%
    select(ss1, ss2, ss3, ss4, ss5, ss6, ss7, ss8, sex, starost) %>%
    write_csv("data.csv", col_names = FALSE)


dat <- read_csv("data.csv", col_names = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8", "sex", "age"))
dat <- read.table("data.csv", header = FALSE, sep = ",")
names(dat) <- c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8", "sex", "age")

group_col <- 9
nsam <- 100
data_list <- list()

id <- dat[, group_col]
min_n <- min(table(id))
max_n <- max(table(id))
ids <- split(seq_along(id), id)
for (i in seq_len(nsam)) {
  new_id <- unlist(lapply(ids, sample, min_n))
  dat_sam <- dat[new_id, ]
  data_list[[i]] <- dat_sam
}

library(simsem)

simout1 <- sim(nRep = NULL, model = mod_cat, rawData = data_list, group = "age", lavaanfun = "cfa", group.equal = "loadings")
summary(simout1)

simout2 <- sim(nRep = NULL, model = mod_cat, rawData = data_list, group = "age", lavaanfun = "cfa", group.equal = c("loadings", "intercepts"))
summary(simout2)

simout3 <- sim(nRep = NULL, model = mod_cat, rawData = data_list, group = "age", lavaanfun = "cfa", group.equal = c("loadings", "intercepts", "residuals"))
summary(simout3)



# Gender
configural <- cfa(mod_cat, data = dat, group = "sex", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "delta")
weak <- cfa(mod_cat, data = dat, estimator = "DWLS", group = "sex", group.equal = "loadings", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
anova(weak, configural)
lavtestScore(weak)
weak_p <- cfa(HS.model_2, data = bla1, estimator = "DWLS", group = "sex", group.equal = "loadings", ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta", group.partial = c("sss=~ss5"))
anova(weak_p, configural)
strong <- cfa(mod_cat, data = dat, estimator = "DWLS", group = "sex", group.equal = c("loadings", "intercepts"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")
strict <- cfa(mod_cat, data = bla2, estimator = "DWLS", group = "starost", group.equal = c("loadings", "intercepts", "residuals"), ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"), parameterization = "theta")

lavTestScore(weak)
parTable(weak)

fitMeasures(configural, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr"))
fitMeasures(weak, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr"))
fitMeasures(strong, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr"))
fitMeasures(strict, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr"))

anova(configural, weak, strong, strict)

measurementInvarianceCat(model = mod_cat, data = dat, group = "age", strict = T,
                         ordered = c("ss1", "ss2", "ss3", "ss4", "ss5", "ss6", "ss7", "ss8"),
                         estimator="WLSMV", method="satorra.bentler.2001",
                         parameterization = "theta")


measurementInvariance(model = mod_cat, data = bla3, group = "sex", strict = T)









baseline <- measEq.syntax(configural.model = mod_cat, #data = bla2,
                          ordered = c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"),
                          parameterization = "delta", ID.fac = "std.lv",
                          ID.cat = "Wu.Estabrook.2016", group = "sex", group.equal = "configural")
model_baseline <- as.character(baseline)
fit_baseline <- cfa(model_baseline, data = bla1, group = "sex",
                    ordered = c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"))


compareFit(config = configural, weak = weak, strong = strong, strict = strict, argsLRT = list(asymptotic = TRUE,method = "satorra.bentler.2010"))



mod_cat <- 'f1 =~ ss1 + ss2 + ss3 + ss4 + ss5 + ss6 + ss7 + ss8
            ss2 ~~ ss4
            ss2 ~~ ss3'


it <- c("ss1","ss2","ss3","ss4","ss5","ss6","ss7","ss8")
grupo="sex"
##test.seq <-  c("thresholds","loadings")
test.seq <-  c("loadings", "intercepts")
meq.list <- list()
for (i in 0L:length(test.seq)) {
  if (i == 0L) {
    meq.label <- "configural"
    group.equal <- ""
  } else {
    meq.label <- test.seq[i]
    group.equal <- test.seq[1:i]
  }
  cat(group.equal, "\n")
  meq.list[[meq.label]] <- measEq.syntax(configural.model = mod_cat,
                                         data = bla2,
                                         ordered = it,
                                         parameterization = "theta",
                                         ID.fac = "std.lv",
                                         ID.cat = "Wu.Estabrook.2016",
                                         group = grupo,
                                         group.equal = group.equal,
                                         return.fit = TRUE)
  }

compareFit(meq.list)













library(fBasics)
colSkewness(dat)
colKurtosis(dat)







mod_cat <- '
  # Latent variable definitions
   sss =~ NA*ss1 + NA*ss2 + NA*ss3 + NA*ss4 + NA*ss5 + NA*ss6 + NA*ss7 + NA*ss8

  # Latent variable variances
  sss ~~ 1*sss

  # Manifest variable variances (uniquenesses)
  ss1 ~~ ss1
  ss2 ~~ ss2
  ss3 ~~ ss3
  ss4 ~~ ss4
  ss5 ~~ ss5
  ss6 ~~ ss6
  ss7 ~~ ss7
  ss8 ~~ ss8

  # Manifest variable covariances
  ss2 ~~ ss4
  ss2 ~~ ss3

  # Manifest variable means
  ss1 ~ 1
  ss2 ~ 1
  ss3 ~ 1
  ss4 ~ 1
  ss5 ~ 1
  ss6 ~ 1
  ss7 ~ 1
  ss8 ~ 1
'

fit1 <- lavaan(mod_cat, data = dat, mimic = "mplus", estimator = "MLR")
summary(fit1, standardized = TRUE, fit.measures = TRUE)
resid(fit1, type = "normalized")$cov
modificationindices(fit1, standardized = TRUE,sort. = TRUE) %>% slice(1:10)
fitmeasures(fit1, fit.measures = c("chisq", "df", "pvalue", "rmsea", "cfi", "tli"))
library(semPlot)

pdf("path-diagram.pdf")
semPaths(fit1, what = "std", intercepts = FALSE, style = "lisrel", asize = 2, esize = 1,
         edge.color = "black", as.expression = "nodes",
         nodeLabels = c(as.expression(lapply(1:8, function(i) bquote(i[.(i)]))), "SSS"),
         arrowAngle = 0.35,
         weighted = FALSE)
dev.off()


measurementInvariance(model = mod_cat, data = dat, group = "sex", strict = T)

baseline <- measEq.syntax(configural.model = mod_cat, data = dat,
                          parameterization = "delta", ID.fac = "std.lv",
                          ID.cat = "Wu.Estabrook.2016", group = "age", group.equal = "configural")
model_baseline <- as.character(baseline)
fit_baseline <- cfa(model_baseline, data = dat, group = "age")


baseline <- measEq.syntax(configural.model = mod_cat,
                       #data = dat,        
                       #ordered =  c("ss1","ss2","ss3","ss4","ss5","ss6","ss7", "ss8"),
                       #parameterization = "delta",
                       #ID.fac = "std.lv",
                       #ID.cat = "Wu.Estabrook.2016",
                       group = "sex",
                       group.equal = "configural")





configural <- cfa(mod_cat, data = dat, group = "age")
weak <- cfa(mod_cat, data = dat, group = "age", group.equal = "loadings")
strong <- cfa(mod_cat, data = dat, group = "age", group.equal = c("loadings", "intercepts"))
strict <- cfa(mod_cat, data = dat, group = "age", group.equal = c("loadings", "intercepts", "residuals"))

fitmeasures(strict)



#######################
#######################
#######################
library(tidyverse)
library(lavaan)
library(MVN)
library(semPlot)
library(psych)
library(ltm)
library(nFactors)
library(qgraph)
library(simsem)

## Import data
data <- read.csv("../../podatki/anketa150259-2018-03-03.csv", sep = ";", skip = 1)
## Prepare data
col_names <- c(paste0("i", 1:8), paste0('alco', 1:3), paste0('smok', 1:3), paste0('drug', 1:2), 'sex', 'age', 'edu')
data <- data %>%
    dplyr::select(8:26) %>%
    mutate_all(funs(gsub("=", "", .))) %>%
    setNames(col_names) %>%
    mutate_if(is.character, as.numeric) %>%
    mutate(sss = rowSums(.[1:8])) %>%
    mutate_at(vars(alco1:edu), funs(dplyr::recode(., `-2` = 0, `-3` = NA_real_))) %>%
    mutate_at(vars(age), funs(dplyr::recode(., `2` = 2, `3` = 2, `4` = 2))) %>%
    mutate_at(vars(edu), funs(dplyr::recode(., `2` = 3))) %>%
    na.omit()

## Descriptive statistics
desc <- data %>%
    dplyr::select(1:8) %>%
    describe() %>%
    dplyr::select(mean, sd, skew, kurtosis)

## Cronbach alpha
alpha(data[, 1:8])

## Corrected item-test correlations
r_cor <- data %>%
    dplyr::select(1:8) %>%
    alpha() %>%
    .$item.stats %>%
    dplyr::select(r.cor)

## KMO test
KMO(data[, 1:8])

## Bartlett test of sphericity
cortest.bartlett(data[, 1:8])

# Determine plausible number of latent factors
R <- cor(data[, 1:8])
range(R[upper.tri(R)])
ev <- eigen(R)
ap <- parallel(subject = nrow(data), var = 8, rep = 100, cent = .05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# Factor analysis
fa(r = data[, 1:8], nfactors = 1, rotate = "none")
fa(r = data[, 1:8], nfactors = 1, rotate = "varimax")
fa(r = data[, 1:8], nfactors = 1, rotate = "bifactor", fm = "pa")

## Test for multivariate normality
mvn(data = data[, 1:8], mvnTest = "hz")

## Specify CFA model 0
model0 <- '
  # Latent variable definitions
  sss =~ NA*i1 + NA*i2 + NA*i3 + NA*i4 + NA*i5 + NA*i6 + NA*i7 + NA*i8
  # Latent variable variances
  sss ~~ 1*sss
  # Manifest variable variances (uniquenesses)
  i1 ~~ i1
  i2 ~~ i2
  i3 ~~ i3
  i4 ~~ i4
  i5 ~~ i5
  i6 ~~ i6
  i7 ~~ i7
  i8 ~~ i8
  # Manifest variable covariances
  # Manifest variable means
  i1 ~ 1
  i2 ~ 1
  i3 ~ 1
  i4 ~ 1
  i5 ~ 1
  i6 ~ 1
  i7 ~ 1
  i8 ~ 1
'
## Fit model 0
fit0 <- lavaan(model = model0, data = data, mimic = "mplus", estimator = "MLR")
summary(fit0, standardized = TRUE, fit.measures = TRUE)
resid(fit0, type = "normalized")$cov
modificationindices(fit0, standardized = TRUE, sort. = TRUE) %>% slice(1:10)
fit0_res <- fitmeasures(fit0, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled",
                                               "rmsea.ci.upper.scaled", "cfi.scaled", "tli.scaled")) %>%
    unclass()

## Specify CFA model 1
model1 <- '
  # Latent variable definitions
  sss =~ NA*i1 + NA*i2 + NA*i3 + NA*i4 + NA*i5 + NA*i6 + NA*i7 + NA*i8
  # Latent variable variances
  sss ~~ 1*sss
  # Manifest variable variances (uniquenesses)
  i1 ~~ i1
  i2 ~~ i2
  i3 ~~ i3
  i4 ~~ i4
  i5 ~~ i5
  i6 ~~ i6
  i7 ~~ i7
  i8 ~~ i8
  # Manifest variable covariances
  i2 ~~ i4
  # Manifest variable means
  i1 ~ 1
  i2 ~ 1
  i3 ~ 1
  i4 ~ 1
  i5 ~ 1
  i6 ~ 1
  i7 ~ 1
  i8 ~ 1
'
## Fit model 1
fit1 <- lavaan(model = model1, data = data, mimic = "mplus", estimator = "MLR")
summary(fit1, standardized = TRUE, fit.measures = TRUE)
resid(fit1, type = "normalized")$cov
modificationindices(fit1, standardized = TRUE, sort. = TRUE) %>% slice(1:10)
fit1_res <- fitmeasures(fit1, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled",
                                               "rmsea.ci.upper.scaled", "cfi.scaled", "tli.scaled")) %>%
    unclass()

## Specify CFA model 2
model2 <- '
  # Latent variable definitions
  sss =~ NA*i1 + NA*i2 + NA*i3 + NA*i4 + NA*i5 + NA*i6 + NA*i7 + NA*i8
  # Latent variable variances
  sss ~~ 1*sss
  # Manifest variable variances (uniquenesses)
  i1 ~~ i1
  i2 ~~ i2
  i3 ~~ i3
  i4 ~~ i4
  i5 ~~ i5
  i6 ~~ i6
  i7 ~~ i7
  i8 ~~ i8
  # Manifest variable covariances
  i2 ~~ i4
  i2 ~~ i3
  # Manifest variable means
  i1 ~ 1
  i2 ~ 1
  i3 ~ 1
  i4 ~ 1
  i5 ~ 1
  i6 ~ 1
  i7 ~ 1
  i8 ~ 1
'
## Fit model 2
fit2 <- lavaan(model = model2, data = data, mimic = "mplus", estimator = "MLR")
summary(fit2, standardized = TRUE, fit.measures = TRUE)
resid(fit2, type = "normalized")$cov
modificationindices(fit2, standardized = TRUE, sort. = TRUE) %>% slice(1:10)
fit2_res <- fitmeasures(fit2, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled",
                                               "rmsea.ci.upper.scaled", "cfi.scaled", "tli.scaled")) %>%
    unclass()

fit_res <- bind_rows(fit0_res, fit1_res, fit2_res)
fit_res %>% kable(digits = c(2,1, rep(3, 6)))


# Plot path diagram
#pdf("path-diagram.pdf")
#semPaths(fit2, what = "std", intercepts = FALSE, style = "lisrel", asize = 2, esize = 1,
#         edge.color = "black", as.expression = "nodes",
#         nodeLabels = c(as.expression(lapply(1:8, function(i) bquote(i[.(i)]))), "BSSS"),
#         arrowAngle = 0.35,
#         weighted = FALSE)
#dev.off()

# Plot path diagram
pdf("path-diagram.pdf")
semPaths(fit2, what = "std", intercepts = FALSE, style = "lisrel", asize = 2, esize = 1,
         edge.color = "black", as.expression = "nodes",
         nodeLabels = c(1:8, "BSSS"),
         arrowAngle = 0.35,
         weighted = FALSE)
dev.off()

## Measurement invariance testing
## Group: sex
configural <- cfa(model = model2, data = data, estimator = "MLR", group = "sex")
weak <- cfa(model = model2, data = data, estimator = "MLR", group = "sex", group.equal = "loadings")
strong <- cfa(model = model2, data = data, estimator = "MLR", group = "sex", group.equal = c("loadings", "intercepts"))
strict <- cfa(model = model2, data = data, estimator = "MLR", group = "sex", group.equal = c("loadings", "intercepts", "residuals"))
## Compare fit indices
compareFit(config = configural, weak = weak, strong = strong, strict = strict)

## Group: age
configural <- cfa(model = model2, data = data, estimator = "MLR", group = "age")
weak <- cfa(model = model2, data = data, estimator = "MLR", group = "age", group.equal = "loadings")
strong <- cfa(model = model2, data = data, estimator = "MLR", group = "age", group.equal = c("loadings", "intercepts"))
strict <- cfa(model = model2, data = data, estimator = "MLR", group = "age", group.equal = c("loadings", "intercepts", "residuals"))
## compare fit indices
compareFit(config = configural, weak = weak, strong = strong, strict = strict)

## Group: edu
configural <- cfa(model = model2, data = data, estimator = "MLR", group = "edu")
weak <- cfa(model = model2, data = data, estimator = "MLR", group = "edu", group.equal = "loadings")
strong <- cfa(model = model2, data = data, estimator = "MLR", group = "edu", group.equal = c("loadings", "intercepts"))
strict <- cfa(model = model2, data = data, estimator = "MLR", group = "edu", group.equal = c("loadings", "intercepts", "residuals"))
## compare fit indices
compareFit(config = configural, weak = weak, strong = strong, strict = strict)




## Measurement invariance through subsampling
set.seed <- 12345
## Create list with subsampled components for sex group
group_col <- 17
nsam <- 100
sim_list_sex <- list()
id <- data[, group_col]
min_n <- min(table(id))
max_n <- max(table(id))
ids <- split(seq_along(id), id)
for (i in seq_len(nsam)) {
  new_id <- unlist(lapply(ids, sample, min_n))
  dat_sam <- data[new_id, c(1:8, 17)]
  sim_list_sex[[i]] <- dat_sam
}

simfit0_sex <- sim(nRep = NULL, model = model2, rawData = sim_list_sex,
                   group = "sex", lavaanfun = "cfa")

simfit1_sex <- sim(nRep = NULL, model = model2, rawData = sim_list_sex,
                   group = "sex", lavaanfun = "cfa",
                   group.equal = "loadings")

simfit2_sex <- sim(nRep = NULL, model = model2, rawData = sim_list_sex,
                   group = "sex", lavaanfun = "cfa",
                   group.equal = c("loadings", "intercepts"))

simfit3_sex <- sim(nRep = NULL, model = model2, rawData = sim_list_sex,
                   group = "sex", lavaanfun = "cfa",
                   group.equal = c("loadings", "intercepts", "residuals"))

sim0 <- summaryFit(simfit0_sex) %>%
    t() %>%
    as_tibble(rownames = "rname") %>%
    dplyr::select(rname, chisq, rmsea, cfi, tli, srmr) %>%
    filter(rname %in% c("Mean", "SD")) %>%
    column_to_rownames("rname")

sim1 <- summaryFit(simfit1_sex) %>%
    t() %>%
    as_tibble(rownames = "rname") %>%
    dplyr::select(rname, chisq, rmsea, cfi, tli, srmr) %>%
    filter(rname %in% c("Mean", "SD")) %>%
    column_to_rownames("rname")

sim2 <- summaryFit(simfit2_sex) %>%
    t() %>%
    as_tibble(rownames = "rname") %>%
    dplyr::select(rname, chisq, rmsea, cfi, tli, srmr) %>%
    filter(rname %in% c("Mean", "SD")) %>%
    column_to_rownames("rname")

sim3 <- summaryFit(simfit3_sex) %>%
    t() %>%
    as_tibble(rownames = "rname") %>%
    dplyr::select(rname, chisq, rmsea, cfi, tli, srmr) %>%
    filter(rname %in% c("Mean", "SD")) %>%
    column_to_rownames("rname")
## Output
bind_rows(sim0, sim1, sim2, sim3) %>% kable(digits = 3)

## Measurement invariance through subsampling
set.seed <- 12345
## Create list with subsampled components for age group
group_col <- 18
nsam <- 100
sim_list_age <- list()
id <- data[, group_col]
min_n <- min(table(id))
max_n <- max(table(id))
ids <- split(seq_along(id), id)
for (i in seq_len(nsam)) {
  new_id <- unlist(lapply(ids, sample, min_n))
  dat_sam <- data[new_id, c(1:8, 18)]
  sim_list_age[[i]] <- dat_sam
}

simfit0_age <- sim(nRep = NULL, model = model2, rawData = sim_list_age,
                   group = "age", lavaanfun = "cfa")


simfit1_age <- sim(nRep = NULL, model = model2, rawData = sim_list_age,
                   group = "age", lavaanfun = "cfa",
                   group.equal = "loadings")

simfit2_age <- sim(nRep = NULL, model = model2, rawData = sim_list_age,
                   group = "age", lavaanfun = "cfa",
                   group.equal = c("loadings", "intercepts"))

simfit3_age <- sim(nRep = NULL, model = model2, rawData = sim_list_age,
                   group = "age", lavaanfun = "cfa",
                   group.equal = c("loadings", "intercepts", "residuals"))

sim0 <- summaryFit(simfit0_age) %>%
    t() %>%
    as_tibble(rownames = "rname") %>%
    dplyr::select(rname, chisq, rmsea, cfi, tli, srmr) %>%
    filter(rname %in% c("Mean", "SD")) %>%
    column_to_rownames("rname")

sim1 <- summaryFit(simfit1_age) %>%
    t() %>%
    as_tibble(rownames = "rname") %>%
    dplyr::select(rname, chisq, rmsea, cfi, tli, srmr) %>%
    filter(rname %in% c("Mean", "SD")) %>%
    column_to_rownames("rname")

sim2 <- summaryFit(simfit2_age) %>%
    t() %>%
    as_tibble(rownames = "rname") %>%
    dplyr::select(rname, chisq, rmsea, cfi, tli, srmr) %>%
    filter(rname %in% c("Mean", "SD")) %>%
    column_to_rownames("rname")

sim3 <- summaryFit(simfit3_age) %>%
    t() %>%
    as_tibble(rownames = "rname") %>%
    dplyr::select(rname, chisq, rmsea, cfi, tli, srmr) %>%
    filter(rname %in% c("Mean", "SD")) %>%
    column_to_rownames("rname")

## Output
bind_rows(sim0, sim1, sim2, sim3) %>% kable(digits = 3)

## Measurement invariance through subsampling
set.seed <- 12345
## Create list with subsampled components for education group
group_col <- 19
nsam <- 100
sim_list_edu <- list()
id <- data[, group_col]
min_n <- min(table(id))
max_n <- max(table(id))
ids <- split(seq_along(id), id)
for (i in seq_len(nsam)) {
  new_id <- unlist(lapply(ids, sample, min_n))
  dat_sam <- data[new_id, c(1:8, 19)]
  sim_list_edu[[i]] <- dat_sam
}

simfit0_edu <- sim(nRep = NULL, model = model2, rawData = sim_list_edu,
                   group = "edu", lavaanfun = "cfa")


simfit1_edu <- sim(nRep = NULL, model = model2, rawData = sim_list_edu,
                   group = "edu", lavaanfun = "cfa",
                   group.equal = "loadings")

simfit2_edu <- sim(nRep = NULL, model = model2, rawData = sim_list_edu,
                   group = "edu", lavaanfun = "cfa",
                   group.equal = c("loadings", "intercepts"))

simfit3_edu <- sim(nRep = NULL, model = model2, rawData = sim_list_edu,
                   group = "edu", lavaanfun = "cfa",
                   group.equal = c("loadings", "intercepts", "residuals"))

sim0 <- summaryFit(simfit0_edu) %>%
    t() %>%
    as_tibble(rownames = "rname") %>%
    dplyr::select(rname, chisq, rmsea, cfi, tli, srmr) %>%
    filter(rname %in% c("Mean", "SD")) %>%
    column_to_rownames("rname")

sim1 <- summaryFit(simfit1_edu) %>%
    t() %>%
    as_tibble(rownames = "rname") %>%
    dplyr::select(rname, chisq, rmsea, cfi, tli, srmr) %>%
    filter(rname %in% c("Mean", "SD")) %>%
    column_to_rownames("rname")

sim2 <- summaryFit(simfit2_edu) %>%
    t() %>%
    as_tibble(rownames = "rname") %>%
    dplyr::select(rname, chisq, rmsea, cfi, tli, srmr) %>%
    filter(rname %in% c("Mean", "SD")) %>%
    column_to_rownames("rname")

sim3 <- summaryFit(simfit3_edu) %>%
    t() %>%
    as_tibble(rownames = "rname") %>%
    dplyr::select(rname, chisq, rmsea, cfi, tli, srmr) %>%
    filter(rname %in% c("Mean", "SD")) %>%
    column_to_rownames("rname")

## Output
bind_rows(sim0, sim1, sim2, sim3) %>% kable(digits = 3)






## Gender and age differences
## Table 3
## Spol
##data %>%
##  dplyr::select(starts_with("i"), sex) %>% 
##  summarise_all(funs(sd)) %>% 
##  round(digits = 2)

## Means 
data %>%
    dplyr::select(starts_with("i"), edu) %>%
    group_by(edu) %>% 
    summarise_all(funs(sd)) %>%
    t() %>% round(2)

## SDs
data %>%
    dplyr::select(starts_with("sss"), sex) %>%
    group_by(sex) %>% 
    summarise_all(funs(sd))


## Compute t.test for all items
data %>% 
  dplyr::select(starts_with("i"), edu) %>% 
  gather(var, val, -edu) %>% 
  group_by(edu, var) %>% 
  summarise(value = list(val)) %>% 
  spread(edu, value) %>% 
  group_by(var) %>% 
  mutate(t_value = t.test(unlist(`1`), unlist(`2`))$statistic,
         p_value = t.test(unlist(`1`), unlist(`2`))$p.value) %>%
  ungroup() %>%
  mutate(p_adj = round(p.adjust(p_value, method = "BH"), 3))

## Compute Cohen's d for all items
data %>%
    dplyr::select(starts_with("sss"), sex) %>%
    summarise_each(funs(effsize::cohen.d(. ~ as.factor(sex))$estimate), -sex) %>%
    round(2)


## Nomological network

round(p.adjust(c(1.87e-07, 0.2495, 1.146e-08, 0.004937, 5.143e-08, 2.099e-09, 6.153e-09, 0.02481)), 3)





n_labels <- c(expression(A[1]), expression(A[2]), expression(S[1]), expression(S[2]),
              expression(D[1]), expression(D[2]),
              expression(Age), expression(Edu),
              expression(BSSS))

pdf("qgraph.pdf", width = 14, height = 7)
par(mfrow = c(1, 2))
data %>%
    dplyr::select(-starts_with("i")) %>%
    dplyr::select(-alco2, -smok2, -sex) %>%
    cor_auto() %>%
    qgraph(layout = "spring", graph = "cor", sampleSize = nrow(data), labels = n_labels)
title("BSSS-8", adj = 0)
## SSS-V
read_tsv("dimenzije_skupaj.dat") %>%
    dplyr::select(E, N, P, SSS, SE, SI, MO, En, S, V, CS, O) %>%
    cor_auto() %>%
    qgraph(layout = "spring", graph = "cor", sampleSize = nrow(.),
           labels = c("E", "N", "P", "SSS", "SE", "SI", "MO", "En", "Ag", "Co", "Ne", "Op"))
title("SSS-V", adj = 0)
dev.off()

## plot

## IRT analysis

# Constrained discrimination
fit1 <- grm(data[, 1:8], constrained = TRUE)
# Unconstrained discrimination
fit2 <- grm(data[, 1:8], constrained = FALSE)
# Which model is better
anova(fit1, fit2)

# Item response category characteristic curve
# Show how the probability of responding in the k-th category, in each item, changes with the values of the latent variabl
plot(fit2)

# Item information curve
# How well and precisely each item measures the latent trait at various levels of the attribute
plot(fit2, type = "IIC", legend = TRUE)

# Test information function curve
# Aggregates the Item Information Curves across all the items
iic_data <- plot(fit2, type = "IIC", items = 0) %>% as_tibble()

plt1 <- ggplot(iic_data, aes(x = z, y = test.info)) +
  geom_line() +
  labs(x = "Sensation seeking", y = "Information") +
  theme_bw()
ggsave("./tmp.pdf", plt1, width = 5, height = 3.5)
system(paste("pdfcrop", "./tmp.pdf", "./information.pdf"))
system("rm ./tmp.pdf")


library(mirt)
model_grm <- "sss = 1-8"
grm <- mirt(data = data[, 1:8], model = model_grm, itemtype = "graded", SE = TRUE, verbose = FALSE)

pcm <- mirt(data = data[, 1:8], model = model_grm, itemtype = "Rasch", SE = TRUE, verbose = FALSE)

gpcm <- mirt(data = data[, 1:8], model = model_grm, itemtype = "gpcm", SE = TRUE, verbose = FALSE)

rsm <- mirt(data = data[, 1:8], model = model_grm, itemtype = "rsm", SE = TRUE, verbose = FALSE)


bla <- coef(grm, IRTpars = TRUE, simplify = TRUE)

g <- plot(grm, type = "infotrace")
my_list <- g$panel.args
my_list[[9]] <- plot(grm, type = "info")$panel.args[[1]]

facet_names <- c(
    `1` = expression(i[1]),
    `2` = expression(i[2]),
    `3` = expression(i[3]),
    `4` = expression(i[4]),
    `5` = expression(i[5]),
    `6` = expression(i[6]),
    `7` = expression(i[7]),
    `8` = expression(i[8]),
    `9` = "BSSS"
)

#df <- my_list %>% map(`[`, c("x", "y")) %>% map_df(data.frame, .id = "dist") %>%
#    mutate_at(.vars = "dist", .funs = factor, labels = facet_names)

labs <- c(1:8, "BSSS")

df <- my_list %>% map(`[`, c("x", "y")) %>% map_df(data.frame, .id = "dist") %>%
    mutate_at(.vars = "dist", .funs = factor, labels = c(1:8, "BSSS"))


plt2 <- ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    facet_wrap(~ dist) +
    theme_bw() +
    labs(x = "Sensation seeking", y = "Information") +
    theme(panel.grid.minor = element_blank())

ggsave(filename = "info.pdf", plot = plt2, width = 150, height = 150,
       units = "mm")


model3 <- '
  # Latent variable definitions
  sss =~ i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8
'

strict1 <- cfa(model3, data = data, group = "sex", group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "means"))

strict2 <- cfa(model3, data = data, group = "sex", group.equal = c("loadings", "intercepts", "residuals", "lv.variances"))



library(sjstats)


data2 <- data %>%
    select(alco1,alco3,smok1,smok3,drug1,drug2,sss,sex,age,edu) %>%
    mutate_at(vars(sex),funs(as.factor)) %>%
    mutate_at(vars(age, edu), funs(factor(., ordered = TRUE)))

library(cluster)
D <- daisy(data2, metric = "gower") %>%
    as.matrix()

sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(D, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}

sil_width <- enframe(sil_width)

plt1 <- ggplot(sil_width, aes(x = name, y = value)) +
    geom_line() +
    geom_point(pch = 21, size = 2, fill = "white") +
    scale_x_continuous(breaks = 1:8, labels = 1:8) +
    labs(x = "Number of clusters", y = "Silhouette width") +
    theme_bw() +
    theme(panel.grid.minor = element_blank())


#plot(1:8, sil_width,
#     xlab = "Number of clusters",
#     ylab = "Silhouette Width")
#lines(1:8, sil_width)

library(Rtsne)
tsne_obj <- Rtsne(D, is_distance = TRUE)

k <- 3
pam_fit <- pam(D, diss = TRUE, k)
pam_results <- data2 %>%
  mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))

pam_results$the_summary

sss_discrete <- ifelse(data$sss > median(data$sss), 2, 1)
sss_discrete <- enframe(sss_discrete, name = NULL)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering))





plt2 <- ggplot(aes(x = X, y = Y), data = tsne_data) +
    geom_point(aes(color = cluster), size = 3) +
    labs(x = "t-SNE dimension 1", y = "t-SNE dimension 2") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.justification = c(0, 0),
          legend.position = c(0, 0),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent"))

gg_all <- plot_grid(plt1, plt2, labels = c("A", "B"))
save_plot("./tsne.pdf", gg_all, base_width = 11, base_height = 5.5)

library(cowplot)




# Omega reliability
library(MBESS)
ci.reliability(data[, 1:8], type = "omega", interval.type="bca", B=1000)


## Check for unidimensionality using Rasch modeling

library(ltm)

out <- unidimTest(rasch(na.omit(data2[, 1:8])), B = 2000)


                  plot(out, type = "b", pch = 1:2)
legend("topright", c("Real Data", "Average Simulated Data"), lty = 1, 
       pch = 1:2, col = 1:2, bty = "n")


m1 <- mirt(na.omit(data2[, 1:8]), 1)
m2 <- mirt(na.omit(data2[, 1:8]), 2)


Anova(lm(i1 ~ sex * age * edu, data = data), type = 2)

C <- data %>%
    dplyr::select(starts_with("i")) %>%
    cor_auto()


fa(r = C, nfactors = 1, n.obs = nrow(data))


library(eRm)

mod1 <- PCM(X = data[, 1:8])


## Check dimensionality
Rdep <- polychoric(data[, 1:8], correct = TRUE)$rho
Rdep <- cor_auto(data[, 1:8])
evals <- eigen(Rdep)$values
scree(Rdep, factors = FALSE)
(evals/sum(evals)*100)[1:2]

set.seed(123)
resPA <- fa.parallel(data[, 1:8], fa = "pc", cor = "poly",fm = "ml")
resvss <- vss(Rdep, fm = "ml", n.obs = nrow(data), plot = FALSE)
fadep <- fa(data[, 1:8], 4, cor = "poly", fm = "ml")

fitifa1 <- mirt(data[, 1:8], 1, verbose = FALSE)
fitifa2 <- mirt(data[, 1:8], 2, verbose = FALSE, TOL = 0.001)
fitifa3 <- mirt(data[, 1:8], 3, verbose = FALSE, TOL = 0.001)
fitifa4 <- mirt(data[, 1:8], 4, verbose = FALSE, TOL = 0.001)
anova(fitifa3, fitifa4, verbose = FALSE)

zar1d <- mirt(data[, 1:8], 1, itemtype = "graded")
zar2d <- mirt(data[, 1:8], 2, itemtype = "graded")
zar3d <- mirt(data[, 1:8], 3, itemtype = "graded")
zar4d <- mirt(data[, 1:8], 4, itemtype = "graded")
anova(zar3d, zar4d)

M2(zar4d, type = "C2")
