#Parts of the following code were partially generated with the assistance of artificial intelligence (AI)
#to enhance efficiency, ensure correctness, and improve readability and usability. Only technical code 
#components (e.g., syntax, structure) were AI-assisted; no data, results, or interpretations were generated, 
#modified, or influenced by AI. #All analyses and conclusions were conducted independently.

#### NOTE
#Item codes are as follows - R8 (item 1 in the original questionnaire), 
#R9 (item 2), R10 (idem 3) and so on.
#Items were recoded for impartiallity during analysis. 


#data
data<-read.csv("C:/Users/wikto/Desktop/R_folder/Trigeminal/data_wiktoria_2.csv", sep = ";", dec = ",", row.names = NULL)

#columns
data$self_rated_sens <- data$How.sensitive.is.your.nose.to.stinging.burning.
data$Ammola <- as.numeric(data$Ammola)
data$Identification <- as.numeric(data$Identification)
data$current_smell <- as.numeric(data$Current.smell)

#15item summary
summary(data$Trigeminal_score)

#packages
library(psych)
library(lavaan)
library(ggplot2)
library(effectsize)
library(rstatix)
library(dplyr)
library(tidyr)
library(patchwork)

#reliability
psych::alpha(items, na.rm = TRUE)

#excluding responses with NA (recomended for EFA)
items2 <- na.omit(items)
psych::alpha(items2)
summary(items2)

#kaiser-meyer-olkin factor adequacy
# 0.00 to 0.49 unacceptable
# 0.50 to 0.59 miserable
# 0.60 to 0.69 mediocre
# 0.70 to 0.79 middling
# 0.80 to 0.89 meritorious
#0.90 to 1.00 marvelous

KMO(items2)
#results:   R8   R9  R10  R11  R12  R13  R14  R15  R16  R17  R18  R19  R20  R21  R22 
#_________0.88 0.82 0.89 0.65 0.89 0.83 0.86 0.88 0.86 0.88 0.89 0.86 0.85 0.88 0.88 
#are acceptable, although R11 requires attention


#Barlett's test for sphericity
#null hypothesis - variables are not factor 
cortest.bartlett(items2)
#null hypothesis rejected; p < .001; sphericity is observed

#screeplot
scree(items2, pc=FALSE)
#check for parallel analysis
fa.parallel(items2, fa ="fa")
#suggests up to 5 factors, but scree shows 1. Let's see further

#exploring 5 factors
Nfacs <- 5

fit <- factanal(items2, Nfacs, rotation = "promax")
print(fit, digits=2, sort=TRUE)


#confirmation of intended factors
model3f <- 'f1 =~ data.R8 + data.R16 + data.R17+ data.R18+ data.R20
            f2 =~ data.R9 + data.R11 + data.R13 + data.R14 + data.R19+ data.R21 + data.R22
            F3 =~ data.R10 + data.R12 + data.R15'


fit <- cfa(model3f, data = items2)
summary(fit, standardized=TRUE, ci=TRUE, fit.measures=TRUE)

#CFI = 0.921, TLI = 0.905, RMSEA = 0.042 - there are no three factors

#confirmation for 1 factor
model1 <- 'f1 =~ data.R8 + data.R16 + data.R17+ data.R18+ data.R20 + data.R9 + data.R11 + data.R13 + data.R14 + data.R19+ data.R21 + data.R22 + data.R10 + data.R12 + data.R15'
fit <- cfa(model1, data = items2)
summary(fit, standardized=TRUE, ci=TRUE, fit.measures=TRUE)
#CFI = 917, TLI = 0.903, RMRSEA = 0.042. These items do not load one factor as well.
#Let's see with the best items only - maybe some baggage holds us down. 

#items above 0.4 factor loading 10 items
model3 <- 'f1 =~ data.R16 + data.R21 + data.R19 + data.R14 + data.R15 + data.R17 + data.R18 + data.R10 + data.R8 + data.R22'
fit3 <- cfa(model3, data = items2)
summary(fit3, standardized = TRUE, ci = TRUE, fit.measures = TRUE)

#8 items with best factor loadings - exploration of reliability 
model2 <- 'f1 =~ data.R16 + data.R21 + data.R19 + data.R14 + data.R15 + data.R17 + data.R18 + data.R10'
fit2 <- cfa(model2, data = items2)
summary(fit2, standardized = TRUE, ci = TRUE, fit.measures = TRUE)
#CFI = 0.969, TLI = 0.957, RMSEA = 0.043 - quite satisfying fit

#reliabilty of 8items model
eight_items <- data.frame(data$R10, data$R14, data$R15, data$R16, data$R17, data$R18, data$R19, data$R21)
psych::alpha(eight_items)

#items above 0.5 factor loading
model4 <- 'f1 =~ data.R14 + data.R15 + data.R16 + data.R19 + data.R21'
fit4 <- cfa(model4, data = items2)
summary(fit4, standardized = TRUE, ci = TRUE, fit.measures = TRUE)

#back to three factor exploration - let's omit items that have the poorest fit 
#in full item-list, and try to explore for the factors then
ten_items <- data.frame(items2$data.R16, items2$data.R21, items2$data.R19, items2$data.R14,
                        items2$data.R15, items2$data.R17, items2$data.R18, items2$data.R10,
                        items2$data.R8, items2$data.R22)
scree(eight_items)
scree(ten_items)

KMO(ten_items) #all items are above 0.8, so all can be explored
Nfacs <- 2
fit <- factanal(ten_items, Nfacs, rotation = "promax")
print(fit, digits = 2, sort = TRUE)

#dropping of the weakest item R10 (factor loadind < 0.3)
nine_items <- data.frame(items2$data.R16, items2$data.R21, items2$data.R19, 
                         items2$data.R14, items2$data.R15, items2$data.R17, 
                         items2$data.R18, items2$data.R8, items2$data.R22)

scree(nine_items)
fit <- factanal(nine_items, Nfacs, rotation = "promax")
print(fit, digits = 2, sort = TRUE)

#confirmation attempt
model6 <- 'f1 =~ data.R16 + data.R19 + data.R8 + data.R21 + data.R14 + data.R15 
          f2 =~ data.R17 + data.R18 + data.R22'
fit6 <- cfa(model6, data = items2)
summary(fit6, standardized = TRUE, ci = TRUE, fit.measures = TRUE)

#reliability of five item scale
five_items <- data.frame(data$R14, data$R15, data$R16, data$R19, data$R21)
alpha(five_items)

#reliability of two factors, nine items
factor1 <- data.frame(items2$data.R16, items2$data.R19, items2$data.R8, items2$data.R21, items2$data.R14, items2$data.R15)
factor2 <- data.frame(items2$data.R17, items2$data.R18, items2$data.R22)

alpha(factor1)
alpha(factor2)


#Back to exploratory analysis
summary(data$age)
sd(data$age, na.rm = TRUE)
table(data$sex_1f_2m)

data$trigeminal_new_score <- rowSums(data[, c("R14", "R15", "R16", "R19", "R21")], na.rm = TRUE)
print(data$trigeminal_new_score)

summary(data$trigeminal_new_score)
sd(data$trigeminal_new_score)
summary(data$Trigeminal_score)
sd(data$Trigeminal_score)

summary(data$How.sensitive.is.your.nose.to.stinging.burning.)
sd(data$How.sensitive.is.your.nose.to.stinging.burning.)

#testing whether the score is correlated with personal rating
summary(data$trigeminal_new_score)
cor.test(data$trigeminal_new_score, data$How.sensitive.is.your.nose.to.stinging.burning.)
#significant correlation

density_plot <- ggplot(data, aes(x = trigeminal_new_score)) + geom_density()
density_plot + coord_cartesian(xlim =c(0,15), ylim = c(0,0.15))
bar_plot <- ggplot(data, aes(x=How.sensitive.is.your.nose.to.stinging.burning.)) + geom_bar()
bar_plot


#SETTLED ON 8 ITEM MODEL
data$trigeminal_score_8_items <- rowSums(data [, c("R10", "R14", "R15", "R16", "R17", "R18", "R19", "R21")], na.rm = FALSE)

summary(data$trigeminal_score_8_items)
sd(data$trigeminal_score_8_items, na.rm = TRUE)
skew(data$trigeminal_score_8_items)
kurtosi(data$trigeminal_score_8_items)

summary(data$Trigeminal_score)
sd(data$Trigeminal_score)
skew(data$Trigeminal_score)
kurtosi(data$Trigeminal_score)

summary(data$Lateralization)
sd(data$Lateralization, na.rm = TRUE)
skew(data$Lateralization)
kurtosi(data$Lateralization)

summary(data$Identification)
sd(data$Identification)
skew(data$Identification)
kurtosi(data$Identification)

summary(data$Ammola)
sd(data$Ammola, na.rm = TRUE)
skew(data$Ammola)
kurtosi(data$Ammola)

summary(data$current_smell)
sd(data$current_smell, na.rm = TRUE)
skew(data$current_smell)
kurtosi(data$current_smell)

summary(data$self_rated_sens)
sd(data$self_rated_sens, na.rm = TRUE)
skew(data$self_rated_sens)
kurtosi(data$self_rated_sens)

summary(data$age)
sd(data$age, na.rm = TRUE)

data %>%
  summarise(
    N_Lateralization = sum(!is.na(Lateralization)),
    N_Identification = sum(!is.na(Identification)),
    N_Trigeminal_score = sum(!is.na(Trigeminal_score)),
    N_Trigeminal_8_item_score = sum(!is.na(trigeminal_score_8_items)),
    N_age = sum(!is.na(age)),
    N_Ammola = sum(!is.na(Ammola)),
    N_current_smell = sum(!is.na(current_smell)),
    N_self_rate = sum(!is.na(self_rated_sens))
  )


#validation
correlation_matrix_0 <- data.frame(data$trigeminal_score_8_items, data$age,
                                   data$Lateralization, data$Identification, data$Ammola, 
                                   data$self_rated_sens, data$current_smell)
correlation_matrix <- na.omit(correlation_matrix_0)

correlation_matrix <- data %>%
  select(trigeminal_score_8_items, age, 
         Lateralization, Identification, 
         Ammola, self_rated_sens, current_smell)

cor_results <- correlation_matrix %>%
  cor_test(method = "spearman") %>%
  mutate(
    r = round(cor, 3),
    r_squared = round(cor^2, 3),  
    p_value = case_when(
      p < 0.001 ~ "< .001",
      TRUE ~ paste0("= ", round(p, 3))
    )
  ) %>%
  select(var1, var2, r, r_squared, p_value)

cor_results %>%
  mutate(result = paste0(
    var1, " – ", var2, 
    ": r = ", r, 
    ", r² = ", r_squared,
    ", p ", p_value
  )) %>%
  pull(result) %>%
  cat(sep = "\n")

correlation_plot_sensitivity <- ggplot(data, aes(x = trigeminal_score_8_items, y = self_rated_sens)) +
  geom_point() +
  labs(title = "Correlation plot", 
       x = "Trigeminal score",
       y = "Self rated nasal sensitivity")
correlation_plot_sensitivity

correlation_plot_identification <- ggplot(data, aes(x = trigeminal_score_8_items, y = Identification)) +
  geom_point() +
  labs(title = "Correlation plot", 
       x = "Trigeminal score",
       y = "Identification")
correlation_plot_identification

correlation_plot_ammola <- ggplot(data, aes(x = trigeminal_score_8_items, y = Ammola)) +
  geom_point() +
  labs(title = "Correlation plot", 
       x = "Trigeminal score",
       y = "Ammola")
correlation_plot_ammola

correlation_plot_lateralization <- ggplot(data, aes(x = trigeminal_score_8_items, y = Lateralization)) +
  geom_point() +
  labs(title = "Correlation plot", 
       x = "Trigeminal score",
       y = "Lateralization")
correlation_plot_lateralization

#density plots for variables in correlation
dens_identification <- ggplot(data = data, aes(x = Identification)) + geom_bar()
dens_identification
dens_lateralization <- ggplot(data = data, aes(x = Lateralization)) + geom_density()
dens_lateralization
dens_ammola <- ggplot(data = data, aes(x = Ammola)) + geom_density()
dens_ammola
dens_trigeminal <- ggplot(data = data, aes(x = trigeminal_score_8_items)) + geom_density()
dens_trigeminal

#some experimental exploration
table(data$Identification_char)
data$Identification_char <- as.character(data$Identification)
exp <- ggplot(data = data, aes(x = Identification_char, y = trigeminal_score_8_items)) +
  geom_boxplot() +
  labs(x = "Identification",
       y = "Trigeminal score") 
exp


identification_anova <- kruskal.test(trigeminal_score_8_items~Identification_char, data = data)
identification_anova

data %>%
  dunn_test(trigeminal_score_8_items~Identification_char, p.adjust.method = "holm") %>%
  add_significance() 

class(identification_anova)
str(identification_anova)

#chceck_lateralization and identification
ggplot(data = data, aes(x = Identification_char, y = Lateralization)) + geom_boxplot()
kruskal.test(Lateralization~Identification_char, data = data)

data %>%
  dunn_test(Lateralization~Identification_char, p.adjust.method = "holm") %>%
  add_significance()

#LATERALIZATION - exploration
summary(data$Lateralization)

#quartile as cutoffs
Qu <- quantile(data$Lateralization, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
data$Lateralization_quartile <- cut(
  data$Lateralization,
  breaks = c(-Inf, Qu[1], Qu[3], Inf),
  labels = c("low", "medium", "high"),
  include.lowest = TRUE
)
table(data$Lateralization_quartile)

#with trigeminal
exp2 <- ggplot(data[!is.na(data$Lateralization_quartile), ], aes(x = Lateralization_quartile, y = trigeminal_score_8_items)) +
  geom_boxplot() +
  labs(x = "Lateralization",
       y = "Trigeminal score") 
exp2

lateralization_anova <- kruskal.test(trigeminal_score_8_items~Lateralization_quartile, data = data)
lateralization_anova

data %>%
  dunn_test(trigeminal_score_8_items~Lateralization_quartile, p.adjust.method = "holm") %>%
  add_significance()

#tercile as cutoffs
Te <- quantile(data$Lateralization, probs = c(0.33, 0.66), na.rm = TRUE)
data$Lateralization_tercile <- cut(
  data$Lateralization,
  breaks = c(-Inf, Te[1], Te[2], Inf),
  labels = c("low", "medium", "high"),
  include.lowest = TRUE
)
table(data$Lateralization_tercile)

data %>%
  group_by(Lateralization_tercile) %>%
  summarise(
    n = sum(!is.na(trigeminal_score_8_items)),
    mean = mean(trigeminal_score_8_items, na.rm = TRUE),
    sd = sd(trigeminal_score_8_items, na.rm = TRUE)
  )
#lateralization extreme terciles
data_extremes <- data %>%
  filter(Lateralization_tercile %in% c("low", "high"))

var.test(trigeminal_score_8_items ~ Lateralization_tercile, data = data_extremes)

t.test(trigeminal_score_8_items ~ Lateralization_tercile, 
       data = data_extremes,
       var.equal = TRUE) #t = -0.41816, p = 0.676
low <- data_extremes$trigeminal_score_8_items[data_extremes$Lateralization_tercile == "low"]
high <- data_extremes$trigeminal_score_8_items[data_extremes$Lateralization_tercile == "high"]
d <- (mean(low, na.rm = TRUE) - mean(high, na.rm = TRUE)) / 
  sqrt(((sd(low, na.rm = TRUE)^2 + sd(high, na.rm = TRUE)^2) / 2))
d # - 0.043

#boxplot lateralization trigeminal scores
ggplot(data_extremes, aes(x = Lateralization_tercile, y = trigeminal_score_8_items)) +
  geom_boxplot() +
  labs(
    x = "Lateralization Tercile",
    y = "Trigeminal Score",
    title = "Trigeminal score by lateralization"
  ) +
  scale_y_continuous(limits = c(0, 25)) +
  geom_jitter(alpha = 0.1) +
  theme_minimal()

#lateralization low hight trigeminal and sex
ggplot(data_extremes, aes(x = Lateralization_tercile, y = trigeminal_score_8_items)) +
  geom_jitter(
    aes(color = sex_1f_2m),
    alpha = 0.6,
    width = 0.2
  ) +
  geom_boxplot(outlier.shape = NA, alpha = 0.0) +
  scale_color_manual(
    values = c("1" = "#f2b5d4", "2" = "#a6d8ff"),
    labels = c("1" = "Female", "2" = "Male"),
    name = "Sex"
  ) +
  stat_summary(
    fun = function(y) length(y),
    geom = "text",
    aes(label = paste("n =", ..y..)),
    vjust = -0.5,
    fun.args = list(na.rm = TRUE)
  ) +
  scale_y_continuous(limits = c(0, 25)) +
  labs(
    x = "Lateralization Tercile",
    y = "Trigeminal Score",
    title = "Trigeminal score by lateralization"
  ) +
  theme_minimal()

#with trigeminal
exp2 <- ggplot(data[!is.na(data$Lateralization_tercile), ], aes(x = Lateralization_tercile, y = trigeminal_score_8_items)) +
  geom_boxplot() 
exp2

kruskal.test(trigeminal_score_8_items~Lateralization_tercile, data = data)

data %>%
  dunn_test(trigeminal_score_8_items~Lateralization_quartile, p.adjust.method = "holm") %>%
  add_significance()

#exploration: age and sex vs.trigeminal function
ggplot(data=data[data$sex_1f_2m %in% c(1,2),], 
       aes(x = factor(sex_1f_2m, levels = c(1,2), labels = c("female", "male")), 
           y = trigeminal_score_8_items)) + 
  geom_boxplot() + 
  labs(x = "Sex",
       y = "Trigeminal_score")
data$Sex_1f_2m_t <- data[data$sex_1f_2m %in% c(,2),]
data$sex_1f_2m_t <- factor(data$sex_1f_2m, levels = c(1, 2), labels = c("female", "male"))

#counts
table(data$sex_1f_2m_t)
#normality
normality_stats_psych <- function(x) {
  c(
    skewness = psych::skew(x, na.rm = TRUE),
    kurtosis = psych::kurtosi(x, na.rm = TRUE)
  )
} #normal distribution in both groups 
by(data$trigeminal_score_8_items, data$sex_1f_2m_t, normality_stats_psych)
#variance
var.test(trigeminal_score_8_items~sex_1f_2m_t, data = data) #not equal, non-parametric test then
t.test(trigeminal_score_8_items ~ sex_1f_2m_t, data = data) #t = 5.51, df = 902.61, p < 0.001
cohens_d(trigeminal_score_8_items ~ sex_1f_2m_t, data = data, pooled_sd = FALSE) #d = 0.35 95%Cl[0.23, 0.48]


#age
summary(data$age)
skew(data$age)
kurtosi(data$age) #normal_distribution

cor_test <- cor.test(data$trigeminal_score_8_items, data$age, use = "complete.obs")
cor_test$estimate
cor_test$p.value
#r = 0.11, p < 0.001

ggplot(data = data, aes(x = age, y = trigeminal_score_8_items)) + geom_point()
ggplot(data = data, aes(x = age, y = trigeminal_score_8_items)) + geom_smooth(method = "lm") + geom_point()

data$Ammola<-as.numeric(data$Ammola)

labels_plot <- c(
  Lateralization = "Lateralization",
  Identification = "Identification",
  age = "Age",
  Ammola = "Ammola",
  self_rated_sens = "Evaluation of nasal airflow",
  current_smell = "Self rated olfactory function"
)
data_long <- data %>%
  select(trigeminal_score_8_items, Lateralization, Identification, age, Ammola, self_rated_sens, current_smell) %>%
  pivot_longer(
    cols = c(Lateralization, Identification, age, Ammola, self_rated_sens, current_smell),
    names_to = "variable",
    values_to = "value"
  )

cor_labels <- data_long %>%
  group_by(variable) %>%
  summarise(
    cor = cor(value, trigeminal_score_8_items, method = "spearman", use = "complete.obs"),
    p = cor.test(value, trigeminal_score_8_items, method = "spearman")$p.value,
    x = mean(range(value, na.rm = TRUE)),                       
    y = max(trigeminal_score_8_items, na.rm = TRUE) * 1.05      
  ) %>%
  mutate(
    r_squared = round(cor^2, 2),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    label = paste0("r = ", round(cor, 2), stars, ", r² = ", r_squared)
  )

ggplot(data_long, aes(x = value, y = trigeminal_score_8_items)) +
  geom_point(alpha = 0.2, color = "skyblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~variable, scales = "free_x", labeller = as_labeller(labels_plot)) +
  geom_text(
    data = cor_labels,
    aes(x = x, y = y + 0.05 * y, label = label),
    inherit.aes = FALSE,
    size = 3.5,
    color = "black"
  ) +
  labs(x = "Value", y = "Trigeminal score") +
  theme_minimal()

data_long_2 <- data %>%
  select(trigeminal_score_8_items, Lateralization, age, Ammola) %>%
  pivot_longer(cols = c(Lateralization, age, Ammola),
               names_to = "variable",
               values_to = "value")
ggplot(data_long_2, aes(x = value, y = trigeminal_score_8_items)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = "Value", y = "Trigeminal score") +
  theme_minimal()

#invariance - how questionnaire differs in different groups
final_model <- '
f1 =~ R10 + R14 + R15 + R16 + R17 + R18 + R19 + R21
'

data_final_model <- data %>%
  select(R10, R14, R15, R16, R17, R18, R19, R21, sex_1f_2m_t) %>%
  na.omit()

data_final_model$sex_1f_2m_t <- factor(data_final_model$sex_1f_2m_t, levels = c("female", "male"))

#configural 
fit_configural <- cfa(final_model, data = data_final_model, group = "sex_1f_2m_t")
summary(fit_configural, fit.measures=TRUE)
#metric
fit_metric <- cfa(final_model, data = data_final_model, group = "sex_1f_2m_t",
                  group.equal = "loadings")
summary(fit_metric, fit.measures=TRUE)
#scalar
fit_scalar <- cfa(final_model, data = data_final_model, group = "sex_1f_2m_t",
                  group.equal = c("loadings", "intercepts"))
summary(fit_scalar, fit.measures=TRUE)
#summary and final comparison
anova(fit_configural, fit_metric, fit_scalar)


#exploration of sexes after scalar
data_female <- data_final_model %>% filter(sex_1f_2m_t == "female")
data_male <- data_final_model %>% filter(sex_1f_2m_t == "male")

#scores of female and male
data_female <- data_female %>%
  mutate(score = R10 + R14 + R15 + R16 + R17 + R18 + R19 + R21)

data_male <- data_male %>%
  mutate(score = R10 + R14 + R15 + R16 + R17 + R18 + R19 + R21)

#summary
summary_female <- data_female %>%
  summarise(mean = mean(score), sd = sd(score),
            p10 = quantile(score, 0.1),
            p25 = quantile(score, 0.25),
            p50 = quantile(score, 0.5),
            p75 = quantile(score, 0.75),
            p90 = quantile(score, 0.9))

summary_male <- data_male %>%
  summarise(mean = mean(score), sd = sd(score),
            p10 = quantile(score, 0.1),
            p25 = quantile(score, 0.25),
            p50 = quantile(score, 0.5),
            p75 = quantile(score, 0.75),
            p90 = quantile(score, 0.9))

summary_female
summary_male

data_final_model <- data_final_model %>%
  mutate(trigeminal_score_8_items = R10 + R14 + R15 + R16 + R17 + R18 + R19 + R21)

ggplot(data_final_model, aes(x = trigeminal_score_8_items, fill = sex_1f_2m_t)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Trigeminal questionnaire by sex",
    x = "Trigeminal score",
    y = "Density"
  ) +
  scale_fill_discrete(name = "Sex") +
  theme_minimal()
#different norms should be applied


data_filtered <- data %>%
  filter(!is.na(trigeminal_score_8_items))

data_sex_filtered <- data_filtered %>%
  filter(sex_1f_2m_t %in% c("female", "male"),
         !is.na(sex_1f_2m_t),
         !is.na(trigeminal_score_8_items))

ggplot(data_sex_filtered, aes(x = Lateralization, fill = sex_1f_2m_t)) +
  geom_density(alpha = 0.4) +
  labs(title = "Lateralization among males and females",
       x = "Lateralization",
       y = "Density") +
  scale_fill_manual(values = c("female" = "pink", "male" = "lightblue"),
                    name = "Sex",
                    labels = c("Women", "Men")) +
  theme_minimal()

data_filtered$Ammola<-as.numeric(data_filtered$Ammola)
data_sex_filtered$Ammola<-as.numeric(data_sex_filtered$Ammola)


#will lateralization differ across genders?
t.test(Lateralization ~ sex_1f_2m_t, data = data_sex_filtered) #t = 1.726 p = 0.0851
ggplot(data = data_sex_filtered, aes(x = sex_1f_2m_t, y = Lateralization)) + geom_boxplot()
cohens_d(Lateralization~sex_1f_2m_t, data = data_sex_filtered) #0.15 (negligible)
#or Ammola?
t.test(Ammola~sex_1f_2m_t, data = data_sex_filtered) #t = 4.22, p < 0.001
ggplot(data = data_sex_filtered, aes(x = sex_1f_2m_t, y = Ammola)) + geom_boxplot()
cohens_d(Ammola~sex_1f_2m_t, data = data_sex_filtered) #0.285 (small)
#Idenfitication
t.test(Identification~sex_1f_2m_t, data = data_filtered) #t = 0.39, p = 0.6942
ggplot(data = data_filtered, aes(x = sex_1f_2m_t, y = Identification)) + geom_boxplot()
cohens_d(Identification~sex_1f_2m_t, data = data_filtered) #0.026 (negligible)
#And score on filtered base
t.test(trigeminal_score_8_items~sex_1f_2m_t, data = data_sex_filtered) #t = 5.6278, p < 0.001
ggplot(data = data_sex_filtered, aes(x = sex_1f_2m_t, y = trigeminal_score_8_items)) + geom_boxplot()
cohens_d(trigeminal_score_8_items~sex_1f_2m_t, data = data_sex_filtered) #0.37 (small)

data_sex_filtered %>%
  filter(sex_1f_2m_t %in% c("female", "male")) %>%
  group_by(sex_1f_2m_t) %>%
  summarise(
    mean_Ammola = mean(Ammola, na.rm = TRUE),
    mean_Lateralization = mean(Lateralization, na.rm = TRUE),
    mean_trigeminal_score = mean(trigeminal_score_8_items, na.rm = TRUE),
    mean_Identification = mean(Identification, na.rm = TRUE)
  )

write.csv(data, file = "data_wiktoria.csv", row.names = FALSE)

#comparison of healthy and non-heatlhy people
table(data_filtered$condition)
condition_summarize <- data %>%
  filter(condition %in% c(0, 1)) %>%
  group_by(condition) %>%
  summarise(
    mean_Lateralization = mean(Lateralization, na.rm = TRUE),
    sd_Lateralization = sd(Lateralization, na.rm = TRUE),
    
    mean_Identification = mean(Identification, na.rm = TRUE),
    sd_Identification = sd(Identification, na.rm = TRUE),
    
    mean_Trigeminal = mean(trigeminal_score_8_items, na.rm = TRUE),
    sd_Trigeminal = sd(trigeminal_score_8_items, na.rm = TRUE),
    
    mean_Ammola = mean(Ammola, na.rm = TRUE),
    sd_Ammola = sd(Ammola, na.rm = TRUE))
print(condition_summarize)    

#t tests healthy and conditions
t.test(trigeminal_score_8_items~condition, data = data_filtered) #t = -3.51, p < 0.001
cohens_d(trigeminal_score_8_items~condition, data = data_filtered) #-0.3 (small)

t.test(Lateralization~condition, data = data_filtered) #t = 0.34329, p = 0.732
cohens_d(Lateralization~condition, data = data_filtered) # 0.0238 (negligible)

t.test(Identification~condition, data = data_filtered) #t = 0.49683, p =0.6198
cohens_d(Identification~condition, data = data) # 0.0422 (negligible)

t.test(Ammola~condition, data = data_filtered) #t = -0.4597, p = 0.6462
cohens_d(Ammola~condition, data = data_filtered) #-0.0402 (negligible)


ggplot(data_filtered[data$condition %in% c(0,1), ], 
       aes(x = factor(condition, levels = c(0, 1), labels = c("healthy", "condition")), 
           y = trigeminal_score_8_items)) +
  geom_boxplot() +
  labs(x = "Condition", y = "Trigeminal Score") +
  theme_minimal()

#covid
data$covid <- data$Have.you.already.had.Covid.
data_filtered$covid <- data_filtered$Have.you.already.had.Covid.
table(data$covid)

t.test(trigeminal_score_8_items~covid, data = data_filtered[data_filtered$covid %in% c("j", "n"),]) #t = -1.985, p = 0.04839
cohens_d(trigeminal_score_8_items~covid, data = data_filtered[data_filtered$covid %in% c("j", "n"),]) #-0.177 (negligible)

t.test(Lateralization~covid, data = data_filtered[data_filtered$covid %in% c("j", "n"),]) #t = 1.2393, p = 0.2176
cohens_d(Lateralization~covid, data = data_filtered[data_filtered$covid %in% c("j", "n"),]) # 0.147 (negligible)

t.test(Identification~covid, data = data_filtered[data_filtered$covid %in% c("j", "n"),]) #t = 1.3355, p = 0.1831
cohens_d(Identification~covid, data = data_filtered[data_filtered$covid %in% c("j", "n"),]) #0.12 (negligible)

t.test(Ammola~covid, data = data_filtered[data_filtered$covid %in% c("j", "n"),]) #t = -0.78419, p = 0.4337
cohens_d(Ammola~covid, data = data_filtered[data_filtered$covid %in% c("j", "n"),]) # -0.0648 (negligible)

covid_summarize <- data_filtered %>%
  filter(covid %in% c("j", "n")) %>%
  group_by(covid) %>%
  summarise(
    mean_Lateralization = mean(Lateralization, na.rm = TRUE),
    
    mean_Identification = mean(Identification, na.rm = TRUE),
    
    mean_Trigeminal = mean(trigeminal_score_8_items, na.rm = TRUE),
    
    mean_Ammola = mean(Ammola, na.rm = TRUE))
print(covid_summarize)    

#tercile as cutoffs
data$current_smell <- as.numeric(data$Current.smell)
data_filtered$current_smell <- as.numeric(data_filtered$Current.smell)
data_sex_filtered$current_smell <- as.numeric(data_sex_filtered$Current.smell)
class(data_filtered$current_smell)

data_filtered %>%
  summarise(
    mean_current_smell = mean(current_smell, na.rm = TRUE),
    sd_current_smell = sd(current_smell, na.rm = TRUE)
  )

Sm <- quantile(data_filtered$current_smell, probs = c(0.33, 0.66), na.rm = TRUE)
data_filtered$Current_smell_tercile <- cut(
  data_filtered$current_smell,
  breaks = c(-Inf, Sm[1], Sm[2], Inf),
  labels = c("low", "medium", "high"),
  include.lowest = TRUE
)
table(data_filtered$Current_smell_tercile)

data_filtered %>%
  group_by(Lateralization_tercile) %>%
  summarise(
    n = sum(!is.na(trigeminal_score_8_items)),  # liczba nie-NA
    mean = mean(trigeminal_score_8_items, na.rm = TRUE),
    sd = sd(trigeminal_score_8_items, na.rm = TRUE))

#d cohena    
smell <- quantile(data_filtered$current_smell, probs = c(0.33, 0.66), na.rm = TRUE)

data_filtered$current_smell_group <- cut(
  data_filtered$current_smell,
  breaks = c(-Inf, smell[1], smell[2], Inf),
  labels = c("low", "medium", "high"),
  include.lowest = TRUE
)

data_extremes <- data_filtered %>%
  filter(current_smell_group %in% c("low", "high"))

low <- data_extremes$trigeminal_score_8_items[data_extremes$current_smell_group == "low"]
high <- data_extremes$trigeminal_score_8_items[data_extremes$current_smell_group == "high"]

d <- (mean(low, na.rm = TRUE) - mean(high, na.rm = TRUE)) / 
  sqrt(((sd(low, na.rm = TRUE)^2 + sd(high, na.rm = TRUE)^2) / 2))
d

ggplot(data_extremes, aes(x = current_smell_group, y = trigeminal_score_8_items)) +
  geom_boxplot() +
  labs(
    x = "Self rated olfactory function",
    y = "Trigeminal score",
    title = "Trigeminal score by self rated olfactory function"
  ) +
  scale_x_discrete(labels = c("low" = "poor", "high" = "good")) +
  scale_y_continuous(limits = c(0, 25)) +
  geom_jitter(alpha = 0.1) + 
  theme_minimal()


#INVARIANCE for healthy and patients
#configural 
fit_configural <- cfa(final_model, data = data_filtered, group = "condition")
summary(fit_configural, fit.measures=TRUE)
#metric
fit_metric <- cfa(final_model, data = data_filtered, group = "condition",
                  group.equal = "loadings")
summary(fit_metric, fit.measures=TRUE)
#scalar
fit_scalar <- cfa(final_model, data = data_filtered, group = "condition",
                  group.equal = c("loadings", "intercepts"))
summary(fit_scalar, fit.measures=TRUE)
#summary and final comparison
anova(fit_configural, fit_metric, fit_scalar)

data$condition <- as.character(data$condition)

data_final_model <- data_final_model %>%
  mutate(trigeminal_score_8_items = R10 + R14 + R15 + R16 + R17 + R18 + R19 + R21)

data$condition <- as.factor(data$condition)
data_filtered$condition <- as.factor(data_filtered$condition)
#horizontal density plot
conditon_plot<-ggplot(data_filtered, aes(x = trigeminal_score_8_items, fill = condition)) +
  geom_density(alpha = 0.4) +
  labs(title = "Trigeminal questionnaire - healthy and condition",
       x = "Trigeminal score",
       y = "Density") +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "orange"),
                    name = "Medical condition",
                    labels = c("Healthy", "Condition")) +
  theme_minimal()
conditon_plot

#vertical density plot
ggplot(data_filtered, aes(x = trigeminal_score_8_items)) +
  geom_density(
    data = subset(data_filtered, condition == "0"),
    aes(y = -..density.., fill = "0"),
    alpha = 0.4
  ) +
  geom_density(
    data = subset(data_filtered, condition == "1"),
    aes(y = ..density.., fill = "1"),
    alpha = 0.4
  ) +
  scale_fill_manual(
    values = c("0" = "lightgreen", "1" = "orange"),
    name = "Condition",
    labels = c("Healthy", "Condition")
  ) +
  labs(
    title = "Trigeminal questionnaire - healthy and condition",
    x = "Trigeminal",
    y = "Density"
  ) +
  coord_flip() +
  theme_minimal()

#is lateralization the same?
t.test(Lateralization ~ sex_1f_2m_t, data = data_sex_filtered) #t = 1.7256, p = 0.08506
var.test(Lateralization ~ sex_1f_2m_t, data = data_sex_filtered) #p = 0.2745
wilcox.test(Lateralization ~ sex_1f_2m_t, data = data_sex_filtered) #W = 37277, p = 0.05003 (istnieją różnice), ale cutoff centralnie
boxplot(Lateralization ~ sex_1f_2m_t, data = data_sex_filtered) 

summary(data_sex_filtered$Lateralization) #M = 14.43
hist(data_sex_filtered$Lateralization)
table(data_sex_filtered$Lateralization)

data_sex_filtered %>% 
  group_by(sex_1f_2m_t) %>% 
  summarise(cor = cor(Lateralization, trigeminal_score_8_items, use = "complete.obs"))
#correlation with trigeminal score do not differ across men and women

data_sex_filtered %>%
  group_by(sex_1f_2m_t) %>%
  summarise(cor = cor(Lateralization, Ammola, use = "complete.obs"))
#for females r = -0.106, for males r = 0.0416

data_sex_filtered %>% 
  group_by(sex_1f_2m_t) %>% 
  summarise(cor = cor(Lateralization, Identification, use = "complete.obs")) 
#for females r = -0.0499, for males r = 0.25***

data_filtered %>%
  group_by(sex_1f_2m_t) %>%
  summarise(cor = cor(Lateralization, current_smell, use = "complete.obs"))
#correlation with current smell do not differ across men and women

data_sex_filtered %>%
  group_by(sex_1f_2m_t) %>%
  summarise(cor = cor(Lateralization, self_rated_sens, use = "complete.obs"))
#correlation with self rating of nasal sensitivity do not differ across men and women

#lateralization and identification by sex
get_cor_p_ident <- function(df) {
  test <- cor.test(df$Lateralization, df$Identification, method = "pearson") 
  data.frame(
    correlation = test$estimate,
    p_value = test$p.value
  )
} 

stats_ident <- data_filtered %>%
  group_by(sex_1f_2m_t) %>%
  do(get_cor_p_ident(.)) %>%
  mutate(
    p_human = ifelse(
      p_value < 0.001,
      "p < 0.001",
      paste0("p = ", round(p_value, 3))
    )
  )
ggplot(data_filtered, aes(x = Lateralization, y = Identification, color = sex_1f_2m_t)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ sex_1f_2m_t) +
  theme_minimal() +
  labs(
    title = "Lateralization and Identification by sex",
    x = "Lateralization",
    y = "Identification",
    color = "Sex"
  ) +
  geom_text(
    data = stats_ident,
    aes(
      x = Inf, y = -Inf,
      label = paste0("r = ", correlation, "\n", p_human)
    ),
    inherit.aes = FALSE,
    hjust = 1.1,
    vjust = -0.1,
    size = 4,
    color = "black"
  )
#lateralization and ammola by sex
get_cor_p_ammola <- function(df) { 
  test <- cor.test(df$Lateralization, df$Ammola, method = "pearson")
  data.frame(
    correlation = test$estimate,
    p_value = test$p.value
  )
}

stats_ammola <- data_filtered %>%
  group_by(sex_1f_2m_t) %>%
  do(get_cor_p_ammola(.)) %>%
  mutate(
    p_human = ifelse(
      p_value < 0.001,
      "p < 0.001",
      paste0("p = ", round(p_value, 3))
    )
  )
ggplot(data_filtered, aes(x = Lateralization, y = Ammola, color = sex_1f_2m_t)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ sex_1f_2m_t) +
  theme_minimal() +
  labs(
    title = "Lateralization and Ammola by sex",
    x = "Lateralization",
    y = "Ammola",
    color = "Sex"
  ) +
  geom_text(
    data = stats_ammola,
    aes(
      x = Inf, y = -Inf,
      label = paste0("r = ", correlation, "\n", p_human)
    ),
    inherit.aes = FALSE,
    hjust = 1.1,
    vjust = -0.1,
    size = 4,
    color = "black"
  )
data$Lateralization <- as.numeric(data$Lateralization)
#correlation lateralization across sexes
vars_to_correlate <- c("Ammola", "Identification", "current_smell", "self_rated_sens", "trigeminal_score_8_items", "age")

get_cor_p <- function(df, var) {
  df2 <- df %>% select(Lateralization, all_of(var)) %>% drop_na()
  if(nrow(df2) < 3) {
    return(tibble(variable = var, r = NA_real_, p = NA_real_, p_human = NA_character_))
  }
  test <- cor.test(df2$Lateralization, df2[[var]], method = "pearson")
  tibble(
    variable = var,
    r = round(test$estimate, 3),
    p = test$p.value,
    p_human = ifelse(test$p.value < 0.001, "< 0.001", paste0("= ", round(test$p.value, 3)))
  )
}

results <- data_sex_filtered %>%
  group_by(sex_1f_2m_t) %>%
  group_modify(~ map_dfr(vars_to_correlate, function(var) get_cor_p(.x, var))) %>%
  ungroup()

print(results)

model_trig <- lm(trigeminal_score_8_items ~ Lateralization * sex_1f_2m_t, data = data_sex_filtered)
summary(model_trig) 

model_ident <- lm(Identification ~ Lateralization * sex_1f_2m_t, data = data_sex_filtered)
summary(model_ident) 

model_self <- lm(self_rated_sens ~ Lateralization * sex_1f_2m_t, data = data_sex_filtered)
summary(model_self) 

model_curent <- lm(current_smell ~ Lateralization * sex_1f_2m_t, data = data_sex_filtered)
summary(model_curent) 


ggplot(data_sex_filtered, aes(x = Lateralization, fill = sex_1f_2m_t)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Lateralization by sex",
    x = "Lateralization",
    fill = "Sex"
  ) +
  theme_minimal()

data_sex_filtered %>%
  group_by(sex_1f_2m_t) %>%
  summarise(
    n = sum(!is.na(Lateralization)),
    mean = mean(Lateralization, na.rm = TRUE),
    sd = sd(Lateralization, na.rm = TRUE),
    min = min(Lateralization, na.rm = TRUE),
    p10 = quantile(Lateralization, 0.10, na.rm = TRUE),
    p25 = quantile(Lateralization, 0.25, na.rm = TRUE),
    p50 = quantile(Lateralization, 0.50, na.rm = TRUE),
    p75 = quantile(Lateralization, 0.75, na.rm = TRUE),
    p90 = quantile(Lateralization, 0.90, na.rm = TRUE),
    max = max(Lateralization, na.rm = TRUE),
    .groups = "drop"
  )

data_sex_filtered %>%
  group_by(sex_1f_2m_t) %>%
  summarise(
    p10 = quantile(Lateralization, 0.10, na.rm = TRUE),
    p90 = quantile(Lateralization, 0.90, na.rm = TRUE)
  ) %>%
  left_join(data_filtered, by = "sex_1f_2m_t") %>%
  group_by(sex_1f_2m_t) %>%
  summarise(
    n_below_p10 = sum(Lateralization <= p10, na.rm = TRUE),
    n_above_p90 = sum(Lateralization >= p90, na.rm = TRUE)
  )

# percentiles for sexes
percentiles <- data_sex_filtered %>%
  group_by(sex_1f_2m_t) %>%
  summarise(
    p10 = quantile(Lateralization, 0.10, na.rm = TRUE),
    p90 = quantile(Lateralization, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(data_sex_filtered, aes(x = Lateralization, fill = sex_1f_2m_t)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = percentiles, 
             mapping = aes(xintercept = p10, color = sex_1f_2m_t), 
             linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  geom_vline(data = percentiles, 
             mapping = aes(xintercept = p90, color = sex_1f_2m_t), 
             linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  labs(
    title = "Lateralization by sex with 10th and 90th percentiles",
    x = "Lateralization",
    fill = "Sex",
    color = "Percentile\n(Sex)"
  ) +
  theme_minimal()

percentiles_by_sex <- data_sex_filtered %>%
  group_by(sex_1f_2m_t) %>%
  summarise(
    p10 = quantile(Lateralization, 0.10, na.rm = TRUE),
    p90 = quantile(Lateralization, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

#lateralization by condition
ggplot(data_sex_filtered, aes(x = Lateralization, fill = condition)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Lateralization by condition group",
    x = "Lateralization",
    y = "Density",
    fill = "Condition"
  ) +
  theme_minimal()

#correlation trigemnial questionnare in men and women
data_sex_filtered %>%
  group_by(sex_1f_2m_t) %>%
  summarise(cor = cor(trigeminal_score_8_items, Ammola, use = "complete.obs"))
#for females r = 0.107*, for males r = 0.11*

#p
data_sex_filtered %>%
  filter(sex_1f_2m_t %in% c("female", "male")) %>%
  group_by(sex_1f_2m_t) %>%
  group_modify(~{
    test <- cor.test(.x$trigeminal_score_8_items, .x$Ammola, use = "complete.obs")
    tibble(
      r = round(test$estimate, 3),
      p = signif(test$p.value, digits = 4)
    )
  })
#difference between correlation in groups nonsignificant

data_sex_filtered %>% 
  group_by(sex_1f_2m_t) %>% 
  summarise(cor = cor(trigeminal_score_8_items, Identification, use = "complete.obs")) 
#for females r = 0.0114, for males r = -0.0576

#p
data_sex_filtered %>%
  filter(sex_1f_2m_t %in% c("female", "male")) %>%
  group_by(sex_1f_2m_t) %>%
  group_modify(~{
    test <- cor.test(.x$trigeminal_score_8_items, .x$Identification, use = "complete.obs")
    tibble(
      r = round(test$estimate, 3),
      p = signif(test$p.value, digits = 4)
    )
  })

data_sex_filtered %>% 
  group_by(sex_1f_2m_t) %>% 
  summarise(cor = cor(trigeminal_score_8_items, Lateralization, use = "complete.obs")) 
#for females r = 0.0114, for males r = -0.0576

#p
data_sex_filtered %>%
  filter(sex_1f_2m_t %in% c("female", "male")) %>%
  group_by(sex_1f_2m_t) %>%
  group_modify(~{
    test <- cor.test(.x$trigeminal_score_8_items, .x$Lateralization, use = "complete.obs")
    tibble(
      r = round(test$estimate, 3),
      p = signif(test$p.value, digits = 4)
    )
  })
#difference between correlation in groups nonsignificant (I have used the same piece of code that
#I have used for self-rated sensitivity below. There is no point keeping al variants, so I only leave one
#and if needed proper variable can be written there)


data_sex_filtered %>%
  group_by(sex_1f_2m_t) %>%
  summarise(cor = cor(trigeminal_score_8_items, current_smell, use = "complete.obs"))
#for females r = 0.105*, for males r = 0.126*

#p
data_sex_filtered %>%
  filter(sex_1f_2m_t %in% c("female", "male")) %>%
  group_by(sex_1f_2m_t) %>%
  group_modify(~{
    test <- cor.test(.x$trigeminal_score_8_items, .x$current_smell, use = "complete.obs")
    tibble(
      r = round(test$estimate, 3),
      p = signif(test$p.value, digits = 4)
    )
  })
#difference between correlation in groups nonsignificant

data_sex_filtered %>%
  group_by(sex_1f_2m_t) %>%
  summarise(cor = cor(trigeminal_score_8_items, self_rated_sens, use = "complete.obs"))
#for females r = 0.412***, for males 0.273*** 

data_sex_filtered %>%
  filter(sex_1f_2m_t %in% c("female", "male")) %>%
  group_by(sex_1f_2m_t) %>%
  group_modify(~{
    test <- cor.test(.x$trigeminal_score_8_items, .x$self_rated_sens, use = "complete.obs")
    tibble(
      r = round(test$estimate, 3),
      p = signif(test$p.value, digits = 4)
    )
  })
#difference between men and women's corelation is significant, p = 0.0174 (the code below)
female_data <- data_sex_filtered %>% filter(sex_1f_2m_t == "female")
male_data   <- data_sex_filtered %>% filter(sex_1f_2m_t == "male")


r1 <- cor(female_data$trigeminal_score_8_items, female_data$Identification, use = "complete.obs")
r2 <- cor(male_data$trigeminal_score_8_items, male_data$Identification, use = "complete.obs")
n1 <- sum(complete.cases(female_data$trigeminal_score_8_items, female_data$Identification))
n2 <- sum(complete.cases(male_data$trigeminal_score_8_items, male_data$Identification))

z1 <- atanh(r1)
z2 <- atanh(r2)
se_diff <- sqrt(1/(n1 - 3) + 1/(n2 - 3))
z <- (z1 - z2) / se_diff
p <- 2 * (1 - pnorm(abs(z)))

tibble(
  r_female = round(r1, 3),
  r_male   = round(r2, 3),
  z        = round(z, 3),
  p_value  = signif(p, 3)
)

ggplot(data = data, aes(x = trigeminal_score_8_items, y = age)) + geom_jitter(alpha = 0.2)
ggplot(data = data, aes(x = Lateralization, y = age)) + geom_jitter(alpha = 0.2)

ggplot(data = data, aes(x = trigeminal_score_8_items, y = Lateralization, color = age)) +
  geom_jitter(alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "navy", name = "Age") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    x = "Trigeminal questionnaire score",
    y = "Lateralization",
    title = "Lateralization and trigeminal questionnaire score"
  ) +
  theme_minimal()

plot_data_general <- data %>%
  filter(
    !is.na(trigeminal_score_8_items),
    !is.na(Lateralization),
    !is.na(age))


#data
data_clean <- data %>%
  drop_na(Lateralization, trigeminal_score_8_items)

#percentiles
percentiles <- data_clean %>%
  summarise(
    p5  = quantile(Lateralization, probs = 0.05),
    p10 = quantile(Lateralization, probs = 0.10),
    p25 = quantile(Lateralization, probs = 0.25),
    p50 = quantile(Lateralization, probs = 0.50),
    p90 = quantile(Lateralization, probs = 0.90),
    p95 = quantile(Lateralization, probs = 0.95)
  )

data_clean <- data_clean %>%
  mutate(Lateralization_per = case_when(
    Lateralization <= percentiles$p5 ~ "≤5%",
    Lateralization <= percentiles$p10 ~ "6–10%",
    Lateralization <= percentiles$p25 ~ "11–25%",
    Lateralization <= percentiles$p50 ~ "26–50%",
    Lateralization <= percentiles$p90 ~ "51–90%",
    Lateralization <= percentiles$p95 ~ "91–95%",
    Lateralization >  percentiles$p95 ~ ">95%",
    TRUE ~ NA_character_
  )) %>%
  mutate(Lateralization_per = factor(
    Lateralization_per,
    levels = c("≤5%", "6–10%", "11–25%", "26–50%", "51–90%", "91–95%", ">95%"))
  )

#function fot t tests
compare_groups <- function(data, pair, group_var, outcome_var) {
  group1 <- data %>% filter(!!sym(group_var) == pair[1]) %>% pull(!!sym(outcome_var))
  group2 <- data %>% filter(!!sym(group_var) == pair[2]) %>% pull(!!sym(outcome_var))
  
  t_test <- t.test(group1, group2, var.equal = FALSE)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    t = round(t_test$statistic, 3),
    df = round(t_test$parameter, 1),
    p = signif(t_test$p.value, 3),
    Mean1 = round(mean(group1, na.rm = TRUE), 2),
    Mean2 = round(mean(group2, na.rm = TRUE), 2),
    SD1 = round(sd(group1, na.rm = TRUE), 2),
    SD2 = round(sd(group2, na.rm = TRUE), 2)
  )
}

#stepwise and extreme t-test
pairs_stepwise <- list(
  c("≤5%", "6–10%"),
  c("6–10%", "11–25%"),
  c("11–25%", "26–50%"),
  c("26–50%", "51–90%"),
  c("51–90%", "91–95%"),
  c("91–95%", ">95%")
)
pair_extreme <- list(c("≤5%", ">95%"))

target_variable <- "trigeminal_score_8_items"

#results
results_stepwise <- lapply(pairs_stepwise, compare_groups, data = data_clean, group_var = "Lateralization_per", outcome_var = target_variable)
results_extreme <- lapply(pair_extreme, compare_groups, data = data_clean, group_var = "Lateralization_per", outcome_var = target_variable)
results_df <- bind_rows(results_stepwise, results_extreme)
print(results_df)

# Kruskal + post-hoc for percentiles
kruskal_result <- data_clean %>%
  kruskal_test(trigeminal_score_8_items ~ Lateralization_per)

posthoc_result <- data_clean %>%
  dunn_test(trigeminal_score_8_items ~ Lateralization_per, p.adjust.method = "BH")


ggplot(data_clean, aes(x = Lateralization_per, y = trigeminal_score_8_items)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +
  geom_jitter(width = 0.1, alpha = 0.4, color = "darkblue") +
  labs(x = "Lateralization_percentile",
       y = "Trigeminal questionnaire score",
       title = "TriFunQ by lateralization percentile") +
  theme_minimal(base_size = 13)

print(kruskal_result)
posthoc_result %>%
  mutate(across(where(is.numeric), ~round(.x, 3))) %>%
  select(group1, group2, statistic, p, p.adj, p.adj.signif) %>%
  arrange(p.adj) %>%
  print(n = Inf)


# Kruskal + post-hoc for norms
kruskal_res <- data_clean %>%
  kruskal_test(trigeminal_score_8_items ~ Lateralization_cat)

posthoc_res <- data_clean %>%
  dunn_test(trigeminal_score_8_items ~ Lateralization_cat, p.adjust.method = "BH")

print(kruskal_res)
print(posthoc_res, n = 6)


ggplot(data_clean, aes(x = Lateralization_cat, y = trigeminal_score_8_items, fill = Lateralization_cat)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "Lateralization", y = "Trigeminal questionnaire score",
       title = "TriFunQ by lateralization") +
  scale_fill_manual(values = c("decreased" = "lightcoral", "grey zone" = "grey", "normal" = "lightgreen")) +
  theme_minimal() +
  theme(legend.position = "none")

#t tests
pairs_cat <- list(
  c("decreased", "grey zone"),
  c("grey zone", "normal"),
  c("decreased", "normal")
)

results_cat <- lapply(pairs_cat, compare_groups, data = data_clean, group_var = "Lateralization_cat", outcome_var = target_variable)
results_df_cat <- bind_rows(results_cat)
print(results_df_cat)


data_filtered <- data %>%
  filter(!is.na(trigeminal_score_8_items))

age_subgroup_healthy <- data_filtered %>%
  filter(
    age >= 18,
    age <= 30,
    condition == "0",
    covid %in% c("j", "n"),
    sex_1f_2m_t %in% c("female", "male")
  )

#TERCILE LATERALIZATION
tercile_1_3_healthy <- age_subgroup_healthy %>%
  filter(lateral_tercile %in% c(1, 3))

#t tests
t.test(Lateralization ~ lateral_tercile, data = tercile_1_3_healthy)
t.test(trigeminal_score_8_items ~ lateral_tercile, data = tercile_1_3_healthy)

ggplot(tercile_1_3_healthy, aes(x = factor(lateral_tercile), y = trigeminal_score_8_items)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0) +
  scale_x_discrete(labels = c("1" = "Low", "3" = "High")) +
  labs(x = "Tercile lateralization", y = "Trigeminal score", title = "Boxplot: lateralization tercile vs TriFunQ") +
  theme_minimal()

#trigeminal score and lateralization in subgroup of healthy and young
ggplot(data = age_subgroup_healthy, aes(x = trigeminal_score_8_items, y = Lateralization, color = age)) +
  geom_jitter(alpha = 0.6) +
  scale_color_gradient(low = "lightgreen", high = "darkgreen", name = "Age") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    x = "Trigeminal questionnaire score",
    y = "Lateralization",
    title = "Lateralization and trigeminal questionnaire score"
  ) +
  theme_minimal()

#frequencies and summary
tercile_1_3_healthy %>%
  mutate(lateral_group = ifelse(lateral_tercile == 1, "low", "high")) %>%
  count(lateral_group)

age_subgroup_healthy %>%
  count(Lateralization_tercile) %>%
  arrange(factor(Lateralization_tercile, levels = c("low", "medium", "high")))

summary(age_subgroup_healthy$trigeminal_score_8_items)
ggplot(age_subgroup_healthy, aes(x = trigeminal_score_8_items)) + geom_density()

summary(age_subgroup_healthy$Lateralization)
ggplot(age_subgroup_healthy, aes(x = Lateralization)) + geom_density()

#correlations
correlation_matrix_healthy <- age_subgroup_healthy %>%
  select(trigeminal_score_8_items, age, Lateralization, Identification, Ammola, self_rated_sens, current_smell)

cor_results_healthy <- correlation_matrix_healthy %>%
  cor_test(method = "spearman") %>%
  mutate(
    r = round(cor, 3),
    p_value = case_when(p < 0.001 ~ "< .001", TRUE ~ paste0("= ", round(p, 3)))
  )
print(cor_results_healthy, n = 50)

#by sex
vars_to_test <- c("Ammola", "Identification", "Lateralization", "current_smell", "self_rated_sens")

cor_by_sex <- function(var) {
  age_subgroup_healthy %>%
    filter(sex_1f_2m_t %in% c("female", "male")) %>%
    group_by(sex_1f_2m_t) %>%
    group_modify(~{
      test <- cor.test(.x$trigeminal_score_8_items, .x[[var]], use = "complete.obs")
      tibble(variable = var, r = round(test$estimate, 3), p = signif(test$p.value, digits = 4))
    })
}

cor_sexwise_results <- bind_rows(lapply(vars_to_test, cor_by_sex))
print(cor_sexwise_results)

#sex differences across correlations
compute_diff_corr <- function(var) {
  female <- age_subgroup_healthy %>% filter(sex_1f_2m_t == "female")
  male   <- age_subgroup_healthy %>% filter(sex_1f_2m_t == "male")
  
  r1 <- cor(female$trigeminal_score_8_items, female[[var]], use = "complete.obs")
  r2 <- cor(male$trigeminal_score_8_items, male[[var]], use = "complete.obs")
  n1 <- sum(complete.cases(female$trigeminal_score_8_items, female[[var]]))
  n2 <- sum(complete.cases(male$trigeminal_score_8_items, male[[var]]))
  
  z1 <- atanh(r1)
  z2 <- atanh(r2)
  se_diff <- sqrt(1/(n1 - 3) + 1/(n2 - 3))
  z <- (z1 - z2) / se_diff
  p <- 2 * (1 - pnorm(abs(z)))
  
  tibble(variable = var, r_female = round(r1, 3), r_male = round(r2, 3), z = round(z, 3), p_value = signif(p, 3))
}

diff_corr_results <- compute_diff_corr("Ammola")
print(diff_corr_results)

#t tests in sex
test_vars <- c("Lateralization", "Ammola", "Identification", "trigeminal_score_8_items")
sex_labels <- c("female", "male")

for (v in test_vars) {
  print(t.test(as.formula(paste(v, "~ sex_1f_2m_t")), data = age_subgroup_healthy))
  print(cohens_d(as.formula(paste(v, "~ sex_1f_2m_t")), data = age_subgroup_healthy))
  
  age_subgroup_healthy %>%
    filter(sex_1f_2m_t %in% sex_labels) %>%
    ggplot(aes_string(x = "sex_1f_2m_t", y = v)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", v, "by sex")) +
    print()
}

#covid
age_subgroup_healthy$covid <- age_subgroup_healthy$Have.you.already.had.Covid.

for (v in test_vars) {
  print(t.test(as.formula(paste(v, "~ covid")), data = age_subgroup_healthy))
  print(cohens_d(as.formula(paste(v, "~ covid")), data = age_subgroup_healthy))
}

#invariance in sex for subsample
final_model <- 'f1 =~ R10 + R14 + R15 + R16 + R17 + R18 + R19 + R21'

data_final_model_healthy <- age_subgroup_healthy %>%
  select(R10, R14, R15, R16, R17, R18, R19, R21, sex_1f_2m_t) %>%
  na.omit() %>%
  mutate(sex_1f_2m_t = factor(sex_1f_2m_t, levels = c("female", "male")))

fit_configural <- cfa(final_model, data = data_final_model_healthy, group = "sex_1f_2m_t")
fit_metric     <- cfa(final_model, data = data_final_model_healthy, group = "sex_1f_2m_t", group.equal = "loadings")
fit_scalar     <- cfa(final_model, data = data_final_model_healthy, group = "sex_1f_2m_t", group.equal = c("loadings", "intercepts"))

anova(fit_configural, fit_metric, fit_scalar)

data_final_model_healthy <- data_final_model_healthy %>%
  mutate(score = R10 + R14 + R15 + R16 + R17 + R18 + R19 + R21,
         trigeminal_score_8_items = score)
#norms for men and women
summary_stats <- function(df) {
  df %>% summarise(
    mean = mean(score),
    sd = sd(score),
    p10 = quantile(score, 0.1),
    p25 = quantile(score, 0.25),
    p50 = quantile(score, 0.5),
    p75 = quantile(score, 0.75),
    p90 = quantile(score, 0.9)
  )
}

summary_female <- summary_stats(data_final_model_healthy %>% filter(sex_1f_2m_t == "female"))
summary_male   <- summary_stats(data_final_model_healthy %>% filter(sex_1f_2m_t == "male"))

print(summary_female)
print(summary_male)

ggplot(data_final_model_healthy, aes(x = trigeminal_score_8_items, fill = sex_1f_2m_t)) +
  geom_density(alpha = 0.4) +
  labs(title = "Trigeminal questionnaire by sex", x = "Trigeminal score", y = "Density") +
  scale_fill_discrete(name = "Sex") +
  theme_minimal()


#lateralization percentiles and norms
data_clean_healthy <- age_subgroup_healthy %>%
  drop_na(Lateralization, trigeminal_score_8_items)

percentiles_healthy <- data_clean_healthy %>%
  summarise(
    p5  = quantile(Lateralization, probs = 0.05),
    p10 = quantile(Lateralization, probs = 0.10),
    p25 = quantile(Lateralization, probs = 0.25),
    p50 = quantile(Lateralization, probs = 0.50),
    p90 = quantile(Lateralization, probs = 0.90),
    p95 = quantile(Lateralization, probs = 0.95)
  )

data_clean_healthy <- data_clean_healthy %>%
  mutate(Lateralization_per_healthy = case_when(
    Lateralization <= percentiles_healthy$p5 ~ "≤5%",
    Lateralization <= percentiles_healthy$p10 ~ "6–10%",
    Lateralization <= percentiles_healthy$p25 ~ "11–25%",
    Lateralization <= percentiles_healthy$p50 ~ "26–50%",
    Lateralization <= percentiles_healthy$p90 ~ "51–90%",
    Lateralization <= percentiles_healthy$p95 ~ "91–95%",
    Lateralization >  percentiles_healthy$p95 ~ ">95%",
    TRUE ~ NA_character_
  )) %>%
  mutate(Lateralization_per_healthy = factor(
    Lateralization_per_healthy,
    levels = c("≤5%", "6–10%", "11–25%", "26–50%", "51–90%", "91–95%", ">95%"))
  )

#group comparison
compare_groups <- function(data, pair, group_var, outcome_var) {
  group1 <- data %>% filter(!!sym(group_var) == pair[1]) %>% pull(!!sym(outcome_var))
  group2 <- data %>% filter(!!sym(group_var) == pair[2]) %>% pull(!!sym(outcome_var))
  
  t_test <- t.test(group1, group2, var.equal = FALSE)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    t = round(t_test$statistic, 3),
    df = round(t_test$parameter, 1),
    p = signif(t_test$p.value, 3),
    Mean1 = round(mean(group1, na.rm = TRUE), 2),
    Mean2 = round(mean(group2, na.rm = TRUE), 2),
    SD1 = round(sd(group1, na.rm = TRUE), 2),
    SD2 = round(sd(group2, na.rm = TRUE), 2)
  )
}

#t testy
pairs_stepwise_healthy <- list(
  c("≤5%", "6–10%"),
  c("6–10%", "11–25%"),
  c("11–25%", "26–50%"),
  c("26–50%", "51–90%"),
  c("51–90%", "91–95%"),
  c("91–95%", ">95%")
)

pair_extreme_healthy <- list(c("≤5%", ">95%"))

target_variable <- "trigeminal_score_8_items"

results_stepwise_healthy <- lapply(
  pairs_stepwise_healthy,
  compare_groups,
  data = data_clean_healthy,
  group_var = "Lateralization_per_healthy",
  outcome_var = target_variable
)

results_extreme_healthy <- lapply(
  pair_extreme_healthy,
  compare_groups,
  data = data_clean_healthy,
  group_var = "Lateralization_per_healthy",
  outcome_var = target_variable
)

results_df_healthy <- bind_rows(results_stepwise_healthy, results_extreme_healthy)
print(results_df_healthy)

#kruskall and posthoc
kruskal_result_healthy <- data_clean_healthy %>%
  kruskal_test(trigeminal_score_8_items ~ Lateralization_per_healthy)

posthoc_result_healthy <- data_clean_healthy %>%
  dunn_test(trigeminal_score_8_items ~ Lateralization_per_healthy, p.adjust.method = "BH")

print(kruskal_result_healthy)

posthoc_result_healthy %>%
  mutate(across(where(is.numeric), ~round(.x, 3))) %>%
  select(group1, group2, statistic, p, p.adj, p.adj.signif) %>%
  arrange(p.adj) %>%
  print(n = Inf)

ggplot(data_clean_healthy, aes(x = Lateralization_per_healthy, y = trigeminal_score_8_items)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +
  geom_jitter(width = 0.1, alpha = 0.4, color = "darkblue") +
  labs(x = "Lateralization percentile",
       y = "Trigeminal questionnaire score",
       title = "TriFunQ by lateralization percentile (healthy subsample)") +
  theme_minimal(base_size = 13)

#norms
data_healthy_cat <- age_subgroup_healthy %>%
  mutate(
    Lateralization_cat_healthy = case_when(
      !is.na(Lateralization) & Lateralization >= 0 & Lateralization <= 10 ~ "decreased",
      Lateralization >= 11 & Lateralization <= 14 ~ "grey zone",
      Lateralization >= 15 ~ "normal",
      TRUE ~ NA_character_
    ),
    Lateralization_cat_healthy = factor(
      Lateralization_cat_healthy,
      levels = c("decreased", "grey zone", "normal")
    )
  )

data_clean_cat_healthy <- data_healthy_cat %>%
  drop_na(Lateralization_cat_healthy, trigeminal_score_8_items)

kruskal_res_healthy <- data_clean_cat_healthy %>%
  kruskal_test(trigeminal_score_8_items ~ Lateralization_cat_healthy)

posthoc_res_healthy <- data_clean_cat_healthy %>%
  dunn_test(trigeminal_score_8_items ~ Lateralization_cat_healthy, p.adjust.method = "BH")

print(kruskal_res_healthy)
print(posthoc_res_healthy, n = 6)


ggplot(data_clean_cat_healthy, aes(x = Lateralization_cat_healthy, y = trigeminal_score_8_items, fill = Lateralization_cat_healthy)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "Lateralization category", y = "Trigeminal questionnaire score",
       title = "TriFunQ by lateralization category (healthy subsample)") +
  scale_fill_manual(values = c("decreased" = "lightcoral", "grey zone" = "grey", "normal" = "lightgreen")) +
  theme_minimal() +
  theme(legend.position = "none")

#t test norms
pairs_cat_healthy <- list(
  c("decreased", "grey zone"),
  c("grey zone", "normal"),
  c("decreased", "normal")
)

results_cat_healthy <- lapply(
  pairs_cat_healthy,
  compare_groups,
  data = data_clean_cat_healthy,
  group_var = "Lateralization_cat_healthy",
  outcome_var = target_variable
)

results_df_cat_healthy <- bind_rows(results_cat_healthy)
print(results_df_cat_healthy)


data_filtered$current_smell <- as.numeric(data_filtered$Current.smell)

age_subgroup <- data_filtered %>%
  filter(
    age >= 18,
    age <= 30,
    covid %in% c("j", "n"),
    condition %in% c("0", "1"),
    sex_1f_2m_t %in% c("male", "female")
  ) %>%
  mutate(
    lateral_tercile = ntile(Lateralization, 3)
  )

tercile_1_3 <- age_subgroup %>%
  filter(lateral_tercile %in% c(1, 3))

t.test(Lateralization ~ lateral_tercile, data = tercile_1_3)

ggplot(tercile_1_3, aes(x = factor(lateral_tercile), y = Lateralization)) +
  geom_boxplot(fill = "#69b3a2", alpha = 0.7) +
  labs(
    x = "Tercile lateralization",
    y = "Lateralization",
    title = "Boxplot lateralization tercile"
  ) +
  scale_x_discrete(labels = c("1" = "Low", "3" = "High")) +
  theme_minimal()

t.test(trigeminal_score_8_items ~ lateral_tercile, data = tercile_1_3)

ggplot(tercile_1_3, aes(x = factor(lateral_tercile), y = trigeminal_score_8_items)) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0.0) +
  labs(
    x = "Tercile lateralization",
    y = "Trigeminal questionnaire score",
    title = "Boxplot lateralization tercile and trigeminal questionnaire"
  ) +
  scale_x_discrete(labels = c("1" = "Low", "3" = "High")) +
  theme_minimal()

tercile_1_3 %>%
  filter(lateral_tercile %in% c(1, 3)) %>%
  mutate(lateral_group = ifelse(lateral_tercile == 1, "low", "high")) %>%
  group_by(lateral_group) %>%
  summarise(n = n())

data_filtered %>%
  group_by(Lateralization_tercile) %>%
  summarise(n = n()) %>%
  arrange(factor(Lateralization_tercile, levels = c("low", "medium", "high")))

summary(age_subgroup$trigeminal_score_8_items)
ggplot(age_subgroup, aes(x = trigeminal_score_8_items)) + geom_density()

summary(age_subgroup$Lateralization)
ggplot(age_subgroup, aes(x = Lateralization)) + geom_density()


#correlations only in subsample of 18-30
correlation_matrix_0 <- data.frame(age_subgroup$trigeminal_score_8_items, age_subgroup$age,
                                   age_subgroup$Lateralization, age_subgroup$Identification, age_subgroup$Ammola, 
                                   age_subgroup$self_rated_sens, age_subgroup$current_smell)
correlation_matrix <- na.omit(correlation_matrix_0)

correlation_matrix <- age_subgroup %>%
  select(trigeminal_score_8_items, age, 
         Lateralization, Identification, 
         Ammola, self_rated_sens, current_smell)

cor_results <- correlation_matrix %>%
  cor_test(method = "spearman") %>%
  mutate(
    r = round(cor, 3),
    p_value = case_when(
      p < 0.001 ~ "< .001",
      TRUE ~ paste0("= ", round(p, 3))
    )
  )
print(cor_results, n = 50)

#correlations by sex of trigeminal questionnaire with:
#ammola
age_subgroup %>%
  filter(sex_1f_2m_t %in% c("female", "male")) %>%
  group_by(sex_1f_2m_t) %>%
  group_modify(~{
    test <- cor.test(.x$trigeminal_score_8_items, .x$Ammola, use = "complete.obs")
    tibble(
      r = round(test$estimate, 3),
      p = signif(test$p.value, digits = 4)
    )
  })
#female r = 0.138, p = 0.0739; male r = 0.155, p = 0.126


#Identificaiton
age_subgroup%>%
  filter(sex_1f_2m_t %in% c("female", "male")) %>%
  group_by(sex_1f_2m_t) %>%
  group_modify(~{
    test <- cor.test(.x$trigeminal_score_8_items, .x$Identification, use = "complete.obs")
    tibble(
      r = round(test$estimate, 3),
      p = signif(test$p.value, digits = 4)
    )
  })
#female = -0.006, p = 0.942; male r = 0.012, p = 0.906

#Lateralization
age_subgroup %>%
  filter(sex_1f_2m_t %in% c("female", "male")) %>%
  group_by(sex_1f_2m_t) %>%
  group_modify(~{
    test <- cor.test(.x$trigeminal_score_8_items, .x$Lateralization, use = "complete.obs")
    tibble(
      r = round(test$estimate, 3),
      p = signif(test$p.value, digits = 4)
    )
  })
#female r = 0.184, p = 0.0961; male = 0.07, p = 0.586

#current smell
age_subgroup %>%
  filter(sex_1f_2m_t %in% c("female", "male")) %>%
  group_by(sex_1f_2m_t) %>%
  group_modify(~{
    test <- cor.test(.x$trigeminal_score_8_items, .x$current_smell, use = "complete.obs")
    tibble(
      r = round(test$estimate, 3),
      p = signif(test$p.value, digits = 4)
    )
  })
#female r = 0.253***, male = -0.003, p = 0.979

#self-rated sense of smell
age_subgroup %>%
  filter(sex_1f_2m_t %in% c("female", "male")) %>%
  group_by(sex_1f_2m_t) %>%
  group_modify(~{
    test <- cor.test(.x$trigeminal_score_8_items, .x$self_rated_sens, use = "complete.obs")
    tibble(
      r = round(test$estimate, 3),
      p = signif(test$p.value, digits = 4)
    )
  })
#female r = 0.428***, male r = 0.298**


#difference between men and women's corelation is significant, p = 0.0174 (the code below)
female_data <- age_subgroup %>% filter(sex_1f_2m_t == "female")
male_data   <- age_subgroup %>% filter(sex_1f_2m_t == "male")


r1 <- cor(female_data$trigeminal_score_8_items, female_data$self_rated_sens, use = "complete.obs")
r2 <- cor(male_data$trigeminal_score_8_items, male_data$self_rated_sens, use = "complete.obs")
n1 <- sum(complete.cases(female_data$trigeminal_score_8_items, female_data$self_rated_sens))
n2 <- sum(complete.cases(male_data$trigeminal_score_8_items, male_data$self_rated_sens))

z1 <- atanh(r1)
z2 <- atanh(r2)
se_diff <- sqrt(1/(n1 - 3) + 1/(n2 - 3))
z <- (z1 - z2) / se_diff
p <- 2 * (1 - pnorm(abs(z)))

tibble(
  r_female = round(r1, 3),
  r_male   = round(r2, 3),
  z        = round(z, 3),
  p_value  = signif(p, 3)
)

#across genders
t.test(Lateralization ~ sex_1f_2m_t, data = age_subgroup) #t = 0.52047 p = 0.6036
ggplot(data = age_subgroup, aes(x = sex_1f_2m_t, y = Lateralization)) + geom_boxplot()
cohens_d(Lateralization~sex_1f_2m_t, data = age_subgroup) #0.08

t.test(Ammola~sex_1f_2m_t, data = age_subgroup) #t = 4.52, p < 0.001
age_subgroup %>%
  filter(sex_1f_2m_t %in% c("male", "female")) %>%
  ggplot(aes(x = sex_1f_2m_t, y = Ammola)) +
  geom_boxplot()
cohens_d(Ammola~sex_1f_2m_t, data = age_subgroup) #0.61 (moderate)

t.test(Identification~sex_1f_2m_t, data = age_subgroup) #t = 0.085, p = 0.9322
ggplot(data = age_subgroup, aes(x = sex_1f_2m_t, y = Identification)) + geom_boxplot()
cohens_d(Identification~sex_1f_2m_t, data = age_subgroup) #0.0109

t.test(trigeminal_score_8_items~sex_1f_2m_t, data = age_subgroup) #t = 2.8714, p < 0.01
age_subgroup %>%
  filter(sex_1f_2m_t %in% c("male", "female")) %>% 
  ggplot(aes(x = sex_1f_2m_t, y = trigeminal_score_8_items)) + geom_boxplot()
cohens_d(trigeminal_score_8_items~sex_1f_2m_t, data = age_subgroup) #0.359

#healthy and conditions
t.test(trigeminal_score_8_items~condition, data = age_subgroup) #t = -2.26, p < 0.05
cohens_d(trigeminal_score_8_items~condition, data = age_subgroup) #-0.394

t.test(Lateralization~condition, data = age_subgroup) #t = 0.018, p = 0.9854
cohens_d(Lateralization~condition, data = age_subgroup) # 0.00458

t.test(Identification~condition, data = age_subgroup) #t = -1.0282, p = 0.3079
cohens_d(Identification~condition, data = age_subgroup) # -0.164

t.test(Ammola~condition, data = age_subgroup) #t = --0.1163, p = 0.9079
cohens_d(Ammola~condition, data = age_subgroup) #-0.0216

#covid
age_subgroup$covid <- age_subgroup$Have.you.already.had.Covid.

t.test(trigeminal_score_8_items~covid, data = age_subgroup) #t = -2.6176, p < 0.05
cohens_d(trigeminal_score_8_items~covid, data = age_subgroup) #-0.44

t.test(Lateralization~covid, data = age_subgroup) #t = -0.011, p = 0.9914
cohens_d(Lateralization~covid, data = age_subgroup) # 0.00262

t.test(Identification~covid, data = age_subgroup) #t = 0.202, p = 0.8408
cohens_d(Identification~covid, data = age_subgroup) # 0.0368

t.test(Ammola~covid, data = age_subgroup) #t = -1.3544, p = 0.1803
cohens_d(Ammola~covid, data = age_subgroup) #-0.211

final_model <- '
f1 =~ R10 + R14 + R15 + R16 + R17 + R18 + R19 + R21
'

data_final_model <- age_subgroup %>%
  select(R10, R14, R15, R16, R17, R18, R19, R21, sex_1f_2m_t) %>%
  na.omit()

data_final_model$sex_1f_2m_t <- factor(data_final_model$sex_1f_2m_t, levels = c("female", "male"))

#configural 
fit_configural <- cfa(final_model, data = data_final_model, group = "sex_1f_2m_t")
summary(fit_configural, fit.measures=TRUE)
#metric
fit_metric <- cfa(final_model, data = data_final_model, group = "sex_1f_2m_t",
                  group.equal = "loadings")
summary(fit_metric, fit.measures=TRUE)
#scalar
fit_scalar <- cfa(final_model, data = data_final_model, group = "sex_1f_2m_t",
                  group.equal = c("loadings", "intercepts"))
summary(fit_scalar, fit.measures=TRUE)
#summary and final comparison
anova(fit_configural, fit_metric, fit_scalar)

data_female <- data_final_model %>% filter(sex_1f_2m_t == "female")
data_male <- data_final_model %>% filter(sex_1f_2m_t == "male")

#scores of female and male
data_female <- data_female %>%
  mutate(score = R10 + R14 + R15 + R16 + R17 + R18 + R19 + R21)

data_male <- data_male %>%
  mutate(score = R10 + R14 + R15 + R16 + R17 + R18 + R19 + R21)

#summary
summary_female <- data_female %>%
  summarise(mean = mean(score), sd = sd(score),
            p10 = quantile(score, 0.1),
            p25 = quantile(score, 0.25),
            p50 = quantile(score, 0.5),
            p75 = quantile(score, 0.75),
            p90 = quantile(score, 0.9))

summary_male <- data_male %>%
  summarise(mean = mean(score), sd = sd(score),
            p10 = quantile(score, 0.1),
            p25 = quantile(score, 0.25),
            p50 = quantile(score, 0.5),
            p75 = quantile(score, 0.75),
            p90 = quantile(score, 0.9))

summary_female
summary_male

data_final_model <- data_final_model %>%
  mutate(trigeminal_score_8_items = R10 + R14 + R15 + R16 + R17 + R18 + R19 + R21)

ggplot(data_final_model, aes(x = trigeminal_score_8_items, fill = sex_1f_2m_t)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Trigeminal questionnaire by sex",
    x = "Trigeminal score",
    y = "Density"
  ) +
  scale_fill_discrete(name = "Sex") +
  theme_minimal()


data_long <- age_subgroup %>%
  select(trigeminal_score_8_items, Lateralization, Identification, age, Ammola, self_rated_sens, current_smell) %>%
  pivot_longer(
    cols = c(Lateralization, Identification, age, Ammola, self_rated_sens, current_smell),
    names_to = "variable",
    values_to = "value"
  )

cor_labels <- data_long %>%
  group_by(variable) %>%
  summarise(
    cor = cor(value, trigeminal_score_8_items, method = "spearman", use = "complete.obs"),
    p = cor.test(value, trigeminal_score_8_items, method = "spearman")$p.value,
    x = mean(range(value, na.rm = TRUE)),                       
    y = max(trigeminal_score_8_items, na.rm = TRUE) * 1.05      
  ) %>%
  mutate(
    r_squared = round(cor^2, 2),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    label = paste0("r = ", round(cor, 2), stars, ", r² = ", r_squared)
  )

ggplot(data_long, aes(x = value, y = trigeminal_score_8_items)) +
  geom_point(alpha = 0.2, color = "limegreen") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~variable, scales = "free_x", labeller = as_labeller(labels_plot)) +
  geom_text(
    data = cor_labels,
    aes(x = x, y = y + 0.05 * y, label = label),
    inherit.aes = FALSE,
    size = 3.5,
    color = "black"
  ) +
  labs(x = "Value", y = "Trigeminal score") +
  theme_minimal()


#patchowrk of the figures
#Figure 2
corr_1 <- ggplot(data_long, aes(x = value, y = trigeminal_score_8_items)) +
  geom_point(alpha = 0.2, color = "skyblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~variable, scales = "free_x", labeller = as_labeller(labels_plot)) +
  geom_text(
    data = cor_labels,
    aes(x = x, y = y + 0.05 * y, label = label),
    inherit.aes = FALSE,
    size = 3.5,
    color = "black"
  ) +
  labs(x = "Value", y = "Trigeminal score") +
  theme_minimal()

corr_2 <- corr_healthy <- ggplot(data_long, aes(x = value, y = trigeminal_score_8_items)) +
  geom_point(alpha = 0.2, color = "limegreen") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~variable, scales = "free_x", labeller = as_labeller(labels_plot)) +
  geom_text(
    data = cor_labels,
    aes(x = x, y = y + 0.05 * y, label = label),
    inherit.aes = FALSE,
    size = 3.5,
    color = "black"
  ) +
  labs(x = "Value", y = "Trigeminal score") +
  theme_minimal()

corr_1 /
  corr_2

#Figure 3
box_1 <- ggplot(data_extremes, aes(x = Lateralization_tercile, y = trigeminal_score_8_items)) +
  geom_boxplot() +
  labs(
    x = "Lateralization Tercile",
    y = "Trigeminal Score",
    title = "Trigeminal score by lateralization"
  ) +
  scale_y_continuous(limits = c(0, 25)) +
  geom_jitter(alpha = 0.1) +
  theme_minimal()

box_2 <- ggplot(data_extremes, aes(x = current_smell_group, y = trigeminal_score_8_items)) +
  geom_boxplot() +
  labs(
    x = "Self rated olfactory function",
    y = "Trigeminal score",
    title = "Trigeminal score by self rated olfactory function"
  ) +
  scale_x_discrete(labels = c("low" = "poor", "high" = "good")) +
  scale_y_continuous(limits = c(0, 25)) +
  geom_jitter(alpha = 0.1) + 
  theme_minimal()

box_1  + box_2

#Figure 4

lat_1 <- ggplot(data = data, aes(x = trigeminal_score_8_items, y = Lateralization, color = age)) +
  geom_jitter(alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "navy", name = "Age") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    x = "Trigeminal questionnaire score",
    y = "Lateralization",
    title = "General sample, n = 527"
  ) +
  theme_minimal()

lat_2 <- ggplot(data = age_subgroup_healthy, aes(x = trigeminal_score_8_items, y = Lateralization, color = age)) +
  geom_jitter(alpha = 0.6) +
  scale_color_gradient(low = "lightgreen", high = "darkgreen", name = "Age") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    x = "Trigeminal questionnaire score",
    y = "Lateralization",
    title = "Subsample, n = 121"
  ) +
  theme_minimal()

lat_1 + lat_2

#Figure 5
dens_1 <- ggplot(data_sex_filtered, aes(x = Lateralization, fill = sex_1f_2m_t)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Lateralization by sex",
    x = "Lateralization",
    fill = "Sex"
  ) +
  theme_minimal()

dens_2 <- ggplot(data_final_model, aes(x = trigeminal_score_8_items, fill = sex_1f_2m_t)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Trigeminal questionnaire by sex",
    x = "Trigeminal score",
    y = "density"
  ) +
  scale_fill_discrete(name = "Sex") +
  theme_minimal()

dens_1 + dens_2

