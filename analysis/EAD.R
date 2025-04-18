dataframe <- read.csv("data/pokemonCards.csv")
attach(dataframe)
summary(dataframe)

# variables in the dataset
colnames(dataframe)

priceNormal <- "tcg.market.price.usd..normal."
priceReverse <- "tcg.market.price.usd..reverse.holofoil."
priceHolo <- "tcg.market.price.usd..holofoil."


str(dataframe[[priceNormal]])
# num of rows
nrow(dataframe)

drops <- c("tcg.low.price.usd..reverse.holofoil.",
           "tcg.low.price.usd..holofoil.",
           "tcg.low.price.usd..normal.",
           "tcg.high.price.usd..reverse.holofoil.",
           "tcg.high.price.usd..holofoil.",
           "tcg.high.price.usd..normal.",
           "tcg.player.url",
           "tcg.price.date",
           "card.image..small.",
           "card.image.hires",
           "retreat.cost",
           "weaknesses",
           "pokedex.number",) 

dataframe <- dataframe[ , !(names(dataframe) %in% drops)] # remove unnecessary columns

# remove values that are NA for all priceNormal, priceReverse, and priceHolo
dataframe <- dataframe[!(is.na(dataframe[[priceNormal]]) & is.na(dataframe[[priceReverse]]) & is.na(dataframe[[priceHolo]])), ] # remove rows with na values for all price columns

# new num of rows
nrow(dataframe)

summary(dataframe) # check the summary of the dataframe after removing unnecessary columns

# Exploratory Data Analysis

# do any trainers have hp?
trainers <- dataframe[dataframe$supertype == "Trainer", ] # filter trainers
trainers <- trainers[trainers$hp > 0, ]
trainers <- trainers[!is.na(trainers$hp), ] # remove na values from hp column
#print(trainers) # print trainers with hp
# what about energies? answer= no
energies <- dataframe[dataframe$supertype == "Energy", ] # filter trainers
energies <- energies[energies$hp > 0, ]
energies <- energies[!is.na(energies$hp), ] # remove na values from hp column


# PRICES
# price normal distribution
hist(dataframe[[priceNormal]], main = "Distribution of Normal Price", xlab = "Normal Price", ylab = "Frequency", col = "lightblue", border = "black") # nolint: line_length_linter.
boxplot(dataframe[[priceNormal]], main = "Boxplot of Normal Price", ylab = "Normal Price", col = "lightblue", border = "black") # nolint
# looks awfully skewed to the right, log transformation of normal price
dataframe$log_price <- log(dataframe[[priceNormal]] + 0.01) # log transformation of normal price
# now look at the distribution of log price
hist(dataframe$log_price, breaks = 50, main = "Log-Transformed Price", xlab = "log(price)")
boxplot(dataframe$log_price, main = "Boxplot of Log Price", ylab = "Log Price", col = "lightblue", border = "black")
# much better but still some outliers
outliers <- boxplot.stats(dataframe$log_price)$out # find outliers in log price
dfNoOutliers <- dataframe[!(dataframe$log_price %in% outliers), ] # remove these
hist(dfNoOutliers$log_price, breaks = 50, main = "Log-Transformed Price", xlab = "log(price)", col="#c0c047")


# price reverse holofoil distribution
hist(dataframe[[priceReverse]], main = "Distribution of Reverse Holofoil Price", xlab = "Reverse Holofoil Price", ylab = "Frequency", col = "lightblue", border = "black") # nolint: line_length_linter.
# perfrom log transformation of reverse holofoil price
dataframe$log_price_r <- log(dataframe[[priceReverse]] + 0.01) # log transformation of reverse holofoil price
hist(dataframe$log_price_r, breaks = 50, main = "Log-Transformed Reverse Holofoil Price", xlab = "log(price)")
# there are very high outliers, lets take a look at them
outliers <- boxplot.stats(dataframe$log_price_r)$out # find outliers in log price
#print(outliers) # print outliers
high_outliers <- dataframe[dataframe$log_price_r > 2.5, ] # filter outliers
high_outliers <- high_outliers[!is.na(high_outliers$log_price_r), ]
#print(high_outliers) # print outliers


ultrarare <- dataframe[dataframe$rarity == "Rare Ultra", ] # only rare ultra cards
ultrarare <- ultrarare[!is.na(ultrarare$rarity), ] # remove na values from rarity column
#print(ultrarare) # print rare ultra cards


# most are from the same set (black and white) and are all pokemon. have a mean of 21.23, and 4 have a normal version (bw11-3, bw11-17, bw11-21, bw11-26)
# remove these outliers
#dataframe <- dataframe[dataframe$log_price_r < 2.5, ] # remove outliers
hist(dataframe$log_price_r, breaks = 25, main = "Log-Transformed Reverse Holofoil Price", xlab = "log(price)", col="#e4e46f", border="#515111") # looks semi normal
# remove other outliers



# price holofoil distribution
hist(dataframe[[priceHolo]], main = "Distribution of Holofoil Price", xlab = "Holofoil Price", ylab = "Frequency", col = "lightblue", border = "black") # nolint: line_length_linter.
# still skwewed but not as bad as normal price, lets log transform it
dataframe$log_price_h <- log(dataframe[[priceHolo]] + 0.01) # log transformation of holofoil price
hist(dataframe$log_price_h, breaks = 40, main = "Log-Transformed Holofoil Price", xlab = "log(price)", col="#ecec8b", border="#515111") # looks very normal


# HP

dataframe <- dataframe[dataframe$hp > 0, ] # remove rows with hp = 0
dataframe <- dataframe[!is.na(dataframe$hp), ] # remove na values from hp column
# look at the distribution of hp
hist(dataframe$hp, main = "Distribution of HP", xlab = "HP", ylab = "Frequency", col = "lightblue", border = "black")
boxplot(dataframe$hp, main = "Boxplot of HP", ylab = "HP", col = "lightblue", border = "black")
# it looks pretty good

# hp and price
plot(dataframe$hp, dataframe$log_price, main = "HP vs Normal Price", xlab = "HP", ylab = "Normal Price", col = "lightblue", pch = 19) # plot hp and normal price
cor(dataframe$hp, dataframe$log_price, use = "complete.obs") # 0.0668
# lets see if outliers are affecting this correlation
# outliers <- boxplot.stats(dataframe$log_price)$out # find outliers in log price
# dataframe <- dataframe[!(dataframe$log_price %in% outliers), ] # remove these
plot(dataframe$hp, dataframe$log_price, main = "HP vs Normal Price", xlab = "HP", ylab = "Normal Price", col = "lightblue", pch = 19) # plot hp and normal price
cor(dataframe$hp, dataframe$log_price, use = "complete.obs") # 0.0.0668

# hp and reverse holofoil price
plot(dataframe$hp, dataframe$log_price_r, main = "HP vs Holofoil Price", xlab = "HP", ylab = "Holofoil Price", col = "lightblue", pch = 19) # plot hp and holofoil price
cor(dataframe$hp, dataframe$log_price_r, use = "complete.obs") # 0.1850941

# hp and holofoil price
plot(dataframe$hp, dataframe$log_price_h, main = "HP vs Holofoil Price", xlab = "HP", ylab = "Holofoil Price", col = "lightblue", pch = 19) # plot hp and holofoil price
cor(dataframe$hp, dataframe$log_price_h, use = "complete.obs") # 0.3059591


# RARITY
# look at the distribution of rarity
table(dataframe$rarity) # print the distribution of rarity
unique(dataframe$rarity)

# since theres rarities that contain both a normal and reverse holofoil version, lets look at the distribution in a scatterplot
dataframe$price_type <- NA # create a new column for price type
dataframe$price_type[!is.na(dataframe[[priceNormal]])] <- "normal" # set normal price type
dataframe$price_type[!is.na(dataframe[[priceReverse]])] <- "reverse holofoil" # set reverse holofoil price type
dataframe$price_type[!is.na(dataframe[[priceHolo]])] <- "holofoil" # set holofoil price type




library(dplyr)
# new rankings for rarity
dataframe$rarity_grouped <- dplyr::case_when(
  dataframe$rarity %in% c("Common") ~ "Common",
  dataframe$rarity %in% c("Uncommon") ~ "Uncommon",
  dataframe$rarity %in% c("Rare", "Rare ACE", "Rare BREAK") ~ "Rare",
  dataframe$rarity %in% c("Rare Holo", "Rare Holo EX", "Rare Holo GX", "Rare Holo V", "Rare Holo VMAX") ~ "Rare Holo",
  dataframe$rarity %in% c("Rare Holo VSTAR") ~ "Rare Holo VSTAR",
  dataframe$rarity %in% c("Rare Secret", "Rare Rainbow") ~ "Rare Secret",
  dataframe$rarity %in% c("Ultra Rare", "Rare Ultra") ~ "Ultra Rare",
  dataframe$rarity %in% c("Illustration Rare", "Special Illustration Rare") ~ "Rare Secret",
  dataframe$rarity %in% c("Radiant Rare") ~ "Rare Holo",  # Or keep as "Radiant Rare"
  dataframe$rarity %in% c("ACE SPEC Rare") ~ "Ultra Rare",  # Or "ACE SPEC"
  dataframe$rarity %in% c("Promo") ~ "Promo",
  dataframe$rarity %in% c("Shiny Rare") ~ "Shiny Rare",  # Or "Illustration Rare"
  dataframe$rarity %in% c("Double Rare") ~ "Ultra Rare",  # Or "Rare Holo"
  TRUE ~ NA_character_
)


dataframe$rarity_grouped <- as.factor(dataframe$rarity_grouped)

table(dataframe$rarity_grouped, useNA = "ifany") # print the distribution of rarity

# drop na values from rarity column
dataframe <- dataframe[!is.na(dataframe$rarity_grouped), ] # remove na values from rarity column
dataframe <- dataframe[!(dataframe$rarity_grouped == "Shiny Rare"), ] # remove empty values from rarity column
dataframe <- dataframe[!(dataframe$rarity_grouped == "Promo"), ] # remove empty values from rarity column
dataframe$rarity_grouped <- droplevels(dataframe$rarity_grouped) # drop empty values from rarity_grouped column

table(dataframe$rarity_grouped) # 8 rarities left: Common, Uncommon, Rare, Rare Holo, Rare VMAX, Rare Secret, Ultra Rare, Illustration Rare

# look at the distribution of prices by rarity
library(tidyr)
price_data_long <- dataframe %>%
  select(rarity_grouped, log_price, log_price_r, log_price_h) %>%
  pivot_longer(cols = c(log_price, log_price_r, log_price_h), 
  names_to = "price_type", 
  values_to = "price") %>%
  drop_na(price)

price_data_long$price_type <- recode(price_data_long$price_type,
                                     log_price = "Normal",
                                     log_price_r = "Reverse Holofoil",
                                     log_price_h = "Holofoil")

# scatterplot of price type vs rarity
library(ggplot2)
ggplot(price_data_long, aes(x = rarity_grouped, y = price, color = price_type)) +
  geom_jitter(width = 0.7, alpha = 0.6) +  # You can use geom_boxplot() if you prefer boxplots
  labs(title = "Price Distribution by Rarity and Price Type",
       x = "Rarity",
       y = "Log(Price)",
       color = "Price Type") +
  theme_minimal()

str(price_data_long)
dataframe$price_data_long <- unlist(dataframe$price_data_long)

# One-way ANOVA
anova_result <- aov(price  ~ rarity_grouped, data = price_data_long)
summary(anova_result)
TukeyHSD(anova_result)

model_interaction <- lm(price ~ rarity_grouped, data = price_data_long)
summary(model_interaction)
# the p value is very small, so we can reject the null hypothesis that there is no difference between the means of the groups

# TYPES
# look at the distribution of types
table(dataframe$type) # print the distribution of type
unique(dataframe$type) # cards of multiple types do not exceed 3

# duplicate rows where cards have multiple types
library(dplyr)
library(tidyr)
dataframe <- dataframe %>%
  separate_rows(types, sep = ",\\s") # separate rows by type
table(dataframe$types) # print the distribution of type

price_type_data <- dataframe %>%
  filter(!is.na(log_price)) %>%
  select(types, log_price)

ggplot(price_type_data, aes(x = types, y = log_price, fill = types)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Pokémon Type",
       x = "Pokémon Type", y = "Log(Reverse Holo Price)") +
  theme_minimal() 
  
anova_result <- aov(log_price ~ types, data = price_type_data)
summary(anova_result)
TukeyHSD(anova_result)
# fairy seems to be having the most effect on the price, but it is not significant
# darkness and fighting and grass are also significantly different from the rest


# RELEASE DATE
# look at the distribution of release dates
table(dataframe$release.date) # print the distribution of release dates
# group by year

# remove rows with na and values not in the format YYYY/YY/DD
dataframe <- dataframe[!is.na(dataframe$release.date), ] # remove na values from release date column
dataframe <- dataframe[grepl("^[0-9]{4}/[0-9]{2}/[0-9]{2}$", dataframe$release.date), ] # remove rows with release date not in the format YYYY/YY/DD
table(dataframe$release.date) # print the distribution of release dates

dataframe$release.year <- as.numeric(format(as.Date(dataframe$release.date, format = "%Y/%m/%d"), "%Y")) # convert release date to year
dataframe$release.year <- as.factor(dataframe$release.year) # convert release year to factor
table(dataframe$release.year) # print the distribution of release years

# remove 2012 and 2021
dataframe <- dataframe[dataframe$release.year != "2012", ] # remove 2012
dataframe <- dataframe[dataframe$release.year != "2021", ] # remove 2021

table(dataframe$release.year) # print the distribution of release years

# look at the distribution of release years
ggplot(dataframe, aes(x = release.year, y = log_price, fill = factor(release.year))) +
  geom_boxplot() +
  labs(title = "Price Distribution by Release Year",
       x = "Release Year", y = "Log(Reverse Holo Price)") +
  theme_minimal()

anova_result <- aov(log_price ~ release.year, data = dataframe)
summary(anova_result)

dataframe$release.year.num <- as.numeric(as.character(dataframe$release.year))
lm_result <- lm(log_price ~ release.year.num, data = dataframe)
summary(lm_result)

# SUPERTYPE
# look at the distribution of supertypes
table(dataframe$supertype) # print the distribution of supertypes
ggplot(dataframe, aes(x = supertype, y = log_price)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Supertype",
       x = "Supertype", y = "Log(Price)") +
  theme_minimal()

anova_supertype <- aov(log_price ~ supertype, data = dataframe)
summary(anova_supertype)

ggplot(subset(dataframe, supertype == "Energy"), aes(y = log_price)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log(Price) for Energy Cards")
summary(subset(dataframe, supertype == "Energy")$log_price)

# Or manually inspect extremes
sort(subset(dataframe, supertype == "Energy")$log_price)
energy_prices <- subset(dataframe, supertype == "Energy")$log_price
# remove na values from energy prices
energy_prices <- energy_prices[!is.na(energy_prices)] # remove na values from energy prices

# to make sure we have a balanced dataset, we will take a sample of 80 from each supertype (pokemon and trainer) and keep all energy cards
set.seed(123)  # for reproducibility
sample_pokemon <- dataframe[dataframe$supertype == "Pokémon", ]
sample_trainer <- dataframe[dataframe$supertype == "Trainer", ]

pokemon_sample <- sample_n(sample_pokemon, 80)
trainer_sample <- sample_n(sample_trainer, 80)

balanced_df <- rbind(
  pokemon_sample,
  trainer_sample,
  dataframe[dataframe$supertype == "Energy", ]
)

# Boxplot with balanced data
ggplot(balanced_df, aes(x = supertype, y = log_price)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Supertype (Balanced Sample)",
       x = "Supertype", y = "Log(Price)")
# kruskal test, Chi-squared is 52.92, p val is 3.22e-12
kruskal.test(log_price ~ supertype, data = balanced_df)


# interaction between supertype and year
dataframe$release.year <- as.factor(dataframe$release.year)
dataframe$supertype <- as.factor(dataframe$supertype)

interaction_model <- aov(log_price ~ release.year * supertype, data = dataframe)
summary(interaction_model)

interaction_model <- lm(log_price ~ release.year * supertype, data = dataframe)
summary(interaction_model)

# 2017 and above
dataframe$isModern <- ifelse(dataframe$release.year.num >= 2017, 1, 0) # create a new column for modern and classic
dataframe$score_modern <- ifelse(dataframe$isModern == 1, dataframe$score, 0)
dataframe$count_modern <- ifelse(dataframe$isModern == 1, dataframe$count, 0)

# COUNT
table(dataframe$count_modern) # print the distribution of counts

# SCORE
# look at the distribution of card scores

# # score and hp
# model_interaction4 <- lm(log_price ~ log_score * hp, data = dfScore)
# summary(model_interaction4)

# for normal price, remove outlier from hp
outliers <- boxplot.stats(dataframe$log_price)$out # find outliers in log price
print(outliers)

model <- lm(log_price ~ hp, data = dataframe)

# Cook's Distance
cooksd <- cooks.distance(model)
plot(cooksd, type = "h", main = "Cook's Distance")
abline(h = 4 / nrow(dataframe), col = "red")

# High leverage points
leverage <- hatvalues(model)
plot(leverage, type = "h", main = "Leverage")
abline(h = 2 * mean(leverage), col = "blue")

# Optional: remove points with high influence
influential <- which(cooksd > (4 / nrow(dataframe)))
print(influential)
dfNormal <- dataframe[-influential, ]

model <- lm(log_price ~ hp, data = dfNormal)
summary(model)
cor(dfNormal$hp, dfNormal$log_price, use = "complete.obs") # 0.12
# dfNormal <- dataframe[!(dataframe$log_price %in% outliers), ] # remove these


# Find interaction terms in full model
model_hp = lm(log_price ~ hp , data = dfNormal)
summary(model_hp) # significant
model_hp = lm(log_price_r ~ hp , data = dataframe)
summary(model_hp) #significant
model_hp = lm(log_price_h ~ hp , data = dataframe)
summary(model_hp)  # significant

#interaction and rarity
mNormalRarityHp <- lm(log_price ~ rarity_grouped * hp, data = dfNormal)
summary(mRarityHp) 
mRarityHp <- lm(log_price_r ~ rarity_grouped * hp, data = dataframe)
summary(mRarityHp)
mRarityHp <- lm(log_price_h ~ rarity_grouped * hp, data = dataframe)
summary(mRarityHp)

# interaction of rarity and year
mNormalRarityHp <- lm(log_price ~ rarity_grouped * release.year.num, data = dfNormal)
summary(mNormalRarityHp) 
mRarityHp <- lm(log_price_r ~ rarity_grouped * release.year.num, data = dataframe)
summary(mRarityHp)
mRarityHp <- lm(log_price_h ~ rarity_grouped * release.year.num, data = dataframe)
summary(mRarityHp)

# interaction hp and year
mNormalRarityHp <- lm(log_price ~ hp * release.year.num, data = dfNormal)
summary(mNormalRarityHp)
mRarityHp <- lm(log_price_r ~ hp * release.year.num, data = dataframe)
summary(mRarityHp)
mRarityHp <- lm(log_price_h ~ hp * release.year.num, data = dataframe)
summary(mRarityHp)

# interaction hp year and rarity
mRarityHp <- lm(log_price_h ~ hp * release.year.num * rarity_grouped, data = dataframe)
summary(mRarityHp)


# BUILDING FULL MODEL FOR NORMAL PRICE
dfNormal <- dataframe[!is.na(dataframe$log_price), ] # remove na values from log price column 
nrow(dfNormal) # 1477

# forward selection
install.packages("olsrr")
library(olsrr)
dfNormal$score_modern <- ifelse(dfNormal$isModern == 1, dfNormal$score, 0)
dfNormal$count_modern <- ifelse(dfNormal$isModern == 1, dfNormal$count, 0)
dfNormal$score_modern <- as.numeric(dfNormal$score_modern)
dfNormal$count_modern <- as.numeric(dfNormal$count_modern)



emptyModel <- lm(log_price ~ 1, data = dfNormal)

fullModel <- lm(log_price ~ 
  hp + release.year.num + rarity_grouped + supertype +
  isModern + score_modern + count_modern +
  hp:rarity_grouped + 
  hp:release.year.num + 
  release.year.num:rarity_grouped +
  release.year.num:supertype +
  hp:rarity_grouped:release.year.num,
  data = dfNormal)


ols_step_forward_p(fullModel, details = TRUE) # 0.588

backward_model <- step(fullModel, direction = "backward", trace = 1)
summary(backward_model) # r of 0.5877 removed "release.year.num:supertypeTrainer" and "supertypeTrainer""
# compare models
anova(backward_model, fullModel) # fail to reject null hypothesis, so we can use the simpler model

# results of full model
summary(backward_model) # r of 0.5877 removed "release.year.num:supertypeTrainer" and "supertypeTrainer""

library(dplyr)

# remove na values from score column
dfNormal <- dfNormal[!is.na(dfNormal$score_modern), ] # remove na values from score column
# remove values of 0 from score column
dfNormal <- dfNormal[dfNormal$score_modern != 0, ] # remove values of 0 from score column

# 

ggplot(dfNormal, aes(x = score_modern, y = log_price)) +
  geom_point(aes(color = score_modern), size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "#f99a51") +
  scale_color_viridis_c(option = "D") +
  labs(
    title = "Score vs Price",
    x = "Score",
    y = "Price",
    color = "Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 18))



# Predict values

dfNormal$predicted <- predict(fullModel)

# Plot
ggplot(dfNormal, aes(x = predicted, y = log_price)) +
  geom_point(alpha = 0.4, color = "#0072B2") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual (Log Price)",
       x = "Predicted Log Price",
       y = "Actual Log Price") +
  theme_minimal()

# Plot
ggplot(dfNormal, aes(x = predicted, y = log_price, color = release.year)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(values = rainbow(length(unique(dfNormal$release.year)))) +
  labs(
    title = "Predicted vs Actual Log Prices by Release Year",
    subtitle = "Color-coded by release year | Dashed line = perfect prediction",
    x = "Predicted Log Price",
    y = "Actual Log Price",
    color = "Release Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  )

olsrr::ols_step_forward_p()


# FULL MODEL FOR REVERSE HOLOFOIL PRICE
dfReverse <- dataframe # remove na values from log price column
dfReverse$score_modern <- ifelse(dfReverse$isModern == 1, dfReverse$score, 0)
dfReverse$count_modern <- ifelse(dfReverse$isModern == 1, dfReverse$count, 0)
dfReverse$score_modern <- as.numeric(dfReverse$score_modern)
dfReverse$count_modern <- as.numeric(dfReverse$count_modern)


emptyModel <- lm(log_price_r ~ 1, data = dfReverse) # empty model
fullModel <- lm(log_price_r  ~ hp + release.year.num + rarity_grouped + supertype +
  isModern + score_modern + count_modern +
  hp:rarity_grouped + 
  hp:release.year.num + 
  release.year.num:rarity_grouped +
  release.year.num:supertype +
  hp:rarity_grouped:release.year.num,
  data =dfReverse) # full model

ols_step_forward_p(fullModel, details = TRUE) # 0.588
backward_model <- step(fullModel, direction = "backward", trace = 1)
summary(backward_model)

 # r of 0.5877 removed "hp:rarity_grouped:release.year.num" "hp:rarity_grouped" and "hp"
# compare models
anova(backward_model, fullModel) # fail to reject null hypothesis, so we can use the simpler model


ggplot(dfReverse, aes(x = score_modern, y = log_price_r)) +
  geom_point(aes(color = score_modern), size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  scale_color_viridis_c(option = "D") +
  labs(
    title = "Score vs Price",
    x = "Score",
    y = "Price",
    color = "Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 18))




# FULL MODEL FOR HOLOFOIL PRICE
dfNormal <- dataframe[!is.na(dataframe$log_price_h), ] # remove na values from log price column 
nrow(dfNormal) # 1477

# forward selection
install.packages("olsrr")
library(olsrr)
dfNormal$score_modern <- ifelse(dfNormal$isModern == 1, dfNormal$score, 0)
dfNormal$count_modern <- ifelse(dfNormal$isModern == 1, dfNormal$count, 0)
dfNormal$score_modern <- as.numeric(dfNormal$score_modern)
dfNormal$count_modern <- as.numeric(dfNormal$count_modern)



emptyModel <- lm(log_price_h ~ 1, data = dfNormal)

fullModel <- lm(log_price_h ~ 
  hp + release.year.num + rarity_grouped + supertype +
  isModern + score_modern + count_modern +
  hp:rarity_grouped + 
  hp:release.year.num + 
  release.year.num:rarity_grouped +
  release.year.num:supertype +
  hp:rarity_grouped:release.year.num,
  data = dfNormal)


ols_step_forward_p(fullModel, details = TRUE) # 0.588

backward_model <- step(fullModel, direction = "backward", trace = 1)
summary(backward_model) # r of 0.5877 removed "release.year.num:supertypeTrainer" and "supertypeTrainer""
# compare models
anova(backward_model, fullModel) # fail to reject null hypothesis, so we can use the simpler model

# results of full model
summary(backward_model) # r of 0.5877 removed "release.year.num:supertypeTrainer" and "supertypeTrainer""

# Predict values
dfNormal$predicted <- predict(backward_model)

# Plot
ggplot(dfNormal, aes(x = predicted, y = log_price_h)) +
  geom_point(alpha = 0.4, color = "#0072B2") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual (Log Price)",
       x = "Predicted Log Price",
       y = "Actual Log Price") +
  theme_minimal()


# Plot
ggplot(dfNormal, aes(x = predicted, y = log_price_h, color = rarity_grouped)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(values = c(
  "Common" = "#F8766D",       # soft red
  "Uncommon" = "#7CAE00",     # green
  "Rare" = "#00BFC4",         # turquoise
  "Rare Holo" = "#C77CFF",    # purple
  "Rare Secret" = "#FF61C3"   # pink
)) +
  labs(
    title = "Predicted vs Actual Log Holofoil Prices by Release Year",
    subtitle = "Color-coded by release year | Dashed line = perfect prediction",
    x = "Predicted Log Holofoil Price",
    y = "Actual Log Holofoil Price",
    color = "Release Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  )
plot(dfNormal)

summary(backward_model)
sqrt(mean((dfNormal$predicted)^2)) # RMSE
sqrt(mean((dfNormal$log_price_h - dfNormal$predicted)^2)) # RMSE

mean(abs(predicted - actual)) # MAE