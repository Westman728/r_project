library("readxl")
library("ggplot2")
library("scales")
library("ggthemes")
library("car")
library("stats")
file <- "LOCAL FILE PATH"
bildata <- read_excel(file, range = "A1:I2736")

sapply(bildata, class)
sum(is.na(bildata))
which(is.na(bildata), arr.ind = TRUE)          # Find where NA's is
bildata[is.na(bildata)] <- "tvåhjulsdriven"    # Only works with specific these specific NA's



## Transforming
bildata$pris <- as.numeric(bildata$pris)
bildata$bränsle <- as.factor(bildata$bränsle)
bildata$växellåda <- as.factor(bildata$växellåda)
bildata$miltal <- as.numeric(bildata$miltal) +10
bildata$biltyp <- as.factor(bildata$biltyp)
bildata$drivning <- as.factor(bildata$drivning)
bildata$hästkrafter <- as.numeric(bildata$hästkrafter)
bildata$märke <- as.factor(bildata$märke)
bildata <- bildata[-c(8, 31, 101, 30, 919), ]               # Removing big outliers
#bildata <- model.matrix(~ . + 0, data = bildata)           # If binary dummy variables are required
#bildata <- as_tibble(bildata)

#bildata$miltal <- log(bildata$miltal)                      #Slightly improves normality, but not really worth the extra steps it imposes
lm.fit <- lm(log(pris)~., bildata)
par(mfrow=c(2,2))
plot(lm.fit)
summary(lm.fit)



# A look at the leverage clusters
lev <- hatvalues(lm.fit)
range_filter <- which(lev >= 0.02 & lev <= 0.04)
filtered <- bildata[range_filter, ]
View(filtered)
?arrayInd
#--------------------- Understanding the variables
levels(bildata$drivning)
contrasts(bildata$bränsle)    # Find baselines, makes interpretation easier
table(bildata$märke)
sapply(bildata, table)
#--------------------- Looking into multicollinearity
round(vif(lm.fit), 2)

#---------------------- Clearer coefficient summary     NOTE: NEEDS EXP() TO INTERPRET
summary_lm <- summary(lm.fit)
coef_fix <- sprintf("%.4f", summary_lm$coefficients[, "Estimate"])
coef_names <- rownames(summary_lm$coefficients)
coef_print <- paste(coef_names, coef_fix, sep = ":")
print(coef_print)

co <- coef(lm.fit)              # A look at how much % the variables change outcome
print(co)
exp_co <- exp(co)
round(exp_co, 4)

#-------------------------- Setting up correlation matrix
bildata_dummy <- model.matrix(~., bildata)
bildata_dummy <- bildata_dummy[, -1]  # Remove the intercept
cor_mat <- cor(bildata_dummy)
par(mfrow=c(2,2))
heatmap(cor_mat)

#-------------------------- NEW MANUAL OBSERVATION PREDICTION

new <- data.frame(
  bränsle = "miljöbränsle/hybrid",
  växellåda = "automat",
  miltal = 9464,
  modellår = 2021,
  biltyp = "suv",
  drivning = "fyrhjulsdriven",
  hästkrafter = 344,
  märke = "volvo"
)

pred_new <- predict(lm.fit, newdata = new, interval = "prediction")
print(pred_new)
print(exp(pred_new))


#------------------ Finding best variables

library(leaps)
regfit.full <- regsubsets(pris~., bildata, nvmax = 20)
summary(regfit.full)      # Horsepower > modelyear > transmission are top 3 variables
names(regfit.full)
reg.summary <- summary(regfit.full)
reg.summary$adjr2    
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab = "Nmbr of variables",
     ylab = "RSS", type = "l")
which.min(reg.summary$rss)
points(18, reg.summary$rss[18], col = "blue", cex = 2, pch = 20)      
plot(reg.summary$adjr2, xlab = "Nmbr of variables",
     ylab = "Adjusted r-squared", type = "l")
which.max(reg.summary$adjr2)
points(17, reg.summary$adjr2[17], col = "blue", cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Nmbr of variables",
     ylab = "CP", type = "l")
which.min(reg.summary$cp)
points(15, reg.summary$cp[16], col = "blue", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Nmbr of variables",
     ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(14, reg.summary$bic[15], col = "blue", cex = 2, pch = 20)            # 15-18 variables seem best, basically all


best_variables_cp <- names(coef(regfit.full, 16))  # Needs dummy variables to work

# Evaluating model on test set

set.seed(42)
train <- sample(c(TRUE, FALSE), nrow(bildata), replace = TRUE)
train_data <- bildata[train, ]
test_data <- bildata[!train, ]
#test_data$miltal_log <- log(test_data$miltal + 1)
#test_data$pris <- log(test_data$pris)
#train_data$miltal_log <- log(train_data$miltal +1)
#train_data$pris <- log(train_data$pris)
lm.fit_test <- lm(pris~., train_data)
preds <- predict(lm.fit_test, newdata = test_data)
true_values <- test_data$pris
rmse <- sqrt(mean((preds - true_values)^2))
rsq <- summary(lm.fit_test)$adj.r.squared
rse <- sqrt(summary(lm.fit_test)$sigma^2)
print(paste("RMSE:", rmse))
print(rmse_original)                    
print(rsq)
print(rse)
print(mean(bildata$pris))             
View(train_data)
rmse_original <- sqrt(mean((preds - true_values)^2))

# Residual vs fitted plot for testset
residuals <- true_values - preds
plot(preds, residuals, 
     xlab = "Predicted Values", 
     ylab = "Residuals",
     main = "Residuals vs Predicted Values",
     pch = 20, col = "blue")
abline(h = 0, col = "red")

#-------- Ridge/Lasso test

x <- model.matrix(pris~., bildata)[, -1]             # glmnet() only takes numeric values
y <- bildata$pris
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
set.seed(42)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 100, newx = x[test, ], exact = T, x = x[train, ], y = y[train])
mean((ridge.pred - y.test) ^2)
mean((mean(y[train]) - y.test) ^2)
rmse_ridge <- sqrt(mean((ridge.pred - y.test) ^2))
rmse_mean <- sqrt(mean((mean(y[train]) - y.test)^2))
print(paste("RMSE Ridge:", rmse_ridge))            # RMSE of ~38k for both ridge/lasso, roughly same as multiple linear regression model
print(paste("RMSE Mean:", rmse_mean))              # tried a range of alphas, 5, 50, 100, 500, 1000, small diff.

