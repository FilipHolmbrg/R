# Bibliotek
library("readxl")
library(MASS)  
library(leaps)  
library(car)   
library(Metrics)
library(boot)

set.seed(123) #For reproducibility.

# Import av data
file_path <- "C:/Users/Elin/Desktop/DS23/R/Kunskapskontroll/cars_from_group.xlsx"
car_data <- read_excel(file_path)

# Ta bort dubletter
duplicates <- duplicated(car_data)

car_data[duplicates, ]
car_data_unique <- car_data[!duplicated(car_data), ]

cat("Antal rader före borttagning:", nrow(car_data), "\n")
cat("Antal rader efter borttagning:", nrow(car_data_unique), "\n")

car_data <- car_data_unique
car_data <- subset(car_data, select = -Färg) # tar bort Färg
car_data <-na.omit(car_data)

## Tranformering av datatyper
column_classes <- sapply(car_data, class)
print(column_classes)               

## Tranformering

car_data$Bränsle <- as.factor(car_data$Bränsle)
car_data$Växellåda <- as.factor(car_data$Växellåda)
car_data$Biltyp <- as.factor(car_data$Biltyp)
car_data$Drivning <- as.factor(car_data$Drivning)
car_data$Märke <- as.factor(car_data$Märke)

## inspecting classes of columns
column_classes <- sapply(car_data, class)
print(column_classes)

## inspect encoding
contrasts(car_data$Drivning)

View(car_data)

## Kontroll att inga NAs eller inf värden finns
anyNA(car_data)
sapply(car_data, function(x) any(is.infinite(x)))

#---------------------------------------------------------------------------------------------------

# Splitta data i träning val och test set
#split data and use train-data for EDA
spec <- c(train = .6, validate = .2, test = .2)

set.seed(123)
g = sample(cut(
  seq(nrow(car_data)), 
  nrow(car_data)*cumsum(c(0,spec)),
  labels = names(spec)
))

res <- split(car_data, g)

car_data_train <- res$train
car_data_val <- res$validate
car_data_test <- res$test

# EDA train data
dim(car_data_train)
head(car_data_train)
summary(car_data_train)
tail(car_data_train)
names(car_data_train)

# Steg 1 - testa en modell med all data -------------------------------------------------------------
# Fit a model with all variables
lm_1 <- lm(Pris~., data=car_data_train)
summary(lm_1)

#p-value: < 0.00000000000000022: there is at least one dependence of Xi-variables and Y. Good! (F-statistic)

pairs(car_data_train)

# Steg 2 - val av variabler (vad är rimligt?)

# Hur ser best subset ut?
regfit_best <- regsubsets(Pris~., data=car_data_train, nvmax = 18)
summary(regfit_best)
par(mfrow = c(2, 2))
plot(summary(regfit_best)$adjr2, , main = "Best subset selection")
```
# Efter ca 10 variabler sker saktar ökningen R_adj^2 av, "Viktiga" variabler blir (ALLA - Drivning), vi kan inkludera märke för att det borde intuitivt ha en påverkan. Alla dessa kan man förstå påverkar pris.


# Hur ser forward subset ut?

regfit_fwd <- regsubsets(Pris~., data=car_data_train, nvmax = 18, method = "forward")
summary(regfit_fwd)
plot(summary(regfit_fwd)$adjr2, main = "Forward stepwise selection")

plot(regfit_best, scale = "adjr2", main = "Best subset selection")
plot(regfit_fwd, scale = "adjr2", main = "Forward stepwise selection")

Forward selection ser liknande ut som best selection. "Viktiga" i ökande ordning: .

# Slutsats: Samma svar som i best subset. Tveksamhet kring ifall Märke/Biltyp ska vara med (väljer att ha med båda) Bygg modell från dessa "Viktiga" variabler blir (ALLA - Drivning) och utvärdera om det uppfyller syftet med modellen.

# Utvärdering av modeller:

## tranforsmation (logaritmering)
car_data_train_log <- car_data_train
car_data_val_log <- car_data_val
car_data_test_log <- car_data_test


car_data_train_log$Pris <- log(car_data_train_log$Pris)
car_data_val_log$Pris <- log(car_data_val_log$Pris)
car_data_test_log$Pris <- log(car_data_test_log$Pris)

## skapande av modeller
lm_2 <- lm(Pris~. -Drivning,data=car_data_train)
lm_3 <- lm(Pris~. -Drivning, data=car_data_train_log)

## Summering av modeller prestanda, på val-data.
val_pred_m1 <- predict(lm_1, newdata = car_data_val)
val_pred_m2 <- predict(lm_2, newdata = car_data_val)
val_pred_m3 <- exp(predict(lm_3, newdata = car_data_val_log))

results <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  RMSE_val_data = c(rmse(car_data_val$Pris, val_pred_m1),
                    rmse(car_data_val$Pris, val_pred_m2),
                    rmse(car_data_val$Pris, val_pred_m3)),
  Adj_R_squared = c(summary(lm_1)$adj.r.squared,
                    summary(lm_2)$adj.r.squared,
                    summary(lm_3)$adj.r.squared),
  BIC = c(BIC(lm_1), BIC(lm_2), BIC(lm_3))
)

results

# Steg 3 - utvärdering av eventuella problem

# 1. Icke linjärt förhållande mellan den beroende variabeln och de oberoende variablerna.
# model 2
par(mfrow = c(2, 2))
plot(lm_2)
# model 3
par(mfrow = c(2, 2))
plot(lm_3) # better QQ-plt

residuals <- residuals(lm_3)

# Subset data for observations in the training dataset
right_indices <- which(car_data_train$Pris >= 490000)
right_fitted <- fitted(lm_2)[right_indices]
right_residuals <- residuals[right_indices]

left_indices <- which(car_data_train$Pris <= 160000)
left_fitted <- fitted(lm_2)[left_indices]
left_residuals <- residuals[left_indices]

middle_indices <- which(car_data_train$Pris >= 300000 & car_data_train$Pris <= 320000)
middle_fitted <- fitted(lm_2)[middle_indices]
middle_residuals <- residuals[middle_indices]

plot(fitted(lm_2), residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at 0

# Add points for Toyota observations in the training dataset
points(right_fitted, right_residuals, col = "red", pch = 16)
points(left_fitted, left_residuals, col = "blue", pch = 16)
points(middle_fitted, middle_residuals, col = "orange", pch = 16)

#lm_2: Det konstiga utseendet beror på pris-intervallet för datainsamling (150-500k)
#lm_2: inget tydligt mönster, t ex U form på residualer och fitted values. OK

#lm_3: Det konstiga utseendet beror på pris-intervallet för datainsamling (150-500k)
#lm_3: inget tydligt mönster, t ex U form på residualer och fitted values. OK

# 2. Korrelerade residualer

# Extract standardized residuals
std_resid <- rstandard(lm_3)

# Plot standardized residuals against each predictor variable
par(mfrow = c(3, 2))  # Set up a 2x2 grid for plots

# Plot residuals against variable 1
plot(car_data_train_log$Bränsle, std_resid, xlab = '"Bränsle"', ylab = "Standardized Residuals", main = 'Residuals vs "Bränsle"')

# Plot residuals against variable 2
plot(car_data_train_log$Växellåda, std_resid, xlab = '"Växellåda"', ylab = "Standardized Residuals", main = 'Residuals vs "Växellåda"')

# Plot residuals against variable 3
plot(car_data_train_log$Miltal, std_resid, xlab = '"Miltal"', ylab = "Standardized Residuals", main = 'Residuals vs "Miltal"')

# Plot residuals against variable 4
plot(car_data_train_log$Modellår, std_resid, xlab = '"Modellår"', ylab = "Standardized Residuals", main = 'Residuals vs "Modellår"')

# Plot residuals against variable 5
plot(car_data_train_log$Biltyp, std_resid, xlab = '"Biltyp"', ylab = "Standardized Residuals", main = 'Residuals vs "Biltyp"')

# Plot residuals against variable 6
plot(car_data_train_log$Hästkrafter, std_resid, xlab = '"Hästkrafter"', ylab = "Standardized Residuals", main = 'Residuals vs "Hästkrafter"')

# Plot residuals against variable 7
plot(car_data_train_log$Märke, std_resid, xlab = '"Märke"', ylab = "Standardized Residuals", main = 'Residuals vs "Märke"')

# 3.	Icke-konstant varians på residualerna (Heteroskedasticitet).

#lm_2: i Residuals vs Fitted plot ses ingen tydlig heteroskedasticitet - variansen påvisar ingen tydlig trend till att öka/minska, utan är slumpmässig.
#lm_3: i Residuals vs Fitted plot ses ingen tydlig heteroskedasticitet - variansen påvisar ingen tydlig trend till att öka/minska, utan är slumpmässig.

# 4.	Ej normalfördelade residualer.
residuals <- residuals(lm_3)
par(mfrow = c(1, 1))
hist(residuals, col = 4, breaks = 40)

#lm_2: histogram ser ok ut, men QQ plot ser ut att avvika lite efter 1.5-2.
#lm_3: histogram ser bättre ut efter transformation av data, QQ plot ser bättre ut.

#-------------- avbryter lm_2 utvärdering här och skapar lm_3 från transformerad data. Genomgår steg 1,3,4 igen med lm_3.

# 5. Outliers
par(mfrow = c(1, 1))
student_resid <- rstudent(lm_3)
plot(fitted(lm_3), student_resid, 
     main = "Studentized Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Studentized Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at 0


# identifiera vilken punkt som är -4 < x < 4. Väljer abs(4) istället för abs(3) eftersom det är väldigt många punkter kring 3, dvs icke-outliers.
limit <- 4
mask <- student_resid[(student_resid > limit | student_resid < -limit)]
print(car_data_train[c(names(mask)), ])

#lm_3: finns 3 möjliga outliers. Kan inte se någon uppenbar systematik för dessa 7 (t ex om alla="0" i miltal) som indikerar att valda variabler kan motivera outlier. Låt punkter vara kvar!

# 6. ”High Leverage” punkter
par(mfrow = c(1,1))
student_resid <- rstudent(lm_3)
plot(hatvalues(lm_3),student_resid, 
     main = "Studentized Residuals vs Leverage",
     xlab = "Leverage", ylab = "Studentized Residuals")
# Ingen uppenbar enskild High leverage punkt (x-led), däremot finns kluster. 

plot(hatvalues(lm_3))
which.max(hatvalues(lm_3)) # tells us which (index) observation has the largest leverage statistic

# sorterar ut high-leverage kluster
hatvector <- hatvalues(lm_3)
limit <- 0.10
mask <- hatvector[(hatvector > limit)]
car_data_train[c(names(mask)),]

summary(car_data_train)
summary(car_data)

# Alla 8 high leverage punkter är "cabs" och motsvarar alla cabs i train-settet. Ta bort/ta inte bort? Ingen skillnad i summary(). Tar inte bort pga begränsing på modell map "cabs" utan någon större nytta i summary() eller större skillnad i coefs().

# 7. Kollinearitet/Multikollinearitet
vif_values <- vif(lm_3)
print(vif_values)

# lm_3: --OK GVIF < 5 och < 10.



# konfidensintervall och prediktionsintervall beta-parametrar (koefficienter)
options(scipen = 999)
confint(lm_3) # OBS att värden behöver transformeras med exp(), sedan anges som faktor vs intercept
exp_confint <- exp(confint(lm_3))
(exp_confint-1)*100 #data anges i % jmf med intercept

# punktskattning
summary_lm <- summary(lm_3)
print(summary_lm)
estimates <- summary_lm$coefficients
print(exp(summary_lm$coefficients[, 1]))
estimate_values <- (exp(summary_lm$coefficients[, 1])-1)*100
estimate_values

formatted_estimates <- format(estimate_values, digits = 4)

#Prediction (och konfidensintervall) för model
# New data that we want to predict
n=dim(car_data_test)[1]
print(n)

random_indices <- sample(nrow(car_data_test), size = n)  # Select 3 random rows
new_data <- car_data_test_log[random_indices,]
#View(new_data)

# Create CI & PI for predictions
confidence_intervals <- predict(lm_3, newdata = new_data, interval = "confidence", level = 0.95)
prediction_intervals <- predict(lm_3, newdata = new_data, interval = "prediction", level = 0.95)

confidence_intervals
prediction_intervals

vector_1x9 <- sapply(confidence_intervals, exp)
confidence_matrix <- matrix(vector_1x9, nrow = n, byrow = FALSE)
print(confidence_matrix)

vector_1x9 <- sapply(prediction_intervals, exp)
prediction_matrix <- matrix(vector_1x9, nrow = n, byrow = FALSE)
print(prediction_matrix)

confidence_range <- (confidence_matrix[,3]-confidence_matrix[,2])
mean(confidence_range)

prediction_range <- (prediction_matrix[,3]-prediction_matrix[,2])
mean(prediction_range)

#plottar histogram för prediktionsintervall och konfidensintervall, samt skillnad mellan "verkligt pris"-"predikterat pris"
par(mfrow = c(1, 2))
mean(confidence_range)
hist(confidence_range, breaks = 50, col = 4,
     xlab = "Range of confidence intervals",
     main = "Histogram of 515 confidence intervals")
mean(prediction_range)
hist(prediction_range, breaks = 50, col = 4,
     xlab = "Range of prediction intervals",
     main = "Histogram of 515 prediction intervals")

pris_diff <- exp(new_data$Pris)-prediction_matrix[,1]
mean(pris_diff) # 2734.388 i genomsnitt
hist(pris_diff, breaks = 50, col = 4,
     xlab = "Difference between prediction and real price [SEK]",
     main = "Histogram of 515 predictions") # Visar att modellen inte systematiskt ger fel prediktion, och ger en bättre bild än bara "genomsnittet".
