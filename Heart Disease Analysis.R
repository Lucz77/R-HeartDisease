
install.packages("tidyverse")
library(tidyverse)


## Carico il dataset e ne analizzo la struttura

heart <- read.csv("heart.csv", header = TRUE, stringsAsFactors = FALSE)
heart <- as_tibble(heart)
heart
summary(heart)
glimpse(heart)
View(heart)


## Trasformo i dati in modo che siano tecnicamente corretti

heart$sex <- as.factor(heart$sex)
heart$sex <- replace(heart$sex, which(heart$sex == "unspecified"), NA)
heart$sex <- droplevels(heart$sex)

heart$cp <- as.factor(heart$cp)

heart$chol <- as.integer(heart$chol)

heart$fbs <- as.factor(heart$fbs)

heart$restecg <- as.factor(heart$restecg)

heart$exang <- as.factor(heart$exang)

heart$slope <- as.factor(heart$slope)

heart$ca <- as.factor(heart$ca)
heart$ca <- replace(heart$ca, which(heart$ca == 4), NA)
heart$ca <- droplevels(heart$ca)

heart$thal <- as.factor(heart$thal)
heart$thal <- replace(heart$thal, which(heart$thal == 0), NA)
heart$thal <- droplevels(heart$thal)

heart$target <- as.factor(heart$target)


## Rinomino le colonne in modo appropriato

names(heart)[names(heart) == "x"] <- "id"


## Rinomino i livelli dei fattori in modo appropriato

heart$cp <- recode_factor(heart$cp,
  "0" = "1", "1" = "2", "2" = "3",
  "3" = "4"
)

heart$slope <- recode_factor(heart$slope, "0" = "1", "1" = "2", "2" = "3")

heart$thal <- recode_factor(heart$thal, "1" = "3", "2" = "6", "3" = "7")


## Verifico la presenza di valori NA e, nel caso, li rimuovo

table(is.na(heart))
heart <- na.omit(heart)


## Rimuovo le colonne ritenute non necessarie

heart <- subset(heart, select = -id)


## Trasformo i dati in modo che siano consistenti

boxplot(heart$age)
outliers <- boxplot(heart$age, plot = FALSE)$out
heart <- heart[-which(heart$age %in% outliers), ]
boxplot(heart$age)

boxplot(heart$trestbps)
outliers <- boxplot(heart$trestbps, plot = FALSE)$out
heart <- heart[-which(heart$trestbps %in% outliers), ]
boxplot(heart$trestbps)

boxplot(heart$chol)
outliers <- boxplot(heart$chol, plot = FALSE)$out
heart <- heart[-which(heart$chol %in% outliers), ]
boxplot(heart$chol)

hist(heart$thalach)
heart$thalach[heart$thalach > 222] <- mean(heart$thalach)
hist(heart$thalach)


## Analisi descrittiva nominali

sex_barplot <- ggplot(heart, aes(x = sex)) +
  geom_bar(aes(fill = sex), width = 0.5) +
  labs(
    title = "Frequency of patients' sexes", x = "Sex of the patient",
    y = "Frequency"
  )
sex_barplot <- sex_barplot + scale_fill_discrete(
  name = "Legend",
  labels = c("Females", "Males")
)
sex_barplot

cp_barplot <- ggplot(heart, aes(x = cp)) +
  geom_bar(aes(fill = cp), width = 0.5) +
  labs(title = "Chest pain frequency", x = "Chest pain type", y = "Frequency")
cp_barplot <- cp_barplot + scale_fill_discrete(
  name = "Legend",
  labels = c(
    "Typical Angina", "Atypical Angina",
    "Non-anginal Pain", "Asymptomatic"
  )
)
cp_barplot

fbs_barplot <- ggplot(heart, aes(x = fbs)) +
  geom_bar(aes(fill = fbs), width = 0.5) +
  labs(
    title = "Fbs frequency", subtitle = "Fasting blood sugar > 120 mg/dl",
    x = "Fbs", y = "Frequency"
  )
fbs_barplot <- fbs_barplot + scale_fill_discrete(
  name = "Legend",
  labels = c("False", "True")
)
fbs_barplot

restecg_barplot <- ggplot(heart, aes(x = restecg)) +
  geom_bar(aes(fill = restecg), width = 0.5) +
  labs(title = "Restecg frequency", x = "Restecg", y = "Frequency")
restecg_barplot <- restecg_barplot + scale_fill_discrete(
  name = "Legend",
  labels = c(
    "Normal", "ST-T wave abnormality",
    "Probable or definite\nleft ventricular hypertrophy"
  )
)
restecg_barplot

exang_barplot <- ggplot(heart, aes(x = exang)) +
  geom_bar(aes(fill = exang), width = 0.5) +
  labs(
    title = "Exang frequency", subtitle = "Exercise induced angina",
    x = "Exang", y = "FrequencY"
  )
exang_barplot <- exang_barplot + scale_fill_discrete(
  name = "Legend",
  labels = c("No", "Yes")
)
exang_barplot

slope_barplot <- ggplot(heart, aes(x = slope)) +
  geom_bar(aes(fill = slope), width = 0.5) +
  labs(title = "Slope frequency", x = "Slope", y = "Frequency")
slope_barplot <- slope_barplot + scale_fill_discrete(
  name = "Legend",
  labels = c("Upsloping", "Flat", "Downsloping")
)
slope_barplot

ca_barplot <- ggplot(heart, aes(x = ca)) +
  geom_bar(aes(fill = ca), show.legend = FALSE, width = 0.5) +
  labs(
    title = "Ca frequency", subtitle = "Number of major vessels",
    x = "Ca", y = "Frequency"
  )
ca_barplot

thal_barplot <- ggplot(heart, aes(x = thal)) +
  geom_bar(aes(fill = thal), width = 0.5) +
  labs(title = "Thal frequency", x = "Thal", y = "Frequency")
thal_barplot <- thal_barplot + scale_fill_discrete(
  name = "Legend",
  labels = c("Normal", "Fixed defect", "Reversable defect")
)
thal_barplot

target_barplot <- ggplot(heart, aes(x = target)) +
  geom_bar(aes(fill = target), width = 0.5) +
  labs(
    title = "Target frequency", subtitle = "Heart disease detected",
    x = "Target", y = "Frequency"
  )
target_barplot <- target_barplot + scale_fill_discrete(
  name = "Legend",
  labels = c("No", "Yes")
)
target_barplot


## Analisi descrittiva ordinali

age_barplot <- ggplot(heart, aes(x = age)) +
  geom_bar(aes(fill = age), show.legend = FALSE, width = 0.5) +
  labs(
    title = "Frequency of patients' age", x = "Age of the patient",
    y = "Frequency"
  )
age_barplot
summary(heart$age)

thalach_barplot <- ggplot(heart, aes(x = thalach)) +
  geom_bar(aes(fill = thalach), show.legend = FALSE, width = 0.5) +
  labs(title = "Thalach frequency", x = "Thalach", y = "Frequency")
thalach_barplot
summary(heart$thalach)

oldpeak_barplot <- ggplot(heart, aes(x = oldpeak)) +
  geom_bar(aes(fill = oldpeak), show.legend = FALSE) +
  labs(title = "Oldpeak frequency", x = "Oldpeak", y = "Frequency")
oldpeak_barplot
summary(heart$oldpeak)


## Regressione lineare age-trestbps

# Visualizzo il grafico delle variabili
plot(heart$age, heart$trestbps,
  xlab = "age",
  ylab = "trestbps (mm/Hg)"
)

# Visualizzo la regressione lineare tra "trestbps" e "age"
reg <- lm(heart$trestbps ~ heart$age)
abline(reg, col = "blue")

# Visualizzo i residui
segments(heart$age, fitted(reg), heart$age, heart$trestbps,
  col = "red", lty = 2
)

# Titolo il grafico di regressione
title(main = "Regressione lineare tra age e trestbps")

# Analizzo la regressione e ne verifico gli intervalli di confidenza
summary(reg)
confint(reg)

# Calcolo il valore del coefficiente di correlazione lineare "r"
# e il valore del coefficiente di determinazione "R^2"
r <- cor(heart$trestbps, heart$age)
r
r^2

# Visualizzo il grafico dei residui
plot(reg$fitted, reg$residuals, main = "Residui")
abline(0, 0)

# Visualizzo la distribuzione in quantili
qqnorm(reg$residuals)
qqline(reg$residuals)


## Previsioni

df_previsione <- tibble(
  "age" = c(28, 33, 48, 70, 66, 42, 50, 48, 47, 61),
  "trestbps" = c(101, 134, 135, 155, 152, 152, 144, 161, 140, 168)
)

plot(df_previsione$age, df_previsione$trestbps,
  xlab = "age",
  ylab = "trestbps (mm/Hg)"
)

reg_2 <- lm(df_previsione$trestbps ~ df_previsione$age)
abline(reg_2, col = "blue")

segments(df_previsione$age, fitted(reg_2), df_previsione$age,
  df_previsione$trestbps,
  col = "red", lty = 2)

title(main = "Reg tra age e trestbps del dataframe di prova")

predict(reg_2, df_previsione)
predict(reg_2, df_previsione, interval = "confidence")
predict(reg_2, df_previsione, interval = "prediction")

prediction <- predict(reg_2, df_previsione, interval = "prediction")
prediction_df <- cbind(df_previsione, prediction)

grafico_previsione <- ggplot(prediction_df, aes(age, trestbps)) +
  labs(title = "Intervalli di previsione e di confidenza") +
  geom_point() +
  geom_line(aes(y = lwr), color = "green", linetype = "dashed") +
  geom_line(aes(y = upr), color = "green", linetype = "dashed") +
  geom_smooth(method = lm, se = TRUE)
grafico_previsione


## Modello di Machine Learning applicato a "heart$slope"

## Installo e carico il package "caret"
install.packages("caret")
library(caret)

## Creo un dataset con le colonne necessarie di "heart"
dataset <- heart[, c("age", "trestbps", "chol", "slope")]
dataset <- as.data.frame(dataset)

## Analizzo la struttura del dataset e di "slope"
dim(dataset)
sapply(dataset, class)
head(dataset)
levels(dataset$slope)

## Verifico la distribuzione delle classi di "slope"
percentage <- prop.table(table(dataset$slope)) * 100
cbind(freq = table(dataset$slope), percentage = percentage)

## Verifico la distribuzione degli attributi
summary(dataset)

## Divido il dataset in input e output
agetrestchol <- dataset[, 1:3]
dim(agetrestchol)
slope <- dataset[, 4]
dim(t(slope))

## Analizzo la distribuzione degli attributi dei valori di input
par(mfrow = c(1, 3))
for (i in 1:3) {
  boxplot(agetrestchol[, i], main = names(dataset)[i])
}

## Barplot della distribuzione delle classi di "slope"
plot(slope)

## Scatterplot tra gli attributi di input e la distribuzione delle classi
## in output
featurePlot(
  x = agetrestchol, y = slope, plot = "ellipse",
  auto.key = list(columns = 3)
)

## Boxplot della distribuzione degli attributi per ogni classe
featurePlot(x = agetrestchol, y = slope, plot = "box")

## Grafico di densità di ogni attributo per classe
scales <- list(
  agetrestchol = list(relation = "free"),
  slope = list(relation = "free")
)
featurePlot(
  x = agetrestchol, y = slope, plot = "density", scales = scales,
  auto.key = list(columns = 2)
)

## Creo un Test Set

## Creo un seed
set.seed(200919)

## Creo una matrice dell'80% delle righe del dataset da usare nel training
training_index <- createDataPartition(dataset$slope, p = .80, list = FALSE)

## Seleziono l'80% dei dati per allenare i modelli
training_set <- dataset[training_index, ]
nrow(training_set)

## Uso il rimanente 20% per i test
test_set <- dataset[-training_index, ]
nrow(test_set)

## Valutazione degli algoritmi

## Definisco il metodo di esecuzione degli algoritmi
seed <- set.seed(200919)
control <- trainControl(method = "cv", number = 10 / 5, seed = seed)
metric <- "Accuracy"

## Algoritmi lineari
fit_lda <- train(slope ~ .,
  data = training_set, metric = metric, trControl = control,
  method = "lda"
)

## Algoritmi non lineari

## CART
fit_cart <- train(slope ~ .,
  data = training_set, metric = metric, trControl = control,
  method = "rpart"
)
## kNN
fit_knn <- train(slope ~ .,
  data = training_set, metric = metric, trControl = control,
  method = "knn"
)
## MLP
fit_mlp <- train(slope ~ .,
  data = training_set, metric = metric, trControl = control,
  method = "mlp"
)

## Algoritmi avanzati

## Random Heart
fit_rh <- train(slope ~ .,
  data = training_set, metric = metric, trControl = control,
  method = "rf"
)
## SVM
fit_svm <- train(slope ~ .,
  data = training_set, metric = metric, trControl = control,
  method = "svmRadial"
)

## Seleziono il modello migliore

## Richiamo i risultati dei test d'accuratezza
results <- resamples(list(
  lda = fit_lda, cart = fit_cart, knn = fit_knn,
  mlp = fit_mlp, rh = fit_rh, svm = fit_svm
))
summary(results)

## Dotplot dei risultati
dotplot(results)

## Identifichiamo "svm" come algoritmo più preciso
fit_svm$results

## Previsione di accuratezza dell'algoritmo "svm" applicato al Test Set
predictions <- predict(fit_svm, test_set)
confusionMatrix(predictions, test_set$slope)

## Grafico d'accuratezza dell'algoritmo
install.packages("rattle")
library(rattle)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("RColorBrewer")
library(RColorBrewer)
fancyRpartPlot(fit_cart$finalModel, sub = "")
