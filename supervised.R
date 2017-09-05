# utils
#-------------------------------------------------------------------------------
source("utils/utils.R")

MyRequire(TH.data)
MyRequire(scatterplot3d)
MyRequire(e1071)
MyRequire(kernlab)
MyRequire(rpart)
MyRequire(randomForest)
MyRequire(mlbench)
MyRequire(nnet)
MyRequire(cluster)
MyRequire(MASS)
MyRequire(klaR)
MyRequire(Rcmdr)

# linear model
#-------------------------------------------------------------------------------
year <- rep(2008:2010, each = 4)
quarter <- rep(1:4, 3)
cpi <- c(162.2, 164.6, 166.5, 166, 166.2, 167, 168.6, 169.5, 171,
         172.1, 173.3, 174)
plot(cpi, xaxt = "n", ylab = "CPI", xlab = "")
# draw x-axis, where las=3 makes text vertical
axis(1, labels = paste(year, quarter, sep = "Q"), at = 1:12, las = 3)

## correlation between CPI and year / quarter
cor(year, cpi)
cor(quarter, cpi)
## build a linear regression model with function lm()
fit <- lm(cpi ~ year + quarter)
fit

## prediction
cpi2011 <- fit$coefficients[[1]] +
  fit$coefficients[[2]] * 2011 +
  fit$coefficients[[3]] * (1:4)
cpi2011

# more details
attributes(fit)
fit$coefficients

# differences between observed values and fitted values
residuals(fit)
summary(fit)

s3d <- scatterplot3d(year, quarter, cpi, highlight.3d = T, type = "h",
                     lab = c(2, 3)) # lab: number of tickmarks on x-/y-axes
s3d$plane3d(fit) # draws the fitted plane

# Prediction of CPIs in 2011
data2011 <- data.frame(year = 2011, quarter = 1:4)
cpi2011 <- predict(fit, newdata = data2011)
style <- c(rep(1, 12), rep(2, 4))
plot(c(cpi, cpi2011), xaxt = "n", ylab = "CPI", xlab = "", pch = style,
     col = style)
axis(1, at = 1:16, las = 3, labels = c(paste(year, quarter, sep = "Q"),
                                       "2011Q1", "2011Q2", "2011Q3", "2011Q4"))

# generalized linear model
#-------------------------------------------------------------------------------
data(spam)
set.seed(12345)
appindex <- sample(1:nrow(spam), floor(nrow(spam)*2/3), replace = FALSE)
app <- spam[appindex, ]
val <- spam[-appindex, ]

### Régression logistique
model_complet <- glm(type ~ ., family = binomial("logit"), data = app)
# Sélection stepwise ascendante de variables selon le critère AIC
best <- stepwise(model_complet, direction = "forward/backward", criterion = "AIC")

# naive bayes
#-------------------------------------------------------------------------------
### Classifieur bayésien naïf
model <- naiveBayes(type ~ ., data = app, laplace = 0)

# discriminant analysis
#-------------------------------------------------------------------------------
### Analyse discriminante linéaire
# Représentation des groupes sur le plan principal d'une ACP :
clusplot(app, app$type, diss = F, shade = T, color = T, labels = 4, main = "")
abline(v=0,h=0)
model <- lda(type ~ ., data = app)
model.forward <- greedy.wilks(type ~ ., data = app, niveau = 0.01)
best <- lda(model.forward$formula, data = app)
pred <- predict(best, newdata = val) # Prédiction
mat <- table(val$type, pred$class) # Matrice de confusion
taux <- sum(diag(mat))/sum(mat) # Taux d'erreur global

# decision tree
#-------------------------------------------------------------------------------

# random forest
#-------------------------------------------------------------------------------

# support vector machines
#-------------------------------------------------------------------------------

# neural network
#-------------------------------------------------------------------------------
