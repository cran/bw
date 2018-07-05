## ---- echo = FALSE, message=FALSE, warning = FALSE-----------------------
require("bw")
require("ggplot2")

## ------------------------------------------------------------------------
female_model1 <- adult_weight(bw = 80, ht = 1.8, age = 40, sex = "female")

## ------------------------------------------------------------------------
female_model2 <- adult_weight(bw = 80, ht = 1.8, age = 40, sex = "female", 
                      EIchange = rep(-250, 365), NAchange = rep(-20, 365))

## ------------------------------------------------------------------------
female_model3 <- adult_weight(bw = 80, ht = 1.8, age = 40, sex = "female", 
                      EIchange = rep(-250, 365))

## ------------------------------------------------------------------------
female_model4 <- adult_weight(bw = 80, ht = 1.8, age = 40, sex = "female", 
                      EIchange = rep(-250, 365), EI = 2000, fat = 22, 
                      PAL = 1.7, pcarb_base = 0.6, pcarb = 0.5)

## ------------------------------------------------------------------------
#Database information
weights <- c(45, 67, 58, 92, 81)
heights <- c(1.30, 1.73, 1.77, 1.92, 1.73)
ages    <- c(45, 23, 66, 44, 23)
sexes   <- c("male", "female", "female", "male", "male") 

#Returns a weight change matrix and other matrices
database_model <- adult_weight(weights, heights, ages, sexes)

## ---- fig.width=7, fig.height=4------------------------------------------
model_plot(female_model2, "Body_Weight")

## ---- fig.width=7, fig.height=4------------------------------------------
model_plot(female_model2, c("Body_Weight", "Fat_Mass"))

## ---- fig.width=7, fig.height=4------------------------------------------
model_plot(female_model2, c("Body_Weight", "Fat_Mass"), timevar = "Age")

## ------------------------------------------------------------------------
EIbrownian <- energy_build(c(0, -250, 100), c(0, 365, 730))

## ---- fig.width=7, fig.height=4------------------------------------------
ggplot() + geom_line(aes(x = 1:730, y = EI), data = data.frame(EI = EIbrownian)) + 
  theme_classic() +
  xlab("Days") + ylab("Energy change (kcals)") + ggtitle("Energy interpolation")

## ------------------------------------------------------------------------
model_brownian <- adult_weight(70, 1.75, 22, "male", EIbrownian, days = 730)

## ------------------------------------------------------------------------
EIlinear      <- energy_build(c(0, -250, 100), c(0, 365, 730), "Linear")
EIexponential <- energy_build(c(0, -250, 100), c(0, 365, 730), "Exponential")
EIstepwise_r  <- energy_build(c(0, -250, 100), c(0, 365, 730), "Stepwise_R")
EIstepwise_l  <- energy_build(c(0, -250, 100), c(0, 365, 730), "Stepwise_L")
EIlogarithmic <- energy_build(c(0, -250, 100), c(0, 365, 730), "Logarithmic")

## ---- fig.width=7, fig.height=4------------------------------------------
ggplot() + 
  geom_line(aes(x = 1:730, y = EI, color = "Brownian"), 
            data = data.frame(EI = EIbrownian)) + 
  geom_line(aes(x = 1:730, y = EI, color = "Linear"), 
            data = data.frame(EI = EIlinear)) + 
  geom_line(aes(x = 1:730, y = EI, color = "Exponential"), 
            data = data.frame(EI = EIexponential)) + 
  geom_step(aes(x = 1:730, y = EI, color = "Right Stepwise"), 
            data = data.frame(EI = EIstepwise_r)) + 
  geom_step(aes(x = 1:730, y = EI, color = "Left Stepwise"), 
            data = data.frame(EI = EIstepwise_l)) + 
  geom_line(aes(x = 1:730, y = EI, color = "Logarithmic"), 
            data = data.frame(EI = EIlogarithmic)) + 
  xlab("Days") + ylab("Energy change (kcals)") + 
  ggtitle("Energy interpolation") +
  theme_classic() + 
  scale_color_manual("Interpolation", 
                     values = c("Brownian" = "red", "Linear" = "deepskyblue3",
                                "Exponential" = "forestgreen", "Logarithmic" = "purple",
                                "Right Stepwise" = "black", "Left Stepwise" = "green"))

## ------------------------------------------------------------------------
model_linear      <- adult_weight(70, 1.75, 22, "male", EIlinear, days = 730)
model_exponential <- adult_weight(70, 1.75, 22, "male", EIexponential, days = 730)
model_logarithmic <- adult_weight(70, 1.75, 22, "male", EIlogarithmic, days = 730)
model_stepwise_r  <- adult_weight(70, 1.75, 22, "male", EIstepwise_r, days = 730)
model_stepwise_l  <- adult_weight(70, 1.75, 22, "male", EIstepwise_l, days = 730)

## ---- echo = FALSE, fig.width=7, fig.height=4----------------------------
ggplot() +
  geom_line(aes(x = 1:730, y = as.vector(model_linear[["Body_Weight"]]), color = "Linear")) + 
  geom_line(aes(x = 1:730, y = as.vector(model_exponential[["Body_Weight"]]), color = "Exponential")) + 
  geom_line(aes(x = 1:730, y = as.vector(model_logarithmic[["Body_Weight"]]), color = "Logarithmic")) + 
  geom_line(aes(x = 1:730, y = as.vector(model_stepwise_r[["Body_Weight"]]), color = "Right Stepwise")) + 
  geom_line(aes(x = 1:730, y = as.vector(model_stepwise_l[["Body_Weight"]]), color = "Left Stepwise")) + 
  geom_line(aes(x = 1:730, y = as.vector(model_brownian[["Body_Weight"]]), color = "Brownian")) +
  xlab("Days") + ylab("Weight (kg)") + 
  theme_classic() + 
  ggtitle("Weight change under different energy interpolations") + 
    scale_color_manual("Interpolation", 
                     values = c("Brownian" = "red", "Linear" = "deepskyblue3",
                                "Exponential" = "forestgreen", "Logarithmic" = "purple",
                                "Right Stepwise" = "black", "Left Stepwise" = "green"))

## ------------------------------------------------------------------------
#Database information
mydata <- data.frame(
  id     = 1:5,
  weight = c(67, 68, 69, 70, 71),
  height = c(1.30, 1.73, 1.77, 1.92, 1.73),
  age    = c(45, 23, 66, 44, 23),
  sex    = c("male", "female", "female", "male", "male"),
  energy_change = runif(5, -200, 200),
  prob   = c(0.1, 0.2, 0.2, 0.05, 0.45))

#Get energy change with energy build function
eichange      <- energy_build(cbind(rep(0,5), mydata$energy_change), c(0, 365))

#Returns a weight change matrix and other matrices
database_model <- adult_weight(mydata$weight, mydata$height, mydata$age, mydata$sex,
                               eichange)

## ----fig.width=7, fig.height=4-------------------------------------------
model_plot(database_model, "Body_Weight")

## ---- eval = FALSE-------------------------------------------------------
#  model_mean(database_model, "Body_Weight")

## ---- echo = FALSE, warning = FALSE--------------------------------------
head(model_mean(database_model, "Body_Weight"))[,1:5]

## ---- eval = FALSE-------------------------------------------------------
#  model_mean(database_model, "Body_Weight", days = 1:365)

## ---- echo = FALSE, warning=FALSE----------------------------------------
head(model_mean(database_model, "Body_Weight", days = 1:365))[,1:5]

## ---- eval = FALSE-------------------------------------------------------
#  model_mean(database_model, "Body_Weight", days = 1:365, group = mydata$sex)

## ---- echo = FALSE, warning = FALSE--------------------------------------
head(model_mean(database_model, "Body_Weight", days = 1:365, group = mydata$sex))[,1:5]

## ---- eval = FALSE-------------------------------------------------------
#  require("survey")
#  design <- svydesign(ids = ~id, probs = ~prob, data = mydata)
#  model_mean(database_model, group = mydata$sex, design = design)

## ---- echo = FALSE, message=FALSE, warning = FALSE-----------------------
require("survey")
design <- svydesign(ids = ~id, probs = ~prob, data = mydata)
head(model_mean(database_model, group = mydata$sex, design = design))[,1:5]

## ---- eval = FALSE-------------------------------------------------------
#  adult_bmi(database_model, group = mydata$sex)

## ---- echo = FALSE-------------------------------------------------------
head(adult_bmi(database_model, group = mydata$sex))

## ---- eval = FALSE-------------------------------------------------------
#  browseVignettes("bw")

## ---- eval = FALSE-------------------------------------------------------
#  browseVignettes("bw")

