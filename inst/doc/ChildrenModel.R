## ---- echo = FALSE, message=FALSE, warning = FALSE-----------------------
require("bw")
require("ggplot2")

## ---- message = FALSE----------------------------------------------------
female_model1 <- child_weight(age = 7, sex = "female")

## ---- message = FALSE----------------------------------------------------
female_model2 <- child_weight(age = 7, sex = "female", FM = 19.9, FFM = 5.74)

## ---- eval = FALSE-------------------------------------------------------
#  female_model3 <- child_weight(age = 7, sex = "female", FM = 19.9, FFM = 5.74,
#                                EI = seq(1600, 1750, length.out = 365))

## ---- message = FALSE----------------------------------------------------
#Database information
ages    <- c(8, 10, 7, 7, 12)
sexes   <- c("male", "female", "female", "male", "male") 

#Returns a weight change matrix and other matrices
database_model <- child_weight(ages, sexes)

## ---- fig.width=7, fig.height=4------------------------------------------
model_plot(female_model2, "Body_Weight")

## ---- fig.width=7, fig.height=4------------------------------------------
model_plot(female_model2, c("Body_Weight", "Fat_Mass"))

## ---- fig.width=7, fig.height=4------------------------------------------
model_plot(female_model2, c("Body_Weight", "Fat_Mass"), timevar = "Age")

## ------------------------------------------------------------------------
EIbrownian <- energy_build(c(1600, 1750, 1820), c(0, 365, 730))

## ---- fig.width=7, fig.height=4------------------------------------------
ggplot() + geom_line(aes(x = 1:730, y = EI), data = data.frame(EI = EIbrownian)) +
  theme_classic() +
  xlab("Days") + ylab("Energy intake (kcals)") + ggtitle("Energy interpolation")

## ---- message = FALSE----------------------------------------------------
model_brownian <- child_weight(10, "male", EI = EIbrownian, days = 730)

## ------------------------------------------------------------------------
EIlinear      <- energy_build(c(1600, 1750, 1820), c(0, 365, 730), "Linear")
EIexponential <- energy_build(c(1600, 1750, 1820), c(0, 365, 730), "Exponential")
EIstepwise_r  <- energy_build(c(1600, 1750, 1820), c(0, 365, 730), "Stepwise_R")
EIstepwise_l  <- energy_build(c(1600, 1750, 1820), c(0, 365, 730), "Stepwise_L")
EIlogarithmic <- energy_build(c(1600, 1750, 1820), c(0, 365, 730), "Logarithmic")

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

## ---- message = FALSE----------------------------------------------------
model_linear      <- child_weight(10, "male", EI = EIlinear, days = 730)
model_exponential <- child_weight(10, "male", EI = EIexponential, days = 730)
model_logarithmic <- child_weight(10, "male", EI = EIlogarithmic, days = 730)
model_stepwise_r  <- child_weight(10, "male", EI = EIstepwise_r, days = 730)
model_stepwise_l  <- child_weight(10, "male", EI = EIstepwise_l, days = 730)

## ---- echo = FALSE, fig.width=7, fig.height=4----------------------------
ggplot() +
  geom_line(aes(x = 1:730, y = as.vector(model_linear[["Body_Weight"]]), color = "Linear")) + 
  geom_line(aes(x = 1:730, y = as.vector(model_exponential[["Body_Weight"]]), color = "Exponential")) + 
  geom_line(aes(x = 1:730, y = as.vector(model_logarithmic[["Body_Weight"]]), color = "Logarithmic")) + 
  geom_line(aes(x = 1:730, y = as.vector(model_stepwise_r[["Body_Weight"]]), color = "Right Stepwise")) + 
  geom_line(aes(x = 1:730, y = as.vector(model_stepwise_l[["Body_Weight"]]), color = "Left Stepwise")) + 
  geom_line(aes(x = 1:730, y = as.vector(model_brownian[["Body_Weight"]]), color = "Brownian")) +
  xlab("Days") + ylab("Weight (kg)") + 
  theme_classic()+
  ggtitle("Weight change under different energy interpolations") + 
    scale_color_manual("Interpolation", 
                     values = c("Brownian" = "red", "Linear" = "deepskyblue3",
                                "Exponential" = "forestgreen", "Logarithmic" = "purple",
                                "Right Stepwise" = "black", "Left Stepwise" = "green"))

## ---- fig.width=7, fig.height=4------------------------------------------
girl <- child_weight(6,"female", days=365, dt = 5, 
                     richardsonparams = list(K = 2700, Q = 10, 
                                             B = 12, A = 3, nu = 4, 
                                             C = 1))
model_plot(girl, "Body_Weight")

## ---- message = FALSE----------------------------------------------------
#Database information
mydata <- data.frame(
  id = 1:5,
  age = c(8, 10, 7, 7, 12),
  sex = c("male", "female", "female", "male", "male"),
  energy = runif(5, 1500, 2000),
  prob = c(0.1, 0.2, 0.2, 0.05, 0.45))

#Get energy change with energy build function
eichange      <- energy_build(cbind(runif(5, 1500, 2000), mydata$energy), c(0, 365))

#Returns a weight change matrix and other matrices
database_model <- child_weight(mydata$age, mydata$sex, EI = t(eichange))

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
#  browseVignettes("bw")

## ---- eval = FALSE-------------------------------------------------------
#  browseVignettes("bw")

