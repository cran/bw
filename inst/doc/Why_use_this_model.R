## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
require(ggplot2)
require(bw)
knitr::opts_chunk$set(fig.width = 7, fig.height = 4)

## ---- echo = FALSE, warning=FALSE----------------------------------------
data1 <- data.frame(Day = c(-5:0,0:5), Weight = c(rep(80, 6), rep(NA, 6)))
data2 <- data.frame(Day = c(-5:0,0:5), Weight = c(rep(NA, 6), rep(78, 6)))

ggplot(data1, aes(x = Day, y = Weight)) + 
  geom_line(color = "deepskyblue3") + 
  geom_line(data = data2, color = "tomato3") +
  annotate("text", x = -2, y = 80.2, label = "Before reducing caloric intake") +
  annotate("text", x = 2, y = 78.2, label = "After 500 kcal reduction") +
  theme_classic()  + 
  ggtitle("Immediate change in body weight  according to\n a loss of 0.45 kg for every 3500 kcals")


## ---- echo = FALSE, warning=FALSE----------------------------------------
ggplot(data.frame(x = c("Man with underweight","Woman with obesity"), y = c(0.45, 0.45)), aes(x = x)) + 
  geom_bar(aes(fill = x), stat="count")  +
  theme_classic()  + ylab("Weight reduction") +
  ggtitle("Weight change in man with underweight and woman with obesity.") +
  scale_fill_manual("Individual", values = c("tomato3","deepskyblue3"))

## ----echo = FALSE, warning=FALSE-----------------------------------------
data1 <- data.frame(Week = 0:266, Weight = seq(60,-60, by = -0.45))

ggplot(data1, aes(x = Week, y = Weight)) + 
  theme_classic() +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_line(color = "deepskyblue3") +
  geom_point(aes(x = 134, y = 0), color = "red") +
  annotate("text", x = 35, y = 60, label = "Individual begins weight loss") +
  annotate("text", x = 140, y = 5, label = "Individual dies") +
  annotate("text", x = 230, y = -40, label = "Individual's ghost continues\n losing weight") +
  ggtitle("Weight trajectory for an individual that reduced 3500 kcal \n under the 0.45 kg/week for every 3500 kcals rule.")


## ----echo = FALSE, warning=FALSE-----------------------------------------
myadult250 <- adult_weight(80,1.8, 40, "male",  EIchange = matrix(250, ncol = 365, nrow = 1))$Body_Weight
myadult500 <- adult_weight(80,1.8, 40, "male",  EIchange = matrix(500, ncol = 365, nrow = 1))$Body_Weight

ggplot(data.frame(Day = 1:365, ad1 = as.vector(myadult250), ad2 = as.vector(myadult500)), aes(x = Day)) + 
  theme_classic() +
  geom_line(aes(y = ad1, color = "Realistic 250 kcal reduction (= 250 extra kcal)", linetype = "Realistic 250 kcal reduction (= 250 extra kcal)")) +
  geom_line(aes(y = ad2, color = "Current weight trajectory consuming 500 extra kcal", linetype = "Current weight trajectory consuming 500 extra kcal")) +
  geom_line(aes(y = c(80,rep(80 - 0.032, 364)), color = "250 kcal reduction under wrong model", linetype = "250 kcal reduction under wrong model")) +
  ggtitle("Weight trajectory for an individual that reduced 3500 kcal \n under the 0.45 kg/week for every 3500 kcals rule.") +
  scale_color_manual("Weight change", values = c("tomato3","deepskyblue3","forestgreen"))+
  scale_linetype_manual("Weight change", values = c("dashed","dotted","dashed"))


## ---- echo = FALSE, warning=FALSE----------------------------------------
ggplot(data.frame(Age = 5:60, EI = 2*(5:60) + 8)) +
  geom_line(aes(x  = Age, y = EI), color = "deepskyblue3") +
  ylab("Energy Intake") +
  theme_classic() +
  ggtitle(paste("ALPS model for predicting children weight"))

## ---- echo = FALSE-------------------------------------------------------
ei <- child_reference_EI(6, "male",FM = 2.9, FFM = 17.5, days = 3650)
ggplot(data.frame(Age = seq(6,16, by = 1/365), EI = ei)) +
  geom_line(aes(x  = Age, y = EI), color = "deepskyblue3") +
  ylab("Energy Intake") +
  theme_classic() +
  ggtitle(paste("Energy intake for a male child changes with time\n",
                "as the child grows."))

## ----echo = TRUE---------------------------------------------------------
trajectory <- adult_weight(80, 1.8, 43, "male", c(0, rep(-250, 3649)), days = 3650)$Body_Weight

ggplot(data.frame(Day = 1:3650, Weight = as.vector(trajectory)), 
       aes(x = Day, y = Weight)) + 
  geom_line(color = "deepskyblue3") +
  theme_classic() + 
  ggtitle(paste("Weight trajectory for a 43 year old male with",
                "height of 1.8m, initial body weight of 80kg,\n",
                "who reduced 250 kcals from his TEI"))

## ----echo = TRUE, message=FALSE------------------------------------------
trajectory <- child_weight(6,"male", FM = 2.9, FFM = 17.5, days = 3650)$Body_Weight

ggplot(data.frame(Day = 1:3650, Weight = as.vector(trajectory)), 
       aes(x = Day, y = Weight)) + 
  geom_line(color = "deepskyblue3") +
  theme_classic() + 
  ggtitle(paste("Weight trajectory for a 6 year old male with",
                "2.9 kg of fat mass and 17.5 kg of free fat mass"))

## ----eval = FALSE--------------------------------------------------------
#  browseVignettes("bw")

