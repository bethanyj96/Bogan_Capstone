
```{r message=FALSE}
library(tidyverse)
library(minpack.lm)
library(nlfitr)
library(broom)
library(knitr)
library(ez)
library(cowplot)
library(viridis)
library(devtools)
```
## Data Simulation

```{r}
n <- 30
sd <- 20
means <- c(50, 70, 90, 110, 130, 190, 210, 230, 250, 270, 290, 310, 330, 360, 390)

flydatamaker <- function(n, means, sd) {

  params <- data.frame(n, means, sd)

 data <- as_tibble(apply(params, 1, function(x) rnorm(x[1], x[2], x[3]))) 

  oldnames <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10","V11", "V12", "V13","V14","V15")

  newnames <- c("0_control", "20_control", "40_control", "60_control", "80_control",
          "0_low", "20_low", "40_low","60_low", "80_low",
          "0_high", "20_high", "40_high", "60_high", "80_high")

  data <- data %>% rename_at(vars(oldnames), ~ newnames)


  data <- pivot_longer(data, everything(), 
                         names_to =c("time", "dose"),
                         names_pattern= "(.*)_(.*)",
                         values_to="Blood_Glucose") %>% 
           mutate(id=as.factor(paste(
             rep(rep(c("a","b","c", "d","e"), times=3), n), 
             rep(1:n, each=15), 
             sep="")),
             time=as.factor(time),
             dose=as.factor(dose))
  
data
}

data <- flydatamaker(n, means, sd)

data
```



## Task 1: *Background*
Type 1 diabetes (T1D) is commonly known as juvenile diabetes due to its ability to afflict young people. In   T1D, the T-cells in the body’s immune system attacks its own pancreas because the immune system is mistaking insulin producing cells in the pancreas as foreign; thus, automatically destroying them. Insulin allows glucose to enter the cells and convert the sugars to energy, so without the production of insulin glucose builds up in the blood stream and causes horrific effects on the body. Recent research seem to show environmental chemical exposure as one of the reasons for the spike in juvenile diabetes. The goal of this study is to assess the T1D effects from a polycyclic aromatic hydrocarbon called 2-aminoanthracene (2AA) exposure in juvenile rats. 2AA is a class of aromatic amines that is considered as mutagens carcinogens. Exposure to 2AA comes from its use in the manufacture of dyes, drugs, inks, and agricultural chemicals.



## Task 2: *What is unknown*
It is not known if exposure to the polycyclic aromatic hydrocarbon 2-aminoanthracene leads to the expression of genetic information and phenotypic characteristics associated with Type 1 Diabetes. A major characteristic of diabetes is increased blood glucose levels.  


## Task 3: *Prediction*
If male Sprague Dawley rat are exposed to 2-aminoanthracene through a diet consisting of 2AA combined with sucrose, then the rats will have increased blood glucose levels.

## Task 4: *Variables*
The dependent variable that will be observed in the experiment is the change in blood glucose levels within each treatment group, and the predictor variable is the length of time that blood glucose levels are measured, which is over the course of 80 minutes. The dependent variable is classified as a measured variable and the predictor is an ordered variable.


## Task 5: *Statistical Hypothesis*
Null Hypothesis: The observed blood glucose level will be the same amongst the control and exposure groups, over the extended length of time. 
Alternate Hypothesis: The blood glucose levels will be higher in rats that ingested the 2AA and sucrose solution. 

## Task 6: *Statistical Test*
I would use an ANOVA test to test my hypothesis. An ANOVA test would be best to use for my experiment and hypothesis because I am observing more than 1 independent treatment groups, and this type of testing is better for studying subjects that are generally the same. 

```{r}
anova_test <- ezANOVA(data=data, 
                dv=Blood_Glucose, 
                wid=id, 
                within=dose, 
                between=time, detailed=F)
anova_test
```


## Task 7: *Procedures and Decision Rules*

The following experiment was organized as a randomized study, in terms of each suject being randomly assigned their treatment type. Although, only male rats were used. I completed my analysis through an ANOVA test and Monte Carlo analysis. The independent replicates within my experiment are the treatments given to the rat models, in which are being monitered over the time course of 80 minutes. 

## Task 8: *Graph of simulated results*

```{r}
ggplot(data, aes("time", "Blood_Glucose"))+
  geom_jitter(width=0.1)+
  facet_grid(~"dose")


ggplot(data, aes(time, Blood_Glucose, color=dose, group=dose, "Control","Low","High"))+
  geom_point(size=2)

```


## Task 9: *Monte Carlo Analysis*

```{r}

sims=100

pval <- replicate(
  sims, {
 
    sample.df <- flydatamaker(n, means, sd)
    
   sim.ezaov <- ezANOVA(data=sample.df, 
                dv=Blood_Glucose, 
                wid=id, 
                within=dose, 
                between=time, 
            detailed=F)
  
  pval <- sim.ezaov$ANOVA[3,5]
    
    }
  )


pwr.pct <- sum(pval<0.05)/sims*100
pwr.pct
```
