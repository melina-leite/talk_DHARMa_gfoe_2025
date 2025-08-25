
```{r}
set.seed(5) 
heteroData = createData(sampleSize = 500, intercept = 0.5,  
                        overdispersion = function(x){
                          return(rnorm(length(x), sd = 0.45 * abs(x)))}, 
                        family = poisson(), randomEffectVariance = 0.01)

ggplot(heteroData, aes(x=Environment1, y=observedResponse))+
  geom_point() +
  geom_smooth(method="glm", method.args = list(family="poisson"))

heteroModel <- glmmTMB(observedResponse ~ Environment1 + (1|group), 
                       family = "poisson", data = heteroData)
summary(heteroModel)

# unconditional sim
heteroRes <- simulateResiduals(heteroModel)
plot(heteroRes)
testDispersion(heteroRes, plot=F)

# conditional sim
set_simcodes(heteroModel$obj, val="fix")
heteroResC <- simulateResiduals(heteroModel)
testDispersion(heteroResC, plot=F)

# corrected model
heteroModelCorrect <- glmmTMB(observedResponse ~ Environment1 + (1|group),
                              dispformula = ~Environment1,
                              family = nbinom2(), data = heteroData)
summary(heteroModelCorrect)
heteroResCorrect <- simulateResiduals(heteroModelCorrect)
plot(heteroResCorrect)
testDispersion(heteroResCorrect, plot=F)
# conditional sim
set_simcodes(heteroModelCorrect$obj, val="fix")
heteroResCorrectC<- simulateResiduals(heteroModelCorrect)
testDispersion(heteroResCorrectC, plot=F)
```


```{r}
set.seed(5) 
zeroData <- createData(sampleSize = 500, intercept = 2,
                       randomEffectVariance = 0.01,
                       pZeroInflation = 0.6)
ggplot(zeroData, aes(x=Environment1, y=observedResponse))+
  geom_point() +
  geom_smooth(method="glm", method.args = list(family="poisson"))

zeroModel <- glmmTMB(observedResponse ~ Environment1 + (1|group), 
                     family = "poisson", data = zeroData)
summary(zeroModel)

# unconditional sim
zeroRes <- simulateResiduals(zeroModel)
plot(zeroRes)
testDispersion(zeroRes, plot=F)

# conditional sim
set_simcodes(zeroModel$obj, val="fix")
zeroResC <- simulateResiduals(zeroModel)
testDispersion(zeroResC, plot=F)

# corrected model
zeroModelCorrect <- glmmTMB(observedResponse ~ Environment1 + (1|group),
                            ziformula = ~1,
                            family = poisson(), data = zeroData)
summary(zeroModelCorrect)
zeroResCorrect <- simulateResiduals(zeroModelCorrect)
plot(zeroResCorrect)
testDispersion(zeroResCorrect, plot=F)
# conditional sim
set_simcodes(zeroModelCorrect$obj, val="fix")
zeroResCorrectC<- simulateResiduals(zeroModelCorrect)
testDispersion(zeroResCorrectC, plot=F)

plot(ggpredict(zeroModelCorrect, term="Environment1", type = "zi_prob")) +
  ylim(0,1)



```


```{r}
set.seed(5) 
overData <- createData(sampleSize = 500, intercept = 1,
                       randomEffectVariance = 0.01,
                       overdispersion = 0.8)
ggplot(overData, aes(x=Environment1, y=observedResponse))+
  geom_point() +
  geom_smooth(method="glm", method.args = list(family="poisson"))

overModel <- glmmTMB(observedResponse ~ Environment1 + (1|group), 
                     family = "poisson", data = overData)
summary(overModel)

# unconditional sim
overRes <- simulateResiduals(overModel)
plot(overRes)
testDispersion(overRes, plot=F)

# conditional sim
set_simcodes(overModel$obj, val="fix")
overResC <- simulateResiduals(overModel)
testDispersion(overResC, plot=F)

# corrected model
overModelCorrect <- glmmTMB(observedResponse ~ Environment1 + (1|group),
                            family = nbinom2(), data = overData)
summary(overModelCorrect)
overResCorrect <- simulateResiduals(overModelCorrect)
plot(overResCorrect)
testDispersion(overResCorrect, plot=F)
# conditional sim
set_simcodes(overModelCorrect$obj, val="fix")
overResCorrectC<- simulateResiduals(overModelCorrect)
testDispersion(overResCorrectC, plot=F)

```