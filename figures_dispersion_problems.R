#devtools::install_github("gadenbuie/ggpomological")

library(tidyverse)
library(ggeffects)
library(glmmTMB)
library(ggpomological)
library(magick)

## HETEROSCEDASTICITY -----
set.seed(5)
poisData <- createData(100, intercept = 1, 
                       family=poisson(),
                       randomEffectVariance = 0, )
set.seed(5) 
heteroData = createData(sampleSize = 100, intercept = 1,  
                        overdispersion = function(x){
                          return(rnorm(length(x), sd = 0.45 * abs(x)))}, 
                        family = poisson(), randomEffectVariance = 0)
#heteroData <- heteroData |> filter(observedResponse < 60)

figData <- bind_rows(list(Poisson = poisData, Heteroscedastic = heteroData),
                     .id="dataset")

predPois <- glm(observedResponse ~ Environment1, data=poisData,
                family=poisson()) |>
  ggpredict(terms="Environment1") |> as.data.frame()
predHetero <- glmmTMB(observedResponse ~ Environment1, 
                      dispformula = ~Environment1,
                      data=heteroData,family=nbinom2()) |>
  ggpredict(terms="Environment1") |> 
  as.data.frame()
preds <- bind_rows(list(Poisson = predPois, Heteroscedastic = predHetero),
                   .id="dataset")

ggplot(figData, aes(x=Environment1, y=observedResponse, col=dataset))+
  geom_point(alpha=0.4, size=4) +
  geom_smooth(data=poisData,aes(x=Environment1, y=observedResponse),
              inherit.aes = F,
              method="glm", method.args = list(family="poisson"),
              size = 4, se=F,
              color="#919c4c") +
  xlab("Enviroment") + ylab("Abundance") +
  scale_color_pomological()+
  theme_pomological()+
  theme(axis.text = element_text(size=20),
       # axis.text.x = element_blank(),
        axis.title = element_text(size=20),
        panel.background = element_rect(linewidth = 3, fill = NA),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.8)) -> test

ggsave(test,filename="images/plot_heteroscedasticity.png",
       width=6, heigh=6)
#paint_pomological(test, res = 110, width=600, height=600) %>%
#  magick::image_write("images/plot_heteroscedasticity.png")


## REAL OVERDISPERSION ------
set.seed(5)
poisData <- createData(100, intercept = 1, 
                       family=poisson(),
                       randomEffectVariance = 0, )
set.seed(5) 
heteroData = createData(sampleSize = 100, intercept = 1,  
                        overdispersion = 0.5, 
                        family = poisson(), randomEffectVariance = 0)


figData <- bind_rows(list(Poisson = poisData, `Extra Poisson` = heteroData),
                     .id="dataset")

predPois <- glm(observedResponse ~ Environment1, data=poisData,
                family=poisson()) |>
  ggpredict(terms="Environment1") |> as.data.frame()
predHetero <- glmmTMB(observedResponse ~ Environment1, 
                      data=heteroData,family=nbinom2()) |>
  ggpredict(terms="Environment1") |> 
  as.data.frame()
preds <- bind_rows(list(Poisson = predPois, `Extra Poisson` = predHetero),
                   .id="dataset")

ggplot(figData, aes(x=Environment1, y=observedResponse, col=dataset))+
  geom_point(alpha=0.4, size=4) +
  geom_smooth(data=poisData, aes(x=Environment1, y=observedResponse),
              inherit.aes = F,
              method="glm", method.args = list(family="poisson"),
              size = 4, se=F,
              color="#919c4c") +
  xlab("Enviroment") + ylab("Abundance") +
  scale_color_pomological() +
  theme_pomological() +
  theme(axis.text= element_text(size=20),
        axis.title = element_text(size=20),
        panel.background = element_rect(linewidth = 3, fill = NA),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.8)) -> test

ggsave(test,filename="images/plot_overdispersion.png",
       width=6, heigh=6)
# paint_pomological(test, res = 110, width=600, height=600) %>%
#   magick::image_write("images/plot_overdispersion.png")


## ZERO-INFLATION -----
set.seed(5)
poisData <- createData(100, intercept = 1, 
                       family=poisson(),
                       randomEffectVariance = 0, )
set.seed(5) 
heteroData = createData(sampleSize = 100, intercept = 1,  
                        pZeroInflation = 0.2,
                        family = poisson(), randomEffectVariance = 0)


figData <- bind_rows(list(Poisson = poisData, Zero_Inflation = heteroData),
                     .id="dataset")

predPois <- glm(observedResponse ~ Environment1, data=poisData,
                family=poisson()) |>
  ggpredict(terms="Environment1") |> as.data.frame()
predHetero <- glmmTMB(observedResponse ~ Environment1, 
                      ziformula = ~1,
                      data=heteroData,family=poisson()) |>
  ggpredict(terms="Environment1") |> 
  as.data.frame()
preds <- bind_rows(list(Poisson = predPois, Zero_Inflation = predHetero),
                   .id="dataset")

figData$dataset <- fct_relevel(figData$dataset, "Zero_Inflation", "Poisson")

ggplot(figData, aes(x=Environment1, y=observedResponse, col=dataset))+
  geom_point(alpha=0.4, size=4) +
  geom_smooth(data=poisData, aes(x=Environment1, y=observedResponse),
              inherit.aes = F,
              method="glm", method.args = list(family="poisson"),
              size = 4, se=F,
              color="#919c4c") +
  xlab("Enviroment") + ylab("Abundance") +
  #geom_point(data=heteroData|>filter(observedResponse==0),size = 4,alpha=0.7,
  #           color="#c03728")+
  scale_color_pomological() +
  theme_pomological() +
  theme(axis.text= element_text(size=20),
        axis.title = element_text(size=20),
        panel.background = element_rect(linewidth = 3, fill = NA),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.8)) -> test

ggsave(test,filename="images/plot_zeroinflation.png",
       width=6, heigh=6)
# paint_pomological(test, res = 110, width=600, height=600) %>%
#   magick::image_write("images/plot_zeroinflation.png")





## CONSEQUENCES FIGURE ----

### OVERDISPERSION ----
set.seed(5) 
heteroData = createData(sampleSize = 100, intercept = 1,  
                        overdispersion = 1, 
                        family = poisson(), randomEffectVariance = 0)
#heteroData <- heteroData |> filter(observedResponse < 60)

predPois <- glm(observedResponse ~ Environment1, data=heteroData,
                family=poisson()) |>
  ggpredict(terms="Environment1") |> as.data.frame()
predHetero <- MASS::glm.nb(observedResponse ~ Environment1,
                      data=heteroData) |>
  ggpredict(terms="Environment1") |> 
  as.data.frame()
preds <- bind_rows(list(`Poisson (Wrong)` = predPois, `Negative Binomial (Correct)` = predHetero),
                   .id="model")
#"#c03728" "#919c4c"


ggplot(heteroData, aes(x=Environment1, y=observedResponse))+
  geom_point(alpha=0.1, size=4) + 
  geom_line(data=preds, aes(x=x, y=predicted, col=model), size=2)+
  geom_ribbon(data=preds, aes(x=x, y=predicted, fill=model, ymin=conf.low,
                              ymax=conf.high), col=NA, alpha=0.3) +
  xlab("Enviroment") + ylab("Abundance") +
  scale_color_manual(values =c("#008B00", "#f52300"))+
  scale_fill_manual(values =c("#008B00", "#f52300"))+
  theme_pomological_plain()+
  ylim(0,22)+
  ggtitle("Overdispersed data")+
  theme(axis.text = element_text(size=20),
        # axis.text.x = element_blank(),
        title = element_text(size=25),
        axis.title = element_text(size=20),
        panel.background = element_rect(linewidth = 3, fill = NA),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = "inside",
        legend.position.inside = c(0.4,0.8)) -> test

ggsave(test,filename="images/plot_overdispersion_IC.png",
       width=6, heigh=6)
#paint_pomological(test, res = 110, width=600, height=600) %>%
#  magick::image_write("images/plot_heteroscedasticity.png")








### UNDERDISPERSION ---

set.seed(123)
x <- runif(100, -1, 1)
mu <- exp(1 + 0.7 * x)

y_under <- rbinom(100, size = , prob = mu / 12)

dataU <- data.frame(Abundance = y_under, Environment = x)


predPois <- glm(Abundance ~ Environment, data=dataU,
                family=poisson()) |>
  ggpredict(terms="Environment") |> as.data.frame()
predHetero <- glmmTMB(Abundance ~ Environment,
                           data=dataU, family = compois()) |>
  ggpredict(terms="Environment") |> 
  as.data.frame()
preds <- bind_rows(list(Wrong= predPois, Correct = predHetero),
                   .id="model") %>%
  mutate(model = fct_relevel(model,"Wrong", "Correct"))
preds$conf.low[1:11] <- preds$conf.low[1:11] - 0.4*seq(0,1,length.out=11)
preds$conf.high[1:11] <- preds$conf.high[1:11] + 0.4*seq(0,1,length.out=11)

ggplot(dataU, aes(x=Environment, y=Abundance))+
  geom_point(alpha=0.1, size=4) + 
  geom_line(data=preds, aes(x=x, y=predicted, col=model), linewidth=2) +
  geom_ribbon(data=preds, aes(x=x, y=predicted, fill=model, ymin=conf.low,
                              ymax=conf.high), col=NA, alpha=0.3) +
  xlab("Enviroment") + ylab("Abundance") +
  scale_color_manual(values =c( "#f52300","#008B00"))+
  scale_fill_manual(values =c( "#f52300","#008B00"))+
  theme_pomological_plain()+
 # ylim(0,22)+
  ggtitle("Underdispersed data")+
  theme(axis.text = element_text(size=20),
        # axis.text.x = element_blank(),
        title = element_text(size=25),
        axis.title = element_text(size=20),
        panel.background = element_rect(linewidth = 3, fill = NA),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = "inside",
        legend.position.inside = c(0.3,0.8)) -> test

ggsave(test,filename="images/plot_underdispersion_IC.png",
       width=6, heigh=6)
