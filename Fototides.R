library(dplyr)
library(tidyr)
library(glmmTMB)
library(DHARMa)
library(MuMIn)
library(emmeans)
library(multcomp)
library(ggplot2)
library(cowplot)
library(car)
library(readxl)

# Fig 1 ####

## Transmitacia filtros ####

datatransmit <- read_excel("~/0 - Fototides script/Dataset.xlsx", sheet = 6)

datatransmit <- datatransmit %>%
  pivot_longer(cols = -`Wavelength nm.`, names_to = "Filtro", values_to = "transm") %>%
  mutate(
    orden = `Wavelength nm.`,
    filtrok = case_when(
      str_detect(Filtro, "UV-BG") ~ "UVBG",
      str_detect(Filtro, "UV") ~ "UV",
      str_detect(Filtro, "Full Sun") ~ "FS",
      TRUE ~ NA_character_
    )
  ) 

figfilters<-ggplot(data= datatransmit %>% dplyr::filter(`Wavelength nm.`<667))+
  geom_ribbon(data = datatransmit %>%
                filter(filtrok=="FS", `Wavelength nm.`<667), 
              aes(x=`Wavelength nm.`, y=transm, ymin=0, ymax=transm), 
              fill = "goldenrod1", alpha = 0.3) +
  geom_ribbon(data = datatransmit %>%
                filter(filtrok=="UV",`Wavelength nm.`<667), 
              aes(x=`Wavelength nm.`, y=transm, ymin=0, ymax=transm),
              fill = "seagreen2", alpha = .3) +
  geom_ribbon(data = datatransmit %>%
                filter(filtrok=="UVBG",`Wavelength nm.`<667), 
              aes(x=`Wavelength nm.`, y=transm, ymin=0, ymax=transm), 
              fill = "magenta3", alpha = 0.3) +
  geom_line(aes(x=`Wavelength nm.`, y=transm, color=filtrok), size=.3, show.legend = T, alpha=.9)+
  theme_cowplot()+
  guides(fill=guide_legend(override.aes=list(color="black"))) + 
  xlab("Wavelenght (nm)")+
  ylab("Transmittance (%)")+
  scale_color_manual(values=c("goldenrod1","seagreen2", "magenta3"), 
                     name = "", 
                     labels = c("Full Sun", "UV attenuation", "UV+BG attenuation")) +
  theme(legend.position = c(0.02, 1.1),
        legend.justification = c("left", "top"),
        legend.direction = "vertical")

figfilters

## MAREA y RAD ####

dataMareas <- read_excel("~/0 - Fototides script/Dataset.xlsx", sheet = 7)
dataRadiacion <- read_excel("~/0 - Fototides script/Dataset.xlsx", sheet = 8)

dataRadiacion$Fecha <- as.Date(dataRadiacion$Fecha, format = "%Y-%m-%d")


dataMareas$ALTURA.m <-as.numeric(dataMareas$`ALTURA (m)`)

dataMareas$Fecha <- as.POSIXct(dataMareas$Fecha, format="%d/%m/%Y %H:%M")


Fig.1C <- ggplot() +
  geom_line(data = dataRadiacion, aes(y = `All Sky Surface Shortwave Downward Irradiance` * 3.6, x = as.Date(Fecha)), col = "#f48d00", alpha = .8) +
  geom_hline(yintercept = 14, linetype = "dashed", color = "black") + # Marisma baja
  geom_hline(yintercept = 21, linetype = "dotted", color = "black") + # Marisma alta
  annotate("text", 
           x = as.Date(c("2021-10-18", "2021-11-30", "2021-12-13", "2022-01-05", "2022-02-22", "2022-04-07", "2022-05-19", "2022-08-13")), 
           y = -1.2, 
           label = c("Initial", "1", "2", "3", "4", "5", "6", "7")) +
  geom_segment(data = data.frame(
    x = as.Date(c("2021-10-18", "2021-11-30", "2021-12-13", "2022-01-05", "2022-02-22", "2022-04-07", "2022-05-19", "2022-08-13")), 
    xend = as.Date(c("2021-10-18", "2021-11-30", "2021-12-13", "2022-01-05", "2022-02-22", "2022-04-07", "2022-05-19", "2022-08-13")), 
    y = 0.4, yend = 3), 
    aes(x = x, xend = xend, y = y, yend = yend), 
    arrow = arrow(length = unit(0.25, "cm")), color = "black") +
  theme_cowplot() +
  scale_y_continuous(name = expression(paste(atop("Incident radiation", "(MJ × m"^-2~"× day"^-2~")"))), 
                     sec.axis = sec_axis(~./10, name = "Tides (m)"), limits = c(-7, 35)) +
  geom_line(data = dataMareas, aes(x = as.Date(Fecha), y = ALTURA.m * 10), colour = 'turquoise4', alpha = .3, inherit.aes = FALSE) +
  scale_x_date(labels = scales::date_format("%b"), date_breaks = "1 month") +
  xlab("Date")

Fig.1C

ggdraw() +
  draw_plot(Fig.1C + 
              annotate("text", col="#E33D6F", x = as.Date(c("2021-10-18", "2021-11-30", "2021-12-13", "2022-01-05", "2022-02-22", "2022-04-07", "2022-05-19", "2022-08-13")), y = -2.8, label = c("■","■","■","■","■","■","■","■"))+
              annotate("text", col="#A666E1", x = as.Date(c("2021-10-18", "2021-12-13", "2022-04-07", "2022-08-13")), y = -3.8, label = c("●","●","●","●"), size = 6)+
              annotate("text", col="#D36F68", x = as.Date(c("2021-11-30", "2021-12-13", "2022-01-05", "2022-02-22", "2022-04-07", "2022-05-19", "2022-08-13")), y = -5.2, label = c("▲","▲","▲","▲","▲","▲","▲"), size = 6)+
              annotate("text", x = as.Date("2021-12-3"), y = -5.2, label = c("*"), size = 6)+
              annotate("text", col="#608F3D", size = 6, x = as.Date(c("2021-11-30", "2021-12-13", "2022-01-05", "2022-02-22", "2022-04-07", "2022-05-19", "2022-08-13")), y = -6.6, label = c("✖","✖","✖","✖","✖","✖","✖"))) 



# Organic matter ####

dataOM <- read_excel("~/0 - Fototides script/Dataset.xlsx", sheet = 2)

dataOM <- dataOM %>% dplyr::filter(`Nº sobre`!="134", `Nº sobre`!="212", `Nº sobre`!= "175")

## Proportion of MO ####

modMOprop<-glmmTMB(log(`prop. MO`*100)~Radiacion*Dias*Marea +(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)

plot(simulateResiduals(modMOprop)) 

summary(modMOprop) 

modMOprop1<-glmmTMB(log(`prop. MO`*100)~Radiacion+Dias+Marea+Radiacion:Dias+Radiacion:Marea+Dias:Marea +(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion) #-D:F:M
modMOprop2<-glmmTMB(log(`prop. MO`*100)~Radiacion+Dias+Marea+Dias:Radiacion+Radiacion:Marea +(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-D:M
modMOprop3<-glmmTMB(log(`prop. MO`*100)~Radiacion+Dias+Marea+Radiacion:Dias+Dias:Marea+(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-F:M
modMOprop4<-glmmTMB(log(`prop. MO`*100)~Radiacion+Dias+Marea+Radiacion:Marea+Dias:Marea +(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-D:F
modMOprop5<-glmmTMB(log(`prop. MO`*100)~Radiacion+Dias+Marea+Radiacion:Dias+(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-F:M - D:M
modMOprop6<-glmmTMB(log(`prop. MO`*100)~Radiacion+Dias+Marea+Marea:Radiacion +(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-D:F - D:M
modMOprop7<-glmmTMB(log(`prop. MO`*100)~Radiacion+Dias+Marea+Marea:Dias+(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-D:F - F:M
modMOprop8<-glmmTMB(log(`prop. MO`*100)~Radiacion+Dias+Radiacion:Dias+(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-M
modMOprop9<-glmmTMB(log(`prop. MO`*100)~Radiacion+Marea+Radiacion:Marea+(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-D
modMOprop10<-glmmTMB(log(`prop. MO`*100)~Dias+Marea+Dias:Marea +(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-F MOD11
modMOprop11<-glmmTMB(log(`prop. MO`*100)~Radiacion+Dias+(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-M - F:D
modMOprop12<-glmmTMB(log(`prop. MO`*100)~Radiacion+Marea+(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-D - F:M
modMOprop13<-glmmTMB(log(`prop. MO`*100)~Marea+Dias+(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-F - D:M
modMOprop14<-glmmTMB(log(`prop. MO`*100)~Radiacion+(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)# solo F
modMOprop15<-glmmTMB(log(`prop. MO`*100)~Dias+(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-solo D
modMOprop16<-glmmTMB(log(`prop. MO`*100)~Marea+(1|Dias), data=dataOM, family= gaussian, na.action = na.omit, dispformula = ~ Marea*Radiacion)#-solo M


aic_propMO <- as.data.frame(AICc(modMOprop, modMOprop1, modMOprop2, modMOprop3, modMOprop4, modMOprop5, modMOprop6, modMOprop7, modMOprop8, modMOprop9, modMOprop10, modMOprop11, modMOprop12, modMOprop13, modMOprop14, modMOprop15, modMOprop16))

aic_propMO$weight <- Weights(aic_propMO$AICc)

aic_propMO$AICc <- round(aic_propMO$AICc, 1)
aic_propMO$weight <- round(aic_propMO$weight, 3)

aic_propMO <- aic_propMO[order(aic_propMO$AICc), ]

aic_propMO #Filtro*Dias*Marea


### Filtro:Dias:Marea ####

cld((lstrends(modMOprop, c("Radiacion", "Marea"), var="Dias")), Letters=letters)

fig2<-ggplot(data = dataOM, aes(y = `prop. MO`*100, x = Dias, col=Radiacion, fill=Radiacion)) +
  stat_smooth(method = lm, se=T, alpha=.5, col="black", linewidth=.5,show.legend = FALSE, fill="grey") +
  geom_jitter(aes(fill=Radiacion), alpha=.8, col="black", width = 5, shape=21, size=2, show.legend = F) +
  facet_grid(Marea ~ Radiacion, labeller = 
               as_labeller(c("Marisma alta"="Upper marsh",
                             "Marisma baja"="Lower marsh",
                             "Full Sun"="Full Sun", "UV"= "UV\nattenuation", "UV+BG"="UV+BG\nattenuation"))) +
  theme_cowplot()+
  stat_smooth(method = lm, se=F, alpha=.5, col="black", linewidth=.5,show.legend = FALSE, fill="grey") +
  ylab("Organic matter remaining  (%)")+
  theme(legend.position = "bottom")+
  xlab(expression(paste(atop("Days"))))+
  scale_fill_manual(name = "Filter", values=c("goldenrod1", "magenta3", "seagreen2"),
                    labels = c("Full Sun", "UV attenuation", "UV+BG attenuation"))+
  scale_color_manual(name = "Filter", values=c("goldenrod1", "magenta3", "seagreen2"),
                     labels = c("Full Sun", "UV attenuation", "UV+BG attenuation"))+ 
  geom_text(
    data    = (data.frame(
      label = c("b","a","c","a","bc","a"),
      Marea   = c("Marisma baja", "Marisma alta"),
      Radiacion   = c("Full Sun", "UV", "UV+BG"))),
    mapping = aes(x = -Inf, y = -Inf, label = label),
    hjust   = -1.31,
    vjust   = -2.5, col="black", size=5)

fig2

# Lignin ####
dataLign <- read_excel("~/0 - Fototides script/Dataset.xlsx", sheet = 5)

modlig.conale <- glmmTMB((Lignin) ~ Dias * filtro * marea + (1|Dias) , data = dataLign, na.action = na.omit, family  = gaussian, dispformula = ~filtro)

plot(simulateResiduals(modlig.conale)) 

summary(modlig.conale)

modlig.conale1<-glmmTMB(Lignin~filtro+Dias+marea+filtro:Dias+filtro:marea+Dias:marea +(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro) #-D:F:M
modlig.conale2<-glmmTMB(Lignin~filtro+Dias+marea+Dias:filtro+filtro:marea +(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-D:M
modlig.conale3<-glmmTMB(Lignin~filtro+Dias+marea+filtro:Dias+Dias:marea+(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-F:M
modlig.conale4<-glmmTMB(Lignin~filtro+Dias+marea+filtro:marea+Dias:marea +(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-D:F
modlig.conale5<-glmmTMB(Lignin~filtro+Dias+marea+filtro:Dias+(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-F:M - D:M
modlig.conale6<-glmmTMB(Lignin~filtro+Dias+marea+marea:filtro +(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-D:F - D:M
modlig.conale7<-glmmTMB(Lignin~filtro+Dias+marea+marea:Dias+(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-D:F - F:M
modlig.conale8<-glmmTMB(Lignin~filtro+Dias+filtro:Dias+(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-M
modlig.conale9<-glmmTMB(Lignin~filtro+marea+filtro:marea+(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-D
modlig.conale10<-glmmTMB(Lignin~Dias+marea+Dias:marea +(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-F MOD11
modlig.conale11<-glmmTMB(Lignin~filtro+Dias+(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-M - F:D
modlig.conale12<-glmmTMB(Lignin~filtro+marea+(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-D - F:M
modlig.conale13<-glmmTMB(Lignin~marea+Dias+(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-F - D:M
modlig.conale14<-glmmTMB(Lignin~filtro+(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)# solo F
modlig.conale15<-glmmTMB(Lignin~Dias+(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-solo D
modlig.conale16<-glmmTMB(Lignin~marea+(1|Dias), data=dataLign, family= gaussian, na.action = na.omit, dispformula = ~ filtro)#-solo M


aic_propLig <- as.data.frame(AIC(modlig.conale, modlig.conale1, modlig.conale2, modlig.conale3, modlig.conale4, modlig.conale5, modlig.conale6, modlig.conale7, modlig.conale8, modlig.conale9, modlig.conale10, modlig.conale11, modlig.conale12, modlig.conale13, modlig.conale14, modlig.conale15, modlig.conale16))

aic_propLig$weight <- Weights(aic_propLig$AIC)

aic_propLig$AIC <- round(aic_propLig$AIC, 1)
aic_propLig$weight <- round(aic_propLig$weight, 3)

aic_propLig <- aic_propLig[order(aic_propLig$AIC), ]

aic_propLig 

confint((model.avg(modlig.conale3, modlig.conale7))) 

### Marea:Dias ####

fig3A<-ggplot(data = dataLign, aes(y = Lignin, x = Dias, col=marea, fill=marea)) +
  geom_jitter(aes(fill=marea), alpha=.8, col="black", width = 5, shape=21, size=2) +

  stat_smooth(method = lm, se=T, alpha=.5, col="black", linewidth=.5,show.legend = FALSE) +
  theme_cowplot()+
  ylab("Lignin (%)")+
  theme(legend.position = "top")+
  xlab(expression(paste(atop("Days"))))+
  scale_fill_manual(name = "Marsh elevation", values=c("skyblue", "skyblue4"),
                    labels = c("Upper", "Lower"))

fig3A

### Filtro ####

cld((emmeans(modlig.conale7, c("filtro"))), Letters=letters)

fig3B<-ggplot(data =dataLign, aes(y = Lignin, x = filtro, fill=filtro)) +
  geom_boxplot(show.legend = F, width = .4, alpha=.6)+
  geom_jitter(aes(fill=filtro), alpha=.8, col="black", width = .2, shape=21, size=2,show.legend = F) +
  stat_smooth(method = lm, se=T, alpha=.5, col="black", linewidth=.5,show.legend = FALSE) +
  theme_cowplot()+
  ylab("Lignin remaining (%)")+
  theme(legend.position = "bottom")+
  xlab("")+
  scale_x_discrete(labels = c("Full Sun" = "Full Sun", "UV" = "UV\nattenuation", "UV-BG" = "UV+BG\nattenuation")) +
  scale_fill_manual(name = "Marsh", values=c("goldenrod1", "magenta3", "seagreen2"),
                    labels = c("Upper", "Lower"))+
  geom_text(data = data.frame(
    label = c("a", "a", "b"),
    filtro = c("Full Sun", "UV", "UV-BG"),
    y_position = c(9.2, 9.2, 9.2)), 
    mapping = aes(x = filtro, y = y_position, label = label),
    hjust = 0.5, vjust = 0, col="black", size=5)

fig3B

# Total C ####

data_TCcont <- read_excel("~/0 - Fototides script/Dataset.xlsx", sheet = 4)

data_TCcont$propC <- (data_TCcont$`Total C (µg)`) / (data_TCcont$`Peso m 2`*1000)

PropdeC <- glmmTMB((propC*100) ~ filtro*marea*dias +(1|dias), family  = gaussian(link = "log"), data = data_TCcont, na.action = na.omit, dispformula = ~dias)

plot(simulateResiduals(PropdeC))

Anova(PropdeC)

PropdeC1<-glmmTMB((propC*100)~filtro+dias+marea+filtro:dias+filtro:marea+dias:marea +(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias) #-D:F:M
PropdeC2<-glmmTMB((propC*100)~filtro+dias+marea+dias:filtro+filtro:marea +(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-D:M
PropdeC3<-glmmTMB((propC*100)~filtro+dias+marea+filtro:dias+dias:marea+(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-F:M
PropdeC4<-glmmTMB((propC*100)~filtro+dias+marea+filtro:marea+dias:marea +(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-D:F NO CORRE
PropdeC4_optim <- update(PropdeC4,
                         control=glmmTMBControl(optimizer=optim,
                                                optArgs=list(method="BFGS")))
PropdeC5<-glmmTMB((propC*100)~filtro+dias+marea+filtro:dias+(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-F:M - D:M
PropdeC6<-glmmTMB((propC*100)~filtro+dias+marea+marea:filtro +(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-D:F - D:M
PropdeC7<-glmmTMB((propC*100)~filtro+dias+marea+marea:dias+(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-D:F - F:M NO CORRE
PropdeC7_optim <- update(PropdeC7,
                         control=glmmTMBControl(optimizer=optim,
                                                optArgs=list(method="BFGS")))
PropdeC8<-glmmTMB((propC*100)~filtro+dias+filtro:dias+(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-M
PropdeC9<-glmmTMB((propC*100)~filtro+marea+filtro:marea+(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-D
PropdeC10<-glmmTMB((propC*100)~dias+marea+dias:marea +(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-F MOD11 NO CORRE
PropdeC10_optim <- update(PropdeC10,
                          control=glmmTMBControl(optimizer=optim,
                                                 optArgs=list(method="BFGS")))
PropdeC11<-glmmTMB((propC*100)~filtro+dias+(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-M - F:D
PropdeC12<-glmmTMB((propC*100)~filtro+marea+(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-D - F:M
PropdeC13<-glmmTMB((propC*100)~marea+dias+(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-F - D:M
PropdeC14<-glmmTMB((propC*100)~filtro+(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)# solo F
PropdeC15<-glmmTMB((propC*100)~dias+(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-solo D
PropdeC16<-glmmTMB((propC*100)~marea+(1|dias), data=data_TCcont, family= gaussian (link = "log"), na.action = na.omit, dispformula = ~dias)#-solo M


aic_propC <- as.data.frame(AICc(PropdeC, PropdeC1,PropdeC2, PropdeC3, PropdeC4_optim, PropdeC5, PropdeC6, PropdeC7_optim, PropdeC8, PropdeC9, PropdeC10_optim, PropdeC11, PropdeC12, PropdeC13, PropdeC14, PropdeC15, PropdeC16))


aic_propC$weight <- Weights(aic_propC$AICc)

aic_propC$AICc <- round(aic_propC$AICc, 1)
aic_propC$weight <- round(aic_propC$weight, 3)

aic_propC <- aic_propC[order(aic_propC$AIC), ]

aic_propC  

confint((model.avg(PropdeC3, PropdeC1, PropdeC))) 


### Marea:dias####

cld(emmeans::lstrends(PropdeC3, c("marea"), var="dias"), Letters=letters)
plot(emmeans::lstrends(PropdeC3, c("marea"), var="dias"), Letters=letters)

Fig4<-ggplot(data_TCcont) +
  geom_jitter(aes(group = interaction(marea, filtro, dias),
                  x = dias, y = propC*100, fill = marea), alpha=.6, size=3, shape=21,
              stat = "identity", position=position_dodge (20), show.legend = T) +
  stat_smooth(aes(x = dias, y = propC*100, fill = marea), method = glm, method.args = list(family = gaussian(link = "log")), col="black", se=T, alpha=.5) +
  labs(y = "TC remanent (%)") +
  xlab("Days")+
  scale_fill_manual(values=c("skyblue","skyblue4"), name = "Marsh", labels = c("Upper", "Lower")) +
  labs(color = "Marsh elevation")+
  scale_color_manual(values=c("gray30","gray30"))+
  theme_cowplot()+
  theme(legend.position = "bottom")

Fig4
# Enzymes ####

dataBeta <- read_excel("~/0 - Fototides script/Dataset.xlsx", sheet = 2)

todobeta <- NULL

for(vuelta in 1:1000){
  print(vuelta)
  entrada<- suppressMessages(dataBeta %>% filter(Dias!="0" & `Nº sobre` != "134" & `Nº sobre` != "175" & `Nº sobre` != "212") %>% group_by(Dias, Marea, Radiacion) %>% summarise( p2= sample(`Actividad enzimática beta`,1)))
  for(marea in unique(tabla $ Marea)){
    for(filt in unique(tabla $ Radiacion)){
      xvuelta <- entrada %>% group_by(Radiacion, Marea)%>%mutate(acu = accumulate(p2, sum)) %>% dplyr::select(-p2) %>% mutate(vuelta = vuelta)
      todobeta <- bind_rows(todobeta, xvuelta)
    }
  }
}

betaacum<-todobeta %>% group_by(Radiacion, Marea, Dias) %>% mutate(q5=quantile(acu,0.025), q95=quantile(acu,0.975), rango =q95-q5)
betaacum$Radiacion<-as.factor(betaacum$Radiacion)
betaacum$Marea<-as.factor(betaacum$Marea)
str(betaacum)

Fig5A <- ggplot(betaacum, aes(Dias, acu, fill=Radiacion))+
  facet_grid(Marea~., labeller = as_labeller(c("Marisma alta"="Upper marsh","Marisma baja"="Lower marsh", "Full Sun"="Full Sun", "UV"= "UV attenuation", "UV+BG"="UV+BG attenuation" )))+
  ylab(expression(paste(atop("Accumulated β-glucosidase activity", (μmol~product~"✕"~g~dry~mass^-1)))))+ 
  xlab("Days")+  
  geom_bar(data=betaacum %>%group_by(Dias, Marea, Radiacion)%>% summarise(prom=mean(acu)), aes(x= Dias, y =prom, fill=Radiacion), inherit.aes = F, stat="identity", color="black", position="dodge2")+
  geom_errorbar(data=betaacum, aes(Dias, ymin=q5, ymax=q95, fill=Radiacion), inherit.aes = F, width=1, position=position_dodge(width=12))+
  theme_cowplot()+
  theme(legend.position = "top")+
  labs(fill='Filter')+
  scale_fill_manual(values=c("goldenrod1","magenta3","seagreen2"), labels=c("Full Sun", "UV attenuation", "UV+BG attenuation")) + scale_x_continuous(breaks=c(0,43,56,79,127,171,213,299))

Fig5A

# Richness of morphotypes ####

datafungi <- read_excel("~/0 - Fototides script/Dataset.xlsx", sheet = 3)

str(datafungi)
datafungi$marea<-as.factor(datafungi$marea)
datafungi$filtro<-as.factor(datafungi$filtro)


modeloriq<- glmmTMB(riq ~ marea * filtro + (1|cosecha), data = datafungi) 
summary(modeloriq)
plot(simulateResiduals(modeloriq))

Anova(modeloriq)

modeloriq1<- glmmTMB(riq ~ marea + filtro + (1|cosecha), data = tabla.diversidad) 
modeloriq2<- glmmTMB(riq ~ marea + (1|cosecha), data = tabla.diversidad) 
modeloriq3<- glmmTMB(riq ~ filtro + (1|cosecha), data = tabla.diversidad) 
modeloriq4<- glmmTMB(riq ~ 1 + (1|cosecha), data = tabla.diversidad) 



aic_diver <- as.data.frame(AICc(modeloriq, modeloriq1, modeloriq2, modeloriq3, modeloriq4))

aic_diver$weight <- Weights(aic_diver$AICc)

aic_diver$AICc <- round(aic_diver$AICc, 1)
aic_diver$weight <- round(aic_diver$weight, 3)

aic_diver  

confint((model.avg(modeloriq, modeloriq1, modeloriq2, modeloriq3, modeloriq4)))


tabla.diversidad$filtros<-tabla.diversidad$filtro
tabla.diversidad$filtros <- as.factor(tabla.diversidad$filtros)
levels(tabla.diversidad$filtros) <- c("Full Sun", "UV attenuation", "UV+BG attenuation")

tabla.diversidad$mareas<-tabla.diversidad$marea
tabla.diversidad$mareas <- as.factor(tabla.diversidad$mareas)
levels(tabla.diversidad$mareas) <- c("Upper marsh", "Lower marsh")

Fig6<- ggplot((tabla.diversidad %>%
                 arrange(filtro, mareas, riq)), aes(x = filtro, y = riq, fill = mareas)) +
  geom_boxplot(aes(fill = mareas), alpha = 0.5, coef = 100000000000000000000) +
  geom_jitter(position = position_jitterdodge(jitter.width = .25, dodge.width = 0.75), 
              size = 2, show.legend = F, alpha=.5) +
  theme_cowplot() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "Marsh elevation", values=c("skyblue", "skyblue4"),
                    labels = c("Upper", "Lower"))+
  scale_x_discrete(labels = c("Full Sun", "UV\nattenuation", "UV+BG\nattenuation")) +
  xlab("") +
  ylab("Richness of fungi")
Fig6

