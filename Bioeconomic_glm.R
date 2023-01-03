library(readr)
library(car)
library(dplyr)

# Importing dataframe
df <- read_delim("C:/Users/Viniciu/Desktop/Vinícius Fellype/Mestrado/Planilhas/Mod_tab/feiranovamod.csv", 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Changing values from a categorical column
df$esp_cult[df$esp_cult == "Camarão"] <- 0
df$esp_cult[df$esp_cult == "Tilápia"] <- 1

# Changing values from other categorical column
df$water_source[df$water_source == 'barragem'] <- 1.0
df$water_source[df$water_source == 'poço artesiano'] <- 2.0
df$water_source[df$water_source == 'compesa'] <- 3.0
df$water_source[df$water_source == 'várzea'] <- 4.0


# Changing the name of the variable
df <- rename(df, "peso_prod" = "peso_prod(g)")
df <- rename(df, "prec_kil" = "prec_kil(R$)")
df <- rename(df, "cons_pre" = "consult_pri(R$)")
df<- rename(df, "eletri" = "eletric_m(R$)")

# Checking the distribution of the data
barplot(df$sac_rac_an)

# Making a full model with production and economic variables

mod0 <- glm(log(sac_rac_an)~arealamhc+eletri+cons_pre+qutd_mil_juv_an+peso_prod+water_source+esp_cult+prec_kil+N_viveiros+asist_tec+trein_trab+qtd_berç+ciclo_ano+solo_treatpc+ qtd_prob_cicl+probiotico+Probiotico_água+Probiotico_ração+Probiótico_solo+peso_prod+water_source+preco_sac+prec_mijuv+prec_prob+numb_work,
            data=df, family=gaussian)

mod0<-step(mod0)
mod0$aic
mod0$coefficients

mod0_ivf <- vif(mod0)
barplot(mod0_ivf, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 6, lwd = 3, lty = 2)
dev.off()
# Reducing variables

mod1 <- glm(log(sac_rac_an)~arealamhc+qutd_mil_juv_an+peso_prod+water_source+esp_cult+N_viveiros,
            data = df, family = gaussian)
mod1<-step(mod1)
mod1$coefficients

mod1_ivf <- vif(mod1)
barplot(mod1_ivf, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 6, lwd = 3, lty = 2)

par(mfrow=c(2,2))
plot(mod1)
# It seems promissing, although, let's test with different link functions
# inverse
mod2<-glm(log(sac_rac_an)~peso_prod+N_viveiros,
          data=df, family = gaussian(link = "inverse"))
mod2$aic

par(mfrow=c(2,2))
plot(mod2)

# log
mod3<-glm(log(sac_rac_an)~peso_prod+N_viveiros,
          data=df, family = gaussian(link = "log"))
mod3$aic

par(mfrow=c(2,2))
plot(mod3)

# identity
mod4<-glm(log(sac_rac_an)~peso_prod+N_viveiros,
          data=df, family = gaussian(link = "identity"))
mod4$aic

#tiff("mod4_rac.tiff",width = 3600, height = 2760, units = "px", res = 300)
par(mfrow=c(2,2))
plot(mod4)
dev.off()

# mod4 was the model with better balance between aic and leverage of the data. 
# the 12th line of data presented a great leverage, although, as the data is not
# balanced, remove this data will make our model weaker.

# Let's test other models with interactions among variables

mod5 <- glm(log(sac_rac_an)~peso_prod+N_viveiros+arealamhc+arealamhc:N_viveiros+peso_prod:N_viveiros,
            data=df, family = gaussian(link="identity"))
mod5$aic
mod5<-step(mod5)
mod5$coefficients
# the interaction seems to be significant once the step method kept it in the model

#tiff("mod5_rac.tiff",width = 3600, height = 2760, units = "px", res = 300)
par(mfrow=c(2,2))
plot(mod5)
dev.off()
# The leverage of the 12th line is slightly stronger in this model than in mod4, although
# the residuals and location seem better.