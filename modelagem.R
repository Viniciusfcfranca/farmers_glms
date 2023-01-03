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

# Checking the df
print(df)

# Making a full model
mod0 <- glm(log(prod_an)~arealamhc+N_viveiros+qtd_prob_cicl+ esp_cult + water_source+ educ + qtd_berç + ciclo_ano + solo_treatpc + Probiotico_água+Probiotico_ração+Probiótico_solo+peso_prod+qutd_mil_juv_an+asist_tec+n_tecn+trein_trab+yers_cult+numb_work+numb_famwork,
            data=df,family=gaussian)
AIC(mod0)
step(mod0)
coefficients(mod0)
# variables with non avaliable coefficients are going to be inored in the follow
# models

#Making a  model
mod1 <- glm(log(prod_an)~arealamhc + N_viveiros + qtd_prob_cicl+esp_cult + water_source + educ + qtd_berç + ciclo_ano + solo_treatpc+ Probiotico_água,
            data = df, family=Gamma)

# Checking the variance inflation factor
vif_mod1<-vif(mod1)
barplot(vif_mod1, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 6, lwd = 3, lty = 2)

mod1$aic
mod1 <- step(mod0)
mod1$aic

resid(mod1)
par(mfrow=c(2,2))
plot(mod1)
dev.off()

coefficients(mod1)

# Let's minimize the number of variables, by removing the least important components in the model

# Making another model
mod2gau <- glm(log(prod_an)~arealamhc+N_viveiros + esp_cult + water_source+qtd_berç + ciclo_ano + Probiotico_água,
            data=df,family=gaussian)

mod2gau$aic
mod2gau <- step(mod2gau)
mod2gau$aic
# AIC had a significant improvement by reducing the variables

vif_mod2gau<-vif(mod2gau)
barplot(vif_mod2gau, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 6, lwd = 3, lty = 2)

par(mfrow=c(2,2))
plot(mod2gau)
dev.off()

coefficients(mod2gau)
# Despite the low AIC in this model, we faced high VIF and some values leveraging the model, let's check with the Gamma family.

mod2 <- glm(log(prod_an)~arealamhc+N_viveiros + esp_cult + water_source+qtd_berç + ciclo_ano + Probiotico_água,
            data=df,family=Gamma)

mod2$aic
mod2 <- step(mod2)
mod2$aic

vif_mod2<-vif(mod2)
barplot(vif_mod2, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 6, lwd = 3, lty = 2)

par(mfrow=c(2,2))
plot(mod2)
dev.off()

coefficients(mod2)


# Despite this model seems promising, its VIF are high, so we could minimize the variables in order to avoid biased models
# The variables that increased the IVF in this model were the number of productive
# cicles in a year and the cultured specie, so we are going to remove one of these variables and check the model performance

# Third model
mod3 <- glm(log(prod_an)~N_viveiros+esp_cult+qtd_berç + Probiotico_água,
            data=df,family=Gamma)

mod3$aic
mod3 <- step(mod3)
mod3$aic

vif_mod3<-vif(mod3)
barplot(vif_mod3, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)

par(mfrow=c(2,2))
plot(mod3)
dev.off()

coefficients(mod3)

# This model only used two variables, let's see if we add the quantitie of probiotics will improve it

mod4 <- glm(log(prod_an)~N_viveiros+Probiotico_água + qtd_prob_cicl,
            data = df, family= Gamma)

mod4$aic
vif_mod4<-vif(mod4)
barplot(vif_mod4, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)

par(mfrow=c(2,2))
plot(mod4)
dev.off()

coefficients(mod4)

# The addition of the quantity of probiotic didn't improve the models predictions
# Now, let's test the model with different link functions.

# Mod 5
mod5 <- glm(log(prod_an)~N_viveiros+Probiotico_água+qtd_prob_cicl,
            data=df,family=Gamma(link = inverse))
mod5$aic

vif_mod5<-vif(mod5)
barplot(vif_mod5, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)

par(mfrow=c(2,2))
plot(mod5)
dev.off()

mod6 <- glm(log(prod_an)~N_viveiros+Probiotico_água+qtd_prob_cicl,
            data=df,family=Gamma(link = identity))
mod6$aic

par(mfrow=c(2,2))
plot(mod6)
dev.off()

mod7 <- glm(log(prod_an)~N_viveiros+Probiotico_água+qtd_prob_cicl,
            data=df,family=Gamma(link = 'log'))
mod7$aic
#tiff("mod7.tiff",width = 3600, height = 2760, units = "px", res = 300)
par(mfrow=c(2,2))
plot(mod7)

dev.off()
# The link log was the link function whose made the better model for yearly production.
###############################################################

#We have removed the variable of productive cicles at first. Now, we will remove the 
# variable of cultured species and model it all again to see if we have different results.

mod8 <- glm(log(prod_an)~N_viveiros+ciclo_ano+qtd_berç + Probiotico_água,
            data=df,family=Gamma)

mod8$aic
mod8 <- step(mod8)
mod8$aic
coefficients(mod8)
# It showed the same result. Let's see with different variables that were pointed
# as important but ignored previously

mod9 <- glm(log(prod_an)~arealamhc+N_viveiros+qtd_berç + water_source,
            data=df,family=Gamma)

mod9$aic
mod9 <- step(mod9)
mod9$aic
coefficients(mod9)

mod9_ivf <- vif(mod9)
barplot(mod9_ivf, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)
dev.off()

par(mfrow=c(2,2))
plot(mod9)
#It showed a high leverage due to the 12th line of data, which showed that it
#wasn't capable of generalize as the mod7.

#With that we finish our production glm using the data from Feira Nova city producers
# choosing mod 7 as our better model.