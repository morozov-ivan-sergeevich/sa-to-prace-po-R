library(factoextra)
library(readr)
library(dplyr)

data = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1",
                 skip=1, comment = "[")
data = data[-1,]
#скрыта недействительная строчка с кучей NA

data[data == -9999] = NA
#при некорректном значении "-9999" приравнять к NA

data <- data %>% filter(year(date) == 2013)
data <- data %>% filter(month(date) %in% c(09, 10, 11))
# получили данные осени 2013 года

#оставили строки только числовые
data = data %>% select(where(is.numeric))
#оставили столбцы без тех что относятся к h2o
data =-data %>% select(-contains("h2o"), h2o_flux)
#создали таблицу var_data со значениями дисперсии каждого показателя
var_data = data %>% summarise_all( ~var(.x,na.rm=T))
#там где результат получился NA, заменяем на 0
var_data[is.na(var_data)] = 0
#создаем таблицу cpa_data используя var_data чтобы исключить показатели с нулевой дисперсией
cpa_data = data[,as.logical(var_data != 0)]
#создали корреляционную таблицу на основе cpa_data исключив столбцы содержащие NA
cor_matrix = cor(na.exclude(cpa_data))
#создали тепловую карту корреляционной матрицы
heatmap(cor_matrix)
#сделали выборку значений из матрицы для H2O
h2o_cor = as.numeric(cor_matrix[82,])
#проименовали эту выборку
names(h2o_cor) = names(cor_matrix[82,])
#выбрали только те значения, что входят в диапазон коэффициента влияния
cpa_dataf = cpa_data[,h2o_cor > 0.35 | h2o_cor < -.35]

#Чтобы не возникало ошибок со специальными символами, переименуем некоторые переменные
data = data %>% rename(z_sub_d__L = `(z-d)/L`)
data = data %>% rename_with( 
  ~gsub("-","_sub_",.x, fixed = T))
data = data %>% rename_with( 
  ~gsub("*","_star",.x, fixed = T))
data = data %>% rename_with( 
  ~gsub("%","_p",.x, fixed = T))
data = data %>% rename_with( 
  ~gsub("/","__",.x, fixed = T))

cpa_dataf = cpa_dataf %>% rename_with( 
  ~gsub("-","_sub_",.x, fixed = T))
cpa_dataf = cpa_dataf %>% rename_with( 
  ~gsub("*","_star",.x, fixed = T))
cpa_dataf = cpa_dataf %>% rename_with( 
  ~gsub("%","_p",.x, fixed = T))
cpa_dataf = cpa_dataf %>% rename_with( 
  ~gsub("/","__",.x, fixed = T))

#произвели анализ основных компонентов
data_pca = prcomp(na.exclude(cpa_dataf),scale=TRUE)
#отобразили розу ветров для влияния различных показателей
fviz_pca_var(data_pca,repel = TRUE, col.var = "salmon4")
#создали модель линейной регрессии исходя из показателей, максимально приближенных к h2o_flux по влиянию
model = lm(h2o_flux ~ flowrate+air_temperature+LE+co2_molar_density+RH, data)
#вывели сводную по получившейся модели
summary(model)
#Далее последовательно несколько раз проводим создания линейных моделей, убирая переменные из формулы, оказывающие наименьшее воздействие на модель 
#Начиная с третьей итерации опираемся на дисперсионный анализ ANOVA для исключения переменных

formula = paste(c("h2o_flux ~ ",
                  paste(names(cpa_dataf)[-27], collapse = "+")),
                collapse = "")
formula = as.formula(formula)


model_first = lm(formula, cpa_dataf)
summary(model_first)

formula2 = formula(h2o_flux ~ LE + rand_err_LE + un_H + un_LE + un_co2_flux + ts_spikes + w__ts_cov + w__co2_cov
                   + flowrate )
model2 = lm(formula2, cpa_dataf)
summary(model2)

formula3 = formula(h2o_flux ~ LE + rand_err_LE + un_H + un_LE + w__ts_cov + un_co2_flux + w__co2_cov)
model3 = lm(formula3, cpa_dataf)
summary(model3)
anova(model3)
formula4 = formula(co2_flux ~ LE + rand_err_LE + un_LE + w__ts_cov + un_co2_flux + w__co2_cov)
model4 = lm(formula4, cpa_dataf)
summary(model4)
anova(model4)
formula5 = formula(co2_flux ~ LE + rand_err_LE + un_LE + w__ts_cov + un_co2_flux)
model5 = lm(formula5, cpa_dataf)
summary(model5)
anova(model5)
