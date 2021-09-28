# *-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-**-*
# ++ REPORTES SECTORIALES ++ ----------------------------------------------------
# *-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-**-*
#
#
# Héctor Garrido Henríquez
# Analista Cuantitativo. Observatorio Laboral Ñuble
# Docente. Facultad de Ciencias Empresariales
# Universidad del Bío-Bío
# Avenida Andrés Bello 720, Casilla 447, Chillán
# Teléfono: +56-942353973
# http://www.observatoriolaboralnuble.cl

rm(list = ls())


# Si al momento de actualizar los archivos arroja un error la primera vez,
# ejecutar el siguiente código en la powershell de windows de la carpeta del repositorio:
# git pull nombre_repositorio --allow-unrelated-histories


#   
#   i)	PRESENTACIÓN: 
#   .	de la Casen (¿qué es?, cómo se realiza, periodicidad,  concepto de pobreza e indigencia, medición por ingreso, medición , evolución de indicadores),  
# .	del Hogar de Cristo (surgimiento, organización , vigencia del  P. Hurtado en la organización) 
# 
# ii)	RESULTADOS E IMPLICANCIAS
# .	Resultados de la Casen: evolución de los indicadores, hogares monoparentales
# .	Implicancias para el Hogar de Cristo y la pobreza dura
# 
# iii)	PREGUNTAS DEL PUBLICO (en torno a pobreza, solidaridad, riqueza)
# 

#
Sys.setenv(LANG = "en")


load_pkg <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

devtools::install_github("martinctc/surveytoolbox")

packages = c("tidyverse", "stringi", "lubridate", 
             "data.table", "srvyr", "pbapply", 
             "ggrepel", "RColorBrewer", "readstata13", 
             "gtable", "gridExtra", "tidytext", 
             "wordcloud", "kableExtra", "captioner", 
             "foreign", "RPostgres", "haven", 
             "rJava", "openxlsx", "timetk", 
             "forecast","sweep", "tidyquant", 
             "ggmap", "rgeos", "ggalt", "maptools", 
             "rgdal", "readxl", "grid", "scales", 
             "fuzzyjoin", "survey", "directlabels", "microbenchmark", 
             "haven", "sjlabelled", "labelled", "surveytoolbox", "multcomp", "XLConnect")

load_pkg(packages)


file_path = "G:/Mi unidad/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/Casen_en_Pandemia_2020_SPSS.sav/Casen en Pandemia 2020 SPSS.sav"

path_mat = "G:/Mi unidad/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Estadísticas de Educación/OFICIAL_WEB_PROCESO_MAT_2007_al_2021_29_06_2021/OFICIAL_WEB_PROCESO_MAT_2007_al_2021_29_06_2021.csv"


casen2020 <- haven::read_sav(file_path) 

matricula <- data.table::fread(path_mat)


##############################################################################################
# Análisis de la matricula en carreras de pedagogía ==========================================
##############################################################################################


matricula_pedagogia = matricula %>% 
  filter(`NIVEL GLOBAL` == "Pregrado") %>% 
  group_by(AÑO, `REGIÓN`) %>% 
  mutate(total_pregrado = sum(`TOTAL MATRICULADOS`)) %>% 
  filter(`CINE-F 2013 AREA` == "Educación", 
         str_detect(`ÁREA CARRERA GENÉRICA`, "Pedagogía")) %>% 
  mutate(peda = ifelse(str_detect(`NOMBRE CARRERA`, "PEDAGOGIA"),1,0)) %>%
  group_by(AÑO, `REGIÓN`) %>% 
  mutate(total_peda = sum(`TOTAL MATRICULADOS`)) %>% 
  group_by(`AÑO`, `ÁREA CARRERA GENÉRICA`,`REGIÓN`) %>% 
  summarise(matriculados = sum(`TOTAL MATRICULADOS`), 
            mujeres = sum(`MATRICULADOS MUJERES POR PROGRAMA`, na.rm = TRUE), 
            pct_region = round((matriculados/total_pregrado[1])*100,1), 
            pct_peda = matriculados/total_peda[1]) %>% 
  group_by(`AÑO`, `ÁREA CARRERA GENÉRICA`) %>% 
  mutate(total_carrera = sum(`matriculados`),
         total_mujeres = sum(`mujeres`), 
         pct_pais = round((matriculados/total_carrera)*100,1),
         pct_mujeres = round((mujeres/total_mujeres)*100,1)) %>% 
  mutate(AÑO = gsub("MAT_", "", AÑO))



wb = openxlsx::loadWorkbook(file = "20210927_boletin_profes.xlsx")


addWorksheet(wb, sheetName = "Data Matricula", gridLines = FALSE)

writeData(wb, sheet = "Data Matricula",
          x = matricula_pedagogia,
          startRow = 1, startCol = 1, 
          colNames = TRUE, rowNames = FALSE,
          keepNA = FALSE, 
          withFilter = FALSE)



##############################################################################################
# Análisis de la encuesta de caracterización socioeconómica nacional (CASEN) =================
##############################################################################################


# Generación de un directorio de variables 

directorio_2020 = casen2020 %>% varl_tb()


# Incorporación del diseño muestral complejo de la encuesta 

casen_2020 = casen2020 %>% as_survey_design(ids = varunit, strata = varstrat, weights = expr)


# names(casen2020)[names(casen2020) %in% names(casen_2017)]

cat_oficio = casen2020 %>% extract_vallab("oficio4_08")


# Generación de macrozonas 


##############################################################################################
# Ingreso promedio según sexo y región =======================================================
##############################################################################################


cat_reg = casen2020 %>%  extract_vallab("region")
cat_sex = casen2020 %>% extract_vallab("sexo")
cat_ofi = casen2020 %>% extract_vallab("oficio4_08")

casen_2020 = casen_2020 %>% 
  mutate(macrozona = ifelse(region %in% c(15,1,2), "Norte Grande", 
                     ifelse(region %in% c(3,4), "Norte Chico", 
                     ifelse(region %in% c(5,6,7,16,8), "Zona Central", 
                     ifelse(region %in% c(13), "RM", 
                     ifelse(region %in% c(9,10,14), "Zona Sur", 
                     ifelse(region %in% c(11,12), "Zona Austral", NA)))))), 
         macrozona2 = ifelse(macrozona %in% c("Norte Grande", "Norte Chico"), "Norte", 
                      ifelse(macrozona %in% c("Zona Sur", "Zona Austral"), "Sur", macrozona)))

ing_med_2020 = casen_2020 %>% 
  filter(oficio4_08 %in% c(2330,2340, 2341, 2353,2354,2355)) %>% 
  group_by(region) %>% 
  summarise(ingreso_promedio = survey_mean(ytrabajocor, na.rm= TRUE, vartype = "cv"),
            edad = survey_mean(edad, na.rm = TRUE, vartype = "cv"), 
            frequency_ = unweighted(n())) %>% 
  mutate(quality = ifelse(ingreso_promedio_cv<=0.15 & frequency_>=60,1,0)) %>% 
  rename(id = region) %>% 
  left_join(cat_reg)


addWorksheet(wb, sheetName = "Ingreso promedio", gridLines = FALSE)

writeData(wb, sheet = "Ingreso promedio",
          x = ing_med_2020,
          startRow = 1, startCol = 1, 
          colNames = TRUE, rowNames = FALSE,
          keepNA = FALSE, 
          withFilter = FALSE)

##############################################################################################
# Distribución según grupo ocupacional =======================================================
##############################################################################################


dist_2020 = casen_2020 %>% 
  mutate(oficio4_08 = ifelse(oficio4_08 %in% c(2353,2354,2355), 2350, oficio4_08)) %>% 
  filter(oficio4_08 %in% c(2330,2341, 2350)) %>% 
  group_by(macrozona2, oficio4_08) %>% 
  summarise(prop = survey_mean(na.rm= TRUE, vartype = "se"),
            frec = unweighted(n())) %>%
  ungroup() %>% 
  mutate(max_se = ifelse(prop>=0.5,((1-prop)^(2/3))/9,((prop)^(2/3))/9)) %>% 
  rename(id = oficio4_08) %>% 
  left_join(cat_ofi) %>% 
  mutate(quality = ifelse(frec>=60 & prop_se<max_se,1,0))


addWorksheet(wb, sheetName = "Tipo de profesor", gridLines = FALSE)

writeData(wb, sheet = "Tipo de profesor",
          x = dist_2020,
          startRow = 1, startCol = 1, 
          colNames = TRUE, rowNames = FALSE,
          keepNA = FALSE, 
          withFilter = FALSE)

##############################################################################################
# Distribución según sexo =======================================================
##############################################################################################


dist_sex_2020 = casen_2020 %>% 
  mutate(oficio4_08 = ifelse(oficio4_08 %in% c(2353,2354,2355), 2350, oficio4_08)) %>% 
  filter(oficio4_08 %in% c(2330,2341, 2350)) %>% 
  group_by(macrozona2, sexo) %>% 
  summarise(prop = survey_mean(na.rm= TRUE, vartype = "se"), 
            frec = unweighted(n())) %>% 
  ungroup() %>% 
  mutate(max_se = ifelse(prop>=0.5,((1-prop)^(2/3))/9,((prop)^(2/3))/9), 
         quality = ifelse(frec>=60 & prop_se<max_se,1,0))

addWorksheet(wb, sheetName = "Distribución por sexo", gridLines = FALSE)

writeData(wb, sheet = "Distribución por sexo",
          x = dist_sex_2020,
          startRow = 1, startCol = 1, 
          colNames = TRUE, rowNames = FALSE,
          keepNA = FALSE, 
          withFilter = FALSE)


##############################################################################################
# Distribución según informalidad =======================================================
##############################################################################################

cat_inf = casen2020 %>% extract_vallab("ocup_inf")

dist_inf_2020 = casen_2020 %>% 
  mutate(oficio4_08 = ifelse(oficio4_08 %in% c(2353,2354,2355), 2350, oficio4_08)) %>% 
  filter(oficio4_08 %in% c(2330,2341, 2350), 
         ocup_inf!=9) %>% 
  group_by(macrozona2, ocup_inf) %>% 
  summarise(prop = survey_mean(na.rm= TRUE, vartype = "se"), 
            frec = unweighted(n())) %>% 
  ungroup() %>% 
  mutate(max_se = ifelse(prop>=0.5,((1-prop)^(2/3))/9,((prop)^(2/3))/9), 
         quality = ifelse(frec>=60 & prop_se<max_se,1,0))

addWorksheet(wb, sheetName = "Distribución por informalidad", gridLines = FALSE)

writeData(wb, sheet = "Distribución por informalidad",
          x = dist_inf_2020,
          startRow = 1, startCol = 1, 
          colNames = TRUE, rowNames = FALSE,
          keepNA = FALSE, 
          withFilter = FALSE)


##############################################################################################
# Distribución según migración =======================================================
##############################################################################################

cat_inf = casen2020 %>% extract_vallab("ocup_inf")

dist_migr_2020 = casen_2020 %>% 
  mutate(oficio4_08 = ifelse(oficio4_08 %in% c(2353,2354,2355), 2350, oficio4_08)) %>% 
  filter(oficio4_08 %in% c(2330,2341, 2350), 
         ocup_inf!=9) %>% 
  mutate(migrante = ifelse(r2 %in% c(1,2), "No migrante", 
                    ifelse(r2 %in% c(3,4), "Migrante", NA))) %>% 
  group_by(macrozona2, migrante, .drop = TRUE) %>% 
  summarise(prop = survey_mean(na.rm= TRUE, vartype = "se"), 
            frec = unweighted(n())) %>% 
  ungroup() %>% 
  mutate(max_se = ifelse(prop>=0.5,((1-prop)^(2/3))/9,((prop)^(2/3))/9), 
         quality = ifelse(frec>=60 & prop_se<max_se,1,0)) %>% 
  filter(!is.na(migrante))

addWorksheet(wb, sheetName = "Distribución migrante", gridLines = FALSE)

writeData(wb, sheet = "Distribución migrante",
          x = dist_migr_2020,
          startRow = 1, startCol = 1, 
          colNames = TRUE, rowNames = FALSE,
          keepNA = FALSE, 
          withFilter = FALSE)


##############################################################################################
# Distribución según tramo etario =======================================================
##############################################################################################


dist_edad_2020 = casen_2020 %>% 
  filter(oficio4_08 %in% c(2320,2330,2340, 2341, 2353,2354,2355)) %>% 
  mutate(tramo = ifelse(edad %in% 15:29, "15 a 29 años",
                 ifelse(edad %in% 30:44, "30 a 44 años", 
                 ifelse(edad %in% 45:59, "45 a 59 años", 
                 ifelse(edad>=60, "Más de 60", NA))))) %>% 
  group_by(macrozona2, tramo) %>% 
  summarise(prop = survey_mean(na.rm= TRUE, vartype = "se"), 
            frec = unweighted(n())) %>% 
  ungroup() %>% 
  mutate(max_se = ifelse(prop>=0.5,((1-prop)^(2/3))/9,((prop)^(2/3))/9), 
         quality = ifelse(frec>=60 & prop_se<max_se,1,0))



addWorksheet(wb, sheetName = "Distribución por edad", gridLines = FALSE)

writeData(wb, sheet = "Distribución por edad",
          x = dist_edad_2020,
          startRow = 1, startCol = 1, 
          colNames = TRUE, rowNames = FALSE,
          keepNA = FALSE, 
          withFilter = FALSE)

##############################################################################################
# Total de profesores por región =======================================================
##############################################################################################


total_profes_2020 = casen_2020 %>% 
  filter(oficio4_08 %in% c(2330,2340, 2341, 2353,2354,2355)) %>% 
  group_by(region) %>% 
  summarise(survey_total(na.rm = TRUE))


# modelo = svyglm(ytrabajocor~region+sexo+sexo*region, design = casen_2017 %>% 
#                   mutate(sexo = factor(sexo), region = factor(region)))
# 
# anova = anova(modelo)
#   
# post_hoc = summary(glht(modelo, linfct = mcp(region="Tukey")))


# datas_ = Filter( function(x) 'data.frame' %in% class( get(x) ), ls() )
# 
# lapply(datas_, function(x) class(get(x)))

##############################################################################################
# Ingreso mediano según región 
##############################################################################################

cat_reg = casen2020 %>%  extract_vallab("region")

ing_med_ = casen2017 %>% select(varstrat, varunit, expr, region, ytrabajocor) %>% mutate(year = 2017) %>% 
  rbind(casen2020 %>% select(varstrat, varunit, expr, region, ytrabajocor) %>% mutate(year = 2020)) %>% 
  as_survey_design(ids = varunit, strata = varstrat, weights = expr) %>% 
  group_by(year, region) %>% 
  summarise(ingreso_mediano = survey_median(ytrabajocor, na.rm= TRUE, vartype = "se")) %>% 
  rename(id = region) %>% 
  left_join(cat_reg) %>% 
  reshape2::dcast(region~year, value.var = "ingreso_mediano")


##############################################################################################
# Ingreso del trabajo según decil de ingreso autónomo ======================================= 
##############################################################################################

cat_reg = casen2020 %>%  extract_vallab("region")

ing_med_dec = casen2017 %>% select(varstrat, varunit, expr, region, dautr, ytrabajocorh, pco1) %>% mutate(year = 2017) %>% 
  rbind(casen2020 %>% select(varstrat, varunit, expr, region, dautr, ytrabajocorh, pco1) %>% mutate(year = 2020)) %>% 
  as_survey_design(ids = varunit, strata = varstrat, weights = expr) %>% 
  group_by(year, region, dautr, .drop = TRUE) %>% 
  filter(pco1 == 1, region == 16) %>% 
  summarise(ingreso_mediano = survey_total(ytrabajocorh, na.rm= TRUE, vartype = "cv")) 

ing_med_dec %>% 
  group_by(year) %>% 
  mutate(ind_10_10 = )


##############################################################################################
# Ingreso mediano según sexo y región 
##############################################################################################

cat_reg = casen2020 %>%  extract_vallab("region")
cat_sex = casen2020 %>% extract_vallab("sexo")

ing_med_2017 = casen_2017 %>% 
  group_by(region, sexo) %>% 
  summarise(ingreso_mediano = survey_median(ytrabajocor, na.rm= TRUE, vartype = "se"))

ing_med_2017 = ing_med_2017 %>%
  rename(id = region) %>%
  left_join(cat_reg) %>% 
  rename(cod_reg = id) %>% 
  rename(id = sexo) %>% 
  left_join(cat_sex)

ing_med_2020 = casen_2020 %>% 
  group_by(region, sexo) %>% 
  summarise(ingreso_mediano = survey_median(ytrabajocor, na.rm= TRUE, vartype = "se"))

ing_med_2020 = ing_med_2020 %>%
  rename(id = region) %>%
  left_join(cat_reg) %>% 
  rename(cod_reg = id) %>% 
  rename(id = sexo) %>% 
  left_join(cat_sex)


##############################################################################################
# Ingreso mediano según región 
##############################################################################################

cat_reg = casen2020 %>%  extract_vallab("region")

ing_med_ = casen2017 %>% select(varstrat, varunit, expr, region, ytrabajocor) %>% mutate(year = 2017) %>% 
  rbind(casen2020 %>% select(varstrat, varunit, expr, region, ytrabajocor) %>% mutate(year = 2020)) %>% 
  as_survey_design(ids = varunit, strata = varstrat, weights = expr) %>% 
  group_by(year, region) %>% 
  summarise(ingreso_mediano = survey_median(ytrabajocor, na.rm= TRUE, vartype = "se")) %>% 
  rename(id = region) %>% 
  left_join(cat_reg) %>% 
  reshape2::dcast(region~year, value.var = "ingreso_mediano")


##############################################################################################
# Ingreso del trabajo según decil de ingreso autónomo del hogar ======================================= 
##############################################################################################

cat_daut_2017 = casen2017 %>%  extract_vallab("dautr")

ing_trab_2017 = casen_2017 %>% 
  filter((region %in% c(16)) &  pco1 == 1) %>% 
  group_by(dautr, .drop = TRUE) %>% 
  mutate(dautr = as.character(dautr)) %>% 
  summarise(ing_trabajo = survey_mean(ypchtrabajo, na.rm= TRUE, vartype = "cv"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = dautr) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_daut_2017) %>% 
  mutate(calidad = ifelse(n_>=60 & ing_trabajo_cv<=.2 & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  add_row(id = NA, ing_trabajo = NA, ing_trabajo_cv = NA, n_ = sum(.$n_), gl = NA, dautr = NA, calidad = mean(.$calidad))



cat_daut_2020 = casen2020 %>%  extract_vallab("dautr")

ing_trab_2020 = casen_2020 %>% 
  filter(region %in% c(16) & pco1 == 1) %>% 
  group_by(dautr, .drop = TRUE) %>% 
  mutate(dautr = as.character(dautr)) %>% 
  summarise(ing_trabajo = survey_mean(ypchtrabcor, na.rm= TRUE, vartype = "cv")*1.086, 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = dautr) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_daut_2020) %>% 
  mutate(calidad = ifelse(n_>=60 & ing_trabajo_cv<=.2 & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  add_row(id = NA, ing_trabajo = NA, ing_trabajo_cv = NA, n_ = sum(.$n_), gl = NA, dautr = NA, calidad = mean(.$calidad))


##############################################################################################
# Subsidios ingreso autónomo del hogar ======================================= 
##############################################################################################

cat_daut_2017 = casen2017 %>%  extract_vallab("dautr")

subsi_2017 = casen_2017 %>% 
  filter((region %in% c(16)) &  pco1 == 1) %>% 
  group_by(dautr, .drop = TRUE) %>% 
  mutate(dautr = as.character(dautr), 
         ypc_ysubh = round(ysubh/numper)) %>% 
  summarise(subsidio = survey_mean(ypc_ysubh, na.rm= TRUE, vartype = "cv"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = dautr) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_daut_2017) %>% 
  mutate(calidad = ifelse(n_>=60 & subsidio_cv<=.2 & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  add_row(id = NA, subsidio = NA, subsidio_cv = NA, n_ = sum(.$n_), gl = NA, dautr = NA, calidad = mean(.$calidad))



cat_daut_2020 = casen2020 %>%  extract_vallab("dautr")

subsi_2020 = casen_2020 %>% 
  filter((region %in% c(16)) &  pco1 == 1) %>% 
  group_by(dautr, .drop = TRUE) %>% 
  mutate(dautr = as.character(dautr), 
         ypc_ysubh = round(ysubh/numper)) %>% 
  summarise(subsidio = survey_mean(ypc_ysubh, na.rm= TRUE, vartype = "cv"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = dautr) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_daut_2020) %>% 
  mutate(calidad = ifelse(n_>=60 & subsidio_cv<=.2 & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  add_row(id = NA, subsidio = NA, subsidio_cv = NA, n_ = sum(.$n_), gl = NA, dautr = NA, calidad = mean(.$calidad))


##############################################################################################
# Razones de inactividad ======================================= 
##############################################################################################

# Encuesta CASEN 2017 
#
# o7r1     o7r1. ¿Cuál es la razón o razones...? razón 1          dbl+lbl  
#       [1] Consiguió trabajo que empezará pronto o iniciará ~
#       [2] Está esperando resultado de gestiones ya emprendi~
#       [3] No tiene con quien dejar a los niños              
#       [4] No tiene con quien dejar a adultos mayores        
#       [5] No tiene con quien dejar a otro familiar          
#       [6] Está enfermo o tiene una discapacidad             
#       [7] Piensa que nadie le dará trabajo (porque no cuent~
#       [8] Las reglas, horarios y distancias de los trabajos~
#       [9] Ofrecen sueldos muy bajos                         
#       [10] Quehaceres del hogar                             
#       [11] Estudiante                                       
#       [12] Jubilado(a), pensionado(a) o montepiado(a)       
#       [13] Tiene otra fuente de ingreso (seguro de cesantía~
#       [14] Se cansó de buscar o cree que no hay trabajo dis~
#       [15] Busca cuando realmente lo necesita o tiene traba~
#       [16] No tiene interés en trabajar                     
#       [17] Otra razón

# Encuesta CASEN 2017 
# 
# # o7       o7. ¿Cuál es la razón principal para no buscar trabajo~ dbl+lbl  
# #       [1] Consiguió trabajo que empezará pronto o iniciará p~
#         [2] Está esperando resultado de gestiones ya emprendid~
#         [3] No tiene con quien dejar los niños
#         [4] No tiene con quien dejar a adultos mayores
#         [5] No tiene con quien a otro familiar
#         [6] Está enfermo o tiene una discapacidad
#         [7] Piensa que nadie le dará trabajo porque no cuenta ~
#         [8] Las reglas, horarios y distancias de los trabajos ~
#         [9] Ofrecen sueldos muy bajos
#         [10] Quehaceres del hogar
#         [11] Estudiante
#         [12] Jubilado(a), montepiado(a) o pensionado(a)
#         [13] Tiene otra fuente de ingreso (Seguro de Cesantía,~
#         [14] Por temor a contagiarse de COVID-19
#         [15] Se cansó de buscar
#         [16] Cree que no hay trabajo disponible
#         [17] Busca cuando realmente lo necesita o tiene trabaj~
#         [18] No tiene interés en trabajar
#         [19] Otra razón. Especifique



cat_reg_2017 = casen2017 %>%  extract_vallab("o7r1")

raz_inac_2017 = casen_2017 %>% 
  filter(region %in% c(5,6,7,8,16)) %>% 
  group_by(o7r1, .drop = TRUE) %>% 
  mutate(o7r1 = as.character(o7r1)) %>% 
  summarise(razones = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = o7r1) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_reg_2017) %>% 
  mutate(se_max = ifelse(razones<0.5, (razones^(2/3))/9, ((1-razones)^(2/3))/9),
         calidad = ifelse(n_>=60 & razones_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  rename(o7 = o7r1)

# %>% 
#   add_row(id = NA, razones = NA, razones_se = NA, n_ = sum(.$n_), o7r1 = "Calidad", gl = NA, se_max = NA, calidad = mean(.$calidad)) %>% 
#   rename(o7 = o7r1)


cat_reg_2020 = casen2020 %>%  extract_vallab("o7")

raz_inac_2020 = casen_2020 %>% 
  filter(region %in% c(5,6,7,16,8)) %>% 
  group_by(o7, .drop = TRUE) %>% 
  mutate(o7 = as.character(o7)) %>% 
  summarise(razones = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = o7) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_reg_2020) %>% 
  mutate(se_max = ifelse(razones<0.5, (razones^(2/3))/9, ((1-razones)^(2/3))/9),
         calidad = ifelse(n_>=60 & razones_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id)) 

# %>% 
#   ungroup %>% 
#   add_row(id = NA, razones = NA, razones_se = NA, n_ = sum(.$n_), o7 = "Calidad", calidad = mean(.$calidad))

qraz_inac_2017 = mean(raz_inac_2017$calidad)
qraz_inac_2017 = mean(raz_inac_2020$calidad)

raz_inac = raz_inac_2017 %>% mutate(year = 2017) %>% 
  bind_rows(raz_inac_2020 %>% mutate(year = 2020)) %>% 
  dplyr::select(id, year, o7, everything()) %>% 
  reshape2::dcast(o7~year, value.var = )



##############################################################################################
# Pobreza por tipo de hogar ======================================= 
##############################################################################################


pobreza_fam_2017  = casen_2017 %>% 
  filter(region == 16) %>% 
  mutate(aux = ifelse(pco2==1 & nucleo!=0, 1,0)) %>% 
  group_by(folio) %>% 
  mutate(nnucleos = sum(aux)) %>% 
  mutate(auxi = ifelse(pco1 %in% c(2,3),1,0),
         conyuge = max(auxi), 
         pobreza = ifelse(pobreza %in% c(1,2), "pobre", "no pobre")) %>% 
  mutate(thogar = ifelse(numper==1, "Unipersonal", 
                         ifelse(numper>1 & conyuge == 0, "monoparental", 
                                ifelse(numper>1 & conyuge == 1, "biparental", 
                                       ifelse(numper!=1 & nnucleos == numper, "Sin nucleo", NA))))) %>% 
  group_by(thogar, pobreza) %>% 
  filter(pco1 == 1) %>% 
  summarise(pob = survey_total(na.rm = TRUE, vartype = "cv"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  mutate(se_max = ifelse(pob<0.5, (pob^(2/3))/9, ((1-pob)^(2/3))/9),
         calidad = ifelse(n_>=60 & pob_cv<=.15 & gl>=9,1,0)) %>% 
  filter(pobreza == "pobre")


pobreza_fam_2020  = casen_2020 %>% 
  filter(region == 16) %>% 
  mutate(aux = ifelse(pco2==1 & nucleo!=0, 1,0)) %>% 
  group_by(folio) %>% 
  mutate(nnucleos = sum(aux)) %>% 
  mutate(auxi = ifelse(pco1 %in% c(2,3),1,0),
         conyuge = max(auxi), 
         pobreza = ifelse(pobreza %in% c(1,2), "pobre", "no pobre")) %>% 
  mutate(thogar = ifelse(numper==1, "Unipersonal", 
                         ifelse(numper>1 & conyuge == 0, "monoparental", 
                                ifelse(numper>1 & conyuge == 1, "biparental", 
                                       ifelse(numper!=1 & nnucleos == numper, "Sin nucleo", NA))))) %>% 
  group_by(thogar, pobreza) %>% 
  filter(pco1 == 1) %>% 
  summarise(pob = survey_total(na.rm = TRUE, vartype = "cv"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  mutate(se_max = ifelse(pob<0.5, (pob^(2/3))/9, ((1-pob)^(2/3))/9),
         calidad = ifelse(n_>=60 & pob_cv<=.15 & gl>=9,1,0)) %>% 
  filter(pobreza == "pobre")


pobreza_fam_2020_2  = casen_2020 %>% 
  filter(region == 16) %>% 
  mutate(aux = ifelse(pco2==1 & nucleo!=0, 1,0)) %>% 
  group_by(folio) %>% 
  mutate(nnucleos = sum(aux)) %>% 
  mutate(auxi = ifelse(pco1 %in% c(2,3),1,0),
         conyuge = max(auxi), 
         pobreza_sinte = ifelse(pobreza_sinte %in% c(1,2), "pobre", "no pobre")) %>% 
  mutate(thogar = ifelse(numper==1, "Unipersonal", 
                         ifelse(numper>1 & conyuge == 0, "monoparental", 
                                ifelse(numper>1 & conyuge == 1, "biparental", 
                                       ifelse(numper!=1 & nnucleos == numper, "Sin nucleo", NA))))) %>% 
  group_by(thogar, pobreza_sinte) %>% 
  filter(pco1 == 1) %>% 
  summarise(pob = survey_total(na.rm = TRUE, vartype = "cv"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  mutate(se_max = ifelse(pob<0.5, (pob^(2/3))/9, ((1-pob)^(2/3))/9),
         calidad = ifelse(n_>=60 & pob_cv<=.15 & gl>=9,1,0)) %>% 
  filter(pobreza_sinte == "pobre")




##############################################################################################
# Tipos de hogar ======================================= 
##############################################################################################


tipo_fam_2017  = casen_2017 %>% 
  filter(region == 16) %>% 
  mutate(aux = ifelse(pco2==1 & nucleo!=0, 1,0)) %>% 
  group_by(folio) %>% 
  mutate(nnucleos = sum(aux)) %>% 
  mutate(auxi = ifelse(pco1 %in% c(2,3),1,0),
         conyuge = max(auxi), 
         pobreza = ifelse(pobreza %in% c(1,2), "pobre", "no pobre")) %>% 
  mutate(thogar = ifelse(numper==1, "Unipersonal", 
                         ifelse(numper>1 & conyuge == 0, "monoparental", 
                                ifelse(numper>1 & conyuge == 1, "biparental", 
                                       ifelse(numper!=1 & nnucleos == numper, "Sin nucleo", NA))))) %>% 
  group_by(thogar) %>% 
  filter(pco1 == 1) %>% 
  summarise(pob = survey_total(na.rm = TRUE, vartype = "cv"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  mutate(se_max = ifelse(pob<0.5, (pob^(2/3))/9, ((1-pob)^(2/3))/9),
         calidad = ifelse(n_>=60 & pob_cv<=.15 & gl>=9,1,0)) 


tipo_fam_2020  = casen_2020 %>% 
  filter(region == 16) %>% 
  mutate(aux = ifelse(pco2==1 & nucleo!=0, 1,0)) %>% 
  group_by(folio) %>% 
  mutate(nnucleos = sum(aux)) %>% 
  mutate(auxi = ifelse(pco1 %in% c(2,3),1,0),
         conyuge = max(auxi), 
         pobreza = ifelse(pobreza %in% c(1,2), "pobre", "no pobre")) %>% 
  mutate(thogar = ifelse(numper==1, "Unipersonal", 
                         ifelse(numper>1 & conyuge == 0, "monoparental", 
                                ifelse(numper>1 & conyuge == 1, "biparental", 
                                       ifelse(numper!=1 & nnucleos == numper, "Sin nucleo", NA))))) %>% 
  group_by(thogar) %>% 
  filter(pco1 == 1) %>% 
  summarise(pob = survey_total(na.rm = TRUE, vartype = "cv"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  mutate(se_max = ifelse(pob<0.5, (pob^(2/3))/9, ((1-pob)^(2/3))/9),
         calidad = ifelse(n_>=60 & pob_cv<=.15 & gl>=9,1,0)) 





##############################################################################################
# Tipos de hogar por sexo======================================= 
##############################################################################################


muj_fam_2017  = casen_2017 %>% 
  filter(region == 16) %>% 
  mutate(aux = ifelse(pco2==1 & nucleo!=0, 1,0)) %>% 
  group_by(folio) %>% 
  mutate(nnucleos = sum(aux)) %>% 
  mutate(auxi = ifelse(pco1 %in% c(2,3),1,0),
         conyuge = max(auxi), 
         pobreza = ifelse(pobreza %in% c(1,2), "pobre", "no pobre")) %>% 
  mutate(thogar = ifelse(numper==1, "Unipersonal", 
                         ifelse(numper>1 & conyuge == 0, "monoparental", 
                                ifelse(numper>1 & conyuge == 1, "biparental", 
                                       ifelse(numper!=1 & nnucleos == numper, "Sin nucleo", NA))))) %>% 
  group_by(thogar) %>% 
  filter(pco1 == 1 & sexo == 2) %>% 
  summarise(pob = survey_total(na.rm = TRUE, vartype = "cv"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  mutate(se_max = ifelse(pob<0.5, (pob^(2/3))/9, ((1-pob)^(2/3))/9),
         calidad = ifelse(n_>=60 & pob_cv<=.15 & gl>=9,1,0)) 


muj_fam_2020  = casen_2020 %>% 
  filter(region == 16) %>% 
  mutate(aux = ifelse(pco2==1 & nucleo!=0, 1,0)) %>% 
  group_by(folio) %>% 
  mutate(nnucleos = sum(aux)) %>% 
  mutate(auxi = ifelse(pco1 %in% c(2,3),1,0),
         conyuge = max(auxi), 
         pobreza = ifelse(pobreza %in% c(1,2), "pobre", "no pobre")) %>% 
  mutate(thogar = ifelse(numper==1, "Unipersonal", 
                         ifelse(numper>1 & conyuge == 0, "monoparental", 
                                ifelse(numper>1 & conyuge == 1, "biparental", 
                                       ifelse(numper!=1 & nnucleos == numper, "Sin nucleo", NA))))) %>% 
  group_by(thogar) %>% 
  filter(pco1 == 1 & sexo == 2) %>% 
  summarise(pob = survey_total(na.rm = TRUE, vartype = "cv"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  mutate(se_max = ifelse(pob<0.5, (pob^(2/3))/9, ((1-pob)^(2/3))/9),
         calidad = ifelse(n_>=60 & pob_cv<=.15 & gl>=9,1,0)) 



##############################################################################################
# oficio grandes grupos ======================================= 
##############################################################################################


cat_ofi_2017 = casen2017 %>%  extract_vallab("oficio1")

oficios_2017 = casen_2017 %>% 
  filter(region %in% c(16), !is.na(oficio1), oficio1!= 999) %>% 
  group_by(oficio1, .drop = TRUE) %>% 
  mutate(oficio1 = as.character(oficio1)) %>% 
  summarise(oficios = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = oficio1) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_ofi_2017) %>% 
  mutate(se_max = ifelse(oficios<0.5, (oficios^(2/3))/9, ((1-oficios)^(2/3))/9),
         calidad = ifelse(n_>=60 & oficios_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  add_row(id = NA, oficios = NA, oficios_se = NA, n_ = sum(.$n_), gl = NA, oficio1 = NA, se_max = NA, calidad = mean(.$calidad))


cat_ofi_2020 = casen2020 %>%  extract_vallab("oficio1_88")

oficios_2020 = casen_2020 %>% 
  filter(region %in% c(16), !is.na(oficio1_88), !(oficio1_88 %in% c(98,99))) %>% 
  group_by(oficio1_88, .drop = TRUE) %>% 
  mutate(oficio1_88 = as.character(oficio1_08)) %>% 
  summarise(oficios = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = oficio1_88) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_ofi_2020) %>% 
  mutate(se_max = ifelse(oficios<0.5, (oficios^(2/3))/9, ((1-oficios)^(2/3))/9),
         calidad = ifelse(n_>=60 & oficios_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  add_row(id = NA, oficios = NA, oficios_se = NA, n_ = sum(.$n_), gl = NA, oficio1_88 = NA, se_max = NA, calidad = mean(.$calidad))



##############################################################################################
# Sector económico ======================================= 
##############################################################################################


cat_ocupa_2017 = casen2017 %>%  extract_vallab("o15")

cat_ocup_2017 = casen_2017 %>% 
  filter(region %in% c(16), !is.na(o15)) %>% 
  group_by(o15, .drop = TRUE) %>% 
  mutate(o15 = as.character(o15)) %>% 
  summarise(categoria = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = o15) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_ocupa_2017) %>% 
  mutate(se_max = ifelse(categoria<0.5, (categoria^(2/3))/9, ((1-categoria)^(2/3))/9),
         calidad = ifelse(n_>=60 & categoria_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  add_row(id = NA, categoria = NA, categoria_se = NA, n_ = sum(.$n_), gl = NA, o15 = NA, se_max = NA, calidad = mean(.$calidad))


cat_ocupa_2020 = casen2020 %>%  extract_vallab("o15")

cat_ocup_2020 = casen_2020 %>% 
  filter(region %in% c(16), !is.na(o15)) %>% 
  group_by(o15, .drop = TRUE) %>% 
  mutate(o15 = as.character(o15)) %>% 
  summarise(categoria = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = o15) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_ocupa_2020) %>% 
  mutate(se_max = ifelse(categoria<0.5, (categoria^(2/3))/9, ((1-categoria)^(2/3))/9),
         calidad = ifelse(n_>=60 & categoria_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  add_row(id = NA, categoria = NA, categoria_se = NA, n_ = sum(.$n_), gl = NA, o15 = NA, se_max = NA, calidad = mean(.$calidad))



##############################################################################################
# Condición de actividad ======================================= 
##############################################################################################


cat_cae_2017 = casen2017 %>%  extract_vallab("activ")

cat_caes_2017 = casen_2017 %>% 
  filter(region %in% c(16), !is.na(activ)) %>% 
  group_by(activ, .drop = TRUE) %>% 
  mutate(activ = as.character(activ)) %>% 
  summarise(cae = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = activ) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_cae_2017) %>% 
  mutate(se_max = ifelse(cae<0.5, (cae^(2/3))/9, ((1-cae)^(2/3))/9),
         calidad = ifelse(n_>=60 & cae_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  add_row(id = NA, cae = NA, cae_se = NA, n_ = sum(.$n_), gl = NA, activ = NA, se_max = NA, calidad = mean(.$calidad))


cat_cae_2020 = casen2020 %>%  extract_vallab("activ")

cat_caes_2020 = casen_2020 %>% 
  filter(region %in% c(16), !is.na(activ)) %>% 
  group_by(activ, .drop = TRUE) %>% 
  mutate(activ = as.character(activ)) %>% 
  summarise(cae = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = activ) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_cae_2020) %>% 
  mutate(se_max = ifelse(cae<0.5, (cae^(2/3))/9, ((1-cae)^(2/3))/9),
         calidad = ifelse(n_>=60 & cae_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  add_row(id = NA, cae = NA, cae_se = NA, n_ = sum(.$n_), gl = NA, activ = NA, se_max = NA, calidad = mean(.$calidad))


##############################################################################################
# Sector económico ======================================= 
##############################################################################################


cat_rama_2017 = casen2017 %>%  extract_vallab("rama1")

cat_ramas_2017 = casen_2017 %>% 
  filter(region %in% c(16), !is.na(rama1), rama1!= 999) %>% 
  group_by(rama1, .drop = TRUE) %>% 
  mutate(rama1 = as.character(rama1)) %>% 
  summarise(rama = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = rama1) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_rama_2017) %>% 
  mutate(se_max = ifelse(rama<0.5, (rama^(2/3))/9, ((1-rama)^(2/3))/9),
         calidad = ifelse(n_>=60 & rama_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  add_row(id = NA, rama = NA, rama_se = NA, n_ = sum(.$n_), gl = NA, rama1 = NA, se_max = NA, calidad = mean(.$calidad))


cat_rama_2020 = casen2020 %>%  extract_vallab("rama1_rev3")

cat_ramas_2020 = casen_2020 %>% 
  filter(region %in% c(16), !is.na(rama1_rev3), !(rama1_rev3 %in% c(88,99))) %>% 
  group_by(rama1_rev3, .drop = TRUE) %>% 
  mutate(rama1_rev3 = as.character(rama1_rev3)) %>% 
  summarise(rama = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = rama1_rev3) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_rama_2020) %>% 
  mutate(se_max = ifelse(rama<0.5, (rama^(2/3))/9, ((1-rama)^(2/3))/9),
         calidad = ifelse(n_>=60 & rama_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id)) %>% 
  ungroup %>% 
  add_row(id = NA, rama = NA, rama_se = NA, n_ = sum(.$n_), gl = NA, rama1_rev3 = NA, se_max = NA, calidad = mean(.$calidad))




##############################################################################################
# oficios detallado ======================================= 
##############################################################################################


cat_ofi4_2017 = casen2017 %>%  extract_vallab("oficio4")

oficio4_2017 = casen_2017 %>% 
  filter(region %in% c(5,6,7,8,16), !is.na(oficio4), oficio4!= 999) %>% 
  group_by(oficio4, .drop = TRUE) %>% 
  mutate(oficio4 = as.character(oficio4)) %>% 
  summarise(oficios = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = oficio4) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_ofi4_2017) %>% 
  mutate(se_max = ifelse(oficios<0.5, (oficios^(2/3))/9, ((1-oficios)^(2/3))/9),
         calidad = ifelse(n_>=60 & oficios_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id), calidad == 1) %>% 
  ungroup %>% 
  add_row(id = NA, oficios = NA, oficios_se = NA, n_ = sum(.$n_), gl = NA, oficio4 = NA, se_max = NA, calidad = mean(.$calidad))


cat_ofi4_2020 = casen2020 %>%  extract_vallab("oficio4_88")

oficio4_88_2020 = casen_2020 %>% 
  filter(region %in% c(5,6,7,8,16), !is.na(oficio4_88), oficio4_88!= 999) %>% 
  group_by(oficio4_88, .drop = TRUE) %>% 
  mutate(oficio4_88 = as.character(oficio4_88)) %>% 
  summarise(oficios = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = oficio4_88) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_ofi4_2020) %>% 
  mutate(se_max = ifelse(oficios<0.5, (oficios^(2/3))/9, ((1-oficios)^(2/3))/9),
         calidad = ifelse(n_>=60 & oficios_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id), calidad == 1) %>% 
  ungroup %>% 
  add_row(id = NA, oficios = NA, oficios_se = NA, n_ = sum(.$n_), gl = NA, oficio4_88 = NA, se_max = NA, calidad = mean(.$calidad))


oficio4 = oficio4_2017 %>% dplyr::select(oficio4, id, oficios) %>% rename(oficio4_88 = oficio4) %>% 
  left_join(oficio4_88_2020 %>% dplyr::select(oficio4_88, id, oficios) %>% rename(oficio2020 = oficios), by = "id")



##############################################################################################
# Actividad económica detallado ======================================= 
##############################################################################################


cat_ram4_2017 = casen2017 %>%  extract_vallab("rama4")

rama4_2017 = casen_2017 %>% 
  filter(region %in% c(5,6,7,8,16), !is.na(rama4), rama4!= 999) %>% 
  group_by(rama4, .drop = TRUE) %>% 
  mutate(rama4 = as.character(rama4)) %>% 
  summarise(ramas = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = rama4) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_ram4_2017) %>% 
  mutate(se_max = ifelse(ramas<0.5, (ramas^(2/3))/9, ((1-ramas)^(2/3))/9),
         calidad = ifelse(n_>=60 & ramas_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id), calidad == 1) %>% 
  ungroup %>% 
  add_row(id = NA, ramas = NA, ramas_se = NA, n_ = sum(.$n_), gl = NA, rama4 = NA, se_max = NA, calidad = mean(.$calidad))


cat_ram4_2020 = casen2020 %>%  extract_vallab("rama4_rev3")

rama4_rev3_2020 = casen_2020 %>% 
  filter(region %in% c(5,6,7,8,16), !is.na(rama4_rev3), rama4_rev3!= 999) %>% 
  group_by(rama4_rev3, .drop = TRUE) %>% 
  mutate(rama4_rev3 = as.character(rama4_rev3)) %>% 
  summarise(ramas = survey_mean(na.rm= TRUE, vartype = "se"), 
            n_ = unweighted(n()), 
            gl = n_distinct(varunit)-n_distinct(varstrat)) %>% 
  rename(id = rama4_rev3) %>% 
  mutate(id = as.double(id)) %>% 
  left_join(cat_ram4_2020) %>% 
  mutate(se_max = ifelse(ramas<0.5, (ramas^(2/3))/9, ((1-ramas)^(2/3))/9),
         calidad = ifelse(n_>=60 & ramas_se<=se_max & gl>=9,1,0)) %>% 
  filter(!is.na(id), calidad == 1) %>% 
  ungroup %>% 
  add_row(id = NA, ramas = NA, ramas_se = NA, n_ = sum(.$n_), gl = NA, rama4_rev3 = NA, se_max = NA, calidad = mean(.$calidad))


rama4 = rama4_2017 %>% dplyr::select(id, ramas, rama4) %>% rename(rama4_rev3 = rama4) %>% 
  left_join(oficio4_88_2020 %>% dplyr::select(oficio4_88, id, oficios) %>% rename(oficio2020 = oficios), by = "id")




# **** Envío a Planillas Excel =============================================

# wb = createWorkbook() 


if (("Índice" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "Índice")
  addWorksheet(wb, sheetName = "Índice", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "Índice", gridLines = FALSE)
}

setColWidths(wb, sheet = "Índice", cols = c(2,3), widths = 30)

#n_ = dim(indicadores)[1]

# if (("raz_inac" %in% sheets(wb)) == TRUE){
#   removeWorksheet(wb, sheet = "raz_inac")
#   addWorksheet(wb, sheetName = "raz_inac", gridLines = FALSE)
# } else {
#   addWorksheet(wb, sheetName = "raz_inac", gridLines = FALSE)
# }


# addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
# setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  


writeData(wb, sheet = "raz_inac",
          x = as.data.frame(raz_inac),
          startRow = 2, startCol = 2)





if (("oficios" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "oficios")
  addWorksheet(wb, sheetName = "oficios", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "oficios", gridLines = FALSE)
}


# addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
# setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  


writeData(wb, sheet = "oficios",
          x = as.data.frame(oficios_2017),
          startRow = 2, startCol = 2)

writeData(wb, sheet = "oficios",
          x = as.data.frame(oficios_2020),
          startRow = 16, startCol = 2)




if (("categorias" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "categorias")
  addWorksheet(wb, sheetName = "categorias", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "categorias", gridLines = FALSE)
}


# addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
# setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  


writeData(wb, sheet = "categorias",
          x = as.data.frame(cat_ocup_2017),
          startRow = 2, startCol = 2)

writeData(wb, sheet = "categorias",
          x = as.data.frame(cat_ocup_2020),
          startRow = 14, startCol = 2)



if (("caes" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "caes")
  addWorksheet(wb, sheetName = "caes", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "caes", gridLines = FALSE)
}

# addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
# setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  


writeData(wb, sheet = "caes",
          x = as.data.frame(cat_caes_2017),
          startRow = 2, startCol = 2)

writeData(wb, sheet = "caes",
          x = as.data.frame(cat_caes_2020),
          startRow = 14, startCol = 2)




if (("ramas" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "ramas")
  addWorksheet(wb, sheetName = "ramas", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "ramas", gridLines = FALSE)
}

# addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
# setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  


writeData(wb, sheet = "ramas",
          x = as.data.frame(cat_ramas_2017),
          startRow = 2, startCol = 2)

writeData(wb, sheet = "ramas",
          x = as.data.frame(cat_ramas_2020),
          startRow = 20, startCol = 2)




if (("ramas" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "ramas")
  addWorksheet(wb, sheetName = "ramas", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "ramas", gridLines = FALSE)
}

# addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
# setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  


writeData(wb, sheet = "ramas",
          x = as.data.frame(cat_ramas_2017),
          startRow = 2, startCol = 2)

writeData(wb, sheet = "ramas",
          x = as.data.frame(cat_ramas_2020),
          startRow = 20, startCol = 2)




if (("subsi" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "subsi")
  addWorksheet(wb, sheetName = "subsi", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "subsi", gridLines = FALSE)
}

# addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
# setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  


writeData(wb, sheet = "subsi",
          x = as.data.frame(subsi_2017),
          startRow = 2, startCol = 2)

writeData(wb, sheet = "subsi",
          x = as.data.frame(subsi_2020),
          startRow = 13, startCol = 2)



if (("pobreza_fam" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "pobreza_fam")
  addWorksheet(wb, sheetName = "pobreza_fam", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "pobreza_fam", gridLines = FALSE)
}

# addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
# setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  


writeData(wb, sheet = "pobreza_fam",
          x = as.data.frame(pobreza_fam_2017),
          startRow = 2, startCol = 2)

writeData(wb, sheet = "pobreza_fam",
          x = as.data.frame(pobreza_fam_2020),
          startRow = 7, startCol = 2)


writeData(wb, sheet = "pobreza_fam",
          x = as.data.frame(pobreza_fam_2020_2),
          startRow = 11, startCol = 2)



if (("tipo_fam" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "tipo_fam")
  addWorksheet(wb, sheetName = "tipo_fam", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "tipo_fam", gridLines = FALSE)
}

# addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
# setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  


writeData(wb, sheet = "tipo_fam",
          x = as.data.frame(tipo_fam_2017),
          startRow = 2, startCol = 2)

writeData(wb, sheet = "tipo_fam",
          x = as.data.frame(tipo_fam_2020),
          startRow = 9, startCol = 2)




if (("muj_fam" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "muj_fam")
  addWorksheet(wb, sheetName = "muj_fam", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "muj_fam", gridLines = FALSE)
}

# addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
# setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  


writeData(wb, sheet = "muj_fam",
          x = as.data.frame(muj_fam_2017),
          startRow = 2, startCol = 2)

writeData(wb, sheet = "muj_fam",
          x = as.data.frame(muj_fam_2020),
          startRow = 8, startCol = 2)





openXL(wb)


saveWorkbook(wb, file = paste0("C:/Users/omen03/Documents/presentaciones/20210811_encuesta_casen.xlsx"), overwrite = TRUE)





if (("ingresos" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "ingresos")
  addWorksheet(wb, sheetName = "ingresos", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "ingresos", gridLines = FALSE)
}

# addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
# setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  


writeData(wb, sheet = "ingresos",
          x = as.data.frame(ing_trab_2017),
          startRow = 2, startCol = 2)

writeData(wb, sheet = "ingresos",
          x = as.data.frame(ing_trab_2020),
          startRow = 20, startCol = 2)

if (("oficios_88" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "oficios_88")
  addWorksheet(wb, sheetName = "oficios_88", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "oficios_88", gridLines = FALSE)
}

# addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
# setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  


writeData(wb, sheet = "oficios_88",
          x = as.data.frame(oficio4),
          startRow = 2, startCol = 2)



openXL(wb)






if (("cuadro 2" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "cuadro 2")
  addWorksheet(wb, sheetName = "cuadro 2", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "cuadro 2", gridLines = FALSE)
}

writeData(wb, sheet = "cuadro 2",
          x = as.data.frame(ing_med_),
          startRow = 2, startCol = 2)




if (("cuadro 3" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "cuadro 3")
  addWorksheet(wb, sheetName = "cuadro 3", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "cuadro 3", gridLines = FALSE)
}

writeData(wb, sheet = "cuadro 3",
          x = as.data.frame(ing_med_dec),
          startRow = 2, startCol = 2)


# writeData(wb, sheet = "indicadores",
#           x = indicadores,
#           startRow = 3, startCol = 2, 
#           colNames = TRUE, rowNames = FALSE,
#           keepNA = FALSE, 
#           withFilter = FALSE)
# writeData(wb, sheet = "indicadores",
#           x = "Fuente: Observatorio Laboral Nacional",
#           startRow = n_+4, startCol = 2)
# writeFormula(wb, sheet ="indicadores", startRow = 1, startCol = 1
#              , x = makeHyperlinkString(sheet = "Índice",
#                                        row = 1, col = 1, 
#                                        text = "Índice"))


saveWorkbook(wb, file = paste0(path, "/20210717_coyuntura_ene.xlsx"), overwrite = TRUE)





mutate(mujer = ifelse(sexo == 2,1,0)) %>% 
  summarise(
    ocupados = survey_total(ocupado, vartype = "cv"), 
    pet = survey_total(edad_activ, vartype = "cv"),
    n_estratos = unweighted(n_distinct(estrato)),
    n_conglomerados = unweighted(n_distinct(id_directorio)), 
    n_ocup = unweighted(sum(ocupado)), 
    n_pet = unweighted(sum(edad_activ)), 
    prop_media = survey_mean(media_completa, proportion = TRUE, na.rm = TRUE, vartype = "se"), 
    prop_inform = survey_mean(ocup_inform, proportion = TRUE, na.rm = TRUE, vartype = "se"), 
    prop_mujer = survey_mean(mujer, proportion = TRUE, na.rm = TRUE, vartype = "se"), 
    prop_edad = survey_mean(edad_obj, proportion = TRUE, na.rm = TRUE, vartype = "se"),
    n_media = unweighted(sum(media_completa)),
    n_inform = unweighted(sum(ocup_inform)),
    n_mujer = unweighted(sum(mujer)),
    n_edad = unweighted(sum(edad_obj))
  )











file_ =  "C:/Users/omen03/Google Drive/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/ENADEL_2019/ENADEL_2019.dta"


enadel = read_dta(file_)