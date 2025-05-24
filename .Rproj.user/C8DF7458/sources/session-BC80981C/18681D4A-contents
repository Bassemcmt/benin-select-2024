library(readxl)
library(dplyr)
library (plotly)
library(rsconnect)
data <- read_excel("data/Declarations_SYDONIA_2024.xlsx", 
                   col_types = c("numeric", "text", "text", 
                                 "date", "date", "date", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "numeric", "text", "numeric", "numeric", 
                                 "numeric", "numeric", "text", "numeric", 
                                 "text", "text", "text", "text", "text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "text", "numeric", "text"))
#library(readxl)
#View(Copy_of_De_clarations_enregistre_es_en_2023_Extrait_le_18_04_2024_11h57)View(data)
data$DATE_ENREGISTREMENT <- as.Date(data$DATE_ENREGISTREMENT)
Droits_compromis <- read_excel("data/CONTRE_ECRITURES_2023_2024.xlsx")
Droits_compromis$NUMDEC <- paste0(Droits_compromis$ANNEE,"-",Droits_compromis$BUREAU_D,"-",substr(Droits_compromis$NUM_DECL, 1, 1), "-", substr(Droits_compromis$NUM_DECL, 3, 8))
Droits_compromis<-filter(Droits_compromis,Droits_compromis$ANNEE=='2024')
AVD <- read_excel("data/Bc_2024.xlsx")
AVD<- select(AVD,IFU_IMPORTATEUR,Ref_dec,Diff_Taxes)
Droits_compromis_dec<- select(Droits_compromis,NUMDEC,DC) %>% group_by(NUMDEC) %>% summarise(tot_DC=sum(DC))

#Droits_compromis<-left_join(Droits_compromis,Droits_compromis_dec,by=c('NUMDEC'))
data <- left_join(data, Droits_compromis_dec, by = c( 'REFERENCE_DECLARATION'='NUMDEC'))
data <- left_join(data, AVD, by = c( 'REFERENCE_DECLARATION'='Ref_dec'))
data <- data %>%
  mutate(
    # DC = replace(DC, is.na(DC), 0),
    `tot_DC` = replace(`tot_DC`, is.na(`tot_DC`), 0),
    Diff_Taxes = replace(Diff_Taxes, is.na(Diff_Taxes), 0)
  )
data$SH2 <- substr(data$NOMENCLATURE, 1, 2)
data$SH4 <- substr(data$NOMENCLATURE, 1, 4)
data$SH6 <- substr(data$NOMENCLATURE, 1, 6)
data$DEC[is.na(data$Diff_Taxes)] <- 0
data$DEC<-ifelse(data$CONTRE_ÉCRITURE=='OUI','Non conforme','Conforme')
#data$AVD<- ifelse (data$Diff_Taxes==0.0, "Conforme",data$Diff_Taxes)
data$AVD<- ifelse (data$Diff_Taxes>10000, "Non conforme","Conforme")
data$DCO<-ifelse(data$tot_DC>0,'Non Conforme','Conforme')
# Remplacer les valeurs vides par 0 dans les colonnes DEC, AVD et DCO
# Pour DEC
data$DEC <- ifelse(is.na(data$DEC), "Conforme",data$DEC)

# Pour AVD
data$AVD <- ifelse(is.na(data$AVD), "Conforme",data$AVD)

# Pour DCO
data$DCO <- ifelse(is.na(data$DCO), "Conforme",data$DCO)
# Appliquer la logique de conformité
data$Conformité <- ifelse((data$DEC == 'Conforme' & data$AVD == 'Conforme' & data$DCO == 'Conforme'), "Conforme", "Non Conforme")
data$Conformité <- ifelse((is.na(data$DEC) & is.na(data$AVD) & is.na(data$DCO)) | (data$DEC == 'Conforme' & data$AVD == 'Conforme' & data$DCO == 'Conforme'), "Conforme", "Non Conforme")
df<-data



lst_operateur <- df %>%
  select(CODE_IMPORTATEUR, NOM_IMPORTATEUR) %>%
  group_by(CODE_IMPORTATEUR) %>%
  slice(n())

# Specify the filename for the Excel file (in the current working directory)
df_XLSX <- "filename.xlsx"

# Write the dataframe to an Excel file
write_xlsx(df, df_XLSX)

