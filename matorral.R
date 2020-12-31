
#unimos los dos dataframes de espectro y codigo
mat_raw <- read_delim("~/Onedrive_Aurelio/OneDrive/Doctorate/scrub_analisys/Nir_inputs/nirs matorral/matorral_1_brutos_limpios.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)
Cod<- read_excel("~/Onedrive_Aurelio/OneDrive/Doctorate/scrub_analisys/Nir_inputs/nirs matorral/Codigos matorral.xlsx")

dtmat<-merge(mat_raw, Cod, by = "cod")

write.csv(dtmat, file = "datos_matorral.csv")
