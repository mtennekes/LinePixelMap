library(cbsodataR)
library(tidyverse)

#cbs_get_catalogs()
#ds = cbs_get_datasets()
#View(ds)
#ds |> filter(Frequency == "Permaand") |> View()
#ds |> filter(Identifier == "80590ned") |> as.list()


dt = cbs_get_data(id = "80590ned")

# https://opendata.cbs.nl/#/CBS/nl/dataset/80590ned/table?dl=38817
# BB Beroepsbevolking
# WZBB Werkzame beroepsbevolking
# WLBB Werkloze beroepsbevolking
# WLP Werkloosheidspercentage
# NBB Niet-beroepsbevolking
# BA Bruto arbeidsparticipatie
# NA Netto arbeidsparticipatie

dt = dt |> 
	filter(Geslacht == "T001038",
		   substr(Perioden, 5,6) == "MM") |> 
	transmute(Leeftijd = Leeftijd,
			  Perioden = Perioden,
			  BB_nsg = NietSeizoengecorrigeerd_1,
			  BB_sg = Seizoengecorrigeerd_2,
			  WZBB_nsg = NietSeizoengecorrigeerd_3,
			  WZBB_sg = Seizoengecorrigeerd_4,
			  WLBB_nsg = NietSeizoengecorrigeerd_5,
			  WLBB_sg = Seizoengecorrigeerd_6,
			  WLP_nsg = NietSeizoengecorrigeerd_7,
			  WLP_sg = Seizoengecorrigeerd_8,
			  NBB_nsg = NietSeizoengecorrigeerd_9,
			  NBB_sg = Seizoengecorrigeerd_10,
			  BA_nsg = NietSeizoengecorrigeerd_11,
			  BA_sg = Seizoengecorrigeerd_12,
			  NA_nsg = NietSeizoengecorrigeerd_13,
			  NA_sg = Seizoengecorrigeerd_14)

# 52052 = 15-75




dt = dt |> 
	filter(Leeftijd == "52052   ") |> 
	mutate(Datum = as.Date(paste0(substr(Perioden, 1,4), "-", substr(Perioden, 7,8), "-01")),
		   Leeftijd = NULL,
		   Perioden = NULL)

dt2 = dt |> 
	pivot_longer(-Datum)

ts = split(dt2, dt2$name)

ts = lapply(ts, function(x) {
	x |> arrange(Datum)
})


dt2 |> 
	group_by(name) |> 
	arrange(Datum) |> 
	


ggplot(dt2, aes(x = Datum, y = value)) +
	geom_line() +
	facet_wrap(~name, scales = "free_y")
	
