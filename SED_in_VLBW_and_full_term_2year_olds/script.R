setwd("C:/Users/Taavi/Projects/triinus-studies-scripts/SED_in_VLBW_and_full_term_2year_olds")

valjasta.tulemus <- function(tabel, pealkiri, kirjuta.faili = TRUE) {
	# sellesse alamkausta salvestatakse kõik tabelid (.csv failid).
	tulemuste_alamkaust <- "results_08.08.2017/"
	dir.create(file.path(getwd(), tulemuste_alamkaust), showWarnings = FALSE)

	print(pealkiri)
	print(tabel)
	
	if (kirjuta.faili) {
		write.csv2(tabel, paste(tulemuste_alamkaust, paste(pealkiri, ".csv", sep=""), sep=""))
	}
}

# Andmete lugemine spetsiaalselt ettevalmistatud CSV-st (Excelist otse lugemine väidetavalt ei tööta 64bit windowsis):
t <- read.delim("sample.csv", fileEncoding = "UTF-8")

# Neid läheb ehk vaja mõnede sõltumatute muutujate teisendamisel
triinu.sotsem.andmed.asendaNaNulliga <- function(x) { if(is.na(x)) 0 else x }
triinu.sotsem.andmed.asendaNullNaga <- function(x) { if(x == 0) NA else x }

# Parandame veergu "lasteaeda vanus päevades". Seal on muidu nullid nendel lastel kes ei käigi lasteaias, aga peaks olema NA. Muidu lineaarregressioon loeb
# nulli normaalseks väärtuseks, st. et nullindast päevast juba läks lasteaeda.
t$lasteaeda_vanus_paevadest <- as.numeric(lapply(t$lasteaeda_vanus_paevades, triinu.sotsem.andmed.asendaNullNaga))

# Defineerime mõningad siinses uurimuses kasutatavad andmete alamhulgad
# Kliiniline grupp
t_kli <- subset(t, kliiniline == 1)
# kontrollgrupp
t_norm <- subset(t, kliiniline == 0)

# Kliinilisest grupist kõrge NDIga
t_kl_ja_ndi_1 <- subset(t, (kliiniline == 1 & ndi_bayley >= 2))
# Kliinilisest grupist madala NDIga
t_kl_ja_ndi_0 <- subset(t, (kliiniline == 1 & ndi_bayley <= 1))

# Kliinilisest grupist need, kes said rinnapiima haiglast kojuminekul
t_kl_ja_rp_koju_1 <- subset(t, (kliiniline == 1 & rp_haiglast_kojuminekul == 1))
# Kliinilisest grupist need, kes ei saanud rinnapiima haiglast kojuminekul
t_kl_ja_rp_koju_0 <- subset(t, (kliiniline == 1 & rp_haiglast_kojuminekul == 0))

# See teeb lineaarregressiooni, väljastades midagi artiklitoorikus oleva tabel 3 sarnast. 
# -- muutujaid on vähemalt esialgu palju rohkem kui toorikus
triinu.sotsem.lineaarregressioon = function(uuritavad, andmed, multivar=FALSE) {

	# Toorikfunktsioon erinevate lineaarsete mudelite uurimiseks.
	muudel <- function(data, x, parempool){
	     "%+%" <- function(x,y) paste(x,y,sep="")
	     valem <- x %+% "~" %+% parempool 
	     valem <- as.formula(valem)
	     summary(lm(valem, data=data))
	}

	# See teeb koefitsentide tabeli rea(d)
	# Parameeter eeldatakse olevat summary.lm tüüpi
	koefitendid_reana <- function(x) {
		coefs <- coef(x)
		# kasutame alates teisest reast, jätame intercepti välja
		as.data.frame(coefs)[2:NROW(coefs),]
	}

	# See teeb significancy codes, sarnaselt summary.lm-ile.
	signifcodes <- function(x) {
		if (x < 0.001) "***"
		else if (x < 0.01) "**"
		else if (x < 0.05) "*"
		else if (x < 0.1) "."
		else ""
	}

	x <- NULL
	if (multivar) {
		print("Meil on tegemist multivariate uuringuga.")
		parempool <- paste(uuritavad, collapse=" + ")
		x <- coef(muudel(andmed, "sotsiaal.em", parempool))
		# Kasutame alates teisest reast, jättes intercepti välja
		x <- as.data.frame(x)[2:NROW(x),]
	} else {
		print("Meil on tegemist univariate uuringuga.")
		lapplytav_muudel <- function(parempool) { muudel(andmed, "sotsiaal.em", parempool) }
		# summary.lm-ide massiiv
		x <- lapply(uuritavad, lapplytav_muudel)
		x <- do.call(rbind,lapply(x, koefitendid_reana))
	}

	# Teeme tärnid:
	tarnid <- sapply(as.numeric(t(x[4])),signifcodes) 

	# Formaadime tulemust pisut
	x[1] <- lapply(x[1], formatC, format="f", digits=2, mode="double")
	x[2] <- format(x[2], scientific=FALSE, digits=1, nsmall=1, trim=TRUE)
	x[3] <- format(x[3], scientific=FALSE, digits=1, nsmall=1, trim=TRUE)
	x[4] <- lapply(x[4], formatC, format="f", digits=3, mode="double")
	

	# lisame tärnid (significancy codes) ka juurde
	x[4] <- paste(t(x[4]), tarnid, sep="")
	
	x
	
}

# Leiab antud muutuja nimele vastava ilusa nime.
leia_vastav_nimi <- function(muutuja_nimi) {

	# See peaks sisaldama kõiki muutujaid, mida üldse mõnes siinses lineaarregressioonis kasutatakse.
	vastavused <- t(data.frame(
		c("factor(as.numeric(ema_harid_mittek6rg == 0))", "Mother's Higher Education"),
		c("factor(ema_harid_mittek6rg)", "Mother's Education Below Higher"),
		c("factor(keel_mitteeesti)", "Non-Estonian Mother"),
		c("factor(as.numeric(keel_mitteeesti == 0))", "Estonian Mother"),
		c("factor(kliiniline)", "VLGA Infant"),
		c("factor(mitmik)", "Multiple Births"),
		c("factor(sugu_mees)", "Male Gender"),
		c("factor(as.numeric(synnikaal < 1000))", "Birthweight <1000 g"),
		c("factor(SGA_in_10th)", "Small for Gestational Age"),
		c("GV", "Gestational Age"),
		c("factor(as.numeric(GV < 27))", "Gestational Age <27 weeks"),
		c("factor(as.numeric(mitmes_laps == 1))", "First-Born Child, Birth Order =1"),
		c("factor(as.numeric(mitmes_laps > 2))", "Birth Order >2"),
		c("factor(as.numeric(mitmes_laps > 3))", "Birth Order >3"),
		c("mitmes_laps", "Birth Order"),
		c("factor(as.numeric(laste_arv == 1))", "No. of Children =1"),
		c("factor(as.numeric(laste_arv > 2))", "No. of Children >2"),
		c("factor(as.numeric(laste_arv > 3))", "No. of Children >3"),
		c("laste_arv", "No. of Children"),
		c("factor(ema_vanus_alla_25)", "Mother's Age <25 y."),
		c("factor(ema_vanus_alla_21)", "Mother's Age <21 y."),
		c("factor(ema_vanus_yle_40)", "Mother's Age >40 y."),
		c("ema_vanus", "Mother's Age"),
		c("factor(perek_yksik_lastekodu)", "Single Mother"),
		c("factor(elukoht_mitte_tall_tart)", "Home Location not Tallinn, Tartu"),
		c("factor(sissetulek_madal)", "Low-Income Family"),
		c("factor(isa_vanus_alla_25)", "Father's Age <25 y."),
		c("factor(isa_vanus_yle_40)", "Father's Age >40 y."),
		c("isa_vanus", "Father's Age"),
		c("factor(isa_haridus_alla_k6rg)", "Father's Education Below Higher"),
		c("factor(as.numeric(isa_haridus_alla_k6rg == 0))", "Father's Higher Education"),
		c("factor(kaal_alla_10prot)", "Weight <10th Percentile at Discharge"),
		c("factor(Rp_alla_180p)", "Breastfeeding <180 days"),
		c("Rp", "Breastfeeding (days)"),
		c("factor(tserebraalparalyys)", "Cerebral Palsy"),
		c("factor(neonataalne_haigus)", "Neonatal Morbidity"),
		c("lasteaeda_vanus_paevades", "Began Pre-School at Age (Days)"),
		c("factor(as.numeric(lasteaias == 1))", "Attends Pre-School"),
		c("ndi_bayley", "NDI (Neurodevelopmental Disability)"),
		c("factor(as.numeric(ndi_bayley >= 2))", "NDI (Moderate/Severe Neurodevelopmental Disability)"),
		c("sotsiaal.em", "Social-Emotional Composite Score"),
		c("kogn", "Cognitive Composite Score"),
		c("k6ne", "Language Composite Score"),
		c("motoorika", "Motor Composite Score"),
		c("factor(dep09)", "EST-Q Maternal Depression"),
		c("factor(uar09)", "EST-Q Maternal General Anxiety"),
		c("factor(paf09)", "EST-Q Maternal Agoraphobia-Panic"),
		c("factor(sar09)", "EST-Q Maternal Social Anxiety"),
		c("factor(ast09)", "EST-Q Maternal Fatigue"),
		c("factor(ins09)", "EST-Q Maternal Insomnia"),
		c("factor(synnitus_keiser)", "Birth by Caesarian Section"),
		c("voodip_arv106", "Days in Hospital"),
		c("factor(haigestumus_sepsis)", "Neonatal Morbidity - Sepsis"),
		c("factor(rp_haiglast_kojuminekul)", "Breastfeeding Status at Hospital Discharge"),
		c("factor(as.numeric(ravikoht == 1))", "Hospital Location is Tallinn")
	))
	rownames(vastavused) <- NULL
	colnames(vastavused) <- c("var","title")

	for (i in seq_len(nrow(vastavused))) {
	
		# Siin on nüüd selline vigur, et lm paneb koefitsentide nimedele "1" otsa kui tegemist on kategoriaalse tunnusega, millel on väärtuse variandid
		# kas 0 või 1. Ehk siis meie uuringus kõik need, mis algavad "factor(..."-iga. Arvestame seda ka muutujate nimede otsimisel, et neid asendada 
		# viisakamate nimedega.
		muutuja_nimi_1 <- vastavused[i,1]
		if (grepl("^factor", muutuja_nimi_1)[1]) {
			muutuja_nimi_2 <- paste(muutuja_nimi_1,"1",sep="")
			if (muutuja_nimi == muutuja_nimi_2) {
				return(vastavused[i,2])
			}
		}
		if (muutuja_nimi == muutuja_nimi_1) {
			return(vastavused[i,2])
		}
	}
	return("")
}

# Asendab antud tabelis (mis eeldab olevat kujul nagu triinu.sotsem.lineaarregressioon seda väljastab) muutujate nimed sellistega, nagu need võiksid artiklisse minna.
# Ehk siis teeb ilusamad nimed.
triinu.sotsem.ridadele_ilusad_nimed <- function(tabel) {

	x <- rownames(tabel)
	
	for (j in seq_len(length(x))) {
		x[j] <- leia_vastav_nimi(x[j])
	}

	rownames(tabel) <- x
	tabel
	
}

# ===========================================================================
# Univariate analüüs ainult kliinilise grupi kohta
# ===========================================================================

# Sõltumatud muutujad, mida uurime univariate uuringus üle kliinilise grupi:
uuritavad <- c(
	"factor(ema_harid_mittek6rg)", 
	"factor(keel_mitteeesti)",
	"factor(mitmik)",
	"factor(sugu_mees)",
	"factor(as.numeric(synnikaal < 1000))",
	"factor(SGA_in_10th)",
	"GV",
	"factor(as.numeric(GV < 27))",
	"factor(as.numeric(mitmes_laps == 1))",
	"factor(as.numeric(mitmes_laps > 2))",
	"factor(as.numeric(mitmes_laps > 3))",
	"factor(as.numeric(laste_arv == 1))",
	"factor(as.numeric(laste_arv > 2))",
	"factor(as.numeric(laste_arv > 3))",
	"factor(ema_vanus_alla_25)",
	"factor(ema_vanus_alla_21)",
	"factor(ema_vanus_yle_40)",
	"factor(perek_yksik_lastekodu)",
	"factor(sissetulek_madal)",
	"factor(isa_vanus_alla_25)",
	"factor(isa_vanus_yle_40)",
	"factor(isa_haridus_alla_k6rg)",
	"factor(kaal_alla_10prot)",
	"factor(Rp_alla_180p)",
	"factor(tserebraalparalyys)",
	"lasteaeda_vanus_paevades",
	"factor(neonataalne_haigus)",
	"factor(as.numeric(lasteaias == 1))",
	"ndi_bayley",
	"kogn",
	"k6ne",
	"motoorika",
	"factor(dep09)",
	"factor(uar09)",
	"factor(paf09)",
	"factor(sar09)",
	"factor(ast09)",
	"factor(ins09)",
	"factor(synnitus_keiser)",
	"voodip_arv106",
	"factor(haigestumus_sepsis)",
	"factor(rp_haiglast_kojuminekul)"
)

triinu.sotsem.kliinilinegrupp.univar <- triinu.sotsem.lineaarregressioon(uuritavad, subset(t, kliiniline == 1))
triinu.sotsem.kliinilinegrupp.univar <- triinu.sotsem.ridadele_ilusad_nimed(triinu.sotsem.kliinilinegrupp.univar)
valjasta.tulemus(triinu.sotsem.kliinilinegrupp.univar, "triinu.sotsem.kliinilinegrupp.univar", kirjuta.faili = FALSE)

# ===============================================================================================
# Multivariante analüüs kliinilise grupi kohta
# ===============================================================================================

uuritavad <- c(
	"factor(ema_harid_mittek6rg)", 
	"factor(mitmik)",
	"factor(ema_vanus_alla_25)",
	"factor(perek_yksik_lastekodu)",
	"factor(isa_haridus_alla_k6rg)",
	"factor(kaal_alla_10prot)",
	"factor(Rp_alla_180p)",
	"factor(tserebraalparalyys)",
	"factor(neonataalne_haigus)",
	"ndi_bayley",
	#"kogn",
	#"k6ne",
	#"motoorika",
	"factor(dep09)",
	"factor(uar09)",
	"factor(ast09)",
	"factor(haigestumus_sepsis)",
	"factor(rp_haiglast_kojuminekul)"
)
triinu.sotsem.kliinilinegrupp.multivar.vahetulemused <- triinu.sotsem.lineaarregressioon(uuritavad, subset(t, kliiniline == 1), multivar=TRUE)
triinu.sotsem.kliinilinegrupp.multivar.vahetulemused <- triinu.sotsem.ridadele_ilusad_nimed(triinu.sotsem.kliinilinegrupp.multivar.vahetulemused)
valjasta.tulemus(triinu.sotsem.kliinilinegrupp.multivar.vahetulemused, "triinu.sotsem.kliinilinegrupp.multivar.vahetulemused", kirjuta.faili = FALSE)

# Huvitav et siin tuleb enamik seoseid tunduvalt tuvevamad kui jätta sisse ka ravikoht. See ravikoht (Tallinn või Tartu) on aga nii kahtlane näitaja, et otsustati igaks juhuks uuringust välja jätta.

# Raund 2:
uuritavad <- c(
	#"factor(Rp_alla_180p)",
	#"factor(tserebraalparalyys)",
	"ndi_bayley",
	"factor(rp_haiglast_kojuminekul)"
)
triinu.sotsem.kliinilinegrupp.multivar <- triinu.sotsem.lineaarregressioon(uuritavad, subset(t, kliiniline == 1), multivar=TRUE)
triinu.sotsem.kliinilinegrupp.multivar <- triinu.sotsem.ridadele_ilusad_nimed(triinu.sotsem.kliinilinegrupp.multivar)
valjasta.tulemus(triinu.sotsem.kliinilinegrupp.multivar, "triinu.sotsem.kliinilinegrupp.multivar", kirjuta.faili = FALSE)

# ===========================================================================
# Univariate analüüs kontrollgrupi kohta
# ===========================================================================

# Sõltumatud muutujad, mida uurime univariate uuringus üle kliinilise grupi:
# Võrreldes kogugrupiga on eemaldatud järgmised muutujad: 
# - "factor(as.numeric(synnikaal < 1000))",
# - "factor(SGA_in_10th)",
# - "factor(as.numeric(GV < 27))",
# - "factor(tserebraalparalyys)",
# sest need on kõigil kontrollgrupi liikmetel 0-id
uuritavad <- c(
	"factor(ema_harid_mittek6rg)", 
	"factor(keel_mitteeesti)",
	"factor(mitmik)",
	"factor(sugu_mees)",
	"GV",
	"factor(as.numeric(mitmes_laps == 1))",
	"factor(as.numeric(mitmes_laps > 2))",
	"factor(as.numeric(mitmes_laps > 3))",
	"factor(as.numeric(laste_arv == 1))",
	"factor(as.numeric(laste_arv > 2))",
	"factor(as.numeric(laste_arv > 3))",
	"factor(ema_vanus_alla_25)",
	"factor(ema_vanus_alla_21)",
	"factor(ema_vanus_yle_40)",
	"factor(perek_yksik_lastekodu)",
	"factor(sissetulek_madal)",
	"factor(isa_vanus_alla_25)",
	"factor(isa_vanus_yle_40)",
	"factor(isa_haridus_alla_k6rg)",
	"factor(kaal_alla_10prot)",
	"factor(Rp_alla_180p)",
	"lasteaeda_vanus_paevades",
	"factor(as.numeric(lasteaias == 1))",
	"ndi_bayley",
	"kogn",
	"k6ne",
	"motoorika",
	"factor(dep09)",
	"factor(uar09)",
	"factor(paf09)",
	"factor(sar09)",
	"factor(ast09)",
	"factor(ins09)"
)
triinu.sotsem.kontrollgrupp.univar <- triinu.sotsem.lineaarregressioon(uuritavad, subset(t, kliiniline == 0))
triinu.sotsem.kontrollgrupp.univar <- triinu.sotsem.ridadele_ilusad_nimed(triinu.sotsem.kontrollgrupp.univar)
valjasta.tulemus(triinu.sotsem.kontrollgrupp.univar, "triinu.sotsem.kontrollgrupp.univar", kirjuta.faili = FALSE)

# ===============================================================================================
# Multivariante analüüs kontrollgrupi kohta
# ===============================================================================================

uuritavad <- c(
	"factor(ema_harid_mittek6rg)", 
	"factor(mitmik)",
	"factor(as.numeric(mitmes_laps > 2))",
	"factor(as.numeric(laste_arv > 2))",
	"factor(ema_vanus_alla_21)",
	"factor(perek_yksik_lastekodu)",
	"factor(sissetulek_madal)",
	"factor(isa_haridus_alla_k6rg)",
	"kogn",
	"k6ne",
	"motoorika",
	"factor(dep09)",
	"factor(uar09)",
	"factor(ast09)"
)
triinu.sotsem.kontrollgrupp.multivar.vahetulemused <- triinu.sotsem.lineaarregressioon(uuritavad, subset(t, kliiniline == 0), multivar=TRUE)
triinu.sotsem.kontrollgrupp.multivar.vahetulemused <- triinu.sotsem.ridadele_ilusad_nimed(triinu.sotsem.kontrollgrupp.multivar.vahetulemused)
valjasta.tulemus(triinu.sotsem.kontrollgrupp.multivar.vahetulemused, "triinu.sotsem.kontrollgrupp.multivar.vahetulemused", kirjuta.faili = FALSE)

# Raund 2:
uuritavad <- c(
	"factor(ema_harid_mittek6rg)", 
	"factor(perek_yksik_lastekodu)",
	"factor(sissetulek_madal)",
	"kogn",
	"k6ne",
	"factor(ast09)"
)
triinu.sotsem.kontrollgrupp.multivar <- triinu.sotsem.lineaarregressioon(uuritavad, subset(t, kliiniline == 0), multivar=TRUE)
triinu.sotsem.kontrollgrupp.multivar <- triinu.sotsem.ridadele_ilusad_nimed(triinu.sotsem.kontrollgrupp.multivar)
valjasta.tulemus(triinu.sotsem.kontrollgrupp.multivar, "triinu.sotsem.kontrollgrupp.multivar", kirjuta.faili = FALSE)

# Kombineerib kokku kaks antud tabelit kõrvuti kokku üheks tabeliks järgmiselt: 
# 1. Kõigepealt tulevad read, mis leiduvad mõlemas tabelid (rea pealkirja järgi). Teise tabeli veerud tulevad esimese tabeli veergude järel. Kombineeritud tabelite ridadel on ühised pealkirjad.
# 2. Seejärel tulevad read, mis leiduvad ainult tabel1-s (ikka rea pealkirja järgi). Need lisatakse ühiste ridade alla selliselt, et tabel2 osa nendes ridades on tühi (asendatakse tyhja.lahtri-sisu-ga).
# 3. Lõpuks tulevad read, mis leiduvad ainult tabel2-s (ikka rea pealkirja järgi). Need lisatakse eelnevate ridade alla selliselt, et tabel1 osa nendes ridades on tühi (asendatakse tyhja.lahtri-sisu-ga).
kombunni.kaks.tabelit.kokku <- function(tabel1, tabel2, tyhja.lahtri.sisu = "-") {

	tabel <- NULL
	veergude.nimed <- c(colnames(tabel1), colnames(tabel2))
	
	loo.tyhjad.lahtrid <- function(mitu.tykki) {
		tulemus <- NULL
		for (i in seq_len(mitu.tykki)) {
			tulemus <- cbind(tulemus, tyhja.lahtri.sisu)
		}
		tulemus
	}
	
	#Ühised read:
	for (i in seq_len(nrow(tabel1))) {
		for (j in seq_len(nrow(tabel2))) {
			if (rownames(tabel1)[i] == rownames(tabel2)[j]) {
				rida <- cbind(tabel1[i,], tabel2[j,])
				tabel <- rbind(tabel, rida)
			}
		}
	}
	
	#Read, mis on ainult tabelis 1
	tabel.lisa <- NULL
	tyhjad.lahtrid <- loo.tyhjad.lahtrid(ncol(tabel2))
	for (i in seq_len(nrow(tabel1))) {
		leidsin <- FALSE
		for (j in seq_len(nrow(tabel2))) {
			if (rownames(tabel1)[i] == rownames(tabel2)[j]) {
				leidsin <- TRUE
			}
		}
		if (!leidsin) {
			rida <- cbind(tabel1[i,], tyhjad.lahtrid)
			tabel.lisa <- rbind(tabel.lisa, rida)
		}
	}
	if (!is.null(tabel.lisa)) {
		colnames(tabel.lisa) <- veergude.nimed
		tabel <- rbind(tabel, tabel.lisa)
	}
	
	#Read, mis on ainult tabelis 2
	tabel.lisa <- NULL
	tyhjad.lahtrid <- loo.tyhjad.lahtrid(ncol(tabel1))
	for (i in seq_len(nrow(tabel2))) {
		leidsin <- FALSE
		for (j in seq_len(nrow(tabel1))) {
			if (rownames(tabel2)[i] == rownames(tabel1)[j]) {
				leidsin <- TRUE
			}
		}
		if (!leidsin) {
			rida <- cbind(tyhjad.lahtrid, tabel2[i,])
			tabel.lisa <- rbind(tabel.lisa, rida)
		}
	}
	if (!is.null(tabel.lisa)) {
		colnames(tabel.lisa) <- veergude.nimed
		tabel <- rbind(tabel, tabel.lisa)
	}
	
	tabel
	
}

# Siin oleks ilus veel, kui saaks tabeli veerge grupeerida ning anda grupile ka pealkirjad nii, et tekiks midagi sellist:
# 			"kliiniline grupp" 				"kontrollgrupp"
# 		"statiskik1"	"statistik2"	"statistik3"	"statistik1"	"statistik2"	"statistik3"
# "näitaja1"	1		2		3		2		2		3
# "näitaja2"	9		8		7		8		8		7
# 
# jne., aga ei leidnud ühtegi head võimalust R-is seda saavutada. Tuleb excelis teha.

# Kombineeritud univariate tabel
triinu.sotsem.univar <- kombunni.kaks.tabelit.kokku(triinu.sotsem.kliinilinegrupp.univar, triinu.sotsem.kontrollgrupp.univar)
valjasta.tulemus(triinu.sotsem.univar, "triinu.sotsem.univar")

# Kombineeritud multivariate tabel
triinu.sotsem.multivar <- kombunni.kaks.tabelit.kokku(triinu.sotsem.kliinilinegrupp.multivar, triinu.sotsem.kontrollgrupp.multivar)
valjasta.tulemus(triinu.sotsem.multivar, "triinu.sotsem.multivar")

# Sellega on meil lineaarregressiooni osa selleks korraks lõppenud. Homme ratsutame jälle!

# ===========================================================================
# Järgnevalt teeme kategoriaalsete tunnuste osa, ehk siis lihtsalt kirjeldame valimit.
# ===========================================================================

# Mõned abifunktsioonid:
veeru.pealkiri.koos.n.ga = function(pealkiri, n) { paste( paste(pealkiri, n,sep=" (n="), ")", sep="") }
naita.na.teksti <- function(x) { if(x != 0) paste(", N/A=", x, sep="") else "" }
	
# # Tahaks teha nii:
# kirjeldatavad = c(
#	"factor(ema_harid_mittek6rg)",
#	"factor(keel_mitteeesti)"
# )
#
# evalq(as.expression(kirjeldatavad[1]), subset(t, kliiniline == 1))
# evalq(as.expression(kirjeldatavad[1]), subset(t, kliiniline == 0))
# 
# # ja edasi lasta ühe lühikese funktsiooniga üle kogu "kirjeldatavad" massiivi. Aga mitte ei saa aru miks see töötab:
#
# evalq(factor(ema_harid_mittek6rg), t)
# 
# # see aga mitte :
#
# evalq(as.expression("factor(ema_harid_mittek6rg)"), t)
#
# # Igasugu muid variante ka proovitud saamaks stringist midagi, mida eval õigesti sööks, aga ükski ei mõika.
# # Seega ei jää muud üle (ei oska muud) kui teha järgnev suhteliselt väärakas funktsioon.
# # (Kui ma oma igapäevases tarkvara-arendustöös midagi sarnaselt kirjutaksin, siis ma saaksin koodiülevaatajatelt kõvasti kolakat).

# Teeb kuueveerulise ülevaatliku tabeli antud alamhulga kategoriaalsetest tunnustest.
triinu.sotsem.kategoriaalsed.veerg = function(andmehulk) {
	tabel <- NULL
	
	tee.blokk <- function(andmehulk) {
		# Huvitav et R-is otseselt sellist funktsiooni pole (või ma ei leidnud)...
		protsent <- function(mis,millest) {
			x <- mis * 100 / millest
			xs <- format(x, digits = 1, nsmall = 1, trim = TRUE)
			paste(xs, "%", sep="")
		}

		summ <- t(summary(addNA(andmehulk)))
		prots_0 <- protsent(summ[1],length(andmehulk))
		prots_1 <- protsent(summ[2],length(andmehulk))
		na_s <- triinu.sotsem.andmed.asendaNaNulliga(summ[3])
		prots_NA <- protsent(na_s,length(andmehulk))
		
		rida_0 <- paste( paste(summ[1], prots_0, sep=" ("), ")", sep="")
		rida_1 <- paste( paste(summ[2], prots_1, sep=" ("), ")", sep="")
		rida_NA <- paste( paste(na_s, prots_NA, sep=" ("), ")", sep="")

		rida_1_koos_NAga <- paste( 
			paste( 
				paste(summ[2], prots_1, sep=" ("), 
				naita.na.teksti(na_s), 
				sep=""),
			")", 
			sep=""
		)
		
		rida <- cbind(rida_1_koos_NAga)
		rida
	}
	
	# Nüüd tulevad blokid iga muutuja kohta (mida mul ei õnnestunud eraldi funktsiooniks teha). Vähemalt tegin need selliselt, et kopi-peist vea tekke tõenäosus on võimalikult väike.
	# Siin võivad olla ainult sellised muutujad, millel on väärtuste variandid "0", "1", "NA".
	
	# factor(as.numeric(ema_harid_mittek6rg == 0))
	# ema haridus == k6rg
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(as.numeric(ema_harid_mittek6rg == 0)))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(as.numeric(ema_harid_mittek6rg == 0))"
	tabel <- rbind(tabel, blokk)
	
	# factor(as.numeric(isa_haridus_alla_k6rg == 0))
	# isa haridus == k6rg
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(as.numeric(isa_haridus_alla_k6rg == 0)))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(as.numeric(isa_haridus_alla_k6rg == 0))"
	tabel <- rbind(tabel, blokk)
	
	# factor(as.numeric(keel_mitteeesti == 0))
	# kodune keel == eesti
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(as.numeric(keel_mitteeesti == 0)))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(as.numeric(keel_mitteeesti == 0))"
	tabel <- rbind(tabel, blokk)	

	# factor(mitmik)
	# mitmik
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(mitmik))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(mitmik)"
	tabel <- rbind(tabel, blokk)	
	
	# factor(sugu_mees)
	# sugu == mees
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(sugu_mees))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(sugu_mees)"
	tabel <- rbind(tabel, blokk)	
	
	# factor(as.numeric(synnikaal < 1000))
	# synnikaal < 1000g
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(as.numeric(synnikaal < 1000)))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(as.numeric(synnikaal < 1000))"
	tabel <- rbind(tabel, blokk)	
	
	# factor(SGA_in_10th)
	# small for gestational age
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(SGA_in_10th))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(SGA_in_10th)"
	tabel <- rbind(tabel, blokk)	
	
	# factor(elukoht_mitte_tall_tart)
	# elukoht mitte tallinn tartu
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(elukoht_mitte_tall_tart))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(elukoht_mitte_tall_tart)"
	tabel <- rbind(tabel, blokk)	
	
	# factor(perek_yksik_lastekodu)
	# yksikvanem või lastekodu
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(perek_yksik_lastekodu))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(perek_yksik_lastekodu)"
	tabel <- rbind(tabel, blokk)	

	# factor(sissetulek_madal)
	# sissetulek madal
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(sissetulek_madal))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(sissetulek_madal)"
	tabel <- rbind(tabel, blokk)	

	# factor(kaal_alla_10prot)
	# kaal esimeses 10 protsendis
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(kaal_alla_10prot))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(kaal_alla_10prot)"
	tabel <- rbind(tabel, blokk)	

	# factor(Rp_alla_180p)
	# rinnapiima alla 180 paeva
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(Rp_alla_180p))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(Rp_alla_180p)"
	tabel <- rbind(tabel, blokk)	
	
	# factor(tserebraalparalyys)
	# tserebraalparalyys
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(tserebraalparalyys))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(tserebraalparalyys)"
	tabel <- rbind(tabel, blokk)	
	
	# factor(as.numeric(ndi_bayley >= 2))
	# NDI == 1
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(as.numeric(ndi_bayley >= 2)))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(as.numeric(ndi_bayley >= 2))"
	tabel <- rbind(tabel, blokk)	

	# factor(as.numeric(lasteaias == 1))
	# lasteaias
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(as.numeric(lasteaias == 1)))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(as.numeric(lasteaias == 1))"
	tabel <- rbind(tabel, blokk)	

	# factor(as.numeric(ravikoht == 1))
	# ravikoht == tallinn
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(as.numeric(ravikoht == 1)))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(as.numeric(ravikoht == 1))"
	tabel <- rbind(tabel, blokk)	

	# factor(dep09)
	# ema depressioon teisel aastal
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(dep09))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(dep09)"
	tabel <- rbind(tabel, blokk)	
	
	# factor(uar09)
	# ema depressioon teisel aastal
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(uar09))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(uar09)"
	tabel <- rbind(tabel, blokk)	
	
	# factor(paf09)
	# ema depressioon teisel aastal
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(paf09))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(paf09)"
	tabel <- rbind(tabel, blokk)	
	
	# factor(sar09)
	# ema depressioon teisel aastal
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(sar09))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(sar09)"
	tabel <- rbind(tabel, blokk)	
	
	# factor(ast09)
	# ema depressioon teisel aastal
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(ast09))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(ast09)"
	tabel <- rbind(tabel, blokk)	
	
	# factor(ins09)
	# ema depressioon teisel aastal
	tee.blokk.konkreetne <- function(t_alamhulk) {
		x <- with(t_alamhulk, factor(ins09))
		tee.blokk(x)
	}
	blokk <- tee.blokk.konkreetne(andmehulk)
	rownames(blokk)[1] <- "factor(ins09)"
	tabel <- rbind(tabel, blokk)

	tabel
}

# ============================================================================
# Kategoriaalsete tunnuste kirjeldus kliinilisel grupil ja normgrupil
# ============================================================================
tee.triinu.sotsem.kategoriaalsed.kli_norm <- function() {
	tulemus <- cbind(
		triinu.sotsem.kategoriaalsed.veerg(t_kli),
		triinu.sotsem.kategoriaalsed.veerg(t_norm)
	)
	colnames(tulemus) <- c(
		veeru.pealkiri.koos.n.ga("VLGA", nrow(t_kli)),
		veeru.pealkiri.koos.n.ga("FT", nrow(t_norm))
	)

	tulemus <- triinu.sotsem.ridadele_ilusad_nimed(tulemus)
	tulemus
}
triinu.sotsem.kategoriaalsed.kli_norm <- tee.triinu.sotsem.kategoriaalsed.kli_norm()
valjasta.tulemus(triinu.sotsem.kategoriaalsed.kli_norm, "triinu.sotsem.kategoriaalsed.kli_norm", kirjuta.faili = FALSE)

# ============================================================================
# Kategoriaalsete tunnuste kirjeldus kliinilisest grupist neil, kel NDI = 0 ja kel NDI = 1
# ============================================================================
tee.triinu.sotsem.kategoriaalsed.kli_NDI <- function() {
	tulemus <- cbind(
		triinu.sotsem.kategoriaalsed.veerg(t_kl_ja_ndi_0),
		triinu.sotsem.kategoriaalsed.veerg(t_kl_ja_ndi_1)
	)
	colnames(tulemus) <- c(
		veeru.pealkiri.koos.n.ga("VLGA NDI=0", nrow(t_kl_ja_ndi_0)),
		veeru.pealkiri.koos.n.ga("VLGA NDI=1", nrow(t_kl_ja_ndi_1))
	)

	tulemus <- triinu.sotsem.ridadele_ilusad_nimed(tulemus)
	tulemus
}
triinu.sotsem.kategoriaalsed.kli_NDI <- tee.triinu.sotsem.kategoriaalsed.kli_NDI()
valjasta.tulemus(triinu.sotsem.kategoriaalsed.kli_NDI, "triinu.sotsem.kategoriaalsed.kli_NDI", kirjuta.faili = FALSE)

# ============================================================================
# Kategoriaalsete tunnuste kirjeldus kliinilisest grupist neil, kes said rinnapiima haiglast kojuminekul ja need, kes ei saanud
# ============================================================================
tee.triinu.sotsem.kategoriaalsed.kli_RP <- function() {
	tulemus <- cbind(
		triinu.sotsem.kategoriaalsed.veerg(t_kl_ja_rp_koju_1),
		triinu.sotsem.kategoriaalsed.veerg(t_kl_ja_rp_koju_0)
	)
	colnames(tulemus) <- c(
		veeru.pealkiri.koos.n.ga("VLGA RP=1", nrow(t_kl_ja_rp_koju_1)),
		veeru.pealkiri.koos.n.ga("VLGA RP=0", nrow(t_kl_ja_rp_koju_0))
	)

	tulemus <- triinu.sotsem.ridadele_ilusad_nimed(tulemus)
	tulemus
}
triinu.sotsem.kategoriaalsed.kli_RP <- tee.triinu.sotsem.kategoriaalsed.kli_RP()
valjasta.tulemus(triinu.sotsem.kategoriaalsed.kli_RP, "triinu.sotsem.kategoriaalsed.kli_RP", kirjuta.faili = FALSE)

# ===========================================================================
# Valimi kirjelduse järjestatud muutujate osa.
# ===========================================================================

triinu.sotsem.jarjestatud.veerg <- function(andmehulk) {
	tabel <- NULL
	
	tee.blokk <- function(andmehulk) {

		summ <- t(summary(andmehulk))

		min <- format(summ[1], digits=1, nsmall = 0, trim = TRUE)
		median <- summ[3]
		mean <- format(summ[4], digits = 1, nsmall = 1, trim = TRUE)
		sd <- format(sd(andmehulk, na.rm=TRUE), digits = 2, nsmall = 2, trim = TRUE)
		max <-format(summ[6], digits=1, nsmall = 0, trim = TRUE)
		na <- triinu.sotsem.andmed.asendaNaNulliga(summ[7])
		
		rida_1_koos_NAga <- paste( paste( paste( paste( paste(mean, sd, sep=" ("), min, sep=", "), max, sep=", "), naita.na.teksti(na), sep=""), ")", sep="")
		
		rida <- cbind(rida_1_koos_NAga)
		rida
	}
	
	# Järgnevaid oleks jälle tore asendada ühe massiivi ja ühe funktsiooniga.

	# GV
	# gestatsioonivanus
	blokk <- tee.blokk(with(andmehulk, GV))
	rownames(blokk)[1] <- "GV"
	tabel <- rbind(tabel, blokk)
	
	# mitmes_laps
	# mitmes laps
	blokk <- tee.blokk(with(andmehulk, mitmes_laps))
	rownames(blokk)[1] <- "mitmes_laps"
	tabel <- rbind(tabel, blokk)

	# laste_arv
	# laste arv
	blokk <- tee.blokk(with(andmehulk, laste_arv))
	rownames(blokk)[1] <- "laste_arv"
	tabel <- rbind(tabel, blokk)
	
	# ema_vanus
	# ema vanus
	blokk <- tee.blokk(with(andmehulk, ema_vanus))
	rownames(blokk)[1] <- "ema_vanus"
	tabel <- rbind(tabel, blokk)

	# isa_vanus
	# isa vanus
	blokk <- tee.blokk(with(andmehulk, isa_vanus))
	rownames(blokk)[1] <- "isa_vanus"
	tabel <- rbind(tabel, blokk)
	
	# Rp
	# rinnapiimatoidul paevi
	blokk <- tee.blokk(with(andmehulk, Rp))
	rownames(blokk)[1] <- "Rp"
	tabel <- rbind(tabel, blokk)

	# lasteaeda_vanus_paevades
	# vanus lasteada minekul paevades
	blokk <- tee.blokk(with(andmehulk, lasteaeda_vanus_paevades))
	rownames(blokk)[1] <- "lasteaeda_vanus_paevades"
	tabel <- rbind(tabel, blokk)

	# kogn
	# kognitiivne areng
	blokk <- tee.blokk(with(andmehulk, kogn))
	rownames(blokk)[1] <- "kogn"
	tabel <- rbind(tabel, blokk)

	# k6ne
	# k6ne areng
	blokk <- tee.blokk(with(andmehulk, k6ne))
	rownames(blokk)[1] <- "k6ne"
	tabel <- rbind(tabel, blokk)
	
	# motoorika
	blokk <- tee.blokk(with(andmehulk, motoorika))
	rownames(blokk)[1] <- "motoorika"
	tabel <- rbind(tabel, blokk)
	
	# sotsiaal.em
	blokk <- tee.blokk(with(andmehulk, sotsiaal.em))
	rownames(blokk)[1] <- "sotsiaal.em"
	tabel <- rbind(tabel, blokk)

	tabel	
	
}

# ============================================================================
# Järjestatud tunnuste kirjeldus kliinilisel grupil ja normgrupil
# ============================================================================
tee.triinu.sotsem.jarjestatud.kli_norm <- function() {
	tulemus <- cbind(
		triinu.sotsem.jarjestatud.veerg(t_kli),
		triinu.sotsem.jarjestatud.veerg(t_norm)
	)
	colnames(tulemus) <- c(
		veeru.pealkiri.koos.n.ga("VLGA", nrow(t_kli)),
		veeru.pealkiri.koos.n.ga("FT", nrow(t_norm))
	)

	tulemus <- triinu.sotsem.ridadele_ilusad_nimed(tulemus)
	tulemus
}
triinu.sotsem.jarjestatud.kli_norm <- tee.triinu.sotsem.jarjestatud.kli_norm()
valjasta.tulemus(triinu.sotsem.jarjestatud.kli_norm, "triinu.sotsem.jarjestatud.kli_norm", kirjuta.faili = FALSE)

# ============================================================================
# Järjestatus tunnuste kirjeldus kliinilisest grupist neil, kel NDI = 0 ja kel NDI = 1
# ============================================================================
tee.triinu.sotsem.jarjestatud.kli_NDI <- function() {
	tulemus <- cbind(
		triinu.sotsem.jarjestatud.veerg(t_kl_ja_ndi_0),
		triinu.sotsem.jarjestatud.veerg(t_kl_ja_ndi_1)
	)
	colnames(tulemus) <- c(
		veeru.pealkiri.koos.n.ga("VLGA NDI=0", nrow(t_kl_ja_ndi_0)),
		veeru.pealkiri.koos.n.ga("VLGA NDI=1", nrow(t_kl_ja_ndi_1))
	)

	tulemus <- triinu.sotsem.ridadele_ilusad_nimed(tulemus)
	tulemus
}
triinu.sotsem.jarjestatud.kli_NDI <- tee.triinu.sotsem.jarjestatud.kli_NDI()
valjasta.tulemus(triinu.sotsem.jarjestatud.kli_NDI, "triinu.sotsem.jarjestatud.kli_NDI", kirjuta.faili = FALSE)

# ============================================================================
# Järjestatud tunnuste kirjeldus kliinilisest grupist neil, kes said rinnapiima haiglast kojuminekul ja need, kes ei saanud
# ============================================================================
tee.triinu.sotsem.jarjestatud.kli_RP <- function() {
	tulemus <- cbind(
		triinu.sotsem.jarjestatud.veerg(t_kl_ja_rp_koju_1),
		triinu.sotsem.jarjestatud.veerg(t_kl_ja_rp_koju_0)
	)
	colnames(tulemus) <- c(
		veeru.pealkiri.koos.n.ga("VLGA RP=1", nrow(t_kl_ja_rp_koju_1)),
		veeru.pealkiri.koos.n.ga("VLGA RP=0", nrow(t_kl_ja_rp_koju_0))
	)

	tulemus <- triinu.sotsem.ridadele_ilusad_nimed(tulemus)
	tulemus
}
triinu.sotsem.jarjestatud.kli_RP <- tee.triinu.sotsem.jarjestatud.kli_RP()
valjasta.tulemus(triinu.sotsem.jarjestatud.kli_RP, "triinu.sotsem.jarjestatud.kli_RP", kirjuta.faili = FALSE)

# ============================================================================
# Kombineeritud kategoriaalsed ja järjestatud tunnused samadest gruppidest
# ============================================================================
triinu.sotsem.kli_norm <- rbind(triinu.sotsem.kategoriaalsed.kli_norm, triinu.sotsem.jarjestatud.kli_norm)
valjasta.tulemus(triinu.sotsem.kli_norm, "triinu.sotsem.kli_norm")

triinu.sotsem.kli_NDI <- rbind(triinu.sotsem.kategoriaalsed.kli_NDI, triinu.sotsem.jarjestatud.kli_NDI)
valjasta.tulemus(triinu.sotsem.kli_NDI, "triinu.sotsem.kli_NDI")

triinu.sotsem.kli_RP <- rbind(triinu.sotsem.kategoriaalsed.kli_RP, triinu.sotsem.jarjestatud.kli_RP)
valjasta.tulemus(triinu.sotsem.kli_RP, "triinu.sotsem.kli_RP")
