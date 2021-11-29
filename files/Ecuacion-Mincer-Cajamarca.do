use "C:\Users\rosme\Google Drive\PUCP\7 ° CICLO\Econometria\Trabajo grupal\Base de datos\enaho01-2018-200.dta"
global main "C:\Users\rosme\Google Drive\PUCP\7 ° CICLO\Econometria\Trabajo grupal"

*==================
*LIMPIEZA DE DATOS
*==================
keep conglome vivienda hogar codperso ubigeo dominio estrato p203 p207 p208a p209 facpob07
merge 1:1 conglome vivienda hogar codperso using "C:\Users\rosme\Google Drive\PUCP\7 ° CICLO\Econometria\Trabajo grupal\Base de datos\enaho01a-2018-300.dta" , keepus(p301a p301b p301c p301d)

drop if _merge==1
drop _merge

merge 1:1 conglome vivienda hogar codperso using "C:\Users\rosme\Google Drive\PUCP\7 ° CICLO\Econometria\Trabajo grupal\Base de datos\enaho01a-2018-500.dta", keepus(i513t i518 i524e1 i530 i538e1 i541a)

*=====================================
*BASE DE DATOS CAJARMARCA (UBIGEO=06)
*=====================================
gen departamento = substr(ubigeo,1,2)
destring departamento, gen(dep)
keep if dep==6             // 4326 observaciones

*============================================
*CONSTRUCCION DE VARIABLES PARA LA REGRESION
*============================================

*VARIABLE SEXO
recode p207 (1=1) (2=0), gen(sexo) 
label define lab_sexo 1 "Hombre" 0 "Mujer"
label values sexo lab_sexo

*VARIABLE REGIÓN
recode estrato (1/5=1) (6/8=0), gen(region)  
label define lab_region 1 "Urbano" 0 "Rural"
label values region lab_region

*VARIABLE EDUCACION 
recode p301a (1/4 = 0) (5/6 = 6) (7/10 = 11) (11 = 16) (.=.), gen(nivelprevio)
egen suma=rowtotal (p301b p301c)
gen educacion = suma + nivelprevio

*VARIABLE EXPERIENCIA
rename p208a edad
drop if edad<=13  //Solo consideramos PEA : edad >= 14  --> 3388 observaciones
gen experiencia = edad - educacion 

*VARIABLE SALARIO
egen horas=rowtotal(i513t i518) // horas por semana
egen ingresos=rowtotal(i524e1 i530a i538e1 i541a) // ingresos anuales
gen salario=ingresos/(horas*48)  // 48 semanas al año --> salario x hora
replace salario=. if salario==0
gen lsalario=ln(salario)  // 2012 observaciones , 1376 missing values

*VARIABLE EXPERIENCIA^2
gen experiencia2=experiencia^2

*VARIABLE SEXO*EDUCACION
gen sex_educ= sexo*educacion

*=============================================
*ESTIMACION ECONOMETRICA - ECUACION DE MINCER
*=============================================

*Modelo1
quietly reg lsalario educacion experiencia experiencia2, robust 
vif 
local vif1= 10.99
outreg2 using "$main/modelos.doc",stats(coef se pval) pdec(3) adds(Adjusted R-Squared, e(r2_a), RSS, e(rss), F-test,e(F), Prob>F, e(p), VIF, `vif1') ///
title("Estimacion de la ecuacion de ingresos de MINCER") ctitle(Modelo 1) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") word replace

*Modelo2
quietly reg lsalario educacion experiencia experiencia2 sexo,robust
vif 
local vif2= 8.51
outreg2 using "$main/modelos.doc",stats(coef se pval) pdec(3) adds(Adjusted R-Squared, e(r2_a), RSS, e(rss), F-test,e(F), Prob>F, e(p), VIF, `vif2') ctitle(Modelo 2) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") word

*Modelo3
quietly reg lsalario educacion experiencia experiencia2 sexo sex_educ, robust
vif 
local vif3= 8.26
outreg2 using "$main/modelos.doc",stats(coef se pval) pdec(3) adds(Adjusted R-Squared, e(r2_a), RSS, e(rss), F-test,e(F), Prob>F, e(p), VIF, `vif3') ctitle(Modelo 3) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") word

*Modelo4
quietly reg lsalario educacion experiencia experiencia2 sexo sex_educ region,robust
vif 
local vif4 7.22
outreg2 using "$main/modelos.doc", stats(coef se pval) pdec(3) adds(Adjusted R-Squared, e(r2_a), RSS, e(rss), F-test,e(F), Prob>F, e(p), VIF, `vif4') ctitle(Modelo 4) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") word

*Modelo5
quietly reg lsalario educacion experiencia experiencia2 sexo region,robust
vif 
local vif5 7.22
outreg2 using "$main/modelos.doc", stats(coef se pval) pdec(3) adds(Adjusted R-Squared, e(r2_a), RSS, e(rss), F-test,e(F), Prob>F, e(p), VIF, `vif5') ctitle(Modelo 5) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") word

*VIF:
// Se debe notar que las variables experiencia y experiencia al cuadrado elevan 
//el valor de la media del VIF, pues presentan un valor VIF mayor a 15 y el VIF de 
//las demas varriables es menor que 4.5


*TEST DE HETEROCEDASTICIDAD
*--------------------------

*Test de White
qui regress lsalario educacion experiencia experiencia2 sexo sex_educ region
whitetst  // Se rechaza al 1% de significancia --> modelo heterocedastico

*Test de Breusch-Pagan
qui regress lsalario educacion experiencia experiencia2 sexo sex_educ region
bpagan educacion  //p<0.01

qui regress lsalario educacion experiencia experiencia2 sexo sex_educ region
bpagan experiencia //p<0.01

qui regress lsalario educacion experiencia experiencia2 sexo sex_educ region
bpagan experiencia2 //p<0.01

qui regress lsalario educacion experiencia experiencia2 sexo sex_educ region
bpagan sexo  // p<0.01  

qui regress lsalario educacion experiencia experiencia2 sexo sex_educ region
bpagan sex_educ // p<0.01

qui regress lsalario educacion experiencia experiencia2 sexo sex_educ region
bpagan region  // 0.01<p - value<0.05  
*En todos los casos se rechaza la hipotesis de homocedasticidad

*=============================
*ANALISIS DE DATOS - GRAFICOS
*=============================

histogram educacion, discrete percent fcolor(purple) ///
     addlabel addlabopts(mlabsize(vsmall)) ///
	 ytitle("Porcentaje") xtitle("Años de educacion") ///
	 title("Años de Educación en Cajarmarca") ///
	 graphregion(color(white)) 
	 note("Fuente: Elaboración propia en base a la ENAHO 2018")

histogram experiencia, discrete percent fcolor(purple) ///
     ytitle("Porcentaje") xtitle("Años de experiencia") ///
	 title("Años de Experiencia en Cajarmarca") ///
	 graphregion(color(white)) 
	 note("Fuente: Elaboración propia en base a la ENAHO 2018")

graph hbar (mean) ingresos, over(sexo,label(labsize(small))) over(region,label(labsize(small))) ///
    bar(1, fcolor(lavender)) blabel(bar) ///
	ytitle(Ingresos de los pobladores de Cajamarca,size(small)) ///
	title("Ingreso promedio anual (S/.) de los pobladores" "de Cajamarca por sexo y por área", size(medsmall)) ///
    note("Fuente: Elaboración propia en base a la ENAHO 2018", size(vsmall))

graph box salario, over(sexo, label(labsize(small))) over(region,label(labsize(small))) nooutsides ///
    marker(1, msize(small)) marker(2, msize(small)) ///
    ytitle(Salarios por hora (S/.)) ///
    title(Diagrama de cajas de los salarios por hora (S/.) en Cajamarca, size(medsmall)) ///
	graphregion(color(white)) ///
    note("Fuente: Elaboración propia en base a la ENAHO 2018", size(small))

graph hbar (mean) salario (mean) horas, over(sexo, label(labsize(small))) ///
    over(region,label(labsize(small))) blabel(bar)  ///
	title("Salario por hora (S/.) y horas semanales promedio en" "Cajamarca por sexo y por área", size(medsmall)) ///
	note("Fuente: Elaboración propia en base a la ENAHO 2018", size(small))

graph hbar (mean) educacion (mean) experiencia, over(sexo, label(labsize(small))) ///
    over(region, label(labsize(small))) blabel(bar) ///
	title("Años de educación y experiencia promedio" "en Cajamarca por sexo y por área", size(medsmall)) ///
	note("Fuente: Elaboración propia en base a la ENAHO 2018", size(small))

*DATOS ESTADISTICOS UTILIZADOS EN EL ANALISIS DE DATOS
*----------------------------------------------

*Para saber la desviacion estandar de los errores --> Cuadro 2
bys region: summarize salario if sexo==1
bys region: summarize salario if sexo==0

*Ingresos según sexo
bys sexo: tabstat ingresos 

*Ingresos según región
bys region: tabstat ingresos
 
*Años de educacion promedio
summarize educacion

*Años de experiencia promedio
summarize experiencia
