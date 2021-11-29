global main     "C:\Users\rosme\Google Drive\PUCP\8° CICLO\Stata\Trabajo Grupal"
global dta     "$main/Bases de Datos"
global results  "$main/Resultados"
global works    "$main/Trabajadas"
global maps     "$main/Mapas"
global graphs   "$main/Graficos"

*=================
*UNIENDO LAS BASES
*=================

use "$dta/enaho01a-2019-300", clear
rename a*o año
keep año mes conglome vivienda hogar codperso ubigeo dominio estrato p300a p301b p301c p301a p207 p208a

merge 1:1 conglome vivienda hogar codperso using "$dta/enaho01a-2019-500", keepus(p506r4 p513t p512b ocupinf emplpsec ocu500 fac500a) nogenerate

merge 1:m conglome vivienda hogar codperso using "$dta/enaho04-2019-1-preg-1-a-13", keepus(e1a1) nogenerate

merge m:1 conglome vivienda hogar using "$dta/sumaria-2019", keepus(pobreza) nogenerate

duplicates list conglome vivienda hogar codperso
duplicates drop conglome vivienda hogar codperso, force
duplicates list conglome vivienda hogar codperso

sort conglome vivienda hogar codperso

*============================
*RECODIFICANDO LAS VARIABLES
*============================
rename (e1a1 p513t) (motivo_no_reg  horas)

recode ocu500 (1/1=1 "PEA Ocupada") (2/4=0 "PEA Desocupada"), gen(empleado)
keep if empleado==1      // Trabajamos con la PEA Ocupada

* VARIABLE DEPENDIENTE
*----------------------

*INFORMAL 
recode ocupinf  (1=1 "Informal") (2=0 "Formal"), gen(informal)

*VARIABLE INDEPENDIENTE
*----------------------
*EDAD
rename p208a edad

*EDAD AL CUADRADO
gen edad_sq=edad^2

*ZONA 
recode dominio (1/3=1 "Costa") (4/6=2 "Sierra") (7=3 "Selva") (8=4 "Lima Metropolitana"), gen(zona)

*AREA
recode estrato (1/5=1 "Urbano") (6/8=0 "Rural"), gen(area) 

*LENGUA MATERNA
recode p300a (1/3=1 "Nativa") (4=2 "Castellano") (6/9=3 "Otros"), gen(lengua_materna)

*EDUCACIÓN
recode p301a (1/4 = 0) (5/6 = 6) (7/10 = 11) (11 = 16) (.=.), gen(nivelprevio)
egen suma= rowtotal(p301b p301c)
gen educacion = suma + nivelprevio

*DEPARTAMENTO
gen dpto = substr(ubigeo,1,2)
destring dpto, replace
recode dpto ///
		(1 = 1 "Amazonas") ///
		(2 = 2 "Ancash") /// 
		(3 = 3 "Apurímac") /// 
		(4 = 4 "Arequipa") /// 
		(5 = 5 "Ayacucho") ///
		(6 = 6 "Cajamarca") ///
		(7 = 7 "Callao") ///
		(8 = 8 "Cusco") ///
		(9 = 9 "Huancavelica") ///
		(10 = 10 "Huánuco") ///
		(11 = 11 "Ica") ///
		(12 = 12 "Junín") ///
		(13 = 13 "La Libertad") ///
		(14 = 14 "Lambayeque") ///
		(15 = 15 "Lima") ///
		(16 = 16 "Loreto") ///
		(17 = 17 "Madre de Dios") ///
		(18 = 18 "Moquegua") ///
		(19 = 19 "Pasco") ///
		(20 = 20 "Piura") ///
		(21 = 21 "Puno") ///
		(22 = 22 "San Martín") ///
		(23 = 23 "Tacna") ///
		(24 = 24 "Tumbes") ///
		(25 = 25 "Ucayali"), gen(departamento) 

*SEXO
recode p207  (1=0 "Hombre") (2=1 "Mujer"), gen(sexo) 

*POBRE
recode pobreza (1/2 = 1 "Pobre")(3 = 0 " No pobre"), gen (pobre)

*ACTIVIDAD ECONÓMICA

recode p506r4 ///
				(111/322 = 1 " Agricultura, ganadería, silvicultura y pesca") ///
		(510/990 = 2 "Explotación de minas y canteras") /// 
		(1010/3320 = 3 " Industrias manufactureras") /// 
		(3510/3530 = 4 "Suministro de electricidad, gas, vapor y aire acondicionado") /// 
		(3600/3900 = 5 "Suministro de agua; evacuación de aguas residuales, gestión de desechos y descontaminación") ///
		(4100/4390 = 6 "Construcción") ///
		(4510/4799 = 7 "Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas") ///
		(4911/5320 = 8 "Transporte y almacenamiento") ///
		(5510/5630 = 9 "Actividades de alojamiento y de servicio de comidas") ///
		(5811/6399 = 10 "Información y comunicaciones") ///
		(6411/6630 =11 "Actividades financieras y de seguros") ///
		(6810/6820 = 12 "Actividades inmobiliarias") ///
		(6910/7500 = 13 "Actividades profesionales, científicas y técnicas") ///
		(7710/8299 = 14 "Actividades de servicios administrativos y de apoyo") ///
		(8411/8430 = 15 "Administración pública y defensa; planes de seguridad social de afiliación obligatoria") ///
		(8510/8550 = 16 "Enseñanza") ///
		(8610/8890 = 17 "Actividades de atención de la salud humana y de asistencia social") ///
		(9000/9329 = 18 "Actividades artísticas, de entretenimiento y recreativas") ///
		(9411/9609 = 19 "Otras actividades de servicios") ///
		(9700/9820 = 20 "Actividades de los hogares como empleadores; actividades no diferenciadas de los hogares como productores de bienes y servicios para uso propio") ///
		(9900= 21 "Actividades de organizaciones y órganos extraterritoriales"), gen(sector_ec) 

*TAMAÑO DE LA EMPRESA

*micro empresa : 1 y 10
*pequeña empresa : 11 y 100
*mediana empresa: 101 y 500
*gran empresa : mas de 500

gen t_empresa=1 if p512b<11
replace t_empresa=2 if p512b>10 & p512b<101
replace t_empresa=3 if p512b>100 & p512b<501
replace t_empresa=4 if p512b>500

tostring t_empresa, replace
encode t_empresa, gen (ta_empresa)
recode ta_empresa ///
        (1 = 1 "Micro Empresa") ///
		(2 = 2 "Pequeña Empresa") /// 
		(3 = 3 "Mediana Empresa") /// 
		(4 = 4 "Empresa Grande"), gen(tamaño_empresa)

recode motivo_no_reg  (1 5=1 "Burocracia") (2/3=2 "Información") (4=3 "Impuestos") (6/7=4 "Caracteristicas del trabajo")  (8=5 "Subjetivo") (9=6 "Otros"), gen(motivo)

* TRAMITE 
gen burocracia = 1 if motivo==1

*FALTA DE INFORMACION
gen informacion = 1 if motivo==2

*CARGA IMPUESTO
gen impuesto = 1 if motivo==3

mvencode burocracia informacion impuesto, mv(0) override


drop dominio estrato p300a p301a p301b p301c p207 dpto p506r4 motivo_no_reg t_empresa ta_empresa p512b ocu500 ocupinf nivelprevio suma pobreza
 
label var area 		     "Area"
label var zona 		     "Zona"
label var departamento	 "Departamento"
label var lengua_materna "Lengua Materna"
label var informal		 "Situacion de Informalidad"
label var sexo 		     "Sexo"
label var sector_ec		 "Sector Economico"
label var empleado	     "Empleado"
label var motivo         "Motivo por el que no se ha registrado en la SUNAT"
label var tamaño_empres  "Tamaño de la empresa"
label var edad           "Edad en años"
label var pobre          "Situación de pobreza"

save "$works/informalidad.dta", replace

*=======================
*DESCRIPCION ESTADISTICA
*=======================
gen facfw = round(fac500a)

*GRÁFICOS
*--------
*Gráfico de la tasa de informalidad según sexo y area
table sexo area [fw=facfw], c(mean informal)  //Solo para comprobar datos
graph hbar (mean) informal [fw=facfw], ///
    over(sexo,label(labsize(small))) ///
    over(area,label(labsize(small))) ///
    bar(1, fcolor(lavender)) blabel(bar, format(%3.2f)) ///
	ytitle(Porcentaje de informales) ///
	graphregion(color(white)) ///
	title("Tasa de informalidad según sexo y area 2019" , size(medsmall)) ///
    note("Fuente: Elaboración propia en base a la ENAHO 2019", size(vsmall))
graph export "$graphs/bar_sexo_area.png", as(png) replace

*Grafico de la informalidad según zona
tab informal zona [iw=fac500a], col

graph pie informal [aw=fac500a], over(zona) ///
	plabel(_all percent, size(vsmall) color(white)) ///
	graphregion(color(white)) ///
	legend(rows(2) region(lcolor(white)))  ///
	title("Informalidad según zona") 
graph export "$graphs/pie_zona.png", as(png) replace

*Gráfico de la cantidad de informales por departamento
table departamento tamaño_empresa [iw=fac500a] if informal==1, c(sum informal) format(%4.0f) 
tab departamento informal [iw=fac500a], row
graph hbar (sum) informal [fw = facfw], ///
	    over(departamento, label(labsize(small))) ///
		blabel(total, format(%12.0f) size(tiny)) ///
		ytitle("Cantidad de informales") ///
		title("Informales por departamento, 2019") ///
		graphregion(color(white)) ylabel(,nogrid) ///
		note("Fuente: Elaboración propia en base a la ENAHO 2019", size(vsmall))
graph export "$graphs/hbar_informal.png", as(png) replace

*Gráfico de los motivos por los que los informales no se han registrado
tab informal motivo if informal==1 [iw=fac500a], row
graph pie informal if informal==1 [aw=fac500a], over(motivo) ///
	  plabel(_all percent, size(vsmall) color(white)) ///
	  graphregion(color(white)) ///
	  legend(rows(3) region(lcolor(white)))  ///
	  title("Motivos por lo que no se ha registrado en la SUNAT") 
graph export "$graphs/pie_motivo.png", as(png) replace

*TABLAS
*-------

*Tabla del porcentaje de formalidad e informalidad según la situación de pobreza
tab informal pobre [iw=fac500a], col

*Tabla de cantidad de informales según departamento y tamaño de empresa
table departamento tamaño_empresa [iw=fac500a], c(sum informal) format(%4.0f) 

*Tabla del porcentaje de formalidad e informalidad según el sector económico
tab sector_ec  informal [iw=fac500a], row

*Tabla de porcentaje del area es rural según zonas
tab zona area, row                    

*MAPA A NIVEL NACIONAL
*---------------------
use "$works/informalidad.dta", clear 
collapse (mean) informal, by(departamento)
gen id=_n
save "$works/informales_dpto_2019.dta",replace

shp2dta using "$maps/LIMITE_DEP.shp", database("$works/dato-dpto.dta") coordinates("$works/coord-dpto.dta") genid(id) genc(c) replace

use "$works/dato-dpto.dta", clear
merge 1:1 id using "$works/informales_dpto_2019.dta",  nogenerate

preserve
gen label = NOMBDEP
keep id x_c y_c label
gen length = length(label)
save "$works/labels_dpto.dta", replace
restore

spmap informal using "$works/coord-dpto.dta", id(id) clmethod(q) cln(6) title("Mapa Cuantiles: Informalidad" "según  departamento 2019") legend(subtitle("Leyenda") size(medium) position(8)) fcolor(Reds2) name(cuantiles, replace) label(data("$works/labels_dpto.dta") x(x_c) y(y_c) color(lime) label(label) size(*0.5) position(4) length(25)) note("Fuente: ENAHO 2019-INEI") 
graph export "$graphs/mapa_informalidad_dpto.png", as(png) replace 


*MAPA A NIVEL PROVINCIAL
*------------------------
use "$works/informalidad.dta", clear 
gen IDPROV= substr(ubigeo,1,4)

collapse (mean) informal, by(IDPROV)
save "$works/informales_prov_2019.dta",replace

shp2dta using "$maps/PROVINCIA_27_04_2015.shp", database("$works/dato-prov.dta") coordinates("$works/coord-prov.dta") genid(id) genc(c) replace

use "$works/dato-prov.dta", clear
merge 1:1 IDPROV using "$works/informales_prov_2019.dta", nogenerate

preserve
gen label = NOMBPROV
keep id x_c y_c label
gen length = length(label)
save "$works/labels_prov.dta", replace
restore

spmap informal using "$works/coord-prov.dta", id(id) clmethod(q) cln(6) title("Mapa Cuantiles: Informalidad" "según provincia 2019") legend(subtitle("Leyenda") size(medium) position(8)) fcolor(Blues2) name(cuantiles, replace) label(data("$works/labels_prov.dta") x(x_c) y(y_c) color(cyan) label(label) size(*0.3) position(6) length(25)) note("Fuente: ENAHO 2019-INEI") 
graph export "$graphs/mapa_informalidad_prov.png", as(png) replace

*========================
*ESTIMACION ECONOMETRICA
*========================
use "$works/informalidad.dta", clear 

*Comparamos el modelo Logit Vs Probit
*------------------------------------
quietly logit informal c.edad##c.edad i.sexo educacion i.lengua_materna i.pobre i.area ib4.zona i.sector_ec i.tamaño_empresa i.burocracia i.impuesto i.informacion
quietly fitstat, saving(log)

quietly probit informal c.edad##c.edad i.sexo educacion i.lengua_materna i.pobre i.area ib4.zona i.sector_ec i.tamaño_empresa i.burocracia i.impuesto i.informacion
fitstat, using (log) force 

display "Punto Máximo= " abs(_b[edad]/(2*_b[c.edad#c.edad])) 

*El modelo más adecuado es el probit

*Comparando los modelos Probit
*-------------------------------
*Caracteristicas individuales:  edad sexo educacion lengua_materna pobreza
*Caracteristicas geográficas :  area zona
*Caracteristicas laborales   :  sector_ec tamaño_empresa
*Motivos de no registrarse   :  burocracia impuesto informacion

*Modelo 1 : Modelo con características personales
quietly probit informal c.edad##c.edad i.sexo educacion i.lengua_materna i.pobre, nolog
quietly fitstat, saving(mod1) 

*Modelo 2 : Modelo con características personales y geográficas
quietly probit informal c.edad##c.edad i.sexo educacion i.lengua_materna i.pobre i.area ib4.zona , nolog
fitstat, using(mod1) saving (mod2)		

*Modelo 3 : Modelo con características personales, geográficas y laborales
quietly probit informal  c.edad##c.edad i.sexo educacion i.lengua_materna i.pobre i.area ib4.zona i.sector_ec i.tamaño_empresa, nolog
fitstat, using(mod2) saving(mod3) 

*Modelo 4 : Modelo con características personales, geográficas, laborales y motivos
quietly probit informal c.edad##c.edad i.sexo educacion i.lengua_materna i.pobre i.area ib4.zona i.sector_ec i.tamaño_empresa i.burocracia i.impuesto i.informacion, nolog
fitstat, using(mod3) saving(mod4) 	

*El modelo completo a desarrollar será el modelo 4 que contempla las variables de características personales, geográficas, laborales y motivos. Vemos que tiene un menor BIC de los 4 modelos presentados.

*====================
*EFECTOS MARGINALES
*====================

quietly probit informal edad edad_sq i.sexo educacion i.lengua_materna i.pobre i.area ib4.zona i.sector_ec i.tamaño_empresa i.burocracia i.impuesto i.informacion, nolog
margins, dydx(_all) atmeans post
outreg2 using "$results/modelo_informalidad.doc", stats(coef) ctitle(Margins) title("ESTIMACION DE LA PROBABILIDAD DE INFORMALIDAD 2019") addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") word replace
outreg2 using "$results/modelo_informalidad.doc", stats(se) ctitle(Standard error) word append 

marginsplot, horizontal unique xline(0) recast(scatter) yscale(reverse) graphregion(color(white)) allx title("Marginal Effects Pr(Informal = 1)") ytitle("Variables") xtitle("") ylabel(, nogrid) name(A, replace)
graph export "$graphs/margins_plot.png", as(png) replace

*==================
*TEST PARAMÉTRICOS
*==================

*TEST DE PROPORCIONES
*--------------------
*Area
prtest informal, by(area) level(95)

*Sexo
prtest informal, by(sexo) level(95)

*Microempresas
recode tamaño_empresa (1=1 "Micro-Empresa") (2/4= 0 "No Micro-Empresa"), gen (microempresa)
prtest informal, by(microempresa) level(95)

*Lima Metropolitana
recode zona (1/3=0 "No Lima Metropolitana") (4=1  "Lima Metropolitana"), gen (lima)
prtest informal, by(lima) level(95)

*TEST DE VARIANZAS
*--------------------
*Area
bys area: sum informal   	           // Urbano tiene mayor dispersion                 
sdtest informal, by(area)

*Sexo
bys sexo: sum informal   	           // Hombre tiene mayor dispersion                 
sdtest informal, by(sexo)

*Microempresas
bys microempresa: sum informal   	           // No Micro-Empresa tiene mayor dispersion                 
sdtest informal, by(microempresa)

*Lima Metropolitana  
bys lima: sum informal   	           // Lima tiene mayor dispersion                 
sdtest informal, by(lima)

*TEST DE MEDIAS
*--------------
*Area
mean informal, over(area) level(95)
ttest informal, by(area) unequal level(95)  

*Sexo
mean informal, over(sexo) level(95)
ttest informal, by(sexo) unequal level(95)  

*Microempresas
mean informal, over(microempresa) level(95)
ttest informal, by(microempresa) unequal level(95)  

*Lima Metropolitana  
mean informal, over(lima) level(95)
ttest informal, by(lima) unequal level(95)

*=========================
*ANALISIS POST-ESTIMACION
*=========================

* Test de Wald : 
*---------------
quietly probit informal edad edad_sq educacion area zona sexo lengua_materna pobre sector_ec tamaño_empresa informacion impuesto burocracia

test edad edad_sq educacion area zona sexo lengua_materna pobre sector_ec tamaño_empresa informacion impuesto burocracia

* Predicción del modelo : 
*------------------------
quietly probit informal c.edad##c.edad educacion area zona sexo lengua_materna pobre sector_ec tamaño_empresa informacion impuesto burocracia
predict prprobit		   
summarize prprobit
* 0.775 es la probabilidad de que la persona sea informal
dotplot prprobit, ylabel(0(.2)1) graphregion(color(white)) 		
graph export "$graphs/dotpredict_probit.png", as(png) replace	

quietly logit informal c.edad##c.edad educacion area zona sexo lengua_materna pobre sector_ec tamaño_empresa informacion impuesto burocracia
predict prlogit		   
summarize prlogit
* 0.773 es la probabilidad de que la persona sea informal
dotplot prlogit, ylabel(0(.2)1)	 graphregion(color(white)) 	
graph export "$graphs/dotpredict_logit.png", as(png) replace

corr prlogit prprobit	

* Predicción individual :
*---------------------------
quietly probit informal edad edad_sq educacion area zona sexo lengua_materna pobre sector_ec tamaño_empresa informacion impuesto burocracia, nolog

*Prediccion según si es pobre o no
prvalue, x(pobre=0) rest(mean)
prvalue, x(pobre=1) rest(mean)


*Hombre que vive en la Costa
prvalue, x(zona=1 sexo=0) rest(mean)
*Mujer que vive en la Costa
prvalue, x(zona=1 sexo=1) rest(mean)

*Hombre que vive en la Sierra
prvalue, x(zona=2 sexo=0) rest(mean)
*Mujer que vive en la Sierra
prvalue, x(zona=2 sexo=1) rest(mean)

*Hombre que vive en la Selva
prvalue, x(zona=3 sexo=0) rest(mean)
*Mujer que vive en la Sierra
prvalue, x(zona=3 sexo=1) rest(mean)

*Hombre que vive en la Lima Metropolitana
prvalue, x(zona=3 sexo=0) rest(mean)
*Mujer que vive en la Lima Metropolitana
prvalue, x(zona=3 sexo=1) rest(mean)

*Prediccion según tamaño de empresa
prvalue, x(tamaño_empresa=1) rest(mean)
prvalue, x(tamaño_empresa=2) rest(mean)
prvalue, x(tamaño_empresa=3) rest(mean)
prvalue, x(tamaño_empresa=4) rest(mean)

*Hombre joven que trabaje en una microempresa, pobre 
prvalue, x(sexo=0 edad=25 tamaño_empresa=1 pobre=1) rest(mean) 

*Mujer joven que trabaje en una microempresa, pobre 
prvalue, x(sexo=1 edad=25 tamaño_empresa=1 pobre=1) rest(mean) 

*Persona rural, joven que trabaje en una microempresa, pobre 
prvalue, x(area=0 edad=25 tamaño_empresa=1 pobre=1) rest(mean) 

*Persona urbana, joven que trabaje en una microempresa, pobre 
prvalue, x(area=1 edad=25 tamaño_empresa=1 pobre=1) rest(mean)


*Graficando las probabilidades :
*-------------------------------

*Efecto del tamaño de la empresa sobre la probabilidad de ser informal por edad
quietly probit informal c.edad##c.edad educacion area zona sexo lengua_materna pobre sector_ec tamaño_empresa informacion impuesto burocracia
margins, at(edad=(14(10)98) tamaño_empresa=(1(1)4))

marginsplot, noci legend(cols(2)) ytitle("Pr(Informal)") xtitle("Edad en años") title("Predicción para tamaño de empresa según edad")  ylabel(0(0.1)1) graphregion(color(white))
graph export "$graphs/margin_binario.png", as(png) replace

/*
* Predicción para microempresas
prgen edad, from(14) to(98) generate(p20) x(tamaño_empresa=1) rest(mean) n(11)
label var p20p1 "Micro-Empresa"

* Predicción para pequeñas empresas
prgen edad, from(14) to(98) generate(p30) x(tamaño_empresa=2) rest(mean) n(11)
label var p30p1 "Pequeña Empresa"

* Predicción para medianas empresas
prgen edad, from(14) to(98) generate(p40) x(tamaño_empresa=3) rest(mean) n(11)
label var p40p1 "Mediana Empresa"

* Predicción para empresas grandes
prgen edad, from(14) to(98) generate(p50) x(tamaño_empresa=4) rest(mean) n(11)
label var p50p1 "Empresa Grande"

list p20p1 p30p1 p40p1 p50p1 p50x in 1/11

#delimit ;
graph twoway connected p20p1 p30p1 p40p1 p50p1 p50x, ytitle("Pr(Informal)") xtitle("Edad en años") ;
marginsplot, noci legend(cols(4)) ytitle("Pr(Informal)") ylabel(0(.25)1) xtitle("Tamano de Empresa") graphregion(color(white));
graph export "$works/pred_logit.png", as(png) replace;
#delimit cr
*/
