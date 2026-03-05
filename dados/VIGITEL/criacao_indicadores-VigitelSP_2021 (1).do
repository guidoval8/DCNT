use "D:\Users\regin\VIGITEL\SP\dados\2021\BD_Vigitel-SP_16-12-2021_pesorake.dta", clear


*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
***** Tabagismo

** Fumante atual
*Criar variável dicotômica, agrupando os que fumam diariamente ou não diariamente (1: Sim, diariamente OU Sim, mas não diariamente / 0: Não)
gen fumante = cond(Q60<3 & Q60!=., 1, 0)
la def sim_nao 1"Sim" 0"Nao"
la val fumante sim_nao
la var fumante "Fumantes"
tab fumante

** Consumo de 20 ou mais cigarros ao dia
*Criar variável dicotômica, agrupando os fumantes pela quantidade de cigarro (1: >=20 cigarros por dia / 0: <20 cigarros por dia)
*Utilizar a variável número de cigarros diários com agrupamento em faixas
gen mais20 = cond(Q61_FX >= 5 & Q61_FX <= 7, 1, 0)
la val mais20 sim_nao
la var mais20 "Consumo de 20 ou mais cigarros ao dia"
tab mais20

** Fumante passivo no domicílio
*Criar variável dicotômica (1: Sim / 0: Não)
gen fumocasa = cond(Q67 ==1, 1, 0)
replace fumocasa = 0 if fumante == 1
la val fumocasa sim_nao
la var fumocasa "Fumantes passivos no domicilio"
tab fumocasa

** Fumante passivo no local de trabalho
*Criar variável dicotômica (1: Sim / 0: Não)
gen fumotrab = cond(Q68 ==1, 1, 0)
replace fumotrab = 0 if fumante == 1
la val fumotrab sim_nao
la var fumotrab "Fumantes passivos no local de trabalho"
tab fumotrab

** Fumante passivo no local de trabalho
*Criar variável dicotômica (1: Sim / 0: Não)
gen eletronico = cond(R403 == 1| R403 == 2, 1, 0)
la val eletronico sim_nao
la var eletronico "Aparelhos eletônicos com nicotina"
tab eletronico

*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
***** Antropometria

**Cálculo do IMC com valores imputados de peso e altura: peso (em kg) dividido pela altura (em cm)/100 ao quadrado
replace Q9_i = Q9 if Q9_i == .
replace Q11_i = Q11 if Q11_i == .
gen imc_i  =  Q9_i/((Q11_i/100)*(Q11_i/100))
replace imc_i = . if (Q11_i >= 700 | Q9_i >= 700)
la var imc_i "IMC com imputações"
label variable Q9_i "peso (kg) - valores imputados"
label variable Q11_i "altura (cm) - valores imputados"

** Excesso de peso (COM VALORES IMPUTADOS DE PESO E ALTURA)
*Criar variável dicotômica, IMC menor que 25 e maior ou igual que 25 (1: >= 25 / 0: <25)
gen excpeso_i = cond(imc_i >= 25 & imc_i <= 115, 1, 0)
la val excpeso_i sim_nao
la var excpeso_i "Excesso de peso"
tab excpeso_i

**Obesidade (COM VALORES IMPUTADOS DE PESO E ALTURA)
*Criar variável dicotômica, IMC menor que 30 e maior ou igual que 30 (1: >= 30 / 0: <30)
gen obesid_i = cond(imc_i >= 30 & imc_i <= 115, 1, 0)
la val obesid_i sim_nao
la var obesid_i "Obesidade"
tab obesid_i

*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
***** Dieta/Alimentação

** Consumo regular de hortaliças (5 ou mais dias/semana)
*Criar variável dicotômica agrupando quem consome hortaliças 5 dias por semana ou mais (1: >=5 dias por semana / 0: <5 dias por semana)
gen hortareg = cond(Q16 == 3 | Q16 == 4, 1, 0)
la val hortareg sim_nao
la var hortareg "Consumo regular de hortaliças (5x ou mais/Sem)"
tab hortareg

** Consumo regular de frutas (5 ou mais dias/semana)
*Criar variável dicotômica agrupando quem consome frutas ou suco de frutas 5 dias por semana ou mais (1: >=5 dias por semana / 0: <5 dias por semana)
gen frutareg = cond(Q25 == 3 | Q25 == 4 | Q27 == 3 | Q27 == 4, 1, 0)
la val frutareg sim_nao
la var frutareg "Consumo regular de frutas (5x ou mais/Sem)"
tab frutareg

** Consumo REGULAR de frutas E hortaliças(5 ou mais dias/semana)
*Criar variável dicotômica juntando consumo de hortaliças e frutas 5 dias por semana ou mais 
gen flvreg = cond(frutareg == 1 & hortareg == 1, 1, 0)
la val flvreg sim_nao
la var flvreg "Consumo regular de frutas E hortaliças(5x ou mais/Sem)"
tab flvreg

** Consumo RECOMENDADO de frutas E hortaliças (5x ou mais/dia)
*Esse indicador é calculado por etapas:

*1a etapa
*Contagem hortaliças cruas (Soma do número de vezes de consumo de hortaliças cruas por dia)
gen cruadia = 1 if (Q18 == 1 | Q18 == 2)
replace cruadia = 2 if Q18 == 3
replace cruadia = 0 if cruadia == .
la var cruadia "Contagem de hortaliças cruas/ dia"

*2a etapa
*Contagem hortaliças cozidas (Soma do número de vezes de consumo de hortaliças cozidas por dia)
gen cozidadia = 1 if (Q20 == 1 | Q20 == 2)
replace cozidadia = 2 if Q20 == 3
replace cozidadia = 0 if cozidadia == .
la var cozidadia "Contagem de hortaliças cozidas/ dia"

*3a etapa
*Contagem hortaliças TOTAL (Soma do total de hortaliças consumidas por dia)
gen hortadia = cruadia + cozidadia
la var hortadia "Contagem de hortaliças/ dia"

*4a etapa
*Contagem suco dia - Máx. 1/dia (Soma do suco de frutas - só considerar 1 copo por dia)
gen sucodia = cond(Q26 >= 1 & Q26 <= 3, 1, 0)
la var sucodia "Contagem suco/ dia - Máx. 1/dia"

*5a etapa
*Contagem fruta dia - Sem suco (Soma do total de frutas consumidas por dia)
gen  sofrutadia = 1 if (Q28 == 1)
replace sofrutadia = 2 if (Q28 == 2)
replace sofrutadia = 3 if (Q28 == 3)
replace sofrutadia = 0 if (sofrutadia == .)
la var sofrutadia "Contagem fruta/ dia - Sem suco"

*6a etapa
*Contagem fruta TOTAL dia (Soma do suco de frutas e total de frutas consumidas por dia)
gen frutadia = sofrutadia + sucodia 
la var frutadia "Contagem fruta TOTAL/ dia"

*7a etapa
*Contagem fruta e hortaliças dia (Soma do total de frutas e hortaliças por dia)
gen flvdia = hortadia + frutadia
la var flvdia "Contagens fruta e hortaliça TOTAL/ dia"

*última etapa
*Criar variável dicotômica considerando atingir a recomendação como consumir 5 ou  mais porções de FLV em 5 ou mais dias na semana
*Consumo recomendado de frutas e hortaliças (FINAL)
gen flvreco = cond((flvdia>=5 & flvdia<=8) & flvreg == 1, 1,0)
la val flvreco sim_nao
la var flvreco "Consumo recomendado de frutas e hortaliças"
tab flvreco

** Consumo regular de refrigerante TOTAL
*Criar variável dicotômica com quem bebe refrigerante de qualquer tipo 5 ou mais dias por semana (1: consome >=5dias/semana de refrigerante de qualquer tipo / 0: consome <5dias/semana de refrigerante de qualquer tipo)
*O tipo do refrigerante não foi questionado no vigitel 2012, 2013 e 2014. Logo, esse indicador considera todos os tipos de refrigerantes.
*Excluído o indicador consumo de refrigerantes adoçados
gen refritl5 = cond(Q29  == 3 | Q29 == 4, 1, 0)
la val refritl5 sim_nao
la var refritl5 "Consumo regular de refrigerantes (5x ou mais/Sem)"
tab refritl5

** Consumo regular de feijão (5x ou mais/Sem)
***OBSERVAÇÃO! A PERGUNTA Q15  FOI FEITA NO VIGITEL 2019, O INDICADOR DE FEIJÃO ESTARÁ NO RELATÓRIO. . 
*Criar variável dicotômica com quem consome feijão 5 ou mais dias por semana (1: >=5 dias por semana / 0: <5 dias por semana)
gen feijao5 = cond(Q15  == 3 | Q15 == 4, 1, 0)
la val feijao5 sim_nao
la var feijao5 "Consumo regular de feijao(5x ou mais/Sem)"
tab feijao5


*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
***** Escores de alimentação

recode R301_a- R302_m  (2 = 0)

gen score_sf = R301_a + R301_b + R301_c + R301_d  + R301_e + R301_g + R301_l
recode score_sf (0 1 2 3 4 = 0) (5 6 7 = 1), gen(score_sf_2cat)
la var score_sf "Consumo de cinco ou mais rupos de alimentos não ou minimamente processados protetores"

gen score_upp = R302_a + R302_b + R302_c + R302_d + R302_e + R302_f + R302_g + R302_h + R302_i + R302_j + R302_k + R302_l+ R302_m
recode score_upp (0 1 2 3 4 = 0) (5/13 = 1), gen( score_upp_2cat)
la var score_upp "Consumo de cinco ou mais grupos de alimentos ultra processados"
************************************
*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
***** Atividade física

*A partir do VIGITEL 2011
*Esse indicador é criado em etapas

**Atividade física suficiente no lazer - SOMENTE TEMPO/ SEM FREQUENCIA (LUANE, SVS/MS) (a partir do VIGITEL 2011)

*1a etapa
*Criar variável agrupando as modalidade de atividade física segundo a intesidade, independente da frequencia
*moderada ou vigorosa (Parc.)
*ATENCAO: UMA NOVA OPCAO FOI INSERIDA EM 2013, agora temos 17 opcoes
gen af=.
replace af=1 if (Q43a==1 |  Q43a==2 | Q43a==5 | Q43a==7 | Q43a==8 | Q43a==9 |  Q43a==10 |  Q43a==11 |  Q43a==14 | Q43a==16 | Q43a==17)
replace af=2 if (Q43a==3 | Q43a==4 | Q43a==6 | Q43a==12 | Q43a==13 | Q43a==15)
replace af=0 if Q43a==.
la var af "Tipo atv. fis. no tempo livre (lazer)"

*2a etapa
*Criar variável recodificando a frequencia de realização de atividade fisica
*frequencia 
gen freq=.
replace freq=1.5 if Q45==1  
replace freq=3.5 if Q45==2  
replace freq=5.5 if Q45==3
replace freq=7 if Q45==4    
replace freq=0 if Q45==.
la var freq "Frequencia de realizacao de atv. fis. no tempo livre (lazer)"

*3a etapa
*Criar variável recodificando a duração da atividade física
*duracao 
gen time=.
replace time=0 if Q46==1  
replace time=14.5 if Q46==2
replace time=24.5 if Q46==3
replace time=34.5 if Q46==4
replace time=44.5 if Q46==5
replace time=54.5 if Q46==6
replace time=60 if Q46==7
replace time=0 if Q46==.
la var time "Tempo de realizacao de atv. fis. no tempo livre (lazer)"

*4a etapa
*Criar a variável contínua da atividade física no lazer multiplicando a intensidade, a frequência e a duração da atividade física
gen ati_livre=af*freq*time
la var ati_livre "af*freq*time (continua)"

*última etapa
*Criar variável dicotômica agrupando a atividade física contínua em inativo ou ativo.
gen ativo_livre=ati_livre
recode ativo_livre 0/149=0 150/max=1 .=.
label define ativo_livre2  0 "Inativo/Ins"  1 "Ativo"
label values ativo_livre ativo_livre2
la var ativo_livre "Atividade física suficiente no lazer - SOMENTE TEMPO/ SEM FREQUENCIA (a partir do VIGITEL 2011)"
tab ativo_livre

**Atividade física no deslocamento
*Criar variável dicotômica agrupando quem se desloca mais de 30 minutos a pé ou de bike para o trabalho ou escola
gen atitrans = cond((Q51>3 & Q51!=.)|(Q54>3 & Q54!=.), 1, 0)
la val atitrans sim_nao
la var atitrans "Atividade fisica no deslocamento"
tab atitrans

**Atividade física na limpeza do domicílio
*Criar variável dicotômica considerando quem faz a tarefa de limpeza pesada sozinho ou não
gen atidom = cond((Q55==1) | (Q56==1), 1, 0)
la val atidom sim_nao
la var atidom "Atividade fisica no trabalho domestico"
tab atidom

**Atividade física na ocupação(parc.)
*Criar variável dicotômica considerando quem carrega peso OU anda muito a pé no trabalho ou não
gen atiocu = cond(Q48 == 1 | Q49 == 1, 1, 0)
la val atiocu sim_nao
la var atiocu "Atividade fisica no trabalho"
tab atiocu

**Inatividade física
*Criar variável dicotômica considerando inativo quem não faz atividade física, não se desloca a pé ou de bike para trabalho/escola, não faz limpeza pesada e não tem trabalho com atividade física
gen inativo = cond(((Q42 == 2) & ((Q50==3 | Q50==. | Q51==1 | Q51==2 )&(Q53==3 | Q53==. | Q54==1 | Q54==2)) & (atiocu==0) & (atidom==0)), 1, 0)
la val inativo sim_nao
la var  inativo "Inatividade fisica"
tab inativo


***Indicador global de AF (Compatível com o GPAQ)
**A fracao da Atividade física de lazer já foi criada, se chama "ati_livre"

**Atividade física no deslocamento
gen Q51medio =.
replace Q51medio = 0 if Q51 == 1
replace Q51medio = 0 if Q51 == 2 
*(a somatória do tempo de ida e volta tem de ser >= 20 min)
replace Q51medio = 24.5 if Q51 == 3
replace Q51medio = 34.5 if Q51 == 4
replace Q51medio = 44.5 if Q51 == 5
replace Q51medio = 54.5 if Q51 == 6
replace Q51medio = 60 if Q51 == 7
replace Q51medio = 0 if Q51medio==. 
la var Q51medio "Duração diária do deslocamento para trabalho (min)"

gen Q54medio =.
replace Q54medio = 0 if Q54 == 1
replace Q54medio = 0 if Q54 == 2 
*(a somatória do tempo de ida e volta tem de ser >= 20 min)
replace Q54medio = 24.5 if Q54 == 3
replace Q54medio = 34.5 if Q54 == 4
replace Q54medio = 44.5 if Q54 == 5
replace Q54medio = 54.5 if Q54== 6
replace Q54medio = 60 if Q54 == 7
replace Q54medio = 0 if Q54medio==. 
la var Q54medio "Duração diária do deslocamento para o escola (min)"

gen deslocdia =  Q51medio +  Q54medio
la var deslocdia "Duração diária do deslocamento para o trabalho e escola (min)"

gen deslocsemana = deslocdia * 5 
*(para ser representativo de uma semana, considerando 5 dias úteis)
la var deslocsemana "Duração semanal do deslocamento para o trabalho e escola (min)"

**Atividade física laboral
recode R147 555/888=.
recode R148_HH 777=.
recode R148_MM 777=.
gen atiocusemana = ((R148_HH * 60) + R148_MM) * R147
replace atiocusemana = 0 if atiocusemana ==.
la var atiocusemana "Duração semanal da atividade física laboral (min)"

**Atividade física doméstica
recode R149 555/888=.
recode R150_HH 777=.
recode R150_MM 777=.
gen faxinasemana = (( R150_HH* 60) +  R150_MM) *  R149
replace faxinasemana = 0 if  faxinasemana ==.
la var faxinasemana "Duração semanal da atividade física doméstica(min)"

*Experimentos anteriores (considerando mais domínios ou ponderando alguns deles) encontram-se disponíveis nas rotinas para os anos de 2014, 15 e 16.

*AF global considerando os domínios: lazer, deslocamento e trabalho 
gen af3dominios = cond(ati_livre + deslocsemana + atiocusemana >= 150, 1, 0)
la val af3dominios sim_nao
la var af3dominios "Atividade física  >= 150min/sem em 3 domínios"

******Prática insuficiente de atividade física
recode af3dominios (0 = 1) (1 = 0), gen(af3dominios_insu)
la val af3dominios_insu sim_nao
la var af3dominios_insu "Atividade física  <= 150min/sem em 3 domínios"
tab af3dominios_insu


*******NÃO ENTROU NO RELATÓRIO 2018
**Hábito de assistir TV - 3hrs/dia 
*Criar variável dicotômica considerando quem assiste 3 horas ou mais de TV por dia ou não
gen tv_d_3 = cond((Q59a >= 4 & Q59a != 8 & Q59a != .) , 1, 0) 
la val tv_d_3 sim_nao
la var tv_d_3 "Hábito de assistir TV - 3hrs/dia"
tab tv_d_3

**Tempo sentado
*gen sentado = cond(R201 == 7, 1, 0)

*********NÃO ENTROU NO RELATÓRIO 2018
**Tempo de tela (não TV) - 3hrs/dia 
*Criar variável dicotômica considerando quem passas 3 horas ou mais na frente de uma tela (lazer) por dia ou não
gen tempo_tela_stv = cond(Q59c >= 4 & Q59c != ., 1, 0)
la val tempo_tela_stv sim_nao
la var tempo_tela_stv "Tempo de tela (não TV) - 3hrs/dia"
tab tempo_tela_stv

**Tempo de tela (TOTAL) - 3hrs/dia 
*Criar variável dicotômica considerando quem passas 3 horas ou mais na frente de uma tela (lazer) por dia ou não
recode Q59a (1=1) (2 = 1.5) (3 = 2.5) (4 = 3.5) (5=4.5) (6=5.5) (7=6) (8=0) (. = 0), gen(Q59a_horas)
recode Q59c (1=1) (2 = 1.5) (3 = 2.5) (4 = 3.5) (5=4.5) (6=5.5) (7=6) (. = 0), gen(Q59c_horas)
gen tempo_tela_total = cond(Q59a_horas + Q59c_horas >= 3, 1, 0)
la val tempo_tela_total sim_nao
la var tempo_tela_total "Tempo de tela (TOTAL) - 3hrs/dia"
tab tempo_tela_total


*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
*****Consumo de bebidas alcoólicas

**Consumo abusivo de bebidas alcoólicas
*Criar variável dicotômica, considerando o número de doses máximo para homens e mulheres
gen alcabu = cond(Q37==1 | Q38==1, 1, 0)
la val alcabu sim_nao
la var alcabu "Consumo abusivo de alcool"
tab alcabu

******NÃO PRESENTE NO RELATÓRIO 2018
**Condução de veículo após consumo ABUSIVO de bebida alcoólica
gen direcao = cond(Q40 == 1, 1, 0)
replace direcao = 0 if Q40 != 1
la val direcao sim_nao
la var direcao "Condução de veículo após consumo ABUSIVO de bebida alcoólica"
tab direcao

*Criar variável dicotômica, considerando a prática de dirigir após beber, independente da quantidade ou da frequencia
**Condução de veículo após consumo bebida alcoólica
gen direcao_alc=cond((Q40b == 1 | Q40b == 2 | Q40b == 3), 1, 0)
replace direcao_alc = 1 if Q40 == 1
la val direcao_alc sim_nao
la var direcao_alc "Condução de veículo após consumo bebida alcoólica"
tab direcao_alc


*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
*****Avaliação do estado de saúde

**Avaliação ruim da condição de saúde
*Criar variável dicotômica, com quem considera sua saúde ruim ou muito ruim e demais
gen saruim = cond(Q74 == 4 | Q74 == 5, 1, 0)
la val saruim sim_nao
la var saruim "Avaliação ruim da condição de saúde"
tab saruim

*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
*****Prevenção do Câncer

**Realização de mamografia

*1a etapa
*Criar variável com categorias de faixa etária
*Faixas de idade alvo
gen iddmamo = 1 if (Q6 >= 50 & Q6<=59) & (Q7==2)
replace iddmamo = 2 if (Q6 >= 60 & Q6<=69) & (Q7==2)
la var iddmamo "Faixas de idade alvo para mamografia"

*2a etapa
*Realização de mamografia 
gen mamo = cond(((iddmamo != .) & (Q81 == 1)), 1 , 0)
replace mamo =. if iddmamo ==.
la val mamo sim_nao
la var mamo "Mamografia"
tab mamo

*última etapa
*Realização de mamografia nos últimos dois anos 
gen mamodois = cond(((iddmamo != .) & (Q82 == 1 | Q82 == 2 )), 1, 0)
replace mamodois= . if iddmamo ==.
la val mamodois sim_nao
la var mamodois "Mamografia nos últimos dois anos"
tab mamodois

**Realização de exame de citologia oncótica - papanicolau

*Criar variável com categorias de faixa etária
*Faixas de idade alvo
gen Q6_t = Q6
recode Q6 (25/34 = 1) (35/44 = 2) (45/54 = 3) (55/59 = 4) if (Q7 == 2) & (Q6 >= 25 & Q6 <= 59), gen(iddpapa_old)
recode Q6 (25/34 = 1) (35/44 = 2) (45/54 = 3) (55/64 = 4) if (Q7 == 2) & (Q6 >= 25 & Q6 <= 64), gen(iddpapa)
replace Q6 = Q6_t
drop Q6_t
la var iddpapa "Faixas de idade alvo para papanicolau"

*Realização de exame de citologia oncótica - papanicolau, alguma vez na vida
*gen papa = cond(Q79a == 1, 1, 0)
gen papa = cond(((iddpapa != .) & (Q79a == 1)), 1, 0)
replace papa = . if iddpapa ==.
la val papa sim_nao
la var papa "Papanicolau"
tab papa

*Realização de exame de citologia oncótica - papanicolau - nos últimos três anos
*gen papatres = cond(Q80 >= 1 & Q80 <= 3, 1, 0)
gen papatres = cond(((Q80 >= 1 & Q80 <= 3) & (iddpapa!= .)), 1, 0)
replace papatres = . if iddpapa ==.
la val papatres sim_nao
la var papatres "Papanicolau nos últimos três anos"
tab papatres

*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
*********************************************************************************************************************************************************************
***** Morbidade referida

** Hipertensão arterial
gen hart = cond((Q75 == 1), 1, 0)
la val hart sim_nao
la var hart "Hipertensao arterial"
tab hart

** Diabetes 
gen diab = cond((Q76 == 1), 1, 0)
la val diab sim_nao
la var diab "Diabetes"
tab diab


****************************************************************************************************************
*****INDICADORES DE TRATAMENTO MEDICAMENTOSO

*****Hipertensos em tratamento medicamentoso

*Criação do indicador em etapas

*1ª etapa
*Criar variável dicotômica com quem refere diagnóstico médico de Hipertensão Arterial
gen has = cond((Q75 == 1), 1, 0)
la val has sim_nao
la var has "Hipertensao arterial"
tab has

*2ª etapa
*Criar variável dicotômica com quem tem indicação médica para uso de medicamentos para tratamento 
gen ind_med_has = cond((R203 == 1), 1, 0)
replace ind_med_has = . if has == 0
la val ind_med_has sim_nao
la var ind_med_has "Indicacao medica"
tab ind_med_has

*3ª etapa 
*Criar variável dicotômica com quem está em uso do medicamento para controlar a pressão alta na época da entrevista
gen med_has = cond((R129 == 1), 1, 0)
replace med_has = . if has == 0
la val med_has sim_nao
la var med_has "Uso de medicamento"
tab med_has

*4ª etapa
*Percentual de hipertensos em tratamento medicamentoso 
*(número de indivíduos que referiram diagnóstico médico de Hipertensão Arterial e indicação médica para uso de medicamentos para tratamento 
*e estar em uso do medicamento para controlar a pressão alta na época da entrevista ÷ número de adultos entrevistados que referiram diagnóstico médico de hipertensão arterial (Q75==1) x 100)
gen trat_med_has = 0
replace trat_med_has = 1 if ((has == 1) & (ind_med_has == 1) & (med_has == 1))
replace trat_med_has = . if has == 0
la var trat_med_has "Hipertensos em tratamento medicamentoso"
tab trat_med_has


drop has

*****Diabéticos em tratamento medicamentoso

*Criação do indicador em etapas

*1ª etapa
*Criar variável dicotômica com quem refere diagnóstico médico de Diabetes
gen db = cond((Q76 == 1), 1, 0)
la val db sim_nao
la var db "Diabetes"
tab db

*2ª etapa
*Criar variável dicotômica com quem tem indicação médica para uso de medicamentos para tratamento 
gen ind_med_db = cond((R204 == 1), 1, 0)
replace ind_med_db = . if db == 0
la val ind_med_db sim_nao
la var ind_med_db "Indicacao medica"
tab ind_med_db

*3ª etapa 
*Criar variável dicotômica com quem está em uso do medicamento oral e/ou insulina na época da entrevista
gen med_db = cond((R133a == 1), 1, 0)
replace med_db = . if db == 0
la val med_db sim_nao
la var med_db "Uso de medicamento oral"
tab med_db

gen insulina = cond((R133b == 1), 1, 0)
replace insulina = . if db == 0
la val insulina sim_nao
la var insulina "Uso de insulina"
tab insulina

*4ª etapa
*Percentual de diabéticos em tratamento medicamentoso
*(número de indivíduos adultos que referiram diagnóstico médico de Diabetes e  indicação médica para uso de medicamentos para tratamento 
*e estar em tratamento medicamentoso com medicamento oral e/ou insulina na época na entrevista ÷ número de adultos entrevistados que referiram diagnóstico médico de diabetes (Q76==1) x 100)
gen trat_med_db = 0
replace trat_med_db = 1 if ((db == 1) & (ind_med_db == 1)) & ((med_db == 1 | insulina == 1))
replace trat_med_db = . if db == 0
la var trat_med_db "Diabeticos em tratamento medicamentoso"
tab trat_med_db



recode trat_med_db .= 0 if R133C ~= .
gen RRAS = .
recode RRAS .=1 if REGIAO == "RRAS01"
recode RRAS .=2 if REGIAO == "RRAS02"
recode RRAS .=3 if REGIAO == "RRAS03"
recode RRAS .=4 if REGIAO == "RRAS04"
recode RRAS .=5 if REGIAO == "RRAS05"
recode RRAS .=6 if REGIAO == "RRAS06"
recode RRAS .=7 if REGIAO == "RRAS07"
recode RRAS .=8 if REGIAO == "RRAS08"
recode RRAS .=9 if REGIAO == "RRAS09"
recode RRAS .=10 if REGIAO == "RRAS10"
recode RRAS .=11 if REGIAO == "RRAS11"
recode RRAS .=12 if REGIAO == "RRAS12"
recode RRAS .=13 if REGIAO == "RRAS13"
recode RRAS .=14 if REGIAO == "RRAS14"
recode RRAS .=15 if REGIAO == "RRAS15"
recode RRAS .=16 if REGIAO == "RRAS16"
recode RRAS .=17 if REGIAO == "RRAS17"
la var RRAS "RRAS"

gen tem_plano = 1 if Q88 == 1 | Q88 == 2
recode tem_plano .=0
la val tem_plano sim_nao
la var tem_plano "Posse de plano de saúde"
tab tem_plano

drop pinterno sexofxesc ttsexofxesc ttfet



save "D:\Users\regin\VIGITEL\SP\dados\2021\BD_Vigitel-SP_16-12-2021_pesorake_indicadores.dta"


* Merge dominio
import excel "D:\Users\regin\VIGITEL\SP\dados\2021\2-Tabela_Dominio.xlsx", sheet("dominio") firstrow clear
merge 1:m CIDADE using "D:\Users\regin\VIGITEL\SP\dados\2021\BD_Vigitel-SP_16-12-2021_pesorake_indicadores.dta"
label define dominio 1 "Sao Paulo" 2 "Gde SP" 3 "Interior"
label values DOMINIO dominio
drop _merge

save "D:\Users\regin\VIGITEL\SP\dados\2021\BD_Vigitel-SP_16-12-2021_pesorake_indicadores.dta", replace


* Criar os indicadores de diabetes
use "D:\Users\regin\VIGITEL\SP\dados\2021\BD_Vigitel-SP_16-12-2021_pesorake_indicadores.dta", clear

gen fet_diab = Q6
recode fet_diab 18/44=1 45/54 =2 55/64=3 65/max =4
la var fet_diab "Faixa etária diabeticos"
label define fet4 1 "18 a 44" 2 "45 a 54" 3 "55 a 64" 4 "65+"
label values fet_diab fet4

* indicador 2 de diabetes
gen ass_medida_db = 1 if R133C == 1
recode ass_medida_db .= 0 if diab==1
la var ass_medida_db "Diabeticos com assistência médica há menos de 1 ano"

* indicador 3 de diabetes
gen cs_medica_db = 1 if R133D == 1
recode cs_medica_db .= 0 if diab == 1 
la var cs_medica_db "Diabeticos atendidos pelo mesmo médico de consultas anteriore"

* indicador 4 de diabetes
gen hemoglobina_db = 1 if R133E == 1 
recode hemoglobina_db .= 0 if diab == 1 
la var hemoglobina_db "Diabeticos com exame de hemoglobina no último ano"


* indicador 5 de diabetes
gen exame_vista_db = 1 if R133F == 1 
recode exame_vista_db .= 0 if diab == 1 
la var exame_vista_db "Diabeticos com exame de vista ou fundo de olho há menos de 1 ano"

* indicador 6 de diabetes
gen exame_pes_db = 1 if R133G == 1 
recode exame_pes_db .= 0 if diab == 1 
la var exame_pes_db "Diabeticos com exame de pés há menos de 1 ano"

* indicador 7 de diabetes
gen presc_med_db = 1 if R204 == 1
recode presc_med_db .= 0 if diab == 1
la var presc_med_db "Diabeticos com prescrição de algum medicamento"

* indicador 8 de diabetes
gen uso1_medic_db = 1 if R133a == 1 
recode uso1_medic_db .= 0 if diab == 1 
la var uso1_medic_db "Diabetico faz uso atual de medicamento oral"

* indicador 9 de diabetes
gen uso2_medic_db = 1 if R133H == 1 
recode uso2_medic_db .= 0 if R133a == 1 
la var uso2_medic_db "Diabeticos tomaram todos os comprimidos nas duas últimas semanas"

* indicador 10 de diabetes
gen uso2_alguns_compr_db = 1 if R133H == 2 
recode uso2_alguns_compr_db .= 0 if R133a == 1 
la var uso2_alguns_compr_db "Diabeticos tomaram alguns dos comprimidos nas duas últimas semanas"

* indicador 11 de diabetes
gen uso2_nenhum_compr_db = 1 if R133H == 3
recode uso2_nenhum_compr_db .= 0 if R133a == 1 
la var uso2_nenhum_compr_db "Diabeticos não tomaram nenhum dos comprimidos nas duas últimas semanas"

* indicador 12 de diabetes
gen uso_insulina_db = 1 if R133b == 1  
recode uso_insulina_db .= 0 if diab == 1 
la var uso_insulina_db "Diabeticos com prescicao medica de insulina"

* indicador 13 de diabetes
gen uso2sem_insulina_db = 1 if R133I == 1 
recode uso2sem_insulina_db .= 0 if R133b == 1
la var uso2sem_insulina_db "Diabeticos com uso de insulina nas duas últimas semanas"


* indicador 14 de diabetes
gen uso_med_insu_db = 1 if R133H == 1 & R133I == 1
recode uso_med_insu_db .= 0 if R133a == 1 & R133b == 1
la var uso_med_insu_db "Diabeticos com medicamentos e insulina nas duas últimas semanas"

label values ass_medida_db sim_nao 
label values cs_medida_db sim_nao
label values hemoglobina_db sim_nao
label values exame_vista_db sim_nao
label values exame_pes_db sim_nao
label values uso_insulina_db sim_nao
label values uso2sem_insulina_db sim_nao
label values uso1_medic_db sim_nao
label values uso_med_insu_db sim_nao
label values presc_med_db sim_nao

label values DOMINIO dominio

save "D:\Users\regin\VIGITEL\SP\dados\2021\BD_Vigitel-SP_16-12-2021_pesorake_indicadores_20220923.dta", replace


use "D:\Users\regin\VIGITEL\SP\dados\2021\BD_Vigitel-SP_16-12-2021_pesorake_indicadores_20220923.dta", clear

svy linearized : mean diab
svy linearized : mean diab , over(Q7)
svy linearized : diab , over(fesc)
svy linearized : diab , over(fet_diab)
svy linearized : diab , over(DOMINIO)


svy linearized, subpop(diab) : mean ass_medida_db
svy linearized, subpop(diab) : mean ass_medida_db , over(Q7)
svy linearized, subpop(diab) : mean ass_medida_db , over(fesc)
svy linearized, subpop(diab) : mean ass_medida_db , over(fet_diab)
svy linearized, subpop(diab) : mean ass_medida_db , over(DOMINIO)

svy linearized, subpop(diab) : mean cs_medida_db
svy linearized, subpop(diab) : mean cs_medida_db , over(Q7)
svy linearized, subpop(diab) : mean cs_medida_db , over(fesc)
svy linearized, subpop(diab) : mean cs_medida_db , over(fet_diab)
svy linearized, subpop(diab) : mean cs_medida_db , over(RRAS)


svy linearized, subpop(diab) : mean hemoglobina_db
svy linearized, subpop(diab) : mean hemoglobina_db , over(Q7)
svy linearized, subpop(diab) : mean hemoglobina_db , over(fesc)
svy linearized, subpop(diab) : mean hemoglobina_db , over(fet_diab)
svy linearized, subpop(diab) : mean hemoglobina_db , over(RRAS)



svy linearized, subpop(diab) : mean exame_vista_db
svy linearized, subpop(diab) : mean exame_vista_db , over(Q7)
svy linearized, subpop(diab) : mean exame_vista_db , over(fesc)
svy linearized, subpop(diab) : mean exame_vista_db , over(fet_diab)
svy linearized, subpop(diab) : mean exame_vista_db , over(RRAS)


svy linearized, subpop(diab) : mean exame_pes_db
svy linearized, subpop(diab) : mean exame_pes_db , over(Q7)
svy linearized, subpop(diab) : mean exame_pes_db , over(fesc)
svy linearized, subpop(diab) : mean exame_pes_db , over(fet_diab)
svy linearized, subpop(diab) : mean exame_pes_db , over(RRAS)



svy linearized, subpop(diab) : mean uso_insulina_db
svy linearized, subpop(diab) : mean uso_insulina_db , over(Q7)
svy linearized, subpop(diab) : mean uso_insulina_db , over(fesc)
svy linearized, subpop(diab) : mean uso_insulina_db , over(fet_diab)
svy linearized, subpop(diab) : mean uso_insulina_db , over(RRAS)


svy linearized, subpop(diab) : mean  uso1_medic_db
svy linearized, subpop(diab) : mean  uso1_medic_db , over(Q7)
svy linearized, subpop(diab) : mean  uso1_medic_db , over(fesc)
svy linearized, subpop(diab) : mean  uso1_medic_db , over(fet_diab)
svy linearized, subpop(diab) : mean  uso1_medic_db , over(RRAS)


