//Statistical analysis for procactive intercentions
//The current version needs to be segmented, that is, follow the steps mentioned below, select the corresponding paragraph and run (Ctrl+D). The one-step running version will be updated later.

global path "C:\Users\10305" //This needs to be changed to the path where you downloaded the file.

global path path_data "$path\Data"
global path path_code "$path\Code"
global path path_out "$path\Results"

cd $path_data //change the working directory
use "$path\Data\Proactive-20220828-final"

/*
The main steps of the analysis:
1. Data normalization and interaction terms generated. Each scoring variable is standardized by Z-score. Based on the standardized variable, meaningful interaction terms are generated.
2. Stepwise regression. According to the classification of proactive intervention (simulation/empirical), and using the scores for effectiveness, feasibility, and generalizability as dependent variables, while using the evaluation indicators of "methods" and "pathways" and their interaction terms as independent variables, stepwise regression was performed to determine factors significantly affecting the effective/feasible/generalizable under different P values(0.01/0.05/0.10).
3. Combined analysis. Based on the stepwise regression of model simulation, the indicators of field research were included one by one, and multiple linear regression was performed. Step by step, until R² no longer increases, or there are no new significant variables.
4. Influence of geographic factors. Based on the results of the pooled analysis, explore the impact of other variables (geography, duration of intervention, etc.) on the effect.
*/

//1.1 Z-score standardization
foreach var in Inv_Time C_1 C_2 s1 s2 s3 C_3 s4 s5 s6 s7 s8 s9 e1_7 e2_7 e3_7{
	egen float Z_`var' = std(`var'), mean(0) std(1)
}

{
//1.2 Interaction terms for proactive
	generate float Z_s2s3 = Z_s2*Z_s3
	generate float Z_C1C2 = Z_C_1*Z_C_2
	generate float Z_C1C3 = Z_C_1*Z_C_3
	generate float Z_C2C3 = Z_C_2*Z_C_3
	generate float Z_s2s5 = Z_s2*Z_s5
	generate float Z_s3s5 = Z_s3*Z_s5
	generate float Z_s2s3s5 = Z_s2*Z_s3*s5
	generate float Z_s4s6 = Z_s4*Z_s6
	generate float Z_s4s7 = Z_s4*Z_s7
	generate float Z_s6s7 = Z_s6*Z_s7
	generate float Z_s8s9 = Z_s8*Z_s9
	generate float Z_C3s9 = Z_C_3*Z_s9
	generate float Z_C3s8 = Z_C_3*Z_s8
	generate float Z_C1s8 = Z_C_1*Z_s8
	generate float Z_s1Time = Z_Inv_Time*Z_s1
	generate float Z_s3Time = Z_Inv_Time*Z_s3
}


cd $path_out

//Proactive-simulation-raw data-initial analysis
{
	stepwise, pr(0.2): regress e1_7 C_1 s1 s2 s3 s5 s8 s9 C1C2 C1C3 C2C3 s2s3 
		s2s5 s3s5 s2s3s5 C3s9 C3s8 C1s8 if c0 == 1
		//R2=0.47
}

//2.1 Proactive-【simulation】-standardization-stepwise regression
{
	stepwise, pr(0.1): regress Z_e1_7 Z_C_1 Z_C_2 Z_s1 Z_s2 Z_s3 Z_C_3 Z_s5 Z_s8 Z_Inv_Time ///
		Z_s9 Z_s2s3 Z_C1C2 Z_C1C3 Z_C2C3 Z_s2s5 Z_s3s5 Z_s2s3s5 Z_s8s9 Z_C3s9 Z_C3s8 ///
		Z_C1s8 if c0 == 1 //Collinearity items not removed
	stepwise, pr(0.2): regress Z_e1_7 Z_C_1 Z_C_2 Z_s1 Z_s2 Z_s3 Z_s5 Z_s8 Z_Inv_Time ///
		Z_s9 Z_s2s3 Z_C1C2 Z_C1C3 Z_C2C3 Z_s2s5 Z_s3s5 Z_s2s3s5 Z_s8s9 Z_s1Time ///
		Z_C1s8 if c0 == 1 //Collinear items removed R²=0.20
}


////2.2 Proactive-effects-simulation-stepwise regression-p=0.2/0.1/0.05
//This step is to find out the p-value corresponding to the appropriate exclusion condition
foreach p in 0.2 0.1 0.05{
	{
		stepwise, pr(`p'): regress e1_7 Z_C_1 Z_s1 Z_s2 Z_s3 Z_C_3 Z_s5 Z_s8 Z_s9 Z_Inv_Time Z_s2s3 Z_C1C2 Z_C1C3 Z_C2C3 Z_s2s5 Z_s3s5 Z_s2s3s5 Z_s8s9 Z_C1s8 Z_s1Time if c0 == 1
			outreg2 using Proactive-Stepwise-simulation-effects-p`p'.xls, replace pvalue ctitle(Effectiveness) 
		stepwise, pr(`p'): regress e2_7 Z_C_1 Z_s1 Z_s2 Z_s3 Z_C_3 Z_s5 Z_s8 Z_s9 Z_Inv_Time Z_s2s3 Z_C1C2 Z_C1C3 Z_C2C3 Z_s2s5 Z_s3s5 Z_s2s3s5 Z_s8s9 Z_C1s8 Z_s1Time if c0 == 1
			outreg2 using Proactive-Stepwise-simulation-effects-p`p'.xls, append pvalue ctitle(Feasibility)
		stepwise, pr(`p'): regress e3_7 Z_C_1 Z_s1 Z_s2 Z_s3 Z_C_3 Z_s5 Z_s8 Z_s9 Z_Inv_Time Z_s2s3 Z_C1C2 Z_C1C3 Z_C2C3 Z_s2s5 Z_s3s5 Z_s2s3s5 Z_s8s9 Z_C1s8 Z_s1Time if c0 == 1
			outreg2 using Proactive-Stepwise-simulation-effects-p`p'.xls, append pvalue ctitle(Generalizability)
	}
}
//After comparison, we chose the final 【P >0.1】 as the exclusion condition for stepwise regression. 

//2.3 Proactive-effect-【empirical】-stepwise-p<0.1
foreach p in 0.2 0.1 0.05{
	{
		stepwise, pr(`p'): regress e1_7 Z_C_1 Z_C_2 Z_s1 Z_C_3 Z_s4 Z_s6 Z_s7 Z_s8 Z_s9 Z_Inv_Time Z_C1C2 Z_C1C3 Z_C2C3 Z_s4s6 Z_s4s7 Z_s6s7 Z_s8s9 Z_C3s9 Z_C1s8 Z_s1Time if c0 == 2
			outreg2 using Proactive-Stepwise-empirical-effects-p`p'.xls, replace pvalue ctitle(Effectiveness) 
		stepwise, pr(`p'): regress e2_7 Z_C_1 Z_C_2 Z_s1 Z_C_3 Z_s4 Z_s6 Z_s7 Z_s8 Z_s9 Z_Inv_Time Z_C1C2 Z_C1C3 Z_C2C3 Z_s4s6 Z_s4s7 Z_s6s7 Z_s8s9 Z_C3s9 Z_C1s8 Z_s1Time if c0 == 2
			outreg2 using Proactive-Stepwise-empirical-effects-p`p'.xls, append pvalue ctitle(Feasibility)
		stepwise, pr(`p'): regress e3_7 Z_C_1 Z_C_2 Z_s1 Z_C_3 Z_s4 Z_s6 Z_s7 Z_s8 Z_s9 Z_Inv_Time Z_C1C2 Z_C1C3 Z_C2C3 Z_s4s6 Z_s4s7 Z_s6s7 Z_s8s9 Z_C3s9 Z_C1s8 Z_s1Time if c0 == 2
			outreg2 using Proactive-Stepwise-empirical-effects-p`p'.xls, append pvalue ctitle(Generalizability)
	}
}


//【Proactive-Combined analysis】
//3.1 All variables for both simulation and empirical studies are included in stepwise regression. p<0.1 (unified with the previous stepwise regression)
//This step corresponds to step (6) of the original text, method robustness.
//All indicator variables were included in the regression equation as independent variables, and compared with stepwise regression for reference. Variables with a p-value less than 0.05 obtained by the model were used as a reference for stepwise regression to avoid missing potential "key factors".
{
stepwise, pr(0.1): regress e1_7 Z_C_1 Z_C_2 Z_s1 Z_s2 Z_s3 Z_C_3 Z_s5 Z_s8 Z_s9 Z_Inv_Time Z_s2s3 Z_C1C2 Z_C1C3 Z_C2C3 Z_s2s5 Z_s3s5 Z_s2s3s5 Z_s8s9 Z_C3s9 Z_C1s8 Z_s1Time ///
	Z_s4 Z_s6 Z_s7 Z_s4s6 Z_s4s7 Z_s6s7 Z_s3Time
	outreg2 using Proactive-combined-all variables Stepwise.xls, replace pvalue ctitle(Effectiveness)
stepwise, pr(0.1): regress e2_7 Z_C_1 Z_C_2 Z_s1 Z_s2 Z_s3 Z_C_3 Z_s5 Z_s8 Z_s9 Z_Inv_Time Z_s2s3 Z_C1C2 Z_C1C3 Z_C2C3 Z_s2s5 Z_s3s5 Z_s2s3s5 Z_s8s9 Z_C3s9 Z_C1s8 Z_s1Time ///
	Z_s4 Z_s6 Z_s7 Z_s4s6 Z_s4s7 Z_s6s7 Z_s3Time
	outreg2 using Proactive-combined-all variables Stepwise.xls, append pvalue ctitle(Feasibility)
stepwise, pr(0.1): regress e3_7 Z_C_1 Z_C_2 Z_s1 Z_s2 Z_s3 Z_C_3 Z_s5 Z_s8 Z_s9 Z_Inv_Time Z_s2s3 Z_C1C2 Z_C1C3 Z_C2C3 Z_s2s5 Z_s3s5 Z_s2s3s5 Z_s8s9 Z_C3s9 Z_C1s8 Z_s1Time ///
	Z_s4 Z_s6 Z_s7 Z_s4s6 Z_s4s7 Z_s6s7 Z_s3Time
	outreg2 using Proactive-combined-all variables Stepwise.xls, append pvalue ctitle(Generalizability)
}//R2=0.468  regress e1_7 Z_s1 Z_s4 Z_s5 Z_Inv_Time Z_C_1 Z_s4s7 Z_s2s3 Z_s2s5 Z_s8
//regress e1_7 Z_s1 Z_s4 Z_s5 Z_Inv_Time Z_C_1 Z_s4s7 Z_s2s3 Z_s2s5 Z_s8
//R2=0.436, p<0.05. regress e1_7 Z_s1 Z_s4 Z_s5 Z_C_1 Z_s4s7 Z_s2s3

//3-2 Combined analysis
//Based on the model of Proactive-simulation-P<0.1
//Sequentially adding variables from field studies for multiple linear regression(MLR)
{

	regress e1_7 Z_s1 Z_s5 Z_s2s3 Z_C1C3
		outreg2 using Proactive-combined-sequential-MLR.xls, replace pvalue ctitle(base) 
	//【Add a variable】
	foreach var_con1 in Z_s4 Z_s6 Z_s7 Z_s4s6 Z_s4s7 Z_s6s7 c00 Z_Inv_Time Z_s1Time{
		regress e1_7 Z_s1 Z_s5 Z_s2s3 Z_C1C3 `var_con1'
			outreg2 using Proactive-combined-sequential-MLR.xls, append pvalue ctitle(model_1_`var_con1')	
	}
	
	//【Add two variables】①Z_s4 ②Z_s6 R² incread for 0.3 and 0.2，In the second step, based on these two situations, new variables are added in a further loop.
	{
		//two baseline models
		regress e1_7 Z_s1 Z_s5 Z_s2s3 Z_C1C3 Z_s4 
			outreg2 using Proactive-combined-sequential-MLR2.xls, replace pvalue ctitle(base1)
		regress e1_7  Z_s1 Z_s5 Z_s2s3 Z_C1C3 Z_s6
			outreg2 using Proactive-combined-sequential-MLR2.xls, append pvalue ctitle(base2)
		foreach var_con2 in Z_s4 Z_s6 Z_s7 Z_s4s6 Z_s4s7 Z_s6s7 c00 Z_Inv_Time Z_s1Time{
			if "`var_con2'" != "Z_s4"{
				regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 Z_s4 `var_con2'
					outreg2 using Proactive-combined-sequential-MLR2.xls, append pvalue ctitle(model_2_`var_con2')
					}
			if ("`var_con2'" != "Z_s4") & ("`var_con2'" != "Z_s6"){
				regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 Z_s6 `var_con2'
					outreg2 using Proactive-combined-sequential-MLR2.xls, append pvalue ctitle(model_2_`var_con2')
					}
			}
	}
	
	//【Add three variables】①Z_s4 Z_s4s7 	②Z_s4 Z_s7 	③Z_s4 Z_s6s7 
	{
		//three baseline models
		regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 Z_s4 Z_s4s7
			outreg2 using Proactive-combined-sequential-MLR3.xls, replace pvalue ctitle(base1)
		regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 Z_s4 Z_s7
			outreg2 using Proactive-combined-sequential-MLR3.xls, append pvalue ctitle(base2)
		regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 Z_s4 Z_s6s7
			outreg2 using Proactive-combined-sequential-MLR3.xls, append pvalue ctitle(base3)
		foreach var_con3 in Z_s4 Z_s6 Z_s7 Z_s4s6 Z_s4s7 Z_s6s7 c00 Z_Inv_Time Z_s1Time{
			if ("`var_con3'" != "Z_s4s6") & ("`var_con3'" != "Z_s6"){
				regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 ///
				Z_s4 Z_s4s7 `var_con3'
					outreg2 using Proactive-combined-sequential-MLR3.xls, append pvalue ctitle(model_3_`var_con3')
					}
			if ("`var_con3'" != "Z_s4s6") & ("`var_con3'" != "Z_s6")& ("`var_con3'" != "Z_s7"){
				regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 ///
				Z_s6 Z_s7 `var_con3'
					outreg2 using Proactive-combined-sequential-MLR3.xls, append pvalue ctitle(model_3_`var_con3')
					}
			if ("`var_con3'" != "Z_s4s6") & ("`var_con3'" != "Z_s6")& ("`var_con3'" != "Z_s7")& ("`var_con3'" != "Z_s4") &("`var_con3'" != "c00") {
				regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 ///
				Z_s4 Z_s4s7 `var_con3'
					outreg2 using Proactive-combined-sequential-MLR3.xls, append pvalue ctitle(model_3_`var_con3')
					}
				regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 ///
				Z_s4s6 c00 `var_con3'
					outreg2 using Proactive-combined-sequential-MLR3.xls, append pvalue ctitle(model_3_`var_con3')
					}
			}
	

	//【Add four variables】①Z_s4s6 Z_s6 Z_s4s7	②Z_s4 Z_s4s7 c00	③Z_s4 Z_s7 Z_s6s7
	{
		regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 Z_s4s6 Z_s6 Z_s4s7
			outreg2 using Proactive-combined-sequential-MLR4.xls, replace pvalue ctitle(base1)
		regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 Z_s4 Z_s4s7 c00 
			outreg2 using Proactive-combined-sequential-MLR4.xls, append pvalue ctitle(base2)
		regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 Z_s4 Z_s7 Z_s6s7
			outreg2 using Proactive-combined-sequential-MLR4.xls, append pvalue ctitle(base3)
		foreach var_con4 in Z_s4 Z_s6 Z_s7 Z_s4s6 Z_s4s7 Z_s6s7 c00{
			if ("`var_con4'" != "Z_s4s6") & ("`var_con4'" != "Z_s6") & ("`var_con4'" != "Z_s4s7"){
				regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 ///
				Z_s4s6 Z_s6 Z_s4s7 `var_con4'
					outreg2 using Proactive-combined-sequential-MLR4.xls, append pvalue ctitle(model_4_`var_con4')
					}
			if ("`var_con4'" != "Z_s4s6") & ("`var_con4'" != "Z_s6") & ("`var_con4'" != "Z_s4s7") & ("`var_con4'" != "Z_s4") & ("`var_con4'" != "c00"){
				regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 ///
				Z_s4 Z_s4s7 c00 `var_con4'
					outreg2 using Proactive-combined-sequential-MLR4.xls, append pvalue ctitle(model_4_`var_con4')
					}
			if ("`var_con4'" != "Z_s4s6") & ("`var_con4'" != "Z_s6") & ("`var_con4'" != "Z_s4s7") & ("`var_con4'" != "Z_s4") & ("`var_con4'" != "c00") & ("`var_con4'" != "Z_s6s7")& ("`var_con4'" != "Z_s7"){
				regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 ///
				Z_s4 Z_s7 Z_s6s7 `var_con4'
					outreg2 using Proactive-combined-sequential-MLR4.xls, append pvalue ctitle(model_4_`var_con4')
					}
		}
		//No more new significant variables，R²=0.4
	}
}

//Final model：regress e1_7 Z_s1 Z_s8s9 Z_s3 Z_C3s9 Z_C1C2 Z_C1C3 c00 Z_s4 Z_s4s7


//4.1 Influence of geographic factors.
//Standardization of geographic variables
	egen double Z_GDP2020 = std(GDP2020), mean(0) std(1)
	egen float Z_Compulsory_education_year = std(Compulsory_education_year), mean(0) std(1)
	egen double Z_education_expenditure_2019 = std(education_expenditure_2019), mean(0) std(1)

	//Based on the simulation and stepwise regression combined results, the influence of geographical variables was analyzed.
	{
	regress e1_7 Z_s1 Z_s4 Z_s5 Z_s2s3 Z_C1C3
			outreg2 using Proactive-geographic.xls, replace pvalue ctitle(base)
	regress e1_7 Z_s1 Z_s4 Z_s5 Z_s2s3 Z_C1C3 Z_GDP2020
			outreg2 using Proactive-geographic.xls, append pvalue ctitle(model_1_GDP)
	regress e1_7 Z_s1 Z_s4 Z_s5 Z_s2s3 Z_C1C3 Z_Compulsory_education_year
			outreg2 using Proactive-geographic.xls, append pvalue ctitle(model_2_cop-edu-year)
	regress e1_7 Z_s1 Z_s4 Z_s5 Z_s2s3 Z_C1C3 Z_education_expenditure_2019
			outreg2 using Proactive-geographic.xls, append pvalue ctitle(model_3_edu-expenditure)
	regress e1_7 Z_s1 Z_s4 Z_s5 Z_s2s3 Z_C1C3 Z_GDP2020 Z_Compulsory_education_year
			outreg2 using Proactive-geographic.xls, append pvalue ctitle(model_4_GDP+edu)
	regress e1_7 Z_s1 Z_s4 Z_s5 Z_s2s3 Z_C1C3 Z_GDP2020 Z_education_expenditure_2019
			outreg2 using Proactive-geographic.xls, append pvalue ctitle(model_5_GDP+edu-expenditure)
	regress e1_7 Z_s1 Z_s4 Z_s5 Z_s2s3 Z_C1C3 Z_Compulsory_education_year Z_education_expenditure_2019
			outreg2 using Proactive-geographic.xls, append pvalue ctitle(model_6_edu+edu-expenditure)
	regress e1_7 Z_s1 Z_s4 Z_s5 Z_s2s3 Z_C1C3 Z_GDP2020 Z_Compulsory_education_year Z_education_expenditure_2019
			outreg2 using Proactive-geographic.xls, append pvalue ctitle(model_7_GDP+edu+edu-expenditure)
	}

//descriptive statistics
mean e1_7 e2_7 e3_7
	outreg2 using Proactive-effects-descriptive statistics.xls, replace noaster
mean e1_7 e2_7 e3_7, over(c01)
	outreg2 using Proactive-effects-descriptive statistics.xls, append noaster

//Need to transpose
bysort c01:outreg2 using Proactive-effects-descriptive.xls, replace sum(log) keep(e1_7 e2_7 e3_7) eqkeep(N mean sd) label
