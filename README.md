# GOM_TRAIT_PHENOLOGY
The analysis of GOM Fish Trait and Phenology 
JOURNAL PUBLICATION CITATION: 
Authors:  
Masami Fujiwara, Fiala Emiko-Mae Bumpers, Milton Torres Ceron, Fernando Martinez-Andrade
Manuscript title:
Demographic traits and phenology of coastal fishes experiencing tropicalization under climate change
Journal name: 
Ecology
________________________________________
Data S1

GAM analysis and subsequent model selection and analysis to compare patterns with fish traits
________________________________________
Author of the material provided in DataS1.zip
Author 1: Masami Fujiwara
Affiliation: Department of Ecology and Conservation Biology, Texas A&M University 
Address: 534 John Kimbrough Blvd, College Station, TX 77843-2258, USA
Email: fujiwara@tamu.edu 
________________________________________
File list (files found within DataS1.zip)

a_GAM_ANALYSIS.R

b_extractExplanatoryVars.R

b_scaling_factor.R

c_ModelSelection.R

d_comparisons.R

e_plotpatternfigures.R

f_plotFigurebySpeciesv.R


The following data will be available upon request.

DATA.Rdata

Demography.Rdata

ExpVars.rdata

SCALE_FACTORS_S.Rdata

STATION.Rdata

TPWD.Rdata


Description

a_GAM_ANALYSIS.R

•	GAM is fit to presence-absence data using binomial model.
•	Environmental variables are the residuals after fitting a model with smooth trends of year and season.
•	Four covariates (temperature, salinity, DO, SL) are fitted one at a time.
•	Including the no-covariate model, there are ten models for each species.
•	The best model for each species is selected with "c_ModelSelection.R".
•	Input data are
o	DATA.Rdata
o	STATION.Rdata
o	TPWD.Rdata
•	Outputs are saved in 
o	ResultsGAM.Rdata


b_extractExplanatoryVars.R

•	Calculate the scaled explanatory variables.
•	These were calculated in a_GAM_ANALYSIS.R, but I forgot to save them in the script.
•	I just copied a part of the script and pasted here and saved the explanatory variables.  

b_scaling_factor.R

•	Calculate the scaling factors used for scaling explanatory variables.
•	These were calculated in a_GAM_ANALYSIS.R, but I forgot to save them in the script.
•	I just copied a part of the script and pasted here and saved the scaling factors.  

c_ModelSelection.R

•	The best model for each species is selected based on AIC.
•	Take the results from GAM_ANALYSIS.R (ResultsGAM.Rdata) and saved in "Seine_Best.Rdata".

d_comparisons.R

•	Conduct a cluster analysis on the yearly and seasonal trends. 
•	Compare the yearly and seasonal trend patterns and demographic characteristics of fishes
•	Take the information in "Demography.Rdata" (fish trait data).
•	Take the information in "Seine_Best.Rdata" from "c_ModelSelection.R".
•	Results are saved in 'clusters.Rdata'
•	Figures produced are Figures 4 and 5. 

e_plotpatternfigures.R

•	Take the information in “cluster.Rdata” and “SCALE_FACTORS_S.Rdata”.
•	Plot figures of patterns Figures 2 and 3. 

f_plotFigurebySpeciesv.R

•	Create Appendix S4. 

DATA.Rdata

 [1] "TPWD_ID": Texas Parks and Wildlife Department fish species ID number    
 [2] "station_id": Texas Parks and Wildlife Department station ID number (unique for each sampling occasion)   
 [3] "major_area": ID for bays 1: Sabine Lake, 2: Galveston Bay, 3: Matagorda Bay, 4: San Antonio Bay, 5: Aransas Bay, 6: Corpus Christi Bay, 7: Upper Laguna Madre, and 8 Lower Laguna Madre 
 [4] "year": Year of sampling       
 [5] "month": Month of sampling      
 [6] "cpue": Catch per unit effort       
 [7] "catch": Number of individuals caught      
 [8] "mean_length": Mean length of selected individuals (not all individuals)
 [9] "day": Day of sampling        
[10] "season": Season (1-24) of sampling (see the main text)      
[11] "sci_name": Scientific name of the species   

Demography.Rdata

•	Fish trait date for 275 species of fishes found in the TPWD bag seine data. 
•	Each list item is for each species as described below:

Information from FishLife R Package.

 [1] "Amax": Maximum age from FishLife R package       
 [2] "Amat": Age of maturity from FishLife R package              
 [3] "Linf": Asymptotic length from FishLife R package       
 [4] "Kapp": von Bertalanffy growth parameter from FishLife R package       
 [5] "Lmat": Length of maturity from FishLife R package       
 [6] "Lamb": The maximum asymptotic population growth rate from FishLife R package   

Information from Texas Parks and Wildlife Department data           

 [7] "common_name": Common name of fish species
 [8] "TPWD_ID": Texas Parks and Wildlife Department fish species ID number        
 [9] "seine": How frequently the species was caught in bag seine.      
[10] "trawl": How frequently the species was caught in bay trawl.      
[11] "gillnet": How frequently the species was caught in bay gill net.          

Information from FishBase

[12] "FB_SPECI": Species information downloaded from FishBase.    
[13] "FB_ESTIM": Estimated trait information downloaded from FishBase.   
[14] "FB_FECUN": Fecundity data downloaded from FishBase.   
[15] "FB_ECOLO": Ecological information downloaded from FishBase.   
[16] "FB_MATUR": Maturity information downloaded from FishBase.    
[17] "FB_FOODI": Food item information downloaded from FishBase.   
[18] "FB_REPRO": Information on reproduction downloaded from FishBase.   
[19] "FB_LENGT": Length information downloaded from FishBase.    
[20] "sci_name": Scientific name.    

Derived parameters

[21] "a_lw": Allometric coefficient for length weight relationship.      
[22] "b_lw": Allometric coefficient (exponent) for length weight relationship.        
[23] "M_u": Mortality at 1 g under Lorenzen (1996) model.         
[24] "B_L": Lorenzen (1996) exponent for the weight-based mortality model.         
[25] "A0": estimated von Bertalanffy growth model parameter (hypothetical age at size 0).         
[26] "Age": Age from 1 to the maximum age of the species.         
[27] "L": Length at given age.          
[28] "W": Weight at given age.          
[29] "M": Instantaneous mortality age given age.          
[30] "S": Finite per-capita survival rate.           
[31] "A1": Population matrix when the asymptotic population growth rate was 1.          
[32] "A2": Population matrix when the asymptotic population growth rate was “Lamb”         
[33] "GT1": Generation time estimated using population matrix A1.        
[34] "NR1"  The expected number of years in mature stage calculated with A1.      
[35] "DR1"  Damping ratio estimated from population matrix A1.       
[36] "GT2": Generation time estimated using population matrix A2.         
[37] "NR2": The expected number of years in mature stage calculated with A2.              
[38] "DR2": Damping ratio estimated from population matrix A2.       

ExpVars.rdata
	
The environmental covariates scaled so that the mean is 0 and standard deviation is 1. 

SCALE_FACTORS_S.Rdata

The mean and standard deviation of environmental variables. 

STATION.Rdata

•	Station specific information.

 [1] "major_area": ID for bays 1: Sabine Lake, 2: Galveston Bay, 3: Matagorda Bay, 4: San Antonio Bay, 5: Aransas Bay, 6: Corpus Christi Bay, 7: Upper Laguna Madre, and 8 Lower Laguna Madre 
 [2] "year": year of sampling       
 [3] "month": month of sampling      
 [4] "station_id": Texas Parks and Wildlife Department station ID number (unique for each sampling occasion)   
 [5] "temperature": temperature
 [6] "salinity": salinity   
 [7] "diss_oxygen": dissolved oxygen
 [8] "turbidity": turbidity  
 [9] "latitude": latitude   
[10] "longitude": longitude  
[11] "depth": depth      
[12] "msl": mean sea level        
[13] "day": day of sampling        
[14] "season": season (1-24) of sampling (see the main text)           
[15] "date": date of sampling       
[16] "DoY": day of the year of sampling

TPWD.Rdata

•	Species specific information from the original data. 

[1] "common_name": Common name
[2] "sci_name": Scientific name   
[3] "TPWD_ID": Texas Parks and Wildlife Department fish species ID number   
[4] "seine":  How frequently the species was caught in bag seine.          
[5] "trawl":  How frequently the species was caught in bay trawl.          
[6] "gillnet":  How frequently the species was caught in gill net.       
