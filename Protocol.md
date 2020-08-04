### PROCEDURE
1. Read the dataset description and metadata
1. Decided upon the following analyses and procedures
1. Explored the data distributions and correlations
1. Visualized the raw data
1. Fitted the models, tested model assumptions and adjusted the models accordingly
1. Visualized the model output


## Key question: "To what extent is the growth of nestling blue tits (Cyanistes caeruleus) influenced by competition with siblings?" (Understanding how the growth of nestlings is influenced by the numbers of siblings in the nest can give researchers insights into factors such as the evolution of clutch size, determination of provisioning rates by parents, and optimal levels of sibling competition.)

## 2 A priory analyses

# exploration 
    - distribution of hatching dates and brood sizes between treatment categories (to check whether we need to control for this)
    - correlation between dependent variables
    - correlations between controls
    - correlations between explanatory
    - distributions of the variables 
    - rear_Cs_at_start_of_rearing ~ d14_rear_nest_brood_size 

# Decisions based on exploration 
    - >0.6 corr between any combination of continuous fixed effects, we check how model results change when one taken out at a time
    - distribution of hatching dates and brood sizes between treatment categories (to check whether we need to control for this) - if well matched (90%) - try paired analyses or no need to control for differences between rear and hatch nest; if not control the models for age differences between rear and hatch date
    - is it possible to pair the treatments with control (to construct enlarged/reduced/unmanipulated "pairs", if not we controls will not be paired within analyses
    - visualize raw data to check for inconsistencies and decide whether (e.g. if brood larger at day 14, the data are excluded; if >33% missing data in an explanatory variable (we run a reduced model, but drop the variable from the global model), we do not use it; if <=33% missing data, we impute according to Nakagawa; check outlier statistics (e.g. boxplot) for tarsus and mass to check whether all are within real scale of blue tit size)

  # Decisions based on model outcomes  
    - if model assumptions not met, we adjust the models accordingly (e.g. log-transformation)
    - if year explains a lot of variation we also run models/plot data for each year separately
  
  Visualise 
    - triad (if possible), else pairs  

# key dependent variables
    chicks in number of chicks ~ 
    day_14_body_mass_index
    day_14_tarsus_length   
    day_14_weight  


# key explanatory variable
    - net_rearing_manipulation
    - rear_nest_CS - The size of the clutch (number of eggs) in the nest in which the chick was reared
    - rear_Cs_at_start_of_rearing   Number of chicks in nest of rearing immediately following experimental chick removals and additions
    - d14_rear_nest_brood_size    "In the nest where the chick was reared, the number of live chicks in the nest at day 14 after hatching"
    
    number_chicks_fledged_from_rear_nest    "In the nest where the chick was reared, the number of chicks that survived to leave the nest"

# control for genetic and rearing environment 
    - done withing the experimental design and season
    - do we need more?
# control for season
    - Date_of_day14   date (1 = April 1) of 14th day after 

# day14_measurer  Code corresponding to the identity of the person who measured the chick at day 14.
# chick_sex_molec Sex of the chick based on molecular genetic analysis. 1 = female; 2 = male.  Not all chicks were sexed.

- control for year or analyze year separately
- control for brood size and chicks moved
- control for hatch_nest_breed_ID or rear_nest_breed_ID or pair_ID 
- control for hatch_mom_Ring, hatch_nest_dad_Ring
- control for Extra-pair_paternity
- control fo genetic_dad_ring_
- control for hatch_Area and/or rear_area
- control for hatch_nest_OH
- controlo for d0_hatch_nest_brood_size and d14_hatch_nest_brood_size

rear_nest_trt   "Nest experimental manipulation treatment code for the nest in which the chick was reared. '5' indicates a net increase in the number of chicks (brood size increased), '6' indicates a net decrease in the number of chicks (brood size decreased), '7' indicates no manipulation (no chicks added or removed). "

net_rearing_manipulation    Net change in chick number in nest where chick reared

# splitting the data to small and large - separate models on the two

1) absolute change in chick numbers ~   treatment * chick#start + 
                                            sex ratio of brood (if doable)+ 
                                            (1|year) + (1|area)
                                            
2a) paired nests (reduced, enlarged, non_manipulated)  
2b) all nests

UPDATE WITH TRUE VARIABLES

dependent ~ treatment * chick#start +
            + treatment * chick#14day +
            sex + sex ratio of brood (if doable) + 

            2a(1|paired_nests_ID)2a +
            
            (1|day14_measurer) + (1|area)
             (1|rear_nest_OH_std) + (1|year) OR (1|year/reare_nest_OH) +
            (1|rear_nest_breed_ID)  + 
            (1|hatch_mom_Ring) + (1|genetic_dad_ring_)  # DROP IF MODEL DOESN'T CONVERGE


2ab - alternative
dependent ~ treatment * chick#start * sex +
            + treatment * chick#14day * sex +
            sex + sex ratio of brood (if doable) + 

            2a(1|paired_nests_ID)2a +
            
            (1|day14_measurer) + (1|area)
             (1|rear_nest_OH_std) + (1|year) OR (1|year/reare_nest_OH) +
            (1|rear_nest_breed_ID)  + 
            (1|hatch_mom_Ring) + (1|genetic_dad_ring_)  # DROP IF MODEL DOESN'T CONVERGE            


NO) mean/median chick size ~   treatment * chick#start + 
                              sex ratio of brood (if doable)+ 
                              (1|year) + (1|area)
# all nests



(1|rear_nest_OH_std) + (1|year) year/hatch


# year also separate

# key model
dependent ~ net_rearing_manipulation * rear_Cs_at_start_of_rearing + 
            net_rearing_manipulation * d14_rear_nest_brood_size +
            chick_sex_molec + 
            (1|rear_nest_OH) + (1|day14_measurer) + 
            (1|rear_nest_breed_ID) + (1|hatch_nest_breed_ID) + 
            OR 
            (1|hatch_mom_Ring) + (1|hatch_nest_dad_Ring) + (1|genetic_dad_ring_)

dependent ~ net_rearing_manipulation * rear_Cs_at_start_of_rearing * chick_sex_molec + 
            net_rearing_manipulation * d14_rear_nest_brood_size * chick_sex_molec + 

# additional exploration - whether own/foreign/female_only (ExtraPair) chicks grew differently
