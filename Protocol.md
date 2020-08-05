### A PRIORI DECIDED PROCEDURE

We have read the data description and metadata provided by the coordinators (https://osf.io/34fzc/) and *A PRIORI* decided upon the following analyses and procedures to answer:

##  Key question: "To what extent is the growth of nestling blue tits (Cyanistes caeruleus) influenced by competition with siblings?" 

## 1. DATA EXPLORATION and ANALYTICAL DECESION
    - distribution of hatching dates and brood sizes between treatment categories to see whether the design was balanced and thus controlling for environmental and social factors (like differences in hatch date) are not necessary; if well matched across treatments (90%) we will attempt analyses with nest pairs (reduced/increased) or triads (reduced/increase/unmanipulated), i.e. no need to control for differences between reared and hatched nest; if the match is <90% we will control the models for the age differences between the reared and hatch nests
    - is it possible to pair the treatments with control (to construct enlarged/reduced/unmanipulated "triads"; if not, controls will not be paired within analyses
    - if we find corr >0.6 between any combination of continuous fixed effects, we check how model results change when each variable is taken out of the model at a time
    - correlation between dependent variables to aid interpretation of the results
    - check distributions and visualize raw data to check for inconsistencies e.g.: 
            - if brood larger at day 14 than at hatch, the data are excluded; 
            - if >33% missing data in an explanatory variable, we run a reduced model, but drop the variable from the global model; if <=33% missing data, we impute the missing data according to Nakagawa;
            - check outlier statistics (e.g. boxplot) for tarsus and mass to check whether all are within real scale of blue tit size)

## MODELS  
    # BACKGROUND
        # data
            We believe that adding or taking four chicks from large broods (12-16 chicks) or from small broods (4-5 chicks) makes for two different experiments. Hence, we divided the dataset into two and analyzed those separately. 

        #  dependent variables
            change_chick_# = change in chick number from start of experiment till day 14 () ~ 

            day_14_body_mass = mass of chick in grams at day 14 after hatching
            day_14_tarsus_length = Length of chick tarsometatarus in mm at day 14 after hatching
            day_14_body_mass_index = similar to the human body mass index $$day\_14\_weight / (day\_14\_tarsus\_length^2)$$


         # key explanatory variable
            - net_rearing_manipulation = essential whether the nest was enlarged or not
            - rear_Cs_at_start_of_rearing   Number of chicks in nest of rearing immediately following experimental chick removals and additions
            - d14_rear_nest_brood_size    "In the nest where the chick was reared, the number of live chicks in the nest at day 14 after hatching"
            - chick_sex_molec = Sex of the chick based on molecular genetic analysis. 1 = female; 2 = male.  Not all chicks were sexed. HERE WE ASSIGN UNSEXED ONES AS U - unknown
            - brood_sex_ratio = sex ration of chicks in the brood (1 = all females, 0 = all males)

        # control for genetic and rearing environment 
            - experimental design controls for the rearing environment. To control for multiple data points per nest, we control for rearing nest. As female and male blue tits grow differently, we also control for sex ration in each nest and for sex of a chick.
            - we also attempted to control for genetic mother and father of each chick, as long as such models converge. 
           
        # control for season and measurer
            - Date_of_day14  date (1 = April 1) of 14th day after hatching
            - day14_measurer  Code corresponding to the identity of the person who measured the chick at day 14.
        

    #1) NO GROWTH - mortality
        change_chick_# ~    net_rearing_manipulation * rear_Cs_at_start_of_rearing + 
                            brood_sex_ration (only if doable)+ 
                            (1|hatch_year) + (1|rear_area)
                                            
    #2) SIZE given treatment
        a) paired/triad nests (reduced, enlarged, non_manipulated) 
        b) all nests individually



       m0 =  day_14_body_mass/tarsus_length/body_mass_index ~ 
            net_rearing_manipulation * rear_Cs_at_start_of_rearing +
            net_rearing_manipulation * d14_rear_nest_brood_size +
            sex (only if doable) + brood_sex_ration (only if doable) + 

            2a(1|paired/triad_nests_ID)2a +
            
            (1|day14_measurer) + (1|rear_area)
            
(1|rear_nest_OH_std) + (1|hatch_year)  +
            (1|rear_nest_breed_ID)
 + 
            (1|hatch_mom_Ring) + (1|genetic_dad_ring_)
 # DROP the last two if the model does not converge


       ms =  day_14_body_mass/tarsus_length/body_mass_index ~ 
            net_rearing_manipulation * rear_Cs_at_start_of_rearing * sex+
            net_rearing_manipulation * d14_rear_nest_brood_size * sex +
            sex (only if doable) + brood_sex_ration (only if doable) + 

            2a(1|paired/triad_nests_ID)2a +
            
            (1|day14_measurer) + (1|rear_area)
            
(1|rear_nest_OH_std) + (1|hatch_year)  +
            (1|rear_nest_breed_ID)
 + 
            (1|hatch_mom_Ring) + (1|genetic_dad_ring_)
 # DROP the last two if the model does not converge

    
        Check with AIC whether model with sex in interaction fits the data better (AICdelta > 5)

##  Decisions based on model outcomes  
    - if model assumptions not met, we adjust the models accordingly (e.g. log-transformation of variables or fitting additional variable visible in the residuals)
    - if year explains a lot of variation we also run models/plot data for each year separately

## Reporting results
    - all tested model outputs will be in tables and visualized in a figure 
    - visualizations of raw data (if possible also in pairs/triads