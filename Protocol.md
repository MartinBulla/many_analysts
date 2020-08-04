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
    - distribution of hatching dates and breed sizes between treatment categories (to check whether we need to control for this)

# key dependent variables
day_14_tarsus_length    Length of chick tarsometatarus in mm at day 14 after hatching
day_14_weight   mass of chick in grams at day 14 after hatching


chick_survival_to_first_breed_season    Indicates whether the chick was documented as attempting to breed on the study site in any subsequent year

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


# key model
dependent ~ net_rearing_manipulation * rear_Cs_at_start_of_rearing + 
            net_rearing_manipulation * d14_rear_nest_brood_size +
            chick_sex_molec + 
            (1|Date_of_day14) + (1|day14_measurer) + 
            (1|rear_nest_breed_ID) + (1|hatch_nest_breed_ID) + 
            OR 
            (1|hatch_mom_Ring) + (1|hatch_nest_dad_Ring) + (1|genetic_dad_ring_)

dependent ~ net_rearing_manipulation * rear_Cs_at_start_of_rearing * chick_sex_molec + 
            net_rearing_manipulation * d14_rear_nest_brood_size * chick_sex_molec + 

# additional exploration - whether own/foreign/female_only (ExtraPair) chicks grew differently
