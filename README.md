## Hemp Metadata and Field Notes 
Dates and season of each hemp trial conducted at TREC, Homestead, FL.
- `PilotPlot1`; May 1st, 2019 (`Summer`)
- `PilotPlot2`; June 21st, 2019 (`Summer`)
- `VarietyTrial1`; May 22nd, 2019 (`Summer`)

`PilotPlot1_summer_2019_Emergence.csv`: Emergence data of the first pilot plot.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Row number in the measurement zone of each experimental unit (`1`: left; `2`: right).
- `Emergence`: Number of plants that have emerged from the soil as a result of planting (Planting date: May 1st, 2019).
- Notes:   - Blanks due to missing data
   - Due to the CBD planting design, numbers in the following experimental units represent the total amounts of CBD plants in it. No measurement zone was established for them unlike the other varieties.
	- Block 1: Row 1 - Column D, Row 2 - Column A
	- Block 2: Row 3 - Column D, Row 4 - Column D
	- Block 3: Row 2 - Column A, and C
	- Block 4: Row 3 - Column A, Row 4 - Column C

`PilotPlot1_summer_2019_StandCount.csv`: Population quantification of the first pilot plot.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `StandCount`: Total amount of plants in experimental unit.
- Notes:   - Blanks due to missing data
	- Block 3 - Row 2 - Column A: Plants did not emerge.
	- Block 3 - Row 2 - Column C: Experimental unit was mixed with adjacent variety.

`PilotPlot1_summer_2019_Flowering.csv`: Flowering data file of the first pilot plot.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Induc_perc`: Percentage of flowering induction
- `MaleOpen_perc`: Percent of male plants with over half of flowers opened.
- `FemaleOpen_perc`: Percent of female plants with over half of flowers opened
- `MonoOpen_perc`: Percent of monoecious plants with over half of female flowers opened.
- `SeedHard_perc`: Percent of seed hardening.
- Notes:   - Blanks due to missing data
	- Block 3 - Row 2 - Column A: Plants did not emerge.
	- Block 3 - Row 2 - Column C: Experimental unit was mixed with adjacent variety.

`PilotPlot1_summer_2019_PlantDensity.csv`: Plant density data file of the first pilot plot
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Row number in the measurement zone of each experimental unit (`1`: left; `2`: right).
- `Quantity`: Amount of plants standing within the measurement zone.
- Notes:   - Blanks due to missing data
	- Block 3 - Row 2 - Column A: Plants did not emerge.
	- Block 3 - Row 2 - Column C: Experimental unit was mixed with adjacent variety.
	- Due to the CBD planting design, numbers in the following experimental units represent the total amounts of CBD plants in it. No measurement zone was established for them unlike the other varieties.
		- Block 1: Row 1 - Column D, Row 2 - Column A
		- Block 2: Row 3 - Column D, Row 4 - Column D
		- Block 4: Row 3 - Column A, Row 4 - Column C
	-From July 31st on, the following blocks were not measured for plant density because they reached 100% seed hardness.
		- Block 1: Row 2 - Column C
		- Block 2: Row 4 - Column A
		- Block 3: Row 1 - Column D; Row 2 - Column A, C, and D
		- Block 4: Row 3 - Column B

`PilotPlot1_summer_2019_PlantHeight-SexRatio.csv`: Plant height and sex ratio file of the first pilot plot.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Individuals within the measurement zone of each experimental unit.
- `Height_cm`: Stem length in centimeters from plant base to apex.
- `Sex`: Sexual identification of the plant.
	- `V`: Vegetative stage
	- `M`: Male
	- `F`: Female
	- `C`: Monoecious (`0`: undetermined; `1`: 80 % male; `2`: 60% male; `3`: 50% male; `4`: 60% female; `5`: 80% female)
- Notes:   - Blanks due to missing data
	- Block 3 - Row 2 - Column A: Plants did not emerge.
	- Block 3 - Row 2 - Column C: Experimental unit was mixed with adjacent variety.
	- Due to the CBD planting design, numbers in the following experimental units represent the total amounts of CBD plants in it. No measurement zone was established for them unlike the other varieties.
		- Block 1: Row 1 - Column D, Row 2 - Column A
		- Block 2: Row 3 - Column D, Row 4 - Column D
		- Block 4: Row 3 - Column A, Row 4 - Column C
	- From July 24th on, the following blocks were not measured for height and ratio because they reached 100% seed hardness.
		- Block 1: Row 2 - Column B and C.
		- Block 2: Row 4 - Column A
		- Block 3: Row 1 - Column D, Row 2 - Column D
		- Block 4: Row 3 - Column B, Row 4 - Column A
	- From July 31st on, the additional following varieties were not measured for height and sex ratio because they reached 100% seed hardness.
		- Block 1: Row 2 - Column D
		- Block 2: Row 3 - Column B and C
		- Block 3: Row 1 - Column A
		- Block 4: Row 4 - Column B

`PilotPlot1_summer_2019_GrainHarvest.csv`: Grain harvest file of the first pilot plot.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `NumberOfPlants`: Individuals within the harvest zone (1 m2) of each experimental unit.
- `BiomassWeight_kg`: Weight of plants' biomass.
- `GrainFreshWeight_kg`: Weight of threshed seeds.
- `GrainDryWeight_g`: Weight of dry seeds, previously threshed.
- `CleanedHarvest_g`: Weight of seeds after threshing, drying, and chaff removal.
- `RootWeight_g`: Weight of plants' roots in grams.
- Notes:   - Blanks due to missing dataBlanks due to missing data
	- Cleaned harvest was ONLY recorded for the following:
		- Block 3 - Row 1 - Column D
		- Block 2 - Row 3 - Column C 
	- Block 2 - Row 3 - Column C: Grain fresh weight, and root weight were not collected.

`PilotPlot1_summer_2019_FiberHarvest_GM.csv`: Fiber harvest (Group measurements) file of the first pilot plot.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `NumberOfPlants`: Individuals within the harvest zone (1 m2) of each experimental unit.
- `FreshWeight_kg`: Weight of plants' biomass.
- `MassOfLeaves_g`: Weight of plants' leaves and branches.
- `SampleFreshWeight_g`: Weight of 5 sampled stems.
- `SampleDryWeight_g`: Weight of 5 dry sampled stems.
- Notes:   -Blanks due to missing data
	- Sample dry weight to be recorded for the following:
		- Block 1 - Row 1 - Column C
		- Block 2 - Row 4 - Column B 
		- Block 3 - Row 2 - Column B
		- Block 4 - Row 3 - Column D

`PilotPlot1_summer_2019_FiberHarvest_IM.csv`: Fiber harvest (Individual measurements) file of the first pilot plot.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Individuals within the harvest zone of each experimental unit.
- `Height_cm`: Stem length in centimeters from plant base to apex.
- `Diameter_cm`: Stem diameter in centimeter at 30 cm from plant base.
- `Sex`: Sexual identification of the plant.
	- `V`: Vegetative stage
	- `M`: Male
	- `F`: Female
- Notes:   - Blanks due to missing data
	- Block 1 - Row 1 - Column A - Rep 3: Missed data.
	- Sex data was not recorded for the following:
		- Block 2 - Row 3 - Column A 
		- Block 2 - Row 4 - Column B

`VarietyTrial1_summer_2019_Emergence.csv`: Emergence data of the first variety trial.
- `Block`: Block composed of 19 varieties, which was replicated 4 times
	- Block 9 is composed of 4 additional fiber varieties replicated 4 times in the same block.
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Row number in the measurement zone of each experimental unit (`1`: left; `2`: right).
- `Emergence`: Number of plants that have emerged from the soil as a result of planting within the measurement zone (Planting date: May 22nd, 2019).
- Notes:   - Blanks due to missing data
	- Block 6, 7, and 8 - Row 5 - Column D: Left blank on purpose.
	- Block 7 - Row 2 - Column A: Mixed planting/missed data.
	- Due to the CBD planting design, numbers in the following experimental units represent the total amounts of CBD plants in it. No measurement zone was established for them unlike the other varieties.
		- Block 5: Row 2 - Column C, and D.
		- Block 6: Row 1 - Column B, Row 2 - Column D
		- Block 7: Row 3 - Column C, Row 4 - Column A
		- Block 8: Row 3 - Column C, Row 4 - Column C

`VarietyTrial1_summer_2019_StandCount.csv`: Population quantification of the first variety trial.
- `Block`: Block composed of 19 varieties, which was replicated 4 times
	- Block 9 is composed of 4 additional fiber varieties replicated 4 times in the same block.
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `StandCount`: Total amount of plants in experimental unit.
- Notes:   - Blanks due to missing data
	- Block 6, 7, and 8 - Row 5 - Column D: Left blank on purpose.
	- Block 7 - Row 2 - Column A: Mixed planting/missed data.

`VarietyTrial1_summer_2019_Plant Density.csv`: Plant density data file of the first variety trial. Data was only taken for the varieties studied in the pilot plot.
- `Block`: Block composed of 19 varieties, which was replicated 4 times
	- Block 9 is composed of 4 additional fiber varieties replicated 4 times in the same block.
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Row number in the measurement zone of each experimental unit (`1`: left; `2`: right).
- `Quantity`: Amount of plants standing within the measurement zone.
- Notes:   - Blanks due to missing data.
	- Due to the CBD planting design, numbers in the following experimental units represent the total amounts of CBD plants in it. No measurement zone was established for them unlike the other varieties.
		- Block 5: Row 2 - Column C, and D.
		- Block 6: Row 1 - Column B, Row 2 - Column D
		- Block 7: Row 3 - Column C, Row 4 - Column A
		- Block 8: Row 3 - Column C, Row 4 - Column C
	- From July 31st on, the following blocks were not measured for plant density because they reached 100% seed hardness.
		- Block 5: Row 4 - Column D
		- Block 6: Row 1 - Column C
		- Block 7: Row 5 - Column C
		- Block 8: Row 3 - Column D

`VarietyTrial1_summer_2019_Flowering.csv`: Flowering data of the first variety trial
- `Block`: Block composed of 19 varieties, which was replicated 4 times
	- Block 9 is composed of 4 additional fiber varieties replicated 4 times in the same block.
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Induc_perc`: Percentage of flowering induction
- `MaleOpen_perc`: Percent of male plants with over half of flowers opened.
- `FemaleOpen_perc`: Percent of female plants with over half of flowers opened
- `MonoOpen_perc`: Percent of monoecious plants with over half of female flowers opened.
- `SeedHard_perc`: Percent of seed hardening.
- Notes:   - Blanks due to missing data
	- Block 6, 7, and 8 - Row 5 - Column D: Left blank on purpose
	- From Jun 19th on, Block 7 - Row 2 - Column A: Experimental unit looked as mixed planting/missed data
	- On June 26th, Block 7 - Row 3 - Column A : Missed data
	- On Aug 12th, Block 6 - Row 4 - Column B and C: Missed data on SeedHard_perc.

`VarietyTrial1_summer_2019_ PlantHeight.csv`: Plant Height data of the first variety trial. 4 data points were taken in the whole each experimental unit regardless of the measurement zone as emergence was scattered.
- `Block`: Block composed of 19 varieties, which was replicated 4 times
	- Block 9 is composed of 4 additional fiber varieties replicated 4 times in the same block.
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Individuals within the measurement zone of each experimental unit.
- `Height_cm`: Stem length in centimeters from plant base to apex.
- Notes:   - Blanks due to missing data
	-Block 6, 7, and 8 - Row 5 - Column D: Left blank on purpose.
	- On June 19th, Block 7 - Row 2 - Column A: Data might be representative of a mixed planting.
	- From June 26th on, Block 7 - Row 3 - Column A: Mixed planting/missed data.
	- From July 24th on, the following blocks were not measured for plant height because they reached 100% seed hardness.
		- Block 5: 
			- Row 1 - Column B and C
			- Row 2 - Column B
			- Row 4 - Column B and D
			- Row 5 - Column B
		- Block 6
			- Row 1 - Column A and C
			- Row 2 - Column B
			- Row 3 - Column B and D (Unit D was 90% seed hardness on July 26th, 29th, 31th)
			- Row 5 - Column A and B
		- Block 7
			- Row 2 - Column D
			- Row 3 - Column A
			- Row 4 - Column B and C
			- Row 5 - Column B and C
		- Block 8
			- Row 2 - Column D
			- Row 3 - Column A and D
			- Row 4 - Column D
			- Row 5 - Column A and B
	- From August 9th on, the following blocks were not measured for plant height because they reached 100% seed hardness.
		- Block 7
			- Row 1 - Column B
		- Block 8
			- Row 4 - Column B
	- From August 20th on, the following blocks were not measured for plant height because they reached 100% seed hardness.
		- Block 5
			- Row 1 - Column A
			- Row 3 - Column A and B
		- Block 6
			- Row 3 - Column A
			- Row 4 - Column C
		- Block 7
			- Row 2 - Column B and C
		- Block 8
			- Row 2 - Column C
			- Row 5 - Column C

`VarietyTrial1_summer_2019_ SexRatio.csv`: Sex data of the first variety trial.
- `Block`: Block composed of 19 varieties, which was replicated 4 times
	- Block 9 is composed of 4 additional fiber varieties replicated 4 times in the same block.
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Male_perc`: Percent of male plants.
- `Female_perc`: Percent of female plants.
- `Mono_perc`: Percent of monoecious plants.
- `Veg_perc`: Percent of plants in vegetative stage.
- Notes:   - Blanks due to missing data
	- Block 6, 7, and 8 - Row 5 - Column D: Left blank on purpose.
	- Block 7 - Row 2 - Column A: Mixed planting/Missed data
	- From July 24th on, the following blocks were not measured for plant height because they reached 100% seed hardness.
		- Block 5: 
			- Row 1 - Column B and C
			- Row 2 - Column B
			- Row 4 - Column B and D
			- Row 5 - Column B
		- Block 6
			- Row 1 - Column A and C
			- Row 2 - Column B
			- Row 3 - Column B and D
			- Row 5 - Column A and B
		- Block 7
			- Row 2 - Column D
			- Row 3 - Column A
			- Row 4 - Column B and C
			- Row 5 - Column B and C
		- Block 8
			- Row 2 - Column D
			- Row 3 - Column A and D
			- Row 4 - Column D
			- Row 5 - Column A and B
	- From August 9th on, the following blocks were not measured for sex ratio because they reached 100% seed hardness.
		- Block 7
			- Row 1 - Column B
		- Block 8
			- Row 4 - Column B
	- From August 20th on, the following blocks were not measured for sex ratio because they reached 100% seed hardness.
		- Block 5
			- Row 1 - Column A
			- Row 3 - Column A and B
		- Block 6
			- Row 3 - Column A
			- Row 4 - Column C
		- Block 7
			- Row 2 - Column B and C
		- Block 8
			- Row 2 - Column C
			- Row 5 - Column C

`VarietyTrial1_summer_2019_PlanHeight-SexRatioPP.csv`: Plant height and sex ratio file of the first variety trial. Data was only taken for the varieties studied in the pilot plot.
- `Block`: Block composed of 19 varieties, which was replicated 4 times
	- Block 9 is composed of 4 additional fiber varieties replicated 4 times in the same block.
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Individuals within the measurement zone of each experimental unit.
- `Height_cm`: Stem length in centimeters from plant base to apex.
- `Sex`: Sexual identification of the plant.
	- `V`: Vegetative stage
	- `M`: Male
	- `F`: Female
	- `C`: Monoecious (`0`: undetermined; `1`: 80 % male; `2`: 60% male; `3`: 50% male; `4`: 60% female; `5`: 80% female)
- Notes:   - Blanks due to missing data
	- Due to the CBD planting design, numbers in the following experimental units represent the total amounts of CBD plants in it. No measurement zone was established for them unlike the other varieties.
		- Block 5: Row 2 - Column C, and D.
		- Block 6: Row 1 - Column B, Row 2 - Column D
		- Block 7: Row 3 - Column C, Row 4 - Column A
		- Block 8: Row 3 - Column C, Row 4 - Column C
	- From July 31st on, the following blocks were not measured for plant height because they reached 100% seed hardness.
		- Block 5: Row 4 - Column D
		- Block 6: Row 1 - Column C
		- Block 7: Row 5 - Column C 
		- Block 8: Row 3 - Column D
	- On August 8th, data was recorded on Block 5 - Row 4 - Column D. Need to be deleted?

`VarietyTrial1_summer_2019_GrainHarvest.csv`: Grain harvest file of the first variety trial.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `NumberOfPlants`: Individuals within the harvest zone (1 m2) of each experimental unit.
- `BiomassWeight_kg`: Weight of plants' biomass.
- `GrainFreshWeight_kg`: Weight of threshed seeds.
- `GrainDryWeight_g`: Weight of dry seeds, previously threshed.
- `CleanedHarvest_g`: Weight of seeds after threshing, drying, and chaff removal
- `RootWeight_g`: Weight of plants' roots in grams
- Notes:   - Blanks due to missing data
	- Grain dry weight and cleaned harvest data to be recorded
		- Block 5
			- Row 3 - Column C
			- Row 1 - Column A
			- Row 3 - Column B
			- Row 3 - Column D
		- Block 6
			- Row 4 - Column B
			- Row 3 - Column D
			- Row 4 - Column C
		- Block 7
			- Row 1 - Column D
			- Row 1 - Column B
			- Row 2 - Column B
		- Block 8
			- Row 4 - Column A
			- Row 4 - Column B
			- Row 2 - Column C
			- Row 1 - Column B
	- Block 7 - Row 2 - Column A was not measured due to mixed planting in the field

`VarietyTrial1_summer_2019_FiberHarvest_GM.csv`: Fiber harvest (Group measurements) file of the first variety trial.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `NumberOfPlants`: Individuals within the harvest zone (1 m2) of each experimental unit.
- `FreshWeight_kg`: Weight of plants' biomass.
- `MassOfLeaves_g`: Weight of plants' leaves and branches.
- `SampleFreshWeight_g`: Weight of 5 sampled stems.
- `SampleDryWeight_g`: Weight of 5 dry sampled stems.
- Notes:   - Blanks due to missing data
	- Sample dry weight to be recorded for the following:
		- Block 5
			- Row 1 - Column D
			- Row 5 - Column C
			- Row 5 - Column A
		- Block 6
			- Row 2 - Column C
			- Row 3 - Column C
			- Row 4 - Column D
		- Block 7
			- Row 5 - Column A
			- Row 3 - Column D
			- Row 1 - Column A
		- Block 8
			- Row 2 - Column A
			- Row 1 - Column C
			- Row 2 - Column B
		- Block 9
			- Row 1 - Column B
			- Row 2 - Column B
			- Row 3 - Column D
			- Row 4 - Column C

`VarietyTrial1_summer_2019_FiberHarvest_IM.csv`: Fiber harvest (Individual measurements) file of the first variety trial.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Individuals within the harvest zone of each experimental unit.
- `Height_cm`: Stem length in centimeters from plant base to apex.
- `Diameter_cm`: Stem diameter in centimeter at 30 cm from plant base.
- `Sex`: Sexual identification of the plant.
	- `V`: Vegetative stage
	- `M`: Male
	- `F`: Female
-Notes:   - Blanks due to missing data
	- No missing data

`PilotPlot2_summer_2019_Emergence.csv`: Emergence data of the second pilot plot.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Row number in the measurement zone of each experimental unit (1: left; 2: right).
- `Emergence`: Number of plants that have emerged from the soil as a result of planting (Planting date: June 21st, 2019).
- Notes:   - Blanks due to missing data
	- Due to the CBD planting design, numbers in the following experimental units represent the total amounts of CBD plants in it. No measurement zone was established for them unlike the other varieties.
		- Block 1: Row 1 - Column D, Row 2 - Column A
		- Block 2: Row 3 - Column D, Row 4 - Column D
		- Block 3: Row 2 - Column A, and C
		- Block 4: Row 3 - Column A, Row 4 - Column C

`PilotPlot2_summer_2019_StandCount.csv`: Population quantification of the second pilot plot.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `StandCount`: Total amount of plants in experimental unit.
- Notes:   - Blanks due to missing data
	- No missing data

`PilotPlot2_summer_2019_Flowering.csv`: Flowering data file of the second pilot plot.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Induc_perc`: Percentage of flowering induction
- `MaleOpen_perc`: Percent of male plants with over half of flowers opened.
- `FemaleOpen_perc`: Percent of female plants with over half of flowers opened
- `MonoOpen_perc`: Percent of monoecious plants with over half of female flowers opened.
- `SeedHard_perc`: Percent of seed hardening.
-Notes:   -Blanks due to missing data
	- From Jun 19th on, the following blocks were not measured due to  
		- Block 1 - Row 1 - Column D 
		- Block 3 - Row 2 - Column C
	-On July 29th, Block 3 - Row 1 - Column A: Missed data

`PilotPlot2_summer_2019_PlantHeight-SexRatio.csv`: Plant height and sex ratio file of the second pilot plot.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Individuals within the measurement zone of each experimental unit.
- `Height_cm`: Stem length in centimeters from plant base to apex.
- `Sex`: Sexual identification of the plant.
	- `V`: Vegetative stage
	- `M`: Male
	- `F`: Female
	- `C`: Monoecious (`0`: undetermined; `1`: 80 % male; `2`: 60% male; `3`: 50% male; `4`: 60% female; `5`: 80% female)
- Notes:   - Blanks due to missing data
	- Block 1 - Row 1 - Column D: Plants did not emerge.
	- Block 3 - Row 2 - Column C: Plants did not emerge.
	- Due to the CBD planting design, numbers in the following experimental units represent the total amounts of CBD plants in it. No measurement zone was established for them unlike the other varieties.
		- Block 1: Row 1 - Column D, Row 2 - Column A
		- Block 2: Row 3 - Column D, Row 4 - Column D
		- Block 3: Row 2 - Column A, and C
		- Block 4: Row 3 - Column A, Row 4 - Column C

`PilotPlotPlus8_summer_2019_Emergence.csv`: Emergence data of the fourth planting.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
	- Block 10-13: Set of pilot plot varieties; Block 14-17: Set of varieties from variety trial
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Row number in the measurement zone of each experimental unit (1: left; 2: right).
- `Emergence`: Number of plants that have emerged from the soil as a result of planting (Planting date: July 18th, 2019).
- Notes:   - Blanks due to missing data
	- Due to the CBD planting design, numbers in the following experimental units represent the total amounts of CBD plants in it. No measurement zone was established for them unlike the other varieties.
		- Block 10: Row 1 - Column D, Row 2 - Column A
		- Block 11: Row 3 - Column D, Row 4 - Column D
		- Block 12: Row 2 - Column A, and C.
		- Block 13: Row 3 - Column A, Row 4 - Column C
		- Block 14: Row 2 - Column B, C, and D
		- Block 15: Row 3 - Column B and D, Row 4 - Column A
		- Block 16: Row 1 - Column C, Row 2 - Column C and D
		- Block 17: Row 3 - Column B and C, Row 4 - Column D

`PilotPlotPlus8_summer_2019_StandCount.csv`: Population quantification of the fourth planting.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
	- Block 10-13: Set of pilot plot varieties; Block 14-17: Set of varieties from variety trial
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `StandCount`: Total amount of plants in experimental unit.
- Notes:   Blanks due to missing data
	- No day was established on the recorded sheet - Maybe July 7th?
	- No missing data

`PilotPlotPlus8_summer_2019_Flowering.csv`: Flowering data file of the fourth planting.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
	- Block 10-13: Set of pilot plot varieties; Block 14-17: Set of varieties from variety trial
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Induc_perc`: Percentage of flowering induction
- `MaleOpen_perc`: Percent of male plants with over half of flowers opened.
- `FemaleOpen_perc`: Percent of female plants with over half of flowers opened
- `MonoOpen_perc`: Percent of monoecious plants with over half of female flowers opened.
- `SeedHard_perc`: Percent of seed hardening.
-Notes:   -Blanks due to missing data
	- From Aug 7thon, Block 15 - Row 3 - Column B and D: plants did not emerge
	- On Aug 9th, Block 14 to 17 data was not recorded: missed data
	- From Aug 12th on, the following observational units were not measured because of no plants
		- Block 12 - Row 1 - Column D
		- Block 15 - Row 3 - Column A
		- Block 16 - Row 2 - Column D
		- Block 17 - Row 3 - Column C
	- From Aug 21st on, Block 14 - Row 1 - Column B was not measured because of no plants
	- From Aug 23rd on, Block 16 - Row 2 - Column D: 1 plant appeared
	- From Aug 26th on, the following blocks were not measured because of no plants
		- Block 10 - Row 1 - Column D
		- Block 16 - Row 2 - Columns C
	- On Sept 9th, Block 14 - Row 2 - Column C: Missed data
	- On Sept 11th, Block 13 - Row 4 - Column C: Missed data

`PilotPlotPlus8_summer_2019_PlantHeight-SexRatio.csv`: Plant height and sex ratio file of the fourth planting.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
	- ONLY Block 10-13: Set of pilot plot varieties
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Individuals within the measurement zone of each experimental unit.
- `Height_cm`: Stem length in centimeters from plant base to apex.
- `Sex`: Sexual identification of the plant.
	- `V`: Vegetative stage
	- `M`: Male
	- `F`: Female
	- `C`: Monoecious (`0`: undetermined; `1`: 80 % male; `2`: 60% male; `3`: 50% male; `4`: 60% female; `5`: 80% female)
- Notes:   - Blanks due to missing data
	- Block 12 - Row 1 - Column D: Plants did not emerge
	- On Oct 15th, Block 10 - Row 2 - Column C was not measured because of no plants.

`PilotPlotPlus8_summer_2019_PlantHeight.csv`: Plant height file of the fourth planting.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
	- ONLY Block 14-17: Set of varieties from variety trial
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Rep`: Individuals within the measurement zone of each experimental unit.
- `Height_cm`: Stem length in centimeters from plant base to apex.
- Notes:   - Blanks due to missing data
	- The following observational units were not measured because of no plants.
		- Block 14
			- Row 1 - Column A and B
			- Row 2 - Column B and D
		- Block 15
			- Row 3 - Column A, B, and D
		- Block 16
			- Row 1 - Column A
			- Row 2 - Column C and D
		- Block 17
			- Row 3 - Column C
			- Row 4 - Column B

`PilotPlotPlus8_summer_2019_SexRatio.csv`: Sex ratio file of the fourth planting.
- `Block`: Block composed of 8 varieties, which was replicated 4 times
	- ONLY Block 14-17: Set of varieties from variety trial
- `Row`: Field rows numbered ascendingly from south to north.
- `Column`: Field columns ordered alphabetically from east to west.
- `Male_perc`: Percent of male plants.
- `Female_perc`: Percent of female plants.
- `Mono_perc`: Percent of monoecious plants.
- `Veg_perc`: Percent of plants in vegetative stage.
- Notes:   - Blanks due to missing data
	- The following observational units were not measured because of no plants.
		- Block 14
			- Row 1 - Column A and B
			- Row 2 - Column B and D
		- Block 15
			- Row 3 - Column A, B, and D
		- Block 16
			- Row 1 - Column A
			- Row 2 - Column C and D
		- Block 17
			- Row 3 - Column C
			- Row 4 - Column B