#!/bin/bash

# Add R package
cp ../../Scripts/SAFR_0.1.tar.gz anc/.
cp ../../Scripts/SAFR_0.1.zip anc/.

# cp ~/mystuff/Programs/C++/DelayDifference/WeeklyDD/FitWeeklyDelayDifference.cxx anc/.
# cp ~/mystuff/Programs/C++/DelayDifference/WeeklyDD/FixPar.h anc/.
# cp ~/mystuff/Programs/C++/DelayDifference/WeeklyDD/LogLikelihoodFunction.cxx anc/.
# cp ~/mystuff/Programs/C++/DelayDifference/WeeklyDD/LogLikelihoodFunction.h anc/.
# cp ~/mystuff/Programs/C++/DelayDifference/WeeklyDD/vonMisesRecDist.cxx anc/.
# cp ~/mystuff/Programs/C++/DelayDifference/WeeklyDD/WeeklyDelayDifference.cxx anc/.
# cp ~/mystuff/Programs/C++/DelayDifference/WeeklyDD/WeeklyDelayDifference.h anc/.
# cp ~/mystuff/Work/DEEDI/Moreton\ Bay\ Prawn\ Trawl\ fishery/Analysis/Scripts/DelayDifferenceModel/1989-2010/Weekly/model7/Data/TigerWeeklyData1989-2010 anc/.

# Create an archive
tar -cvf Kienzle-SurvivalAnalysis.tar HazardFunctionsForFisheries4Arxiv.tex Estimating-NaturalMortality.ps Estimating-Catchability.ps ComparisonOfNegLL.ps HazardFunctionsForFisheries.bbl anc/*


