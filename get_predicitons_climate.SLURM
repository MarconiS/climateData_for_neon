#!/bin/bash
#SBATCH --job-name=climate_to_predictions    # Job name
#SBATCH --mail-type=END               # Mail events (NONE, BEGIN, END, FAIL, AL$
#SBATCH --mail-user=sergio.marconi@weecology.org   # Where to send mail
#SBATCH --account=ewhite
#SBATCH --qos=ewhite-b

#SBATCH --ntasks=1                 # Number of MPI ranks
#SBATCH --cpus-per-task=64            # Number of cores per MPI rank
#SBATCH --mem=320000
#SBATCH --time=95:59:00       #Time limit hrs:min:sec
#SBATCH --output=climate_to_predictions.out   # Standard output and error log
#SBATCH --error=climate_to_predictions.err


pwd; hostname; date
ml R/3.4.3
echo "starting downloading data from day met"

Rscript --default-packages=stats,utils,datasets,base,graphics,grDevices,methods /ufrc/ewhite/s.marconi/climateData_for_neon/src/get_values_from_netcdf.R
echo "done"

date
