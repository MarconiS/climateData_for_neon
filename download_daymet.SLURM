#!/bin/bash
#SBATCH --job-name=get_sites_daymet    # Job name
#SBATCH --mail-type=END               # Mail events (NONE, BEGIN, END, FAIL, AL$
#SBATCH --mail-user=sergio.marconi@weecology.org   # Where to send mail
#SBATCH --account=white
#SBATCH --qos=ewhite

#SBATCH --ntasks=1                 # Number of MPI ranks
#SBATCH --cpus-per-task=1            # Number of cores per MPI rank
#SBATCH --mem=2000
#SBATCH --time=95:59:00       #Time limit hrs:min:sec
#SBATCH --output=get_site_daymet.out   # Standard output and error log
#SBATCH --error= get_sites_daymet.err


pwd; hostname; date
ml R/3.4.3
echo "starting downloading data from day met"
echo $1

Rscript --default-packages=stats,utils,datasets,base,graphics,grDevices,methods /ufrc/ewhite/s.marconi/climateData_for_neon/src/daymet_per_site.R
echo "done"

date
