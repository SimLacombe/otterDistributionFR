#!/usr/bin/bash

################################ Slurm options #################################

### Job name
#SBATCH --job-name=otterISDM                              # Change job name

### Requirements
#SBATCH --nodes=1
#SBATCH --ntasks=4  
#SBATCH --mem-per-cpu=12G
#SBATCH --partition=defq

### Email
#SBATCH --mail-user=simon.lacombe@cefe.cnrs.fr        # Change user for e-mail notification
#SBATCH --mail-type=ALL

### Output & Error
#SBATCH --output=/lustre/lacombres/full_model-%j.out        # Change output name
#SBATCH --error=/lustre/lacombres/full_model-%j.err         # Change error name

################################################################################

echo " Running on: $SLURM_NODELIST"

##load R
module load singularity


##SINGULARITY settings
#create CACHEDIR and TMPDIR accessible in R/W (default location are not accessible)
mkdir -p /lustre/$USER/singularity/cache
mkdir -p /lustre/$USER/singularity/tmp
export SINGULARITY_CACHEDIR="/lustre/$USER/singularity/cache"
export SINGULARITY_TMPDIR="/lustre/$USER/singularity/tmp"
#specify binding directory (skip home directory to avoid conflicts with R personal library)
export SINGULARITY_BIND="/sys:/sys, /proc:/proc, /tmp:/tmp, /var/tmp:/var/tmp, /etc/resolv.conf:/etc/resolv.conf, /etc/passwd:/etc/passwd"

##run R script
singularity exec R-4.3.2-equipe-HAIR-nobinding.img Rscript sourcer_mod.R

