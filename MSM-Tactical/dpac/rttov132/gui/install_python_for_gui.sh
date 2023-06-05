#!/bin/sh
# this script install the python environement required by RTTOV GUI on your $HOME
# WRANING this script will update your .bashrc file
# change PREFIX below and prefix on the last line of the rttovguienv.yml file
# if you want a different installation

PREFIX=~/miniconda3
if [ -d "$PREFIX" ] ; then
	echo "directory $PREFIX already exists"
	echo "change PREFIX in this script"
        echo "and prefix on the last"
	echo "line of rttovguienv.yml"
	exit 1
fi

wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh  
bash Miniconda3-latest-Linux-x86_64.sh -bf -p ${PREFIX} 
~/miniconda3/bin/conda init
. ~/.bashrc
${PREFIX}/bin/conda env create -f ./rttovguienv.yml
conda clean --tarballs
conda clean --packages
echo "conda activate rttovgui" >> ~/.bashrc


