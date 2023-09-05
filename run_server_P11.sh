# export LD_LIBRARY_PATH=/mingw64/lib/:$LD_LIBRARY_PATH 
# export LD_LIBRARY_PATH=/home/jz964/miniconda3/lib/:$LD_LIBRARY_PATH 
export LD_LIBRARY_PATH=/home/zhou_j/miniconda3/envs/spruce/lib/:$LD_LIBRARY_PATH 
# gfortran -g  teco/src/dataType.f90 teco/src/updateAndSummary.f90 teco/src/writeOutputs2nc.f90 teco/src/soil.f90 teco/src/vegetation.f90 teco/src/transfer.f90 teco/tools/mcmc_functions.f90 teco/src/driver.f90 teco/tools/mcmc.f90 teco/tools/spinup.f90  teco/main.f90 -o run_teco -I/home/jz964/miniconda3/include -L/home/jz964/miniconda3/lib -lnetcdff -lnetcdf
# gfortran -g dataType.f90 driver.f90  mcmc.f90 spinup.f90 vegetation.f90 main.f90 -o run_teco
gfortran -g -fbacktrace -Wall -fcheck=all teco/src/dataType.f90 teco/src/updateAndSummary.f90 teco/src/writeOutputs2nc.f90 teco/src/soil.f90 teco/src/vegetation.f90 teco/src/transfer.f90 teco/tools/mcmc_functions.f90 teco/tools/mcmc_outputs.f90 teco/src/driver.f90 teco/tools/mcmc.f90 teco/tools/spinup.f90  teco/main.f90 -o run_teco -I/home/jz964/miniconda3/include -L/home/jz964/miniconda3/lib -lnetcdff -lnetcdf
# gfortran -g  teco/src/dataType.f90 teco/src/updateAndSummary.f90 teco/src/writeOutputs2nc.f90 teco/src/soil.f90 teco/src/vegetation.f90 teco/src/transfer.f90 teco/tools/mcmc_functions.f90 teco/src/driver.f90 teco/tools/mcmc.f90 teco/tools/spinup.f90  teco/main.f90 -o run_teco #-I/home/jz964/miniconda3/include -L/home/jz964/miniconda3/lib -lnetcdff -lnetcdf

if find "teco" -name "*.mod" -print -quit | grep -q '.*'; then
    rm teco/*.mod
fi

if find "teco/src" -name "*.mod" -print -quit | grep -q '.*'; then
    rm teco/src/*.mod
fi

if find "teco/tools" -name "*.mod" -print -quit | grep -q '.*'; then
    rm teco/tools/*.mod
fi

rm *.mod

./run_teco TECO_model_configs_P11.nml
rm run_teco