# This is where we make changes when a new PIC resource becomes available or when one is retired

.onLoad <- function(libname, pkgname) {

  # A list containing the PIC system names, partition name, names the compute nodes, the number of cores, and the MPI compiler path

  x <- list(# Olympus has been decommissioned
            #
            #olympus = list(partitions =   list(short = list(computeNodeNames = "node", numCores = 32),
            #                                   long  = list(computeNodeNames = "node", numCores = 32),
            #                                   slurm = list(computeNodeNames = "node", numCores = 32),
            #                                   fat   = list(computeNodeNames = "fat",  numCores = 32)),
            #               MPIpath = "/share/apps/mvapich2/1.7/gcc/4.6.0/bin/mpicc",
            #               moduleCommands = "module load gcc/4.6.0; module load mvapich2/1.7"),

            # Constance
            constance = list(partitions = list(short = list(computeNodeNames = "node", numCores = 24),
                                               long  = list(computeNodeNames = "node", numCores = 24),
                                               slurm = list(computeNodeNames = "node", numCores = 24),
                                               gpu   = list(computeNodeNames = "gpu",  numCores = 24),
                                               phi   = list(computeNodeNames = "phi",  numCores = 16)),
                             MPIpath = "/share/apps/mvapich2/2.0.1/gcc/4.8.2/bin/mpicc",
                             moduleCommands = "module load gcc/4.8.2; module load mvapich2/2.0.1")
            )

  options(piclapplyParms = x)

}


.onUnload <- function(libpath) {

  options(piclapplyParms = NULL)

}


