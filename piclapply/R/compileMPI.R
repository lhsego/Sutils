# Write and compile the MPI code used to launch multiple jobs
# This function is used to support piclapply()

compileMPI <- function(wkdir, rscript.file, MPIpath, verbose = FALSE) {

  # NOTE: rscript.file is presumed to include its full path

  # Remake the wkdir.logs variable
  wkdir.logs <- paste(wkdir, "logs", sep = "/")

  # Create the string that will be used to make the R system call in the c code
  str1 <- paste(Sys.getenv("R_HOME"), "/bin/R CMD BATCH --no-restore --no-save '--args %d' ",
                rscript.file, " ", wkdir.logs, "/", Smisc::stripPath(Smisc::stripExtension(rscript.file)),
               "_%d.Rout", sep = "")
  str2 <- paste("    sprintf(str, \"", str1, "\", myid, myid);\n", sep = "")


  # Write wkdir/launch.c
  cat("#include \"mpi.h\"\n",
      "#include <stdio.h>\n",
      "#include <math.h>\n",
      "\n",
      "int main(int argc, char *argv[])\n",
      "{\n",
      "    int done = 0, n, myid, numprocs, i;\n",
      "    double startwtime = 0.0, endwtime;\n",
      "    int  namelen;\n",
      "    char processor_name[MPI_MAX_PROCESSOR_NAME];\n",
      "    char str[512];\n",
      "\n",
      "    MPI_Init(&argc,&argv);\n",
      "    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);\n",
      "    MPI_Comm_rank(MPI_COMM_WORLD,&myid);\n",
      "    MPI_Get_processor_name(processor_name,&namelen);\n",
      "\n",
      "    fprintf(stdout, \"Process %d on %s\\n\", myid, processor_name);\n",
      "\n",
      "    // Issue the command\n",
      str2,
      "    system(str);\n",
      "    \n",
      "    MPI_Finalize();\n",
      "\n",
      "    return 0;\n",
      "}\n",

      sep = "",
      file = paste(wkdir, "launch.c", sep = "/"))

  # This little trick captures a spurious warning on constance:
  # /usr/bin/ld: warning: libquadmath.so.0, needed by /share/apps/mvapich2/2.0.1/gcc/4.8.2/lib/libmpich.so, not found (try using -rpath or -rpath-link)
  compileFun <- function(ignoreErr = TRUE) {
    system(paste(MPIpath, " -lpmi ", wkdir, "/launch.c -o ", wkdir, "/launch.o", sep = ""),
           ignore.stderr = ignoreErr)
  }

  # Now compile launch.c code
  compilation.failed <- compileFun()

  # Check compilation error
  if (compilation.failed) {
    compileFun(FALSE)
    stop("MPI code '", wkdir, "/launch.c' did not compile correctly\n")
  }

  if (verbose) {
    star("MPI code '", wkdir, "/launch.c' written and compiled")
  }

} # compileMPI
