#pragma once

// constants to be used throughout the code for error signaling
#if CORRT_IMPL_INSIDE_OMPI
    #define CORRT_ERR_SUCCESS   OMPI_SUCCESS
    #define CORRT_ERR_NO_MEM    OMPI_ERR_OUT_OF_RESOURCE
    #define CORRT_ERR_NOT_IMPL  OMPI_ERR_NOT_IMPLEMENTED
    #define CORRT_ERR_WRONG_ARG OMPI_ERR_NOT_SUPPORTED
#else
    #define CORRT_ERR_SUCCESS   MPI_SUCCESS
    #define CORRT_ERR_NO_MEM    MPI_ERR_NO_MEM
    #define CORRT_ERR_NOT_IMPL  MPI_ERR_UNSUPPORTED_OPERATION
    #define CORRT_ERR_WRONG_ARG MPI_ERR_ARG
#endif


char const *
read_env_or_fail(char const *const var_name);

int
read_env_int(char const *const var_name);
