#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

#include "util.h"

const char *read_env_or_fail(const char *var_name)
{
    const char *var = getenv(var_name);

    if (!var) {
        fprintf(stderr, "Variable '%s' is unset\n", var_name);
        PMPI_Abort(MPI_COMM_WORLD, -1);
        /* no return */
    }

    return var;
}


int read_env_int(const char *var_name)
{
    const char *var = read_env_or_fail(var_name);

    int res;
    if (1 != sscanf(var, "%d", &res)) {
        fprintf(stderr, "Could not parse variable '%s' (%s) as integer\n", var_name, var);
        PMPI_Abort(MPI_COMM_WORLD, -1);
        /* no return */
    };

    return res;
}

