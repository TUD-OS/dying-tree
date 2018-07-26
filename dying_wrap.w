// forward declaration
int corrected_broadcast(void *const, int const, MPI_Datatype const, int const, MPI_Comm const);


{{fn fn_name MPI_Init}}
     {{callfn}}
     die_if_needed();
{{endfn}}

{{fn fn_name MPI_Finalize}}
     // dying_finalize();
     {{callfn}}
{{endfn}}

{{fn fn_name MPI_Bcast}}
     if (!die_in_bcast()) {
       // {{callfn}}
       {{ret_val}} = corrected_broadcast{{argList}};
     }
{{endfn}}

{{fnall func MPI_Bcast MPI_Init MPI_Wtime MPI_Comm_split MPI_Finalize}}
     {{apply_to_type MPI_Comm REPLACE_COMM_WORLD}}
     {{callfn}}
{{endfnall}}
