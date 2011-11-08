
-define(EQC(P),
    case code:lib_dir(eqc, include) of
        {error, bad_name} ->
            proper:quickcheck(P);
        _ ->
            eqc:check(P)
    end).
