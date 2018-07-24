recode_comps = function( model_data_summary ){
    model_data_summary[ , component := {
        comp_vect = as.character( component )
        new_comp_codes = vector( "numeric", length(comp_vect) )
        numbered_vars = startsWith( comp_vect, "C" )
        new_comp_codes[ numbered_vars ] = as.numeric( substring( comp_vect[ numbered_vars ], 2 ) )
        new_comp_codes[ comp_vect == "x" ] = 99
        new_comp_codes
    } ]
}
