emit = function( org, resp_probs, transition_prob, last_resp, state, start_state ){
    
    rft_matches = row_match1( org$rft_memory, org$stimulus )
    
    if ( length( rft_matches ) == 0 ){
        if ( state == 1 ){
            if ( runif(1) <= resp_probs[1] ){
                resp = 1
            } else resp = 0
        } else{
            if ( runif(1) <= resp_probs[2] ) {
                resp = 2
            } else resp = 0
        }
        start_state = F
    } else{
        resp = org$response_association[ rft_matches ]
        state = resp
        start_state = T
    }
    
    list( resp = resp, ind_match = rft_matches, state = state, start_state = start_state )
}


learn = function( org, response, resp_ind ){
    if ( length( resp_ind ) == 0 ){
        org$rft_memory = rbind( org$rft_memory, org$stimulus )
        org$response_association = c( org$response_association, response )
    }
}

prp_learn = function( org, response, resp_ind, x_param ){
    if ( runif(1) < x_param ){
        if ( length(row_match1( org$rft_memory, org$stimulus )) == 0 ){
        org$rft_memory = rbind( org$rft_memory, org$stimulus )
        org$response_association = c( org$response_association, response )
        }
    }
}

unlearn = function( org, pr, resp_index ){
    if ( length( resp_index ) > 0 ){
        if ( runif(1) <= pr ){
            org$rft_memory = org$rft_memory[ -resp_index, , drop = F]
            org$response_association = org$response_association[ - resp_index ]
        }
    }
}

prealloc_conc_vivi = function( expt_info ){
    
    components = expt_info$rft_prob_l[[1]]
    n_rft = expt_info$n_rft
    iri = expt_info$vi_iri

    lapply( components, function( rft_prob_left ){
        iris = rexp( n_rft, 1/iri )
        rft_classes = sample( 2, length( iris ), replace = T, c( rft_prob_left, 1-rft_prob_left ) )
        list( iri = iris, classes = rft_classes )
    } )
}

blackout_forget_activate = function( org, blackout_iterations, prob_remember, blackout_index, features ){
    # Forget
    stim = org$stimulus
    # Need to subtract 2. Subtract 1 for equality in pgeom and subtract 1 so that we don't do unlearn() twice after blackout.
    remember_prob = pgeom( blackout_iterations - 2, prob_remember, lower.tail = F )
    stim = rbinom( length(stim), stim, remember_prob )
    
    # Activate
    stim[ blackout_index ] = features[ blackout_index ]
    org$stimulus = stim
}

add_component_labels = function( event_records ){
    event_records[ , component := {
        comp_starts = which( event > 100 )
        comp_labels = event[comp_starts]-100
        component_lengths = diff( c( comp_starts, .N+1 ) )
        rep( comp_labels, times = component_lengths )
    } ]
}

get_variable_names = function( model_data_summary ){
    all_variable_names = unique( model_data_summary[ , var_names ] )
    pp_names = startsWith( all_variable_names, "pp" )
    list( all_variable_names[ !pp_names ], all_variable_names[ pp_names ] )
}

make_pretraining_expt_info = function( expt_info ){
    pretrain_expt_info = expt_info[1]
    pretrain_expt_info[ , rft_prob_l := list( list( rep(0.5, 7) ) ) ]
}

make_org = function( params ){
    org = new.env( parent = emptyenv() )
    org$rft_memory = matrix( numeric(0), ncol = sum( startsWith( names(params),"features") ) )
    org$stimulus = rep( 0, ncol( org$rft_memory ) )
    org$response_association = numeric(0)
    org
}

get_date = function() format( Sys.time(), "%H_%M_%d_%b_%y" )

make_org_fitting = function( params ){
    org = new.env( parent = emptyenv() )
    # 5 columns, 2 for left/right resp, 2 for left/right rft, 1 for blackout
    org$rft_memory = matrix( numeric(0), ncol = 5 )
    org$stimulus = rep( 0, ncol( org$rft_memory ) )
    org$response_association = numeric(0)
    org
}
