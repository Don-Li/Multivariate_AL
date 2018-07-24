AL.do_fitting = function( org, expt_info_, params ){

    #Unpack parameters
    min_irt = expt_info_$min_irt
    min_prp = expt_info_$min_prp
    blackout_time = expt_info_$blackout_time
    COD = expt_info_$COD
    max_n_rft = expt_info_$n_rft
    rft_duration = expt_info_$rft_duration
    total_time = expt_info_$total_time
    blackout_iterations = ceiling( blackout_time/min_irt )
    
    # More parameters
    # For fitting the model, we set all features to 10
    resp_probs = params[1:2]; features = rep(10,5); prob_remember = params[3]; prob_rft_forget = params[4]
    
    # Transformed parameters
    resp_cum_probs = cumsum( resp_probs )
    n_features = length( features )
    
    # Stimulus indices
    rft_index = 3:4; blackout_index = 5
    
    rft_container = prealloc_conc_vivi( expt_info_ )
    component_sequence = as.numeric( substring( names( rft_container ), 2 ) )
    max_comps = length( rft_container )
    
    # Counters
    n_rft = 1; n_comps = 1; n_events = 1L
    time_ = 0
    last_resp = 0; COD_off = 0; resp_value = 0
    rft = F
    
    # Event record
    event_record = data.table( time = rep( NaN, 140000 ), event = NaN )
    header = c("time", "event")
    set( event_record, i = n_events, j = header, list( time_, component_sequence[ n_comps ]+100 ) )
    n_events = n_events + 1L
    
    # Do the model
    rft_time = rft_container[[ n_comps ]]$iri[ n_rft ]
    rft_loc = rft_container[[ n_comps ]]$classes[ n_rft ]
    
    repeat{
        # Get the response time
        if ( rft ){
            time_ = time_ + min_prp
            rft = F
        } else{
            time_ = time_ + min_irt
        }
        # Terminate condition
        if ( time_ > total_time ) break
        
        # Emit
        response = emit( org, resp_cum_probs )
        
        # Check response
        if ( response$resp > 0 ){
            last_resp = resp_value
            resp_value = response$resp; resp_ind = response$ind_match
            
            set( event_record, i = n_events, j = header, list( time_, resp_value ) )
            n_events = n_events + 1L
            
            # Changeover delay
            if ( resp_value != last_resp ) COD_off = time_ + COD
            
            # Deliver a reinforcer and learn
            if ( time_ >= rft_time & resp_value == rft_loc & time_ >= COD_off ){
                learn( org, resp_value, resp_ind )
                rft = TRUE
                
                # Activate reinforcement features
                org$stimulus[ rft_index[ resp_value ] ] = features[ rft_index[ resp_value ] ]
                #Activate
                org$stimulus[ resp_value ] = features[ resp_value ]
                
                # Record food
                set( event_record, i = n_events, j = header, list( time_, resp_value + 10 ) )
                n_events = n_events + 1L
                
                # Progress through schedule
                n_rft = n_rft + 1
                time_ = time_ + rft_duration
                
                # Go to next component
                if ( n_rft == max_n_rft + 1 ){
                    # Record blackout
                    set( event_record, i = n_events, j = header, list( time_, 99 ) )
                    n_events = n_events + 1L
                    
                    time_ = time_ + blackout_time
                    n_comps = n_comps + 1; n_rft = 1
                    
                    #Terminate condition
                    if ( time_ > total_time | ( n_comps > max_comps ) ) break
                    
                    rft = FALSE
                    COD_off = 0
                    
                    # Next component
                    set( event_record, i = n_events, j = header, list( time_, component_sequence[ n_comps ]+100 ) )
                    n_events = n_events + 1L
                    
                    # Blackout effect, activate and forget
                    blackout_forget_activate( org, blackout_iterations, prob_remember, blackout_index, features )
                }
                
                # Get next reinforcer
                rft_time = rft_container[[ n_comps ]]$iri[ n_rft ] + time_
                rft_loc = rft_container[[ n_comps ]]$classes[ n_rft ]
                
            } else{ #
                #Unlearn
                unlearn( org, prob_rft_forget, resp_ind )
                #Activate
                org$stimulus[ resp_value ] = features[ resp_value ]
            }
        }
        
        # forget
        org$stimulus = rbinom( features, org$stimulus, prob_remember )
    }
    
    event_record = event_record[ time < total_time ]
}


AL.pre_train_fitting = function( org, expt_info_, params ){

    #Unpack parameters
    min_irt = expt_info_$min_irt
    min_prp = expt_info_$min_prp
    blackout_time = expt_info_$blackout_time
    COD = expt_info_$COD
    max_n_rft = expt_info_$n_rft
    rft_duration = expt_info_$rft_duration
    total_time = expt_info_$total_time
    blackout_iterations = ceiling( blackout_time/min_irt )
    
    # More parameters
    resp_probs = params[1:2]; features = rep(10,5); prob_remember = params[3]; prob_rft_forget = params[4]
    
    # Transformed parameters
    resp_cum_probs = cumsum( resp_probs )
    n_features = length( features )
    
    # Stimulus indices
    rft_index = 3:4; blackout_index = 5
    
    rft_container = prealloc_conc_vivi( expt_info_ )
    max_comps = length( rft_container )
    
    # Counters
    n_rft = 1; n_comps = 1; n_events = 1L
    time_ = 0
    last_resp = 0; COD_off = 0; resp_value = 0
    rft = F
    
    # Do the model
    rft_time = rft_container[[ n_comps ]]$iri[ n_rft ]
    rft_loc = rft_container[[ n_comps ]]$classes[ n_rft ]
    
    repeat{
        # Get the response time
        if ( rft ){
            time_ = time_ + min_prp
            rft = F
        } else{
            time_ = time_ + min_irt
        }
        # Terminate condition
        if ( time_ > total_time ) break
        
        # Emit
        response = emit( org, resp_cum_probs )
        
        # Check response
        if ( response$resp > 0 ){
            last_resp = resp_value
            resp_value = response$resp; resp_ind = response$ind_match
            
            # Changeover delay
            if ( resp_value != last_resp ) COD_off = time_ + COD
            
            # Deliver a reinforcer and learn
            if ( time_ >= rft_time & resp_value == rft_loc & time_ >= COD_off ){
                learn( org, resp_value, resp_ind )
                rft = TRUE
                
                # Activate reinforcement features
                org$stimulus[ rft_index[ resp_value ] ] = features[ rft_index[ resp_value ] ]
                # Activate response features
                org$stimulus[ resp_value ] = features[ resp_value ]
                
                # Progress through schedule
                n_rft = n_rft + 1
                time_ = time_ + rft_duration
                
                # Go to next component
                if ( n_rft == max_n_rft + 1 ){
                    time_ = time_ + blackout_time
                    n_comps = n_comps + 1; n_rft = 1
                    #Terminate condition
                    if ( time_ > total_time | ( n_comps > max_comps ) ) break
                    
                    rft = FALSE
                    COD_off = 0
                    
                    # Blackout effect, activate and forget
                    blackout_forget_activate( org, blackout_iterations, prob_remember, blackout_index, features )
                }
                
                # Get next reinforcer
                rft_time = rft_container[[ n_comps ]]$iri[ n_rft ] + time_
                rft_loc = rft_container[[ n_comps ]]$classes[ n_rft ]
                
            } else{
                #Unlearn
                unlearn( org, prob_rft_forget, resp_ind )
                #Activate
                org$stimulus[ resp_value ] = features[ resp_value ]
            }
        }
        
        # forget
        org$stimulus = rbinom( features, org$stimulus, prob_remember )
    }
}
