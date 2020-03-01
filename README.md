# BurstSpeed
Data and analysis for my senior thesis project on development/performance tradeoffs in wood frog tadpoles, 2019-2020.

# Project Description
Amphibians are particularly vulnerable to climate change, and the wood frog, a widely distributed North American anuran, is a good candidate for studying the effects of warming. Because wood frogs breed in temporary ponds, their breeding timelines are constrained by spring ice melt and summer pond drying. As ice melts later and summers get hotter, wood frog tadpoles are being forced to develop more quickly than before in order to avoid death by desiccation when their ponds dry up.

My project examines what effects might result from this more rapid development. Previous research has shown that fast-developing organisms often pay some sort of performance cost. One analogy is to IKEA furniture: you can put it together quickly, but it's not as high-quality as a hand-carved, artisan piece that takes much longer.

Over the summer of 2019, I measured burst swimming speed (how fast the tadpoles can "sprint" away from a predator attack, for example) as a metric of swimming performance in tadpoles from ponds in northwestern Connecticut. These tadpoles were reared in the lab under two different temperature treaments, representing the high and low ends of the temperature regime they would likely encounter in the wild.

I test the hypothesis that if there is a tradeoff between developmental rate and swimming performance, then slower-developing tadpoles (cold temperature treatment) will show faster burst swimming speeds than their fast-developing counterparts.

# Project Structure
The following is a tree diagram of this project's folders, with brief explanations of contents.

├── 2019_PostResp_photos    # photos of tadpoles after experimentation
│   └── Exposure_Edit     # edited photos
├── Analysis.Rmd    # final analysis code
├── Analysis_prep.Rmd     # final data cleaning and preparation for analysis
├── BurstSpeed2019.Rproj    # R project (OPEN FIRST)
├── Morphometrics.Rmd     # morphometric analysis from landmarks: procrustes etc.
├── README.md     # this file: explanation of the project.
├── Step1_Process_Coordinates.Rmd     # step 1 of data cleaning
├── Step2_Coords_to_Trajectories.Rmd    # step 2 of data cleaning
├── Step3_Trajectories_to_Speeds.Rmd    # step 3 of data cleaning
├── Step4_Find_Burst_Beginnings.Rmd     # step 4 of data cleaning
├── Step5_Edit_Bursts.Rmd     # step 5 of data cleaning
├── data # all project data
│   ├── inputs    # raw data (DO NOT MODIFY)
│   └── outputs     # intermediates, mostly .Rda files to load into further data cleaning steps.
├── detect_error_videos.R
├── dev_growth_rates.Rmd    # analyzing Kaija/Andis's development data
├── dev_growth_rates_baasim.Rmd     # analyzing Baasim's development data
├── failedvideos.R
├── figures_for_andis.R     # quick figure showing variability between clutches vs. in wild tadpoles
├── frames_code.R
├── graphics
│   └── poster    # poster for YURA, September 2019
├── idtracker_data_fix.R
├── idtracker_matlab_comparison.R     # comparison of tracking coordinates results: idTracker vs. custom MATLAB code
├── libraries.R     # list of all packages required in the project. Sourced at the beginning of cleaning/analysis scripts.
├── loess_wrapper_extrapolate.R
├── manuscript.rmd    # pre-emptive template for writing the manuscript in RMarkdown
├── matlab_scripts    # all scripts for the MATLAB tadpole-tracking algorithm
│   ├── findtad.m     # detect the tadpole on the white background
│   ├── get_appears.m     # identify frames where the tadpole comes into view at the beginning of trials
│   ├── get_disappears.m    # identify frames where the trial cuts to black
│   ├── get_trial_inds.m    # get frame indices for each trial
│   ├── gettrialinds.m    # get frame indices for each trial
│   ├── indices_batch.m     # run indices script on all videos
│   ├── matlab_batch.m    # run tracking script on all vidoes
│   ├── processvideo.m    # load and process the video
│   ├── testfun.m
│   ├── trackBlob.m     # wrapper function: loading video through tracking tadpole and exporting coordinates
│   └── tracktad.m    # track the tadpole's location across the frames 
├── model_selection.Rmd     # selecting random and fixed effects for the mixed-effects model. Also variance structure.
├── model_selection_archive.Rmd     # deprecated model selection code
├── photos_random_rename.R    # randomly shuffle photos
├── process_coordinates.R     # deprecated code for processing tadpole coordinates
├── process_coordinates_funs.R    # functions called for processing coordinates
├── protocols
│   ├── Burst Speed Trials Protocol.docx    # how to run burst speed trials
│   └── BurstSpeedDataSheet.docx    # data sheet for recording trial info
├── tally_by_group.R    # function to apply a tally to rows after dplyr::group_by()
├── test_data_editing.R
├── test_loess_plots.R
└── tests
    ├── test_tracking_idtracker
    └── test_tracking_matlab

# Contributors
Kaija Gahm (kaija.gahm@yale.edu)

