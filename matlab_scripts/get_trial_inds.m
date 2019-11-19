function trial_inds = get_trial_inds(vid, horizbox, vertbox)
%GET_TRIAL_INDS Finds trial frame indices
%   Finds tadpole appearances and disappearances
%   Determines the order of trial beginnings and ends
%   Finds trial frames and puts them into a cell array

    % define the number of frames in the video
    nframes = size(vid, 3);

%GET APPEARS
    % number of objects in each frame of the cropped video
    vidcropped = vid(vertbox, horizbox,:);
    numobjects_c = [];
    for i = 1:nframes
        [~, num] = bwlabel(vidcropped(:,:,i));
        numobjects_c(i) = num;  %#ok<*AGROW>
    end
    hasobjects_c = numobjects_c ~= 0;

    % find places where the object comes into the frame
    appear_string = [0,1,1,1,1]; % add an extra 1's to safeguard against blips
    beforeappears = strfind(hasobjects_c, appear_string); % indices right before the tadpole appears
    appears = beforeappears + 1; % first frames where the tadpole appears

%GET DISAPPEARS
    % number of objects in each frame of the uncropped video
    numobjects = [];
    for i = 1:nframes
        [~, num] = bwlabel(vid(:,:,i));
        numobjects(i) = num;
    end

    hasobjects = numobjects ~= 0; % make this logical: either there is or isn't an object
    cut_to_black = [1,0,0,0,0]; % add extra 0;s to safeguard against blips
    beforecuts = strfind(hasobjects, cut_to_black); % indices right before the cut to black
    disappears = beforecuts + 1; % first black frames
    
    % Check to see if there are multiple "appearances" between
    % disappearances
    for i = 1:(numel(disappears)-1)
        d1 = disappears(i);
        d2 = disappears(i+1);
        a1 = min(appears(appears > d1 & appears < d2));
        a2 = max(appears(appears > d1 & appears < d2));
        if a1 ~= a2
            appears(appears > a1 & appears < d2) = NaN;
        end
    end
    
    % Remove any NA's resulting from the above check
    appears(isnan(appears)) = [];
        

    % define the possible types of videos
    normal = 0;
    fadein = 0;
    error = 0;
    straightthrough = 0;

    % check which type of video we have
    if ismember(numel(disappears), [0,1]) && numel(appears) == 0 && numobjects_c(1) == 1
        straightthrough = 1; % this code actually doesn't quite work, but I can't figure out how to fix it, so I will do it manually.
    elseif disappears(1) < appears(1) && numobjects_c(1) == 1
        normal = 1;
    elseif disappears(1) < appears(1) && numobjects_c(1) > 1 % there's a non-tadpole object
            error = 1;
            error('More than one object in the center of the frame at start. Check to be sure that the tadpole is being tracked.');
    elseif disappears(1) > appears(1) && numobjects_c(1) == 0
        fadein = 1;
    else % any other conditions I'm not thinking of???
        error = 1;
        error('Video does not meet normal, onetrial, or fadein conditions');
    end

    % now we're going to find the indices of the trials
    ntrials = [];
    if straightthrough == 1 % if there's only one trial from the beginning to the end
        trial_inds = cell(1, 1);
        trial_inds{1} = 1:nframes;
        
    elseif normal == 1 % if the video starts with a tadpole and proceeds, as defined above
        if numel(appears) > numel(disappears)
            ntrials = numel(disappears) + 1;
        elseif numel(appears) == numel(disappears) % if no cut to black before end
            ntrials = numel(disappears) + 1;
        else % if cut to black before end
            ntrials = numel(disappears);
        end
        trial_inds = cell(1, ntrials);
        trial_inds{1} = 1:disappears(1); % indices of the first trial
        if numel(appears) == numel(disappears) % indices of the last trial, if no cut to black before end
            trial_inds{ntrials} = max(appears):nframes;
        elseif numel(appears) > numel(disappears) && appears(length(appears)-1) > max(disappears)
            trial_inds{ntrials} = appears(length(appears)-1):max(appears);
        elseif numel(appears) > numel(disappears)
            error('Tadpole swims into the frame in a trial other than the last one');
        else % indices of the last trial, if cut to black before end
            trial_inds{ntrials} = max(appears):max(disappears);
        end
        
        % now fill in the rest of the trials
        if ntrials > 2
            for i = 2:(ntrials-1)
                trial_inds{i} = appears(i-1):disappears(i);
            end
        end
        
    elseif fadein == 1
        ntrials = numel(appears);
        trial_inds = cell(1, ntrials);
        for i = 1:numel(disappears)
            trial_inds{i} = appears(i):disappears(i);
        end
        if numel(appears) > numel(disappears)
            trial_inds{ntrials} = max(appears):nframes;
        end
    end
    % check for empty cells
    emptycells = [];
    for i = 1:length(trial_inds)
        emptycells(i) = isempty(trial_inds{i});
    end
    if sum(emptycells) > 0
        error('One or more empty cells in trial_inds');
    end
end

