function trial_inds = get_trial_inds(vid,boxL, boxR, boxD, boxU)
%GET_TRIAL_INDS Summary of this function goes here
%   Detailed explanation goes here
% get a vector of all frames that are entirely black
sums = squeeze(sum(sum(binVid2,1),2));

% get indices for trial frames
trialframes = find(sums ~= 0);

% find the first frame in a sequence that contains a white element in a
% central bounding box
horizbox = 250:600;
vertbox = 150:350;

% number of objects in each frame of the cropped video
binVidcropped = binVid2(vertbox, horizbox,:);
numobjects_c = [];
for i = 1:nframes
    [matrix, num] = bwlabel(binVidcropped(:,:,i));
    numobjects_c(i) = num; 
end

% find places where the object comes into the frame
appear_string = [0,1,1,1,1]; % add an extra 1's to safeguard against blips
beforeappears = strfind(numobjects_c, appear_string); % indices right before the tadpole appears
appears = beforeappears + 1; % first frames where the tadpole appears

% number of objects in each frame of the uncropped video
for i = 1:nframes
    [matrix, num] = bwlabel(binVid2(:,:,i));
    numobjects(i) = num;
end

hasobjects = numobjects ~= 0; % make this logical: either there is or isn't an object
cut_to_black = [1,0,0,0,0]; % add extra 0;s to safeguard against blips
beforecuts = strfind(hasobjects, cut_to_black); % indices right before the cut to black
disappears = beforecuts + 1; % first black frames

% define the possible types of videos
normal = 0;
fadein = 0;
error = 0;

% check which type of video we have
if disappears(1) < appears(1) && numobjects_c(1) == 1
    normal = 1;
elseif disappears(1) < appears(1) && numobjects_c(1) > 1 % there's a non-tadpole object
        error = 1;
        disp('More than one object in the center of the frame at start. Check to be sure that the tadpole is being tracked.')
elseif disappears(1) > appears(1) && numobjects_c(1) == 0
    fadein = 1;
else % any other conditions I'm not thinking of???
    error = 1;
    disp('Video does not meet normal or fadein conditions')
end

% define the number of frames in the video
nframes = size(binVid2, 3);

% now we're going to find the indices of the trials
if normal == 1 % if the video starts with a tadpole and proceeds, as defined above
    if numel(appears) == numel(disappears) % if no cut to black before end
        ntrials = numel(disappears) + 1;
    else % if cut to black before end
        ntrials = numel(disappears);
    end
    trial_inds = cell(1, ntrials);
    trial_inds{1} = 1:disappears(1);
    if numel(appears) == numel(disappears) % if no cut to black before end
        trial_inds{ntrials} = max(appears):nframes;
    else % if cut to black before end
        trial_inds{ntrials} = max(appears):max(disappears);
    end
    for i = 2:ntrials-1 % fill in the rest of the trials
        trial_inds{i} = appears(i-1):disappears(i);
    end
elseif fadein == 1
    ntrials = numel(appears);
    trial_inds = cell(1, ntrials);
    for i = numel(d)
        trial_inds{i} = appears(i):disappears(i);
    end
    if numel(appears) > numel(disappears)
        trial_inds{ntrials} = max(appears):nframes;
    end
elseif error == 1
    disp('Error')    
end

end

