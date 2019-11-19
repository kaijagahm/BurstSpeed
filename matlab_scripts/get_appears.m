function appears = get_appears(vid, horizbox, vertbox)
%GET_APPEARS Finds all frames where the tadpole appears in the bounding
    %box
    % define the number of frames in the video
    nframes = size(vid, 3);
    
    % get a vector of all frames that are entirely black
    sums = squeeze(sum(sum(vid,1),2));

    % get indices for trial frames
    trialframes = find(sums ~= 0);

    % number of objects in each frame of the cropped video
    vidcropped = vid(vertbox, horizbox,:);
    numobjects_c = [];
    for i = 1:nframes
        [~, num] = bwlabel(vidcropped(:,:,i));
        numobjects_c(i) = num; 
    end

    % find places where the object comes into the frame
    appear_string = [0,1,1,1,1]; % add an extra 1's to safeguard against blips
    beforeappears = strfind(numobjects_c, appear_string); % indices right before the tadpole appears
    appears = beforeappears + 1; % first frames where the tadpole appears
end

