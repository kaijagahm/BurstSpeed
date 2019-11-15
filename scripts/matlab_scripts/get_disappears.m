function disappears = get_disappears(vid)
%GET_DISAPPEARS Finds when the video cuts to black
    % define the number of frames in the video
    nframes = size(vid, 3);
    
    % number of objects in each frame of the uncropped video
    for i = 1:nframes
        [~, num] = bwlabel(vid(:,:,i));
        numobjects(i) = num;
    end

    hasobjects = numobjects ~= 0; % make this logical: either there is or isn't an object
    cut_to_black = [1,0,0,0,0]; % add extra 0;s to safeguard against blips
    beforecuts = strfind(hasobjects, cut_to_black); % indices right before the cut to black
    disappears = beforecuts + 1; % first black frames
end

