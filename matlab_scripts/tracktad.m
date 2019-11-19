function [coordinates] = tracktad(vid)
%TRACKTAD Track the isolated tadpole
%   Finds the white tadpole on a black background, finds its centroid,
%       tracks it
    nframes = size(vid, 3); % how many frames are in the input video?
    coordinates = zeros(nframes, 4); % define the size of the coordinates object
    for i = 1:nframes % for each frame
        if sum(sum(vid(:,:,i),1),2) == 0
            coordinates(i,:) = NaN;
        else
        rp = regionprops(vid(:,:,i), 'Centroid', 'Orientation'); % get properties of the white object
        centroid = rp.Centroid; % get the centroid of the object
        orientation = rp.Orientation; % get the orientation
        coordinates(i,1:2) = centroid; % add those coordinates to the centroid_coordinates output object
        coordinates(i, 3) = orientation;
        end
    end
    coordinates(:,4) = vertcat(NaN, squeeze(sum(sum(abs(diff(vid, 1, 3)), 1), 2)));
end

