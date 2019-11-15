function tadvid = findtad(vid)
%TADVID Function to pick out the tadpole in a video by finding it in the
    %first frame and tracking it through
    ff = vid(:,:,1);
    width = size(vid, 2);
    height = size(vid, 1);
    center = [width/2, height/2];
    bcc = bwconncomp(ff);
    
    % throw an error if no object is detected in the first frame
    if bcc.NumObjects < 1
        error('No objects detected in the first frame');
    end
    
    rp = regionprops(bcc, 'centroid');
    distance = [];
    for i  = 1:numel(rp) % calculate the distance to center for each object in the first frame
        obj = rp(i).Centroid;
        distance(i) = norm(obj-center);
    end
    [~,idx] = min(distance); % find the index of the object closest to the center
    tadpole_centroid = round(rp(idx).Centroid); % grab the centroid of the tadpole in the first frame
    tadvid = bwselect3(vid, tadpole_centroid(1), tadpole_centroid(2), 1, 6);
end
