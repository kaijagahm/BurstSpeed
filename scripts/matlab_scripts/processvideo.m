function binVid = processvideo(filename, minsize, maxsize)
%PROCESSVIDEO Reads in video, converts to binary, clears border, filters
    vR = VideoReader(filename); % open video reader
    vid = uint8.empty(vR.Height, vR.Width,0); % make empty uint8
    while hasFrame(vR) % read in video and convert to greyscale
        vid(:,:,end+1) = uint8(median(readFrame(vR), 3)); %#ok<AGROW>
    end
    
    % binarize using adaptive thresholding
    binVid = [];
    for i = 1:size(vid, 3)
        binVid(:,:,i) = ~imbinarize(vid(:,:,i), 'adaptive', 'ForegroundPolarity', 'dark');
    end
    
    % clear border and remove small artefacts
    for i = 1:size(binVid, 3)
        binVid(:,:,i) = bwareafilt(logical(imclearborder(binVid(:,:,i))), [minsize, maxsize]);
    end
end

