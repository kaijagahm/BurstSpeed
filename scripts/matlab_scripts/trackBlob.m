function allcoords = trackBlob(filename)
%trackBlob returns the centroid of a dark object against a white background
%   Frames of the video found at fileName are binarized, then
%       image centroids are found and returned in 'coordinates'
%   coordinates is an Z-by-2 array of [row, col] for a trial with Z frames
%   frames without an object detected are NaN

disp('Processing video');
binary_video = processvideo(filename, 30, 400);
disp('Video processed successfully');

% get the frame indices for the trials
disp('Getting trial indices');
trial_inds = get_trial_inds(binary_video, 250:600, 150:350);
ntrials = length(trial_inds); % how many trials are there
if ntrials < 1
    error('Less than 1 trial detected');
end
disp('Got trial indices');

% Get a cell array of tadpole-only clips by looping through the trials
disp('Making tadpole-only clips');
tadpoleclips = cell(1, ntrials);
for i = 1:ntrials
    vidframes = binary_video(:,:,trial_inds{i});
    justthetadpole = findtad(vidframes); % this function will only work in matlab 2017 or higher because of the bwselect3 function
    tadpoleclips{i} = justthetadpole;
end
if sum(cellfun(@isempty, tadpoleclips)) ~= 0
    error('1 or more empty cells in `tadpoleclips` cell array.');
end
disp('Made tadpole-only clips');


% track the tadpole in each clip; put centroid coordinates in cell array
disp('Tracking tadpole');
coords = cell(1, ntrials); % make a cell array to hold the coordinates for the trials
for i = 1:ntrials % for each trial
    coords{i} = tracktad(tadpoleclips{i}); % as many rows as there are frames, 2 columns
end
disp('Tadpole tracked');

% Add trial identifiers
disp('Numbering trials');
for i = 1:length(coords)
    id = transpose(repelem(i, length(coords{i}))); % create a new id column
    coords{i} = horzcat(coords{i}, id);
end
disp('Trials numbered');

% Vertically concatenate all of the trials
disp('Making coordinates file');
allcoords = [];
for i = 1:length(coords)
    allcoords = vertcat(allcoords, coords{i});
end
disp('Made coordinates file');

disp('Getting tadpole ID');
pattern = '(?<=edited_videos[\/\\]).*(?=\.avi)'; % should pick out the name with filename slashes going either way.
moniker = string(regexp(filename, pattern, 'match'));
disp(moniker);
if isempty(moniker)
    error('Tadpole ID is blank. Aborted function to avoid writing a mislabeled coordinates file.');
end

disp('Writing coordinates file');
coords_file_name = sprintf('coordinates/coordinates_%s.txt', moniker); % save to the 'coordinates' folder
writematrix(allcoords, coords_file_name);
disp('Process completed successfully!');
end




