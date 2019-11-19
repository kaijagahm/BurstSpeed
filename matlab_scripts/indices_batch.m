files = dir('edited_videos');
fid = fopen('frames/errors.txt', 'wt');
fprintf(fid, 'THE FOLLOWING VIDEO FILES FAILED IN MATLAB');
fclose(fid);
for i = 3:length(files)
    minfilename = files(i).name;
    filename = fullfile('edited_videos', minfilename);
    try
        binary_video = processvideo(filename, 30, 400);
        trial_inds = get_trial_inds(binary_video, 250:600, 150:350);
        ntrials = length(trial_inds); % how many trials are there
        if ntrials < 1
            error('Less than 1 trial detected');
        end

        nframes = size(binary_video, 3);
        frames_to_trials = zeros(nframes, 2);
        frames_to_trials(:,1) = 1:nframes;

        for i = 1:ntrials
            t = transpose(trial_inds{i});
            frames_to_trials(t(1):t(length(t)), 2) = i;
        end

        disp('Getting tadpole ID');
        pattern = '(?<=edited_videos[\/\\]).*(?=\.avi)'; % should pick out the name with filename slashes going either way.
        moniker = string(regexp(filename, pattern, 'match'));
        disp(moniker);
        if isempty(moniker)
            error('Tadpole ID is blank. Aborted function to avoid writing a mislabeled frames file.');
        end

        disp('Writing frames file');
        frames_file_name = sprintf('frames/frames_%s.txt', moniker); % save to the 'frames' folder
        writematrix(frames_to_trials, frames_file_name);
        disp('Process completed successfully!');    
    catch
        a = 'There was a problem processing';
        s = ' ';
        b = minfilename;
        toprint = [a,s,b];
        pattern = '.*(?=\.avi$)'; % should pick out the name with filename slashes going either way.
        moniker = string(regexp(minfilename, pattern, 'match'));
        errorfilename = strcat('frames/frames_', moniker, '_ERROR.txt'); % name the error file
        
        % write the error file
        fid = fopen(errorfilename, 'wt'); % open a new error file with write privileges
        fprintf(fid, toprint); % print the message in the file
        fclose(fid); % close the error file
        
        % add a new line to the master error file
        fid = fopen('frames/errors.txt', 'a');
        fprintf(fid, '\n%s', toprint);
        fclose(fid);
    end
end







