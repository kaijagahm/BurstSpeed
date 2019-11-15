files = dir('edited_videos');
fid = fopen('coordinates/errors.txt', 'wt');
fprintf(fid, 'THE FOLLOWING VIDEO FILES FAILED IN MATLAB');
fclose(fid);
for i = 3:length(files)
    minfilename = files(i).name;
    filename = fullfile('edited_videos', minfilename);
    try
    trackBlob(filename)
    catch
        a = 'There was a problem processing';
        s = ' ';
        b = minfilename;
        toprint = [a,s,b];
        pattern = '.*(?=\.avi$)'; % should pick out the name with filename slashes going either way.
        moniker = string(regexp(minfilename, pattern, 'match'));
        errorfilename = strcat('coordinates/coordinates_', moniker, '_ERROR.txt'); % name the error file
        
        % write the error file
        fid = fopen(errorfilename, 'wt'); % open a new error file with write privileges
        fprintf(fid, toprint); % print the message in the file
        fclose(fid); % close the error file
        
        % add a new line to the master error file
        fid = fopen('coordinates/errors.txt', 'a');
        fprintf(fid, '\n%s', toprint);
        fclose(fid);
    end
end
