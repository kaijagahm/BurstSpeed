function result = testfun(input)
    moniker = fullfile('edited_videos', input, '.txt');
    result = mod(input, 2);
    if result ~= 0
        error('the input was an odd number.')
    else
       writematrix(result, moniker)
    end
end

