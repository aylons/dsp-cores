filename = 'cic.samples';
decimation = 1000;

%Put 1/4 the maximum value in the last sample of the first decimation.
%In the next sample, put the other half.
%If first output = 0.25, second = 0.5 it is properly aligned. If the first
%output = 0, it is decimating before, if 0.5, it is decimating after the
%point.

signal(1:decimation-1) = 0;
signal(decimation:decimation+1) = 0.25;
signal(decimation+2:2*decimation) = 0;


fileID = fopen(filename,'w');
for count = 1 : length(signal)
    fprintf(fileID, '%e\r\n',signal(count));
end

beep;