filename = 'downconv4x.samples';
duration = 5e-3;
data_freq = 250e6;
carrier_freq = 20e6;
modulating_freq = 2e3;

Ts = 1/data_freq;
t = 0:Ts:duration-Ts;

carrier = sin(2*pi*t*carrier_freq);
modulating = sin(2*pi*t*modulating_freq);

signal = (1+0.1*modulating).*carrier;

fileID = fopen(filename,'w');
for count = 1 : length(signal)
    fprintf(fileID, '%e\t%e\t%e\t%e\t%e\r\n',t(count), signal(count), ... 
        signal(count), signal(count), signal(count));
end
