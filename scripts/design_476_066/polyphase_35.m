function Hd = polyphase_35(Fs, decim_factor)
%POLYPHASE_35 Returns a multirate filter object.

%
% MATLAB Code
% Generated by MATLAB(R) 7.13 and the DSP System Toolbox 8.1.
%
% Generated on: 25-Jan-2013 20:16:37
%

% FIR least-squares Lowpass filter designed using the FIRLS function.

% All frequency values are in Hz.
%Fs = 112583175.68;  % Sampling Frequency

N     = 840;      % Order
Fpass = 800000;   % Passband Frequency
Fstop = 1200000;  % Stopband Frequency
Wpass = 2;        % Passband Weight
Wstop = 1;        % Stopband Weight

% Calculate the coefficients using the FIRLS function.
b  = firls(N, [0 Fpass Fstop Fs/2]/(Fs/2), [1 1 0 0], [Wpass Wstop]);
Hd = dfilt.dffir(b);
decf = decim_factor;                    % Decimation Factor
num  = get(Hd, 'Numerator');  % Get the numerator from the current filter.

Hd  = mfilt.firdecim(decf, num);

% [EOF]