cd('D:/')
ls
nvar = 883
elem = 1:nvar 
filename = 'WE38.recharge.dat'



%   function [time, var] = pihm_read_output_func (filename, nvar, elem);
% 
%   % filename: Name of the output file
%   % nvar: Number of elements contained. It should be either the number of
%   % triangular element or the number of river segments, depending on the file
%   % you are reading.   Triangles = 883 , River Seg =114
%   % elem: Index of elements that you want to read. If you want to read all
%   % elements, use 1:nvar.
% 
  fid = fopen('WE38.recharge.dat');

   fseek(fid, 0, 'eof'); % fseek / Move to specified position in file / eof = end of file 
   dim = ftell (fid) / 8 / (nvar + 1); % ftell  /Position in open file / divided by 8 bytes per entry/
   % divided by the number triangle elements (nvar = 883)
   % results in the number of time steps the ouput was recorded (if in days for one year is 366 lines) %

 
  frewind(fid);  %  frewind / Move file position indicator to beginning of open file
 rawtime  = fread (fid, double (dim), 'double', nvar * 8) % fread / Read data from binary file / fread(fileID,sizeA,precision,skip)

   frewind (fid); %  frewind / Move file position indicator to beginning of open file
   var = zeros (dim,length(elem)); %  zeros / Create array of all zeros / 
   %zeros(sz1,...,szN) returns an sz1-by-...-by-szN array of zeros where sz1,...,szN indicate the size of each dimension. For example, zeros(2,3) returns a 2-by-3 matrix
   for i = 1 : dim
   fseek (fid, 8, 0);
   data = fread (fid, double (nvar), 'double');
   var(i, :) = data(elem);
   end
% 
  time = rawtime / 60.0 / 60.0 / 24.0 + datenum (1970, 1, 1);
%   fclose (fid);
