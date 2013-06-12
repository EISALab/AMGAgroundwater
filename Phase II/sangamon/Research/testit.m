function testit(sed)
rand('seed', sed);
reg_nitr(150);
%reg_N(500);
%day_reg_N(3000);
%rand('seed', 897654);
rand('seed', sed);
reg_sangamon( sed );
%reg_nitr2(sed, false);
%reg_N2(sed);
%day_reg_N2(sed);
%rand('seed', sed);
%reg_nitr2(sed, true);
%close all;