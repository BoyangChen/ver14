function [] = uelplot2dfigvids

clc
close('All')
%clear mex

% --- turn on profile to see computational time distribution; ---------
% --- need to uncomment the last line 'view profle' as well -----------
%profile on;

%
%**********************************************************************
%**************** Define useful stuffs ********************************
%**********************************************************************
%
% ***** Things need to change: *********
fname='uel-Patch1'; %filename
tnelm=9; % no. of elements in the mesh
xmin=-2; % axis limits
xmax=2;
ymin=-2;
ymax=2;
% output stress color bar limit
smin=0;
smax=30;
% output stress terms (1: sigma_xx; 2: sigma_yy; 3: sigma_xy)
sdir=1;

ndim=2; % dimension
nst=3; %no. of stresses
mxnd=4; % max no. of nodes in 2d element
nndr=4; % no. of material(real) nodes in the parent element
ncrack=1; % no. of cohesive cracks modelled in the parent element
nig=2; % no. of integration points on cohesive element
nsub=5; % no. of sub-elms after break
nsdv0=18; % no. of sdv to read over without storing
nsdv=153; % total no. of sdv
freq=1; % frequency of sdv output


%------------ Defined based on above info -----------------------------
ndofr=nndr*ndim; % d.o.f per parent element

file1=strcat(fname,'.dat');
file2=strcat(fname,'.sta');
videoname=strcat(fname,'.avi'); % name of the video for output

% Open files
fid = fopen(file1, 'r');
fid2 = fopen(file2, 'r');

% Define empty newline for newline identification later on
newline = sprintf('\r\n');
newline2 = sprintf('\n');


%**********************************************************************
%*************** Read .sta file for no. of increments *****************
%**********************************************************************
% Read over non-useful stuff
for i=1:5,
    tline=fgets(fid2);
end
% Read through for no. of increments
ninc=0;
tline=fgets(fid2);
while ~strcmp('T',tline(2)),% before the last line
    if ~strcmp('U',tline(15)),% be careful of multiple attempts
    ninc=ninc+1;
    end
    tline=fgets(fid2);
end 
ninc=ninc-1;

% Row number: number of increments recorded
nrow=ceil(ninc/freq);


%----------------------------------------------------------------------
%------------ Initialize arrays ---------------------------------------
%----------------------------------------------------------------------
% Initialize sdv
sdv=zeros(nrow,nsdv-nsdv0,tnelm);
% Initialize kinc
kinc=zeros(nrow,1);
% Initialize step time and total time array (currently the same for 1 step
% analysis)
tstep=zeros(nrow,1);
ttotal=zeros(nrow,1);
% Initialize x and y coordinate matrices
xcor=zeros(nrow,(nsub-ncrack)*(mxnd+1),tnelm);
ycor=zeros(nrow,(nsub-ncrack)*(mxnd+1),tnelm);
xtemp=zeros(mxnd+1,1);
ytemp=zeros(mxnd+1,1);
% Initialize array for failure status in the elm at each loading step
fstat=zeros(nrow,tnelm);
% Initialize array for the no. of sub-elms at each step in the element
ksub=zeros(nrow,tnelm);
% Initialize integration point coordinate matrices
xgpt=zeros(nrow,nig*ncrack,tnelm);
ygpt=zeros(nrow,nig*ncrack,tnelm);
% Initialize damage index at gauss points
dm=zeros(nrow,nig*ncrack,tnelm);
% Initialize stress arrays
sigma=zeros(nrow,(nsub-ncrack)*nst,tnelm);


%*********************************************************************
%****************** Read SDVs from .dat file *************************
%*********************************************************************

% Read over non-useful lines
tline=fgets(fid);
while (length(tline)<9||~strcmp('(6)',tline(7:9))),
    tline=fgets(fid);
end
for i=1:2,
    tline=fgets(fid);
end

%row number for sdv, irow th increment output
irow=1;

while irow<=nrow,

%****************** Read information for one increment ***************

%---------------------- Read for increment number --------------------
% read a line
tline=fgets(fid); 
% convert this line into a continuous string with no space in between
inc=sscanf(tline,'%s');
% find the part of the string that contains the number of increment
kincs=inc(10:length(inc)-7);
% convert ninc into an integer
kinc(irow)=sscanf(kincs,'%u');

%---------- Read for step time and total time completed -----------------
% read over empty lines
for i=1:2,
    tline=fgets(fid);
end
% read over time increment info (use it if needed)
tline=fgets(fid);
% read step time & total time
tline=fgets(fid);
tline=sscanf(tline,'%s');
% find the position of the comma
i=1;
while ~strcmp(',',tline(i)),
    i=i+1;
end
% find the step time completed
tsteps=tline(18:i-1);
tstep(irow)=sscanf(tsteps,'%E');
% find the total time completed
ttotals=tline(i+19:length(tline));
ttotal(irow)=sscanf(ttotals,'%E');
% Read over non-useful lines
for i=1:8,
    tline=fgets(fid);
end

%---------- Read for SDVs -------------------------------------------
isdv=0;
% Read all the full rows
while ((isdv+9)<=nsdv),
	% Read over non-useful lines
	for i=1:7,
    	tline=fgets(fid);
	end
    % Read first elm's SDVs
    tline=fgets(fid);
    if(~strcmp(newline,tline)&&~strcmp(newline2,tline)), % not empty line
        if(isdv<nsdv0), %SDVs to be ignored
            % Read over other elms
            tline=fgets(fid);
            while(~strcmp(newline,tline)&&~strcmp(newline2,tline)), % not empty line
                tline=fgets(fid);
            end % when it reaches an empty line
            % Read over non-useful lines
            for i=1:7,
                tline=fgets(fid);
            end
        else  % SDVs to be recorded  
            % Record first elm's SDVs
            tline=sscanf(tline,'%E');
            for i=1:9,
                %sdv(irow,isdv-nsdv0+i,1)=tline(2+i);
                sdv(irow,isdv-nsdv0+i,tline(1))=tline(2+i);
            end
            % Record following elms' SDVs
            if(tnelm >= 2),
                %ielm=2;
                tline=fgets(fid);
                while(~strcmp(newline,tline)&&~strcmp(newline2,tline)), % not empty line                 
                    tline=sscanf(tline,'%E');
                    for j=1:9,
                        %sdv(irow,isdv-nsdv0+j,ielm)=tline(2+j);
                        sdv(irow,isdv-nsdv0+j,tline(1))=tline(2+j);
                    end
                    tline=fgets(fid);
                    %ielm=ielm+1;
                end
            else
                tline=fgets(fid); % read over empty line
            end
            % Read over non-useful lines
            for i=1:7,
                tline=fgets(fid);
            end
        end
    else % empty line
        % read over non-useful rows
        for i=1:3,
            tline=fgets(fid);
        end      
    end
    isdv=isdv+9;     
end
% Read the remaining non-full rows if any
if(isdv<nsdv),
	% Read over non-useful lines
	for i=1:7,
    	tline=fgets(fid);
	end
    % Read SDVs
    tline=fgets(fid);
    if(~strcmp(newline,tline)&&~strcmp(newline2,tline)), % not empty line
        tline=sscanf(tline,'%E');
        for i=1:(nsdv-isdv),
            sdv(irow,isdv-nsdv0+i,1)=tline(2+i);
        end
        % Record following elms' SDVs
        for ielm=2:tnelm,
            tline=fgets(fid);
            tline=sscanf(tline,'%E');
            for j=1:(nsdv-isdv),
                sdv(irow,isdv-nsdv0+j,ielm)=tline(2+j);
            end
        end
        % Read over non-useful lines
    	for i=1:8,
        	tline=fgets(fid);
    	end
    else % empty line
        % read over non-useful rows
        for i=1:3,
            tline=fgets(fid);
        end
    end
    isdv=nsdv;    
end

% Update row number
irow=irow+1;

end


%*********************************************************************
%****************** Plot deformation and failure *********************
%*********************************************************************
% writerObj=VideoWriter(videoname);
% writerObj.FrameRate = 1; % must be placed before open
% writerObj.Quality = 100;
% open(writerObj);
   
scrsz = get(0,'ScreenSize');
figure('Position',[0 0 scrsz(3) scrsz(4)]);
set(gca, 'nextplot','replacechildren');
%******* Figure properties ************************************************
         axis image
         axis off % remove all axis labels
%       set axis limits so that axis unit scale will not change with each frame
%       it's also important for writeVideo to work properly
        xlim([xmin,xmax])
        ylim([ymin,ymax])
%       The above property commands only work when placed after figure/fill

% Obtain x and y coordinate matrices
for irow=1:nrow, % no. of increments recorded
    for ielm=1:tnelm,
      nndpr=sdv(irow,1,ielm); % 1st sdv for 1st elm
      if (nndpr > 100), % parent elm intact
          fstat(irow,ielm)=0;
          ksub(irow,ielm)=1;
        for isb=1:ksub(irow,ielm), % no. of sub-elements
          ksdv=(isb-1)*18+1; % count of no. of sdvs
          knds=(isb-1)*(mxnd+1); % count of no. of nodes
          for inds=1:mxnd,
              xcor(irow,knds+inds,ielm)=sdv(irow,ksdv+1,ielm);
              ycor(irow,knds+inds,ielm)=sdv(irow,ksdv+2,ielm);
              ksdv=ksdv+2;
          end
          % final coords to form a closed plot 
          xcor(irow,knds+(mxnd+1),ielm)=xcor(irow,knds+1,ielm);
          ycor(irow,knds+(mxnd+1),ielm)=ycor(irow,knds+1,ielm);
          % stress
          ksdv2=(isb-1)*18+9;
          sigma(irow,(isb-1)*nst+1,ielm)=sdv(irow,ksdv2+1,ielm);
          sigma(irow,(isb-1)*nst+2,ielm)=sdv(irow,ksdv2+2,ielm);
          sigma(irow,(isb-1)*nst+3,ielm)=sdv(irow,ksdv2+3,ielm);          
        end
      else % parent elm failed
          fstat(irow,ielm)=1;
          ksub(irow,ielm)=nsub;
        for isb=1:ksub(irow,ielm), % no. of sub-elements
          ksdv=(isb-1)*18+1;
          knds=(isb-1)*(mxnd+1); % count of no. of nodes
          nnds=sdv(irow,ksdv,ielm); % 1st sdv for sub elm isb        
          if(nnds > 0), % bulk sub-elm
            for inds=1:nnds,
                xcor(irow,knds+inds,ielm)=sdv(irow,ksdv+1,ielm);
                ycor(irow,knds+inds,ielm)=sdv(irow,ksdv+2,ielm);
                ksdv=ksdv+2;
            end
            % final coords to form a closed plot 
            xcor(irow,knds+(nnds+1),ielm)=xcor(irow,knds+1,ielm);
            ycor(irow,knds+(nnds+1),ielm)=ycor(irow,knds+1,ielm);
            % stress
            ksdv2=(isb-1)*18+9;
            sigma(irow,(isb-1)*nst+1,ielm)=sdv(irow,ksdv2+1,ielm);
            sigma(irow,(isb-1)*nst+2,ielm)=sdv(irow,ksdv2+2,ielm);
            sigma(irow,(isb-1)*nst+3,ielm)=sdv(irow,ksdv2+3,ielm);
          elseif(nnds<0), % coh sub-elm
            for m=1:ncrack,
                for n=1:nig,
                    xgpt(irow,(m-1)*nig+n,ielm)=sdv(irow,ksdv+1,ielm);
                    ygpt(irow,(m-1)*nig+n,ielm)=sdv(irow,ksdv+2,ielm);
                    ksdv=ksdv+2;
                end
                for n=1:nig,
                    dm(irow,(m-1)*nig+n,ielm)=sdv(irow,ksdv+1,ielm);
                    ksdv=ksdv+1;
                end
            end
          end
        end
      end
    end
end

  drow=1;
  for q=1:floor(nrow/drow),
      irow=q*drow;

% *** To keep the axis properties defined above:***
    set(gca, 'nextplot','replacechildren');
    
    colorbar('location','EastOutside');
    caxis([smin smax]);

% ------ Plot deformation shape ---------------------------------     
      for ielm=1:tnelm,
        if(fstat(irow,ielm)==0),% parent elm intact
           ksb=ksub(irow,ielm);
        else
           ksb=ksub(irow,ielm)-ncrack;
        end
        for isb=1:ksb, % loop over all bulk sub-elm
        ksdv=(isb-1)*18+1;
        nnds=sdv(irow,ksdv,ielm); % 1st sdv for sub elm isb   
        knds=(isb-1)*(mxnd+1);
        if(nnds>100),
            nnds=nnds-100;
        elseif(nnds<0)
            nnds=-nnds;
        end
        % store nodal coords in temporary x & y arrays
        xtemp(1:nnds+1)=xcor(irow,knds+1:knds+nnds+1,ielm);
        ytemp(1:nnds+1)=ycor(irow,knds+1:knds+nnds+1,ielm);
%       *** comment on the following: ***
%       plot gives only the outline
%       area gives wierd overlapping of element boundaries
%       Patch does not delete the previous graphs
%       fill is the best
%         fcolor='b';
         %fcolor=[0.9 0.9 0.9]; % Grey/white color
         fcolor=sigma(irow,(isb-1)*nst+sdir,ielm); % x-direction stress
         fill(xtemp(1:nnds+1),ytemp(1:nnds+1),fcolor,'LineWidth',0.2)
%       *** colorbar makes the program very very slow!***
%       colorbar('location','EastOutside');
%       caxis([0 1]);
        hold on
        end    
      end
% ------------------ Plot failure ---------------------------------   
      for ielm=1:tnelm,
        for ig=1:nig*ncrack, % loop over all integration points
            if dm(irow,ig,ielm)>0.0, % failure
            size=25;
%            size=100*dm(irow,i,ielm); % size of the ball grows with damage
            %color='r';
            color=dm(irow,ig,ielm); % color changes w.r.t damage level            
         scatter(xgpt(irow,ig,ielm),ygpt(irow,ig,ielm),size,color,'filled')
            end
        end
      end
  hold off

    frame=getframe; %add (gcf) to capture the entire figure
%    writeVideo(writerObj,frame);   
    if((irow==1)||(rem(irow,1)==0)),
    fgname=strcat(fname,'-');
    fgname=strcat(fgname,int2str(irow));
    % Print figure
    print('-dpdf',fgname);
    print('-depsc',fgname);
    end
 end

%close(writerObj);
close(figure);
fclose(fid);
fclose(fid2);

% Notification of finish
cf = 2000;                  % carrier frequency (Hz)
sf = 22050;                 % sample frequency (Hz)
d = 0.5;                    % duration (s)
n = sf * d;                 % number of samples
s = (1:n) / sf;             % sound data preparation
s = sin(2 * pi * cf * s);   % sinusoidal modulation
sound(s, sf);               % sound presentation
%pause(d + 0.5);             % waiting for sound end

%profile viewer;

fclose('all');
end

