function [] = fnm2dpreproc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Preprocessing for 3D linear brick FNM elmt %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clc
close('All')

%***************************************************************
%****************** Define useful variables ********************
%***************************************************************

% ***** Things need to change: *********
filename='Patch1'; %filename

elmname=2040; % just a random name, must be consistent with uel.f
ndim=2; % dimension
rnode=4; %original no. of (real)nodes in the element. (here, 8 for linear brick elmt)
%nply=1; %no. of plies
%layup=zeros(nply); % laminate lay-up
%layup=[45,-45]; % ply angles
nintfc=0; % no. of interfaces btw ply-blocks
int_fnode=0; % no. of internal f. nodes
nprops=15; % no. of input material properties used in uel code
nsvars=108; % no. of sol. dpdnt var. used and ouput by uel code; to be determined
intfc_fnode=nintfc*rnode; % no. of f. nodes used to include ply interfaces
elmedge=4*(1+nintfc); % no. of breakable edges (horizontal) in the fnm element
edge_fnode=2*elmedge; % no. of f. nodes needed for these edges; 2 f.nodes per edge
ttlnode=rnode+intfc_fnode+edge_fnode+int_fnode; % total nodes (original and floating) in this elmt

outfreq=1; % output frequency for SDVs in .dat file

% props array
props=zeros(nprops);
props(1)=100000.0; %E1
props(2)=10000.0; %E2
props(3)=10000.0; %E3
props(4)=5000.0; % G12
props(5)=5000.0; % G23
props(6)=5000.0; % G13
props(7)=0.3; % nu12
props(8)=0.3; % nu23
props(9)=0.3; % nu13
props(10)=0.0; % theta, orientation of the ply
props(11)=60.0; % Yt
props(12)=90.0; % S
props(13)=0.2; % Gnc
props(14)=1.0; % Gsc
props(15)=2.0; % Eta (BK law)


% Fortran modules to be written during pre-processing
% mdndelm: nodal connectivities of all elm
% mdedgelm: edge connectivities of all elm
% mdglbedg: global edge arrays
% mdrndedg: real node to edge matrix
% mdfndedg: floating node to edge arrays
modndelm=fopen('modndelm.f','w');
% mdedgelm=fopen('mdedgelm.f','w');
% mdglbedg=fopen('mdglbedg.f','w');
% mdrndedg=fopen('mdrndedg.f','w');
modfnode=fopen('modfnode.f','w');

fprintf(modndelm,'       module modndelm\n');
fprintf(modndelm,'       use modparam\n');
fprintf(modndelm,'       implicit none\n');

% fprintf(mdedgelm,'       module modndelm\n');
% fprintf(mdedgelm,'       use param\n');
% fprintf(mdedgelm,'       implicit none\n');
% 
% fprintf(mdglbedg,'       module modndelm\n');
% fprintf(mdglbedg,'       use param\n');
% fprintf(mdglbedg,'       implicit none\n');
% 
% fprintf(mdrndedg,'       module modndelm\n');
% fprintf(mdrndedg,'       use param\n');
% fprintf(mdrndedg,'       implicit none\n');

fprintf(modfnode,'       module modfnode\n');
fprintf(modfnode,'       use modparam\n');
fprintf(modfnode,'       implicit none\n');


% *********************************

% .inp file from Abaqus for reading
file1=strcat(filename,'.inp');
file2=strcat('uel-',file1);


%***************************************************************
%********** first read to find no. nodes & elms ****************
%***************************************************************

fid1 = fopen(file1, 'r');

%************************ Read nodes ***************************
% Read over non-useful lines
tline=fgets(fid1);
for i=1:8,
    tline=fgets(fid1);
end

nndmsh1=0; % no. of original nodes
tline=fgets(fid1);
while ~strcmp('*',tline(1)),% before the *Element
    nndmsh1=nndmsh1+1; % update no. of nodes
    tline=fgets(fid1);
end
%*********************** Read elements *************************
nelm1=0; % no. of elements
tline=fgets(fid1);
while ~strcmp('*',tline(1)),% before the *End Part
    nelm1=nelm1+1; % update no. of elms
    tline=fgets(fid1);
end
%**************** Define useful matrices ***********************
ndcoords=zeros(nndmsh1,ndim); % nodal coords real matrix
elmnodes=zeros(nelm1,ttlnode+1);
maxedge=elmedge*nelm1/2; %max no. of breakable edges in the mesh, each internal edge is shared by 2 elmts
glb_edge=zeros(maxedge,4); %global edge matrix; col.1&2: edge tip nodes; col.3&4: f.nodes for this edge 
lcl_edge=zeros(nelm1,elmedge);%local edge matrx; col.: glb edge no. of local edges
elmt_edge=zeros(elmedge,3);%local elmt edges of an elmt, each defined by 3 nodes, used only temporarily
nds_edge=zeros(nndmsh1,nndmsh1); %matrix containing edges formed by two arbitrary nodes

% newline = sprintf('\r\n'); % empty newline for newline identification later on
% newline2 = sprintf('\n');

fclose(fid1);

%***************************************************************
%***************************************************************
%***************************************************************


%***************************************************************
%************************ Read nodes ***************************
%***************************************************************

fid = fopen(file1, 'r'); %r+ for reading and writing
fid2 = fopen(file2, 'w');

% Read over non-useful lines
tline=fgets(fid);
fprintf(fid2,'%s',tline);
for i=1:6,
    tline=fgets(fid);
    fprintf(fid2,'%s',tline);
end
    tline=fgets(fid);
    fprintf(fid2,'%s',tline);
    partname=tline(13:length(tline)-1);
    tline=fgets(fid);
    fprintf(fid2,'%s',tline);
nndmsh=0; % no. of original nodes
tline=fgets(fid);
while ~strcmp('*',tline(1)),% before the *Element
    nndmsh=nndmsh+1; % update no. of nodes
    % Store the real values of the nodal coords in ndcoords()
    % Re-read line as integers & real numbers.
    if (ndim==1)
        tline=sscanf(tline,'%u%c%f');
    elseif(ndim==2)
        tline=sscanf(tline,'%u%c%f%c%f'); 
    else
        tline=sscanf(tline,'%u%c%f%c%f%c%f');
    end    
    for i=1:ndim,
        ndcoords(nndmsh,i)=tline(2*i+1);
    end
%    ndcoords(nnodes,:)
    tline=fgets(fid);
end

%***************************************************************
%*********************** Read elements *************************
%***************************************************************
nelm=0; % no. of elements
tline=fgets(fid);
while ~strcmp('*',tline(1)),% before the *End Part
    nelm=nelm+1; % update no. of elms
    % Store the integer values of the nodes of the elm in elmnodes()
    tline=sscanf(tline,'%u%*c'); % Re-read line as integers and space char.
    % sscanf can read repetative formats until end of line.
    elmnodes(nelm,1)=rnode; % no. of real nodes in this elm
    for i=1:length(tline)-1, % first no. in tline is the elm no.
        elmnodes(nelm,i+1)=tline(i+1);
    end
%    elmnodes(nelm,:)

    tline=fgets(fid);
end


%***************************************************************
%**************** Find the original edges **********************
%***************************************************************

nedge=0; % no. of original edges defined now
    
for i=1:nelm, %looping over all elmt
    % find local elmt edges, their end nodes and store in elmt_edge matrix
    % w.r.t local edge index
    elmt_edge(1,1:2)=elmnodes(i,2:3); % local edge 1
    elmt_edge(2,1:2)=elmnodes(i,3:4); % local edge 2
    elmt_edge(3,1:2)=elmnodes(i,4:5); % local edge 3
    elmt_edge(4,1)=elmnodes(i,5); % local edge 4
    elmt_edge(4,2)=elmnodes(i,2); % local edge 4
%     elmt_edge(5,1:2)=elmnodes(i,6:7); % local edge 5
%     elmt_edge(6,1:2)=elmnodes(i,7:8); % local edge 6
%     elmt_edge(7,1:2)=elmnodes(i,8:9); % local edge 7
%     elmt_edge(8,1)=elmnodes(i,9); % local edge 8
%     elmt_edge(8,2)=elmnodes(i,6); % local edge 8

    % store the mid node on the edge
    elmt_edge(1,3)=elmnodes(i,6);
    elmt_edge(2,3)=elmnodes(i,7);
    elmt_edge(3,3)=elmnodes(i,8);
    elmt_edge(4,3)=elmnodes(i,9);
    
%     if nintfc > 0, % local edges 9 onwards, edges of the interfaces
%     for n=1:nintfc, 
%         for j=1:8,
%             elmt_edge(n*8+j,1)=elmt_edge(j,1)+n*nndmsh; % f.nodes for interface
%             elmt_edge(n*8+j,2)=elmt_edge(j,2)+n*nndmsh; % f.nodes for interface
%         end
%     end
%     end
    
    % fill in the glb_edge and lcl_edge matrices; glb_edge matrix stores
    % end nodes and f.nodes of all breakable edges according to their
    % global index; lcl_edge matrix stores global indices of the breakable
    % edges of a local element, a negative edge index indicates that the end nodes
    % of this edge in this elmt appears in the reverse order in the elmt connectivity
    % matrix, as compared to the order stored in the glb_edge matrix for
    % this edge.
    if(i==1) % original edges of 1st elmt, store them all in glb_edge matrix
       glb_edge(1:4,1:3)=elmt_edge(1:4,1:3);
       glb_edge(1:4,4)=glb_edge(1:4,3)+(1+nintfc)*nndmsh; % added 2nd fl. node on edge
       lcl_edge(i,1:4)=[1 2 3 4];
       nedge=4;
    else
        for j=1:4, % for local edge j of elmt i
            dupl=0; % initially, assume it's the 1st appearance of this edge
            for k=1:nedge, % compare with all existing edges in glb_edge matrix
                if(elmt_edge(j,1:2)==glb_edge(k,1:2)), % if this edge appeared before
                    dupl=1; % duplicated
                    lcl_edge(i,j)=k; % global index of local edge j of elmt i is k
                elseif((elmt_edge(j,1)==glb_edge(k,2))&&(elmt_edge(j,2)==glb_edge(k,1))), % if this edge appeared before
                    dupl=1; % duplicated
                    lcl_edge(i,j)=-k; % global index of local edge j of elmt i is -k ('-' for reverse ordering of nodes)
                end
            end
            if(dupl==0), % indeed 1st appearance of this edge
                nedge=nedge+1;
                glb_edge(nedge,1:3)=elmt_edge(j,1:3);
                glb_edge(nedge,4)=glb_edge(nedge,3)+(1+nintfc)*nndmsh;
                lcl_edge(i,j)=nedge;
            end
        end
    end
    
    % fill in the nds_edge matrix, to store the edge info w.r.t node index
    for j=1:4,
            row=glb_edge(abs(lcl_edge(i,j)),1); %node 1 of jth edge of elmt i
            col=glb_edge(abs(lcl_edge(i,j)),2); %node 2 of jth edge of elmt i
            nds_edge(row,col)=abs(lcl_edge(i,j)); % edge formed by nodes row and col
            nds_edge(col,row)=abs(lcl_edge(i,j)); % edge formed by nodes row and col
            
            row=glb_edge(abs(lcl_edge(i,j)),3); %node 3 of jth edge of elmt i
            %nds_edge(row,1)=-1; % a f. node in the mid of the edge
            nds_edge(row,1)=-glb_edge(abs(lcl_edge(i,j)),4); % the pair f. node 
            
    end
    
end


%***************************************************************
%******* write the intfc edges in glb and lcl matrices**********
%***************************************************************
% if nintfc > 0,
%     for i=1:nintfc,
%         for j=1:nedge,% write interface edge nodes in the glb array, nedge: no. of original edges
%             glb_edge(i*nedge+j,1)=glb_edge(nedge,1)+i*nndmsh;
%             glb_edge(i*nedge+j,2)=glb_edge(nedge,2)+i*nndmsh;
%         end
%         for k=1:nelm, % write the interface edge indices in the local array
%             for l=1:8,
%                 lcl_edge(k,i*8+l)=lcl_edge(k,l)+sign(lcl_edge(k,l))*i*nedge;
%             end
%         end
%     end
% end

% Assign 2 f. nodes to each edge in the glb_edge matrix

% fnode=(1+nintfc)*nndmsh; % glb node number starts from this value
% for i=1:(nintfc+1)*nedge,
%     glb_edge(i,4)=fnode+glb_edge(i,3);
% end
     
%***************************************************************
%************************ Write nodes **************************
%***************************************************************

% Write original nodes:
 for i=1:nndmsh,
     fprintf(fid2,' %u,',i);
     for j=1:ndim-1,
         fprintf(fid2,' %f,',ndcoords(i,j));
     end
     fprintf(fid2,' %f\n',ndcoords(i,ndim));
 end
% % Write f. nodes for interfaces:
% if(nintfc > 0)
%  for j=1:nintfc,
%     for i=1:nndmsh,
%         fprintf(fid2,' %u,',i+nndmsh*j);
%         for k=1:ndim-1,
%             fprintf(fid2,' %f,',0); % floating nodes' location doesn't matter
%         end
%         fprintf(fid2,' %f\n',0);
%     end
%  end
% end

%write f. nodes for breakable edges
for i=1:(nintfc+1)*nndmsh,
    fprintf(fid2,' %u,',i+(nintfc+1)*nndmsh);
    for k=1:ndim-1,
        fprintf(fid2,' %f,',0); % floating nodes' location doesn't matter
    end
    fprintf(fid2,' %f\n',0);      
    
    
end

% Write internal floating nodes:
% nodes listed according to elm no.
% eg. first 4 int. fl. nodes are for elm 1
cnt=2*(nintfc+1)*nndmsh;
if(int_fnode > 0)    
    for i=1:nelm,
    for k=1:int_fnode,    
          cnt=cnt+1;
          fprintf(fid2,' %u,',cnt);
          for m=1:ndim-1,
                fprintf(fid2,' %f,',0);
          end
          fprintf(fid2,' %f\n',0);    
    end
    end
end

%write to fortran module modfnode
fprintf(modfnode,'          real(kind=dp)::fnode(10,%u)\n',cnt);
fprintf(modfnode,'         contains\n');
fprintf(modfnode,'         subroutine initfnode()\n');
fprintf(modfnode,'          fnode(:,:)=zero\n');
% Write original nodes:
 for i=1:nndmsh,
     for j=1:ndim,
         fprintf(modfnode,'          fnode(%u,%u)=%f\n',j+1,i,ndcoords(i,j));
     end
 end
fprintf(modfnode,'         end subroutine initfnode\n');
% finish module modfnode
fprintf(modfnode,'       end module modfnode');


%***************************************************************
%*********************** Write elements ************************
%***************************************************************

%write to fortran module
fprintf(modndelm,'          integer::ndelm(%u,%u)\n',ttlnode,nelm);
fprintf(modndelm,'         contains\n');
fprintf(modndelm,'         subroutine initndelm()\n');
%fprintf(modndelm,'          integer,intent(inout)::ndelm(%u,%u)\n',ttlnode,nelm);
fprintf(modndelm,'          ndelm(:,:)=0\n');


% ------- Write User Element information -----------------------
fprintf(fid2,...
    '*USER ELEMENT, TYPE=U%u, NODES=%u, COORDINATES=%u, PROPERTIES=%u, VARIABLES=%u\n',...
    elmname, ttlnode, ndim, nprops, nsvars);
for i=1:ndim-1,
    fprintf(fid2,'%u,',i);
end
fprintf(fid2,'%u\n',ndim);

% ------- Write *Element ---------------------------------------
fprintf(fid2,'*Element, type=U%u, elset=whole\n',elmname);

cntint=2*(nintfc+1)*nndmsh; % counter for internal nodes
 for i=1:nelm,
    fprintf(fid2,' %u',i); % print elm no.
    cntnd=0; % counter for nodes in this elm
     % print real nodes, elmnodes(i,1) is the no. of orig. nds in elmt i
    for j=1:elmnodes(i,1),
        fprintf(fid2,', %u',elmnodes(i,j+1)); % print original nodes of elmt i
        fprintf(modndelm,'          ndelm(%u,%u)=%u\n',j,i,elmnodes(i,j+1));
        cntnd=cntnd+1;
    end
%      % print f. nodes for interfaces
%     if(nintfc > 0)
%     for k=1:nintfc, % print interface f. nodes
%         for j=1:elmnodes(i,1),
%             fprintf(fid2,', %u',k*nndmsh+elmnodes(i,j+1));
%         end
%     end
%     end
    % print f. nodes for edges, according to lcl_edge(i,j=1:elmedge), glb_edge(j,3:4)
    for j=1:elmedge,
        if(lcl_edge(i,j)>0), % edge end nodes are ordered the same as in the glb_edge matrix
            fprintf(fid2,', %u', glb_edge(lcl_edge(i,j),3));
            fprintf(fid2,', %u', glb_edge(lcl_edge(i,j),4));
            fprintf(modndelm,'          ndelm(%u,%u)=%u\n',cntnd+1,i,glb_edge(lcl_edge(i,j),3));
            fprintf(modndelm,'          ndelm(%u,%u)=%u\n',cntnd+2,i,glb_edge(lcl_edge(i,j),4));
            cntnd=cntnd+2;
        else % edge end nodes are ordered in reverse as compared to that in the glb_edge matrix
            fprintf(fid2,', %u', glb_edge(-lcl_edge(i,j),4));
            fprintf(fid2,', %u', glb_edge(-lcl_edge(i,j),3));
            fprintf(modndelm,'          ndelm(%u,%u)=%u\n',cntnd+1,i,glb_edge(-lcl_edge(i,j),4));
            fprintf(modndelm,'          ndelm(%u,%u)=%u\n',cntnd+2,i,glb_edge(-lcl_edge(i,j),3));
            cntnd=cntnd+2;
        end
    end
    % print internal f. nodes
    if(int_fnode > 0)
        for k=1:int_fnode, 
            cntint=cntint+1;
            fprintf(fid2,', %u',cntint);
            
            cntnd=cntnd+1;
            fprintf(modndelm,'          ndelm(%u,%u)=%u\n',cntnd,i,cntint);
        end
    end
    
    fprintf(fid2,'\n');
 end 
 
 % finish module modndelm
 fprintf(modndelm,'         end subroutine initndelm\n');
 fprintf(modndelm,'       end module modndelm');
 
 
 
% --------- Write UEL properties --------------------------------
fprintf(fid2,'*UEL PROPERTY, ELSET=whole\n');
cntprops=0;
for i=1:nprops,
    cntprops=cntprops+1;
   if (cntprops < 8)
       fprintf(fid2,' %f,',props(i));
   else
       fprintf(fid2,' %f,\n',props(i));
       cntprops=0;
   end
end
if(cntprops==0)
    fprintf(fid2,'*End Part\n');
else
    fprintf(fid2,'\n*End Part\n');
end
fprintf(fid2,'\n');

%***************************************************************
%*************** Modify nsets  *********************************
%***************************************************************
tline=fgets(fid);
while ((length(tline)<13)||(~strcmp('*End Assembly',tline(1:13)))),   
    if((length(tline)<6)||(~strcmp('*Nset,',tline(1:6)))),% not about node set
        fprintf(fid2,'%s',tline); % re-print it
        tline=fgets(fid);
    else
        fprintf(fid2,'%s',tline); % re-print set definition line
        tline=fgets(fid);
        while(~strcmp('*',tline(1))), % while still reading nodes
            fprintf(fid2,'%s',tline); % re-print original nodes
            tline=sscanf(tline,'%u%*c'); % Re-read line as integers and comma char.
            lgth=length(tline);
            for k=1:lgth,            
                if(nds_edge(tline(k),1)<0), %if it is a node in the mid of the edge
                    % add in its f. node pair on the same edge
                    fprintf(fid2,' %u,',abs(nds_edge(tline(k),1)));
                end                
            end
            fprintf(fid2,'\n');
            tline=fgets(fid);
        end
        
    end
end
fprintf(fid2,'%s',tline);

%----------- Copy *STEP -------------------------------------
while ((length(tline)<5)||(~strcmp('*Step',tline(1:5)))),
    tline=fgets(fid);
    fprintf(fid2,'%s',tline);
end
for i=1:2,
    tline=fgets(fid);
    fprintf(fid2,'%s',tline);
end

%----------- Write *Control parameters ----------------------
fprintf(fid2,'*CONTROLS, PARAMETERS=TIME INCREMENTATION\n');
fprintf(fid2,'1600, 2000, , 3000, 2400, 50, , 125, , , \n');

%----------- Copy *BOUNDARY CONDITIONS ----------------------
while ((length(tline)<9)||(~strcmp('*Restart,',tline(1:9)))),
    tline=fgets(fid);
    fprintf(fid2,'%s',tline);
end

%----------- Write SDV output to .dat file ------------------
fprintf(fid2,'**\n');
fprintf(fid2,'*EL PRINT,elset=%s-1.whole, FREQ=%u\n',partname,outfreq);
fprintf(fid2,'SDV,\n');

% ---------- Copy the rest ---------------------------------
while ~feof(fid),
    tline=fgets(fid);
    fprintf(fid2,'%s',tline);
end

fclose(fid);
fclose(fid2);

fclose(modndelm);
% fclose(mdedgelm);
% fclose(mdglbedg);
% fclose(mdrndedg);
fclose(modfnode);

end

