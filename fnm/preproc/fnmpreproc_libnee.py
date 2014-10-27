################################################################
################# Preprocessing for the FNM ####################
##  writing lib_node, lib_edge and lib_elem modules           ##
################################################################

from fnmclasses import* # glb objects defined for FNM
import math


#***************************************************************
#   fetch Abaqus input files
#***************************************************************

jobname=raw_input('abaqus job name:')

abqinputfile=jobname+'.inp'         # abaqus input file
fnminputfile='fnm-'+jobname+'.inp'  # fnm uel input file



#***************************************************************
#   define model information
#***************************************************************

ndim=3          # dimension

nprops=1        # no. of input material properties used in uel code (min 1)
nsvars=1        # no. of sol. dpdnt var. used and ouput by uel code to be determined (min 1)


#***************************************************************
#   Open Fortran modules to be written during pre-processing
#***************************************************************
lib_node=open('init_lib_node.f90','w')  # array of all nodes
lib_edge=open('init_lib_edge.f90','w')  # array of all edges
lib_elem=open('init_lib_elem.f90','w')  # array of all elements




#***************************************************************
#       write lib_node_module.f90
#***************************************************************    
lib_node.write('    subroutine initialize_lib_node()          \n')
lib_node.write('                                              \n')
lib_node.write('        integer :: nnode=0                    \n')   
lib_node.write('        integer :: i=0                        \n')





#***************************************************************
#       write lib_edge_module.f90
#***************************************************************
lib_edge.write('    subroutine initialize_lib_edge()          \n')
lib_edge.write('                                              \n')
lib_edge.write('        integer :: nedge=0                    \n')
lib_edge.write('        integer :: i=0                        \n')
lib_edge.write('                                              \n')







#***************************************************************
#       write lib_elem_module.f90
#***************************************************************    
lib_elem.write('    subroutine initialize_lib_elem()                                      \n')
lib_elem.write('                                                                          \n') 
lib_elem.write('        integer ::  nelem=0, ntri=0, nquad=0, nwedge=0, nbrick=0 &        \n')
lib_elem.write('        &          ,ncoh2d=0, ncoh3d6=0, ncoh3d8=0, nsub2d=0, nsub3d=0 &  \n')
lib_elem.write('        &          ,nxquad=0, nxbrick=0                                   \n')
lib_elem.write('        integer :: i=0                                                    \n')









#***************************************************************
#       open input files for I/O
#***************************************************************
abqinp=open(abqinputfile,'r')
fnminp=open(fnminputfile,'w')


#***************************************************************
#       define usefule variables
#***************************************************************
nodes=[]    # list of all nodes in the mesh
edges=[]    # list of all breakable edges in the mesh
elems=[]    # list of all elements in the mesh
ndedg=[]    # matrix of node_i-to-node_j edge no. 
eltype=[]   # list of all element types in the mesh
elcount=[]  # count of elements of each type in the mesh

bcd=[]      # list of all edges with b.c.d

rawlayup=[]    # layup of the laminate in its basic format (list of fibre angles of all plies)
blklayup=[]    # layup of ply blocks, used in xlam element; an xlayup class is needed


# ask for layup from user

# ask for no. of plies
nply=input('number of plies for xlam element (must be integer, 0 if not applicable):')
# check if nply is integer
while (not isinstance(nply,int)):
    nply=input('number of plies must be integer, re-enter (0 if not applicable):')

# ask for layups if nply >0
if (nply>0):
    # ask for a list of ply angles
    rawlayup=input('layup of laminate is (fibre angle (int/float) of each ply, separated by comma):')
    # check if it is a list and if all elements in the list are numbers
    while ((not isinstance(rawlayup, (list,tuple))) or (not all(isinstance(item, (int,float)) for item in rawlayup))):
        rawlayup=input('layup of laminate is (fibre angle (int/float) of each ply, separated by comma):')      
        
    # find blocked plies and update blklayup
    blklayup.append(xlayup(angle=rawlayup[0],ratio=1.0/nply)) # initiate blklayup
    for i in range(1,nply):
        if (rawlayup[i]==blklayup[-1].angle):
            blklayup[-1].ratio+=1.0/nply
        else:
            blklayup.append(xlayup(angle=rawlayup[i],ratio=1.0/nply))
        



#***************************************************************
#       Read & store nodes and elements, find edges
#***************************************************************
lines=abqinp.readlines()
isheader=True
isnode=False
iselem=False
cycle=False

for ln, line in enumerate(lines):
    cycle=False
    
    if(len(line)>=5 and line[0:5]=='*Node'):
        isheader=False
        isnode=True
        cycle=True

    elif(len(line)>=8 and line[0:8]=='*Element'):
        isnode=False
        iselem=True
        cycle=True
        
        if 'C2D3' in line:
            eltype.append('xtri')
            elcount.append(0)
        elif 'C2D4' in line:
            eltype.append('xquad')
            elcount.append(0)
        elif 'COH2D4' in line:
            eltype.append('coh2d')  # change to x version in future
            elcount.append(0)
        elif ('C3D6' in line) or ('SC6' in line):
            eltype.append('xwedge')
            elcount.append(0)
        elif ('C3D8' in line) or ('SC8' in line):
            eltype.append('xbrick')
            elcount.append(0)
        elif 'COH3D6' in line:
            eltype.append('coh3d6') # change to x version in future
            elcount.append(0)
        elif 'COH3D8' in line:
            eltype.append('coh3d8') # change to x version in future
            elcount.append(0)
        else:
            print 'warning: abaqus element type in line:', ln, 'is not supported in fnm.'

    elif(len(line)>=9 and line[0:9]=='*End Part'):
        iselem=False
      
    if not cycle:
        if isheader:
            fnminp.write(line)
        elif isnode:
        # store nodes first
            l = []
            for t in line.split(','):
                try:
                    l.append(float(t))
                except ValueError:
                    pass

            if ndim==2:
                nodes.append(node(x=l[1], y=l[2], z=0.0))
            else:
                nodes.append(node(x=l[1], y=l[2], z=l[3]))  

            # append the ndedg row to the correct length (nnode)
            ndedg.append([]) 
            
            # append the ndedg col to the correct length (nnode)
            for i in range(len(ndedg)):
                ndedg[i]=[0]*len(ndedg)
            
        elif iselem:
        # build edges, store elements' nodecnc and edgecnc
            
            # increase the elem count of the latest elem type
            elcount[-1]+=1
        
            l = []
            for t in line.split(','):
                try:
                    l.append(int(t))
                except ValueError:
                    pass
 
            id=l[0]
            nds=l[1:]
            
            elems.append(element(index=id,nodes=nds,edges=[]))
            
            if ndim==2:
            # in 2D, all edges are breakable
                for i in range(len(nds)):
                    row=nds[i]-1
                    
                    if i==len(nds)-1:
                        col=nds[0]-1
                    else:
                        col=nds[i+1]-1
                        
                    if ndedg[row][col]==0:
                    # this pair of nodes hasn't been assigned to an edge
                        fn1=len(nodes)                          # indices of 2 fl. nodes on this new edge
                        fn2=fn1+1
                        for nf in range(2):                     # create floating nodes on this edge
                            nodes.append(node(0.0,0.0,0.0))
                            
                        edges.append(edge(nodes=[row+1,col+1,fn1+1,fn2+1]))     # create this edge (for output in Fortran)
                        ndedg[row][col]=len(edges)              # fill the new edge index in the ndedg matrix
                        ndedg[col][row]=-(len(edges))           # nodes in rev. order makes the same edge in rev. dir.
                        elems[-1].edges.append(ndedg[row][col]) # append this edge no. in this elem
                    else:
                    # this pair of nodes has been assigned to an edge
                        elems[-1].edges.append(ndedg[row][col]) # append this edge no. in this elem
                        
            else:
            # in 3D FNM for composites, only edges parallel to shell plane are breakable
                for j in range(2):
                    for i in range(len(nds)/2):
                        row=nds[i+j*len(nds)/2]-1
                        
                        if i==len(nds)/2-1:
                            col=nds[j*len(nds)/2]-1
                        else:
                            col=nds[i+1+j*len(nds)/2]-1
                            
                        if ndedg[row][col]==0:
                        # this pair of nodes hasn't been assigned to an edge
                            fn1=len(nodes)                          # indices of 2 fl. nodes on this new edge
                            fn2=fn1+1
                            for nf in range(2):                     # create floating nodes on this edge
                                nodes.append(node(0.0,0.0,0.0))
                                
                            edges.append(edge(nodes=[row+1,col+1,fn1+1,fn2+1]))     # create this edge
                            ndedg[row][col]=len(edges)              # fill the new edge index in the ndedg matrix
                            ndedg[col][row]=-(len(edges))           # nodes in rev. order makes the same edge in rev. dir.
                            elems[-1].edges.append(ndedg[row][col]) # append this edge no. in this elem
                        else:
                        # this pair of nodes has been assigned to an edge
                            elems[-1].edges.append(ndedg[row][col]) # append this edge no. in this elem            
            
        else:
            break
            #fnminp.write(line)


#***************************************************************        
#       Write Nodes
#***************************************************************
fnminp.write('*Node\n')
lib_node.write('        nnode='+str(len(nodes))+'   \n')
lib_node.write('        allocate(lib_node(nnode))   \n')

for cntr0, nd in enumerate(nodes):
    cntr=cntr0+1
    if ndim==2:
        fnminp.write(str(cntr)+', '+str(nd.x)+', '+str(nd.y)+'\n')
        lib_node.write('        call update(lib_node('+str(cntr)+'),x=['+str(nd.x)+'_dp,'+str(nd.y)+'_dp],u=[zero,zero])\n')
    else:
        fnminp.write(str(cntr)+', '+str(nd.x)+', '+str(nd.y)+', '+str(nd.z)+'\n')
        lib_node.write('        call update(lib_node('+str(cntr)+'),x=['+str(nd.x)+'_dp,'+str(nd.y)+'_dp,'+str(nd.z)+'_dp],u=[zero,zero,zero])\n')


#***************************************************************        
#       Write Edges
#***************************************************************
lib_edge.write('        nedge='+str(len(edges))+'   \n')
lib_edge.write('        allocate(lib_edge(nedge))   \n')
lib_edge.write('        lib_edge=0                  \n')

    
#***************************************************************        
#       Write Elements
#***************************************************************
nelem=sum(elcount)
lib_elem.write('        nelem='+str(nelem)+'            \n')
lib_elem.write('        allocate(lib_elem(nelem))       \n')


for eli, elt in enumerate(eltype):

    # determine no. of nodes and uel element integer type
    # uel element integer type:
    #   1st digit: ndim
    #   2nd digit: 0: bulk elem; 1: coh elem; 2: x elem
    #   3rd onwards: no. of real nodes
    if ndim == 2:
        if elt == 'xtri':
            nndrl=3         # no. of (real)nodes in the element. (here, 8 for linear brick elmt)
            nedge=3         # no. of breakable edges in this element
            nndfl=2*nedge
            nnode=nndrl+nndfl
            eltuel=223
        elif elt == 'xquad':
            nndrl=4         # no. of (real)nodes in the element. (here, 8 for linear brick elmt)
            nedge=4         # no. of breakable edges in this element
            nndfl=2*nedge
            nnode=nndrl+nndfl
            eltuel=224
        elif elt == 'coh2d':
            nnode=4
            eltuel=214
    elif ndim == 3:
        if elt == 'xwedge':
            nndrl=6         # no. of (real)nodes in the element. (here, 8 for linear brick elmt)
            nedge=6         # no. of breakable edges in this element
            nndfl=2*nedge
            nnode=nndrl+nndfl
            eltuel=326
        elif elt == 'xbrick':
            nndrl=8         # no. of (real)nodes in the element. (here, 8 for linear brick elmt)
            nedge=8         # no. of breakable edges in this element
            nndfl=2*nedge
            nnode=nndrl+nndfl
            eltuel=328
        elif elt == 'coh3d6':
            nnode=6
            eltuel=316
        elif elt == 'coh3d8':
            nnode=8
            eltuel=318
    
    # write user element definition in abaqus fnm input file
    fnminp.write('*USER ELEMENT, TYPE=U'+str(eltuel)+', NODES='+str(nnode)+', COORDINATES='+str(ndim)+
        ', PROPERTIES='+str(nprops)+', VARIABLES='+str(nsvars)+'\n')
    if ndim==2:
        fnminp.write('1,2\n')
    else:
        fnminp.write('1,2,3\n')
      
    # allocate lib_elem in fnm lib_elem module
    lib_elem.write('        n'+elt+'='+str(elcount[eli])+'     \n')
    lib_elem.write('        allocate(lib_'+elt+'(n'+elt+'))   \n')

    # write elem nodal connec in abaqus fnm input file
    fnminp.write('*Element, type=U'+str(eltuel)+', elset=uel_'+elt+'\n')
    
    # find start and end elem indices for elements of the same type
    if eli == 0:
        elstart=0
    else:
        elstart=sum(elcount[:eli])
        
    elend=elstart+elcount[eli]
    
    
    # write elem nodal connec of elements of the same type
    for cntr0, el in enumerate(elems[elstart:elend]):
        cntr=cntr0+1
        
        # elem line to be printed in abaqus fnm input file
        #eline=str(cntr+elstart) 
        eline=str(el.index)
        
        # elem node and edge cnc to be printed in fnm lib_elem module
        nodecnc=''
        edgecnc=''
        
        # include real nodes in lists
        for k in el.nodes:
            eline=eline+', '+str(k)
            nodecnc=nodecnc+str(k)+','
            
        # include edge fl. nodes in lists for x-version elements
        if (elt[0] == 'x'):    
            for m in el.edges:
                edgecnc=edgecnc+str(abs(m))+','
                if m>0:     # elem edge's endnodes are in the same order as the definition of the edge
                    eline=eline+', '+str(edges[m-1].nodes[2])+', '+str(edges[m-1].nodes[3])
                    nodecnc=nodecnc+str(edges[m-1].nodes[2])+', '+str(edges[m-1].nodes[3])+','
                elif m<0:   # elem edge's endnodes are in the reverse order as the definition of the edge
                    eline=eline+', '+str(edges[-m-1].nodes[3])+', '+str(edges[-m-1].nodes[2])
                    nodecnc=nodecnc+str(edges[-m-1].nodes[3])+', '+str(edges[-m-1].nodes[2])+','
                else:
                    print 'warning: edges in elem ',cntr,' are empty'
        
        # wrap up eline and print in abaqus fnm input file
        eline=eline+'\n'
        fnminp.write(eline)
        
        
        # write node cnc and edge cnc (for x version elems only) to the respective type arrays in fnm lib_elem module
        if (elt[0] == 'x'):
            lib_elem.write('        call prepare(lib_'+elt+'('+str(cntr)+'),key='+str(el.index)+', & \n')
            #lib_elem.write('& nodecnc=['+nodecnc[:-1]+'], & \n')
            if len(nodecnc) <= 100:
                lib_elem.write('& nodecnc=['+nodecnc[:-1]+'], & \n')
            else:
                
                for ic, c in enumerate(nodecnc):
                    if ic >= 90 and c==',':
                        break
                lib_elem.write('& nodecnc=['+nodecnc[:ic]+' & \n')
                lib_elem.write('& '+nodecnc[ic:-1]+'], & \n')

            lib_elem.write('& edgecnc=['+edgecnc[:-1]+'], & \n')
            lib_elem.write('& bulkmat=1, cohmat=5 ) \n')    # update matkeys later accord. to mat section assignment
        else:
            lib_elem.write('        call prepare(lib_'+elt+'('+str(cntr)+'),key='+str(el.index)+', & \n')
            lib_elem.write('& connec=['+nodecnc[:-1]+'], & \n')
            lib_elem.write('& matkey=1 ) \n')   # update matkey later accord. to mat section assignment
        
        # write elem type and typekey (elem index in its own type array) 
        lib_elem.write('        call update(lib_elem('+str(el.index)+'),elname="'+elt+'",eltype="'+elt+'",typekey='+str(cntr)+') \n')
        lib_elem.write('\n')

    # print the mandatory uel property line (not needed for calculation)
    fnminp.write('*UEL PROPERTY, elset=uel_'+elt+'\n')
    fnminp.write('1\n')


#***************************************************************        
#       Write the rest
#***************************************************************

for line in lines[ln:]:
    fnminp.write(line)





#   close lib_node_module.f90
lib_node.write('    end subroutine initialize_lib_node        \n')
lib_node.close()
#   close lib_node_module.f90
lib_edge.write('    end subroutine initialize_lib_edge        \n')
lib_edge.close()
#   close lib_elem_module.f90
lib_elem.write('    end subroutine initialize_lib_elem        \n')
lib_elem.close()


