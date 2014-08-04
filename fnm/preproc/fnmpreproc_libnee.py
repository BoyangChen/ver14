################################################################
################# Preprocessing for the FNM ####################
##  writing lib_node, lib_edge and lib_elem modules           ##
################################################################

from fnmclasses import* # glb objects defined for FNM
import math


#***************************************************************
#   fetch Abaqus input files
#***************************************************************

jobname='Patch1'

abqinputfile=jobname+'.inp'         # abaqus input file
fnminputfile='fnm-'+jobname+'.inp'  # fnm uel input file



#***************************************************************
#   define model information
#***************************************************************

ndim=3          # dimension

nprops=0        # no. of input material properties used in uel code
nsvars=0        # no. of sol. dpdnt var. used and ouput by uel code to be determined


#***************************************************************
#   Open Fortran modules to be written during pre-processing
#***************************************************************
lib_node=open('lib_node_module.f90','w')  # array of all nodes
lib_edge=open('lib_edge_module.f90','w')  # array of all edges
lib_elem=open('lib_elem_module.f90','w')  # array of all elements




#***************************************************************
#       write lib_node_module.f90
#***************************************************************
lib_node.write('    !***************************************! \n')
lib_node.write('    !   the global library of nodes         ! \n')
lib_node.write('    !***************************************! \n')
lib_node.write('                                              \n')
lib_node.write('    include "globals/xnode_module.f90"        \n')
lib_node.write('                                              \n')   
lib_node.write('    module lib_node_module                    \n')
lib_node.write('    use parameter_module                      \n')
lib_node.write('    use xnode_module                          \n')
lib_node.write('                                              \n')   
lib_node.write('    implicit none                             \n')
lib_node.write('    save                                      \n')
lib_node.write('                                              \n')   
lib_node.write('    type(xnode),allocatable :: lib_node(:)    \n')
lib_node.write('                                              \n')   
lib_node.write('    contains                                  \n')
lib_node.write('                                              \n')    
lib_node.write('    subroutine initialize_lib_node()          \n')
lib_node.write('                                              \n')
lib_node.write('        integer :: nnode=0                    \n')   
lib_node.write('        integer :: i=0                        \n')





#***************************************************************
#       write lib_edge_module.f90
#***************************************************************
lib_edge.write('    !***************************************! \n')
lib_edge.write('    !   the global library of edges         ! \n')
lib_edge.write('    !***************************************! \n')
lib_edge.write('                                              \n')    
lib_edge.write('    module lib_edge_module                    \n')
lib_edge.write('    use parameter_module                      \n')
lib_edge.write('                                              \n')
lib_edge.write('    implicit none                             \n')
lib_edge.write('    save                                      \n')
lib_edge.write('                                              \n')
lib_edge.write('    integer,allocatable :: lib_edge(:)        \n')
lib_edge.write('                                              \n')
lib_edge.write('    contains                                  \n')
lib_edge.write('                                              \n')
lib_edge.write('    subroutine initialize_lib_edge()          \n')
lib_edge.write('                                              \n')
lib_edge.write('        integer :: nedge=0                    \n')
lib_edge.write('        integer :: i=0                        \n')
lib_edge.write('                                              \n')







#***************************************************************
#       write lib_elem_module.f90
#***************************************************************
lib_elem.write('    !***************************************!                             \n')
lib_elem.write('    !   the global library of elements      !                             \n')
lib_elem.write('    !***************************************!                             \n')
#if(ndim==2)
lib_elem.write('    include "elements/tri_element_module.f90"                             \n')
lib_elem.write('    include "elements/quad_element_module.f90"                            \n')
lib_elem.write('    include "elements/coh2d_element_module.f90"                           \n')
lib_elem.write('    include "elements/sub2d_element_module.f90"                           \n')
lib_elem.write('    include "elements/xquad_element_module.f90"                           \n')
lib_elem.write('    include "elements/element_module.f90"                                 \n')
#else
lib_elem.write('    include "elements/wedge_element_module.f90"                           \n')
lib_elem.write('    include "elements/brick_element_module.f90"                           \n')
lib_elem.write('    include "elements/coh3d6_element_module.f90"                          \n')
lib_elem.write('    include "elements/coh3d8_element_module.f90"                          \n')
lib_elem.write('    include "elements/sub3d_element_module.f90"                           \n')
lib_elem.write('    include "elements/xbrick_element_module.f90"                          \n')
#end
lib_elem.write('    include "elements/element_module.f90"                                 \n')
lib_elem.write('                                                                          \n')
lib_elem.write('                                                                          \n')
lib_elem.write('    module lib_elem_module                                                \n')
lib_elem.write('    use parameter_module                                                  \n')
#if(ndim==2)
lib_elem.write('    use tri_element_module                                                \n')
lib_elem.write('    use quad_element_module                                               \n')
lib_elem.write('    use coh2d_element_module                                              \n')
lib_elem.write('    use sub2d_element_module                                              \n')
lib_elem.write('    use xquad_element_module                                              \n')
#else
lib_elem.write('    use wedge_element_module                                              \n')
lib_elem.write('    use brick_element_module                                              \n')
lib_elem.write('    use coh3d6_element_module                                             \n')
lib_elem.write('    use coh3d8_element_module                                             \n')
lib_elem.write('    use sub3d_element_module                                              \n')
lib_elem.write('    use xbrick_element_module                                             \n')
#end
lib_elem.write('    use element_module                                                    \n')
lib_elem.write('                                                                          \n')
lib_elem.write('    implicit none                                                         \n')
lib_elem.write('    save                                                                  \n')
lib_elem.write('                                                                          \n')
lib_elem.write('    type(element),          allocatable :: lib_elem(:)                    \n')
#if(ndim==2)     
lib_elem.write('    type(tri_element),      allocatable :: lib_tri(:)                     \n')
lib_elem.write('    type(quad_element),     allocatable :: lib_quad(:)                    \n')
lib_elem.write('    type(coh2d_element),    allocatable :: lib_coh2d(:)                   \n')
lib_elem.write('    type(xquad_element),    allocatable :: lib_xquad(:)                   \n')
#else
lib_elem.write('    type(wedge_element),    allocatable :: lib_wedge(:)                   \n')
lib_elem.write('    type(brick_element),    allocatable :: lib_brick(:)                   \n')
lib_elem.write('    type(coh3d6_element),   allocatable :: lib_coh3d6(:)                  \n')
lib_elem.write('    type(coh3d8_element),   allocatable :: lib_coh3d8(:)                  \n')  
lib_elem.write('    type(xbrick_element),   allocatable :: lib_xbrick(:)                  \n')
#end
lib_elem.write('                                                                          \n')   
lib_elem.write('    contains                                                              \n')
lib_elem.write('                                                                          \n')    
lib_elem.write('    subroutine initialize_lib_elem                                        \n')
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

#eltype=3080     # just a random name, must be consistent with uel.f
#nndrl=8         # no. of (real)nodes in the element. (here, 8 for linear brick elmt)
#nedge=8         # no. of breakable edges in this element
#nndfl=2*nedge
#nnode=nndrl+nndfl





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
 
            nds=l[1:]
            
            elems.append(element(nodes=nds,edges=[]))
            
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
        lib_node.write('        call update(lib_node('+str(cntr)+'),x=['+str(nd.x)+','+str(nd.y)+'],u=[zero,zero])\n')
    else:
        fnminp.write(str(cntr)+', '+str(nd.x)+', '+str(nd.y)+', '+str(nd.z)+'\n')
        lib_node.write('        call update(lib_node('+str(cntr)+'),x=['+str(nd.x)+','+str(nd.y)+','+str(nd.z)+'],u=[zero,zero,zero])\n')


#***************************************************************        
#       Write Edges
#***************************************************************
lib_edge.write('        nedge='+str(len(edges))+'   \n')
lib_edge.write('        allocate(lib_edge(nedge))   \n')
lib_edge.write('        lib_edge=0                  \n')

    
#***************************************************************        
#       Write Elements
#***************************************************************
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
    fnminp.write('*USER ELEMENT, TYPE='+str(eltuel)+', NODES='+str(nnode)+', COORDINATES='+str(ndim)+
        ', PROPERTIES='+str(nprops)+', VARIABLES='+str(nsvars)+'\n')
    if ndim==2:
        fnminp.write('1,2\n')
    else:
        fnminp.write('1,2,3\n')
      
    # allocate lib_elem in fnm lib_elem module
    lib_elem.write('        n'+elt+'='+str(elcount[eli])+'     \n')
    lib_elem.write('        allocate(lib_'+elt+'(n'+elt+'))   \n')

    # write elem nodal connec in abaqus fnm input file
    fnminp.write('*Element, type='+str(eltuel)+', elset=uel_'+elt+'\n')
    
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
        eline=str(cntr+elstart) 
        
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
            lib_elem.write('        call prepare(lib_'+elt+'('+str(cntr)+'),key='+str(cntr+elstart)+', & \n')
            lib_elem.write('& nodecnc=['+nodecnc[:-1]+'], & \n')
            lib_elem.write('& edgecnc=['+edgecnc[:-1]+'], & \n')
            lib_elem.write('& bulkmat=0, cohmat=0 ) \n')    # update matkeys later accord. to mat section assignment
        else:
            lib_elem.write('        call prepare(lib_'+elt+'('+str(cntr)+'),key='+str(cntr+elstart)+', & \n')
            lib_elem.write('& connec=['+nodecnc[:-1]+'], & \n')
            lib_elem.write('& matkey=0 ) \n')   # update matkey later accord. to mat section assignment
        
        # write elem type and typekey (elem index in its own type array) 
        lib_elem.write('        call update(lib_elem('+str(cntr+elstart)+'),elname="'+elt+'",eltype="'+elt+'",typekey='+str(cntr)+') \n')
        lib_elem.write('\n')


nelem=sum(elcount)
lib_elem.write('        nelem='+str(nelem)+'            \n')
lib_elem.write('        allocate(lib_elem(nelem))       \n')

#***************************************************************        
#       Write the rest
#***************************************************************

for line in lines[ln:]:
    fnminp.write(line)





#   close lib_node_module.f90
lib_node.write('    end subroutine initialize_lib_node        \n')
lib_node.write('    end module lib_node_module                \n')
lib_node.close()
#   close lib_node_module.f90
lib_edge.write('    end subroutine initialize_lib_edge        \n')
lib_edge.write('    end module lib_edge_module                \n')
lib_edge.close()
#   close lib_elem_module.f90
lib_elem.write('    end subroutine initialize_lib_elem        \n')
lib_elem.write('    end module lib_elem_module                \n')
lib_elem.close()


