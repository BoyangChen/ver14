
class lamina_modulus:

    def __init__(self, E1, E2, G12, G23, nu12, nu23):
        self.E1 = E1
        self.E2 = E2
        self.G12 = G12
        self.G23 = G23
        self.nu12 = nu12
        self.nu23 = nu23

class lamina_strength:

    def __init__(self, Xt, Xc, Yt, Yc, Sl, St):
        self.Xt=Xt
        self.Xc=Xc
        self.Yt=Yt
        self.Yc=Yc
        self.Sl=Sl
        self.St=St

class lamina_matrixtoughness:

    def __init__(self, GmcI, GmcII, eta):
        self.GmcI=GmcI
        self.GmcII=GmcII
        self.eta=eta

class lamina_fibretoughness:

    def __init__(self, GfcT, GfcC):
        self.GfcT=GfcT
        self.GfcC=GfcC

class lamina:

    def __init__(self, modulus, strength, matrixtoughness, fibretoughness):
        self.modulus = modulus
        self.strength=strength
        self.matrixtoughness=matrixtoughness
        self.fibretoughness=fibretoughness


class interface_modulus:

    def __init__(self, Dnn, Dtt, Dll):
        self.Dnn=Dnn
        self.Dtt=Dtt
        self.Dll=Dll

class interface_strength:

    def __init__(self, tau_nc, tau_tc, tau_lc):
        self.tau_nc=tau_nc
        self.tau_tc=tau_tc
        self.tau_lc=tau_lc

class interface_toughness:

    def __init__(self, Gnc, Gtc, Glc, eta):
        self.Gnc=Gnc
        self.Gtc=Gtc
        self.Glc=Glc
        self.eta=eta

class interface:

    def __init__(self, modulus, strength, toughness):
        self.modulus = modulus
        self.strength=strength
        self.toughness=toughness


class node:

    def __init__(self, x, y, z):
        self.x=x
        self.y=y
        self.z=z


class edge:

    def __init__(self, nd1, nd2, nd3, nd4):
        self.nd1=nd1
        self.nd2=nd2
        self.nd3=nd3
        self.nd4=nd4


class element:

    def __init__(self, nodes, edges):
        self.nodes=nodes
        self.edges=edges


node1=node(x=0.1,y=0.1,z=0.5)
node2=node(0.2,0.2,1)
node3=node(0.11,0.11,0.51)
node4=node(0.21,0.21,11)
nodes=[node1, node2, node3, node4]
edges=[edge(node1, node2, node3, node4), edge(node4, node2, node3, node1)]
elem=element(nodes,edges)

print elem.edges[1].nd1.z