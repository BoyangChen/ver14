#***************************************************************
#****************** Define useful classes **********************
#***************************************************************

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

    def __init__(self, nodes):
        self.nodes=nodes


class element:

    def __init__(self, nodes, edges):
        self.nodes=nodes
        self.edges=edges

