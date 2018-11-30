import numpy as np
import math

from sympy.geometry import *

# equilateral triangular grid
# basis vectors: ds = [ds, 0], dr = [ds*cos(120°), ds*sin(120°)]

"""
                *
               /|dy = sqrt(3)*ds
              / |
             *--|--*-----*
              \ |   \   /
          dr   \|    \ /
                *-----*
                   ds = dx
"""

class TriangularGrid:

    def __init__(self, dx):
        self.ds = [ dx, 0.0 ]
        theta = math.radians(120)
        self.dr = [ dx*math.cos(theta), dx*math.sin(theta) ]
        self.A = np.transpose(np.array([self.ds,
                                        self.dr]))
        self.Ainv = np.linalg.inv(self.A)

    def cart2tri( self, x, y ):
        pt_cart = np.array([x, y])
        pt_tri = self.Ainv.dot(pt_cart)
        pt_tri[0] = 0. if abs(pt_tri[0]) < 1e-15 else pt_tri[0]
        pt_tri[1] = 0. if abs(pt_tri[1]) < 1e-15 else pt_tri[1]
        return Point(pt_tri[0], pt_tri[1])

    def tri2cart( self, s, r ):
        pt_tri = np.array([s, r])
        pt_cart = self.A.dot(pt_tri)
        return Point(pt_cart[0], pt_cart[1])

    def index2cart(self, i, j):
        s = i*self.ds[0] + j*self.dr[0]
        r = i*self.ds[1] + j*self.dr[1]
        return self.tri2cart(s, r)

def minimal_enclosing_parallelogram( tg, rect ):
    #cartesian points
    left = rect.x
    right = rect.x + rect.w
    bottom = rect.y
    top = rect.y + rect.h

    # triangular grid coords
    top_left = tg.cart2tri( left, top )
    bottom_left = tg.cart2tri( left, bottom )
    top_right = tg.cart2tri( right, top )
    bottom_right = tg.cart2tri( right, bottom )

    imin = min( math.floor( top_left.x ), math.floor( bottom_left.x ) ) # leftmost
    imax = max( math.ceil( top_right.x ), math.ceil( bottom_right.x ) ) # rightmost
    jmin = min( math.floor( bottom_left.y ), math.ceil( bottom_right.y ) ) # bottommost
    jmax = max( math.ceil( top_left.y ), math.ceil( top_right.y ) ) # topmost

    return imin, imax, jmin, jmax


def cart_rect_contains_tri_pt( tg, rect, triIdx):
    cart_pt = tg.tri2cart(triIdx.x, triIdx.y)
    x_in_bounds = cart_pt[0] > rect.x and cart_pt[0] < rect.x + rect.w
    y_in_bounds = cart_pt[1] > rect.y and cart_pt[1] < rect.y + rect.h
    return x_in_bounds and y_in_bounds

def triangular_grid_coordinates( tg, rect ):

    imin, imax, jmin, jmax = minimal_enclosing_parallelogram( tg, rect )

    Is = range(imin, imax+1)
    Js = range(jmin, jmax+1)

    return [(i,j) for i in Is for j in Js if cart_rect_contains_tri_pt(tg, rect, Point(i, j))]
