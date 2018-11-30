import unittest
import os
import sys
import math
import TriangularGrid as tg
from sympy.geometry import Point
from collections import namedtuple


# the directory 'tonnetz/' must be a sibling directory to one containing this folder
tonnetz_dir = os.path.abspath(os.path.join(os.path.dirname(__file__),
                                           '..',
                                           'tonnetz'))
sys.path.insert(0, tonnetz_dir)

Rect = namedtuple("Rect", "x y w h")


def rect(xmin, ymin, width, height):
    return Rect(x=xmin, y=ymin, w=width, h=height)


class TestTriangularGridMethods(unittest.TestCase):

    ds = 0.125
    grid = tg.TriangularGrid(ds)

    def test_tri2cart(self):
        s = Point(self.ds, 0.)
        r = Point(self.ds*math.cos(math.radians(120)),
                  self.ds*math.sin(math.radians(120)))
        self.assertEqual(self.grid.tri2cart(1, 0), s)
        self.assertEqual(self.grid.tri2cart(0, 1), r)

    def test_cart2tri(self):
        s = Point(self.ds, 0.)
        r = Point(self.ds*math.cos(math.radians(120)),
                  self.ds*math.sin(math.radians(120)))
        self.assertEqual(self.grid.cart2tri(s.x, s.y), Point(1., 0.))
        self.assertEqual(self.grid.cart2tri(r.x, r.y), Point(0., 1.))

    def test_minimal_enclosing_parallelogram(self):
        dx = 0.631
        eps = 0.1*dx
        grid = tg.TriangularGrid(dx)
        dy = math.sqrt(3)*dx
        r = rect(-eps, -eps, 2*dy+2*eps, 2*dy+2*eps)
        imin, imax, jmin, jmax = tg.minimal_enclosing_parallelogram(grid, r)
        self.assertEqual(imin, -1)
        self.assertEqual(imax, 6)
        self.assertEqual(jmin, -1)
        self.assertEqual(jmax, 5)

    def test_triangular_grid_coordinates(self):
        dx = 0.125
        eps = 0.1*dx
        grid = tg.TriangularGrid(dx)
        dy = math.sqrt(3)*dx
        r = rect(-eps, -eps, 2*dy+2*eps, 2*dy+2*eps)
        tg_coords = tg.triangular_grid_coordinates(grid, r)
        gt_coords = [(0, 0), (1, 0), (1, 1), (1, 2),
                     (2, 0), (2, 1), (2, 2), (2, 3), (2, 4),
                     (3, 0), (3, 1), (3, 2), (3, 3), (3, 4),
                     (4, 1), (4, 2), (4, 3), (4, 4),
                     (5, 3), (5, 4)]
        self.assertTrue(list(tg_coords) == gt_coords)


if __name__ == '__main__':
    unittest.main()