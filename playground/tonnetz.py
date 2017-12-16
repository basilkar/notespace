import kivy
kivy.require('1.10.0')
from kivy.app import App
from kivy.graphics import Color, Rectangle
from kivy.uix.floatlayout import FloatLayout
from kivy.uix.button import Button

from mingus.midi import fluidsynth
from mingus.containers import Note

import math
import os
import sys
import time

from collections import namedtuple
from functools import partial

this_dir = os.path.dirname( __file__ )
# the directory 'soundfonts/' must be a sibling directory to one containing this file
soundfont_filepath = os.path.abspath(os.path.join(this_dir, '..', 'soundfonts/GeneralUserGSv1.471.sf2'))
fluidsynth.init(soundfont_filepath)
# the directory 'tonnetz/' must be a sibling directory to one containing this file
tonnetz_dir = os.path.abspath(os.path.join(this_dir, '..', 'tonnetz'))
sys.path.insert(0, tonnetz_dir)

import TriangularGrid as tg
import TonnetzLogicalGrid as tnlg


Rect = namedtuple("Rect", "x y w h")
def rect(xmin, ymin, width, height):
    return Rect(x=xmin, y=ymin, w=width, h=height)

def play_note_callback(note_name, instance):
    fluidsynth.play_Note(Note(note_name))
    time.sleep(1.0)

def play_note(note_name):
    return partial(play_note_callback, note_name)



def tonnetz_nodes( grid, node_coordinates ):
    tn = tnlg.TonnetzLogicalGrid()
    tonnetz_node = lambda index: ( tn.node(index[0],index[1]), grid.tri2cart(index[0],index[1]))
    return map(tonnetz_node, node_coordinates)

class RootWidget(FloatLayout):

    def __init__(self, **kwargs):
        # make sure we aren't overriding any important functionality
        super(RootWidget, self).__init__(**kwargs)

        grid = tg.TriangularGrid(0.125)
        coords_tg = tg.triangular_grid_coordinates(grid, rect(-0.5,-0.5,1,1))

        for node in tonnetz_nodes(grid, coords_tg):
            tone = node[0]
            pos = (0.5 + float(node[1].x), 0.5 + float(node[1].y))
            add_tone_button(self, tone, pos)
            

class Tonnetz(App):

    def build(self):
        self.root = root = RootWidget()
        root.bind(size=self._update_rect, pos=self._update_rect)

        with root.canvas.before:
            Color(0,0,0,1) # rgba:[0,1]
            self.rect = Rectangle(size=root.size, pos=root.pos)
        return root

    def _update_rect(self, instance, value):
        self.rect.pos = instance.pos
        self.rect.size = instance.size


if __name__ == '__main__':
    Tonnetz().run()