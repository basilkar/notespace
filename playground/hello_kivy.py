import kivy
kivy.require('1.10.0')

from kivy.app import App
from kivy.uix.label import Label


class HelloKivy(App):

    def build(self):
        return Label(text='Hello.')



if __name__ == '__main__':
    HelloKivy().run()