from PyQt5.QtWidgets import *
from PyQt5.QtGui import QIcon

from style import darkgrey_css

import mingus.core.keys as mingkeys
import mingus.core.scales as mingscales


def keys():
    return list(map(lambda x: x[0], mingkeys.keys))


def notesOfTheScale(key, scale):
    if scale == 'Chromatic':
        return mingscales.Chromatic(key).ascending()
    elif scale == 'Whole Tone':
        return mingscales.WholeTone(key).ascending()
    elif scale == 'Octatonic':
        return mingscales.Octatonic(key).ascending()
    else:
        return []


def keyScaleSelectorWidget():

    keysComboBox = QComboBox()
    keysComboBox.addItems(keys())
    scalesComboBox = QComboBox()
    scalesComboBox.addItems(['Chromatic', 'Whole Tone', 'Octatonic'])
    displayOut = QTextEdit()
    displayOut.setReadOnly(True)

    def setScale(scale):
        key = keysComboBox.currentText()
        displayOut.setText(' - '.join(notesOfTheScale(key, scale)))

    scalesComboBox.currentTextChanged.connect(setScale)

    def setKey(key):
        scale = scalesComboBox.currentText()
        displayOut.setText(' - '.join(notesOfTheScale(key, scale)))

    keysComboBox.currentTextChanged.connect(setKey)

    mainLayout = QVBoxLayout()

    layout0 = QHBoxLayout()
    layout0.addWidget(QLabel('Key: '))
    layout0.addWidget(keysComboBox)
    layout0.addWidget(QLabel('Scale: '))
    layout0.addWidget(scalesComboBox)
    layout0.setStretch(0, 1)
    layout0.setStretch(1, 1)
    layout0.setStretch(2, 1)
    layout0.setStretch(3, 3)

    runButton = QPushButton("Run")
    runButton.clicked.connect(lambda checked: print(keys()))

    mainLayout.addLayout(layout0)
    mainLayout.addWidget(displayOut)
    # mainLayout.addWidget(runButton)

    # initialize
    setKey(keysComboBox.currentText())

    w = QWidget()
    w.setLayout(mainLayout)

    return w


class MainWindow(QMainWindow):

    def __init__(self, *args, **kwargs):
        super(MainWindow, self).__init__(*args, **kwargs)

        self.setWindowTitle('Arm me with harmony.')

        self.setCentralWidget(keyScaleSelectorWidget())

        self.setWindowIcon(QIcon('HH.png'))


if __name__ == '__main__':
    app = QApplication([])

    mainWindow = MainWindow()
    mainWindow.resize(1200, 200)
    mainWindow.show()

    # stylize
    app.setStyleSheet(darkgrey_css)
    font = app.font()
    font.setPointSize(32)
    app.setFont(font)

    # launch
    app.exec_()
