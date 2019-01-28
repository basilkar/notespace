from PyQt5.QtWidgets import *
from PyQt5.QtGui import QIcon

from style import darkgrey_css

import mingus.core.keys as mingkeys
import mingus.core.scales as mingscales


def keys():
    return list(map(lambda x: x[0], mingkeys.keys))


def scaleNotes(key, scale):
    if scale == 'Chromatic':
        return mingscales.Chromatic(key).ascending()
    elif scale == 'Whole Tone':
        return mingscales.WholeTone(key).ascending()
    elif scale == 'Octatonic':
        return mingscales.Octatonic(key).ascending()
    else:
        return []

app = QApplication([])
app.setApplicationName("Arm me with harmony.")

mainLayout = QVBoxLayout()

layout0 = QHBoxLayout()

textArea = QTextEdit()

keysComboBox = QComboBox()
keysComboBox.addItems(keys())
scalesComboBox = QComboBox()
scalesComboBox.addItems(['Chromatic', 'Whole Tone', 'Octatonic'])


def setScale(scale):
    key = keysComboBox.currentText()
    textArea.setText(' - '.join(scaleNotes(key, scale)))

scalesComboBox.currentTextChanged.connect(setScale)


def setKey(key):
    scale = scalesComboBox.currentText()
    textArea.setText(' - '.join(scaleNotes(key, scale)))

keysComboBox.currentTextChanged.connect(setKey)

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
mainLayout.addWidget(textArea)
# mainLayout.addWidget(runButton)

setKey(keysComboBox.currentText())

w = QWidget()
w.setLayout(mainLayout)

mainWindow = QMainWindow()
mainWindow.setCentralWidget(w)
mainWindow.resize(1200, 200)
mainWindow.show()

app.setStyleSheet(darkgrey_css)

font = app.font()
font.setPointSize(32)
app.setFont(font)
app.setWindowIcon(QIcon('HH.png'))
app.exec_()
