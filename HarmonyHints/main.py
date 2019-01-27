from PyQt5.QtWidgets import (QApplication, QMainWindow, QWidget, QTextEdit, QLabel, QVBoxLayout, QHBoxLayout, QComboBox, QPushButton)



app = QApplication([])
app.setApplicationName("Harmony Hints")

mainLayout = QVBoxLayout()

layout0 = QHBoxLayout()

startChord = QTextEdit("C#")
endChord = QTextEdit("Ebm")

chordTransitions = QTextEdit()

def foo(checked):
    print("pressed: ", startChord.toPlainText())
    transition = startChord.toPlainText() + " -> " + endChord.toPlainText() 
    chordTransitions.setText(transition)

layout0.addWidget(startChord)
layout0.addWidget(endChord)

runButton = QPushButton("Run")

runButton.clicked.connect(foo)

mainLayout.addLayout(layout0)
mainLayout.addWidget(chordTransitions)
mainLayout.addWidget(runButton)

w = QWidget()
w.resize(512, 512)
w.setLayout(mainLayout)

mainWindow = QMainWindow()
mainWindow.setCentralWidget(w)
mainWindow.show()

app.exec_()