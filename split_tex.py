import re

f = open("GALR presentation.tex")


f1 = f.read()
f1 = f1.replace('\documentclass[11pt]{article}','')


preamble = re.split(r'\\begin{document}', f1)[0]

actual_text = re.split(r'\\begin{document}', f1)[1]



actual_text = actual_text.replace('\maketitle','')
actual_text = actual_text.replace('\end{document}','')


galr_preamble = open('/Users/Billy/Documents/Uni/cam/GAN/essay tex/galr_preamble.tex', 'w')
galr_preamble.write(preamble)
galr_preamble.close()


galr_text = open('/Users/Billy/Documents/Uni/cam/GAN/essay tex/galr_text.tex', 'w')
galr_text.write(actual_text)
galr_text.close()