#!/bin/bash
# Génère un snippet à copy pasté dans l'annexe LaTeX

for f in *.ml; do 
    asmf=${f%%.ml}.s # code assembleur mips
    resf=${f%%.ml}.result # resultat de l'execution du mips
    expf=${f%%.ml}.expected # resultat attendu

    printf "\subsection*{Test de $f}\n"
    printf "\lstinputlisting[language=Caml]{ml2mips/tests/$f}\n"
    printf "\lstinputlisting{ml2mips/tests/$asmf}\n"
    printf "\lstinputlisting{ml2mips/tests/$resf}\n\n"

done
